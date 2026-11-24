;;; omnidiff.el --- Native Emacs viewer for omnidiff content-aware diffs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; URL: https://github.com/oatiz/omnidiff.el
;; Keywords: vc, diff, tools

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Native Emacs viewer for content-aware Git diffs generated through
;; `omnidiff --mode json' (parallel Git blob pipeline and `GIT_EXTERNAL_DIFF'):
;;
;;  - Three interactive layouts (`unified', `side-by-side', `stacked') with
;;    inline syntax-aware refine highlights and move tracking.
;;  - Repo-relative collapsible file cards and hunk headers with sticky
;;    header-line orientation.
;;  - Hunk/file navigation, source jump (`RET'), preset cycling (`default',
;;    `minimal', `full'), and theme-adaptive color schemes.
;;  - Optional Magit DWIM and `magit-diff' transient integration via
;;    `omnidiff-magit-setup'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'text-property-search)
(require 'transient)
(require 'omnidiff-theme)
(require 'omnidiff-engine)
(require 'omnidiff-image)
(require 'omnidiff-dual)
(require 'omnidiff-render)
(require 'omnidiff-git)

(declare-function magit-toplevel "ext:magit-git" (&optional directory))
(declare-function magit-file-at-point "ext:magit-git" (&optional expand assert))
(declare-function magit-thing-at-point "ext:magit-section" (type &optional demand))
(declare-function magit-section-value-if "ext:magit-section" (type &optional section))
(declare-function magit-branch-or-commit-at-point "ext:magit-git" ())
(declare-function magit-read-branch-or-commit "ext:magit-git" (prompt &optional secondary-default))
(declare-function magit-diff-read-range-or-commit "ext:magit-diff" (prompt &optional secondary-default mbase))
(declare-function magit-diff--dwim "ext:magit-diff" ())
(declare-function magit-diff-arguments "ext:magit-diff" (&optional mode))
(declare-function magit-current-section "ext:magit-section" ())
(declare-function magit-section-match "ext:magit-section" (condition &optional section))
(declare-function magit-diff-hunk-line "ext:magit-diff" (section goto-from))
(declare-function magit-refresh "ext:magit-mode" ())
(declare-function transient-append-suffix "transient" (prefix loc suffix &optional keep-other))
(declare-function transient-get-suffix "transient" (prefix loc))
(declare-function eat-make "ext:eat" (name program &optional startfile &rest switches))
(declare-function eat "ext:eat" (&optional program arg))
(declare-function vterm "ext:vterm" (&optional arg))
(declare-function term-mode "term" ())
(declare-function term-exec "term" (buffer name command startfile switches))
(declare-function term-char-mode "term" ())

(defvar vterm-shell)

;;; Customization & UI buffer state

(defcustom omnidiff-tui-launcher 'auto
  "Terminal backend used by `omnidiff-open-in-tui' to launch `omnidiff' TUI.
- `auto': Prefer `eat', then `vterm', then built-in `ansi-term'.
- `eat': Launch in an `eat' terminal buffer.
- `vterm': Launch in a `vterm' buffer.
- `ansi-term': Launch in Emacs's built-in `term-mode' (`ansi-term').
- Function: Called as `(FN ARGV BUFFER-NAME DIR)'.  If FN returns a live
  Emacs buffer, temporary diff files are deleted when that buffer is killed.
  If FN returns a non-buffer (for example, when spawning an external
  terminal asynchronously), temporary diff files stay registered in the
  calling buffer's `omnidiff--tui-tmpdirs' and are cleaned up when that
  buffer is killed or when Emacs exits."
  :type '(choice (const :tag "Auto (eat -> vterm -> ansi-term)" auto)
                 (const :tag "Eat" eat)
                 (const :tag "Vterm" vterm)
                 (const :tag "Built-in ansi-term" ansi-term)
                 (function :tag "Custom launcher function"))
  :group 'omnidiff)

(defcustom omnidiff-line-spacing 0.15
  "Buffer-local `line-spacing' used in `omnidiff-mode' buffers."
  :type '(choice (const :tag "Default" nil) number)
  :group 'omnidiff)

(defcustom omnidiff-archive-extract-max-bytes (* 64 1024 1024)
  "Maximum uncompressed bytes extracted for one archive member drilldown.
Set to 0 to disable the limit."
  :type 'natnum
  :group 'omnidiff)

(defvar-local omnidiff--owned-tmpdirs nil
  "Temporary directories owned by this `omnidiff-mode' buffer, deleted on kill.")

(defvar-local omnidiff--tui-tmpdirs nil
  "Temporary directories owned by a spawned `omnidiff' TUI or caller buffer.")

(defvar omnidiff--ext-tui-tmpdirs nil
  "Temporary directories retained for async external TUI launchers until exit.")

(defvar-local omnidiff--sbs-resize-timer nil
  "Active debounce timer for `side-by-side' Overview window-resize reflow.")

;;; Navigation

(defun omnidiff--jump-prop (prop direction)
  "Move point to the next (DIRECTION > 0) or previous visible occurrence of PROP."
  (let* ((forward (> direction 0))
         (match
          (save-excursion
            (if forward
                (when (get-text-property (point) prop)
                  (goto-char (or (next-single-property-change (point) prop)
                                 (point-max))))
              (when (and (get-text-property (point) prop)
                         (> (point) (point-min))
                         (get-text-property (1- (point)) prop))
                (goto-char (or (previous-single-property-change (point) prop)
                               (point-min)))))
            (let ((m nil)
                  (found nil))
              (while (and (not found)
                          (setq m (if forward
                                      (text-property-search-forward prop)
                                    (text-property-search-backward prop))))
                (unless (invisible-p (prop-match-beginning m))
                  (setq found m)))
              found))))
    (if match
        (progn
          (goto-char (prop-match-beginning match))
          (beginning-of-line)
          (when (get-buffer-window (current-buffer))
            (recenter (if (eq prop 'omnidiff-file-header) 0 1))))
      (message "No %s %s"
               (if forward "next" "previous")
               (if (eq prop 'omnidiff-hunk) "hunk" "file")))))

(defun omnidiff-next-hunk ()
  "Jump to the next diff hunk."
  (interactive)
  (omnidiff--jump-prop 'omnidiff-hunk 1))

(defun omnidiff-prev-hunk ()
  "Jump to the previous diff hunk."
  (interactive)
  (omnidiff--jump-prop 'omnidiff-hunk -1))

(defun omnidiff-next-file ()
  "Jump to the next file card."
  (interactive)
  (omnidiff--jump-prop 'omnidiff-file-header 1))

(defun omnidiff-prev-file ()
  "Jump to the previous file card."
  (interactive)
  (omnidiff--jump-prop 'omnidiff-file-header -1))

;;; Folding

(defun omnidiff--set-overlay-folded (ov hidden invis-spec accent-face)
  "Set OV fold visibility to HIDDEN under INVIS-SPEC and update disclosure mark.
ACCENT-FACE styles the collapsed triangle."
  (when (and (not hidden) (overlay-get ov 'omnidiff-lazy-file))
    (omnidiff-render-materialize-file-overlay ov))
  (overlay-put ov 'invisible (and hidden invis-spec))
  (when-let* ((fold-ov (overlay-get ov 'omnidiff-fold-ov)))
    (overlay-put fold-ov 'display
                 (when hidden
                   (propertize "▶" 'face accent-face)))))

(defun omnidiff--set-overlays-folded (ovs hidden invis-spec accent-face)
  "Set fold visibility for all overlays in OVS to HIDDEN.
When unfolding overlays that include lazy-folded files, raises GC threshold,
shares render caches across batch materialization, and inhibits per-file
modification hooks, firing change hooks once."
  (let ((has-lazy (and (not hidden)
                       (cl-some (lambda (ov) (overlay-get ov 'omnidiff-lazy-file))
                                ovs))))
    (if (not has-lazy)
        (dolist (ov ovs)
          (omnidiff--set-overlay-folded ov hidden invis-spec accent-face))
      (let ((gc-cons-threshold most-positive-fixnum)
            (gc-cons-percentage 0.8)
            (old-size (- (point-max) (point-min))))
        (unless inhibit-modification-hooks
          (run-hook-with-args 'before-change-functions (point-min) (point-max)))
        (unwind-protect
            (let ((inhibit-modification-hooks t))
              (omnidiff-render-call-with-batch-caches
               (lambda ()
                 (dolist (ov ovs)
                   (omnidiff--set-overlay-folded ov nil invis-spec accent-face)))))
          (unless inhibit-modification-hooks
            (run-hook-with-args 'after-change-functions
                                (point-min) (point-max) old-size)))))))

(defun omnidiff--hunk-ov-at (&optional pos)
  "Return the `omnidiff-hunk-ov' overlay at POS (default point), or nil."
  (let ((p (or pos (point))))
    (or (get-text-property p 'omnidiff-hunk-ov)
        (cl-find-if (lambda (ov) (overlay-get ov 'omnidiff-hunk-ov))
                    (overlays-at p))
        (and (> p (point-min))
             (cl-find-if (lambda (ov)
                           (and (overlay-get ov 'omnidiff-hunk-ov)
                                (overlay-get ov 'invisible)))
                         (overlays-at (1- p)))))))

(defun omnidiff-toggle-fold ()
  "Toggle folding of the current file (on a header) or current hunk (in body)."
  (interactive)
  (cond
   ((get-text-property (point) 'omnidiff-file-header)
    (when-let* ((pos (line-beginning-position))
                (ov (get-text-property pos 'omnidiff-body-ov)))
      (let ((hidden (not (overlay-get ov 'invisible))))
        (omnidiff--set-overlay-folded ov hidden 'omnidiff-fold 'omnidiff-file-accent))))
   ((when-let* ((hov (omnidiff--hunk-ov-at)))
      (let ((hidden (not (overlay-get hov 'invisible))))
        (omnidiff--set-overlay-folded hov hidden 'omnidiff-hunk-fold 'omnidiff-hunk-accent)
        (when hidden
          (goto-char (or (omnidiff-render-overlay-header-pos hov)
                         (overlay-start hov)))))
      t))
   ;; Flattened raster image cards have no inner hunk overlay: TAB folds the file card.
   ((when-let* ((hdr (omnidiff-render-file-header-pos-at (point)))
                (ov (get-text-property hdr 'omnidiff-body-ov)))
      (let ((hidden (not (overlay-get ov 'invisible))))
        (omnidiff--set-overlay-folded ov hidden 'omnidiff-fold 'omnidiff-file-accent)
        (when hidden
          (goto-char hdr)))
      t))))

(defun omnidiff-toggle-fold-all ()
  "Toggle folding across all files in the omnidiff buffer."
  (interactive)
  (let* ((ovs (cl-remove-if-not
               (lambda (ov) (overlay-get ov 'omnidiff-file-body))
               (overlays-in (point-min) (point-max))))
         (any-open (cl-some (lambda (ov) (not (overlay-get ov 'invisible))) ovs))
         ;; Resolve the enclosing file header BEFORE hiding overlays.
         (target-hdr (when any-open
                       (omnidiff-render-file-header-pos-at (point)))))
    (omnidiff--set-overlays-folded ovs any-open 'omnidiff-fold 'omnidiff-file-accent)
    (when (and any-open target-hdr (invisible-p (point)))
      (goto-char target-hdr))))

(defconst omnidiff--trivial-summaries
  '(whitespace-only comment-only refactor-moved-only no-changes)
  "Semantic summary symbols folded/unfolded by `omnidiff-toggle-fold-trivial'.")

(defun omnidiff-toggle-fold-trivial ()
  "Toggle folding of file cards with trivial semantic summaries."
  (interactive)
  (let ((trivial-targets (make-hash-table :test #'equal))
        (trivial-ovs nil))
    (when omnidiff--diff
      (dolist (f (omnidiff-diff-files omnidiff--diff))
        (when (memq (omnidiff-engine-file-summary f) omnidiff--trivial-summaries)
          (puthash (omnidiff-file-target-path f) t trivial-targets))))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((m (text-property-search-forward 'omnidiff-file-header)))
          (if (null m)
              (goto-char (point-max))
            (let* ((pos (prop-match-beginning m))
                   (obj (get-text-property pos 'omnidiff-file-obj))
                   (rel (get-text-property pos 'omnidiff-file))
                   (ov (get-text-property pos 'omnidiff-body-ov))
                   (trivial-p (if (omnidiff-file-p obj)
                                  (memq (omnidiff-engine-file-summary obj)
                                        omnidiff--trivial-summaries)
                                (and rel (gethash rel trivial-targets)))))
              (when (and ov trivial-p)
                (push ov trivial-ovs)))))))
    (if (null trivial-ovs)
        (message "No trivial file cards to fold")
      (let* ((any-open (cl-some (lambda (ov) (not (overlay-get ov 'invisible))) trivial-ovs))
             (target-hdr (when any-open
                           (omnidiff-render-file-header-pos-at (point)))))
        (omnidiff--set-overlays-folded trivial-ovs any-open 'omnidiff-fold 'omnidiff-file-accent)
        (when (and any-open target-hdr (invisible-p (point)))
          (goto-char target-hdr))
        (message "%s %d trivial %s"
                 (if any-open "Folded" "Unfolded")
                 (length trivial-ovs)
                 (if (= (length trivial-ovs) 1) "file" "files"))))))

;;; Multi-file review tracking

(defun omnidiff--hash-text (text)
  "Return a SHA-1 digest of TEXT encoded as UTF-8."
  (secure-hash 'sha1 (if (stringp text)
                         (encode-coding-string text 'utf-8)
                       "")))

(defun omnidiff--file-review-signature (file)
  "Compute a content/OID signature for `omnidiff-file' FILE.
Used to preserve reviewed status across refreshes only when the file's diff
has not changed."
  (let* ((status (omnidiff-file-status file))
         (path (omnidiff-file-path file))
         (target (omnidiff-file-target-path file))
         (old-mode (omnidiff-file-old-mode file))
         (new-mode (omnidiff-file-new-mode file))
         (old-oid (omnidiff-file-old-oid file))
         (new-oid (omnidiff-file-new-oid file))
         (oid-complete-p
          (or (and old-oid new-oid)
              (and (eq status 'deleted) old-oid)
              (and (eq status 'added) new-oid))))
    (if oid-complete-p
        (list 'oid status path target old-oid new-oid old-mode new-mode)
      (let ((pic (omnidiff-engine-file-picture file))
            (cont (omnidiff-engine-file-container file)))
        (list 'content
              status path target old-oid new-oid old-mode new-mode
              (omnidiff--hash-text (omnidiff-file-before-source file))
              (omnidiff--hash-text (omnidiff-file-after-source file))
              (omnidiff-engine-file-binary-p file)
              (omnidiff-file-note file)
              (omnidiff-engine-file-summary file)
              (omnidiff-file-added file)
              (omnidiff-file-removed file)
              (omnidiff-file-updated file)
              (omnidiff-file-moved file)
              (when pic
                (list (omnidiff-picture-comparison pic)
                      (omnidiff-picture-changed-pixels pic)
                      (omnidiff-picture-total-pixels pic)
                      (when-let* ((b (omnidiff-picture-before pic)))
                        (list (omnidiff-picture-info-width b)
                              (omnidiff-picture-info-height b)
                              (omnidiff-picture-info-bytes b)))
                      (when-let* ((a (omnidiff-picture-after pic)))
                        (list (omnidiff-picture-info-width a)
                              (omnidiff-picture-info-height a)
                              (omnidiff-picture-info-bytes a)))))
              (when cont
                (mapcar (lambda (m)
                          (cons (omnidiff-json-container-member-key m)
                                (omnidiff-json-container-member-status m)))
                        (omnidiff-json-container-members cont))))))))

(defun omnidiff--sync-reviewed-for-diff (diff)
  "Prune `omnidiff--reviewed-files' against DIFF, invalidating changed files.
Returns a list of target paths that remain marked reviewed."
  (if (not (hash-table-p omnidiff--reviewed-files))
      nil
    (let ((kept (make-hash-table :test #'equal))
          (kept-paths nil))
      (when (omnidiff-diff-p diff)
        (dolist (file (omnidiff-diff-files diff))
          (let* ((target (omnidiff-file-target-path file))
                 (prev-sig (gethash target omnidiff--reviewed-files)))
            (when (and prev-sig
                       (equal prev-sig (omnidiff--file-review-signature file)))
              (puthash target prev-sig kept)
              (push target kept-paths)))))
      (setq omnidiff--reviewed-files (when (> (hash-table-count kept) 0) kept))
      (nreverse kept-paths))))

(defun omnidiff--find-file-header-pos (target)
  "Return start position of the file header for TARGET in the current buffer.
TARGET may be an `omnidiff-file' object (matched by `eq' against
`omnidiff-file-obj') or a path string (matched by `equal' against
`omnidiff-file')."
  (let ((by-obj (omnidiff-file-p target)))
    (save-excursion
      (goto-char (point-min))
      (let ((found nil))
        (while (and (not found) (< (point) (point-max)))
          (let ((m (text-property-search-forward 'omnidiff-file-header)))
            (if (null m)
                (goto-char (point-max))
              (let ((pos (prop-match-beginning m)))
                (when (if by-obj
                          (eq (get-text-property pos 'omnidiff-file-obj) target)
                        (equal (get-text-property pos 'omnidiff-file) target))
                  (setq found pos))))))
        found))))

(defun omnidiff--file-reviewed-p (target)
  "Return non-nil when TARGET is marked reviewed in the current Overview buffer."
  (let ((key (if (omnidiff-file-p target)
                 (omnidiff-file-target-path target)
               target)))
    (and (hash-table-p omnidiff--reviewed-files)
         (not (null (gethash key omnidiff--reviewed-files))))))

(defun omnidiff--set-file-reviewed (target reviewed-p)
  "Set reviewed state for TARGET to REVIEWED-P in the current Overview buffer.
TARGET may be an `omnidiff-file' object or a target path string.
Updates `omnidiff--reviewed-files', toggles the `✓' header indicator, folds
or unfolds the file card, and refreshes the header line."
  (let* ((by-obj (omnidiff-file-p target))
         (file (if by-obj
                   target
                 (and omnidiff--diff
                      (cl-find target (omnidiff-diff-files omnidiff--diff)
                               :key #'omnidiff-file-target-path
                               :test #'equal))))
         (key (if file (omnidiff-file-target-path file) target)))
    (when file
      (if reviewed-p
          (progn
            (unless (hash-table-p omnidiff--reviewed-files)
              (setq omnidiff--reviewed-files (make-hash-table :test #'equal)))
            (puthash key (omnidiff--file-review-signature file)
                     omnidiff--reviewed-files))
        (when (hash-table-p omnidiff--reviewed-files)
          (remhash key omnidiff--reviewed-files)
          (when (= (hash-table-count omnidiff--reviewed-files) 0)
            (setq omnidiff--reviewed-files nil))))
      (when-let* ((hdr (omnidiff--find-file-header-pos (if by-obj target key))))
        (omnidiff-render-set-file-reviewed-indicator hdr reviewed-p)
        (when-let* ((ov (get-text-property hdr 'omnidiff-body-ov)))
          (omnidiff--set-overlay-folded
           ov reviewed-p 'omnidiff-fold 'omnidiff-file-accent)))
      (force-mode-line-update)
      reviewed-p)))

(defun omnidiff--next-unreviewed-file (files current-target)
  "Return the next unreviewed `omnidiff-file' in FILES after CURRENT-TARGET.
CURRENT-TARGET may be an `omnidiff-file' object or a target path string.
Wraps around once and returns nil when all files in FILES are reviewed."
  (let ((len (length files)))
    (when (> len 0)
      (let* ((vec (vconcat files))
             (cur-idx (or (if (omnidiff-file-p current-target)
                              (cl-position current-target vec :test #'eq)
                            (cl-position current-target vec
                                         :key #'omnidiff-file-target-path
                                         :test #'equal))
                          -1))
             (found nil)
             (step 1))
        (while (and (<= step len) (null found))
          (let* ((cand (aref vec (mod (+ cur-idx step) len)))
                 (ctarget (omnidiff-file-target-path cand)))
            (when (and (stringp ctarget)
                       (not (string-empty-p ctarget))
                       (not (omnidiff--file-reviewed-p ctarget)))
              (setq found cand)))
          (setq step (1+ step)))
        found))))

(defun omnidiff-toggle-reviewed ()
  "Toggle reviewed status for the file at point in Overview.
Marking a file reviewed shows `✓', folds its card, and advances to the next
unreviewed file.  Unmarking removes `✓' and unfolds the card."
  (interactive)
  (let ((file (omnidiff--file-at-point)))
    (unless (and file (not (string-empty-p (omnidiff-file-target-path file))))
      (user-error "No file at point"))
    (let* ((target (omnidiff-file-target-path file))
           (new-state (not (omnidiff--file-reviewed-p target)))
           (files (and omnidiff--diff (omnidiff-diff-files omnidiff--diff)))
           (total (cl-count-if
                   (lambda (f)
                     (not (string-empty-p (or (omnidiff-file-target-path f) ""))))
                   files)))
      (omnidiff--set-file-reviewed file new-state)
      (let ((rev-cnt (omnidiff-render-reviewed-count)))
        (if new-state
            (let ((next (omnidiff--next-unreviewed-file files file)))
              (if next
                  (let* ((next-target (omnidiff-file-target-path next))
                         (next-pos (omnidiff--find-file-header-pos next)))
                    (when next-pos (goto-char next-pos))
                    (message "Reviewed %s (%d/%d) — next: %s"
                             target rev-cnt total next-target))
                (when-let* ((hdr (omnidiff--find-file-header-pos file)))
                  (when (invisible-p (point))
                    (goto-char hdr)))
                (message "All %d %s reviewed!"
                         total (if (= total 1) "file" "files"))))
          (message "Unmarked %s (%d/%d reviewed)" target rev-cnt total)))
      new-state)))

;;; View controls & source jump

(transient-define-prefix omnidiff-overview-menu ()
  "Interactive command dispatch menu for `omnidiff-mode' Overview buffers."
  [:description
   (lambda ()
     (let ((rev (omnidiff-render-reviewed-count)))
       (format "OmniDiff Overview — %s (%d %s%s)"
               (or omnidiff--label "diff")
               omnidiff--file-count
               (if (= omnidiff--file-count 1) "file" "files")
               (if (> rev 0) (format ", %d reviewed" rev) ""))))
   ["Navigate & Fold"
    ("d" "Open file in Dual viewer" omnidiff-open-dual-at-point)
    ("R" "Toggle file reviewed" omnidiff-toggle-reviewed)
    ("RET" "Jump to source / drill member" omnidiff-jump-to-source)
    ("n" "Next hunk" omnidiff-next-hunk)
    ("p" "Previous hunk" omnidiff-prev-hunk)
    ("]" "Next file card" omnidiff-next-file)
    ("[" "Previous file card" omnidiff-prev-file)
    ("TAB" "Fold / unfold current" omnidiff-toggle-fold)
    ("S-TAB" "Fold / unfold all files" omnidiff-toggle-fold-all)
    ("F" "Fold / unfold trivial files" omnidiff-toggle-fold-trivial)]
   ["View & Style"
    ("v" (lambda ()
           (format "Layout [%s]" (or omnidiff--layout omnidiff-default-layout)))
     omnidiff-cycle-layout)
    ("H" (lambda ()
           (format "Highlight [%s]" (omnidiff-render-effective-highlight-style)))
     omnidiff-cycle-highlight-style)
    ("c" (lambda () (format "Color scheme [%s]" omnidiff-color-scheme))
     omnidiff-cycle-color-scheme)
    ("w" (lambda ()
           (format "Line wrap [%s]" (if truncate-lines "truncated" "wrapped")))
     omnidiff-toggle-wrap)]
   ["Diff Engine & Actions"
    ("m" (lambda () (format "Preset [%s]" (omnidiff-git-effective-preset)))
     omnidiff-cycle-preset)
    ("W" (lambda ()
           (format "--whole-updates [%s]"
                   (if (omnidiff-git-effective-whole-updates) "on" "off")))
     omnidiff-toggle-whole-updates)
    ("I" (lambda ()
           (format "--paint-reindent-moves [%s]"
                   (if (omnidiff-git-effective-paint-reindent-moves) "on" "off")))
     omnidiff-toggle-paint-reindent-moves)
    ("T" "Open file in native TUI" omnidiff-open-in-tui)
    ("g" "Refresh diff" omnidiff-refresh)
    ("q" "Quit viewer" quit-window)]])

(defun omnidiff-help ()
  "Open context-aware transient menu for `omnidiff-mode'.
In noninteractive sessions, displays a concise echo-area keybinding summary."
  (interactive)
  (if (not noninteractive)
      (if (omnidiff-image-card-ov-at)
          (call-interactively #'omnidiff-image-menu)
        (call-interactively #'omnidiff-overview-menu))
    (if (omnidiff-image-card-ov-at)
        (message "Omnidiff [image]: 1-6/i mode · b flip (DEL back) · h/l/←/→ adjust · +/-/0 zoom · t backdrop · r regions (</>) · o/RET open · x external · T tui · L load · TAB fold · v layout (%s) · q quit"
                 (or omnidiff--layout omnidiff-default-layout))
      (message "Omnidiff: d dual · R reviewed · n/p hunk · ]/[ file · TAB/S-TAB/F fold · RET jump/drill · v view (%s) · H style (%s) · m preset · W whole · I reindent · c color (%s) · T tui · w wrap · g refresh · q quit"
               (or omnidiff--layout omnidiff-default-layout)
               (omnidiff-render-effective-highlight-style)
               omnidiff-color-scheme))))

(defun omnidiff-cycle-color-scheme ()
  "Cycle `omnidiff-color-scheme' across semantic, calm, and paper schemes."
  (interactive)
  (let ((scheme (omnidiff-theme-cycle-color-scheme)))
    (message "Omnidiff color scheme: %s" scheme)))

(defun omnidiff--nearest-new-line-in-file (pos)
  "Return nearest `omnidiff-new-line' integer to POS within the same file card."
  (when-let* ((hdr (omnidiff-render-file-header-pos-at pos)))
    (let* ((file-end (or (save-excursion
                           (goto-char (or (next-single-property-change
                                           hdr 'omnidiff-file-header)
                                          (point-max)))
                           (when-let* ((nm (text-property-search-forward 'omnidiff-file-header)))
                             (prop-match-beginning nm)))
                         (point-max)))
           (best-ln nil)
           (best-dist most-positive-fixnum))
      (save-excursion
        (goto-char hdr)
        (forward-line 1)
        (while (< (point) file-end)
          (when-let* ((nl (get-text-property (point) 'omnidiff-new-line)))
            (when (integerp nl)
              (let ((d (abs (- (point) pos))))
                (when (< d best-dist)
                  (setq best-dist d
                        best-ln nl)))))
          (forward-line 1)))
      best-ln)))

(defun omnidiff-jump-to-source ()
  "Inspect container member, open Dual on text headers, or visit source at point."
  (interactive)
  (cond
   ((when-let* ((member (get-text-property (point) 'omnidiff-container-member))
                (cfile (get-text-property (point) 'omnidiff-container-file)))
      (omnidiff--drilldown-container-member cfile member)
      t))
   ((omnidiff-image-jump-at-point))
   ((and (get-text-property (point) 'omnidiff-file-header)
         (when-let* ((file (omnidiff--file-at-point)))
           (omnidiff--file-text-dual-p file)))
    (omnidiff-open-dual-at-point))
   (t
    (let* ((pos (point))
           (rel (get-text-property pos 'omnidiff-file))
           (new-ln (get-text-property pos 'omnidiff-new-line))
           (old-ln (get-text-property pos 'omnidiff-old-line))
           (side (get-text-property pos 'omnidiff-side))
           (fallback-ln (get-text-property pos 'omnidiff-line))
           (before-only (and (null new-ln) (eq side 'before)))
           (nearest-new (when before-only
                          (omnidiff--nearest-new-line-in-file pos)))
           (target-ln (cond
                       ((integerp new-ln) new-ln)
                       (before-only nearest-new)
                       ((integerp fallback-ln) fallback-ln))))
      (unless (and (stringp rel) (not (string-empty-p rel)))
        (user-error "No source file at point"))
      (let* ((dir (or omnidiff--repo-dir default-directory))
             (full (omnidiff-engine-expand-repo-path rel dir)))
        (if (not (file-exists-p full))
            (let ((entry (or (omnidiff--file-at-point)
                             (and omnidiff--diff
                                  (cl-find-if (lambda (f)
                                                (equal (omnidiff-file-target-path f) rel))
                                              (omnidiff-diff-files omnidiff--diff))))))
              (unless (and entry
                           (omnidiff-image-open-blob-other-window entry 'before dir))
                (user-error "File does not exist on disk: %s" rel)))
          (find-file-other-window full)
          (when (integerp target-ln)
            (goto-char (point-min))
            (forward-line (1- target-ln))
            (recenter))
          (when (and before-only (integerp old-ln))
            (if nearest-new
                (message "Deleted line (old L%d); jumped to nearest line L%d" old-ln nearest-new)
              (message "Old-side line L%d has no working-tree target" old-ln)))))))))

(defun omnidiff-toggle-wrap ()
  "Toggle line truncation in the omnidiff buffer."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  (message "Omnidiff line wrap: %s" (if truncate-lines "truncated" "wrapped")))

(defun omnidiff--cleanup-owned-tmpdirs ()
  "Delete temporary directories owned by the current `omnidiff-mode' buffer."
  (let ((dirs omnidiff--owned-tmpdirs))
    (setq omnidiff--owned-tmpdirs nil)
    (dolist (d dirs)
      (when (and (stringp d) (file-directory-p d))
        (ignore-errors (delete-directory d t))))))

(defun omnidiff--cleanup-tui-tmpdirs ()
  "Delete temporary directories registered in `omnidiff--tui-tmpdirs'."
  (let ((dirs omnidiff--tui-tmpdirs))
    (setq omnidiff--tui-tmpdirs nil)
    (dolist (dir dirs)
      (setq omnidiff--ext-tui-tmpdirs (delete dir omnidiff--ext-tui-tmpdirs))
      (when (and (stringp dir) (file-directory-p dir))
        (ignore-errors (delete-directory dir t))))))

(defun omnidiff--cleanup-ext-tui-tmpdirs ()
  "Delete any remaining external TUI temporary directories on Emacs exit."
  (let ((dirs omnidiff--ext-tui-tmpdirs))
    (setq omnidiff--ext-tui-tmpdirs nil)
    (dolist (dir dirs)
      (when (and (stringp dir) (file-directory-p dir))
        (ignore-errors (delete-directory dir t))))))

(defun omnidiff--cancel-sbs-resize-timer ()
  "Cancel any pending `side-by-side' text reflow timer for the current buffer."
  (when (timerp omnidiff--sbs-resize-timer)
    (cancel-timer omnidiff--sbs-resize-timer))
  (setq omnidiff--sbs-resize-timer nil))

(defun omnidiff--snapshot-folded-files ()
  "Return list of repo-relative file paths whose cards are currently folded."
  (let ((folded nil))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((m (text-property-search-forward 'omnidiff-file-header)))
          (if (null m)
              (goto-char (point-max))
            (let* ((pos (prop-match-beginning m))
                   (rel (get-text-property pos 'omnidiff-file))
                   (ov (get-text-property pos 'omnidiff-body-ov)))
              (when (and rel ov (overlay-buffer ov) (overlay-get ov 'invisible))
                (push rel folded)))))))
    folded))

(defun omnidiff--snapshot-folded-table ()
  "Return hash table of repo-relative file paths whose cards are currently folded."
  (let ((tbl (make-hash-table :test #'equal)))
    (dolist (rel (omnidiff--snapshot-folded-files))
      (puthash rel t tbl))
    tbl))

(defun omnidiff--reflow-sbs-buffer (buf)
  "Re-render cached `side-by-side' Overview BUF for its new window width."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq omnidiff--sbs-resize-timer nil)
      (when (and (derived-mode-p 'omnidiff-mode)
                 (eq (or omnidiff--layout omnidiff-default-layout) 'side-by-side)
                 omnidiff--diff
                 (null (omnidiff-git-pending-request)))
        (when-let* ((win (get-buffer-window buf t)))
          (let ((cur-w (window-body-width win))
                (saved-file (or (get-text-property (point) 'omnidiff-file-obj)
                                (get-text-property (point) 'omnidiff-file)))
                (saved-old (get-text-property (point) 'omnidiff-old-line))
                (saved-new (get-text-property (point) 'omnidiff-new-line))
                (saved-side (get-text-property (point) 'omnidiff-side))
                (saved-line (get-text-property (point) 'omnidiff-line))
                (folded (omnidiff--snapshot-folded-table)))
            (setq omnidiff--last-sbs-width cur-w)
            (omnidiff-render-buffer omnidiff--diff folded)
            (omnidiff--restore-location
             saved-file saved-old saved-new saved-side saved-line)
            (force-mode-line-update)))))))

(defun omnidiff--on-window-resize (&optional _frame-or-win)
  "Debounced `window-size-change-functions' handler reflowing `side-by-side' diffs."
  (when (and (derived-mode-p 'omnidiff-mode)
             (eq (or omnidiff--layout omnidiff-default-layout) 'side-by-side)
             omnidiff--diff
             (null (omnidiff-git-pending-request)))
    (when-let* ((win (get-buffer-window (current-buffer) t)))
      (let ((cur-w (window-body-width win)))
        (cond
         ((null omnidiff--last-sbs-width)
          (setq omnidiff--last-sbs-width cur-w))
         ((>= (abs (- cur-w omnidiff--last-sbs-width)) 4)
          (omnidiff--cancel-sbs-resize-timer)
          (setq omnidiff--sbs-resize-timer
                (run-with-idle-timer 0.15 nil #'omnidiff--reflow-sbs-buffer
                                     (current-buffer)))))))))

(defun omnidiff-refresh ()
  "Re-run the current omnidiff command in place, preserving image card states."
  (interactive)
  (let ((cmd (omnidiff-git-effective-command))
        (lbl (omnidiff-git-effective-label))
        (dir (omnidiff-git-effective-repo-dir)))
    (unless cmd
      (user-error "No omnidiff command recorded for this buffer"))
    (omnidiff-run (current-buffer) cmd lbl dir
                  (omnidiff-git-effective-preset)
                  (omnidiff-git-effective-layout)
                  nil t nil
                  (omnidiff-git-effective-whole-updates)
                  (omnidiff-git-effective-paint-reindent-moves))))

(defun omnidiff--restore-location (file old-ln new-ln side &optional fallback-ln)
  "Move point to the rendered position matching FILE, OLD-LN, NEW-LN, and SIDE.
FILE may be an `omnidiff-file' object or a target path string."
  (when file
    (when-let* ((file-pos (omnidiff--find-file-header-pos file)))
      (goto-char file-pos)
      (let ((any-target (or new-ln old-ln fallback-ln)))
        (when (integerp any-target)
          (when-let* ((body-ov (get-text-property file-pos 'omnidiff-body-ov)))
            (when (overlay-get body-ov 'omnidiff-lazy-file)
              (omnidiff-render-materialize-file-overlay body-ov)))
          (let ((file-end (or (save-excursion
                                (goto-char (or (next-single-property-change
                                                file-pos 'omnidiff-file-header)
                                               (point-max)))
                                (when-let* ((nm (text-property-search-forward 'omnidiff-file-header)))
                                  (prop-match-beginning nm)))
                              (point-max)))
                (best-pos nil)
                (best-tier 99)
                (best-dist most-positive-fixnum))
            (save-excursion
              (goto-char file-pos)
              (forward-line 1)
              (while (and (< (point) file-end)
                          (not (and (= best-tier 0) (= best-dist 0))))
                (unless (get-text-property (point) 'omnidiff-hunk)
                  (let* ((r-old (get-text-property (point) 'omnidiff-old-line))
                         (r-new (get-text-property (point) 'omnidiff-new-line))
                         (r-ln (get-text-property (point) 'omnidiff-line))
                         (tier nil)
                         (dist nil))
                    (cond
                     ((and (eq side 'before) (integerp old-ln) (integerp r-old))
                      (setq tier 0
                            dist (abs (- r-old old-ln))))
                     ((and (eq side 'after) (integerp new-ln) (integerp r-new))
                      (setq tier 0
                            dist (abs (- r-new new-ln))))
                     ((and (eq side 'both)
                           (or (and (integerp new-ln) (integerp r-new))
                               (and (integerp old-ln) (integerp r-old))))
                      (setq tier 0
                            dist (if (and (integerp new-ln) (integerp r-new))
                                     (abs (- r-new new-ln))
                                   (abs (- r-old old-ln)))))
                     ((and (integerp new-ln) (integerp r-new))
                      (setq tier 1
                            dist (abs (- r-new new-ln))))
                     ((and (integerp old-ln) (integerp r-old))
                      (setq tier 1
                            dist (abs (- r-old old-ln))))
                     ((integerp r-ln)
                      (setq tier 2
                            dist (abs (- r-ln any-target)))))
                    (when (and tier
                               (or (< tier best-tier)
                                   (and (= tier best-tier) (< dist best-dist))))
                      (setq best-tier tier
                            best-dist dist
                            best-pos (point)))))
                (forward-line 1)))
            (when best-pos
              (goto-char best-pos)
              (beginning-of-line)
              ;; In side-by-side rows, place point inside the right cell when restoring `after'.
              (when (and (eq side 'after)
                         (not (eq (get-text-property (point) 'omnidiff-side) 'after)))
                (let ((eol (line-end-position))
                      (scan (point))
                      (found nil))
                  (while (and (not found) (< scan eol))
                    (setq scan (next-single-property-change scan 'omnidiff-side nil eol))
                    (when (and (< scan eol)
                               (eq (get-text-property scan 'omnidiff-side) 'after))
                      (setq found scan)))
                  (when found
                    (goto-char found))))
              (when (invisible-p (point))
                (let ((body-ov (get-text-property file-pos 'omnidiff-body-ov)))
                  (if (and body-ov (overlay-buffer body-ov)
                           (overlay-get body-ov 'invisible))
                      (omnidiff--set-overlay-folded
                       body-ov nil 'omnidiff-fold 'omnidiff-file-accent)
                    (goto-char file-pos)))
                (when (invisible-p (point))
                  (goto-char file-pos))))))))))

(defun omnidiff-cycle-layout (&optional reset-pinned-p)
  "Cycle omnidiff view layout (`unified' -> `side-by-side' -> `stacked').
Preserves image cards whose mode was explicitly pinned unless prefix
argument RESET-PINNED-P is non-nil."
  (interactive "P")
  (let ((next-layout (cl-ecase (omnidiff-git-effective-layout)
                       (unified 'side-by-side)
                       (side-by-side 'stacked)
                       (stacked 'unified))))
    (cond
     ;; In-flight request: update target layout in place without restarting workers.
     ((omnidiff-git-pending-request)
      (let ((msg (format "Omnidiff layout: %s..." next-layout)))
        (omnidiff-git-update-pending-layout next-layout msg)
        (message "%s" msg)))
     ;; Cached parsed diff: re-render in memory with zero subprocesses.
     (omnidiff--diff
      (let ((saved-file (or (get-text-property (point) 'omnidiff-file-obj)
                            (get-text-property (point) 'omnidiff-file)))
            (saved-old (get-text-property (point) 'omnidiff-old-line))
            (saved-new (get-text-property (point) 'omnidiff-new-line))
            (saved-side (get-text-property (point) 'omnidiff-side))
            (saved-line (get-text-property (point) 'omnidiff-line))
            (folded (omnidiff--snapshot-folded-table)))
        (setq omnidiff--layout next-layout)
        (omnidiff-image-reset-layout-overrides reset-pinned-p)
        (omnidiff-render-buffer omnidiff--diff folded)
        (setq omnidiff--last-sbs-width
              (when (eq next-layout 'side-by-side)
                (when-let* ((w (get-buffer-window (current-buffer) t)))
                  (window-body-width w))))
        (omnidiff--restore-location saved-file saved-old saved-new saved-side saved-line)
        (force-mode-line-update)
        (message "Omnidiff layout: %s" next-layout)))
     ;; No cache yet: fall back to re-running the recorded command.
     (omnidiff--command
      (omnidiff-run (current-buffer) omnidiff--command omnidiff--label omnidiff--repo-dir
                    (omnidiff-git-effective-preset)
                    next-layout
                    (format "Omnidiff layout: %s..." next-layout)))
     (t
      (user-error "No omnidiff command recorded for this buffer")))))

(defun omnidiff-cycle-preset ()
  "Cycle omnidiff verbosity preset (`minimal' -> `full' -> `default')."
  (interactive)
  (let ((cmd (omnidiff-git-effective-command))
        (lbl (omnidiff-git-effective-label))
        (dir (omnidiff-git-effective-repo-dir)))
    (unless cmd
      (user-error "No omnidiff command recorded for this buffer"))
    (let ((next-preset (pcase (omnidiff-git-effective-preset)
                         ('minimal 'full)
                         ('full 'default)
                         (_ 'minimal))))
      (omnidiff-run (current-buffer) cmd lbl dir
                    next-preset
                    (omnidiff-git-effective-layout)
                    (format "Omnidiff preset: %s..." next-preset)
                    nil nil
                    (omnidiff-git-effective-whole-updates)
                    (omnidiff-git-effective-paint-reindent-moves)))))

(defun omnidiff-cycle-highlight-style ()
  "Cycle Overview highlight style (`token' -> `subtle-line' -> `traditional')."
  (interactive)
  (let ((next (pcase (omnidiff-render-effective-highlight-style)
                ('token 'subtle-line)
                ('subtle-line 'traditional)
                (_ 'token))))
    (setq omnidiff--highlight-style next)
    (when omnidiff--diff
      (let ((saved-file (or (get-text-property (point) 'omnidiff-file-obj)
                            (get-text-property (point) 'omnidiff-file)))
            (saved-old (get-text-property (point) 'omnidiff-old-line))
            (saved-new (get-text-property (point) 'omnidiff-new-line))
            (saved-side (get-text-property (point) 'omnidiff-side))
            (saved-line (get-text-property (point) 'omnidiff-line))
            (folded (omnidiff--snapshot-folded-table)))
        (omnidiff-render-buffer omnidiff--diff folded)
        (omnidiff--restore-location saved-file saved-old saved-new saved-side saved-line)
        (force-mode-line-update)))
    (message "Omnidiff highlight style: %s" next)))

(defun omnidiff-toggle-whole-updates ()
  "Toggle `--whole-updates' for this omnidiff buffer and re-run."
  (interactive)
  (let ((cmd (omnidiff-git-effective-command))
        (lbl (omnidiff-git-effective-label))
        (dir (omnidiff-git-effective-repo-dir))
        (next (not (omnidiff-git-effective-whole-updates))))
    (unless cmd
      (user-error "No omnidiff command recorded for this buffer"))
    (omnidiff-run (current-buffer) cmd lbl dir
                  (omnidiff-git-effective-preset)
                  (omnidiff-git-effective-layout)
                  (format "Omnidiff --whole-updates: %s..." (if next "on" "off"))
                  t nil next (omnidiff-git-effective-paint-reindent-moves))))

(defun omnidiff-toggle-paint-reindent-moves ()
  "Toggle `--paint-reindent-moves' for this omnidiff buffer and re-run."
  (interactive)
  (let ((cmd (omnidiff-git-effective-command))
        (lbl (omnidiff-git-effective-label))
        (dir (omnidiff-git-effective-repo-dir))
        (next (not (omnidiff-git-effective-paint-reindent-moves))))
    (unless cmd
      (user-error "No omnidiff command recorded for this buffer"))
    (omnidiff-run (current-buffer) cmd lbl dir
                  (omnidiff-git-effective-preset)
                  (omnidiff-git-effective-layout)
                  (format "Omnidiff --paint-reindent-moves: %s..." (if next "on" "off"))
                  t nil (omnidiff-git-effective-whole-updates) next)))

;;; Dual bridge, TUI launcher & container drilldown

(defun omnidiff--file-text-dual-p (file)
  "Return non-nil when `omnidiff-file' FILE is a non-binary text file for Dual."
  (let ((target (omnidiff-file-target-path file))
        (pic (omnidiff-engine-file-picture file))
        (cont (omnidiff-engine-file-container file))
        (bin (omnidiff-engine-file-binary-p file)))
    (and (stringp target)
         (not (string-empty-p target))
         (not (omnidiff-file-too-large-p file))
         (null cont)
         (or (null pic) (omnidiff-picture-svg-p pic))
         (not bin))))

(defun omnidiff--file-at-point ()
  "Return the `omnidiff-file' at point in the current Overview buffer, or nil."
  (let ((obj (or (get-text-property (point) 'omnidiff-file-obj)
                 (when-let* ((hdr (omnidiff-render-file-header-pos-at (point))))
                   (get-text-property hdr 'omnidiff-file-obj)))))
    (if (omnidiff-file-p obj)
        obj
      (when omnidiff--diff
        (let* ((files (omnidiff-diff-files omnidiff--diff))
               (rel (or (get-text-property (point) 'omnidiff-file)
                        (when-let* ((hdr (omnidiff-render-file-header-pos-at (point))))
                          (get-text-property hdr 'omnidiff-file)))))
          (or (and rel
                   (cl-find-if (lambda (f)
                                 (equal (omnidiff-file-target-path f) rel))
                               files))
              (car files)))))))

(defun omnidiff--worktree-after-path (file repo-dir &optional cmd)
  "Return real working-tree path for FILE's AFTER side when applicable, else nil."
  (let* ((status (omnidiff-file-status file))
         (target (omnidiff-file-target-path file))
         (new-mode (omnidiff-file-new-mode file))
         (eff-cmd (or cmd omnidiff--command))
         (full (when (and repo-dir target (not (string-empty-p target)))
                 (omnidiff-engine-expand-repo-path target repo-dir))))
    (when (and (not (eq status 'deleted))
               (not (equal new-mode "120000"))
               full
               (not (file-symlink-p full))
               (file-regular-p full)
               (or (omnidiff-engine-unstaged-worktree-diff-p eff-cmd)
                   (and (null (omnidiff-file-new-oid file))
                        (consp eff-cmd)
                        (equal (nth 2 eff-cmd) "diff"))))
      full)))

(defun omnidiff--materialize-file-sides (file repo-dir tmpdir &optional cmd)
  "Materialize BEFORE and AFTER paths for FILE under TMPDIR.
Returns `(BEFORE-PATH . AFTER-PATH)' using existing disk files where valid
and `git cat-file blob' into TMPDIR for Git revisions."
  (let* ((status (omnidiff-file-status file))
         (old-path (omnidiff-file-path file))
         (new-path (omnidiff-file-target-path file))
         (old-oid (omnidiff-file-old-oid file))
         (new-oid (omnidiff-file-new-oid file))
         (new-mode (omnidiff-file-new-mode file))
         (b-disk (omnidiff-file-before-disk-path file))
         (a-disk (omnidiff-file-after-disk-path file))
         (wt-after (omnidiff--worktree-after-path file repo-dir cmd))
         (full-after (when (and repo-dir new-path (not (string-empty-p new-path)))
                       (omnidiff-engine-expand-repo-path new-path repo-dir)))
         (before-file
          (cond
           ((eq status 'added) "/dev/null")
           ((and (stringp b-disk) (file-exists-p b-disk)) b-disk)
           ((omnidiff-git-cat-blob-to-file old-oid old-path repo-dir tmpdir "before"))
           ((stringp (omnidiff-file-before-source file))
            (let ((dest (omnidiff-git-blob-dest "before-src" old-path tmpdir)))
              (with-temp-buffer
                (insert (omnidiff-file-before-source file))
                (let ((coding-system-for-write 'utf-8-unix)
                      (write-region-inhibit-fsync t))
                  (write-region (point-min) (point-max) dest nil 'silent)))
              dest))
           (t "/dev/null")))
         (after-file
          (cond
           ((eq status 'deleted) "/dev/null")
           ((and (stringp a-disk) (file-exists-p a-disk)) a-disk)
           (wt-after wt-after)
           ((omnidiff-git-cat-blob-to-file new-oid new-path repo-dir tmpdir "after"))
           ((and full-after (file-symlink-p full-after))
            (omnidiff-git-write-small-temp
             (omnidiff-git-blob-dest "after-sym" new-path tmpdir)
             (encode-coding-string (file-symlink-p full-after) 'utf-8)))
           ((and full-after
                 (not (equal new-mode "120000"))
                 (file-regular-p full-after))
            full-after)
           ((stringp (omnidiff-file-after-source file))
            (let ((dest (omnidiff-git-blob-dest "after-src" new-path tmpdir)))
              (with-temp-buffer
                (insert (omnidiff-file-after-source file))
                (let ((coding-system-for-write 'utf-8-unix)
                      (write-region-inhibit-fsync t))
                  (write-region (point-min) (point-max) dest nil 'silent)))
              dest))
           (t "/dev/null"))))
    (cons before-file after-file)))

(defun omnidiff--launch-tui-pair (before-file after-file label dir &optional tmpdir)
  "Launch interactive `omnidiff' TUI on BEFORE-FILE and AFTER-FILE.
LABEL names the terminal buffer, DIR sets `default-directory', and optional
TMPDIR is cleaned up when the terminal buffer (or calling buffer for an
external custom launcher returning a non-buffer) is killed."
  (let* ((bin (or (executable-find omnidiff-executable)
                  (user-error "Cannot find %s in exec-path" omnidiff-executable)))
         (argv (list bin "--mode" "tui" "--" before-file after-file))
         (buf-name (format "*omnidiff tui: %s*" (or label "diff")))
         (base-clean (format "omnidiff tui: %s" (or label "diff")))
         (default-directory (or dir default-directory))
         (caller-buf (current-buffer))
         (launcher (if (eq omnidiff-tui-launcher 'auto)
                       (cond
                        ((or (fboundp 'eat-make) (require 'eat nil t)) 'eat)
                        ((or (fboundp 'vterm) (require 'vterm nil t)) 'vterm)
                        (t 'ansi-term))
                     omnidiff-tui-launcher))
         (custom-p (and (not (memq launcher '(eat vterm ansi-term)))
                        (functionp launcher)))
         (tui-buf nil)
         (keep-tmpdir nil))
    (unwind-protect
        (progn
          (setq tui-buf
                (cond
                 ((eq launcher 'eat)
                  (unless (fboundp 'eat-make)
                    (require 'eat nil t))
                  (unless (or (fboundp 'eat-make) (fboundp 'eat))
                    (user-error "Package `eat' is not installed"))
                  (cond
                   ((fboundp 'eat-make)
                    (let* ((uniq-name (generate-new-buffer-name buf-name))
                           (clean-name
                            (let ((c (replace-regexp-in-string
                                      "\\`\\*\\(.*?\\)\\*\\(<[0-9]+>\\)?\\'"
                                      "\\1\\2" uniq-name))
                                  (n 2))
                              (while (get-buffer (format "*%s*" c))
                                (setq c (format "%s<%d>" base-clean n)
                                      n (1+ n)))
                              c))
                           (b (apply #'eat-make clean-name (car argv) nil (cdr argv))))
                      (when (buffer-live-p b) (pop-to-buffer b))
                      b))
                   (t
                    (eat (mapconcat #'shell-quote-argument argv " ")))))
                 ((eq launcher 'vterm)
                  (unless (or (fboundp 'vterm) (require 'vterm nil t))
                    (user-error "Package `vterm' is not installed"))
                  (let ((vterm-shell (mapconcat #'shell-quote-argument argv " ")))
                    (vterm (generate-new-buffer-name buf-name))))
                 (custom-p
                  (funcall launcher argv buf-name default-directory))
                 (t
                  (require 'term)
                  (let ((b (generate-new-buffer buf-name)))
                    (with-current-buffer b
                      (setq default-directory (or dir default-directory))
                      (term-mode)
                      (term-exec b buf-name (car argv) nil (cdr argv))
                      (term-char-mode))
                    (pop-to-buffer b)
                    b))))
          (when tmpdir
            (cond
             ((buffer-live-p tui-buf)
              (with-current-buffer tui-buf
                (push tmpdir omnidiff--tui-tmpdirs)
                (add-hook 'kill-buffer-hook #'omnidiff--cleanup-tui-tmpdirs nil t))
              (setq keep-tmpdir t))
             (custom-p
              ;; Custom launcher returned a non-buffer (e.g. external terminal):
              ;; retain tmpdir until calling buffer teardown or Emacs exit.
              (when (buffer-live-p caller-buf)
                (with-current-buffer caller-buf
                  (push tmpdir omnidiff--tui-tmpdirs)
                  (add-hook 'kill-buffer-hook #'omnidiff--cleanup-tui-tmpdirs nil t)))
              (push tmpdir omnidiff--ext-tui-tmpdirs)
              (add-hook 'kill-emacs-hook #'omnidiff--cleanup-ext-tui-tmpdirs)
              (setq keep-tmpdir t))))
          tui-buf)
      (when (and tmpdir (not keep-tmpdir))
        (when (file-directory-p tmpdir)
          (ignore-errors (delete-directory tmpdir t)))))))

(defun omnidiff--launch-tui-for-file (file repo-dir &optional cmd)
  "Materialize FILE sides and launch interactive `omnidiff' TUI."
  (let* ((tmpdir (make-temp-file "omnidiff-tui-" t))
         (sides (condition-case err
                    (omnidiff--materialize-file-sides file repo-dir tmpdir cmd)
                  (error
                   (ignore-errors (delete-directory tmpdir t))
                   (signal (car err) (cdr err))))))
    (omnidiff--launch-tui-pair (car sides) (cdr sides)
                               (omnidiff-file-target-path file)
                               (or repo-dir default-directory)
                               tmpdir)))

(defun omnidiff--launch-tui-for-session (session)
  "Launch interactive `omnidiff' TUI for active Dual SESSION."
  (let* ((hint (or (omnidiff-dual-session-display-path session)
                   (omnidiff-dual-session-after-path session)
                   (omnidiff-dual-session-before-path session)
                   "diff"))
         (tmpdir (make-temp-file "omnidiff-tui-" t))
         (snap (condition-case err
                   (omnidiff-dual-session-snapshot-paths session tmpdir)
                 (error
                  (when (file-directory-p tmpdir)
                    (ignore-errors (delete-directory tmpdir t)))
                  (signal (car err) (cdr err)))))
         (before-file (nth 0 snap))
         (after-file (nth 1 snap))
         (temps (nth 2 snap)))
    (unless temps
      (when (file-directory-p tmpdir)
        (ignore-errors (delete-directory tmpdir t)))
      (setq tmpdir nil))
    (omnidiff--launch-tui-pair before-file after-file hint default-directory tmpdir)))

(defun omnidiff-open-in-tui ()
  "Open the file at point in the interactive `omnidiff' terminal UI."
  (interactive)
  (let ((file (omnidiff--file-at-point)))
    (unless (and file (not (string-empty-p (omnidiff-file-target-path file))))
      (user-error "No file at point to open in omnidiff TUI"))
    (omnidiff--launch-tui-for-file file
                                   (or omnidiff--repo-dir default-directory)
                                   omnidiff--command)))

(defun omnidiff--maybe-refresh-magit (&optional origin-buf)
  "Softly refresh a Magit buffer after worktree edits when Magit is loaded."
  (when (fboundp 'magit-refresh)
    (let ((magit-buf
           (cond
            ((derived-mode-p 'magit-mode) (current-buffer))
            ((and (buffer-live-p origin-buf)
                  (with-current-buffer origin-buf
                    (derived-mode-p 'magit-mode)))
             origin-buf))))
      (when magit-buf
        (with-current-buffer magit-buf
          (ignore-errors (magit-refresh)))))))

(cl-defun omnidiff--open-dual-for-overview-file (overview-buf file
                                                              &key reuse-session
                                                              target-line target-side)
  "Open or update Dual viewer for FILE belonging to OVERVIEW-BUF.
Uses cached JSON diff and sources (zero subprocesses) when available, and
materializes blobs on demand for legacy headless entries.  Returns the
active Dual session."
  (let* ((jd-existing (omnidiff-file-json-diff file))
         (pic (omnidiff-engine-file-picture file))
         (cont (omnidiff-engine-file-container file))
         (target (omnidiff-file-target-path file)))
    (cond
     ((omnidiff-file-too-large-p file)
      (user-error "File exceeds blob size limit; press T to view in native omnidiff TUI"))
     (cont
      (user-error "Cannot open container %s in Dual (use RET on a member or T for TUI)" target))
     ((and pic (not (omnidiff-picture-svg-p pic)))
      (user-error "Cannot open image %s in Dual (use image keys or T for TUI)" target))
     ((not (omnidiff--file-text-dual-p file))
      (user-error "Cannot open binary file %s in Dual" target)))
    (let* ((diff (buffer-local-value 'omnidiff--diff overview-buf))
           (repo-dir (or (buffer-local-value 'omnidiff--repo-dir overview-buf) default-directory))
           (cmd (buffer-local-value 'omnidiff--command overview-buf))
           (preset (with-current-buffer overview-buf (omnidiff-git-effective-preset)))
           (whole (with-current-buffer overview-buf (omnidiff-git-effective-whole-updates)))
           (reindent (with-current-buffer overview-buf (omnidiff-git-effective-paint-reindent-moves)))
           (text-files (when diff
                         (cl-remove-if-not #'omnidiff--file-text-dual-p
                                           (omnidiff-diff-files diff))))
           (idx (or (cl-position file text-files :test #'eq) 0))
           (total (max 1 (length text-files)))
           (status (omnidiff-file-status file))
           (path (omnidiff-file-path file))
           (wt-after (omnidiff--worktree-after-path file repo-dir cmd))
           (existing-after-buf (when wt-after (find-buffer-visiting wt-after)))
           (after-dirty-p (and existing-after-buf (buffer-modified-p existing-after-buf)))
           (before-disk (omnidiff-file-before-disk-path file))
           (after-disk (or wt-after (omnidiff-file-after-disk-path file)))
           (json-diff jd-existing)
           (before-src (omnidiff-file-before-source file))
           (after-src (omnidiff-file-after-source file)))
      ;; Materialize & run on demand if cached JSON/sources are absent or live buffer is dirty.
      (when (or (null json-diff)
                after-dirty-p
                (and (not (eq status 'added)) (not (stringp before-src)))
                (and (not (eq status 'deleted)) (not wt-after) (not (stringp after-src))))
        (let ((tmpdir (make-temp-file "omnidiff-dual-mat-" t)))
          (unwind-protect
              (let* ((sides (omnidiff--materialize-file-sides file repo-dir tmpdir cmd))
                     (b-file (car sides))
                     (a-file (if after-dirty-p
                                 (omnidiff-engine-write-temp-for-buffer
                                  existing-after-buf tmpdir target)
                               (cdr sides)))
                     (computed (omnidiff-engine-run-pair
                                b-file a-file preset nil whole reindent))
                     (b-enc (omnidiff-json-side-encoding (omnidiff-json-diff-before computed)))
                     (a-enc (omnidiff-json-side-encoding (omnidiff-json-diff-after computed))))
                (setq json-diff computed
                      before-src (if (string= b-file "/dev/null")
                                     ""
                                   (omnidiff-engine-read-file-decoded b-file b-enc))
                      after-src (if (string= a-file "/dev/null")
                                    ""
                                  (omnidiff-engine-read-file-decoded a-file a-enc)))
                (unless after-dirty-p
                  (setf (omnidiff-file-json-diff file) json-diff
                        (omnidiff-file-before-source file) before-src
                        (omnidiff-file-after-source file) after-src)))
            (when (file-directory-p tmpdir)
              (ignore-errors (delete-directory tmpdir t))))))
      (let ((nav-callbacks
             (list
              :file-index (1+ idx)
              :file-total total
              :next-file
              (lambda (sess)
                (cond
                 ((not (buffer-live-p overview-buf))
                  (message "Overview buffer is no longer open"))
                 ((<= total 1)
                  (message "Only one text file in diff"))
                 (t
                  (let ((next-file (nth (mod (1+ idx) total) text-files)))
                    (omnidiff--open-dual-for-overview-file
                     overview-buf next-file :reuse-session sess)))))
              :prev-file
              (lambda (sess)
                (cond
                 ((not (buffer-live-p overview-buf))
                  (message "Overview buffer is no longer open"))
                 ((<= total 1)
                  (message "Only one text file in diff"))
                 (t
                  (let ((prev-file (nth (mod (1- idx) total) text-files)))
                    (omnidiff--open-dual-for-overview-file
                     overview-buf prev-file :reuse-session sess)))))
              :open-overview
              (lambda (sess)
                (when (omnidiff-dual-session-p sess)
                  (omnidiff-dual-close sess t))
                (if (buffer-live-p overview-buf)
                    (progn
                      (pop-to-buffer overview-buf)
                      (with-current-buffer overview-buf
                        (omnidiff--restore-location file nil nil nil nil)))
                  (message "Overview buffer is no longer open")))
              :reviewed-p
              (lambda ()
                (and (buffer-live-p overview-buf)
                     (with-current-buffer overview-buf
                       (omnidiff--file-reviewed-p target))))
              :review-progress
              (lambda ()
                (when (buffer-live-p overview-buf)
                  (with-current-buffer overview-buf
                    (cons (omnidiff-render-reviewed-count) omnidiff--file-count))))
              :toggle-reviewed
              (lambda ()
                (if (not (buffer-live-p overview-buf))
                    (user-error "Overview buffer is no longer open")
                  (with-current-buffer overview-buf
                    (let ((new-state (not (omnidiff--file-reviewed-p target))))
                      (omnidiff--set-file-reviewed file new-state)))))
              :next-unreviewed
              (lambda (sess)
                (if (not (buffer-live-p overview-buf))
                    (progn
                      (message "Overview buffer is no longer open")
                      nil)
                  (let ((next-file
                         (with-current-buffer overview-buf
                           (let ((cands (or (and omnidiff--diff
                                                 (cl-remove-if-not
                                                  #'omnidiff--file-text-dual-p
                                                  (omnidiff-diff-files omnidiff--diff)))
                                            text-files)))
                             (omnidiff--next-unreviewed-file cands file)))))
                    (when next-file
                      (omnidiff--open-dual-for-overview-file
                       overview-buf next-file :reuse-session sess)
                      next-file))))
              :open-tui
              (lambda (sess)
                (let ((bb (and (omnidiff-dual-session-p sess)
                               (omnidiff-dual-session-before-buf sess)))
                      (ab (and (omnidiff-dual-session-p sess)
                               (omnidiff-dual-session-after-buf sess))))
                  (if (and (buffer-live-p overview-buf)
                           (not (and (buffer-live-p bb) (buffer-modified-p bb)))
                           (not (and (buffer-live-p ab) (buffer-modified-p ab))))
                      (omnidiff--launch-tui-for-file file repo-dir cmd)
                    (omnidiff--launch-tui-for-session sess))))
              :on-quit
              (lambda (sess)
                (when (and (omnidiff-dual-session-p sess)
                           (omnidiff-dual-session-mutated-p sess))
                  (omnidiff--maybe-refresh-magit
                   (and (buffer-live-p overview-buf)
                        (buffer-local-value 'omnidiff--origin-buffer overview-buf))))))))
        (omnidiff-dual-open
         :before-path (if (eq status 'added) "/dev/null" before-disk)
         :after-path (if (eq status 'deleted) "/dev/null" after-disk)
         :before-source (unless (eq status 'added) (or before-src ""))
         :after-source (unless (eq status 'deleted) (or after-src ""))
         :after-buffer (when after-dirty-p existing-after-buf)
         :before-label (cond
                        ((eq status 'added) "/dev/null")
                        ((eq status 'renamed) path)
                        (t target))
         :after-label (if (eq status 'deleted) "/dev/null" target)
         :display-path target
         :json-diff json-diff
         :preset preset
         :whole-updates whole
         :paint-reindent-moves reindent
         :editable-after (and wt-after t)
         :initial-side target-side
         :initial-line (when (and (integerp target-line) (> target-line 0))
                         target-line)
         :nav-callbacks nav-callbacks
         :reuse-session reuse-session)))))

(defun omnidiff-open-dual-at-point ()
  "Open the Dual semantic source viewer for the file at point in Overview."
  (interactive)
  (let ((file (omnidiff--file-at-point)))
    (unless file
      (user-error "No file at point"))
    (let* ((pos (point))
           (on-header (get-text-property pos 'omnidiff-file-header))
           (saved-side (unless on-header (get-text-property pos 'omnidiff-side)))
           (saved-old (unless on-header (get-text-property pos 'omnidiff-old-line)))
           (saved-new (unless on-header (get-text-property pos 'omnidiff-new-line)))
           (saved-line (unless on-header (get-text-property pos 'omnidiff-line)))
           (status (omnidiff-file-status file))
           (target-side (cond
                         ((eq status 'deleted) 'before)
                         ((eq status 'added) 'after)
                         ((and (eq saved-side 'before) (integerp saved-old)) 'before)
                         ((integerp (or saved-new saved-line)) 'after)
                         ((integerp saved-old) 'before)))
           (target-line (if (eq target-side 'before)
                            (or saved-old saved-line)
                          (or saved-new saved-line))))
      (omnidiff--open-dual-for-overview-file
       (current-buffer) file
       :target-line target-line
       :target-side target-side))))

(defun omnidiff-finalize-git-render (buffer diff saved-loc entry-policy
                                            &optional target-file target-line
                                            target-side final-msg)
  "Finalize rendering DIFF in Overview BUFFER after an async Git request.
SAVED-LOC is either nil or `(FILE OLD-LN NEW-LN SIDE FALLBACK-LN)' captured
when preserving buffer state across a refresh.  ENTRY-POLICY is nil, `auto',
`overview', or `dual'.  Optional TARGET-FILE, TARGET-LINE, and TARGET-SIDE
focus a specific file/line (e.g. from Magit).  Optional FINAL-MSG clears the
matching progress message from the echo area after rendering."
  ;; Display BUFFER first so SBS metrics measure its actual target window.
  (pop-to-buffer buffer)
  (with-current-buffer buffer
    (let ((kept-reviewed (omnidiff--sync-reviewed-for-diff diff)))
      (if kept-reviewed
          (omnidiff-render-buffer diff kept-reviewed)
        (omnidiff-render-buffer diff)))
    (when (car saved-loc)
      (pcase-let ((`(,saved-file ,saved-old ,saved-new ,saved-side ,saved-line)
                   saved-loc))
        (omnidiff--restore-location
         saved-file saved-old saved-new saved-side saved-line)))
    (run-hooks 'omnidiff-after-diff-hook))
  (when (and final-msg (equal (current-message) final-msg))
    (message nil))
  (let* ((files (omnidiff-diff-files diff))
         (matched
          (when target-file
            (cl-find-if (lambda (f)
                          (or (equal (omnidiff-file-target-path f) target-file)
                              (equal (omnidiff-file-path f) target-file)))
                        files)))
         (dual-target
          (when entry-policy
            (cond
             ((and matched
                   (omnidiff--file-text-dual-p matched)
                   (or (eq entry-policy 'dual)
                       (and (eq entry-policy 'auto)
                            (eq omnidiff-magit-file-entry 'dual))))
              matched)
             ((eq entry-policy 'dual)
              (or (cl-find-if
                   (lambda (f)
                     (and (omnidiff--file-text-dual-p f)
                          (not (omnidiff-render-trivial-summary-p
                                (omnidiff-engine-file-summary f)))))
                   files)
                  (cl-find-if #'omnidiff--file-text-dual-p files)))
             ((and (eq entry-policy 'auto)
                   (= (length files) 1)
                   (omnidiff--file-text-dual-p (car files))
                   (omnidiff-file-json-diff (car files)))
              (car files))))))
    (when (and matched (null saved-loc))
      (with-current-buffer buffer
        (when-let* ((hdr (omnidiff--find-file-header-pos matched)))
          (goto-char hdr)
          ;; When staying in Overview (e.g. image/container or overview policy),
          ;; expand the targeted file card so it is immediately visible.
          (unless dual-target
            (when-let* ((ov (get-text-property hdr 'omnidiff-body-ov)))
              (omnidiff--set-overlay-folded
               ov nil 'omnidiff-fold 'omnidiff-file-accent))))))
    (when dual-target
      (omnidiff--open-dual-for-overview-file
       buffer dual-target
       :target-line (when (eq dual-target matched) target-line)
       :target-side (when (eq dual-target matched) target-side)))))

(defun omnidiff--open-standalone-overview (file label dir &optional owned-tmpdir)
  "Render standalone `omnidiff-file' FILE in a dedicated `omnidiff-mode' buffer."
  (let* ((buf-name (format "*omnidiff: %s*" label))
         (buf (get-buffer-create buf-name))
         (diff (make-omnidiff-diff :files (list file))))
    (with-current-buffer buf
      (unless (derived-mode-p 'omnidiff-mode)
        (omnidiff-mode))
      (omnidiff-git-cancel-request)
      (omnidiff-image-cleanup)
      (omnidiff--cleanup-owned-tmpdirs)
      (when owned-tmpdir
        (push owned-tmpdir omnidiff--owned-tmpdirs))
      (setq default-directory (or dir default-directory)
            omnidiff--command nil
            omnidiff--repo-dir (or dir default-directory)
            omnidiff--label label
            omnidiff--preset omnidiff-default-preset
            omnidiff--layout (or omnidiff--layout omnidiff-default-layout)
            omnidiff--diff diff
            omnidiff--reviewed-files nil))
    (pop-to-buffer buf)
    (with-current-buffer buf
      (omnidiff-render-buffer diff))
    buf))

(cl-defun omnidiff--open-pair-result (before-abs after-abs json-diff
                                                 &key display-path before-label after-label
                                                 dir preset
                                                 (whole-updates omnidiff-whole-updates)
                                                 (paint-reindent-moves omnidiff-paint-reindent-moves)
                                                 editable-after owned-tmpdir)
  "Dispatch JSON-DIFF for BEFORE-ABS and AFTER-ABS to the best native viewer.
Pictures open in `omnidiff-image' view, containers and binaries open in
Overview, and text/code opens directly in `omnidiff-dual-open'.
OWNED-TMPDIR, when non-nil, is retained on Overview/image buffers until killed
and deleted immediately once text sources are decoded for Dual."
  (let* ((eff-preset (or preset omnidiff-default-preset))
         (eff-whole (and whole-updates t))
         (eff-reindent (and paint-reindent-moves t))
         (disp (or display-path
                   (if (and (not (string= before-abs "/dev/null"))
                            (not (string= after-abs "/dev/null")))
                       (format "%s vs %s"
                               (file-name-nondirectory before-abs)
                               (file-name-nondirectory after-abs))
                     (file-name-nondirectory
                      (if (string= before-abs "/dev/null") after-abs before-abs)))))
         (pic (omnidiff-json-diff-picture json-diff))
         (cont (omnidiff-json-diff-container json-diff))
         (bin (omnidiff-json-diff-binary-p json-diff))
         (b-disk (unless (string= before-abs "/dev/null") before-abs))
         (a-disk (unless (string= after-abs "/dev/null") after-abs)))
    (cond
     (pic
      (let ((file (omnidiff-image-make-file-from-paths before-abs after-abs pic disp)))
        (omnidiff--open-standalone-overview file disp dir owned-tmpdir)))
     ((or cont bin)
      (let ((file (omnidiff-git-file-from-json
                   disp before-abs after-abs disp json-diff
                   nil nil nil nil b-disk a-disk)))
        (omnidiff--open-standalone-overview file disp dir owned-tmpdir)))
     (t
      (let* ((b-enc (omnidiff-json-side-encoding (omnidiff-json-diff-before json-diff)))
             (a-enc (omnidiff-json-side-encoding (omnidiff-json-diff-after json-diff)))
             (b-src (unless (string= before-abs "/dev/null")
                      (omnidiff-engine-read-file-decoded before-abs b-enc)))
             (a-src (unless (string= after-abs "/dev/null")
                      (omnidiff-engine-read-file-decoded after-abs a-enc)))
             (can-edit (and editable-after
                            (null owned-tmpdir)
                            (not (string= after-abs "/dev/null"))
                            (not (file-symlink-p after-abs))
                            (file-regular-p after-abs)
                            (file-writable-p after-abs)))
             (session
              (omnidiff-dual-open
               :before-path (if owned-tmpdir
                                (when (string= before-abs "/dev/null") "/dev/null")
                              before-abs)
               :after-path (if owned-tmpdir
                               (when (string= after-abs "/dev/null") "/dev/null")
                             after-abs)
               :before-source b-src
               :after-source a-src
               :before-label (or before-label
                                 (if (string= before-abs "/dev/null")
                                     "/dev/null"
                                   (file-name-nondirectory before-abs)))
               :after-label (or after-label
                                (if (string= after-abs "/dev/null")
                                    "/dev/null"
                                  (file-name-nondirectory after-abs)))
               :display-path disp
               :json-diff json-diff
               :preset eff-preset
               :whole-updates eff-whole
               :paint-reindent-moves eff-reindent
               :editable-after can-edit
               :nav-callbacks
               (list :open-tui (lambda (sess)
                                 (omnidiff--launch-tui-for-session sess))))))
        (when (and owned-tmpdir (file-directory-p owned-tmpdir))
          (ignore-errors (delete-directory owned-tmpdir t)))
        session)))))

(defconst omnidiff--archive-extract-py
  "import bz2, gzip, lzma, os, sys, tarfile, zipfile

def norm_name(s):
    while s.startswith('./'):
        s = s[2:]
    return s.lstrip('/')

def copy_bounded(fp, dst, max_bytes):
    written = 0
    exceeded = False
    try:
        with open(dst, 'wb') as out:
            while True:
                chunk = fp.read(65536)
                if not chunk:
                    break
                written += len(chunk)
                if max_bytes > 0 and written > max_bytes:
                    exceeded = True
                    break
                out.write(chunk)
    except Exception:
        try:
            os.unlink(dst)
        except OSError:
            pass
        raise
    if exceeded:
        try:
            os.unlink(dst)
        except OSError:
            pass
        sys.exit(3)

def extract_one(src, key, dst, max_bytes):
    if not src or src == '/dev/null' or not os.path.isfile(src):
        return False
    if zipfile.is_zipfile(src):
        with zipfile.ZipFile(src, 'r') as zf:
            infos = zf.infolist()
            info = None
            for cand in infos:
                if cand.filename == key:
                    info = cand
                    break
            if info is None:
                norm = norm_name(key)
                for cand in infos:
                    if norm_name(cand.filename) == norm:
                        info = cand
                        break
            if info is None or info.is_dir():
                return False
            if max_bytes > 0 and info.file_size > max_bytes:
                sys.exit(3)
            with zf.open(info, 'r') as fp:
                copy_bounded(fp, dst, max_bytes)
            return True
    try:
        if tarfile.is_tarfile(src):
            with tarfile.open(src, 'r:*') as tf:
                members = tf.getmembers()
                member = None
                for m in members:
                    if m.name == key:
                        member = m
                        break
                if member is None:
                    norm = norm_name(key)
                    for m in members:
                        if norm_name(m.name) == norm:
                            member = m
                            break
                if member is None or not member.isfile():
                    return False
                if max_bytes > 0 and member.size > max_bytes:
                    sys.exit(3)
                fp = tf.extractfile(member)
                if fp is None:
                    return False
                with fp:
                    copy_bounded(fp, dst, max_bytes)
                return True
    except tarfile.TarError:
        pass
    with open(src, 'rb') as fp:
        head = fp.read(6)
    opener = None
    if head.startswith(b'\\x1f\\x8b'):
        opener = gzip.open
    elif head.startswith(b'BZh'):
        opener = bz2.open
    elif head.startswith(b'\\xfd7zXZ\\x00') or src.endswith(('.xz', '.lzma')):
        opener = lzma.open
    if opener is not None:
        with opener(src, 'rb') as fp:
            copy_bounded(fp, dst, max_bytes)
        return True
    return False

b_src, a_src, key, b_dst, a_dst = sys.argv[1:6]
max_bytes = int(sys.argv[6]) if len(sys.argv) > 6 else 0
extract_one(b_src, key, b_dst, max_bytes)
extract_one(a_src, key, a_dst, max_bytes)
"
  "Python 3 script extracting one archive member from BEFORE and AFTER files.")

(defun omnidiff--drilldown-archive-member (file key repo-dir cmd)
  "Extract archive member KEY from FILE and open the appropriate diff viewer."
  (when (or (not (stringp key)) (string-search "\x00" key))
    (user-error "Invalid archive member key"))
  (let ((python (or (executable-find "python3")
                    (user-error "python3 is required to inspect archive members (or press T for omnidiff TUI)")))
        (tmpdir (make-temp-file "omnidiff-archive-" t))
        (max-bytes (if (and (integerp omnidiff-archive-extract-max-bytes)
                            (>= omnidiff-archive-extract-max-bytes 0))
                       omnidiff-archive-extract-max-bytes
                     (* 64 1024 1024)))
        (transferred nil))
    (unwind-protect
        (let* ((sides (omnidiff--materialize-file-sides file repo-dir tmpdir cmd))
               (b-arch (car sides))
               (a-arch (cdr sides))
               (hint (if (string= key "(content)")
                         (file-name-sans-extension
                          (file-name-nondirectory (omnidiff-file-target-path file)))
                       key))
               (raw-base (omnidiff-engine-sanitize-filename-hint hint))
               (clean-base (replace-regexp-in-string "[\\\\/\x00]" "_" raw-base))
               (base (cond
                      ((or (string-empty-p clean-base)
                           (string= clean-base ".")
                           (string= clean-base ".."))
                       "member")
                      ((string-prefix-p "~" clean-base)
                       (concat "_" (substring clean-base 1)))
                      (t clean-base)))
               (b-dir (expand-file-name "before" tmpdir))
               (a-dir (expand-file-name "after" tmpdir))
               (b-out (expand-file-name (concat "./" base) b-dir))
               (a-out (expand-file-name (concat "./" base) a-dir)))
          (make-directory b-dir t)
          (make-directory a-dir t)
          (unless (and (string-prefix-p (file-name-as-directory b-dir) b-out)
                       (string-prefix-p (file-name-as-directory a-dir) a-out)
                       (file-in-directory-p b-out b-dir)
                       (file-in-directory-p a-out a-dir))
            (user-error "Refusing unsafe archive member path `%s'" key))
          (let ((rc (call-process python nil nil nil
                                  "-c" omnidiff--archive-extract-py
                                  b-arch a-arch key b-out a-out
                                  (number-to-string max-bytes))))
            (cond
             ((and (integerp rc) (= rc 3))
              (user-error
               "Archive member `%s' exceeds extraction size limit (%s; see `omnidiff-archive-extract-max-bytes') — press T for native omnidiff TUI"
               key (file-size-human-readable max-bytes)))
             ((not (and (integerp rc) (zerop rc)))
              (user-error "Failed to extract archive member `%s'" key))))
          (let ((eff-b (if (file-exists-p b-out) b-out "/dev/null"))
                (eff-a (if (file-exists-p a-out) a-out "/dev/null")))
            (when (and (string= eff-b "/dev/null") (string= eff-a "/dev/null"))
              (user-error "Could not extract member `%s' from archive" key))
            (let* ((preset (omnidiff-git-effective-preset))
                   (whole (omnidiff-git-effective-whole-updates))
                   (reindent (omnidiff-git-effective-paint-reindent-moves))
                   (jdiff (omnidiff-engine-run-pair eff-b eff-a preset nil whole reindent))
                   (disp (format "%s :: %s" (omnidiff-file-target-path file) key))
                   (res (omnidiff--open-pair-result
                         eff-b eff-a jdiff
                         :display-path disp
                         :before-label (if (string= eff-b "/dev/null") "/dev/null" (format "%s (before)" key))
                         :after-label (if (string= eff-a "/dev/null") "/dev/null" (format "%s (after)" key))
                         :dir repo-dir
                         :preset preset
                         :whole-updates whole
                         :paint-reindent-moves reindent
                         :editable-after nil
                         :owned-tmpdir tmpdir)))
              (setq transferred t)
              res)))
      (when (and (not transferred) (file-directory-p tmpdir))
        (ignore-errors (delete-directory tmpdir t))))))

(defun omnidiff--drilldown-pdf-page (file key repo-dir cmd)
  "Rasterize PDF page identified by KEY in FILE and open visual image diff."
  (let* ((cont (omnidiff-engine-file-container file))
         (fmts (delq nil
                     (mapcar (lambda (side)
                               (when (omnidiff-json-container-side-p side)
                                 (let ((f (string-trim
                                           (or (omnidiff-json-container-side-format side) ""))))
                                   (unless (string-empty-p f) f))))
                             (when cont
                               (list (omnidiff-json-container-before cont)
                                     (omnidiff-json-container-after cont))))))
         (pdf-p (or (null fmts)
                    (cl-every (lambda (f)
                                (let ((lf (downcase f)))
                                  (or (string= lf "pdf")
                                      (string-prefix-p "pdf " lf)
                                      (string-prefix-p "pdf/" lf))))
                              fmts)))
         (page-num (when (and pdf-p (stringp key))
                     (let ((case-fold-search t))
                       (when (string-match "\\`\\(?:page[[:space:]_-]+\\)?\\([0-9]+\\)\\'" key)
                         (string-to-number (match-string 1 key))))))
         (pdftoppm (executable-find "pdftoppm"))
         (mutool (executable-find "mutool")))
    (cond
     ((not pdf-p)
      (message "Document member `%s' — press T to inspect in native omnidiff TUI" key))
     ((not (and (integerp page-num) (> page-num 0)))
      (message "PDF member `%s' — press T to inspect in native omnidiff TUI" key))
     ((not (or pdftoppm mutool))
      (message "PDF page preview requires `pdftoppm' (poppler) or `mutool' (mupdf); press T for native omnidiff TUI"))
     (t
      (let ((tmpdir (make-temp-file "omnidiff-pdf-" t))
            (page-str (number-to-string page-num))
            (transferred nil))
        (unwind-protect
            (let* ((sides (omnidiff--materialize-file-sides file repo-dir tmpdir cmd))
                   (b-pdf (car sides))
                   (a-pdf (cdr sides))
                   (b-png (expand-file-name "before.png" tmpdir))
                   (a-png (expand-file-name "after.png" tmpdir)))
              (cl-flet ((rasterize (pdf out-png prefix)
                          (when (and (stringp pdf)
                                     (not (string= pdf "/dev/null"))
                                     (file-exists-p pdf))
                            (let* ((abs-pdf (expand-file-name
                                             (if (string-prefix-p "~" pdf)
                                                 (concat "./" pdf)
                                               pdf)))
                                   (rc (if pdftoppm
                                           (call-process pdftoppm nil nil nil
                                                         "-png" "-r" "150"
                                                         "-f" page-str "-l" page-str
                                                         "-singlefile" abs-pdf prefix)
                                         (call-process mutool nil nil nil
                                                       "draw" "-F" "png" "-r" "150"
                                                       "-o" out-png abs-pdf page-str)))
                                   (attrs (and (integerp rc) (zerop rc)
                                               (file-attributes out-png)))
                                   (sz (and attrs (file-attribute-size attrs))))
                              (unless (and (integerp sz) (> sz 0))
                                (when (file-exists-p out-png)
                                  (ignore-errors (delete-file out-png))))))))
                (rasterize b-pdf b-png (expand-file-name "before" tmpdir))
                (rasterize a-pdf a-png (expand-file-name "after" tmpdir)))
              (let ((eff-b (if (and (file-exists-p b-png)
                                    (> (or (file-attribute-size (file-attributes b-png)) 0) 0))
                               b-png
                             "/dev/null"))
                    (eff-a (if (and (file-exists-p a-png)
                                    (> (or (file-attribute-size (file-attributes a-png)) 0) 0))
                               a-png
                             "/dev/null")))
                (when (and (string= eff-b "/dev/null") (string= eff-a "/dev/null"))
                  (user-error "Could not rasterize PDF page %d" page-num))
                (let* ((jdiff (omnidiff-engine-run-pair eff-b eff-a))
                       (disp (format "%s (page %d)" (omnidiff-file-target-path file) page-num))
                       (img-file (omnidiff-image-make-file-from-paths
                                  eff-b eff-a (omnidiff-json-diff-picture jdiff) disp))
                       (buf (omnidiff--open-standalone-overview img-file disp repo-dir tmpdir)))
                  (setq transferred t)
                  buf)))
          (when (and (not transferred) (file-directory-p tmpdir))
            (ignore-errors (delete-directory tmpdir t)))))))))

(defun omnidiff--drilldown-container-member (file member)
  "Inspect container MEMBER of FILE when pressing `RET' in Overview."
  (let* ((cont (omnidiff-engine-file-container file))
         (family (and cont (omnidiff-json-container-family cont)))
         (key (omnidiff-json-container-member-key member))
         (repo-dir (or omnidiff--repo-dir default-directory))
         (cmd omnidiff--command))
    (pcase family
      ('archives
       (omnidiff--drilldown-archive-member file key repo-dir cmd))
      ('documents
       (omnidiff--drilldown-pdf-page file key repo-dir cmd))
      (_
       (let ((detail-str (omnidiff-render-format-container-detail
                          (omnidiff-json-container-member-detail-kind member)
                          (omnidiff-json-container-member-detail member))))
         (message "%s member `%s' (%s)%s — press T to inspect visually in native omnidiff TUI"
                  (omnidiff-render-container-family-label family)
                  key
                  (omnidiff-json-container-member-status member)
                  (if (and detail-str (not (string-empty-p detail-str)))
                      (concat ": " detail-str)
                    "")))))))

;;; Major mode

(defvar-keymap omnidiff-mode-map
  :parent special-mode-map
  "d" #'omnidiff-open-dual-at-point
  "n" #'omnidiff-next-hunk
  "p" #'omnidiff-prev-hunk
  "]" #'omnidiff-next-file
  "[" #'omnidiff-prev-file
  "N" #'omnidiff-next-file
  "P" #'omnidiff-prev-file
  "TAB" #'omnidiff-toggle-fold
  "<backtab>" #'omnidiff-toggle-fold-all
  "F" #'omnidiff-toggle-fold-trivial
  "R" #'omnidiff-toggle-reviewed
  "RET" #'omnidiff-jump-to-source
  "v" #'omnidiff-cycle-layout
  "H" #'omnidiff-cycle-highlight-style
  "m" #'omnidiff-cycle-preset
  "W" #'omnidiff-toggle-whole-updates
  "I" #'omnidiff-toggle-paint-reindent-moves
  "c" #'omnidiff-cycle-color-scheme
  "T" #'omnidiff-open-in-tui
  "w" #'omnidiff-toggle-wrap
  "g" #'omnidiff-refresh
  "?" #'omnidiff-help)

;;;###autoload
(define-derived-mode omnidiff-mode special-mode "Omnidiff"
  "Major mode for viewing styled content-aware diffs from `omnidiff'."
  (buffer-disable-undo)
  (setq-local truncate-lines t
              line-spacing omnidiff-line-spacing
              cursor-in-non-selected-windows nil
              face-remapping-alist (assq-delete-all 'header-line face-remapping-alist))
  (face-remap-add-relative 'header-line 'omnidiff-header-line)
  (add-to-invisibility-spec 'omnidiff-fold)
  (add-to-invisibility-spec 'omnidiff-hunk-fold)
  (cursor-intangible-mode 1)
  (add-hook 'window-size-change-functions #'omnidiff-image-on-window-resize nil t)
  (add-hook 'window-size-change-functions #'omnidiff--on-window-resize nil t)
  (add-hook 'kill-buffer-hook #'omnidiff--cancel-sbs-resize-timer nil t)
  (add-hook 'change-major-mode-hook #'omnidiff--cancel-sbs-resize-timer nil t)
  (add-hook 'kill-buffer-hook #'omnidiff-git-cancel-request nil t)
  (add-hook 'change-major-mode-hook #'omnidiff-git-cancel-request nil t)
  (add-hook 'kill-buffer-hook #'omnidiff-image-cleanup nil t)
  (add-hook 'change-major-mode-hook #'omnidiff-image-cleanup nil t)
  (add-hook 'kill-buffer-hook #'omnidiff--cleanup-owned-tmpdirs nil t)
  (add-hook 'change-major-mode-hook #'omnidiff--cleanup-owned-tmpdirs nil t))

;;; Git / Magit / standalone interactive commands

(defun omnidiff--read-revision ()
  "Read a Git revision using Magit when available, else prompt with a default."
  (if (and (fboundp 'magit-read-branch-or-commit)
           (fboundp 'magit-thing-at-point)
           (fboundp 'magit-branch-or-commit-at-point))
      (or (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          (magit-read-branch-or-commit "Revision"))
    (let ((rev (read-string "Revision (default HEAD): " nil nil "HEAD")))
      (if (string-empty-p rev) "HEAD" rev))))

(defun omnidiff--magit-diff-files ()
  "Return active Magit diff file filter list when invoked from Magit, else nil."
  (when (and (fboundp 'magit-diff-arguments)
             (or (and (boundp 'transient-current-command)
                      (memq transient-current-command '(magit-diff magit-diff-refresh)))
                 (derived-mode-p 'magit-diff-mode)))
    (let ((files (cadr (magit-diff-arguments))))
      (when (and (listp files) files (cl-every #'stringp files))
        files))))

(defun omnidiff--magit-entry-spec ()
  "Return target plist from the current Magit buffer, or nil outside Magit.
Includes `:origin-buf', plus `:file', `:line', `:side', and `:untracked'
when point sits on a Magit file or hunk section."
  (when (derived-mode-p 'magit-mode)
    (let* ((rel (omnidiff--magit-target-file))
           (untracked
            (and rel
                 (fboundp 'magit-section-match)
                 (ignore-errors (magit-section-match '[file untracked]))))
           (in-hunk
            (and rel
                 (not untracked)
                 (fboundp 'magit-section-match)
                 (fboundp 'magit-current-section)
                 (fboundp 'magit-diff-hunk-line)
                 (ignore-errors (magit-section-match 'hunk))))
           (before-p (and in-hunk (eq (char-after (line-beginning-position)) ?-)))
           (line (when in-hunk
                   (ignore-errors
                     (magit-diff-hunk-line (magit-current-section) before-p)))))
      (list :origin-buf (current-buffer)
            :file rel
            :line (when (and (integerp line) (> line 0)) line)
            :side (when (and (integerp line) (> line 0))
                    (if before-p 'before 'after))
            :untracked (and untracked t)))))

(defun omnidiff--read-diff-arg ()
  "Resolve Git diff range argument via Magit DWIM when available."
  (if (and (fboundp 'magit-diff--dwim)
           (fboundp 'magit-diff-read-range-or-commit))
      (or (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          (pcase (magit-diff--dwim)
            ('unmerged (user-error "Unmerged diffs are not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (cons 'stash value))
            (`(commit . ,value) (cons 'commit value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))
    (when current-prefix-arg
      (let ((range (read-string "Git diff range (empty for unstaged): ")))
        (unless (string-empty-p range) range)))))

(defun omnidiff--files-label-suffix (files)
  "Format compact label suffix for pathspec filter FILES, or empty string."
  (cond
   ((null files) "")
   ((= (length files) 1) (format " (%s)" (car files)))
   (t (format " (%d files)" (length files)))))

;;;###autoload
(defun omnidiff-show (rev &optional files target-spec auto-dual)
  "Show the result of \"git show REV\" with `omnidiff'.
Optional FILES restricts the diff to matching pathspecs.  TARGET-SPEC
carries optional Magit target metadata (`:file', `:line', `:side',
`:origin-buf').  AUTO-DUAL overrides entry policy (default t)."
  (interactive (list (omnidiff--read-revision)
                     (omnidiff--magit-diff-files)
                     (omnidiff--magit-entry-spec)))
  (unless rev
    (user-error "No revision specified"))
  (omnidiff-run
   (get-buffer-create (concat "*git show omnidiff " rev "*"))
   `("git" "--no-pager" "show" "--ext-diff" ,rev ,@(when files (cons "--" files)))
   (concat (format "show %s" rev) (omnidiff--files-label-suffix files))
   nil nil nil nil nil (or auto-dual t) nil nil target-spec))

;;;###autoload
(defun omnidiff-diff (&optional arg files target-spec auto-dual)
  "Show the result of \"git diff ARG\" with `omnidiff'.
Optional FILES restricts the diff to matching pathspecs.  When ARG is
`(commit . REV)', delegates to `omnidiff-show' so root commits work cleanly;
when `(stash . REV)', diffs `REV^' against `REV'.  TARGET-SPEC carries
optional Magit target metadata (`:file', `:line', `:side', `:origin-buf',
`:untracked').  AUTO-DUAL overrides entry policy (default t)."
  (interactive
   (let ((spec (omnidiff--magit-entry-spec)))
     (if (and (not current-prefix-arg) (plist-get spec :untracked))
         (list nil nil spec)
       (list (omnidiff--read-diff-arg) (omnidiff--magit-diff-files) spec))))
  (cond
   ((and (plist-get target-spec :untracked)
         (plist-get target-spec :file))
    (let* ((repo (or (and (fboundp 'magit-toplevel) (magit-toplevel))
                     (omnidiff-git-toplevel)
                     default-directory))
           (full (omnidiff-engine-expand-repo-path (plist-get target-spec :file) repo)))
      (omnidiff-files "/dev/null" full)))
   ((and (consp arg) (eq (car arg) 'commit))
    (omnidiff-show (cdr arg) files target-spec auto-dual))
   ((and (consp arg) (eq (car arg) 'stash))
    (let* ((rev (cdr arg))
           (name (format "*git diff omnidiff %s*" rev)))
      (omnidiff-run
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff"
         ,(concat rev "^") ,rev
         ,@(when files (cons "--" files)))
       (concat rev (omnidiff--files-label-suffix files))
       nil nil nil nil nil (or auto-dual t) nil nil target-spec)))
   (t
    (let ((name (concat "*git diff omnidiff"
                        (if arg (concat " " arg) "")
                        "*")))
      (omnidiff-run
       (get-buffer-create name)
       `("git" "--no-pager" "diff" "--ext-diff"
         ,@(when arg (list arg))
         ,@(when files (cons "--" files)))
       (concat (pcase arg
                 ('nil "unstaged")
                 ("--cached" "staged")
                 (_ arg))
               (omnidiff--files-label-suffix files))
       nil nil nil nil nil (or auto-dual t) nil nil target-spec)))))

;;;###autoload
(defun omnidiff-files (before after)
  "Compare any two files BEFORE and AFTER on disk with `omnidiff'.
Dispatches pictures to the visual image viewer, containers and opaque
binaries to Overview, and text/code directly to the Dual semantic viewer."
  (interactive
   (let* ((default-a (when buffer-file-name
                       (file-name-nondirectory buffer-file-name)))
          (a (read-file-name "Before file: " nil buffer-file-name t default-a))
          (b (read-file-name (format "After file (compare %s with): "
                                     (file-name-nondirectory a))
                             (file-name-directory a) nil t)))
     (list a b)))
  (let* ((before-abs (if (or (null before) (string-empty-p before)
                             (string= before "/dev/null"))
                         "/dev/null"
                       (expand-file-name before)))
         (after-abs (if (or (null after) (string-empty-p after)
                            (string= after "/dev/null"))
                        "/dev/null"
                      (expand-file-name after))))
    (unless (or (string= before-abs "/dev/null")
                (and (file-exists-p before-abs) (not (file-directory-p before-abs))))
      (user-error "Before file does not exist or is a directory: %s" before))
    (unless (or (string= after-abs "/dev/null")
                (and (file-exists-p after-abs) (not (file-directory-p after-abs))))
      (user-error "After file does not exist or is a directory: %s" after))
    (let* ((after-buf (unless (string= after-abs "/dev/null")
                        (find-buffer-visiting after-abs)))
           (after-dirty-p (and (buffer-live-p after-buf)
                               (buffer-modified-p after-buf)))
           (snap-tmpdir (when after-dirty-p
                          (make-temp-file "omnidiff-files-" t)))
           (transferred nil))
      (unwind-protect
          (let* ((after-diff-path (if after-dirty-p
                                      (omnidiff-engine-write-temp-for-buffer
                                       after-buf snap-tmpdir after-abs)
                                    after-abs))
                 (jdiff (omnidiff-engine-run-pair
                         before-abs after-diff-path
                         omnidiff-default-preset nil
                         omnidiff-whole-updates omnidiff-paint-reindent-moves))
                 (b-base (if (string= before-abs "/dev/null")
                             "/dev/null"
                           (file-name-nondirectory before-abs)))
                 (a-base (if (string= after-abs "/dev/null")
                             "/dev/null"
                           (file-name-nondirectory after-abs)))
                 (same-base (and (not (string= before-abs "/dev/null"))
                                 (not (string= after-abs "/dev/null"))
                                 (string= b-base a-base)))
                 (b-lbl (if same-base (abbreviate-file-name before-abs) b-base))
                 (a-lbl (if same-base (abbreviate-file-name after-abs) a-base))
                 (disp (cond
                        ((string= before-abs "/dev/null") a-base)
                        ((string= after-abs "/dev/null") b-base)
                        (same-base a-base)
                        (t (format "%s vs %s" b-base a-base))))
                 (dir (file-name-directory
                       (if (string= after-abs "/dev/null") before-abs after-abs))))
            (if (and after-dirty-p
                     (not (omnidiff-json-diff-picture jdiff))
                     (not (omnidiff-json-diff-container jdiff))
                     (not (omnidiff-json-diff-binary-p jdiff)))
                ;; Live buffer attaches directly to Dual; `snap-tmpdir' is cleaned
                ;; immediately by `unwind-protect' once `omnidiff-dual-open' returns.
                (let* ((b-enc (omnidiff-json-side-encoding
                               (omnidiff-json-diff-before jdiff)))
                       (b-src (unless (string= before-abs "/dev/null")
                                (omnidiff-engine-read-file-decoded
                                 before-abs b-enc)))
                       (can-edit (and (not (file-symlink-p after-abs))
                                      (file-regular-p after-abs)
                                      (file-writable-p after-abs))))
                  (omnidiff-dual-open
                   :before-path before-abs
                   :after-path after-abs
                   :before-source b-src
                   :after-buffer after-buf
                   :before-label b-lbl
                   :after-label a-lbl
                   :display-path disp
                   :json-diff jdiff
                   :preset omnidiff-default-preset
                   :whole-updates omnidiff-whole-updates
                   :paint-reindent-moves omnidiff-paint-reindent-moves
                   :editable-after can-edit
                   :nav-callbacks
                   (list
                    :open-tui (lambda (sess)
                                (omnidiff--launch-tui-for-session sess)))))
              (let ((res (omnidiff--open-pair-result
                          before-abs after-diff-path jdiff
                          :display-path disp
                          :before-label b-lbl
                          :after-label a-lbl
                          :dir (or dir default-directory)
                          :preset omnidiff-default-preset
                          :whole-updates omnidiff-whole-updates
                          :paint-reindent-moves omnidiff-paint-reindent-moves
                          :editable-after t
                          :owned-tmpdir snap-tmpdir)))
                (setq transferred t)
                res)))
        (when (and (not transferred) snap-tmpdir (file-directory-p snap-tmpdir))
          (ignore-errors (delete-directory snap-tmpdir t)))))))

;;;###autoload
(defun omnidiff-buffers (buf-before buf-after)
  "Compare two live Emacs buffers BUF-BEFORE and BUF-AFTER in Dual viewer.
Includes unsaved modifications in both buffers."
  (interactive
   (let* ((cur (current-buffer))
          (other (other-buffer cur t))
          (a (read-buffer "Before buffer: " (and other (buffer-name other)) t))
          (b (read-buffer "After buffer: " (buffer-name cur) t)))
     (list (get-buffer a) (get-buffer b))))
  (let ((b1 (if (bufferp buf-before) buf-before (get-buffer buf-before)))
        (b2 (if (bufferp buf-after) buf-after (get-buffer buf-after))))
    (unless (buffer-live-p b1)
      (user-error "Before buffer is not live: %S" buf-before))
    (unless (buffer-live-p b2)
      (user-error "After buffer is not live: %S" buf-after))
    (when (eq b1 b2)
      (user-error "Cannot compare buffer %s with itself" (buffer-name b1)))
    (let ((tmpdir (make-temp-file "omnidiff-buffers-" t)))
      (unwind-protect
          (let* ((f1 (omnidiff-engine-write-temp-for-buffer b1 tmpdir))
                 (f2 (omnidiff-engine-write-temp-for-buffer b2 tmpdir))
                 (jdiff (omnidiff-engine-run-pair
                         f1 f2
                         omnidiff-default-preset nil
                         omnidiff-whole-updates omnidiff-paint-reindent-moves)))
            (omnidiff-dual-open
             :before-buffer b1
             :after-buffer b2
             :before-path (buffer-file-name b1)
             :after-path (buffer-file-name b2)
             :before-label (buffer-name b1)
             :after-label (buffer-name b2)
             :display-path (format "%s vs %s" (buffer-name b1) (buffer-name b2))
             :json-diff jdiff
             :preset omnidiff-default-preset
             :whole-updates omnidiff-whole-updates
             :paint-reindent-moves omnidiff-paint-reindent-moves
             :editable-after (not (with-current-buffer b2 buffer-read-only))
             :nav-callbacks
             (list :open-tui (lambda (sess)
                               (omnidiff--launch-tui-for-session sess)))))
        (when (file-directory-p tmpdir)
          (ignore-errors (delete-directory tmpdir t)))))))

;;;###autoload
(defun omnidiff-buffer-vs-head (&optional rev)
  "Compare the current file buffer against Git revision REV (default \"HEAD\").
With prefix argument, prompt for REV.  Attaches the Dual viewer directly to
the live current buffer so unsaved edits are shown and `e' unlocks editing."
  (interactive
   (list (if current-prefix-arg
             (omnidiff--read-revision)
           "HEAD")))
  (let ((cur-buf (current-buffer))
        (file (buffer-file-name)))
    (unless file
      (user-error "Current buffer is not visiting a file"))
    (let* ((repo-root (or (omnidiff-git-toplevel (file-name-directory file))
                          (user-error "File is not inside a Git repository: %s" file)))
           (relpath (file-relative-name (file-truename file)
                                        (file-truename repo-root)))
           (target-rev (if (and (stringp rev) (not (string-empty-p rev)))
                           rev
                         "HEAD"))
           (tmpdir (make-temp-file "omnidiff-vs-rev-" t))
           (transferred nil))
      (unwind-protect
          (let* ((b-file (or (omnidiff-git-show-to-file
                              target-rev relpath repo-root tmpdir "before")
                             "/dev/null"))
                 (a-file (if (and (not (buffer-modified-p cur-buf))
                                  (file-regular-p file))
                             file
                           (omnidiff-engine-write-temp-for-buffer cur-buf tmpdir relpath)))
                 (jdiff (omnidiff-engine-run-pair
                         b-file a-file
                         omnidiff-default-preset nil
                         omnidiff-whole-updates omnidiff-paint-reindent-moves))
                 (b-lbl (if (string= b-file "/dev/null")
                            "/dev/null (new file)"
                          (format "%s:%s" target-rev relpath))))
            (if (or (omnidiff-json-diff-picture jdiff)
                    (omnidiff-json-diff-container jdiff)
                    (omnidiff-json-diff-binary-p jdiff))
                (let ((res (omnidiff--open-pair-result
                            b-file a-file jdiff
                            :display-path relpath
                            :before-label b-lbl
                            :after-label (format "%s (live)" relpath)
                            :dir repo-root
                            :preset omnidiff-default-preset
                            :whole-updates omnidiff-whole-updates
                            :paint-reindent-moves omnidiff-paint-reindent-moves
                            :editable-after nil
                            :owned-tmpdir tmpdir)))
                  (setq transferred t)
                  res)
              (let* ((b-enc (omnidiff-json-side-encoding (omnidiff-json-diff-before jdiff)))
                     (b-src (if (string= b-file "/dev/null")
                                ""
                              (omnidiff-engine-read-file-decoded b-file b-enc))))
                (omnidiff-dual-open
                 :before-path (when (string= b-file "/dev/null") "/dev/null")
                 :before-source b-src
                 :after-buffer cur-buf
                 :after-path file
                 :before-label b-lbl
                 :after-label (format "%s (live)" relpath)
                 :display-path relpath
                 :json-diff jdiff
                 :preset omnidiff-default-preset
                 :whole-updates omnidiff-whole-updates
                 :paint-reindent-moves omnidiff-paint-reindent-moves
                 :editable-after t
                 :nav-callbacks
                 (list :open-tui (lambda (sess)
                                   (omnidiff--launch-tui-for-session sess)))))))
        (when (and (not transferred) (file-directory-p tmpdir))
          (ignore-errors (delete-directory tmpdir t)))))))

(defun omnidiff--magit-target-file ()
  "Return repo-relative target file at point in Magit or current buffer, or nil."
  (or (and (fboundp 'magit-file-at-point) (magit-file-at-point))
      (and (fboundp 'magit-thing-at-point) (magit-thing-at-point 'filename t))
      (and (fboundp 'magit-section-value-if)
           (let ((val (magit-section-value-if 'file)))
             (when (stringp val) val)))
      (when buffer-file-name
        (when-let* ((root (omnidiff-git-toplevel (file-name-directory buffer-file-name))))
          (file-relative-name (file-truename buffer-file-name)
                              (file-truename root))))))

;;;###autoload
(defun omnidiff-magit-dual ()
  "Open the Dual semantic viewer for the file or range at point in Magit/Git.
Inside Magit, retains the full multi-file review context (`]'/`['/`O'/`R')
and lands directly on the file and hunk line at point."
  (interactive)
  (let* ((in-magit (and (derived-mode-p 'magit-mode)
                        (fboundp 'magit-diff--dwim)))
         (spec (when in-magit (omnidiff--magit-entry-spec)))
         (repo (or (and (fboundp 'magit-toplevel) (magit-toplevel))
                   (omnidiff-git-toplevel))))
    (cond
     ;; Outside Magit on a file buffer in a Git repo -> compare live buffer vs HEAD.
     ((and (not in-magit) buffer-file-name repo)
      (omnidiff-buffer-vs-head "HEAD"))
     ;; Magit untracked file at point -> compare /dev/null vs worktree file directly.
     ((and in-magit (plist-get spec :untracked) (plist-get spec :file))
      (omnidiff-diff nil nil spec 'force))
     ;; Magit diff/status/log/stash/range -> full multi-file context landing directly in Dual.
     (in-magit
      (omnidiff-diff (omnidiff--read-diff-arg)
                     (omnidiff--magit-diff-files)
                     spec
                     'force))
     ;; Outside Magit with no file buffer -> prompt for a repo file and compare vs HEAD.
     (repo
      (let* ((picked (read-file-name "Diff file against HEAD: " repo nil t))
             (buf (find-file-noselect picked)))
        (with-current-buffer buf
          (omnidiff-buffer-vs-head "HEAD"))))
     (t
      (user-error "Not in a Git repository or visiting a file")))))

;;;###autoload
(defun omnidiff-magit-setup ()
  "Idempotently append Omnidiff `D', `S', and `V' suffixes to `magit-diff'."
  (with-eval-after-load 'magit-diff
    (let ((missing
           (delq nil
                 (list (unless (ignore-errors (transient-get-suffix 'magit-diff "D"))
                         '("D" "Omnidiff DWIM" omnidiff-diff))
                       (unless (ignore-errors (transient-get-suffix 'magit-diff "S"))
                         '("S" "Omnidiff Show commit" omnidiff-show))
                       (unless (ignore-errors (transient-get-suffix 'magit-diff "V"))
                         '("V" "Omnidiff Dual (review)" omnidiff-magit-dual))))))
      (when missing
        (transient-append-suffix 'magit-diff '(-1 -1) (vconcat missing))))))

(provide 'omnidiff)
;;; omnidiff.el ends here
