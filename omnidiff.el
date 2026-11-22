;;; omnidiff.el --- Native Emacs viewer for omnidiff content-aware diffs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
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
;;    `minimal', `full'), and Modus-adaptive color schemes.
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

(declare-function magit-toplevel "magit-git" (&optional directory))
(declare-function magit-file-at-point "magit-git" (&optional expand assert))
(declare-function magit-thing-at-point "magit-section" (type &optional demand))
(declare-function magit-section-value-if "magit-section" (type &optional section))
(declare-function magit-branch-or-commit-at-point "magit-git" ())
(declare-function magit-read-branch-or-commit "magit-git" (prompt &optional secondary-default))
(declare-function magit-diff-read-range-or-commit "magit-diff" (prompt &optional secondary-default mbase))
(declare-function magit-diff--dwim "magit-diff" ())
(declare-function magit-diff-arguments "magit-diff" (&optional mode))
(declare-function magit-current-section "magit-section" ())
(declare-function magit-section-match "magit-section" (condition &optional section))
(declare-function magit-diff-hunk-line "magit-diff" (section goto-from))
(declare-function magit-refresh "magit-mode" ())
(declare-function transient-append-suffix "transient" (prefix loc suffix))
(declare-function transient-get-suffix "transient" (prefix loc))
(declare-function eat-make "eat" (name program &optional startfile &rest switches))
(declare-function eat "eat" (&optional program arg))
(declare-function vterm "vterm" (&optional arg))
(declare-function term-mode "term" ())
(declare-function term-exec "term" (buffer name command startfile switches))
(declare-function term-char-mode "term" ())

(defvar vterm-shell)

;;; Customization & process state

(defcustom omnidiff-executable "omnidiff"
  "Executable name or path for the `omnidiff' CLI."
  :type 'string
  :group 'omnidiff)

(defcustom omnidiff-default-preset 'minimal
  "Default verbosity preset for `omnidiff-mode'."
  :type '(choice (const :tag "Minimal (Only What Changed)" minimal)
                 (const :tag "Full" full)
                 (const :tag "Default (CLI saved setting)" default))
  :group 'omnidiff)

(defcustom omnidiff-single-file-entry 'auto
  "Entry view strategy for `omnidiff-diff' and `omnidiff-show'.
- `auto': Open Dual directly when a diff targets exactly one non-binary
  text file (not a raster image or container), keeping Overview accessible
  via `O'; multi-file, visual, and container diffs open Overview.
- `overview': Always open Overview first.
- `dual': Always open Dual on the first non-binary text file."
  :type '(choice (const :tag "Auto (Dual for single text file)" auto)
                 (const :tag "Always Overview" overview)
                 (const :tag "Always Dual (first text file)" dual))
  :group 'omnidiff)

(defcustom omnidiff-magit-file-entry 'dual
  "Entry behavior when invoking `omnidiff-diff' on a specific file in Magit.
- `dual': Open Dual directly on the file (and hunk line) at point while
  retaining the full multi-file review context (`]'/`['/`O'/`R'); image
  and container files open expanded in Overview.
- `overview': Open Overview positioned on (and expanding) that file card."
  :type '(choice (const :tag "Dual directly (with multi-file context)" dual)
                 (const :tag "Overview focused on file" overview))
  :group 'omnidiff)

(defcustom omnidiff-tui-launcher 'auto
  "Terminal backend used by `omnidiff-open-in-tui' to launch `omnidiff' TUI.
- `auto': Prefer `eat', then `vterm', then built-in `ansi-term'.
- `eat': Launch in an `eat' terminal buffer.
- `vterm': Launch in a `vterm' buffer.
- `ansi-term': Launch in Emacs's built-in `term-mode' (`ansi-term').
- Function: Called as `(FN ARGV BUFFER-NAME DIR)'."
  :type '(choice (const :tag "Auto (eat -> vterm -> ansi-term)" auto)
                 (const :tag "Eat" eat)
                 (const :tag "Vterm" vterm)
                 (const :tag "Built-in ansi-term" ansi-term)
                 (function :tag "Custom launcher function"))
  :group 'omnidiff)

(defcustom omnidiff-after-diff-hook nil
  "Normal hook run in the Overview buffer after `omnidiff-run' renders a diff."
  :type 'hook
  :group 'omnidiff)

(defcustom omnidiff-line-spacing 0.15
  "Buffer-local `line-spacing' used in `omnidiff-mode' buffers."
  :type '(choice (const :tag "Default" nil) number)
  :group 'omnidiff)

(defcustom omnidiff-max-parallel (min 12 (max 4 (num-processors)))
  "Maximum concurrent `omnidiff' processes spawned for multi-file Git diffs.
Set to 1 to fall back to Git's serial `GIT_EXTERNAL_DIFF' invocation."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-max-parallel-blob-bytes (* 32 1024 1024)
  "Maximum blob bytes buffered per `git cat-file --batch' batch in parallel mode.
Multi-file diffs whose total blob bytes exceed this limit are streamed across
multiple batches; only when a single blob exceeds this limit does `omnidiff'
fall back to Git's serial `GIT_EXTERNAL_DIFF' invocation."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-archive-extract-max-bytes (* 64 1024 1024)
  "Maximum uncompressed bytes extracted for one archive member drilldown.
Set to 0 to disable the limit."
  :type 'natnum
  :group 'omnidiff)

(cl-defstruct (omnidiff--request (:copier nil))
  "In-flight asynchronous `omnidiff' request bound to a buffer."
  (id 0 :type integer)
  (proc nil)
  (stderr-proc nil)
  (workers nil :type list)
  (tmpdir nil)
  (command nil :type list)
  (label "diff" :type string)
  (repo-dir nil)
  (preset 'minimal :type symbol)
  (layout 'unified :type symbol)
  (whole-updates nil :type boolean)
  (paint-reindent-moves nil :type boolean)
  (auto-dual nil)
  (target-file nil)
  (target-line nil)
  (target-side nil)
  (origin-buf nil)
  (last-msg nil)
  (failed-event nil)
  (stderr-done nil :type boolean)
  (cancelled nil :type boolean)
  (preserve-state nil :type boolean)
  (image-blobs nil))

(cl-defstruct (omnidiff--git-entry (:copier nil))
  "Single file change record parsed from `git --raw --no-abbrev -z'."
  (status "" :type string)
  (old-mode "" :type string)
  (new-mode "" :type string)
  (old-sha "" :type string)
  (new-sha "" :type string)
  (path "" :type string)
  (new-path "" :type string))

(defvar-local omnidiff--active-request nil
  "Active `omnidiff--request' associated with the current buffer, if any.")

(defvar-local omnidiff--request-id 0
  "Monotonic request counter rejecting stale process completions.")

(defvar-local omnidiff--diff nil
  "Last rendered `omnidiff-diff' cached for instant in-memory layout switching.")

(defvar-local omnidiff--whole-updates nil
  "Buffer-local `--whole-updates' override, else `omnidiff-whole-updates'.")

(defvar-local omnidiff--paint-reindent-moves nil
  "Buffer-local `--paint-reindent-moves' override, else default custom.")

(defvar-local omnidiff--owned-tmpdirs nil
  "Temporary directories owned by this `omnidiff-mode' buffer, deleted on kill.")

(defvar-local omnidiff--tui-tmpdir nil
  "Most recent temporary directory owned by a spawned `omnidiff' TUI buffer.")

(defvar-local omnidiff--tui-tmpdirs nil
  "Temporary directories owned by a spawned `omnidiff' TUI terminal buffer.")

(defvar-local omnidiff--last-sbs-width nil
  "Last rendered window column width for debounced `side-by-side' text reflow.")

(defvar-local omnidiff--sbs-resize-timer nil
  "Active debounce timer for `side-by-side' Overview window-resize reflow.")

(defvar-local omnidiff--origin-buffer nil
  "Originating buffer (e.g. a Magit buffer) from which Overview was launched.")

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
    (dolist (ov ovs)
      (omnidiff--set-overlay-folded ov any-open 'omnidiff-fold 'omnidiff-file-accent))
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
        (let ((sum (or (omnidiff-file-summary f)
                       (and (omnidiff-file-json-diff f)
                            (omnidiff-json-diff-summary (omnidiff-file-json-diff f))))))
          (when (memq sum omnidiff--trivial-summaries)
            (puthash (omnidiff-file-target-path f) t trivial-targets)))))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((m (text-property-search-forward 'omnidiff-file-header)))
          (if (null m)
              (goto-char (point-max))
            (let* ((pos (prop-match-beginning m))
                   (rel (get-text-property pos 'omnidiff-file))
                   (ov (get-text-property pos 'omnidiff-body-ov)))
              (when (and ov (gethash rel trivial-targets))
                (push ov trivial-ovs)))))))
    (if (null trivial-ovs)
        (message "No trivial file cards to fold")
      (let* ((any-open (cl-some (lambda (ov) (not (overlay-get ov 'invisible))) trivial-ovs))
             (target-hdr (when any-open
                           (omnidiff-render-file-header-pos-at (point)))))
        (dolist (ov trivial-ovs)
          (omnidiff--set-overlay-folded ov any-open 'omnidiff-fold 'omnidiff-file-accent))
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
      (let* ((jd (omnidiff-file-json-diff file))
             (pic (or (omnidiff-file-picture file)
                      (and jd (omnidiff-json-diff-picture jd))))
             (cont (or (omnidiff-file-container file)
                       (and jd (omnidiff-json-diff-container jd)))))
        (list 'content
              status path target old-oid new-oid old-mode new-mode
              (omnidiff--hash-text (omnidiff-file-before-source file))
              (omnidiff--hash-text (omnidiff-file-after-source file))
              (omnidiff-file-binary-p file)
              (omnidiff-file-note file)
              (omnidiff-file-summary file)
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
  "Return start position of the file header for TARGET in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let ((found nil))
      (while (and (not found) (< (point) (point-max)))
        (let ((m (text-property-search-forward 'omnidiff-file-header)))
          (if (null m)
              (goto-char (point-max))
            (let ((pos (prop-match-beginning m)))
              (when (equal (get-text-property pos 'omnidiff-file) target)
                (setq found pos))))))
      found)))

(defun omnidiff--file-reviewed-p (target)
  "Return non-nil when TARGET is marked reviewed in the current Overview buffer."
  (and (hash-table-p omnidiff--reviewed-files)
       (not (null (gethash target omnidiff--reviewed-files)))))

(defun omnidiff--set-file-reviewed (target reviewed-p)
  "Set reviewed state for TARGET to REVIEWED-P in the current Overview buffer.
Updates `omnidiff--reviewed-files', toggles the `✓' header indicator, folds
or unfolds the file card, and refreshes the header line."
  (let ((file (and omnidiff--diff
                   (cl-find target (omnidiff-diff-files omnidiff--diff)
                            :key #'omnidiff-file-target-path
                            :test #'equal))))
    (when file
      (if reviewed-p
          (progn
            (unless (hash-table-p omnidiff--reviewed-files)
              (setq omnidiff--reviewed-files (make-hash-table :test #'equal)))
            (puthash target (omnidiff--file-review-signature file)
                     omnidiff--reviewed-files))
        (when (hash-table-p omnidiff--reviewed-files)
          (remhash target omnidiff--reviewed-files)
          (when (= (hash-table-count omnidiff--reviewed-files) 0)
            (setq omnidiff--reviewed-files nil))))
      (when-let* ((hdr (omnidiff--find-file-header-pos target)))
        (omnidiff-render-set-file-reviewed-indicator hdr reviewed-p)
        (when-let* ((ov (get-text-property hdr 'omnidiff-body-ov)))
          (omnidiff--set-overlay-folded
           ov reviewed-p 'omnidiff-fold 'omnidiff-file-accent)))
      (force-mode-line-update)
      reviewed-p)))

(defun omnidiff--next-unreviewed-file (files current-target)
  "Return the next unreviewed `omnidiff-file' in FILES after CURRENT-TARGET.
Wraps around once and returns nil when all files in FILES are reviewed."
  (let ((len (length files)))
    (when (> len 0)
      (let* ((vec (vconcat files))
             (cur-idx (or (cl-position current-target vec
                                       :key #'omnidiff-file-target-path
                                       :test #'equal)
                          -1))
             (found nil)
             (step 1))
        (while (and (<= step len) (null found))
          (let* ((cand (aref vec (mod (+ cur-idx step) len)))
                 (ctarget (omnidiff-file-target-path cand)))
            (unless (omnidiff--file-reviewed-p ctarget)
              (setq found cand)))
          (setq step (1+ step)))
        found))))

(defun omnidiff-toggle-reviewed ()
  "Toggle reviewed status for the file at point in Overview.
Marking a file reviewed shows `✓', folds its card, and advances to the next
unreviewed file.  Unmarking removes `✓' and unfolds the card."
  (interactive)
  (let ((file (omnidiff--file-at-point)))
    (unless file
      (user-error "No file at point"))
    (let* ((target (omnidiff-file-target-path file))
           (new-state (not (omnidiff--file-reviewed-p target)))
           (files (and omnidiff--diff (omnidiff-diff-files omnidiff--diff)))
           (total (length files)))
      (omnidiff--set-file-reviewed target new-state)
      (let ((rev-cnt (omnidiff-render-reviewed-count)))
        (if new-state
            (let ((next (omnidiff--next-unreviewed-file files target)))
              (if next
                  (let* ((next-target (omnidiff-file-target-path next))
                         (next-pos (omnidiff--find-file-header-pos next-target)))
                    (when next-pos (goto-char next-pos))
                    (message "Reviewed %s (%d/%d) — next: %s"
                             target rev-cnt total next-target))
                (when-let* ((hdr (omnidiff--find-file-header-pos target)))
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
    ("m" (lambda () (format "Preset [%s]" (omnidiff--effective-preset)))
     omnidiff-cycle-preset)
    ("W" (lambda ()
           (format "--whole-updates [%s]"
                   (if (omnidiff--effective-whole-updates) "on" "off")))
     omnidiff-toggle-whole-updates)
    ("I" (lambda ()
           (format "--paint-reindent-moves [%s]"
                   (if (omnidiff--effective-paint-reindent-moves) "on" "off")))
     omnidiff-toggle-paint-reindent-moves)
    ("T" "Open file in native TUI" omnidiff-open-in-tui)
    ("g" "Refresh diff" omnidiff-refresh)
    ("q" "Quit viewer" quit-window)]])

(defun omnidiff-help ()
  "Open context-aware transient menu for `omnidiff-mode'.
In noninteractive sessions, displays a concise echo-area keybinding summary."
  (interactive)
  (if (not noninteractive)
      (if (omnidiff-image--card-ov-at)
          (call-interactively #'omnidiff-image-menu)
        (call-interactively #'omnidiff-overview-menu))
    (if (omnidiff-image--card-ov-at)
        (message "Omnidiff [image]: 1-6/i mode · b flip (DEL back) · h/l/←/→ adjust · +/-/0 zoom · t backdrop · r regions (</>) · o/RET open · x external · T tui · L load · TAB fold · v layout (%s) · q quit"
                 (or omnidiff--layout omnidiff-default-layout))
      (message "Omnidiff: d dual · R reviewed · n/p hunk · ]/[ file · TAB/S-TAB/F fold · RET jump/drill · v view (%s) · H style (%s) · m preset · W whole · I reindent · c color (%s) · T tui · w wrap · g refresh · q quit"
               (or omnidiff--layout omnidiff-default-layout)
               (omnidiff-render-effective-highlight-style)
               omnidiff-color-scheme))))

(defun omnidiff-cycle-color-scheme ()
  "Cycle `omnidiff-color-scheme' across semantic, calm, and paper schemes."
  (interactive)
  (setq omnidiff-color-scheme
        (pcase omnidiff-color-scheme
          ('omni-semantic 'calm-modus)
          ('calm-modus 'neutral-paper)
          (_ 'omni-semantic)))
  (if (omnidiff-apply-theme)
      (message "Omnidiff color scheme: %s" omnidiff-color-scheme)
    (message "Omnidiff color scheme set to %s (theme unchanged)"
             omnidiff-color-scheme)))

(defun omnidiff--decode-git-path (bytes)
  "Decode unibyte Git path BYTES without converting embedded CRLF endings."
  (decode-coding-string bytes 'utf-8-unix))

(defun omnidiff--expand-repo-path (rel dir)
  "Expand repo-relative path REL under DIR without expanding a leading `~'."
  (if (string-prefix-p "~" rel)
      (expand-file-name (concat "./" rel) dir)
    (expand-file-name rel dir)))

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
      (unless rel
        (user-error "No source file at point"))
      (let* ((dir (or omnidiff--repo-dir default-directory))
             (full (omnidiff--expand-repo-path rel dir)))
        (if (not (file-exists-p full))
            (let ((entry (and omnidiff--diff
                              (cl-find-if (lambda (f)
                                            (equal (omnidiff-file-target-path f) rel))
                                          (omnidiff-diff-files omnidiff--diff)))))
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

(defun omnidiff--pending-request ()
  "Return the active unfailed `omnidiff--request' for this buffer, or nil."
  (when (and omnidiff--active-request
             (null (omnidiff--request-failed-event omnidiff--active-request)))
    omnidiff--active-request))

(defun omnidiff--effective-layout ()
  "Return the pending layout if an unfailed request is in flight, else committed."
  (or (when-let* ((req (omnidiff--pending-request)))
        (omnidiff--request-layout req))
      omnidiff--layout
      omnidiff-default-layout))

(defun omnidiff--effective-preset ()
  "Return the pending preset if an unfailed request is in flight, else committed."
  (or (when-let* ((req (omnidiff--pending-request)))
        (omnidiff--request-preset req))
      omnidiff--preset
      omnidiff-default-preset))

(defun omnidiff--effective-whole-updates ()
  "Return pending or committed `--whole-updates' setting for this buffer."
  (if-let* ((req (omnidiff--pending-request)))
      (omnidiff--request-whole-updates req)
    (if (local-variable-p 'omnidiff--whole-updates)
        omnidiff--whole-updates
      omnidiff-whole-updates)))

(defun omnidiff--effective-paint-reindent-moves ()
  "Return pending or committed `--paint-reindent-moves' setting for this buffer."
  (if-let* ((req (omnidiff--pending-request)))
      (omnidiff--request-paint-reindent-moves req)
    (if (local-variable-p 'omnidiff--paint-reindent-moves)
        omnidiff--paint-reindent-moves
      omnidiff-paint-reindent-moves)))

(defun omnidiff--cleanup-owned-tmpdirs ()
  "Delete temporary directories owned by the current `omnidiff-mode' buffer."
  (let ((dirs omnidiff--owned-tmpdirs))
    (setq omnidiff--owned-tmpdirs nil)
    (dolist (d dirs)
      (when (and (stringp d) (file-directory-p d))
        (ignore-errors (delete-directory d t))))))

(defun omnidiff--cleanup-tui-tmpdir ()
  "Delete temporary directories owned by a spawned `omnidiff' TUI buffer."
  (let ((dirs (if (and (stringp omnidiff--tui-tmpdir)
                       (not (member omnidiff--tui-tmpdir omnidiff--tui-tmpdirs)))
                  (cons omnidiff--tui-tmpdir omnidiff--tui-tmpdirs)
                omnidiff--tui-tmpdirs)))
    (setq omnidiff--tui-tmpdirs nil
          omnidiff--tui-tmpdir nil)
    (dolist (dir dirs)
      (when (and (stringp dir) (file-directory-p dir))
        (ignore-errors (delete-directory dir t))))))

(defun omnidiff--cleanup-request-resources (req)
  "Kill any worker processes and delete temporary blob storage owned by REQ."
  (let ((workers (omnidiff--request-workers req))
        (tmpdir (omnidiff--request-tmpdir req)))
    (setf (omnidiff--request-workers req) nil
          (omnidiff--request-tmpdir req) nil)
    (dolist (w workers)
      (when (and w (process-live-p w))
        (delete-process w)))
    (when (and tmpdir (file-directory-p tmpdir))
      (ignore-errors (delete-directory tmpdir t)))))

(defun omnidiff--cancel-request ()
  "Cancel any in-flight `omnidiff--request' owned by the current buffer."
  (when-let* ((req omnidiff--active-request))
    (setf (omnidiff--request-cancelled req) t)
    (setq omnidiff--active-request nil)
    (let ((proc (omnidiff--request-proc req))
          (stderr-proc (omnidiff--request-stderr-proc req)))
      (when (and proc (process-live-p proc))
        (delete-process proc))
      (when (and stderr-proc (process-live-p stderr-proc))
        (delete-process stderr-proc)))
    (omnidiff--cleanup-request-resources req)))

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

(defun omnidiff--restore-folded-files (folded)
  "Fold all file cards in the current buffer whose paths appear in FOLDED."
  (when folded
    (let ((tbl (make-hash-table :test #'equal)))
      (dolist (rel folded) (puthash rel t tbl))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) (point-max))
          (let ((m (text-property-search-forward 'omnidiff-file-header)))
            (if (null m)
                (goto-char (point-max))
              (let* ((pos (prop-match-beginning m))
                     (rel (get-text-property pos 'omnidiff-file))
                     (ov (get-text-property pos 'omnidiff-body-ov)))
                (when (and rel ov (gethash rel tbl))
                  (omnidiff--set-overlay-folded
                   ov t 'omnidiff-fold 'omnidiff-file-accent))))))))))

(defun omnidiff--reflow-sbs-buffer (buf)
  "Re-render cached `side-by-side' Overview BUF for its new window width."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq omnidiff--sbs-resize-timer nil)
      (when (and (derived-mode-p 'omnidiff-mode)
                 (eq (or omnidiff--layout omnidiff-default-layout) 'side-by-side)
                 omnidiff--diff
                 (null (omnidiff--pending-request)))
        (when-let* ((win (get-buffer-window buf t)))
          (let ((cur-w (window-body-width win))
                (saved-file (get-text-property (point) 'omnidiff-file))
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
             (null (omnidiff--pending-request)))
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
  (let* ((req (omnidiff--pending-request))
         (cmd (if req (omnidiff--request-command req) omnidiff--command))
         (lbl (if req (omnidiff--request-label req) omnidiff--label))
         (dir (if req (omnidiff--request-repo-dir req) omnidiff--repo-dir)))
    (unless cmd
      (user-error "No omnidiff command recorded for this buffer"))
    (omnidiff-run (current-buffer) cmd lbl dir
                  (omnidiff--effective-preset)
                  (omnidiff--effective-layout)
                  nil t nil
                  (omnidiff--effective-whole-updates)
                  (omnidiff--effective-paint-reindent-moves))))

(defun omnidiff--restore-location (file old-ln new-ln side &optional fallback-ln)
  "Move point to the rendered position matching FILE, OLD-LN, NEW-LN, and SIDE."
  (when file
    (goto-char (point-min))
    (let ((file-pos nil))
      (while (and (null file-pos) (< (point) (point-max)))
        (let ((m (text-property-search-forward 'omnidiff-file-header)))
          (if (null m)
              (goto-char (point-max))
            (when (equal (get-text-property (prop-match-beginning m) 'omnidiff-file) file)
              (setq file-pos (prop-match-beginning m))))))
      (when file-pos
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
                  (let ((eol (line-end-position)))
                    (when-let* ((m (save-excursion
                                     (text-property-search-forward 'omnidiff-side 'after t))))
                      (when (< (prop-match-beginning m) eol)
                        (goto-char (prop-match-beginning m))))))
                (when (invisible-p (point))
                  (let ((body-ov (get-text-property file-pos 'omnidiff-body-ov)))
                    (if (and body-ov (overlay-buffer body-ov)
                             (overlay-get body-ov 'invisible))
                        (omnidiff--set-overlay-folded
                         body-ov nil 'omnidiff-fold 'omnidiff-file-accent)
                      (goto-char file-pos)))
                  (when (invisible-p (point))
                    (goto-char file-pos)))))))))))

(defun omnidiff-cycle-layout (&optional reset-pinned-p)
  "Cycle omnidiff view layout (`unified' -> `side-by-side' -> `stacked').
Preserves image cards whose mode was explicitly pinned unless prefix
argument RESET-PINNED-P is non-nil."
  (interactive "P")
  (let* ((req (omnidiff--pending-request))
         (next-layout (cl-ecase (omnidiff--effective-layout)
                        (unified 'side-by-side)
                        (side-by-side 'stacked)
                        (stacked 'unified))))
    (cond
     ;; In-flight request: update target layout in place without restarting workers.
     (req
      (let ((msg (format "Omnidiff layout: %s..." next-layout)))
        (setf (omnidiff--request-layout req) next-layout
              (omnidiff--request-last-msg req) msg)
        (message "%s" msg)))
     ;; Cached parsed diff: re-render in memory with zero subprocesses.
     (omnidiff--diff
      (let ((saved-file (get-text-property (point) 'omnidiff-file))
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
                    (omnidiff--effective-preset)
                    next-layout
                    (format "Omnidiff layout: %s..." next-layout)))
     (t
      (user-error "No omnidiff command recorded for this buffer")))))

(defun omnidiff-cycle-preset ()
  "Cycle omnidiff verbosity preset (`minimal' -> `full' -> `default')."
  (interactive)
  (let* ((req (omnidiff--pending-request))
         (cmd (if req (omnidiff--request-command req) omnidiff--command))
         (lbl (if req (omnidiff--request-label req) omnidiff--label))
         (dir (if req (omnidiff--request-repo-dir req) omnidiff--repo-dir)))
    (unless cmd
      (user-error "No omnidiff command recorded for this buffer"))
    (let ((next-preset (pcase (omnidiff--effective-preset)
                         ('minimal 'full)
                         ('full 'default)
                         (_ 'minimal))))
      (omnidiff-run (current-buffer) cmd lbl dir
                    next-preset
                    (omnidiff--effective-layout)
                    (format "Omnidiff preset: %s..." next-preset)
                    nil nil
                    (omnidiff--effective-whole-updates)
                    (omnidiff--effective-paint-reindent-moves)))))

(defun omnidiff-cycle-highlight-style ()
  "Cycle Overview highlight style (`token' -> `subtle-line' -> `traditional')."
  (interactive)
  (let ((next (pcase (omnidiff-render-effective-highlight-style)
                ('token 'subtle-line)
                ('subtle-line 'traditional)
                (_ 'token))))
    (setq omnidiff--highlight-style next)
    (when omnidiff--diff
      (let ((saved-file (get-text-property (point) 'omnidiff-file))
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
  (let* ((req (omnidiff--pending-request))
         (cmd (if req (omnidiff--request-command req) omnidiff--command))
         (lbl (if req (omnidiff--request-label req) omnidiff--label))
         (dir (if req (omnidiff--request-repo-dir req) omnidiff--repo-dir))
         (next (not (omnidiff--effective-whole-updates))))
    (unless cmd
      (user-error "No omnidiff command recorded for this buffer"))
    (setq omnidiff--whole-updates next)
    (omnidiff-run (current-buffer) cmd lbl dir
                  (omnidiff--effective-preset)
                  (omnidiff--effective-layout)
                  (format "Omnidiff --whole-updates: %s..." (if next "on" "off"))
                  t nil next (omnidiff--effective-paint-reindent-moves))))

(defun omnidiff-toggle-paint-reindent-moves ()
  "Toggle `--paint-reindent-moves' for this omnidiff buffer and re-run."
  (interactive)
  (let* ((req (omnidiff--pending-request))
         (cmd (if req (omnidiff--request-command req) omnidiff--command))
         (lbl (if req (omnidiff--request-label req) omnidiff--label))
         (dir (if req (omnidiff--request-repo-dir req) omnidiff--repo-dir))
         (next (not (omnidiff--effective-paint-reindent-moves))))
    (unless cmd
      (user-error "No omnidiff command recorded for this buffer"))
    (setq omnidiff--paint-reindent-moves next)
    (omnidiff-run (current-buffer) cmd lbl dir
                  (omnidiff--effective-preset)
                  (omnidiff--effective-layout)
                  (format "Omnidiff --paint-reindent-moves: %s..." (if next "on" "off"))
                  t nil (omnidiff--effective-whole-updates) next)))

;;; Dual bridge, TUI launcher & container drilldown

(defun omnidiff--unstaged-worktree-diff-p (&optional cmd)
  "Return non-nil when CMD (default `omnidiff--command') is an unstaged diff."
  (let ((c (or cmd omnidiff--command)))
    (and (consp c)
         (proper-list-p c)
         (equal (nth 0 c) "git")
         (equal (nth 2 c) "diff")
         (let ((tail (nthcdr 4 c))
               (non-worktree nil))
           (while (and tail (not non-worktree))
             (let ((arg (pop tail)))
               (cond
                ((not (stringp arg)) (setq non-worktree t))
                ((string= arg "--") (setq tail nil))
                ((or (member arg '("--cached" "--staged"))
                     (string-search ".." arg)
                     (not (string-prefix-p "-" arg)))
                 (setq non-worktree t)))))
           (not non-worktree)))))

(defun omnidiff--file-text-dual-p (file)
  "Return non-nil when `omnidiff-file' FILE is a non-binary text file for Dual."
  (let* ((jd (omnidiff-file-json-diff file))
         (pic (or (omnidiff-file-picture file)
                  (and jd (omnidiff-json-diff-picture jd))))
         (cont (or (omnidiff-file-container file)
                   (and jd (omnidiff-json-diff-container jd))))
         (bin (or (omnidiff-file-binary-p file)
                  (and jd (omnidiff-json-diff-binary-p jd)))))
    (and (null cont)
         (or (null pic) (omnidiff-picture-svg-p pic))
         (not bin))))

(defun omnidiff--file-at-point ()
  "Return the `omnidiff-file' at point in the current Overview buffer, or nil."
  (when omnidiff--diff
    (let* ((files (omnidiff-diff-files omnidiff--diff))
           (rel (or (get-text-property (point) 'omnidiff-file)
                    (when-let* ((hdr (omnidiff-render-file-header-pos-at (point))))
                      (get-text-property hdr 'omnidiff-file)))))
      (or (and rel
               (cl-find-if (lambda (f)
                             (equal (omnidiff-file-target-path f) rel))
                           files))
          (car files)))))

(defun omnidiff--git-cat-blob-to-file (oid path repo-dir tmpdir tag)
  "Write Git blob OID from REPO-DIR into TMPDIR preserving PATH's hint.
Returns the written file path, or nil if `git cat-file' fails."
  (when (and oid repo-dir (not (omnidiff--zero-oid-p oid)))
    (let* ((default-directory repo-dir)
           (dest (omnidiff--blob-dest (concat tag "-" oid) path tmpdir)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'no-conversion))
          (when (zerop (call-process "git" nil t nil "cat-file" "blob" oid))
            (omnidiff--write-small-temp dest (buffer-string))))))))

(defun omnidiff--git-show-to-file (rev relpath repo-dir tmpdir tag)
  "Write `REV:RELPATH' from Git REPO-DIR into TMPDIR preserving RELPATH's hint.
Returns the written file path, or nil when `REV:RELPATH' does not exist."
  (when (and rev relpath repo-dir)
    (let* ((default-directory repo-dir)
           (dest (omnidiff--blob-dest tag relpath tmpdir)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'no-conversion))
          (when (zerop (call-process "git" nil t nil "show" (concat rev ":" relpath)))
            (omnidiff--write-small-temp dest (buffer-string))))))))

(defun omnidiff--worktree-after-path (file repo-dir &optional cmd)
  "Return real working-tree path for FILE's AFTER side when applicable, else nil."
  (let* ((status (omnidiff-file-status file))
         (target (omnidiff-file-target-path file))
         (full (when (and repo-dir target (not (string-empty-p target)))
                 (omnidiff--expand-repo-path target repo-dir))))
    (when (and (not (eq status 'deleted))
               full
               (file-regular-p full)
               (or (omnidiff--unstaged-worktree-diff-p cmd)
                   (and (null (omnidiff-file-new-oid file))
                        (consp cmd)
                        (equal (nth 2 cmd) "diff"))))
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
         (b-disk (omnidiff-file-before-disk-path file))
         (a-disk (omnidiff-file-after-disk-path file))
         (wt-after (omnidiff--worktree-after-path file repo-dir cmd))
         (full-after (when (and repo-dir new-path (not (string-empty-p new-path)))
                       (omnidiff--expand-repo-path new-path repo-dir)))
         (before-file
          (cond
           ((eq status 'added) "/dev/null")
           ((and (stringp b-disk) (file-exists-p b-disk)) b-disk)
           ((omnidiff--git-cat-blob-to-file old-oid old-path repo-dir tmpdir "before"))
           ((stringp (omnidiff-file-before-source file))
            (let ((dest (omnidiff--blob-dest "before-src" old-path tmpdir)))
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
           ((omnidiff--git-cat-blob-to-file new-oid new-path repo-dir tmpdir "after"))
           ((and full-after (file-regular-p full-after)) full-after)
           ((stringp (omnidiff-file-after-source file))
            (let ((dest (omnidiff--blob-dest "after-src" new-path tmpdir)))
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
TMPDIR is cleaned up when the terminal buffer is killed."
  (let* ((bin (or (executable-find omnidiff-executable)
                  (user-error "Cannot find %s in exec-path" omnidiff-executable)))
         (argv (list bin "--mode" "tui" "--" before-file after-file))
         (buf-name (format "*omnidiff tui: %s*" (or label "diff")))
         (base-clean (format "omnidiff tui: %s" (or label "diff")))
         (default-directory (or dir default-directory))
         (launcher (if (eq omnidiff-tui-launcher 'auto)
                       (cond
                        ((or (fboundp 'eat-make) (require 'eat nil t)) 'eat)
                        ((or (fboundp 'vterm) (require 'vterm nil t)) 'vterm)
                        (t 'ansi-term))
                     omnidiff-tui-launcher))
         (tui-buf nil)
         (launched nil))
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
                 ((and (not (eq launcher 'ansi-term)) (functionp launcher))
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
          (setq launched t)
          (when (and tmpdir (buffer-live-p tui-buf))
            (with-current-buffer tui-buf
              (push tmpdir omnidiff--tui-tmpdirs)
              (setq-local omnidiff--tui-tmpdir tmpdir)
              (add-hook 'kill-buffer-hook #'omnidiff--cleanup-tui-tmpdir nil t)))
          tui-buf)
      (when (and tmpdir (or (not launched) (not (buffer-live-p tui-buf))))
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
  (let* ((bb (omnidiff-dual--session-before-buf session))
         (ab (omnidiff-dual--session-after-buf session))
         (bp (omnidiff-dual--session-before-path session))
         (ap (omnidiff-dual--session-after-path session))
         (hint (or (omnidiff-dual--session-display-path session) ap bp "diff"))
         (tmpdir nil)
         (before-file nil)
         (after-file nil))
    (cl-flet ((ensure-tmp ()
                (or tmpdir (setq tmpdir (make-temp-file "omnidiff-tui-" t)))))
      (condition-case err
          (setq before-file
                (cond
                 ((and (stringp bp) (string= bp "/dev/null")) "/dev/null")
                 ((and (stringp bp) (file-exists-p bp)
                       (or (not (buffer-live-p bb)) (not (buffer-modified-p bb))))
                  bp)
                 ((buffer-live-p bb)
                  (omnidiff-engine-write-temp-for-buffer bb (ensure-tmp) hint))
                 (t "/dev/null"))
                after-file
                (cond
                 ((and (stringp ap) (string= ap "/dev/null")) "/dev/null")
                 ((and (stringp ap) (file-exists-p ap)
                       (or (not (buffer-live-p ab)) (not (buffer-modified-p ab))))
                  ap)
                 ((buffer-live-p ab)
                  (omnidiff-engine-write-temp-for-buffer ab (ensure-tmp) hint))
                 (t "/dev/null")))
        (error
         (when (and tmpdir (file-directory-p tmpdir))
           (ignore-errors (delete-directory tmpdir t)))
         (signal (car err) (cdr err))))
      (omnidiff--launch-tui-pair before-file after-file hint default-directory tmpdir))))

(defun omnidiff-open-in-tui ()
  "Open the file at point in the interactive `omnidiff' terminal UI."
  (interactive)
  (let ((file (omnidiff--file-at-point)))
    (unless file
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
active `omnidiff-dual--session'."
  (let* ((jd-existing (omnidiff-file-json-diff file))
         (pic (or (omnidiff-file-picture file)
                  (and jd-existing (omnidiff-json-diff-picture jd-existing))))
         (cont (or (omnidiff-file-container file)
                   (and jd-existing (omnidiff-json-diff-container jd-existing))))
         (target (omnidiff-file-target-path file)))
    (cond
     (cont
      (user-error "Cannot open container %s in Dual (use RET on a member or T for TUI)" target))
     ((and pic (not (omnidiff-picture-svg-p pic)))
      (user-error "Cannot open image %s in Dual (use image keys or T for TUI)" target))
     ((not (omnidiff--file-text-dual-p file))
      (user-error "Cannot open binary file %s in Dual" target)))
    (let* ((diff (buffer-local-value 'omnidiff--diff overview-buf))
           (repo-dir (or (buffer-local-value 'omnidiff--repo-dir overview-buf) default-directory))
           (cmd (buffer-local-value 'omnidiff--command overview-buf))
           (preset (with-current-buffer overview-buf (omnidiff--effective-preset)))
           (whole (with-current-buffer overview-buf (omnidiff--effective-whole-updates)))
           (reindent (with-current-buffer overview-buf (omnidiff--effective-paint-reindent-moves)))
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
      (let* ((nav-callbacks
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
                 (when (omnidiff-dual--session-p sess)
                   (omnidiff-dual--teardown sess nil t))
                 (if (buffer-live-p overview-buf)
                     (progn
                       (pop-to-buffer overview-buf)
                       (with-current-buffer overview-buf
                         (omnidiff--restore-location target nil nil nil nil)))
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
                       (omnidiff--set-file-reviewed target new-state)))))
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
                              (omnidiff--next-unreviewed-file cands target)))))
                     (when next-file
                       (omnidiff--open-dual-for-overview-file
                        overview-buf next-file :reuse-session sess)
                       next-file))))
               :open-tui
               (lambda (sess)
                 (let ((bb (and (omnidiff-dual--session-p sess)
                                (omnidiff-dual--session-before-buf sess)))
                       (ab (and (omnidiff-dual--session-p sess)
                                (omnidiff-dual--session-after-buf sess))))
                   (if (and (buffer-live-p overview-buf)
                            (not (and (buffer-live-p bb) (buffer-modified-p bb)))
                            (not (and (buffer-live-p ab) (buffer-modified-p ab))))
                       (omnidiff--launch-tui-for-file file repo-dir cmd)
                     (omnidiff--launch-tui-for-session sess))))
               :on-quit
               (lambda (sess)
                 (when (and (omnidiff-dual--session-p sess)
                            (omnidiff-dual--session-mutated-p sess))
                   (omnidiff--maybe-refresh-magit
                    (and (buffer-live-p overview-buf)
                         (buffer-local-value 'omnidiff--origin-buffer overview-buf)))))))
             (session
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
               :nav-callbacks nav-callbacks
               :reuse-session reuse-session)))
        (when (and session (integerp target-line) (> target-line 0))
          (let* ((side (if (eq target-side 'before) 'before 'after))
                 (buf (omnidiff-dual--side-buf session side))
                 (starts (omnidiff-dual--side-line-starts session side)))
            (when (and (buffer-live-p buf) starts (> (length starts) 0))
              (omnidiff-dual--focus-side session side)
              (let ((pos (omnidiff-dual--row-byte-col-to-point
                          buf starts (1- target-line) 0)))
                (with-current-buffer buf
                  (omnidiff-dual--expand-fold-at pos)
                  (goto-char pos)
                  (ignore-errors (recenter))))
              (omnidiff-dual--sync-focus-and-counterpart session nil t))))
        session))))

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

(defun omnidiff--open-standalone-overview (file label dir &optional owned-tmpdir)
  "Render standalone `omnidiff-file' FILE in a dedicated `omnidiff-mode' buffer."
  (let* ((buf-name (format "*omnidiff: %s*" label))
         (buf (get-buffer-create buf-name))
         (diff (make-omnidiff-diff :files (list file))))
    (with-current-buffer buf
      (unless (derived-mode-p 'omnidiff-mode)
        (omnidiff-mode))
      (omnidiff--cancel-request)
      (omnidiff-image-cleanup nil nil)
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
      (let ((file (omnidiff--file-from-json
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
               (raw-base (omnidiff-engine--sanitize-filename-hint hint))
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
            (let* ((preset (omnidiff--effective-preset))
                   (whole (omnidiff--effective-whole-updates))
                   (reindent (omnidiff--effective-paint-reindent-moves))
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
  (let* ((jd (omnidiff-file-json-diff file))
         (cont (or (omnidiff-file-container file)
                   (and jd (omnidiff-json-diff-container jd))))
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
                            (let ((abs-pdf (expand-file-name
                                            (if (string-prefix-p "~" pdf)
                                                (concat "./" pdf)
                                              pdf))))
                              (if pdftoppm
                                  (call-process pdftoppm nil nil nil
                                                "-png" "-r" "150"
                                                "-f" page-str "-l" page-str
                                                "-singlefile" abs-pdf prefix)
                                (call-process mutool nil nil nil
                                              "draw" "-F" "png" "-r" "150"
                                              "-o" out-png abs-pdf page-str))))))
                (rasterize b-pdf b-png (expand-file-name "before" tmpdir))
                (rasterize a-pdf a-png (expand-file-name "after" tmpdir)))
              (let ((eff-b (if (file-exists-p b-png) b-png "/dev/null"))
                    (eff-a (if (file-exists-p a-png) a-png "/dev/null")))
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
  (let* ((jd (omnidiff-file-json-diff file))
         (cont (or (omnidiff-file-container file)
                   (and jd (omnidiff-json-diff-container jd))))
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
       (let ((detail-str (omnidiff-render--format-container-detail
                          (omnidiff-json-container-member-detail-kind member)
                          (omnidiff-json-container-member-detail member))))
         (message "%s member `%s' (%s)%s — press T to inspect visually in native omnidiff TUI"
                  (omnidiff-render--container-family-label family)
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
  (add-hook 'kill-buffer-hook #'omnidiff--cancel-request nil t)
  (add-hook 'change-major-mode-hook #'omnidiff--cancel-request nil t)
  (add-hook 'kill-buffer-hook #'omnidiff-image-cleanup nil t)
  (add-hook 'change-major-mode-hook #'omnidiff-image-cleanup nil t)
  (add-hook 'kill-buffer-hook #'omnidiff--cleanup-owned-tmpdirs nil t)
  (add-hook 'change-major-mode-hook #'omnidiff--cleanup-owned-tmpdirs nil t))

;;; Process runner & Git / Magit commands

(defun omnidiff--repo-root ()
  "Return the Git repository root for `default-directory'."
  (or (and (fboundp 'magit-toplevel) (magit-toplevel))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun omnidiff--build-ext-diff-wrapper (bin flags &optional end-sentinel-p)
  "Build a `GIT_EXTERNAL_DIFF' shell command invoking BIN with FLAGS.
When END-SENTINEL-P is non-nil, emit `omnidiff-engine-end-sentinel' after
BIN exits so multi-commit `git show' output cleanly delimits JSON payloads."
  (let* ((cmd-str (mapconcat #'shell-quote-argument (cons bin flags) " "))
         (tail (if end-sentinel-p
                   (concat cmd-str " -- \"$@\"; rc=$?; printf '\\034OMNIDIFF-END\\000'; exit $rc")
                 (concat "exec " cmd-str " -- \"$@\"")))
         (script
          (concat
           "printf '\\034OMNIDIFF-FILE\\000%s\\000%s\\000%s\\000%s\\000' "
           "\"$1\" \"$2\" \"$5\" \"${8:-}\"; "
           "if [ -n \"$4\" ] && [ -n \"$7\" ] && [ \"$4\" != \"$7\" ] "
           "&& [ \"$4\" != \".\" ] && [ \"$7\" != \".\" ]; then "
           "printf '\\034OMNIDIFF-MODE\\000%s\\000%s\\000' \"$4\" \"$7\"; fi; "
           "if [ \"${#3}\" -ge 40 ] || [ \"${#6}\" -ge 40 ]; then "
           "printf '\\034OMNIDIFF-OID\\000%s\\000%s\\000' \"$3\" \"$6\"; fi; "
           tail)))
    (concat "sh -c " (shell-quote-argument script) " _")))

(defconst omnidiff--zero-sha "0000000000000000000000000000000000000000"
  "Null Git object SHA indicating an absent or working-tree file.")

(defun omnidiff--zero-oid-p (oid)
  "Return non-nil when OID is empty or an all-zero Git object ID (SHA-1/SHA-256)."
  (or (null oid)
      (string-empty-p oid)
      (not (null (string-match-p "\\`0+\\'" oid)))))

(defun omnidiff--valid-oid-p (oid)
  "Return non-nil when OID is a valid 40-char SHA-1 or 64-char SHA-256 hex ID."
  (and (stringp oid)
       (or (= (length oid) 40) (= (length oid) 64))
       (not (null (string-match-p "\\`[0-9a-f]+\\'" oid)))))

(defconst omnidiff--raw-byte-regexp (string ?\[ #x3fff80 ?- #x3fffff ?\])
  "Regexp matching Emacs raw-byte characters produced by invalid UTF-8.")

(defconst omnidiff--utf16le-bom (unibyte-string #xff #xfe)
  "Unibyte UTF-16LE / UTF-32LE BOM prefix.")

(defconst omnidiff--utf16be-bom (unibyte-string #xfe #xff)
  "Unibyte UTF-16BE BOM prefix.")

(defconst omnidiff--utf32be-bom (unibyte-string #x00 #x00 #xfe #xff)
  "Unibyte UTF-32BE BOM prefix.")

(defun omnidiff--parallel-diff-args-p (args)
  "Return non-nil when `git diff' tail ARGS are safe for `--raw --no-abbrev -z'."
  (let ((ok t)
        (in-paths nil))
    (dolist (a args)
      (cond
       ((not ok))
       (in-paths)
       ((string= a "--")
        (setq in-paths t))
       ((or (member a '("--cached" "--staged"))
            (and (stringp a) (not (string-empty-p a)) (not (string-prefix-p "-" a)))))
       (t
        (setq ok nil))))
    ok))

(defun omnidiff--parallel-show-args-p (args)
  "Return non-nil when `git show' tail ARGS target a single revision."
  (or (null args)
      (and (stringp (car args))
           (not (string-empty-p (car args)))
           (not (string-prefix-p "-" (car args)))
           (not (string-search ".." (car args)))
           (or (null (cdr args))
               (equal (cadr args) "--")))))

(defun omnidiff--git-parallel-spec (command)
  "Return `(KIND . RAW-GIT-COMMAND)' when COMMAND supports parallel diffing.
KIND is `diff' or `show', and RAW-GIT-COMMAND replaces `--ext-diff' with
`--raw --no-abbrev -z'.  Unsupported options and multi-commit `git show'
ranges return nil so Git's serial `GIT_EXTERNAL_DIFF' handles them."
  (when (and (> omnidiff-max-parallel 1)
             (equal (nth 0 command) "git")
             (equal (nth 1 command) "--no-pager")
             (member (nth 2 command) '("diff" "show"))
             (equal (nth 3 command) "--ext-diff"))
    (let ((kind (intern (nth 2 command)))
          (rest (nthcdr 4 command)))
      (when (if (eq kind 'diff)
                (omnidiff--parallel-diff-args-p rest)
              (omnidiff--parallel-show-args-p rest))
        (cons kind
              (append (list "git" "--no-pager" (nth 2 command)
                            "--raw" "--no-abbrev" "-z")
                      rest))))))

(defun omnidiff--parse-raw-git-entries (raw kind)
  "Parse unibyte RAW output of `git --raw --no-abbrev -z' for KIND.
KIND is `diff' or `show'.  Return `(PREAMBLE . ENTRIES)', or the symbol
`fallback-serial' when RAW contains combined diffs (`::'), unmerged paths,
multiple `git show' commits, or unrecognized raw metadata."
  (if (or (string-match-p "\\(?:\\`\\|[\n\0]\\)::[0-7]" raw)
          (and (eq kind 'show) (string-search "\0commit " raw)))
      'fallback-serial
    (let ((len (length raw))
          (pos 0)
          (preamble nil)
          (entries nil)
          (fallback nil))
      (when (eq kind 'show)
        (if (string-match
             "\\(?:\\`\\|[\n\0]\\):[0-7]\\{6\\} [0-7]\\{6\\} \\(?:[0-9a-f]\\{64\\}\\|[0-9a-f]\\{40\\}\\) "
             raw)
            (let ((mstart (match-beginning 0)))
              (setq pos (if (memq (aref raw mstart) '(?\n ?\0)) (1+ mstart) mstart))
              (when (> pos 0)
                (let ((pre (string-trim-right (substring raw 0 pos) "\0")))
                  (unless (string-empty-p pre)
                    (setq preamble (decode-coding-string pre 'utf-8))))))
          (let ((pre (string-trim-right raw "\0")))
            (unless (string-empty-p pre)
              (setq preamble (decode-coding-string pre 'utf-8))))
          (setq pos len)))
      (while (and (< pos len) (not fallback))
        (while (and (< pos len) (memq (aref raw pos) '(?\n ?\0)))
          (setq pos (1+ pos)))
        (when (< pos len)
          (let ((nul1 (string-search "\0" raw pos)))
            (if (null nul1)
                (setq fallback t)
              (let ((meta (substring raw pos nul1)))
                (setq pos (1+ nul1))
                (cond
                 ((string-prefix-p "::" meta)
                  (setq fallback t))
                 ((string-prefix-p ":" meta)
                  (let* ((cols (split-string (substring meta 1) " "))
                         (old-mode (nth 0 cols))
                         (new-mode (nth 1 cols))
                         (old-sha (nth 2 cols))
                         (new-sha (nth 3 cols))
                         (status (nth 4 cols)))
                    (if (or (not (and old-mode new-mode status
                                      (string-match-p "\\`[0-7]\\{6\\}\\'" old-mode)
                                      (string-match-p "\\`[0-7]\\{6\\}\\'" new-mode)
                                      (omnidiff--valid-oid-p old-sha)
                                      (omnidiff--valid-oid-p new-sha)
                                      (not (string-empty-p status))))
                            (equal status "U"))
                        (setq fallback t)
                      (let* ((nul2 (string-search "\0" raw pos))
                             (path (when nul2
                                     (omnidiff--decode-git-path (substring raw pos nul2))))
                             (new-path path))
                        (if (null nul2)
                            (setq fallback t)
                          (setq pos (1+ nul2))
                          (when (memq (aref status 0) '(?R ?C))
                            (let ((nul3 (string-search "\0" raw pos)))
                              (if (null nul3)
                                  (setq fallback t)
                                (setq new-path (omnidiff--decode-git-path
                                                (substring raw pos nul3))
                                      pos (1+ nul3)))))
                          (unless fallback
                            (push (make-omnidiff--git-entry
                                   :status status
                                   :old-mode old-mode
                                   :new-mode new-mode
                                   :old-sha old-sha
                                   :new-sha new-sha
                                   :path path
                                   :new-path new-path)
                                  entries)))))))
                 (t
                  (setq fallback t))))))))
      (if fallback
          'fallback-serial
        (cons preamble (nreverse entries))))))

(defun omnidiff--blob-key (sha path)
  "Return cache key `(SHA . HINT)' preserving PATH's extension or basename.
`omnidiff' infers parser language from the temp file's extension or exact
basename (e.g. `Makefile', `Dockerfile')."
  (let* ((base (file-name-nondirectory path))
         (ext (file-name-extension base t)))
    (cons sha (if (and ext (not (string-empty-p ext))) ext base))))

(defun omnidiff--blob-dest (sha path tmpdir)
  "Return a temp path under TMPDIR for SHA preserving PATH's language hint."
  (let* ((raw-base (file-name-nondirectory (or path "")))
         (clean-base (replace-regexp-in-string "[\\\\/\x00]" "_" raw-base))
         (safe-base (cond
                     ((or (string-empty-p clean-base)
                          (string= clean-base ".")
                          (string= clean-base ".."))
                      "blob")
                     ((string-prefix-p "~" clean-base)
                      (concat "_" (substring clean-base 1)))
                     (t clean-base)))
         (ext (file-name-extension safe-base t)))
    (if (and ext (not (string-empty-p ext)))
        (expand-file-name (concat "./" sha ext) tmpdir)
      (let ((sub (expand-file-name (concat "./" sha) tmpdir)))
        (make-directory sub t)
        (expand-file-name (concat "./" safe-base) sub)))))

(defun omnidiff--write-small-temp (dest content)
  "Write unibyte/UTF-8 string CONTENT to DEST without fsync or handlers."
  (let ((file-name-handler-alist nil)
        (write-region-inhibit-fsync t)
        (coding-system-for-write 'no-conversion))
    (write-region content nil dest nil 'silent)
    dest))

(defun omnidiff--binary-prefix-p (bytes)
  "Return non-nil when unibyte BYTES looks binary, PDF, control, or UTF-16/32."
  (or (string-prefix-p "%PDF-" bytes)
      (string-prefix-p omnidiff--utf16le-bom bytes)
      (string-prefix-p omnidiff--utf16be-bom bytes)
      (string-prefix-p omnidiff--utf32be-bom bytes)
      (not (null (string-match-p "[\x00-\x08\x0b\x0c\x0e-\x1f\x7f]" bytes)))))

(defun omnidiff--read-text-span-from-buffer (buf start end)
  "Extract unibyte text from BUF between START and END if plain text, else nil."
  (with-current-buffer buf
    (let ((len (- end start)))
      (if (<= len 8192)
          (buffer-substring-no-properties start end)
        (unless (omnidiff--binary-prefix-p
                 (buffer-substring-no-properties start (+ start 8192)))
          (buffer-substring-no-properties start end))))))

(defun omnidiff--read-text-file-from-disk (path)
  "Read regular file PATH as unibyte text if plain text, else nil."
  (when (and (file-exists-p path)
             (not (file-directory-p path))
             (not (file-symlink-p path)))
    (let ((file-name-handler-alist nil))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path nil 0 8192)
        (let ((prefix (buffer-string)))
          (if (< (buffer-size) 8192)
              prefix
            (unless (omnidiff--binary-prefix-p prefix)
              (erase-buffer)
              (insert-file-contents-literally path)
              (buffer-string))))))))

(defun omnidiff--source-line-count (source)
  "Return logical line count of decoded string SOURCE."
  (if (or (null source) (string-empty-p source))
      0
    (let ((len (length source))
          (count 0)
          (pos 0))
      (while (setq pos (string-search "\n" source pos))
        (setq count (1+ count)
              pos (1+ pos)))
      (if (eq (aref source (1- len)) ?\n)
          count
        (1+ count)))))

(defun omnidiff--compute-json-side-row-counts (hunks num-rows)
  "Return `(INS DEL UPD MOV)' row counts for JSON HUNKS across NUM-ROWS."
  (if (or (null hunks) (<= num-rows 0))
      (list 0 0 0 0)
    (let ((row-ops (make-vector num-rows nil))
          (row-moved (make-vector num-rows nil)))
      (dolist (h hunks)
        (let* ((op (omnidiff-json-hunk-operation h))
               (prio (omnidiff-render--op-priority op))
               (rng (omnidiff-json-hunk-range h))
               (sr (omnidiff-json-range-start-row rng))
               (last (omnidiff-render--range-last-row rng num-rows)))
          (when last
            (let ((r sr))
              (while (<= r last)
                (let ((cur (aref row-ops r)))
                  (when (or (null cur) (> prio (omnidiff-render--op-priority cur)))
                    (aset row-ops r op)))
                (when (eq op 'move)
                  (aset row-moved r t))
                (setq r (1+ r)))))))
      (let ((ins 0) (del 0) (upd 0) (mov 0))
        (dotimes (r num-rows)
          (pcase (aref row-ops r)
            ('insert (setq ins (1+ ins)))
            ('delete (setq del (1+ del)))
            ('update (setq upd (1+ upd))))
          (when (aref row-moved r)
            (setq mov (1+ mov))))
        (list ins del upd mov)))))

(defun omnidiff--file-from-json (path old-file new-file new-path json-diff
                                      &optional old-mode new-mode old-oid new-oid
                                      before-disk-path after-disk-path
                                      pre-before-src pre-after-src preamble)
  "Construct a full `omnidiff-file' from JSON-DIFF and file metadata.
Decodes BEFORE and AFTER sources from OLD-FILE and NEW-FILE using the JSON
side encodings whenever the entry is not binary and PRE-BEFORE-SRC /
PRE-AFTER-SRC are not already supplied."
  (let* ((status (cond
                  ((string= old-file "/dev/null") 'added)
                  ((string= new-file "/dev/null") 'deleted)
                  ((and (stringp new-path)
                        (not (string-empty-p new-path))
                        (not (string= new-path path)))
                   'renamed)
                  (t 'modified)))
         (target (if (eq status 'renamed) new-path path))
         (b-side (omnidiff-json-diff-before json-diff))
         (a-side (omnidiff-json-diff-after json-diff))
         (binary-p (omnidiff-json-diff-binary-p json-diff))
         (picture (omnidiff-json-diff-picture json-diff))
         (container (omnidiff-json-diff-container json-diff))
         (summary (omnidiff-json-diff-summary json-diff))
         (language (or (and a-side (omnidiff-json-side-language a-side))
                       (and b-side (omnidiff-json-side-language b-side))))
         (large-residual (omnidiff-json-diff-large-residual json-diff))
         (before-src (unless binary-p
                       (or (and (stringp pre-before-src) pre-before-src)
                           (ignore-errors
                             (omnidiff-engine-read-file-decoded
                              old-file (and b-side (omnidiff-json-side-encoding b-side))))
                           "")))
         (after-src (unless binary-p
                      (or (and (stringp pre-after-src) pre-after-src)
                          (ignore-errors
                            (omnidiff-engine-read-file-decoded
                             new-file (and a-side (omnidiff-json-side-encoding a-side))))
                          "")))
         (added 0) (removed 0) (updated 0) (moved 0) (max-line 0))
    (cond
     (container
      (dolist (m (omnidiff-json-container-members container))
        (pcase (omnidiff-json-container-member-status m)
          ('added (setq added (1+ added)))
          ('removed (setq removed (1+ removed)))
          ('changed (setq updated (1+ updated))))))
     ((not binary-p)
      (let* ((b-count (omnidiff--source-line-count before-src))
             (a-count (omnidiff--source-line-count after-src))
             (bc (omnidiff--compute-json-side-row-counts
                  (and b-side (omnidiff-json-side-hunks b-side)) b-count))
             (ac (omnidiff--compute-json-side-row-counts
                  (and a-side (omnidiff-json-side-hunks a-side)) a-count)))
        (setq added (+ (nth 0 bc) (nth 0 ac))
              removed (+ (nth 1 bc) (nth 1 ac))
              updated (max (nth 2 bc) (nth 2 ac))
              moved (max (nth 3 bc) (nth 3 ac))
              max-line (max b-count a-count)))))
    (let ((note (cond
                 ((and binary-p (null picture) (null container))
                  (format "Binary file %s differs" target))
                 ((and (eq summary 'no-changes)
                       (= added 0) (= removed 0) (= updated 0) (= moved 0))
                  "No changes - files are identical"))))
      (make-omnidiff-file
       :path path
       :target-path target
       :status status
       :preamble preamble
       :old-mode (omnidiff-engine--normalize-mode old-mode)
       :new-mode (omnidiff-engine--normalize-mode new-mode)
       :old-oid (omnidiff-engine--normalize-oid old-oid)
       :new-oid (omnidiff-engine--normalize-oid new-oid)
       :picture picture
       :container container
       :json-diff json-diff
       :before-source before-src
       :after-source after-src
       :before-disk-path before-disk-path
       :after-disk-path after-disk-path
       :summary summary
       :language language
       :large-residual large-residual
       :binary-p binary-p
       :note note
       :added added
       :removed removed
       :updated updated
       :moved moved
       :max-line max-line))))

(defun omnidiff--batch-read-git-blobs (oids repo-dir)
  "Batch-read Git blob OIDS in REPO-DIR into a hash table `OID -> UNIBYTE'."
  (let ((table (make-hash-table :test #'equal :size (max 16 (length oids)))))
    (when (and oids repo-dir (file-directory-p repo-dir))
      (let ((default-directory repo-dir)
            (coding-system-for-read 'no-conversion)
            (coding-system-for-write 'no-conversion))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (when (zerop (call-process-region
                        (concat (string-join oids "\n") "\n")
                        nil "git" nil t nil "cat-file" "--batch"))
            (goto-char (point-min))
            (dolist (oid oids)
              (when (< (point) (point-max))
                (let* ((eol (line-end-position))
                       (hdr (split-string (buffer-substring-no-properties (point) eol) " ")))
                  (forward-line 1)
                  (unless (equal (nth 1 hdr) "missing")
                    (let* ((sz (string-to-number (or (nth 2 hdr) "0")))
                           (start (point))
                           (end (min (point-max) (+ start sz))))
                      (puthash oid (buffer-substring-no-properties start end) table)
                      (goto-char (min (point-max) (1+ end))))))))))))
    table))

(defun omnidiff--read-serial-side-source (oid mode rel-path raw-file encoding repo-dir blob-cache)
  "Resolve decoded text source for one side of a serial JSON diff entry."
  (let ((norm-oid (omnidiff-engine--normalize-oid oid)))
    (cond
     ((or (null raw-file) (string= raw-file "/dev/null")) "")
     ((equal mode "160000")
      (format "Subproject commit %s\n" (or norm-oid oid "")))
     ((when-let* ((raw (and norm-oid (gethash norm-oid blob-cache))))
        (omnidiff-engine-decode-bytes raw encoding)))
     ((and repo-dir rel-path (not (string-empty-p rel-path)))
      (let ((full (omnidiff--expand-repo-path rel-path repo-dir)))
        (cond
         ((when-let* ((link (file-symlink-p full)))
            link))
         ((file-regular-p full)
          (or (ignore-errors (omnidiff-engine-read-file-decoded full encoding)) ""))
         (norm-oid
          (format "Subproject commit %s\n" norm-oid))
         (t ""))))
     (t ""))))

(defun omnidiff--parse-serial-json-output (raw repo-dir)
  "Parse serial `GIT_EXTERNAL_DIFF' JSON output RAW in REPO-DIR."
  (let* ((sentinel omnidiff-engine-file-sentinel)
         (slen (length sentinel))
         (msent omnidiff-engine-mode-sentinel)
         (mlen (length msent))
         (osent omnidiff-engine-oid-sentinel)
         (olen (length osent))
         (esent omnidiff-engine-end-sentinel)
         (elen (length esent))
         (first (string-search sentinel raw)))
    (if (null first)
        (make-omnidiff-diff :preamble raw :files nil)
      (let ((preamble (substring raw 0 first))
            (pos first)
            (len (length raw))
            (pending-pre nil)
            (records nil)
            (needed-oids nil)
            (seen-oids (make-hash-table :test #'equal)))
        (while pos
          (let ((cursor (+ pos slen))
                (fields nil))
            (dotimes (_ 4)
              (let ((nul (string-search "\x00" raw cursor)))
                (if nul
                    (progn
                      (push (substring raw cursor nul) fields)
                      (setq cursor (1+ nul)))
                  (push (substring raw cursor len) fields)
                  (setq cursor len))))
            (let ((old-mode nil)
                  (new-mode nil)
                  (old-oid nil)
                  (new-oid nil))
              (when (and (<= (+ cursor mlen) len)
                         (string= (substring raw cursor (+ cursor mlen)) msent))
                (setq cursor (+ cursor mlen))
                (let ((mfields nil))
                  (dotimes (_ 2)
                    (let ((nul (string-search "\x00" raw cursor)))
                      (if nul
                          (progn
                            (push (substring raw cursor nul) mfields)
                            (setq cursor (1+ nul)))
                        (push (substring raw cursor len) mfields)
                        (setq cursor len))))
                  (setq old-mode (nth 1 mfields)
                        new-mode (nth 0 mfields))))
              (when (and (<= (+ cursor olen) len)
                         (string= (substring raw cursor (+ cursor olen)) osent))
                (setq cursor (+ cursor olen))
                (let ((ofields nil))
                  (dotimes (_ 2)
                    (let ((nul (string-search "\x00" raw cursor)))
                      (if nul
                          (progn
                            (push (substring raw cursor nul) ofields)
                            (setq cursor (1+ nul)))
                        (push (substring raw cursor len) ofields)
                        (setq cursor len))))
                  (setq old-oid (nth 1 ofields)
                        new-oid (nth 0 ofields))))
              (let* ((next (string-search sentinel raw cursor))
                     (end-marker (string-search esent raw cursor))
                     (has-end (and end-marker (or (null next) (< end-marker next))))
                     (body (substring raw cursor (if has-end end-marker (or next len))))
                     (after-end (when has-end
                                  (substring raw (+ end-marker elen) (or next len))))
                     (rec-pre pending-pre))
                (setq pending-pre
                      (when (and after-end (not (string-blank-p after-end)))
                        (string-trim-left after-end "[\n\r]+")))
                (pcase-let* ((`(,path ,old-file ,new-file ,new-path) (nreverse fields))
                             (parsed-jd nil)
                             (parse-err-msg nil))
                  (condition-case err
                      (setq parsed-jd (omnidiff-engine-parse-json body))
                    (error
                     (setq parse-err-msg (format "omnidiff parse error: %s"
                                                 (error-message-string err)))))
                  (when (and parsed-jd (not (omnidiff-json-diff-binary-p parsed-jd)))
                    (dolist (pair (list (cons old-oid old-mode) (cons new-oid new-mode)))
                      (let ((oid (omnidiff-engine--normalize-oid (car pair)))
                            (mode (cdr pair)))
                        (when (and oid (not (equal mode "160000"))
                                   (not (gethash oid seen-oids)))
                          (puthash oid t seen-oids)
                          (push oid needed-oids)))))
                  (push (list path old-file new-file new-path old-mode new-mode
                              old-oid new-oid parsed-jd parse-err-msg rec-pre)
                        records))
                (setq pos next)))))
        (let ((blob-cache (omnidiff--batch-read-git-blobs (nreverse needed-oids) repo-dir))
              (files nil))
          (dolist (rec (nreverse records))
            (pcase-let ((`(,path ,old-file ,new-file ,new-path ,old-mode ,new-mode
                                 ,old-oid ,new-oid ,jd ,err-msg ,rec-pre)
                         rec))
              (if jd
                  (let* ((bin-p (omnidiff-json-diff-binary-p jd))
                         (b-side (omnidiff-json-diff-before jd))
                         (a-side (omnidiff-json-diff-after jd))
                         (target (if (and (stringp new-path)
                                          (not (string-empty-p new-path))
                                          (not (string= new-path path)))
                                     new-path
                                   path))
                         (b-src (unless bin-p
                                  (omnidiff--read-serial-side-source
                                   old-oid old-mode path old-file
                                   (and b-side (omnidiff-json-side-encoding b-side))
                                   repo-dir blob-cache)))
                         (a-src (unless bin-p
                                  (omnidiff--read-serial-side-source
                                   new-oid new-mode target new-file
                                   (and a-side (omnidiff-json-side-encoding a-side))
                                   repo-dir blob-cache))))
                    (push (omnidiff--file-from-json
                           path old-file new-file new-path jd
                           old-mode new-mode old-oid new-oid nil nil
                           b-src a-src rec-pre)
                          files))
                (push (make-omnidiff-file
                       :path path
                       :target-path (if (and (stringp new-path)
                                             (not (string-empty-p new-path))
                                             (not (string= new-path path)))
                                        new-path
                                      path)
                       :status (cond
                                ((string= old-file "/dev/null") 'added)
                                ((string= new-file "/dev/null") 'deleted)
                                ((and (stringp new-path)
                                      (not (string-empty-p new-path))
                                      (not (string= new-path path)))
                                 'renamed)
                                (t 'modified))
                       :preamble rec-pre
                       :old-mode (omnidiff-engine--normalize-mode old-mode)
                       :new-mode (omnidiff-engine--normalize-mode new-mode)
                       :old-oid (omnidiff-engine--normalize-oid old-oid)
                       :new-oid (omnidiff-engine--normalize-oid new-oid)
                       :note err-msg)
                      files))))
          (make-omnidiff-diff :preamble preamble :files (nreverse files)))))))

(defun omnidiff--make-fast-json-hunks (text op)
  "Build contiguous non-blank `omnidiff-json-hunk's with OP for TEXT."
  (let* ((lines (omnidiff-render--split-source-lines text))
         (n (length lines))
         (hunks nil)
         (row 0))
    (while (< row n)
      (let ((blen (omnidiff-engine-paint-row-byte-len (aref lines row))))
        (if (= blen 0)
            (setq row (1+ row))
          (let ((sr row)
                (er row)
                (ec blen))
            (setq row (1+ row))
            (while (and (< row n)
                        (> (setq blen (omnidiff-engine-paint-row-byte-len (aref lines row))) 0))
              (setq er row
                    ec blen
                    row (1+ row)))
            (push (make-omnidiff-json-hunk
                   :operation op
                   :range (make-omnidiff-json-range
                           :start-row sr :start-col 0
                           :end-row er :end-col ec))
                  hunks)))))
    (nreverse hunks)))

(defun omnidiff--fast-parse-whole-file (path status raw-bytes
                                             &optional old-oid new-oid)
  "Construct a fast-path JSON `omnidiff-file' from unibyte RAW-BYTES, or nil.
Returns nil for binary, invalid UTF-8, stray CR, or all-blank files so the
`omnidiff' CLI handles classification and encoding directly."
  (when raw-bytes
    (let ((added-p (eq status 'added)))
      (if (zerop (length raw-bytes))
          (make-omnidiff-file
           :path path
           :target-path path
           :status status
           :old-oid (omnidiff-engine--normalize-oid old-oid)
           :new-oid (omnidiff-engine--normalize-oid new-oid)
           :before-source ""
           :after-source ""
           :summary 'no-changes
           :note "No changes - files are identical"
           :json-diff (make-omnidiff-json-diff
                       :before (make-omnidiff-json-side
                                :path (if added-p "/dev/null" path))
                       :after (make-omnidiff-json-side
                               :path (if added-p path "/dev/null"))
                       :summary 'no-changes))
        (unless (or (omnidiff--binary-prefix-p raw-bytes)
                    (string-match-p "\r[^\n]\\|\r\\'" raw-bytes))
          (let ((text (decode-coding-string raw-bytes 'utf-8-unix)))
            (unless (string-match-p omnidiff--raw-byte-regexp text)
              (let* ((norm-text (if (string-search "\r\n" text)
                                    (string-replace "\r\n" "\n" text)
                                  text))
                     (lines (omnidiff-render--split-source-lines norm-text))
                     (op (if added-p 'insert 'delete))
                     (hunks (omnidiff--make-fast-json-hunks norm-text op)))
                (when hunks
                  (let ((changed 0)
                        (sum (if added-p 'new-file 'deleted-file)))
                    (dotimes (i (length lines))
                      (when (> (omnidiff-engine-paint-row-byte-len (aref lines i)) 0)
                        (setq changed (1+ changed))))
                    (make-omnidiff-file
                     :path path
                     :target-path path
                     :status status
                     :old-oid (omnidiff-engine--normalize-oid old-oid)
                     :new-oid (omnidiff-engine--normalize-oid new-oid)
                     :before-source (if added-p "" norm-text)
                     :after-source (if added-p norm-text "")
                     :summary sum
                     :note (if added-p
                               "New file - everything inserted"
                             "Deleted file - everything removed")
                     :added (if added-p changed 0)
                     :removed (if added-p 0 changed)
                     :max-line (length lines)
                     :json-diff (make-omnidiff-json-diff
                                 :before (make-omnidiff-json-side
                                          :path (if added-p "/dev/null" path)
                                          :hunks (unless added-p hunks))
                                 :after (make-omnidiff-json-side
                                         :path (if added-p path "/dev/null")
                                         :hunks (when added-p hunks))
                                 :summary sum))))))))))))

(defun omnidiff--parse-batch-check-sizes (raw)
  "Parse unibyte output RAW of `git cat-file --batch-check' into a size table.
Returns a hash table mapping Git OID string to non-negative integer byte size."
  (let ((sizes (make-hash-table :test #'equal))
        (len (length raw))
        (pos 0))
    (while (< pos len)
      (let* ((nl (or (string-search "\n" raw pos) len))
             (line (substring raw pos nl)))
        (setq pos (1+ nl))
        (unless (string-empty-p line)
          (let ((cols (split-string line " ")))
            (when (>= (length cols) 2)
              (puthash (nth 0 cols)
                       (if (equal (nth 1 cols) "missing")
                           0
                         (string-to-number (or (nth 2 cols) "0")))
                       sizes))))))
    sizes))

(defun omnidiff--partition-sha-batches (sha-list size-table max-bytes)
  "Partition SHA-LIST into batches whose blob sums stay within MAX-BYTES.
SIZE-TABLE maps each SHA to its integer byte size.  Returns a list of
non-empty SHA batches, or the symbol `overflow' if any single blob exceeds
MAX-BYTES."
  (if (or (null max-bytes) (<= max-bytes 0))
      (when sha-list (list sha-list))
    (let ((batches nil)
          (cur nil)
          (cur-bytes 0)
          (overflow nil))
      (dolist (sha sha-list)
        (unless overflow
          (let ((sz (or (gethash sha size-table) 0)))
            (cond
             ((> sz max-bytes)
              (setq overflow t))
             ((and cur (> (+ cur-bytes sz) max-bytes))
              (push (nreverse cur) batches)
              (setq cur (list sha)
                    cur-bytes sz))
             (t
              (push sha cur)
              (setq cur-bytes (+ cur-bytes sz)))))))
      (cond
       (overflow 'overflow)
       (t
        (when cur (push (nreverse cur) batches))
        (nreverse batches))))))

(defun omnidiff--collect-sha-batch-maps (entries)
  "Build `(SHA-TARGETS . VISUAL-SHAS)' lookup tables for ENTRIES.
SHA-TARGETS maps each non-zero non-submodule OID to a deduplicated list of
`(BLOB-KEY . PATH)' pairs; VISUAL-SHAS marks OIDs used by image/SVG paths."
  (let ((targets (make-hash-table :test #'equal))
        (seen-keys (make-hash-table :test #'equal))
        (visual (make-hash-table :test #'equal)))
    (dolist (e entries)
      (let* ((om (omnidiff--git-entry-old-mode e))
             (nm (omnidiff--git-entry-new-mode e))
             (os (omnidiff--git-entry-old-sha e))
             (ns (omnidiff--git-entry-new-sha e))
             (path (omnidiff--git-entry-path e))
             (new-path (omnidiff--git-entry-new-path e))
             (vis-p (or (omnidiff-image-path-looks-visual-p path)
                        (omnidiff-image-path-looks-visual-p new-path))))
        (dolist (side (list (list os om path) (list ns nm new-path)))
          (pcase-let ((`(,sha ,mode ,p) side))
            (when (and (not (omnidiff--zero-oid-p sha))
                       (not (equal mode "160000")))
              (let ((key (omnidiff--blob-key sha p)))
                (unless (gethash key seen-keys)
                  (puthash key t seen-keys)
                  (puthash sha (cons (cons key p) (gethash sha targets)) targets)))
              (when vis-p
                (puthash sha t visual)))))))
    (cons targets visual)))

(defun omnidiff--materialize-cat-batch (cat-buf batch-shas sha-targets visual-shas
                                                ensure-tmpdir image-blobs blob-files
                                                seeded-cell visual-limit)
  "Extract blobs in BATCH-SHAS from CAT-BUF onto disk in BLOB-FILES.
Also seeds IMAGE-BLOBS for visual OIDs in VISUAL-SHAS up to VISUAL-LIMIT,
updating the counter stored in SEEDED-CELL."
  (let ((file-name-handler-alist nil)
        (write-region-inhibit-fsync t)
        (coding-system-for-write 'no-conversion))
    (with-current-buffer cat-buf
      (goto-char (point-min))
      (dolist (sha batch-shas)
        (when (< (point) (point-max))
          (let* ((eol (line-end-position))
                 (hdr (split-string (buffer-substring-no-properties (point) eol) " ")))
            (forward-line 1)
            (unless (equal (nth 1 hdr) "missing")
              (let* ((sz (string-to-number (or (nth 2 hdr) "0")))
                     (start (point))
                     (end (min (point-max) (+ start sz))))
                (when (and (hash-table-p image-blobs)
                           (gethash sha visual-shas)
                           (< (car seeded-cell) visual-limit)
                           (not (gethash sha image-blobs))
                           (> sz 0)
                           (or (<= omnidiff-image-max-bytes 0)
                               (<= sz omnidiff-image-max-bytes)))
                  (let ((raw (buffer-substring-no-properties start end)))
                    (unless (and (< sz 512)
                                 (string-prefix-p
                                  "version https://git-lfs.github.com/spec/v1\n" raw))
                      (puthash sha raw image-blobs)
                      (setcar seeded-cell (1+ (car seeded-cell))))))
                (dolist (kp (gethash sha sha-targets))
                  (let ((key (car kp))
                        (path (cdr kp)))
                    (unless (gethash key blob-files)
                      (let ((dest (omnidiff--blob-dest sha path (funcall ensure-tmpdir))))
                        (write-region start end dest nil 'silent)
                        (puthash key dest blob-files)))))
                (goto-char (min (point-max) (1+ end)))))))))))

(cl-defun omnidiff--prepare-parallel-work (cat-buf sha-list entries ensure-tmpdir dir active-preset
                                                   &optional image-blobs
                                                   pre-blob-files)
  "Build pre-populated results vector and worker job list for ENTRIES.
Pure plain-text added/deleted entries under `default'/`full' presets are
constructed in memory without writing temp blobs or spawning `omnidiff'.
Remaining entries materialize blobs lazily via ENSURE-TMPDIR.
When IMAGE-BLOBS is a hash table, seed visual image/SVG blobs from CAT-BUF
so inline image rendering avoids duplicate `git cat-file' calls.
Optional PRE-BLOB-FILES supplies pre-materialized `(SHA . HINT) -> PATH'
temp files produced by multi-batch `git cat-file --batch' streaming."
  (let ((spans (make-hash-table :test #'equal :size (max 16 (length sha-list))))
        (blob-files (or pre-blob-files
                        (make-hash-table :test #'equal :size (max 16 (length sha-list)))))
        (sub-files (make-hash-table :test #'equal))
        (results (make-vector (length entries) nil))
        (fast-ad-p (memq active-preset '(default full)))
        (jobs nil)
        (idx 0)
        (file-name-handler-alist nil)
        (write-region-inhibit-fsync t)
        (coding-system-for-write 'no-conversion))
    (when cat-buf
      (with-current-buffer cat-buf
        (goto-char (point-min))
        (dolist (sha sha-list)
          (when (< (point) (point-max))
            (let* ((eol (line-end-position))
                   (hdr (split-string (buffer-substring-no-properties (point) eol) " ")))
              (forward-line 1)
              (unless (equal (nth 1 hdr) "missing")
                (let* ((sz (string-to-number (or (nth 2 hdr) "0")))
                       (start (point))
                       (end (min (point-max) (+ start sz))))
                  (puthash sha (cons start end) spans)
                  (goto-char (min (point-max) (1+ end))))))))))
    (when (and cat-buf (hash-table-p image-blobs))
      (let ((limit (if (> omnidiff-image-auto-load-limit 0)
                       (* 2 omnidiff-image-auto-load-limit)
                     most-positive-fixnum))
            (seeded 0))
        (dolist (e entries)
          (when (and (< seeded limit)
                     (or (omnidiff-image-path-looks-visual-p (omnidiff--git-entry-path e))
                         (omnidiff-image-path-looks-visual-p (omnidiff--git-entry-new-path e))))
            (dolist (sha (list (omnidiff--git-entry-old-sha e)
                               (omnidiff--git-entry-new-sha e)))
              (when-let* ((span (and (not (omnidiff--zero-oid-p sha))
                                     (not (gethash sha image-blobs))
                                     (gethash sha spans))))
                (let ((len (- (cdr span) (car span))))
                  (when (and (> len 0)
                             (or (<= omnidiff-image-max-bytes 0)
                                 (<= len omnidiff-image-max-bytes)))
                    (let ((raw (with-current-buffer cat-buf
                                 (buffer-substring-no-properties (car span) (cdr span)))))
                      (unless (and (< len 512)
                                   (string-prefix-p
                                    "version https://git-lfs.github.com/spec/v1\n" raw))
                        (puthash sha raw image-blobs)
                        (setq seeded (1+ seeded))))))))))))
    (cl-flet ((write-blob (sha path)
                (let ((key (omnidiff--blob-key sha path)))
                  (or (gethash key blob-files)
                      (when-let* ((span (and cat-buf (gethash sha spans))))
                        (let ((dest (omnidiff--blob-dest sha path (funcall ensure-tmpdir))))
                          (with-current-buffer cat-buf
                            (write-region (car span) (cdr span) dest nil 'silent))
                          (puthash key dest blob-files))))))
              (sub-file (sha)
                (or (gethash sha sub-files)
                    (puthash sha
                             (omnidiff--write-small-temp
                              (expand-file-name (concat "sub-" sha) (funcall ensure-tmpdir))
                              (concat "Subproject commit " sha "\n"))
                             sub-files))))
      (dolist (e entries)
        (let* ((st (omnidiff--git-entry-status e))
               (om (omnidiff--git-entry-old-mode e))
               (nm (omnidiff--git-entry-new-mode e))
               (os (omnidiff--git-entry-old-sha e))
               (ns (omnidiff--git-entry-new-sha e))
               (path (omnidiff--git-entry-path e))
               (new-path (omnidiff--git-entry-new-path e))
               (ad-status
                (when fast-ad-p
                  (cond
                   ((and (omnidiff--zero-oid-p os)
                         (not (string-prefix-p "D" st))
                         (member nm '("100644" "100755")))
                    'added)
                   ((and (string-prefix-p "D" st)
                         (not (omnidiff--zero-oid-p os))
                         (member om '("100644" "100755")))
                    'deleted))))
               (fast-file
                (when ad-status
                  (let ((raw-bytes
                         (if (eq ad-status 'deleted)
                             (cond
                              ((when-let* ((span (and cat-buf (gethash os spans))))
                                 (omnidiff--read-text-span-from-buffer
                                  cat-buf (car span) (cdr span))))
                              ((when-let* ((pre (gethash (omnidiff--blob-key os path) blob-files)))
                                 (omnidiff--read-text-file-from-disk pre))))
                           (cond
                            ((when-let* ((span (and cat-buf
                                                    (not (omnidiff--zero-oid-p ns))
                                                    (gethash ns spans))))
                               (omnidiff--read-text-span-from-buffer
                                cat-buf (car span) (cdr span))))
                            ((when-let* ((pre (and (not (omnidiff--zero-oid-p ns))
                                                   (gethash (omnidiff--blob-key ns new-path) blob-files))))
                               (omnidiff--read-text-file-from-disk pre)))
                            (t
                             (omnidiff--read-text-file-from-disk
                              (omnidiff--expand-repo-path new-path dir)))))))
                    (omnidiff--fast-parse-whole-file
                     path ad-status raw-bytes os ns)))))
          (if fast-file
              (aset results idx fast-file)
            (let* ((old-file (cond
                              ((omnidiff--zero-oid-p os) "/dev/null")
                              ((equal om "160000") (sub-file os))
                              (t (or (write-blob os path) "/dev/null"))))
                   (new-file (cond
                              ((string-prefix-p "D" st) "/dev/null")
                              ((equal nm "160000") (sub-file ns))
                              ((and (not (omnidiff--zero-oid-p ns))
                                    (write-blob ns new-path)))
                              ;; Working-tree files have either zero SHA or an in-memory
                              ;; hash not written to `.git/objects/'.
                              (t
                               (let ((full (omnidiff--expand-repo-path new-path dir)))
                                 (cond
                                  ((equal nm "120000")
                                   (omnidiff--write-small-temp
                                    (expand-file-name (format "sym-%d" idx) (funcall ensure-tmpdir))
                                    (encode-coding-string (or (file-symlink-p full) "") 'utf-8)))
                                  ((file-exists-p full) full)
                                  (t "/dev/null"))))))
                   (old-hex (if (string= old-file "/dev/null") "." os))
                   (old-m (if (string= old-file "/dev/null") "." om))
                   (new-hex (if (string= new-file "/dev/null") "." ns))
                   (new-m (if (string= new-file "/dev/null") "." nm))
                   (rename-p (and (> (length st) 0) (memq (aref st 0) '(?R ?C))))
                   (cli-args (if rename-p
                                 (list path old-file old-hex old-m new-file new-hex new-m
                                       new-path (substring st 1))
                               (list path old-file old-hex old-m new-file new-hex new-m))))
              (push (list idx path old-file new-file new-path cli-args om nm os ns) jobs)))
          (setq idx (1+ idx)))))
    (cons results (nreverse jobs))))

(cl-defun omnidiff-run (buffer command &optional label repo-dir preset layout
                               status-message preserve-state-p auto-dual-p
                               (whole-updates nil whole-updates-supplied-p)
                               (paint-reindent-moves nil paint-reindent-moves-supplied-p)
                               target-spec)
  "Run Git COMMAND with `omnidiff' and render the result in BUFFER.
LABEL annotates the header line; REPO-DIR overrides the repository root;
PRESET (`minimal', `full', `default') and LAYOUT (`unified', `side-by-side',
`stacked') select CLI and render options.  Optional STATUS-MESSAGE overrides
the progress echo message shown while the process runs.  When AUTO-DUAL-P
is non-nil, `omnidiff-single-file-entry' may open Dual automatically after
Overview renders.  Standard Git diff and show invocations parallelize
per-file `omnidiff' workers up to `omnidiff-max-parallel'; other commands
run via `GIT_EXTERNAL_DIFF'.  Supersedes any in-flight process for BUFFER
and leaves existing content intact if COMMAND fails."
  (let* ((omnidiff-bin (or (executable-find omnidiff-executable)
                           (user-error "Cannot find %s in exec-path" omnidiff-executable)))
         (dir (or repo-dir (omnidiff--repo-root)))
         (default-directory dir)
         (active-preset (or preset omnidiff-default-preset))
         (active-layout (or layout
                            (with-current-buffer buffer (omnidiff--effective-layout))))
         (active-whole (if whole-updates-supplied-p
                           (and whole-updates t)
                         (and omnidiff-whole-updates t)))
         (active-reindent (if paint-reindent-moves-supplied-p
                              (and paint-reindent-moves t)
                            (and omnidiff-paint-reindent-moves t)))
         (active-label (or label "diff"))
         (worker-flags (omnidiff-engine-build-args
                        active-preset active-whole active-reindent))
         (par-spec (omnidiff--git-parallel-spec command))
         (err-chunks nil)
         (req-id nil)
         (req nil)
         (start-msg (or status-message
                        (format "Running omnidiff (%s)..." active-label))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'omnidiff-mode)
        (omnidiff-mode))
      ;; Cancel any superseded request and advance request id before spawning.
      (omnidiff--cancel-request)
      (setq req-id (1+ omnidiff--request-id)
            omnidiff--request-id req-id
            req (make-omnidiff--request
                 :id req-id
                 :command command
                 :label active-label
                 :repo-dir dir
                 :preset active-preset
                 :layout active-layout
                 :whole-updates active-whole
                 :paint-reindent-moves active-reindent
                 :auto-dual (cond
                             ((eq auto-dual-p 'force) 'dual)
                             (auto-dual-p omnidiff-single-file-entry)
                             (t nil))
                 :target-file (plist-get target-spec :file)
                 :target-line (plist-get target-spec :line)
                 :target-side (plist-get target-spec :side)
                 :origin-buf (plist-get target-spec :origin-buf)
                 :last-msg start-msg
                 :preserve-state (and preserve-state-p t))))
    (cl-labels
        ((request-current-p ()
           (and (not (omnidiff--request-cancelled req))
                (buffer-live-p buffer)
                (eq req (buffer-local-value 'omnidiff--active-request buffer))
                (eql req-id (buffer-local-value 'omnidiff--request-id buffer))))
         (ensure-tmpdir ()
           (or (omnidiff--request-tmpdir req)
               (setf (omnidiff--request-tmpdir req)
                     (make-temp-file "omnidiff-blobs-" t))))
         (fail-request (msg)
           (when (request-current-p)
             (setf (omnidiff--request-failed-event req) nil)
             (omnidiff--cleanup-request-resources req)
             (with-current-buffer buffer
               (setq omnidiff--active-request nil))
             (message "%s" msg)))
         (commit-and-render (raw-or-diff)
           (omnidiff--cleanup-request-resources req)
           (when (request-current-p)
             (let ((diff (if (omnidiff-diff-p raw-or-diff)
                             raw-or-diff
                           (omnidiff--parse-serial-json-output raw-or-diff dir)))
                   (final-layout (omnidiff--request-layout req))
                   (final-msg (omnidiff--request-last-msg req))
                   (preserve-p (omnidiff--request-preserve-state req))
                   (seeded (omnidiff--request-image-blobs req))
                   (saved-file nil) (saved-old nil) (saved-new nil)
                   (saved-side nil) (saved-line nil))
               (setf (omnidiff--request-image-blobs req) nil)
               (with-current-buffer buffer
                 (when preserve-p
                   (setq saved-file (get-text-property (point) 'omnidiff-file)
                         saved-old (get-text-property (point) 'omnidiff-old-line)
                         saved-new (get-text-property (point) 'omnidiff-new-line)
                         saved-side (get-text-property (point) 'omnidiff-side)
                         saved-line (get-text-property (point) 'omnidiff-line)))
                 (omnidiff-image-cleanup nil preserve-p)
                 (when preserve-p
                   (omnidiff-image-prune-states (omnidiff-diff-files diff)))
                 (when seeded
                   (omnidiff-image-seed-blobs seeded))
                 (unless (or preserve-p
                             (and (equal dir omnidiff--repo-dir)
                                  (equal command omnidiff--command)))
                   (setq omnidiff--reviewed-files nil))
                 (setq omnidiff--active-request nil
                       default-directory dir
                       omnidiff--command command
                       omnidiff--repo-dir dir
                       omnidiff--label active-label
                       omnidiff--preset active-preset
                       omnidiff--layout final-layout
                       omnidiff--whole-updates active-whole
                       omnidiff--paint-reindent-moves active-reindent
                       omnidiff--origin-buffer (or (omnidiff--request-origin-buf req)
                                                   omnidiff--origin-buffer)
                       omnidiff--diff diff))
               ;; Display BUFFER first so SBS metrics measure its actual target window.
               (pop-to-buffer buffer)
               (with-current-buffer buffer
                 (let ((kept-reviewed (omnidiff--sync-reviewed-for-diff diff)))
                   (if kept-reviewed
                       (omnidiff-render-buffer diff kept-reviewed)
                     (omnidiff-render-buffer diff)))
                 (when (and preserve-p saved-file)
                   (omnidiff--restore-location
                    saved-file saved-old saved-new saved-side saved-line))
                 (run-hooks 'omnidiff-after-diff-hook))
               (when (equal (current-message) final-msg)
                 (message nil))
               (let* ((files (omnidiff-diff-files diff))
                      (entry-policy (omnidiff--request-auto-dual req))
                      (req-file (omnidiff--request-target-file req))
                      (req-line (omnidiff--request-target-line req))
                      (req-side (omnidiff--request-target-side req))
                      (matched
                       (when req-file
                         (cl-find-if (lambda (f)
                                       (or (equal (omnidiff-file-target-path f) req-file)
                                           (equal (omnidiff-file-path f) req-file)))
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
                                       (not (omnidiff-render--trivial-summary-p
                                             (or (omnidiff-file-summary f)
                                                 (when-let* ((jd (omnidiff-file-json-diff f)))
                                                   (omnidiff-json-diff-summary jd)))))))
                                files)
                               (cl-find-if #'omnidiff--file-text-dual-p files)))
                          ((and (eq entry-policy 'auto)
                                (= (length files) 1)
                                (omnidiff--file-text-dual-p (car files))
                                (omnidiff-file-json-diff (car files)))
                           (car files))))))
                 (when (and matched (not preserve-p))
                   (with-current-buffer buffer
                     (when-let* ((hdr (omnidiff--find-file-header-pos
                                       (omnidiff-file-target-path matched))))
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
                    :target-line (when (eq dual-target matched) req-line)
                    :target-side (when (eq dual-target matched) req-side)))))))
         (report-failure ()
           (when (and (request-current-p)
                      (omnidiff--request-failed-event req)
                      (omnidiff--request-stderr-done req))
             (let ((ev (omnidiff--request-failed-event req))
                   (diag (string-trim (apply #'concat (nreverse err-chunks)))))
               (fail-request
                (format "Omnidiff failed (%s)%s"
                        ev
                        (if (string-empty-p diag) "" (concat ": " diag)))))))
         (spawn-git-command (cmd serial-p on-success)
           (let* ((default-directory dir)
                  (wrapper (when serial-p
                             (omnidiff--build-ext-diff-wrapper omnidiff-bin worker-flags t)))
                  (process-environment
                   (if wrapper
                       (cons (concat "GIT_EXTERNAL_DIFF=" wrapper) process-environment)
                     process-environment))
                  (raw-chunks nil)
                  (stderr-proc nil)
                  (proc nil)
                  (created-ok nil))
             (setq err-chunks nil)
             (setf (omnidiff--request-failed-event req) nil
                   (omnidiff--request-stderr-done req) nil)
             (unwind-protect
                 (progn
                   (setq stderr-proc
                         (make-pipe-process
                          :name (concat (buffer-name buffer) " stderr")
                          :buffer nil
                          :noquery t
                          :filter (lambda (_p chunk)
                                    (unless (omnidiff--request-cancelled req)
                                      (push chunk err-chunks)))
                          :sentinel
                          (lambda (p _event)
                            (when (memq (process-status p) '(closed exit signal))
                              (setf (omnidiff--request-stderr-done req) t)
                              (report-failure)))))
                   (setq proc
                         (make-process
                          :name (buffer-name buffer)
                          :buffer nil
                          :command cmd
                          :connection-type 'pipe
                          :coding (if serial-p 'utf-8-unix 'binary)
                          :stderr stderr-proc
                          :noquery t
                          :filter (lambda (_p chunk)
                                    (unless (omnidiff--request-cancelled req)
                                      (push chunk raw-chunks)))
                          :sentinel
                          (lambda (p event)
                            (when (memq (process-status p) '(exit signal))
                              (let ((current-p (and (request-current-p)
                                                    (eq p (omnidiff--request-proc req)))))
                                (cond
                                 ((not current-p)
                                  (when (process-live-p stderr-proc)
                                    (delete-process stderr-proc))
                                  (omnidiff--cleanup-request-resources req))
                                 ((and (eq (process-status p) 'exit)
                                       (zerop (process-exit-status p)))
                                  (when (process-live-p stderr-proc)
                                    (delete-process stderr-proc))
                                  (condition-case err
                                      (funcall on-success
                                               (apply #'concat (nreverse raw-chunks)))
                                    (error
                                     (fail-request
                                      (format "Omnidiff failed: %s"
                                              (error-message-string err))))))
                                 (t
                                  (setf (omnidiff--request-failed-event req)
                                        (string-trim event))
                                  (report-failure))))))))
                   (setf (omnidiff--request-proc req) proc
                         (omnidiff--request-stderr-proc req) stderr-proc)
                   (with-current-buffer buffer
                     (setq omnidiff--active-request req))
                   (setq created-ok t))
               (unless created-ok
                 (setf (omnidiff--request-cancelled req) t)
                 (when (and proc (process-live-p proc))
                   (delete-process proc))
                 (when (and stderr-proc (process-live-p stderr-proc))
                   (delete-process stderr-proc))
                 (omnidiff--cleanup-request-resources req)))
             proc))
         (launch-workers (preamble results jobs)
           (if (not (request-current-p))
               (omnidiff--cleanup-request-resources req)
             (if (null jobs)
                 (commit-and-render
                  (make-omnidiff-diff :preamble preamble :files (append results nil)))
               (let* ((total-files (length results))
                      (total-jobs (length jobs))
                      (done-files (- total-files total-jobs))
                      (queue jobs)
                      (active 0)
                      (done-jobs 0)
                      (last-progress-time (float-time))
                      (max-w (max 1 omnidiff-max-parallel))
                      (launch-next nil))
                 (setq launch-next
                       (lambda ()
                         (while (and (request-current-p) (< active max-w) queue)
                           (let* ((job (pop queue))
                                  (ji (nth 0 job))
                                  (jpath (nth 1 job))
                                  (jfa (nth 2 job))
                                  (jfb (nth 3 job))
                                  (jnew (nth 4 job))
                                  (jargs (nth 5 job))
                                  (jom (nth 6 job))
                                  (jnm (nth 7 job))
                                  (jos (nth 8 job))
                                  (jns (nth 9 job))
                                  (chunks nil)
                                  (default-directory dir)
                                  (wp nil))
                             (condition-case err
                                 (progn
                                   (setq wp
                                         (make-process
                                          :name (format "omnidiff-%d" ji)
                                          :buffer nil
                                          :command (cons omnidiff-bin
                                                         (append worker-flags (cons "--" jargs)))
                                          :connection-type 'pipe
                                          :coding 'utf-8-unix
                                          :noquery t
                                          :filter (lambda (_p chunk)
                                                    (unless (omnidiff--request-cancelled req)
                                                      (push chunk chunks)))
                                          :sentinel
                                          (lambda (p ev)
                                            (when (memq (process-status p) '(exit signal))
                                              (setf (omnidiff--request-workers req)
                                                    (delq p (omnidiff--request-workers req)))
                                              (if (not (request-current-p))
                                                  (omnidiff--cleanup-request-resources req)
                                                (cl-flet ((make-error-file (note)
                                                            (make-omnidiff-file
                                                             :path jpath
                                                             :target-path (if (and (not (string-empty-p jnew))
                                                                                   (not (string= jnew jpath)))
                                                                              jnew
                                                                            jpath)
                                                             :status (cond
                                                                      ((string= jfa "/dev/null") 'added)
                                                                      ((string= jfb "/dev/null") 'deleted)
                                                                      ((and (not (string-empty-p jnew))
                                                                            (not (string= jnew jpath)))
                                                                       'renamed)
                                                                      (t 'modified))
                                                             :old-mode (omnidiff-engine--normalize-mode jom)
                                                             :new-mode (omnidiff-engine--normalize-mode jnm)
                                                             :old-oid (omnidiff-engine--normalize-oid jos)
                                                             :new-oid (omnidiff-engine--normalize-oid jns)
                                                             :note note)))
                                                  (let ((gc-cons-threshold most-positive-fixnum)
                                                        (gc-cons-percentage 0.8))
                                                    (aset results ji
                                                          (if (and (eq (process-status p) 'exit)
                                                                   (zerop (process-exit-status p)))
                                                              (condition-case parse-err
                                                                  (let ((payload (apply #'concat (nreverse chunks))))
                                                                    (omnidiff--file-from-json
                                                                     jpath jfa jfb jnew
                                                                     (omnidiff-engine-parse-json payload)
                                                                     jom jnm jos jns))
                                                                (error
                                                                 (make-error-file
                                                                  (format "omnidiff parse error: %s"
                                                                          (error-message-string parse-err)))))
                                                            (make-error-file
                                                             (format "omnidiff failed (%s)"
                                                                     (string-trim ev)))))))
                                                (setq active (1- active)
                                                      done-jobs (1+ done-jobs)
                                                      done-files (1+ done-files))
                                                (if (= done-jobs total-jobs)
                                                    (condition-case render-err
                                                        (commit-and-render
                                                         (make-omnidiff-diff
                                                          :preamble preamble
                                                          :files (append results nil)))
                                                      (error
                                                       (fail-request
                                                        (format "Omnidiff render failed: %s"
                                                                (error-message-string render-err)))))
                                                  (let ((now (float-time)))
                                                    (when (and (>= total-files 16)
                                                               (>= (- now last-progress-time) 0.35))
                                                      (setq last-progress-time now)
                                                      (let ((message-log-max nil)
                                                            (msg (format "Running omnidiff (%s): %d/%d..."
                                                                         active-label done-files total-files)))
                                                        (setf (omnidiff--request-last-msg req) msg)
                                                        (message "%s" msg))))
                                                  (funcall launch-next)))))))
                                   (setq active (1+ active))
                                   (push wp (omnidiff--request-workers req)))
                               (error
                                (fail-request
                                 (format "Omnidiff failed to start worker: %s"
                                         (error-message-string err)))))))))
                 (funcall launch-next)))))
         (start-parallel-stage (raw-meta)
           (let ((parsed (omnidiff--parse-raw-git-entries raw-meta (car par-spec))))
             (cond
              ((eq parsed 'fallback-serial)
               (spawn-git-command command t #'commit-and-render))
              ((null (cdr parsed))
               (commit-and-render (make-omnidiff-diff :preamble (car parsed) :files nil)))
              (t
               (let ((preamble (car parsed))
                     (entries (cdr parsed))
                     (seen (make-hash-table :test #'equal))
                     (sha-list nil))
                 (dolist (e entries)
                   (let ((om (omnidiff--git-entry-old-mode e))
                         (nm (omnidiff--git-entry-new-mode e))
                         (os (omnidiff--git-entry-old-sha e))
                         (ns (omnidiff--git-entry-new-sha e)))
                     (when (and (not (omnidiff--zero-oid-p os))
                                (not (equal om "160000"))
                                (not (gethash os seen)))
                       (puthash os t seen)
                       (push os sha-list))
                     (when (and (not (omnidiff--zero-oid-p ns))
                                (not (equal nm "160000"))
                                (not (gethash ns seen)))
                       (puthash ns t seen)
                       (push ns sha-list))))
                 (setq sha-list (nreverse sha-list))
                 (if (null sha-list)
                     (pcase-let ((`(,results . ,jobs)
                                  (omnidiff--prepare-parallel-work
                                   nil nil entries #'ensure-tmpdir dir active-preset
                                   nil)))
                       (launch-workers preamble results jobs))
                   (let ((budget omnidiff-max-parallel-blob-bytes))
                     (cl-labels
                         ((run-cat-batch (batch wire-cap on-done)
                            (let ((cat-buf (generate-new-buffer " *omnidiff-cat-file*"))
                                  (default-directory dir)
                                  (cat-bytes 0)
                                  (cat-overflow nil)
                                  (cat-proc nil))
                              (with-current-buffer cat-buf
                                (set-buffer-multibyte nil))
                              (condition-case err
                                  (progn
                                    (setq cat-proc
                                          (make-process
                                           :name "omnidiff cat-file"
                                           :buffer nil
                                           :command '("git" "cat-file" "--batch")
                                           :connection-type 'pipe
                                           :coding 'binary
                                           :noquery t
                                           :filter
                                           (lambda (cp chunk)
                                             (unless (or (omnidiff--request-cancelled req) cat-overflow)
                                               (setq cat-bytes (+ cat-bytes (length chunk)))
                                               (if (and wire-cap (> wire-cap 0) (> cat-bytes wire-cap))
                                                   (progn
                                                     (setq cat-overflow t)
                                                     (when (process-live-p cp)
                                                       (delete-process cp)))
                                                 (when (buffer-live-p cat-buf)
                                                   (with-current-buffer cat-buf
                                                     (goto-char (point-max))
                                                     (insert chunk))))))
                                           :sentinel
                                           (lambda (cp _ev)
                                             (when (memq (process-status cp) '(exit signal))
                                               (setf (omnidiff--request-workers req)
                                                     (delq cp (omnidiff--request-workers req)))
                                               (unwind-protect
                                                   (when (request-current-p)
                                                     (condition-case stage-err
                                                         (cond
                                                          (cat-overflow
                                                           (when (buffer-live-p cat-buf)
                                                             (kill-buffer cat-buf))
                                                           (spawn-git-command command t #'commit-and-render))
                                                          ((and (eq (process-status cp) 'exit)
                                                                (zerop (process-exit-status cp)))
                                                           (funcall on-done cat-buf))
                                                          (t
                                                           (fail-request
                                                            (format "Omnidiff failed (git cat-file exited with status %s)"
                                                                    (process-exit-status cp)))))
                                                       (error
                                                        (fail-request
                                                         (format "Omnidiff failed: %s"
                                                                 (error-message-string stage-err))))))
                                                 (when (buffer-live-p cat-buf)
                                                   (kill-buffer cat-buf)))))))
                                    (push cat-proc (omnidiff--request-workers req))
                                    (process-send-string cat-proc (concat (string-join batch "\n") "\n"))
                                    (process-send-eof cat-proc))
                                (error
                                 (when (buffer-live-p cat-buf)
                                   (kill-buffer cat-buf))
                                 (fail-request
                                  (format "Omnidiff failed to start git cat-file: %s"
                                          (error-message-string err)))))))
                          (run-single-batch (batch)
                            (run-cat-batch
                             batch
                             (when (> budget 0)
                               (+ budget (* 128 (length batch))))
                             (lambda (cat-buf)
                               (pcase-let ((`(,results . ,jobs)
                                            (omnidiff--prepare-parallel-work
                                             cat-buf batch entries
                                             #'ensure-tmpdir dir active-preset
                                             (or (omnidiff--request-image-blobs req)
                                                 (setf (omnidiff--request-image-blobs req)
                                                       (make-hash-table :test #'equal)))
                                             )))
                                 (launch-workers preamble results jobs)))))
                          (run-multi-batches (batches)
                            (let* ((maps (omnidiff--collect-sha-batch-maps entries))
                                   (sha-targets (car maps))
                                   (visual-shas (cdr maps))
                                   (image-blobs
                                    (or (omnidiff--request-image-blobs req)
                                        (setf (omnidiff--request-image-blobs req)
                                              (make-hash-table :test #'equal))))
                                   (blob-files (make-hash-table :test #'equal
                                                                :size (max 16 (length sha-list))))
                                   (seeded-cell (list 0))
                                   (visual-limit (if (> omnidiff-image-auto-load-limit 0)
                                                     (* 2 omnidiff-image-auto-load-limit)
                                                   most-positive-fixnum))
                                   (rem batches)
                                   (step nil))
                              (setq step
                                    (lambda ()
                                      (if (null rem)
                                          (pcase-let ((`(,results . ,jobs)
                                                       (omnidiff--prepare-parallel-work
                                                        nil nil entries
                                                        #'ensure-tmpdir dir active-preset
                                                        image-blobs blob-files)))
                                            (launch-workers preamble results jobs))
                                        (let ((batch (pop rem)))
                                          (run-cat-batch
                                           batch
                                           (when (> budget 0)
                                             (+ budget (* 128 (length batch))))
                                           (lambda (cat-buf)
                                             (omnidiff--materialize-cat-batch
                                              cat-buf batch sha-targets visual-shas
                                              #'ensure-tmpdir image-blobs blob-files
                                              seeded-cell visual-limit)
                                             (funcall step)))))))
                              (funcall step))))
                       (if (<= budget 0)
                           (run-single-batch sha-list)
                         (let ((check-chunks nil)
                               (default-directory dir)
                               (check-proc nil))
                           (condition-case err
                               (progn
                                 (setq check-proc
                                       (make-process
                                        :name "omnidiff cat-file check"
                                        :buffer nil
                                        :command '("git" "cat-file" "--batch-check")
                                        :connection-type 'pipe
                                        :coding 'binary
                                        :noquery t
                                        :filter
                                        (lambda (_cp chunk)
                                          (unless (omnidiff--request-cancelled req)
                                            (push chunk check-chunks)))
                                        :sentinel
                                        (lambda (cp _ev)
                                          (when (memq (process-status cp) '(exit signal))
                                            (setf (omnidiff--request-workers req)
                                                  (delq cp (omnidiff--request-workers req)))
                                            (when (request-current-p)
                                              (condition-case stage-err
                                                  (if (and (eq (process-status cp) 'exit)
                                                           (zerop (process-exit-status cp)))
                                                      (let* ((sizes (omnidiff--parse-batch-check-sizes
                                                                     (apply #'concat (nreverse check-chunks))))
                                                             (batches (omnidiff--partition-sha-batches
                                                                       sha-list sizes budget)))
                                                        (cond
                                                         ((eq batches 'overflow)
                                                          (spawn-git-command command t #'commit-and-render))
                                                         ((<= (length batches) 1)
                                                          (run-single-batch (or (car batches) sha-list)))
                                                         (t
                                                          (run-multi-batches batches))))
                                                    (fail-request
                                                     (format "Omnidiff failed (git cat-file --batch-check exited with status %s)"
                                                             (process-exit-status cp))))
                                                (error
                                                 (fail-request
                                                  (format "Omnidiff failed: %s"
                                                          (error-message-string stage-err))))))))))
                                 (push check-proc (omnidiff--request-workers req))
                                 (process-send-string check-proc (concat (string-join sha-list "\n") "\n"))
                                 (process-send-eof check-proc))
                             (error
                              (fail-request
                               (format "Omnidiff failed to start git cat-file --batch-check: %s"
                                       (error-message-string err))))))))))))))))
      (let ((proc (if par-spec
                      (spawn-git-command (cdr par-spec) nil #'start-parallel-stage)
                    (spawn-git-command command t #'commit-and-render))))
        (message "%s" start-msg)
        proc))))

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

(defun omnidiff--git-toplevel (&optional dir)
  "Return the Git repository root directory for DIR, or nil."
  (let ((default-directory (or dir default-directory)))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
        (let ((out (string-trim (buffer-string))))
          (unless (string-empty-p out)
            (file-name-as-directory out)))))))

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
                     (omnidiff--git-toplevel)
                     default-directory))
           (full (omnidiff--expand-repo-path (plist-get target-spec :file) repo)))
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
                       (if (string= after-abs "/dev/null") before-abs after-abs)))
                 (res (if (and after-dirty-p
                               (not (omnidiff-json-diff-picture jdiff))
                               (not (omnidiff-json-diff-container jdiff))
                               (not (omnidiff-json-diff-binary-p jdiff)))
                          (let* ((b-enc (omnidiff-json-side-encoding
                                         (omnidiff-json-diff-before jdiff)))
                                 (b-src (unless (string= before-abs "/dev/null")
                                          (omnidiff-engine-read-file-decoded
                                           before-abs b-enc)))
                                 (can-edit (and (file-regular-p after-abs)
                                                (file-writable-p after-abs)))
                                 (cleanup-dir snap-tmpdir))
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
                                          (omnidiff--launch-tui-for-session sess))
                              :on-quit (lambda (_sess)
                                         (when (and cleanup-dir
                                                    (file-directory-p cleanup-dir))
                                           (ignore-errors
                                             (delete-directory cleanup-dir t)))))))
                        (omnidiff--open-pair-result
                         before-abs after-diff-path jdiff
                         :display-path disp
                         :before-label b-lbl
                         :after-label a-lbl
                         :dir (or dir default-directory)
                         :preset omnidiff-default-preset
                         :whole-updates omnidiff-whole-updates
                         :paint-reindent-moves omnidiff-paint-reindent-moves
                         :editable-after t
                         :owned-tmpdir snap-tmpdir))))
            (setq transferred t)
            res)
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
    (let* ((repo-root (or (omnidiff--git-toplevel (file-name-directory file))
                          (user-error "File is not inside a Git repository: %s" file)))
           (relpath (file-relative-name (file-truename file)
                                        (file-truename repo-root)))
           (target-rev (if (and (stringp rev) (not (string-empty-p rev)))
                           rev
                         "HEAD"))
           (tmpdir (make-temp-file "omnidiff-vs-rev-" t))
           (transferred nil))
      (unwind-protect
          (let* ((b-file (or (omnidiff--git-show-to-file
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
        (when-let* ((root (omnidiff--git-toplevel (file-name-directory buffer-file-name))))
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
                   (omnidiff--git-toplevel))))
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
