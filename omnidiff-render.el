;;; omnidiff-render.el --- Buffer renderer and layouts for omnidiff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/omnidiff.el
;; Keywords: vc, diff, tools

;; This file is part of omnidiff.
;;
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

;; Renders parsed `omnidiff-diff' structures into an `omnidiff-mode' buffer
;; across `unified', `side-by-side', and `stacked' layouts, along with the
;; sticky file-oriented header line.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'text-property-search)
(require 'omnidiff-theme)
(require 'omnidiff-engine)
(require 'omnidiff-image)

(declare-function nxml-mode "nxml-mode" ())
(declare-function xml-mode "nxml-mode" ())
(declare-function omnidiff-toggle-fold "omnidiff" ())
(declare-function omnidiff-jump-to-source "omnidiff" ())

(defvar delayed-mode-hooks)
(defvar enable-dir-local-variables)
(defvar rng-nxml-auto-validate-flag)
(defvar font-lock-mode-hook)

;;; Customization & buffer-local state

(defcustom omnidiff-default-layout 'unified
  "Default diff layout for `omnidiff-mode'.
- `unified': Interleaved single-column diff (shared context shown once) with
  `omnidiff' syntax-aware inline highlights.
- `side-by-side': Two-column BEFORE | AFTER alignment from `omnidiff'.
- `stacked': Full BEFORE block followed by AFTER block per hunk."
  :type '(choice (const :tag "Unified (interleaved)" unified)
                 (const :tag "Side-by-side (two columns)" side-by-side)
                 (const :tag "Stacked (BEFORE then AFTER)" stacked))
  :group 'omnidiff)

(defcustom omnidiff-overview-highlight-style 'token
  "Visual diff highlight style for JSON-backed Overview buffers.
- `token': OmniDiff's \"Only What Changed\" aesthetic — unchanged code keeps
  pure native syntax highlighting with no full-line background wash; only
  changed token spans receive `omnidiff-token-*' highlights.
- `subtle-line': Gentle line-level background tint plus crisp token highlights.
- `traditional': Classic full-line `omnidiff-added'/`removed'/`changed' wash
  plus inline refine highlights."
  :type '(choice (const :tag "Token only (Only What Changed)" token)
                 (const :tag "Subtle line tint + tokens" subtle-line)
                 (const :tag "Traditional full-line wash" traditional))
  :group 'omnidiff)

(defcustom omnidiff-overview-syntax-highlight t
  "When non-nil, apply native Emacs `font-lock' highlighting in Overview.
Applies to JSON-backed diff entries whose decoded source does not exceed
`omnidiff-syntax-highlight-max-bytes'."
  :type 'boolean
  :group 'omnidiff)

(defcustom omnidiff-syntax-highlight-max-bytes (* 512 1024)
  "Maximum source byte size eligible for Overview `font-lock' highlighting.
Larger files fall back to plain text lines to keep rendering fast."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-overview-context-lines 3
  "Number of unchanged context lines kept around changed rows in Overview."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-overview-auto-fold-trivial nil
  "Automatically fold file cards with trivial semantic summaries on render.
- nil: Never auto-fold file cards.
- `whitespace-and-comments': Auto-fold `whitespace-only' and `comment-only'.
- t: Also auto-fold `refactor-moved-only' and `no-changes'."
  :type '(choice (const :tag "Never auto-fold" nil)
                 (const :tag "Whitespace and comments only" whitespace-and-comments)
                 (const :tag "All trivial summaries" t))
  :group 'omnidiff)

(defcustom omnidiff-overview-auto-fold-threshold 6
  "Auto-fold file cards when an Overview contains at least this many files.
When set to a positive integer N and a diff has >= N files, file cards start
folded as a scannable review checklist (press RET or `d' on a file header to
open Dual, or TAB to preview hunks inline).  Set to nil to disable."
  :type '(choice (const :tag "Never auto-fold by count" nil)
                 (natnum :tag "Minimum file count"))
  :group 'omnidiff)

(defcustom omnidiff-overview-tab-width 4
  "Tab stop width used when expanding literal TAB characters in Overview.
When nil, uses the buffer's `tab-width'."
  :type '(choice (const :tag "Use buffer tab-width" nil)
                 (integer :tag "Columns"))
  :group 'omnidiff)

(defvar-local omnidiff--command nil)
(defvar-local omnidiff--repo-dir nil)
(defvar-local omnidiff--label nil)
(defvar-local omnidiff--preset 'default)
(defvar-local omnidiff--layout nil)
(defvar-local omnidiff--highlight-style nil)
(defvar-local omnidiff--file-count 0)
(defvar-local omnidiff--reviewed-files nil
  "Hash table mapping reviewed file target paths to content signatures.")

(defun omnidiff-render-reviewed-count ()
  "Return the number of reviewed files recorded in the current buffer."
  (if (hash-table-p omnidiff--reviewed-files)
      (hash-table-count omnidiff--reviewed-files)
    0))

(defun omnidiff-render-set-file-reviewed-indicator (header-pos reviewed-p)
  "Update the `✓' reviewed indicator overlay on the file header at HEADER-POS."
  (when-let* ((ov (and (integerp header-pos)
                       (get-text-property header-pos 'omnidiff-review-ov))))
    (when (overlay-buffer ov)
      (overlay-put ov 'before-string
                   (when reviewed-p
                     (propertize " ✓" 'face 'omnidiff-stat-added))))))

(defun omnidiff-render-effective-highlight-style ()
  "Return the active highlight style for the current Overview buffer."
  (or omnidiff--highlight-style omnidiff-overview-highlight-style))

(defvar omnidiff-render--style-cache nil
  "Per-render cache mapping `(OP . MOVED-P)' to `omnidiff-op-style'.")

(defvar omnidiff-render--sbs-metrics-cache nil
  "Per-render cache mapping NUM-WIDTH to `omnidiff-render--sbs-metrics'.")

(defvar omnidiff-render--hints-cache nil
  "Cached `(KEY HINTS . HINTS-PX)' for `omnidiff-render-header-line'.")

;;; Internal render structures

(cl-defstruct (omnidiff-op-style (:copier nil))
  "Face and gutter decoration bundle for a diff operation."
  gutter-face rail-face rail-char sign-str rail-sign-str cont-rail-str
  line-face newline-str refine-face)

(cl-defstruct (omnidiff-header-span (:copier nil))
  "Buffer region and disclosure overlay of a rendered hunk header."
  start end fold-ov)

;;; Shared formatting primitives

(defun omnidiff-render--right-align-px (str extra-px)
  "Compute pixel width of STR plus EXTRA-PX padding for `:align-to'."
  (+ (if (fboundp 'string-pixel-width)
         (string-pixel-width str)
       (* (length str) (frame-char-width)))
     extra-px))

(defun omnidiff-render--format-range (start-line end-line)
  "Format hunk line range label from START-LINE and END-LINE."
  (cond
   ((and start-line end-line (/= start-line end-line))
    (format "L%d-%d" start-line end-line))
   (start-line (format "L%d" start-line))
   (t "hunk")))

(defsubst omnidiff-render--display-width (str)
  "Return display column width of STR, using O(1) length for pure ASCII."
  (if (and (= (length str) (string-bytes str))
           (not (string-search "\t" str)))
      (length str)
    (string-width str)))

(defun omnidiff-render--make-op-style (op moved-p)
  "Construct an `omnidiff-op-style' for diff operation OP and MOVED-P."
  (let* ((gutter-face (cl-ecase op
                        (insert 'omnidiff-lineno-added)
                        (delete 'omnidiff-lineno-removed)
                        (update 'omnidiff-lineno-changed)
                        (move 'omnidiff-lineno-moved)
                        (context 'omnidiff-lineno)))
         (rail-face (if (and moved-p (not (eq op 'move)))
                        (list 'omnidiff-moved-indicator gutter-face)
                      gutter-face))
         (rail-char (if (or moved-p (not (eq op 'context))) "┃" "│"))
         (sign-str (cl-ecase op
                     (insert " + ")
                     (delete " - ")
                     (update " ~ ")
                     (move " > ")
                     (context "   ")))
         (line-face (cl-ecase op
                      (insert 'omnidiff-added)
                      (delete 'omnidiff-removed)
                      (update 'omnidiff-changed)
                      (move 'omnidiff-moved)
                      (context 'default)))
         (refine-face (cl-ecase op
                        (insert 'omnidiff-added-refine)
                        (delete 'omnidiff-removed-refine)
                        (update 'omnidiff-changed-refine)
                        ((move context) nil))))
    (make-omnidiff-op-style
     :gutter-face gutter-face
     :rail-face rail-face
     :rail-char rail-char
     :sign-str sign-str
     :rail-sign-str (concat (propertize rail-char 'face rail-face)
                            (propertize sign-str 'face gutter-face))
     :cont-rail-str (concat (propertize rail-char 'face rail-face)
                            (propertize "   " 'face gutter-face))
     :line-face line-face
     :newline-str (if (eq line-face 'default)
                      "\n"
                    (propertize "\n" 'face line-face))
     :refine-face refine-face)))

(defun omnidiff-render--op-style (op moved-p)
  "Return a cached `omnidiff-op-style' for OP and MOVED-P."
  (let ((key (cons op (and moved-p t))))
    (if omnidiff-render--style-cache
        (or (gethash key omnidiff-render--style-cache)
            (puthash key (omnidiff-render--make-op-style op moved-p)
                     omnidiff-render--style-cache))
      (omnidiff-render--make-op-style op moved-p))))

(defun omnidiff-render--insert-preamble (preamble)
  "Insert formatted commit metadata PREAMBLE for `git show'."
  (when (and preamble (not (string-blank-p preamble)))
    (dolist (line (split-string (string-trim-right preamble) "\n"))
      (cond
       ((string-match "\\`\\(commit \\)\\(.*\\)\\'" line)
        (insert (propertize (match-string 1 line) 'face 'omnidiff-commit-meta)
                (propertize (match-string 2 line) 'face 'omnidiff-commit-hash)
                "\n"))
       ((string-match "\\`\\(Author:\\|Date:\\|Merge:\\)\\(.*\\)\\'" line)
        (insert (propertize (match-string 1 line) 'face 'omnidiff-commit-meta)
                (propertize (match-string 2 line) 'face 'default)
                "\n"))
       (t
        (insert line "\n"))))
    (insert "\n")))

;;; Mouse interaction & path truncation helpers

(defun omnidiff-render--mouse-toggle-fold (event)
  "Toggle folding of the file or hunk header clicked by mouse EVENT."
  (interactive "e")
  (let* ((start (event-start event))
         (win (posn-window start))
         (pos (posn-point start)))
    (when (and (window-live-p win) (integerp pos))
      (select-window win)
      (goto-char pos)
      (omnidiff-toggle-fold))))

(defun omnidiff-render--mouse-container-jump (event)
  "Inspect the container member row clicked by mouse EVENT."
  (interactive "e")
  (let* ((start (event-start event))
         (win (posn-window start))
         (pos (posn-point start)))
    (when (and (window-live-p win) (integerp pos))
      (select-window win)
      (goto-char pos)
      (omnidiff-jump-to-source))))

(defconst omnidiff-render--fold-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'omnidiff-render--mouse-toggle-fold)
    map)
  "Keymap allowing mouse clicks to fold/unfold file and hunk headers.")

(defconst omnidiff-render--image-fold-header-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map omnidiff-image-header-map)
    (define-key map [mouse-1] #'omnidiff-render--mouse-toggle-fold)
    map)
  "Keymap for image file/hunk headers: safe image keys plus click-to-fold.")

(defconst omnidiff-render--container-row-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'omnidiff-render--mouse-container-jump)
    map)
  "Keymap allowing mouse clicks to drill down into container members.")

(defun omnidiff-render--truncate-single-path (path max-cols)
  "Truncate single PATH to at most MAX-COLS columns, keeping trailing segments."
  (let ((w (omnidiff-render--display-width path)))
    (cond
     ((or (<= max-cols 0) (<= w max-cols)) path)
     ((<= max-cols 4) (truncate-string-to-width path max-cols nil nil "…"))
     ((string-search "/" path)
      (let* ((parts (split-string path "/"))
             (tail (car (last parts)))
             (rest (nreverse (butlast parts)))
             (acc tail))
        (while (and rest
                    (<= (+ 2 (omnidiff-render--display-width (car rest))
                           1 (omnidiff-render--display-width acc))
                        max-cols))
          (setq acc (concat (pop rest) "/" acc)))
        (let ((cand (concat "…/" acc)))
          (if (<= (omnidiff-render--display-width cand) max-cols)
              cand
            (truncate-string-to-width cand max-cols nil nil "…")))))
     (t
      (truncate-string-to-width path max-cols nil nil "…")))))

(defun omnidiff-render--truncate-path (path max-cols)
  "Truncate PATH (including `old → new' renames) to at most MAX-COLS columns."
  (let ((w (omnidiff-render--display-width path)))
    (cond
     ((or (<= max-cols 0) (<= w max-cols)) path)
     ((string-search " → " path)
      (let* ((sep-pos (string-search " → " path))
             (lhs (substring path 0 sep-pos))
             (rhs (substring path (+ sep-pos 3)))
             (avail (max 8 (- max-cols 3)))
             (rhs-budget (min (omnidiff-render--display-width rhs)
                              (max 4 (/ (* avail 3) 5))))
             (lhs-budget (max 4 (- avail rhs-budget))))
        (concat (omnidiff-render--truncate-single-path lhs lhs-budget)
                " → "
                (omnidiff-render--truncate-single-path rhs rhs-budget))))
     (t
      (omnidiff-render--truncate-single-path path max-cols)))))


;;; Hunk chrome

(defun omnidiff-render--insert-hunk-header (range-str ha hr hu hm gap-text scope-item
                                                      hunk-idx hunk-total)
  "Insert a blank canvas spacer and structured hunk header line.
Return an `omnidiff-header-span'."
  (let* ((hstat-parts
          (delq nil
                (list (when (> ha 0)
                        (propertize (format "+%d" ha) 'face 'omnidiff-hunk-stat-added))
                      (when (> hr 0)
                        (propertize (format "-%d" hr) 'face 'omnidiff-hunk-stat-removed))
                      (when (> hu 0)
                        (propertize (format "~%d" hu) 'face 'omnidiff-hunk-stat-changed))
                      (when (> hm 0)
                        (propertize (format ">%d" hm) 'face 'omnidiff-hunk-stat-moved)))))
         (hstats-str (string-join hstat-parts (propertize " " 'face 'omnidiff-hunk-header)))
         (hidx-str (propertize (format "%d/%d" hunk-idx hunk-total)
                               'face 'omnidiff-hunk-meta))
         (hidx-px (omnidiff-render--right-align-px hidx-str 16))
         (hunk-header-start nil)
         (hunk-fold-ov nil))
    (insert "\n")
    (setq hunk-header-start (point))
    (insert (propertize "  " 'face 'omnidiff-hunk-header))
    (let ((tri-start (point)))
      (insert (propertize "▼" 'face 'omnidiff-hunk-accent))
      (setq hunk-fold-ov (make-overlay tri-start (point)))
      (overlay-put hunk-fold-ov 'omnidiff-ov t))
    (insert (propertize (concat " " range-str) 'face 'omnidiff-hunk-header))
    (unless (string-empty-p hstats-str)
      (insert (propertize "   " 'face 'omnidiff-hunk-header)
              hstats-str))
    (when (and gap-text (not (string-empty-p gap-text)))
      (insert (propertize (concat "  · " gap-text) 'face 'omnidiff-hunk-meta)))
    (when scope-item
      (let ((stxt (truncate-string-to-width
                   (string-trim (omnidiff-scope-item-text scope-item))
                   60 nil nil "…")))
        (insert (propertize (format "  · @ %d: %s"
                                    (omnidiff-scope-item-line scope-item)
                                    stxt)
                            'face 'omnidiff-hunk-meta))))
    (insert (propertize "  " 'face 'omnidiff-hunk-header)
            (propertize " " 'face 'omnidiff-hunk-header
                        'display `(space :align-to (- right (,hidx-px))))
            hidx-str
            (propertize "\n" 'face 'omnidiff-hunk-header))
    (make-omnidiff-header-span
     :start hunk-header-start
     :end (point)
     :fold-ov hunk-fold-ov)))

(defun omnidiff-render--finalize-hunk (hdr body-start sline)
  "Attach folding overlay and jump properties to a rendered hunk using HDR."
  (let ((hstart (omnidiff-header-span-start hdr))
        (hend (omnidiff-header-span-end hdr))
        (hov (make-overlay body-start (point))))
    (overlay-put hov 'omnidiff-hunk-ov t)
    (overlay-put hov 'omnidiff-ov t)
    (overlay-put hov 'omnidiff-fold-ov (omnidiff-header-span-fold-ov hdr))
    (overlay-put hov 'evaporate t)
    (add-text-properties hstart hend
                         (list 'omnidiff-hunk t
                               'omnidiff-hunk-ov hov
                               'omnidiff-line sline
                               'mouse-face 'highlight
                               'help-echo "Click or press TAB to fold/unfold hunk"
                               'keymap omnidiff-render--fold-header-map))))

(defun omnidiff-render--insert-side-label (side num-width &optional scope-item)
  "Insert a full-width BEFORE or AFTER sub-header for SIDE.
Aligns with the code column past NUM-WIDTH gutter and optional SCOPE-ITEM."
  (let ((label (cl-ecase side (before " BEFORE ") (after " AFTER  ")))
        (face (cl-ecase side (before 'omnidiff-side-before) (after 'omnidiff-side-after)))
        (pad (make-string (+ num-width 6) ?\s)))
    (insert (propertize pad 'face 'omnidiff-side-bar)
            (propertize label 'face face))
    (when scope-item
      (insert (propertize (format "  @ %d: %s"
                                  (omnidiff-scope-item-line scope-item)
                                  (omnidiff-scope-item-text scope-item))
                          'face 'omnidiff-side-bar)))
    (insert (propertize "\n" 'face 'omnidiff-side-bar))))

;;; Side-by-side layout

(cl-defstruct (omnidiff-render--sbs-metrics (:copier nil))
  "Measured column and pixel geometry for side-by-side rendering."
  (gutter-w 10 :type integer)
  (code-w 28 :type integer)
  (center-px nil))

(defun omnidiff-render--sbs-metrics (num-width)
  "Compute side-by-side geometry for NUM-WIDTH digits in the current buffer."
  (or (and omnidiff-render--sbs-metrics-cache
           (gethash num-width omnidiff-render--sbs-metrics-cache))
      (let* ((win (or (get-buffer-window (current-buffer) t)
                      (selected-window)))
             (frame (if win (window-frame win) (selected-frame)))
             (win-w (or (and win (window-body-width win)) 140))
             (gutter-w (+ num-width 6))
             (gui-p (and (display-graphic-p frame) (fboundp 'string-pixel-width)))
             (char-w (max 1 (frame-char-width frame)))
             (left-gutter-px (when gui-p
                               (string-pixel-width
                                (propertize (make-string gutter-w ?\s)
                                            'face 'omnidiff-lineno))))
             (div-px (when gui-p
                       (string-pixel-width (propertize " │ " 'face 'omnidiff-lineno))))
             (win-px (when (and gui-p win)
                       (window-body-width win t)))
             (code-w (if (and gui-p win-px)
                         (/ (- win-px (* 2 left-gutter-px) div-px)
                            (* 2 char-w))
                       (/ (- win-w (* 2 gutter-w) 3) 2)))
             (center-px (when gui-p
                          (+ left-gutter-px (* (max 1 code-w) char-w))))
             (metrics (make-omnidiff-render--sbs-metrics
                       :gutter-w gutter-w
                       :code-w code-w
                       :center-px center-px)))
        (when omnidiff-render--sbs-metrics-cache
          (puthash num-width metrics omnidiff-render--sbs-metrics-cache))
        metrics)))

;;; JSON-backed semantic rendering (syntax highlighting, token overlays, layouts)

(cl-defstruct (omnidiff-json-span (:copier nil))
  "Single character span with exact operation on a rendered JSON code line."
  (start 0 :type integer)
  (end 0 :type integer)
  (op 'update :type symbol)) ; insert | delete | update | move

(cl-defstruct (omnidiff-json-line-cell (:copier nil))
  "One side (`before' or `after') of an aligned JSON overview row."
  (row 0 :type integer)
  (line 1 :type integer)
  (text "" :type string)
  (op 'context :type symbol) ; insert | delete | update | move | context
  (moved nil :type boolean)
  (spans nil :type list) ; list of `omnidiff-json-span'
  (move-ref nil)
  (ref-line nil))

(cl-defstruct (omnidiff-json-aligned-row (:copier nil))
  "Aligned BEFORE/AFTER pair for JSON-backed Overview rendering."
  (left nil)
  (right nil)
  (changed-p nil :type boolean))

(cl-defstruct (omnidiff-json-overview-hunk (:copier nil))
  "Context-grouped hunk of `omnidiff-json-aligned-row's."
  (start-line nil)
  (end-line nil)
  (gap-text nil)
  (scope nil)
  (added 0 :type integer)
  (removed 0 :type integer)
  (updated 0 :type integer)
  (moved 0 :type integer)
  (rows nil :type list))

(cl-defstruct (omnidiff-json-side-index (:copier nil))
  "Per-row hunk metadata indexed over one side of a JSON diff."
  (row-spans nil)
  (row-ops nil)
  (row-moved nil)
  (row-move-ref nil)
  (row-ref-line nil))

(cl-defstruct (omnidiff-json-render-plan (:copier nil))
  "Precomputed JSON overview hunks and aggregate counts for one file."
  (hunks nil :type list)
  (added 0 :type integer)
  (removed 0 :type integer)
  (updated 0 :type integer)
  (moved 0 :type integer)
  (max-line 0 :type integer))

(defun omnidiff-render--op-priority (op)
  "Return precedence rank for diff operation OP (higher wins)."
  (pcase op
    ('update 3)
    ('insert 2)
    ('delete 2)
    ('move 1)
    (_ 0)))

(defun omnidiff-render--token-face (op)
  "Return semantic token face for diff operation OP."
  (pcase op
    ('insert 'omnidiff-token-insert)
    ('delete 'omnidiff-token-delete)
    ('update 'omnidiff-token-update)
    ('move 'omnidiff-token-move)
    (_ 'omnidiff-token-update)))

(defun omnidiff-render--traditional-refine-face (op)
  "Return traditional inline refine face for diff operation OP."
  (pcase op
    ('insert 'omnidiff-added-refine)
    ('delete 'omnidiff-removed-refine)
    ('update 'omnidiff-changed-refine)
    ('move 'omnidiff-token-move)
    (_ nil)))

(defun omnidiff-render--json-line-face (op highlight-style)
  "Return line-level background face for OP under HIGHLIGHT-STYLE.
In `token' style, or for `context' lines, always returns `default' so no
`:extend t' background wash bleeds across unchanged code."
  (if (or (eq highlight-style 'token) (eq op 'context))
      'default
    (cl-ecase op
      (insert 'omnidiff-added)
      (delete 'omnidiff-removed)
      (update 'omnidiff-changed)
      (move 'omnidiff-moved))))

(defun omnidiff-render--split-source-lines (source)
  "Split decoded SOURCE string into a vector of logical 0-based lines.
Empty SOURCE yields an empty vector `[]'; a single trailing newline is
treated as a line terminator rather than a phantom empty final line."
  (if (or (null source) (string-empty-p source))
      []
    (let ((trimmed (if (string-suffix-p "\n" source)
                       (substring source 0 -1)
                     source)))
      (vconcat (split-string trimmed "\n")))))

(defun omnidiff-render--extract-face-only-line (beg end)
  "Return buffer text from BEG to END retaining only `face' properties."
  (let ((str (buffer-substring-no-properties beg end))
        (pos beg))
    (while (< pos end)
      (let* ((f-next (next-single-property-change pos 'face nil end))
             (fl-next (next-single-property-change pos 'font-lock-face nil end))
             (next (min end f-next fl-next))
             (face (or (get-text-property pos 'face)
                       (get-text-property pos 'font-lock-face))))
        (when face
          (put-text-property (- pos beg) (- next beg) 'face face str))
        (setq pos next)))
    str))

(defun omnidiff-render--fontify-lines (source path-hint)
  "Fontify SOURCE using PATH-HINT's major mode and return a line vector.
Each line retains only `face' text properties so foreign mode keymaps,
invisibility, and display properties never leak into Overview buffers.
Falls back to plain lines when `omnidiff-overview-syntax-highlight' is
nil, SOURCE is empty or exceeds `omnidiff-syntax-highlight-max-bytes', or
PATH-HINT resolves to a binary, viewer, or fundamental major mode."
  (cond
   ((or (null source) (string-empty-p source))
    [])
   ((or (not omnidiff-overview-syntax-highlight)
        (> (string-bytes source) omnidiff-syntax-highlight-max-bytes)
        (null path-hint)
        (not (stringp path-hint))
        (string-empty-p path-hint)
        (string= path-hint "/dev/null"))
    (omnidiff-render--split-source-lines source))
   (t
    (with-temp-buffer
      (let ((change-major-mode-hook nil)
            (font-lock-mode-hook nil))
        (insert source)
        (goto-char (point-min))
        (let* ((safe-hint (if (string-prefix-p "~" path-hint)
                              (concat "./" path-hint)
                            path-hint))
               (buffer-file-name (expand-file-name safe-hint))
               (enable-local-variables nil)
               (enable-dir-local-variables nil)
               (rng-nxml-auto-validate-flag nil)
               (inhibit-message t)
               (message-log-max nil))
          (ignore-errors
            (delay-mode-hooks
              (if (let ((case-fold-search t))
                    (string-match-p "\\.svgz?\\'" path-hint))
                  (if (fboundp 'nxml-mode) (nxml-mode) (xml-mode))
                (set-auto-mode t))))
          (setq delayed-mode-hooks nil))
        (if (or (eq major-mode 'fundamental-mode)
                (derived-mode-p 'image-mode 'archive-mode 'tar-mode
                                'doc-view-mode 'pdf-view-mode 'hexl-mode))
            (omnidiff-render--split-source-lines source)
          (let ((inhibit-message t)
                (message-log-max nil))
            (ignore-errors
              (font-lock-mode 1)
              (font-lock-ensure)))
          (widen)
          (if (= (point-max) (point-min))
              (omnidiff-render--split-source-lines source)
            (let* ((limit (if (string-suffix-p "\n" source)
                              (1- (length source))
                            (length source)))
                   (expected-count 1)
                   (idx 0)
                   (end-pos (if (and (string-suffix-p "\n" source)
                                     (> (point-max) (point-min)))
                                (1- (point-max))
                              (point-max)))
                   (pos (point-min))
                   (lines nil)
                   (done nil))
              (while (and (< idx limit)
                          (setq idx (string-search "\n" source idx))
                          (< idx limit))
                (setq expected-count (1+ expected-count)
                      idx (1+ idx)))
              (goto-char pos)
              (while (not done)
                (if (search-forward "\n" end-pos t)
                    (progn
                      (push (omnidiff-render--extract-face-only-line pos (1- (point))) lines)
                      (setq pos (point)))
                  (push (omnidiff-render--extract-face-only-line pos end-pos) lines)
                  (setq done t)))
              (let ((result (vconcat (nreverse lines))))
                (if (= (length result) expected-count)
                    result
                  (omnidiff-render--split-source-lines source)))))))))))

(defun omnidiff-render--resolve-json-source (source disk-path encoding)
  "Return decoded text string from SOURCE, else read DISK-PATH with ENCODING."
  (cond
   ((stringp source) source)
   ((and (stringp disk-path)
         (not (string-empty-p disk-path))
         (not (string= disk-path "/dev/null"))
         (file-readable-p disk-path)
         (not (file-directory-p disk-path)))
    (or (ignore-errors (omnidiff-engine-read-file-decoded disk-path encoding)) ""))
   (t "")))

(defun omnidiff-render--pick-path-hint (&rest candidates)
  "Return the first usable non-empty, non-`/dev/null' path in CANDIDATES."
  (cl-find-if
   (lambda (p)
     (and (stringp p) (not (string-empty-p p)) (not (string= p "/dev/null"))))
   candidates))

(defun omnidiff-render--range-last-row (range num-rows)
  "Return inclusive 0-based last row covered by RANGE in NUM-ROWS, or nil."
  (when (and range (> num-rows 0))
    (let ((sr (omnidiff-json-range-start-row range))
          (sc (omnidiff-json-range-start-col range))
          (er (omnidiff-json-range-end-row range))
          (ec (omnidiff-json-range-end-col range)))
      (when (and (< sr num-rows)
                 (<= sr er)
                 (or (> er sr) (> ec sc)))
        (let ((last (if (and (> er sr) (= ec 0))
                        (1- (min er num-rows))
                      (min er (1- num-rows)))))
          (when (>= last sr)
            last))))))

(defun omnidiff-render--normalize-row-spans (spans)
  "Order SPANS by op priority and start offset, merging touching same-op spans."
  (when spans
    (let* ((sorted (sort (copy-sequence spans)
                         (lambda (a b)
                           (let ((pa (omnidiff-render--op-priority (omnidiff-json-span-op a)))
                                 (pb (omnidiff-render--op-priority (omnidiff-json-span-op b))))
                             (cond
                              ((/= pa pb) (< pa pb))
                              ((/= (omnidiff-json-span-start a) (omnidiff-json-span-start b))
                               (< (omnidiff-json-span-start a) (omnidiff-json-span-start b)))
                              (t (< (omnidiff-json-span-end a) (omnidiff-json-span-end b))))))))
           (merged nil))
      (dolist (sp sorted)
        (let ((prev (car merged)))
          (if (and prev
                   (eq (omnidiff-json-span-op prev) (omnidiff-json-span-op sp))
                   (<= (omnidiff-json-span-start sp) (omnidiff-json-span-end prev)))
              (setf (omnidiff-json-span-end prev)
                    (max (omnidiff-json-span-end prev) (omnidiff-json-span-end sp)))
            (push (make-omnidiff-json-span
                   :start (omnidiff-json-span-start sp)
                   :end (omnidiff-json-span-end sp)
                   :op (omnidiff-json-span-op sp))
                  merged))))
      (nreverse merged))))

(defun omnidiff-render--index-side-hunks (hunks raw-lines side-sym)
  "Index JSON HUNKS over RAW-LINES for SIDE-SYM (`before' or `after').
Preserves exact per-span operations, move target `→ L<n>' / `← L<n>' labels,
and enclosing `reference-line' breadcrumbs."
  (let* ((n (length raw-lines))
         (row-spans (make-vector n nil))
         (row-ops (make-vector n nil))
         (row-moved (make-vector n nil))
         (row-move-ref (make-vector n nil))
         (row-ref-line (make-vector n nil))
         (arrow (if (eq side-sym 'before) "→" "←")))
    (dolist (h hunks)
      (let* ((op (omnidiff-json-hunk-operation h))
             (prio (omnidiff-render--op-priority op))
             (rng (omnidiff-json-hunk-range h))
             (sr (omnidiff-json-range-start-row rng))
             (last-row (omnidiff-render--range-last-row rng n))
             (mt (omnidiff-json-hunk-move-target h))
             (ref-line (omnidiff-json-hunk-reference-line h)))
        (when last-row
          (let ((r sr))
            (while (<= r last-row)
              (let ((cur (aref row-ops r)))
                (when (or (null cur) (> prio (omnidiff-render--op-priority cur)))
                  (aset row-ops r op)))
              (when (eq op 'move)
                (aset row-moved r t)
                (when (and (= r sr) mt (null (aref row-move-ref r)))
                  (aset row-move-ref r
                        (format "%s L%d" arrow (1+ (omnidiff-json-range-start-row mt))))))
              (when (and (integerp ref-line) (null (aref row-ref-line r)))
                (aset row-ref-line r ref-line))
              (setq r (1+ r)))))
        (dolist (seg (omnidiff-engine-range-line-spans rng raw-lines))
          (pcase-let ((`(,row ,sc ,ec) seg))
            (when (and (>= row 0) (< row n) (< sc ec))
              (push (make-omnidiff-json-span :start sc :end ec :op op)
                    (aref row-spans row)))))))
    (dotimes (r n)
      (when (aref row-spans r)
        (aset row-spans r (omnidiff-render--normalize-row-spans (aref row-spans r)))))
    (make-omnidiff-json-side-index
     :row-spans row-spans
     :row-ops row-ops
     :row-moved row-moved
     :row-move-ref row-move-ref
     :row-ref-line row-ref-line)))

(defun omnidiff-render--make-json-cell (row fontified-lines side-idx)
  "Construct an `omnidiff-json-line-cell' for 0-based ROW."
  (make-omnidiff-json-line-cell
   :row row
   :line (1+ row)
   :text (aref fontified-lines row)
   :op (or (aref (omnidiff-json-side-index-row-ops side-idx) row) 'context)
   :moved (and (aref (omnidiff-json-side-index-row-moved side-idx) row) t)
   :spans (aref (omnidiff-json-side-index-row-spans side-idx) row)
   :move-ref (aref (omnidiff-json-side-index-row-move-ref side-idx) row)
   :ref-line (aref (omnidiff-json-side-index-row-ref-line side-idx) row)))

(defun omnidiff-render--row-changed-p (left right b-raw a-raw)
  "Return non-nil when aligned pair LEFT/RIGHT represents a changed row."
  (cond
   ((or (null left) (null right)) t)
   ((not (eq (omnidiff-json-line-cell-op left) 'context)) t)
   ((not (eq (omnidiff-json-line-cell-op right) 'context)) t)
   ((omnidiff-json-line-cell-moved left) t)
   ((omnidiff-json-line-cell-moved right) t)
   ((omnidiff-json-line-cell-spans left) t)
   ((omnidiff-json-line-cell-spans right) t)
   ((not (equal (aref b-raw (omnidiff-json-line-cell-row left))
                (aref a-raw (omnidiff-json-line-cell-row right))))
    t)
   (t nil)))

(defun omnidiff-render--build-aligned-rows (b-raw a-raw b-font a-font b-idx a-idx alignment)
  "Build a vector of `omnidiff-json-aligned-row's from ALIGNMENT anchors."
  (let* ((nb (length b-raw))
         (na (length a-raw))
         (anchors (omnidiff-line-alignment-anchors alignment))
         (cur-b 0)
         (cur-a 0)
         (rows nil))
    (dolist (bound (append anchors (list (cons nb na))))
      (let* ((next-b (car bound))
             (next-a (cdr bound))
             (b-gap (- next-b cur-b))
             (a-gap (- next-a cur-a))
             (paired (min b-gap a-gap)))
        ;; Gap before anchor: zip overlapping positions, emit remaining one-sided rows.
        (dotimes (k paired)
          (let* ((br (+ cur-b k))
                 (ar (+ cur-a k))
                 (lcell (omnidiff-render--make-json-cell br b-font b-idx))
                 (rcell (omnidiff-render--make-json-cell ar a-font a-idx)))
            (push (make-omnidiff-json-aligned-row
                   :left lcell
                   :right rcell
                   :changed-p (omnidiff-render--row-changed-p lcell rcell b-raw a-raw))
                  rows)))
        (let ((br (+ cur-b paired)))
          (while (< br next-b)
            (push (make-omnidiff-json-aligned-row
                   :left (omnidiff-render--make-json-cell br b-font b-idx)
                   :right nil
                   :changed-p t)
                  rows)
            (setq br (1+ br))))
        (let ((ar (+ cur-a paired)))
          (while (< ar next-a)
            (push (make-omnidiff-json-aligned-row
                   :left nil
                   :right (omnidiff-render--make-json-cell ar a-font a-idx)
                   :changed-p t)
                  rows)
            (setq ar (1+ ar))))
        ;; Anchor row itself (skipped for the trailing (NB . NA) sentinel).
        (when (and (< next-b nb) (< next-a na))
          (let* ((lcell (omnidiff-render--make-json-cell next-b b-font b-idx))
                 (rcell (omnidiff-render--make-json-cell next-a a-font a-idx)))
            (push (make-omnidiff-json-aligned-row
                   :left lcell
                   :right rcell
                   :changed-p (omnidiff-render--row-changed-p lcell rcell b-raw a-raw))
                  rows)))
        (setq cur-b (1+ next-b)
              cur-a (1+ next-a))))
    (vconcat (nreverse rows))))

(defun omnidiff-render--pick-hunk-scope (rows b-raw a-raw)
  "Pick an enclosing `omnidiff-scope-item' for hunk ROWS if elided above hunk."
  (let ((first-a-row nil)
        (first-b-row nil)
        (cand-a nil)
        (cand-b nil))
    (dolist (row rows)
      (when-let* ((rc (omnidiff-json-aligned-row-right row)))
        (unless first-a-row
          (setq first-a-row (omnidiff-json-line-cell-row rc)))
        (when (and (null cand-a) (integerp (omnidiff-json-line-cell-ref-line rc)))
          (setq cand-a (omnidiff-json-line-cell-ref-line rc))))
      (when-let* ((lc (omnidiff-json-aligned-row-left row)))
        (unless first-b-row
          (setq first-b-row (omnidiff-json-line-cell-row lc)))
        (when (and (null cand-b) (integerp (omnidiff-json-line-cell-ref-line lc)))
          (setq cand-b (omnidiff-json-line-cell-ref-line lc)))))
    (cl-flet ((make-scope (ref-row first-row lines)
                (when (and (integerp ref-row)
                           (integerp first-row)
                           (>= ref-row 0)
                           (< ref-row first-row)
                           (< ref-row (length lines)))
                  (let ((txt (string-trim (aref lines ref-row))))
                    (unless (string-empty-p txt)
                      (make-omnidiff-scope-item :line (1+ ref-row) :text txt))))))
      (or (make-scope cand-a first-a-row a-raw)
          (make-scope cand-b first-b-row b-raw)))))

(defun omnidiff-render--group-aligned-rows (rows-vec b-raw a-raw)
  "Group ROWS-VEC into context-bounded `omnidiff-json-overview-hunk's."
  (let* ((m (length rows-vec))
         (ctx (max 0 omnidiff-overview-context-lines))
         (keep (make-vector m nil))
         (any-changed nil))
    (dotimes (i m)
      (when (omnidiff-json-aligned-row-changed-p (aref rows-vec i))
        (setq any-changed t)
        (let ((lo (max 0 (- i ctx)))
              (hi (min (1- m) (+ i ctx))))
          (while (<= lo hi)
            (aset keep lo t)
            (setq lo (1+ lo))))))
    (when any-changed
      (let ((hunks nil)
            (i 0)
            (skipped 0))
        (while (< i m)
          (if (not (aref keep i))
              (setq skipped (1+ skipped)
                    i (1+ i))
            (let ((hrows nil)
                  (a-start nil) (a-end nil)
                  (b-start nil) (b-end nil)
                  (added 0) (removed 0) (updated 0) (moved 0))
              (while (and (< i m) (aref keep i))
                (let* ((row (aref rows-vec i))
                       (l (omnidiff-json-aligned-row-left row))
                       (r (omnidiff-json-aligned-row-right row))
                       (lop (and l (omnidiff-json-line-cell-op l)))
                       (rop (and r (omnidiff-json-line-cell-op r))))
                  (when l
                    (let ((ln (omnidiff-json-line-cell-line l)))
                      (setq b-start (if b-start (min b-start ln) ln)
                            b-end (if b-end (max b-end ln) ln))))
                  (when r
                    (let ((ln (omnidiff-json-line-cell-line r)))
                      (setq a-start (if a-start (min a-start ln) ln)
                            a-end (if a-end (max a-end ln) ln))))
                  (cond
                   ((or (eq lop 'update) (eq rop 'update))
                    (setq updated (1+ updated)))
                   (t
                    (when (or (eq lop 'delete) (eq rop 'delete))
                      (setq removed (1+ removed)))
                    (when (or (eq lop 'insert) (eq rop 'insert))
                      (setq added (1+ added)))))
                  (when (or (and l (omnidiff-json-line-cell-moved l))
                            (and r (omnidiff-json-line-cell-moved r)))
                    (setq moved (1+ moved)))
                  (push row hrows))
                (setq i (1+ i)))
              (setq hrows (nreverse hrows))
              (push (make-omnidiff-json-overview-hunk
                     :start-line (or a-start b-start)
                     :end-line (or a-end b-end)
                     :gap-text (when (> skipped 0)
                                 (format "%d unchanged %s"
                                         skipped (if (= skipped 1) "line" "lines")))
                     :scope (omnidiff-render--pick-hunk-scope hrows b-raw a-raw)
                     :added added
                     :removed removed
                     :updated updated
                     :moved moved
                     :rows hrows)
                    hunks)
              (setq skipped 0))))
        (nreverse hunks)))))

(defun omnidiff-render--build-json-plan (file json-diff)
  "Build an `omnidiff-json-render-plan' for JSON-backed FILE and JSON-DIFF."
  (let* ((b-side (or (omnidiff-json-diff-before json-diff)
                     (make-omnidiff-json-side)))
         (a-side (or (omnidiff-json-diff-after json-diff)
                     (make-omnidiff-json-side)))
         (norm-diff (if (and (omnidiff-json-diff-before json-diff)
                             (omnidiff-json-diff-after json-diff))
                        json-diff
                      (make-omnidiff-json-diff
                       :before b-side :after a-side
                       :large-residual (omnidiff-json-diff-large-residual json-diff)
                       :summary (omnidiff-json-diff-summary json-diff)
                       :binary-p (omnidiff-json-diff-binary-p json-diff)
                       :picture (omnidiff-json-diff-picture json-diff)
                       :container (omnidiff-json-diff-container json-diff))))
         (b-src (omnidiff-render--resolve-json-source
                 (omnidiff-file-before-source file)
                 (omnidiff-file-before-disk-path file)
                 (omnidiff-json-side-encoding b-side)))
         (a-src (omnidiff-render--resolve-json-source
                 (omnidiff-file-after-source file)
                 (omnidiff-file-after-disk-path file)
                 (omnidiff-json-side-encoding a-side)))
         (b-hint (omnidiff-render--pick-path-hint
                  (omnidiff-file-path file)
                  (omnidiff-file-target-path file)
                  (omnidiff-json-side-path b-side)))
         (a-hint (omnidiff-render--pick-path-hint
                  (omnidiff-file-target-path file)
                  (omnidiff-file-path file)
                  (omnidiff-json-side-path a-side)))
         (b-raw (omnidiff-render--split-source-lines b-src))
         (a-raw (omnidiff-render--split-source-lines a-src))
         (b-font (omnidiff-render--fontify-lines b-src b-hint))
         (a-font (omnidiff-render--fontify-lines a-src a-hint))
         (b-idx (omnidiff-render--index-side-hunks
                 (omnidiff-json-side-hunks b-side) b-raw 'before))
         (a-idx (omnidiff-render--index-side-hunks
                 (omnidiff-json-side-hunks a-side) a-raw 'after))
         (alignment (omnidiff-engine-compute-alignment b-raw a-raw norm-diff))
         (rows-vec (omnidiff-render--build-aligned-rows
                    b-raw a-raw b-font a-font b-idx a-idx alignment))
         (hunks (omnidiff-render--group-aligned-rows rows-vec b-raw a-raw))
         (added 0) (removed 0) (updated 0) (moved 0))
    (dolist (h hunks)
      (setq added (+ added (omnidiff-json-overview-hunk-added h))
            removed (+ removed (omnidiff-json-overview-hunk-removed h))
            updated (+ updated (omnidiff-json-overview-hunk-updated h))
            moved (+ moved (omnidiff-json-overview-hunk-moved h))))
    (make-omnidiff-json-render-plan
     :hunks hunks
     :added added
     :removed removed
     :updated updated
     :moved moved
     :max-line (max (length b-raw) (length a-raw)))))

(defun omnidiff-render--json-plan-key (json-diff)
  "Return cache key for `omnidiff-json-render-plan' derived from JSON-DIFF."
  (list json-diff
        (and omnidiff-overview-syntax-highlight t)
        omnidiff-syntax-highlight-max-bytes
        omnidiff-overview-context-lines
        omnidiff-alignment-max-lines))

(defun omnidiff-render--cached-json-plan (file json-diff)
  "Return valid cached `omnidiff-json-render-plan' for FILE and JSON-DIFF, or nil."
  (let ((cached (omnidiff-file-json-render-plan file))
        (ckey (omnidiff-file-json-render-plan-key file))
        (key (omnidiff-render--json-plan-key json-diff)))
    (when (and cached
               (consp ckey)
               (eq (car ckey) json-diff)
               (equal (cdr ckey) (cdr key)))
      cached)))

(defun omnidiff-render--get-json-plan (file json-diff)
  "Return cached or freshly built `omnidiff-json-render-plan' for FILE."
  (or (omnidiff-render--cached-json-plan file json-diff)
      (let ((plan (omnidiff-render--build-json-plan file json-diff)))
        (setf (omnidiff-file-json-render-plan file) plan
              (omnidiff-file-json-render-plan-key file)
              (omnidiff-render--json-plan-key json-diff))
        plan)))

(defun omnidiff-render--insert-json-body (file num-width &optional json-plan)
  "Insert rendered JSON hunks or fallback note for FILE using NUM-WIDTH."
  (let* ((status (omnidiff-file-status file))
         (old-mode (omnidiff-file-old-mode file))
         (new-mode (omnidiff-file-new-mode file))
         (mode-changed-p (and old-mode new-mode
                              (not (string= old-mode new-mode))
                              (not (memq status '(added deleted)))))
         (json-diff (omnidiff-file-json-diff file))
         (summary (or (omnidiff-file-summary file)
                      (and json-diff (omnidiff-json-diff-summary json-diff))))
         (summary-label (omnidiff-engine-summary-label summary))
         (binary-p (or (omnidiff-file-binary-p file)
                       (and json-diff (omnidiff-json-diff-binary-p json-diff))))
         (note (omnidiff-file-note file))
         (plan (or json-plan
                   (and json-diff (omnidiff-render--get-json-plan file json-diff))))
         (json-hunks (and plan (omnidiff-json-render-plan-hunks plan)))
         (eff-num-width (max num-width
                             (if plan
                                 (length (number-to-string
                                          (max 1 (omnidiff-json-render-plan-max-line plan))))
                               0))))
    (cond
     (json-hunks
      (let ((hunk-total (length json-hunks))
            (hunk-idx 0)
            (layout (or omnidiff--layout omnidiff-default-layout))
            (hstyle (omnidiff-render-effective-highlight-style)))
        (dolist (hunk json-hunks)
          (setq hunk-idx (1+ hunk-idx))
          (cl-ecase layout
            (side-by-side
             (omnidiff-render--insert-json-sbs-hunk
              hunk status eff-num-width hstyle hunk-idx hunk-total))
            (stacked
             (omnidiff-render--insert-json-stacked-hunk
              hunk status eff-num-width hstyle hunk-idx hunk-total))
            (unified
             (omnidiff-render--insert-json-unified-hunk
              hunk eff-num-width hstyle hunk-idx hunk-total))))))
     ((or note mode-changed-p binary-p (and json-diff summary-label))
      (let ((display-note
             (cond
              ((and mode-changed-p
                    (or (null note)
                        (string= note "No changes - files are identical")))
               (format "Mode changed: %s → %s" old-mode new-mode))
              (note note)
              (binary-p "Binary files differ")
              (summary-label
               (concat (upcase (substring summary-label 0 1))
                       (substring summary-label 1)))
              (t "No changes")))
            (note-start (point)))
        (insert (propertize (concat (make-string (+ eff-num-width 2) ?\s) display-note "\n")
                            'face 'omnidiff-gap))
        (put-text-property note-start (min (1+ note-start) (point))
                           'omnidiff-hunk t))))))

(defun omnidiff-render-materialize-file-overlay (ov)
  "Materialize deferred JSON body inside file-body overlay OV, if pending."
  (when (and (overlayp ov) (overlay-buffer ov))
    (when-let* ((file (overlay-get ov 'omnidiff-lazy-file)))
      (let ((buf (overlay-buffer ov))
            (num-width (or (overlay-get ov 'omnidiff-lazy-num-width) 4)))
        (overlay-put ov 'omnidiff-lazy-file nil)
        (overlay-put ov 'omnidiff-lazy-num-width nil)
        (with-current-buffer buf
          (let* ((inhibit-read-only t)
                 (gc-cons-threshold most-positive-fixnum)
                 (gc-cons-percentage 0.8)
                 (omnidiff-render--style-cache
                  (or omnidiff-render--style-cache
                      (make-hash-table :test #'equal :size 16)))
                 (omnidiff-render--sbs-metrics-cache
                  (or omnidiff-render--sbs-metrics-cache
                      (make-hash-table :test #'eql :size 4)))
                 (start (overlay-start ov))
                 (end (overlay-end ov))
                 (tail-len (- end start)))
            (save-excursion
              (goto-char start)
              (omnidiff-render--insert-json-body file num-width)
              (let ((body-end (point)))
                (put-text-property start body-end 'omnidiff-file
                                   (omnidiff-file-target-path file))
                (move-overlay ov start (+ body-end tail-len))))
            (set-buffer-modified-p nil)))))))

(defun omnidiff-render--expand-tabs-propertized (str)
  "Expand literal TAB characters in STR using `omnidiff-overview-tab-width'."
  (if (not (string-search "\t" str))
      str
    (let ((tw (or (and (integerp omnidiff-overview-tab-width)
                       (> omnidiff-overview-tab-width 0)
                       omnidiff-overview-tab-width)
                  (and (integerp tab-width) (> tab-width 0) tab-width)
                  4))
          (pos 0)
          (len (length str))
          (col 0)
          (parts nil))
      (while (< pos len)
        (let ((tab (string-search "\t" str pos)))
          (if (null tab)
              (progn
                (push (substring str pos) parts)
                (setq pos len))
            (when (> tab pos)
              (let ((seg (substring str pos tab)))
                (push seg parts)
                (setq col (+ col (omnidiff-render--display-width seg)))))
            (let* ((spaces (- tw (mod col tw)))
                   (face (get-text-property tab 'face str))
                   (sp (make-string spaces ?\s)))
              (when face
                (put-text-property 0 spaces 'face face sp))
              (push sp parts)
              (setq col (+ col spaces)
                    pos (1+ tab))))))
      (apply #'concat (nreverse parts)))))

(defun omnidiff-render--compose-json-cell-text (cell highlight-style)
  "Compose syntax + diff faces on `omnidiff-json-line-cell' CELL.
HIGHLIGHT-STYLE is `token', `subtle-line', or `traditional'."
  (let* ((raw-text (omnidiff-json-line-cell-text cell))
         (str (if (string-suffix-p "\r" raw-text)
                  (substring raw-text 0 -1)
                (copy-sequence raw-text)))
         (len (length str))
         (op (omnidiff-json-line-cell-op cell))
         (spans (omnidiff-json-line-cell-spans cell))
         (line-face (omnidiff-render--json-line-face op highlight-style)))
    (when (> len 0)
      (cl-ecase highlight-style
        (token
         (dolist (sp spans)
           (let ((s (max 0 (min (omnidiff-json-span-start sp) len)))
                 (e (max 0 (min (omnidiff-json-span-end sp) len))))
             (when (< s e)
               (add-face-text-property
                s e (omnidiff-render--token-face (omnidiff-json-span-op sp))
                nil str)))))
        (subtle-line
         (unless (eq line-face 'default)
           (add-face-text-property 0 len line-face t str))
         (dolist (sp spans)
           (let ((s (max 0 (min (omnidiff-json-span-start sp) len)))
                 (e (max 0 (min (omnidiff-json-span-end sp) len))))
             (when (< s e)
               (add-face-text-property
                s e (omnidiff-render--token-face (omnidiff-json-span-op sp))
                nil str)))))
        (traditional
         (unless (eq line-face 'default)
           (put-text-property 0 len 'face line-face str))
         (let* ((paint-end (omnidiff-engine-line-byte-to-char
                            str (omnidiff-engine-paint-row-byte-len str)))
                (full-pure-line
                 (and (memq op '(insert delete move))
                      spans
                      (null (cdr spans))
                      (eq (omnidiff-json-span-op (car spans)) op)
                      (= (omnidiff-json-span-start (car spans)) 0)
                      (>= (omnidiff-json-span-end (car spans)) paint-end))))
           (unless full-pure-line
             (dolist (sp spans)
               (let* ((s (max 0 (min (omnidiff-json-span-start sp) len)))
                      (e (max 0 (min (omnidiff-json-span-end sp) len)))
                      (rface (omnidiff-render--traditional-refine-face
                              (omnidiff-json-span-op sp))))
                 (when (and rface (< s e))
                   (put-text-property s e 'face rface str)))))))))
    (omnidiff-render--expand-tabs-propertized str)))

(defun omnidiff-render--wrap-propertized-text (styled-str width &optional move-ref)
  "Wrap propertized STYLED-STR to display column WIDTH preserving faces.
Return a non-empty list of `(SEGMENT-STRING . SEGMENT-MOVE-REF)' cons cells."
  (let* ((w (max 1 width))
         (clean-w (omnidiff-render--display-width styled-str))
         (suffix-w (if move-ref (+ 2 (omnidiff-render--display-width move-ref)) 0)))
    (if (<= (+ clean-w suffix-w) w)
        (list (cons styled-str move-ref))
      (let* ((len (length styled-str))
             (ascii-p (and (= len (string-bytes styled-str))
                           (not (string-search "\t" styled-str))))
             (pos 0)
             (segs nil))
        (if (= len 0)
            (push (list "" 0) segs)
          (while (< pos len)
            (let ((seg-start pos)
                  (seg-w 0))
              (if ascii-p
                  (let ((step (min w (- len pos))))
                    (setq pos (+ pos step)
                          seg-w step))
                (while (and (< pos len)
                            (let ((cw (char-width (aref styled-str pos))))
                              (or (= pos seg-start)
                                  (<= (+ seg-w cw) w))))
                  (setq seg-w (+ seg-w (char-width (aref styled-str pos)))
                        pos (1+ pos))))
              (push (list (substring styled-str seg-start pos) seg-w) segs))))
        (setq segs (nreverse segs))
        (let ((result (mapcar (lambda (s) (cons (car s) nil)) segs)))
          (when move-ref
            (cond
             ((<= (+ (cadr (car segs)) suffix-w) w)
              (setcdr (car result) move-ref))
             ((<= (+ (cadr (car (last segs))) suffix-w) w)
              (setcdr (car (last result)) move-ref))
             (t
              (setq result (nconc result (list (cons "" move-ref)))))))
          result)))))

(defun omnidiff-render--insert-propertized-code (styled-str line-face
                                                            &optional move-ref col-width align-px)
  "Insert propertized STYLED-STR with LINE-FACE padding and optional MOVE-REF."
  (let* ((suffix (if move-ref (concat "  " move-ref) ""))
         (suffix-w (if move-ref (omnidiff-render--display-width suffix) 0))
         (move-face (when move-ref
                      (if (eq line-face 'default)
                          'omnidiff-moved-indicator
                        (list 'omnidiff-moved-indicator line-face))))
         (avail (when col-width
                  (if (and move-ref (> col-width (+ suffix-w 4)))
                      (- col-width suffix-w)
                    col-width)))
         (str-w (when (or avail (and col-width (not align-px)))
                  (omnidiff-render--display-width styled-str)))
         (truncated-p (and avail (> str-w avail)))
         (show-str (if truncated-p
                       (truncate-string-to-width styled-str avail nil nil "…")
                     styled-str))
         (show-w (if truncated-p
                     (omnidiff-render--display-width show-str)
                   str-w)))
    (insert show-str)
    (cond
     (align-px
      (let ((want-suffix (and move-ref col-width
                              (>= (- col-width show-w) suffix-w))))
        (if want-suffix
            (let* ((sfx-str (propertize suffix 'face move-face))
                   (sfx-px (string-pixel-width sfx-str))
                   (pre-px (max 0 (- align-px sfx-px))))
              (insert (propertize " " 'face line-face
                                  'display `(space :align-to (,pre-px)))
                      sfx-str
                      (propertize " " 'face line-face
                                  'display `(space :align-to (,align-px)))))
          (insert (propertize " " 'face line-face
                              'display `(space :align-to (,align-px)))))))
     (col-width
      (let* ((want-suffix (and move-ref (>= (- col-width show-w) suffix-w)))
             (pad (max 0 (- col-width show-w (if want-suffix suffix-w 0)))))
        (when (> pad 0)
          (insert (if (eq line-face 'default)
                      (make-string pad ?\s)
                    (propertize (make-string pad ?\s) 'face line-face))))
        (when want-suffix
          (insert (propertize suffix 'face move-face)))))
     (move-ref
      (insert (propertize suffix 'face move-face))))))

(defun omnidiff-render--json-newline-str (line-face)
  "Return newline string with LINE-FACE, or plain `\"\\n\"' when `default'."
  (if (eq line-face 'default)
      "\n"
    (propertize "\n" 'face line-face)))

(defun omnidiff-render--insert-json-unified-line (cell old-ln new-ln gutter-op
                                                       highlight-style
                                                       old-fmt new-fmt
                                                       old-blank new-blank)
  "Insert one JSON-backed interleaved unified line for CELL."
  (let* ((old-str (if old-ln (format old-fmt old-ln) old-blank))
         (new-str (if new-ln (format new-fmt new-ln) new-blank))
         (style (omnidiff-render--op-style gutter-op (omnidiff-json-line-cell-moved cell)))
         (line-face (omnidiff-render--json-line-face
                     (omnidiff-json-line-cell-op cell) highlight-style))
         (styled-str (omnidiff-render--compose-json-cell-text cell highlight-style))
         (line-start (point)))
    (insert (propertize (concat old-str new-str) 'face (omnidiff-op-style-gutter-face style))
            (omnidiff-op-style-rail-sign-str style))
    (omnidiff-render--insert-propertized-code
     styled-str line-face (omnidiff-json-line-cell-move-ref cell))
    (insert (omnidiff-render--json-newline-str line-face))
    (let ((props (list 'omnidiff-line (or new-ln old-ln)
                       'omnidiff-side (cond ((and old-ln new-ln) 'both)
                                            (new-ln 'after)
                                            (old-ln 'before)))))
      (when old-ln (setq props (nconc props (list 'omnidiff-old-line old-ln))))
      (when new-ln (setq props (nconc props (list 'omnidiff-new-line new-ln))))
      (add-text-properties line-start (point) props))))

(defun omnidiff-render--insert-json-unified-hunk (hunk num-width highlight-style
                                                       hunk-idx hunk-total)
  "Insert `omnidiff-json-overview-hunk' HUNK in interleaved unified layout."
  (let* ((sline (omnidiff-json-overview-hunk-start-line hunk))
         (eline (omnidiff-json-overview-hunk-end-line hunk))
         (range-str (omnidiff-render--format-range sline eline))
         (hdr (omnidiff-render--insert-hunk-header
               range-str
               (omnidiff-json-overview-hunk-added hunk)
               (omnidiff-json-overview-hunk-removed hunk)
               (omnidiff-json-overview-hunk-updated hunk)
               (omnidiff-json-overview-hunk-moved hunk)
               (omnidiff-json-overview-hunk-gap-text hunk)
               (omnidiff-json-overview-hunk-scope hunk)
               hunk-idx hunk-total))
         (body-start (point))
         (old-fmt (format " %%%dd" num-width))
         (new-fmt (format " %%%dd " num-width))
         (old-blank (make-string (1+ num-width) ?\s))
         (new-blank (make-string (+ num-width 2) ?\s))
         (pending-before nil)
         (pending-after nil))
    (cl-flet ((flush-changes ()
                (dolist (entry (nreverse pending-before))
                  (let ((cell (car entry))
                        (gop (cdr entry)))
                    (omnidiff-render--insert-json-unified-line
                     cell (omnidiff-json-line-cell-line cell) nil gop
                     highlight-style old-fmt new-fmt old-blank new-blank)))
                (dolist (entry (nreverse pending-after))
                  (let ((cell (car entry))
                        (gop (cdr entry)))
                    (omnidiff-render--insert-json-unified-line
                     cell nil (omnidiff-json-line-cell-line cell) gop
                     highlight-style old-fmt new-fmt old-blank new-blank)))
                (setq pending-before nil
                      pending-after nil)))
      (dolist (row (omnidiff-json-overview-hunk-rows hunk))
        (let ((left (omnidiff-json-aligned-row-left row))
              (right (omnidiff-json-aligned-row-right row)))
          (if (and left right (not (omnidiff-json-aligned-row-changed-p row)))
              (progn
                (flush-changes)
                (omnidiff-render--insert-json-unified-line
                 right (omnidiff-json-line-cell-line left)
                 (omnidiff-json-line-cell-line right)
                 'context highlight-style old-fmt new-fmt old-blank new-blank))
            ;; Changed row: preserve each side's real cell-op on the code text
            ;; while marking paired-unchanged gutters as `update' (no false delete paint).
            (when left
              (let* ((lop (omnidiff-json-line-cell-op left))
                     (eff-lop (cond
                               ((not (eq lop 'context)) lop)
                               (right 'update)
                               (t 'delete))))
                (push (cons left eff-lop) pending-before)))
            (when right
              (let* ((rop (omnidiff-json-line-cell-op right))
                     (eff-rop (cond
                               ((not (eq rop 'context)) rop)
                               (left 'update)
                               (t 'insert))))
                (push (cons right eff-rop) pending-after))))))
      (flush-changes))
    (omnidiff-render--finalize-hunk hdr body-start sline)))

(defun omnidiff-render--insert-json-stacked-cell (cell ln-fmt side highlight-style)
  "Insert one stacked JSON line CELL on SIDE (`before' or `after')."
  (let* ((op (omnidiff-json-line-cell-op cell))
         (ln (omnidiff-json-line-cell-line cell))
         (style (omnidiff-render--op-style op (omnidiff-json-line-cell-moved cell)))
         (line-face (omnidiff-render--json-line-face op highlight-style))
         (styled-str (omnidiff-render--compose-json-cell-text cell highlight-style))
         (line-start (point)))
    (insert (propertize (format ln-fmt ln) 'face (omnidiff-op-style-gutter-face style))
            (omnidiff-op-style-rail-sign-str style))
    (omnidiff-render--insert-propertized-code
     styled-str line-face (omnidiff-json-line-cell-move-ref cell))
    (insert (omnidiff-render--json-newline-str line-face))
    (add-text-properties
     line-start (point)
     (pcase side
       ('before (list 'omnidiff-line ln 'omnidiff-old-line ln 'omnidiff-side 'before))
       ('after (list 'omnidiff-line ln 'omnidiff-new-line ln 'omnidiff-side 'after))
       (_ (list 'omnidiff-line ln))))))

(defun omnidiff-render--insert-json-stacked-hunk (hunk status num-width highlight-style
                                                       hunk-idx hunk-total)
  "Insert `omnidiff-json-overview-hunk' HUNK in stacked BEFORE-then-AFTER layout."
  (let* ((sline (omnidiff-json-overview-hunk-start-line hunk))
         (eline (omnidiff-json-overview-hunk-end-line hunk))
         (range-str (omnidiff-render--format-range sline eline))
         (hdr (omnidiff-render--insert-hunk-header
               range-str
               (omnidiff-json-overview-hunk-added hunk)
               (omnidiff-json-overview-hunk-removed hunk)
               (omnidiff-json-overview-hunk-updated hunk)
               (omnidiff-json-overview-hunk-moved hunk)
               (omnidiff-json-overview-hunk-gap-text hunk)
               (omnidiff-json-overview-hunk-scope hunk)
               hunk-idx hunk-total))
         (body-start (point))
         (ln-fmt (format " %%%dd " num-width))
         (before-cells nil)
         (after-cells nil))
    (dolist (row (omnidiff-json-overview-hunk-rows hunk))
      (when-let* ((l (omnidiff-json-aligned-row-left row)))
        (push l before-cells))
      (when-let* ((r (omnidiff-json-aligned-row-right row)))
        (push r after-cells)))
    (setq before-cells (nreverse before-cells)
          after-cells (nreverse after-cells))
    (when before-cells
      (when (or after-cells (not (eq status 'deleted)))
        (omnidiff-render--insert-side-label 'before num-width))
      (dolist (cell before-cells)
        (omnidiff-render--insert-json-stacked-cell cell ln-fmt 'before highlight-style)))
    (when after-cells
      (when (or before-cells (not (eq status 'added)))
        (omnidiff-render--insert-side-label 'after num-width))
      (dolist (cell after-cells)
        (omnidiff-render--insert-json-stacked-cell cell ln-fmt 'after highlight-style)))
    (omnidiff-render--finalize-hunk hdr body-start sline)))

(defun omnidiff-render--insert-json-sbs-hunk (hunk status num-width highlight-style
                                                   hunk-idx hunk-total)
  "Insert `omnidiff-json-overview-hunk' HUNK in two-column BEFORE | AFTER layout.
Automatically falls back to stacked layout when per-side code width < 20."
  (let ((metrics (omnidiff-render--sbs-metrics num-width)))
    (if (< (omnidiff-render--sbs-metrics-code-w metrics) 20)
        (omnidiff-render--insert-json-stacked-hunk
         hunk status num-width highlight-style hunk-idx hunk-total)
      (let* ((sline (omnidiff-json-overview-hunk-start-line hunk))
             (eline (omnidiff-json-overview-hunk-end-line hunk))
             (range-str (omnidiff-render--format-range sline eline))
             (hdr (omnidiff-render--insert-hunk-header
                   range-str
                   (omnidiff-json-overview-hunk-added hunk)
                   (omnidiff-json-overview-hunk-removed hunk)
                   (omnidiff-json-overview-hunk-updated hunk)
                   (omnidiff-json-overview-hunk-moved hunk)
                   (omnidiff-json-overview-hunk-gap-text hunk)
                   (omnidiff-json-overview-hunk-scope hunk)
                   hunk-idx hunk-total))
             (body-start (point))
             (gutter-w (omnidiff-render--sbs-metrics-gutter-w metrics))
             (code-w (omnidiff-render--sbs-metrics-code-w metrics))
             (center-px (omnidiff-render--sbs-metrics-center-px metrics))
             (ln-fmt (format " %%%dd " num-width))
             (blank-ln (make-string (+ num-width 2) ?\s))
             (empty-prefix (concat (propertize blank-ln 'face 'omnidiff-lineno)
                                   (propertize "│   " 'face 'omnidiff-lineno)))
             (empty-left (concat empty-prefix
                                 (if center-px
                                     (propertize " " 'face 'default
                                                 'display `(space :align-to (,center-px)))
                                   (propertize (make-string code-w ?\s) 'face 'default))))
             (div-str (propertize " │ " 'face 'omnidiff-lineno))
             (code-pad (make-string gutter-w ?\s))
             (before-lbl " BEFORE ")
             (after-lbl " AFTER  ")
             (before-fill (max 1 (- code-w (length before-lbl)))))
        (insert (propertize code-pad 'face 'omnidiff-side-bar)
                (propertize before-lbl 'face 'omnidiff-side-before)
                (if center-px
                    (propertize " " 'face 'omnidiff-side-bar
                                'display `(space :align-to (,center-px)))
                  (propertize (make-string before-fill ?\s) 'face 'omnidiff-side-bar))
                (propertize " │ " 'face 'omnidiff-side-bar)
                (propertize code-pad 'face 'omnidiff-side-bar)
                (propertize after-lbl 'face 'omnidiff-side-after)
                (propertize "\n" 'face 'omnidiff-side-bar))
        (dolist (row (omnidiff-json-overview-hunk-rows hunk))
          (let* ((left (omnidiff-json-aligned-row-left row))
                 (right (omnidiff-json-aligned-row-right row))
                 (lop (and left (omnidiff-json-line-cell-op left)))
                 (rop (and right (omnidiff-json-line-cell-op right)))
                 (lstyle (when left
                           (omnidiff-render--op-style lop (omnidiff-json-line-cell-moved left))))
                 (rstyle (when right
                           (omnidiff-render--op-style rop (omnidiff-json-line-cell-moved right))))
                 (lline-face (when left (omnidiff-render--json-line-face lop highlight-style)))
                 (rline-face (when right (omnidiff-render--json-line-face rop highlight-style)))
                 (lln (and left (omnidiff-json-line-cell-line left)))
                 (rln (and right (omnidiff-json-line-cell-line right)))
                 (lsegs (when left
                          (omnidiff-render--wrap-propertized-text
                           (omnidiff-render--compose-json-cell-text left highlight-style)
                           code-w (omnidiff-json-line-cell-move-ref left))))
                 (rsegs (when right
                          (omnidiff-render--wrap-propertized-text
                           (omnidiff-render--compose-json-cell-text right highlight-style)
                           code-w (omnidiff-json-line-cell-move-ref right))))
                 (jump-ln (or rln lln))
                 (first-p t))
            (while (or lsegs rsegs)
              (let ((line-start (point))
                    (lseg (pop lsegs))
                    (rseg (pop rsegs))
                    (div-start nil)
                    (div-end nil))
                (if (null lseg)
                    (insert empty-left)
                  (insert (propertize (if first-p (format ln-fmt lln) blank-ln)
                                      'face (omnidiff-op-style-gutter-face lstyle))
                          (if first-p
                              (omnidiff-op-style-rail-sign-str lstyle)
                            (omnidiff-op-style-cont-rail-str lstyle)))
                  (omnidiff-render--insert-propertized-code
                   (car lseg) lline-face (cdr lseg) code-w center-px))
                (setq div-start (point))
                (insert div-str)
                (setq div-end (point))
                (if (null rseg)
                    (insert empty-prefix "\n")
                  (insert (propertize (if first-p (format ln-fmt rln) blank-ln)
                                      'face (omnidiff-op-style-gutter-face rstyle))
                          (if first-p
                              (omnidiff-op-style-rail-sign-str rstyle)
                            (omnidiff-op-style-cont-rail-str rstyle)))
                  (omnidiff-render--insert-propertized-code
                   (car rseg) rline-face (cdr rseg))
                  (insert (omnidiff-render--json-newline-str rline-face)))
                (let ((line-end (point)))
                  (when jump-ln
                    (let ((row-props (list 'omnidiff-line jump-ln
                                           'omnidiff-side (cond ((and lln rln) 'both)
                                                                (rln 'after)
                                                                (lln 'before)))))
                      (when lln
                        (setq row-props (nconc row-props (list 'omnidiff-old-line lln))))
                      (when rln
                        (setq row-props (nconc row-props (list 'omnidiff-new-line rln))))
                      (add-text-properties line-start line-end row-props)))
                  (when lln
                    (add-text-properties line-start div-start
                                         (list 'omnidiff-line lln
                                               'omnidiff-side 'before)))
                  (when rln
                    (add-text-properties div-end line-end
                                         (list 'omnidiff-line rln
                                               'omnidiff-side 'after))))
                (setq first-p nil)))))
        (omnidiff-render--finalize-hunk hdr body-start sline)))))

;;; Structured container diff cards

(defun omnidiff-render--format-bytes (bytes)
  "Format byte count BYTES into a compact human-readable string."
  (cond
   ((< bytes 1024) (format "%d B" bytes))
   ((< bytes (* 1024 1024)) (format "%.1f KiB" (/ bytes 1024.0)))
   (t (format "%.1f MiB" (/ bytes 1048576.0)))))

(defun omnidiff-render--container-family-label (family)
  "Return human-readable label for container FAMILY symbol."
  (pcase family
    ('archives "Archive")
    ('documents "Document")
    ('fonts "Font")
    ('cursors "Cursor")
    ('catalogs "Catalog")
    (_ (capitalize (string-replace "-" " " (symbol-name (or family 'container)))))))

(defun omnidiff-render--container-counts (container)
  "Return plist `(:changed N :added M :removed K :unchanged U)' for CONTAINER."
  (let ((changed 0)
        (added 0)
        (removed 0)
        (unchanged (omnidiff-json-container-unchanged container)))
    (dolist (m (omnidiff-json-container-members container))
      (pcase (omnidiff-json-container-member-status m)
        ('changed (setq changed (1+ changed)))
        ('added (setq added (1+ added)))
        ('removed (setq removed (1+ removed)))
        ('same (setq unchanged (1+ unchanged)))))
    (list :changed changed :added added :removed removed :unchanged unchanged)))

(defun omnidiff-render--container-side-label (side)
  "Format one `omnidiff-json-container-side' SIDE concisely, or nil."
  (when side
    (let ((fmt (omnidiff-json-container-side-format side))
          (cnt (omnidiff-json-container-side-members side))
          (bytes (omnidiff-json-container-side-bytes side)))
      (format "%s (%d %s, %s)"
              (if (string-empty-p fmt) "container" fmt)
              cnt (if (= cnt 1) "member" "members")
              (omnidiff-render--format-bytes bytes)))))

(defun omnidiff-render--format-picture-detail (pic)
  "Format concise summary string for `omnidiff-picture' PIC."
  (let ((cmp (omnidiff-picture-comparison pic))
        (before (omnidiff-picture-before pic))
        (after (omnidiff-picture-after pic))
        (pct (omnidiff-picture-changed-pct pic))
        (rcount (omnidiff-picture-region-count pic)))
    (pcase cmp
      ('identical "identical pixels")
      ('pixels
       (format "%.1f%% pixels changed (%d %s)"
               (or pct 0.0) rcount (if (= rcount 1) "region" "regions")))
      ('resized-scaled
       (if (and before after)
           (format "resized %d×%d → %d×%d, %.1f%% changed (%d %s)"
                   (omnidiff-picture-info-width before)
                   (omnidiff-picture-info-height before)
                   (omnidiff-picture-info-width after)
                   (omnidiff-picture-info-height after)
                   (or pct 0.0) rcount (if (= rcount 1) "region" "regions"))
         (format "resized, %.1f%% changed (%d %s)"
                 (or pct 0.0) rcount (if (= rcount 1) "region" "regions"))))
      ('resized
       (if (and before after)
           (format "resized %d×%d → %d×%d"
                   (omnidiff-picture-info-width before)
                   (omnidiff-picture-info-height before)
                   (omnidiff-picture-info-width after)
                   (omnidiff-picture-info-height after))
         "resized"))
      ('one-sided
       (let ((info (or after before)))
         (if info
             (format "%s %d×%d"
                     (omnidiff-picture-info-format info)
                     (omnidiff-picture-info-width info)
                     (omnidiff-picture-info-height info))
           "image")))
      (_ (or (omnidiff-picture-raw-comparison pic) "image changed")))))

(defun omnidiff-render--format-container-detail (kind detail)
  "Format human-readable member detail string from KIND and DETAIL."
  (pcase kind
    ('picture
     (when (omnidiff-picture-p detail)
       (omnidiff-render--format-picture-detail detail)))
    ('text
     (when (listp detail)
       (let* ((rem (or (plist-get detail :removed) 0))
              (add (or (plist-get detail :added) 0))
              (blines (or (plist-get detail :before-lines) 0))
              (alines (or (plist-get detail :after-lines) 0))
              (ws (plist-get detail :whitespace))
              (words-kept (plist-get detail :words-kept))
              (hints (delq nil
                           (list
                            (pcase ws
                              ('trailing "trailing whitespace")
                              ('anywhere "whitespace only")
                              (_ nil))
                            (cond
                             ((and words-kept (not ws)) "words kept")
                             ((and (not words-kept) (not ws)
                                   (> blines 0) (= rem blines) (= add alines))
                              "rewritten")
                             (t nil))))))
         (concat (format "-%d +%d lines" rem add)
                 (when hints
                   (format " (%s)" (string-join hints ", ")))))))
    ('binary
     (when (listp detail)
       (format "%d -> %d bytes"
               (or (plist-get detail :before-bytes) 0)
               (or (plist-get detail :after-bytes) 0))))
    ('content
     (cond
      ((omnidiff-picture-p detail)
       (omnidiff-render--format-picture-detail detail))
      ((omnidiff-json-container-p detail)
       (let ((cnts (omnidiff-render--container-counts detail)))
         (format "%s: %d changed · %d added · %d removed · %d unchanged"
                 (omnidiff-render--container-family-label
                  (omnidiff-json-container-family detail))
                 (plist-get cnts :changed)
                 (plist-get cnts :added)
                 (plist-get cnts :removed)
                 (plist-get cnts :unchanged))))
      (t nil)))
    (_ nil)))

(defun omnidiff-render--insert-container-card (file container counts hunk-idx hunk-total)
  "Insert a structured container diff card for FILE and CONTAINER."
  (let* ((family-lbl (omnidiff-render--container-family-label
                      (omnidiff-json-container-family container)))
         (b-lbl (omnidiff-render--container-side-label (omnidiff-json-container-before container)))
         (a-lbl (omnidiff-render--container-side-label (omnidiff-json-container-after container)))
         (side-meta (cond
                     ((and b-lbl a-lbl) (format "%s → %s" b-lbl a-lbl))
                     (a-lbl (format "added %s" a-lbl))
                     (b-lbl (format "removed %s" b-lbl))
                     (t nil)))
         (changed (plist-get counts :changed))
         (added (plist-get counts :added))
         (removed (plist-get counts :removed))
         (unchanged (plist-get counts :unchanged))
         (hdr (omnidiff-render--insert-hunk-header
               (format "%s diff" family-lbl)
               added removed changed 0 side-meta nil hunk-idx hunk-total))
         (body-start (point)))
    ;; Container summary strip.
    (insert "  "
            (propertize family-lbl 'face 'bold)
            (propertize " · " 'face 'omnidiff-commit-meta)
            (propertize (format "%d changed" changed) 'face 'omnidiff-container-member-changed)
            (propertize " · " 'face 'omnidiff-commit-meta)
            (propertize (format "%d added" added) 'face 'omnidiff-container-member-added)
            (propertize " · " 'face 'omnidiff-commit-meta)
            (propertize (format "%d removed" removed) 'face 'omnidiff-container-member-removed)
            (propertize (format " · %d unchanged" unchanged) 'face 'omnidiff-commit-meta)
            "\n")
    ;; Changed / added / removed member rows.
    (dolist (member (omnidiff-json-container-members container))
      (let ((mstatus (omnidiff-json-container-member-status member)))
        (unless (eq mstatus 'same)
          (let* ((row-start (point))
                 (badge (pcase mstatus
                          ('changed (propertize " ~ changed " 'face 'omnidiff-container-member-changed))
                          ('added (propertize " + added   " 'face 'omnidiff-container-member-added))
                          ('removed (propertize " - removed " 'face 'omnidiff-container-member-removed))
                          (_ (propertize (format " %s " mstatus) 'face 'omnidiff-commit-meta))))
                 (key (omnidiff-render--display-path
                       (omnidiff-json-container-member-key member)))
                 (detail (omnidiff-render--format-container-detail
                          (omnidiff-json-container-member-detail-kind member)
                          (omnidiff-json-container-member-detail member))))
            (insert "  " badge " " key)
            (when (and detail (not (string-empty-p detail)))
              (insert (propertize (concat "  · " detail) 'face 'omnidiff-commit-meta)))
            (insert "\n")
            (add-text-properties row-start (point)
                                 (list 'omnidiff-container-file file
                                       'omnidiff-container-member member
                                       'mouse-face 'highlight
                                       'help-echo "Click or press RET to inspect member"
                                       'keymap omnidiff-render--container-row-map))))))
    (insert "  "
            (propertize "RET inspect member · T open in omnidiff TUI"
                        'face 'omnidiff-commit-meta)
            "\n")
    (omnidiff-render--finalize-hunk hdr body-start nil)))

(defun omnidiff-render--trivial-summary-p (summary)
  "Return non-nil if SUMMARY should auto-fold.
Controlled by `omnidiff-overview-auto-fold-trivial'."
  (pcase omnidiff-overview-auto-fold-trivial
    ('nil nil)
    ('whitespace-and-comments
     (memq summary '(whitespace-only comment-only)))
    (_
     (memq summary '(whitespace-only comment-only refactor-moved-only no-changes)))))

;;; File cards

(defun omnidiff-render--display-path (path)
  "Escape control characters in PATH for single-line card and header display."
  (if (string-match-p "[\x00-\x1f\x7f]" path)
      (replace-regexp-in-string
       "[\x00-\x1f\x7f]"
       (lambda (s)
         (pcase (aref s 0)
           (?\n "\\n")
           (?\r "\\r")
           (?\t "\\t")
           (ch (format "\\x%02x" ch))))
       path t t)
    path))

(defun omnidiff-render--insert-picture-hunk (file pic hunk-idx hunk-total)
  "Insert a foldable picture diff or SVG preview hunk for FILE and PIC.
Return the `omnidiff-image-ov' overlay owning the interactive image body."
  (pcase-let* ((pre-start (point))
               (`(,label . ,meta) (omnidiff-image-hunk-summary pic))
               (hdr (omnidiff-render--insert-hunk-header
                     label 0 0 0 0 meta nil hunk-idx hunk-total))
               (body-start (point))
               (card-ov (omnidiff-image-insert-body file pic)))
    (omnidiff-render--finalize-hunk hdr body-start nil)
    ;; Cover leading spacer line too so moving down from the file header stays active.
    (add-text-properties pre-start
                         (omnidiff-header-span-end hdr)
                         (list 'omnidiff-image-ov card-ov
                               'keymap omnidiff-render--image-fold-header-map))
    card-ov))

(defun omnidiff-render--insert-file (file idx total &optional folded-files)
  "Insert formatted `omnidiff-file' card (IDX of TOTAL) with foldable body.
Optional FOLDED-FILES controls initial fold state: when a hash table, folds
exactly the target paths present in it; when a list, folds those paths in
addition to `omnidiff-overview-auto-fold-trivial'; when nil, uses only
`omnidiff-overview-auto-fold-trivial'."
  (when-let* ((file-pre (omnidiff-file-preamble file)))
    (omnidiff-render--insert-preamble file-pre))
  (let* ((path (omnidiff-file-path file))
         (target (omnidiff-file-target-path file))
         (status (omnidiff-file-status file))
         (old-mode (omnidiff-file-old-mode file))
         (new-mode (omnidiff-file-new-mode file))
         (mode-changed-p (and old-mode new-mode
                              (not (string= old-mode new-mode))
                              (not (memq status '(added deleted)))))
         (json-diff (omnidiff-file-json-diff file))
         (container (or (omnidiff-file-container file)
                        (and json-diff (omnidiff-json-diff-container json-diff))))
         (summary (or (omnidiff-file-summary file)
                      (and json-diff (omnidiff-json-diff-summary json-diff))))
         (summary-label (omnidiff-engine-summary-label summary))
         (large-residual (or (omnidiff-file-large-residual file)
                             (and json-diff (omnidiff-json-diff-large-residual json-diff))))
         (binary-p (or (omnidiff-file-binary-p file)
                       (and json-diff (omnidiff-json-diff-binary-p json-diff))))
         (pic (omnidiff-image-file-picture file))
         (render-pic-p (and pic (omnidiff-image-should-render-preview-p pic)))
         (svg-preview-p (and render-pic-p (omnidiff-picture-svg-p pic)))
         (container-counts (when container (omnidiff-render--container-counts container)))
         (auto-fold-count-p
          (and (integerp omnidiff-overview-auto-fold-threshold)
               (> omnidiff-overview-auto-fold-threshold 0)
               (>= total omnidiff-overview-auto-fold-threshold)))
         (initial-fold-p
          (cond
           ((hash-table-p folded-files)
            (not (null (gethash target folded-files))))
           ((consp folded-files)
            (or (not (null (member target folded-files)))
                auto-fold-count-p
                (omnidiff-render--trivial-summary-p summary)))
           (t
            (or auto-fold-count-p
                (omnidiff-render--trivial-summary-p summary)))))
         (json-eligible-p
          (and json-diff
               (null container)
               (not (and render-pic-p (not svg-preview-p)))
               (not (and binary-p
                         (null (and (omnidiff-json-diff-before json-diff)
                                    (omnidiff-json-side-hunks
                                     (omnidiff-json-diff-before json-diff))))
                         (null (and (omnidiff-json-diff-after json-diff)
                                    (omnidiff-json-side-hunks
                                     (omnidiff-json-diff-after json-diff))))))))
         (lazy-fold-p (and initial-fold-p json-eligible-p (not render-pic-p)))
         (json-plan (when json-eligible-p
                      (if lazy-fold-p
                          (omnidiff-render--cached-json-plan file json-diff)
                        (omnidiff-render--get-json-plan file json-diff))))
         (file-stats-zero-p (and (= (omnidiff-file-added file) 0)
                                 (= (omnidiff-file-removed file) 0)
                                 (= (omnidiff-file-updated file) 0)
                                 (= (omnidiff-file-moved file) 0)))
         (added (cond
                 ((not file-stats-zero-p) (omnidiff-file-added file))
                 (json-plan (omnidiff-json-render-plan-added json-plan))
                 (container-counts (plist-get container-counts :added))
                 (t 0)))
         (removed (cond
                   ((not file-stats-zero-p) (omnidiff-file-removed file))
                   (json-plan (omnidiff-json-render-plan-removed json-plan))
                   (container-counts (plist-get container-counts :removed))
                   (t 0)))
         (updated (cond
                   ((not file-stats-zero-p) (omnidiff-file-updated file))
                   (json-plan (omnidiff-json-render-plan-updated json-plan))
                   (container-counts (plist-get container-counts :changed))
                   (t 0)))
         (moved (cond
                 ((not file-stats-zero-p) (omnidiff-file-moved file))
                 (json-plan (omnidiff-json-render-plan-moved json-plan))
                 (t 0)))
         (max-line (max 1
                        (omnidiff-file-max-line file)
                        (if json-plan (omnidiff-json-render-plan-max-line json-plan) 0)))
         (num-width (max 4 (length (number-to-string max-line))))
         (badge (cl-ecase status
                  (added (propertize " NEW " 'face 'omnidiff-file-status-added))
                  (deleted (propertize " DEL " 'face 'omnidiff-file-status-deleted))
                  (renamed (propertize " REN " 'face 'omnidiff-file-status-renamed))
                  (modified nil)))
         (title (if (eq status 'renamed)
                    (format "%s → %s"
                            (omnidiff-render--display-path path)
                            (omnidiff-render--display-path target))
                  (omnidiff-render--display-path path)))
         (idx-width (length (number-to-string (max 1 total))))
         (idx-fmt (format "[%%%dd/%%d]" idx-width))
         (pic-stats (omnidiff-image-header-stats file))
         (stat-parts
          (delq nil
                (append
                 (list badge)
                 pic-stats
                 (list
                      (when (> added 0)
                        (propertize (format "+%d" added) 'face 'omnidiff-stat-added))
                      (when (> removed 0)
                        (propertize (format "-%d" removed) 'face 'omnidiff-stat-removed))
                      (when (> updated 0)
                        (propertize (format "~%d" updated) 'face 'omnidiff-stat-changed))
                      (when (> moved 0)
                        (propertize (format ">%d" moved) 'face 'omnidiff-stat-moved))
                      (when mode-changed-p
                        (propertize (format "%s→%s" old-mode new-mode)
                                    'face 'omnidiff-stat-index))
                      (propertize (format idx-fmt idx total) 'face 'omnidiff-stat-index)))))
         (stats-str (string-join stat-parts (propertize "  " 'face 'omnidiff-file-header)))
         (stats-px (omnidiff-render--right-align-px stats-str 16))
         (win (or (get-buffer-window (current-buffer) t) (selected-window)))
         (win-cols (if (window-live-p win) (window-body-width win) 120))
         (extra-cols (+ 8 ; "▎ ▼ " + trailing spacing
                        (omnidiff-render--display-width stats-str)
                        (if summary-label (+ 4 (omnidiff-render--display-width summary-label)) 0)
                        (if large-residual 18 0)
                        (if (and (hash-table-p omnidiff--reviewed-files)
                                 (gethash target omnidiff--reviewed-files))
                            2
                          0)))
         (shown-title (omnidiff-render--truncate-path
                       title (max 20 (- win-cols extra-cols))))
         (file-label (format "%s [%d/%d]" title idx total))
         (reviewed-p (and (hash-table-p omnidiff--reviewed-files)
                          (not (null (gethash target omnidiff--reviewed-files)))))
         (header-start (point))
         (fold-ov nil)
         (review-ov nil))
    (insert (propertize "▎ " 'face 'omnidiff-file-accent))
    (let ((tri-start (point)))
      (insert (propertize "▼" 'face 'omnidiff-file-accent))
      (setq fold-ov (make-overlay tri-start (point)))
      (overlay-put fold-ov 'omnidiff-ov t))
    (let ((rev-pos (point)))
      (setq review-ov (make-overlay rev-pos rev-pos))
      (overlay-put review-ov 'omnidiff-ov t)
      (when reviewed-p
        (overlay-put review-ov 'before-string
                     (propertize " ✓" 'face 'omnidiff-stat-added))))
    (insert (propertize (concat " " shown-title) 'face 'omnidiff-file-header))
    (when summary-label
      (insert (propertize "  " 'face 'omnidiff-file-header)
              (propertize (concat " " summary-label " ") 'face 'omnidiff-summary-badge)))
    (when large-residual
      (insert (propertize "  · large residual" 'face 'omnidiff-stat-index)))
    (insert
            (propertize "  " 'face 'omnidiff-file-header)
            (propertize " " 'face 'omnidiff-file-header
                        'display `(space :align-to (- right (,stats-px))))
            stats-str
            (propertize "\n" 'face 'omnidiff-file-header))
    (let ((header-end (point))
          (body-start (point))
          (note (omnidiff-file-note file))
          (json-hunks (and json-plan (omnidiff-json-render-plan-hunks json-plan)))
          (header-img-ov nil))
      (add-text-properties header-start header-end
                           (list 'omnidiff-file-header t
                                 'omnidiff-file target
                                 'omnidiff-file-label file-label
                                 'omnidiff-review-ov review-ov
                                 'mouse-face 'highlight
                                 'help-echo (if (string= shown-title title)
                                                "Click or press TAB to fold/unfold; RET/d for Dual"
                                              (format "%s — TAB to fold/unfold; RET/d for Dual"
                                                      title))
                                 'keymap omnidiff-render--fold-header-map))
      (let ((hunk-offset (if svg-preview-p 1 0)))
        (cond
         (lazy-fold-p nil)
         ((and render-pic-p (not svg-preview-p))
          ;; Raster image files flatten out the redundant 1/1 hunk header.
          (setq header-img-ov (omnidiff-image-insert-body file pic t)))
         (container
          (omnidiff-render--insert-container-card file container container-counts 1 1))
         (t
          (when svg-preview-p
            (let ((text-total (if json-hunks (length json-hunks) 0)))
              (setq header-img-ov
                    (omnidiff-render--insert-picture-hunk
                     file pic 1 (+ 1 text-total)))))
          (cond
           (json-hunks
            (let ((hunk-total (+ hunk-offset (length json-hunks)))
                  (hunk-idx hunk-offset)
                  (layout (or omnidiff--layout omnidiff-default-layout))
                  (hstyle (omnidiff-render-effective-highlight-style)))
              (dolist (hunk json-hunks)
                (setq hunk-idx (1+ hunk-idx))
                (cl-ecase layout
                  (side-by-side
                   (omnidiff-render--insert-json-sbs-hunk
                    hunk status num-width hstyle hunk-idx hunk-total))
                  (stacked
                   (omnidiff-render--insert-json-stacked-hunk
                    hunk status num-width hstyle hunk-idx hunk-total))
                  (unified
                   (omnidiff-render--insert-json-unified-hunk
                    hunk num-width hstyle hunk-idx hunk-total))))))
           ((or note mode-changed-p binary-p (and json-diff summary-label))
            (let ((display-note
                   (cond
                    ((and mode-changed-p
                          (or (null note)
                              (string= note "No changes - files are identical")))
                     (format "Mode changed: %s → %s" old-mode new-mode))
                    (note note)
                    (binary-p "Binary files differ")
                    (summary-label
                     (concat (upcase (substring summary-label 0 1))
                             (substring summary-label 1)))
                    (t "No changes")))
                  (note-start (point)))
              (insert (propertize (concat (make-string (+ num-width 2) ?\s) display-note "\n")
                                  'face 'omnidiff-gap))
              (put-text-property note-start (min (1+ note-start) (point))
                                 'omnidiff-hunk t)))))))
      (when header-img-ov
        (add-text-properties header-start header-end
                             (list 'omnidiff-image-ov header-img-ov
                                   'keymap omnidiff-render--image-fold-header-map)))
      (put-text-property body-start (point) 'omnidiff-file target)
      (insert "\n")
      (let ((ov (make-overlay body-start (point))))
        (overlay-put ov 'omnidiff-file-body t)
        (overlay-put ov 'omnidiff-ov t)
        (overlay-put ov 'omnidiff-fold-ov fold-ov)
        (overlay-put ov 'evaporate t)
        (when lazy-fold-p
          (overlay-put ov 'omnidiff-lazy-file file)
          (overlay-put ov 'omnidiff-lazy-num-width num-width))
        (when initial-fold-p
          (overlay-put ov 'invisible 'omnidiff-fold)
          (overlay-put fold-ov 'display
                       (propertize "▶" 'face 'omnidiff-file-accent)))
        (put-text-property header-start header-end 'omnidiff-body-ov ov)))))

;;; Header line & visible-position helpers

(defun omnidiff-render-overlay-header-pos (ov)
  "Return live start position of the header line owning fold overlay OV."
  (when (and ov (overlay-buffer ov))
    (let ((fold-ov (overlay-get ov 'omnidiff-fold-ov)))
      (when (and fold-ov (overlay-buffer fold-ov))
        (save-excursion
          (goto-char (overlay-start fold-ov))
          (line-beginning-position))))))

(defun omnidiff-render-visible-pos (pos)
  "Advance POS past any invisible folded overlays."
  (let ((p pos))
    (while (and (< p (point-max)) (invisible-p p))
      (setq p (next-single-char-property-change p 'invisible)))
    p))

(defun omnidiff-render-file-header-pos-at (&optional pos)
  "Return start position of the file header enclosing POS (default point), or nil."
  (let ((p (or pos (point))))
    (cond
     ((>= p (point-max)) nil)
     ((get-text-property p 'omnidiff-file-header)
      (save-excursion (goto-char p) (line-beginning-position)))
     ((when-let* ((ov (cl-find-if (lambda (o) (overlay-get o 'omnidiff-file-body))
                                  (overlays-at p))))
        (omnidiff-render-overlay-header-pos ov)))
     (t
      (save-excursion
        (goto-char p)
        (when-let* ((m (text-property-search-backward 'omnidiff-file-header)))
          (prop-match-beginning m)))))))

(defun omnidiff-render--header-hints (&optional context)
  "Return cached `(HINTS-STRING . HINTS-PX)' for header-line CONTEXT.
CONTEXT is `default', `image', `container', or `standalone'."
  (let* ((ctx (or context 'default))
         (key (list omnidiff-theme--revision
                    (selected-frame)
                    (display-graphic-p)
                    (frame-char-width)
                    (face-font 'omnidiff-header-line)))
         (table (if (equal (car omnidiff-render--hints-cache) key)
                    (cdr omnidiff-render--hints-cache)
                  (let ((fresh nil))
                    (setq omnidiff-render--hints-cache (cons key fresh))
                    fresh)))
         (found (assq ctx table)))
    (if found
        (cdr found)
      (let* ((text (pcase ctx
                     ('image "1-6 mode · b flip · h/l adj · ? help · q ")
                     ('container "RET inspect · T tui · TAB · ? help · q ")
                     ('standalone "n/p · TAB · v · c · ? help · q ")
                     (_ "RET/d dual · TAB · R review · v · ? help · q ")))
             (hints (propertize text 'face 'omnidiff-header-line))
             (hints-px (omnidiff-render--right-align-px hints 8))
             (entry (cons hints hints-px)))
        (setcdr omnidiff-render--hints-cache (cons (cons ctx entry) table))
        entry))))

(defun omnidiff-render--header-context ()
  "Detect active UI context at point for dynamic Overview header-line hints."
  (cond
   ((omnidiff-image--card-ov-at) 'image)
   ((or (get-text-property (point) 'omnidiff-container-member)
        (get-text-property (point) 'omnidiff-container-file))
    'container)
   ((null omnidiff--command) 'standalone)
   (t 'default)))

(defun omnidiff-render-header-line ()
  "Build the fixed header-line with sticky current-file orientation."
  (let* ((layout-tag (cl-ecase (or omnidiff--layout omnidiff-default-layout)
                       (side-by-side "side-by-side")
                       (stacked "stacked")
                       (unified "unified")))
         (preset-tag (cl-ecase omnidiff--preset
                       (minimal "minimal")
                       (full "full")
                       (default "default")))
         (mode-tag (format "%s · %s" layout-tag preset-tag))
         (vis-start (omnidiff-render-visible-pos (window-start)))
         (top-hdr (omnidiff-render-file-header-pos-at vis-start))
         (scrolled-into-file (and top-hdr (< top-hdr vis-start)))
         (file-label (and scrolled-into-file
                          (get-text-property top-hdr 'omnidiff-file-label)))
         (hints-pair (omnidiff-render--header-hints (omnidiff-render--header-context)))
         (hints (car hints-pair))
         (hints-px (cdr hints-pair))
         (win (selected-window))
         (win-cols (if (window-live-p win) (window-body-width win) 120))
         (hints-w (omnidiff-render--display-width hints))
         (show-hints (>= win-cols (+ hints-w 36)))
         (rev-cnt (omnidiff-render-reviewed-count))
         (rev-str (when (and (> omnidiff--file-count 0) (> rev-cnt 0))
                    (format "  ·  reviewed %d/%d" rev-cnt omnidiff--file-count)))
         (meta-tail (if file-label
                        (format "  ·  %s%s  ·  [%s]"
                                (or omnidiff--label "diff")
                                (or rev-str "")
                                mode-tag)
                      (format "  %s  ·  %d %s%s  ·  [%s]"
                              (or omnidiff--label "diff")
                              omnidiff--file-count
                              (if (= omnidiff--file-count 1) "file" "files")
                              (or rev-str "")
                              mode-tag)))
         (context (if file-label
                      (let* ((budget (- win-cols 14
                                        (if show-hints (+ hints-w 2) 0)
                                        (omnidiff-render--display-width meta-tail)))
                             (shown (omnidiff-render--truncate-path
                                     file-label (max 16 budget))))
                        (concat (propertize "  " 'face 'omnidiff-header-line)
                                (propertize shown 'face 'omnidiff-header-file)
                                (propertize meta-tail 'face 'omnidiff-header-line)))
                    (propertize meta-tail 'face 'omnidiff-header-line))))
    (if show-hints
        (concat
         (propertize " OMNIDIFF " 'face 'omnidiff-header-badge)
         context
         (propertize "  " 'face 'omnidiff-header-line)
         (propertize " " 'face 'omnidiff-header-line
                     'display `(space :align-to (- right (,hints-px))))
         hints)
      (concat
       (propertize " OMNIDIFF " 'face 'omnidiff-header-badge)
       context))))

;;; Top-level render entry point

(defun omnidiff-render-buffer (raw-or-diff &optional folded-files)
  "Render RAW-OR-DIFF into the current `omnidiff-mode' buffer.
RAW-OR-DIFF may be a raw CLI output string or a parsed `omnidiff-diff'.
Optional FOLDED-FILES is a hash table of exact folded target paths, a list
of additional folded target paths, or nil for default auto-folding."
  (let* ((gc-cons-threshold most-positive-fixnum)
         (gc-cons-percentage 0.8)
         (diff (if (omnidiff-diff-p raw-or-diff)
                   raw-or-diff
                 (make-omnidiff-diff :preamble raw-or-diff :files nil)))
         (preamble (omnidiff-diff-preamble diff))
         (files (omnidiff-diff-files diff))
         (total (length files))
         (omnidiff-render--style-cache (make-hash-table :test #'equal :size 16))
         (omnidiff-render--sbs-metrics-cache (make-hash-table :test #'eql :size 4))
         (inhibit-read-only t)
         (old-size (- (point-max) (point-min))))
    (run-hook-with-args 'before-change-functions (point-min) (point-max))
    (let ((inhibit-modification-hooks t))
      (remove-overlays (point-min) (point-max) 'omnidiff-ov t)
      (erase-buffer)
      (omnidiff-image-begin-render)
      (omnidiff-image-prefetch-blobs files omnidiff--repo-dir)
      (setq omnidiff--file-count total)
      (omnidiff-render--insert-preamble preamble)
      (if (= total 0)
          (when (or (null preamble) (string-blank-p preamble))
            (insert (propertize "\n  No changes to display.\n" 'face 'omnidiff-commit-meta)))
        (let ((idx 0))
          (dolist (file files)
            (setq idx (1+ idx))
            (omnidiff-render--insert-file file idx total folded-files)))))
    (run-hook-with-args 'after-change-functions (point-min) (point-max) old-size)
    (setq-local header-line-format '(:eval (omnidiff-render-header-line)))
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(provide 'omnidiff-render)
;;; omnidiff-render.el ends here
