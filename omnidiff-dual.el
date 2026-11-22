;;; omnidiff-dual.el --- Native dual semantic source viewer for omnidiff -*- lexical-binding: t; -*-

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

;; Native Emacs Dual Semantic Source Viewer backed by `omnidiff-engine.el'
;; (`omnidiff --mode json') and `omnidiff-theme.el'.
;;
;; Highlights only changed semantic tokens inside real source buffers with
;; native Emacs syntax highlighting, zero synthetic padding lines, active
;; change focus, counterpart cross-highlighting, left-fringe change bars,
;; a cached SVG/TTY header-line minimap, unchanged-region folding with
;; enclosing scope breadcrumbs, single/dual/auto layouts, and live editing
;; of worktree AFTER buffers with asynchronous diff refreshes.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'omnidiff-engine)
(require 'omnidiff-theme)
(require 'face-remap)
(require 'transient)

(declare-function define-fringe-bitmap "fringe"
                  (bitmap bits &optional height width align))
(declare-function display-line-numbers-mode "display-line-numbers"
                  (&optional arg))
(declare-function image-type-available-p "image" (type))
(declare-function create-image "image"
                  (file-or-data &optional type data-p &rest props))
(declare-function tab-bar-new-tab "tab-bar" (&optional arg))
(declare-function tab-bar-close-tab "tab-bar" (&optional arg to-index))
(declare-function tab-bar-tabs "tab-bar" (&optional frame))

(defvar display-line-numbers-mode)
(defvar delayed-mode-hooks)
(defvar enable-dir-local-variables)

;;; Customization

(defcustom omnidiff-dual-layout 'auto
  "Pane layout for the dual semantic source viewer.
- `auto': Side-by-side dual panes when frame width >= `omnidiff-dual-min-width',
  otherwise a single flip-able pane.
- `dual': Always display BEFORE and AFTER side by side.
- `single': Display one pane at a time (switch with `o')."
  :type '(choice (const :tag "Auto (width threshold)" auto)
                 (const :tag "Dual (side by side)" dual)
                 (const :tag "Single (one pane)" single))
  :group 'omnidiff)

(defcustom omnidiff-dual-min-width 100
  "Minimum frame width in columns for `auto' layout to show dual panes."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-dual-show-minimap t
  "When non-nil, show a compact change-distribution minimap in the header."
  :type 'boolean
  :group 'omnidiff)

(defcustom omnidiff-dual-show-fringe t
  "When non-nil, show slim left-fringe colored bars on changed lines in GUI."
  :type 'boolean
  :group 'omnidiff)

(defcustom omnidiff-dual-fold-unchanged nil
  "Initial folding policy for unchanged line runs in the dual viewer.
- nil: Show full source buffers without folding.
- `auto': Fold unchanged runs when max line count exceeds
  `omnidiff-dual-auto-fold-lines'.
- t: Always fold unchanged runs on open."
  :type '(choice (const :tag "Full source (unfolded)" nil)
                 (const :tag "Auto-fold large files" auto)
                 (const :tag "Always fold unchanged runs" t))
  :group 'omnidiff)

(defcustom omnidiff-dual-context-lines 5
  "Number of unchanged context lines kept visible around each changed hunk."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-dual-auto-fold-lines 600
  "Line count threshold above which `auto' folding collapses unchanged runs."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-dual-sync-scroll t
  "When non-nil, keep the counterpart line aligned in the opposite pane."
  :type 'boolean
  :group 'omnidiff)

(defcustom omnidiff-dual-highlight-counterpart t
  "When non-nil, highlight the counterpart range in the opposite pane."
  :type 'boolean
  :group 'omnidiff)

(defcustom omnidiff-dual-live-update 'on-save
  "When to recompute semantic overlays while editing the AFTER buffer.
- nil: Refresh only manually (`C-c C-r' or returning to review mode).
- `on-save': Recompute asynchronously whenever the buffer is saved.
- `idle': Recompute on save and after `omnidiff-dual-live-delay' idle seconds."
  :type '(choice (const :tag "Manual only" nil)
                 (const :tag "On save" on-save)
                 (const :tag "On idle and save" idle))
  :group 'omnidiff)

(defcustom omnidiff-dual-live-delay 0.35
  "Idle delay in seconds before recomputing overlays in `idle' live mode."
  :type 'number
  :group 'omnidiff)

(defcustom omnidiff-dual-window-setup 'current-frame
  "How `omnidiff-dual-open' arranges windows for a new session.
- `current-frame': Use the current frame, restoring window config on quit.
- `new-tab': Open the session in a new `tab-bar' tab, closing it on quit.
- A function: Called as `(FN BEFORE-BUF AFTER-BUF SESSION)'."
  :type '(choice (const :tag "Current frame" current-frame)
                 (const :tag "New tab-bar tab" new-tab)
                 (function :tag "Custom setup function"))
  :group 'omnidiff)

(defcustom omnidiff-dual-syntax-max-bytes (* 512 1024)
  "Maximum buffer size in bytes for automatic syntax highlighting.
Ephemeral revision buffers larger than this skip `set-auto-mode' and
`font-lock-mode' to stay responsive on huge generated files."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-dual-after-diff-hook nil
  "Normal hook run after a dual session finishes applying a diff."
  :type 'hook
  :group 'omnidiff)

;;; Session & buffer-local state

(cl-defstruct (omnidiff-dual--session (:copier nil))
  "Runtime state for a dual semantic source viewer session."
  (id 0 :type integer)
  (before-buf nil)
  (after-buf nil)
  (before-win nil)
  (after-win nil)
  (before-owns-buf-p nil :type boolean)
  (after-owns-buf-p nil :type boolean)
  (before-orig-read-only nil)
  (after-orig-read-only nil)
  (before-orig-line-numbers nil)
  (after-orig-line-numbers nil)
  (before-orig-header-line nil)
  (after-orig-header-line nil)
  (before-orig-header-line-local-p nil :type boolean)
  (after-orig-header-line-local-p nil :type boolean)
  (before-orig-font-lock nil)
  (after-orig-font-lock nil)
  (before-hl-remap-cookie nil)
  (after-hl-remap-cookie nil)
  (before-path nil)
  (after-path nil)
  (before-label nil)
  (after-label nil)
  (display-path nil)
  (json-diff nil)
  (alignment nil)
  (stops nil :type list)
  (stop-index 0 :type integer)
  (preset 'default :type symbol)
  (whole-updates nil :type boolean)
  (paint-reindent-moves nil :type boolean)
  (layout 'auto :type symbol)
  (active-side 'after :type symbol) ; before | after
  (fold-unchanged nil :type boolean)
  (show-minimap t :type boolean)
  (show-syntax t :type boolean)
  (show-cross-highlight t :type boolean)
  (saved-winconf nil)
  (created-tab-p nil :type boolean)
  (created-tab-frame nil)
  (created-tab-id nil)
  (tmpdir nil)
  (live-timer nil)
  (active-proc nil)
  (request-seq 0 :type integer)
  (editable-after-p nil :type boolean)
  (editing-p nil :type boolean)
  (nav-callbacks nil :type list)
  (before-line-starts nil)
  (after-line-starts nil)
  (before-lines-tick nil)
  (after-lines-tick nil)
  (before-lines nil)
  (after-lines nil)
  (minimap-cache nil)
  (active-ovs nil :type list)
  (cross-ovs nil :type list)
  (last-cursor-state nil)
  (last-jump nil)
  (mutated-p nil :type boolean)
  (tearing-down-p nil :type boolean))

(defvar omnidiff-dual--sessions nil
  "List of currently active `omnidiff-dual--session' instances.")

(defvar omnidiff-dual--next-id 0
  "Monotonic counter for `omnidiff-dual--session' identifiers.")

(defvar omnidiff-dual--in-post-command nil
  "Non-nil while `omnidiff-dual--post-command' is executing.")

(defvar omnidiff-dual--fringe-bitmap-defined nil
  "Non-nil once `omnidiff-dual-bar' fringe bitmap has been registered.")

(defvar-local omnidiff-dual--session nil
  "Active `omnidiff-dual--session' attached to the current buffer.")

(defvar-local omnidiff-dual--side nil
  "Pane side (`before' or `after') for the current buffer in a dual session.")

;;;###autoload
(defun omnidiff-dual-active-session ()
  "Return the active `omnidiff-dual--session' for the current buffer, or nil."
  (when (and (omnidiff-dual--session-p omnidiff-dual--session)
             (memq omnidiff-dual--session omnidiff-dual--sessions))
    omnidiff-dual--session))

(defun omnidiff-dual--require-session ()
  "Return the active dual session for the current buffer or signal an error."
  (or (omnidiff-dual-active-session)
      (user-error "No active OmniDiff dual session in this buffer")))

;;; Small side accessors & coordinate helpers

(defun omnidiff-dual--other-side (side)
  "Return the opposite pane symbol for SIDE (`before' or `after')."
  (if (eq side 'before) 'after 'before))

(defun omnidiff-dual--side-buf (session side)
  "Return the buffer on SIDE (`before' or `after') for SESSION."
  (if (eq side 'before)
      (omnidiff-dual--session-before-buf session)
    (omnidiff-dual--session-after-buf session)))

(defun omnidiff-dual--side-win (session side)
  "Return the recorded window on SIDE (`before' or `after') for SESSION."
  (if (eq side 'before)
      (omnidiff-dual--session-before-win session)
    (omnidiff-dual--session-after-win session)))

(defun omnidiff-dual--side-line-starts (session side)
  "Return the cached line-offset vector on SIDE for SESSION."
  (if (eq side 'before)
      (omnidiff-dual--session-before-line-starts session)
    (omnidiff-dual--session-after-line-starts session)))

(defun omnidiff-dual--side-lines-tick (session side)
  "Return the cached `buffer-chars-modified-tick' on SIDE for SESSION."
  (if (eq side 'before)
      (omnidiff-dual--session-before-lines-tick session)
    (omnidiff-dual--session-after-lines-tick session)))

(defun omnidiff-dual--line-starts-fresh-p (buf line-starts tick)
  "Return non-nil when LINE-STARTS still matches BUF at TICK."
  (and (buffer-live-p buf)
       line-starts
       (> (length line-starts) 0)
       (integerp tick)
       (= tick (buffer-chars-modified-tick buf))
       (<= (aref line-starts (1- (length line-starts)))
           (with-current-buffer buf
             (save-restriction (widen) (point-max))))))

(defun omnidiff-dual--side-lines (session side)
  "Return the cached line-strings vector on SIDE for SESSION."
  (if (eq side 'before)
      (omnidiff-dual--session-before-lines session)
    (omnidiff-dual--session-after-lines session)))

(defun omnidiff-dual--side-json (session side)
  "Return the `omnidiff-json-side' on SIDE for SESSION, or nil."
  (when-let* ((diff (omnidiff-dual--session-json-diff session)))
    (if (eq side 'before)
        (omnidiff-json-diff-before diff)
      (omnidiff-json-diff-after diff))))

(defun omnidiff-dual--logical-line-count (buf line-starts)
  "Return the number of logical lines in BUF given LINE-STARTS."
  (let ((n (if line-starts (length line-starts) 0)))
    (if (or (<= n 0) (not (buffer-live-p buf)))
        0
      (with-current-buffer buf
        (save-restriction
          (widen)
          (cond
           ((= (point-min) (point-max)) 0)
           ((and (> n 1) (= (aref line-starts (1- n)) (point-max))) (1- n))
           (t n)))))))

(defun omnidiff-dual--buffer-lines (buf line-starts)
  "Extract a vector of 0-based line strings from BUF using LINE-STARTS."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (let* ((starts-len (length line-starts))
             (n (omnidiff-dual--logical-line-count buf line-starts))
             (pmax (point-max))
             (vec (make-vector n "")))
        (dotimes (i n)
          (let ((beg (aref line-starts i))
                (end (if (< (1+ i) starts-len)
                         (1- (aref line-starts (1+ i)))
                       pmax)))
            (aset vec i (buffer-substring-no-properties beg end))))
        vec))))

(defun omnidiff-dual--find-row-for-pos (line-starts pos)
  "Return 0-based row index in LINE-STARTS containing buffer position POS."
  (let ((lo 0)
        (hi (1- (length line-starts))))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi 1) 2)))
        (if (<= (aref line-starts mid) pos)
            (setq lo mid)
          (setq hi (1- mid)))))
    lo))

(defun omnidiff-dual--resolve-line-starts-tick (buf line-starts tick)
  "Return TICK or the matching session tick for LINE-STARTS in BUF."
  (or tick
      (when (and line-starts (buffer-live-p buf))
        (with-current-buffer buf
          (when (and (omnidiff-dual--session-p omnidiff-dual--session)
                     omnidiff-dual--side
                     (eq line-starts
                         (omnidiff-dual--side-line-starts
                          omnidiff-dual--session omnidiff-dual--side)))
            (omnidiff-dual--side-lines-tick
             omnidiff-dual--session omnidiff-dual--side))))))

(defun omnidiff-dual--point-to-row-byte-col (buf line-starts pos &optional tick)
  "Convert buffer position POS in BUF to `(ROW . BYTE-COL)' (0-based).
Falls back to `line-number-at-pos' when LINE-STARTS is stale against TICK."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (let* ((pmin (point-min))
             (pmax (point-max))
             (p (max pmin (min pos pmax)))
             (eff-tick (omnidiff-dual--resolve-line-starts-tick buf line-starts tick))
             (use-table (and line-starts
                             (> (length line-starts) 0)
                             (integerp eff-tick)
                             (= eff-tick (buffer-chars-modified-tick buf))
                             (<= (aref line-starts (1- (length line-starts))) pmax)))
             (row (if use-table
                      (omnidiff-dual--find-row-for-pos line-starts p)
                    (1- (line-number-at-pos p t))))
             (row-beg (if (and use-table (< row (length line-starts)))
                          (max pmin (min p (aref line-starts row)))
                        (save-excursion
                          (goto-char p)
                          (max pmin (min p (line-beginning-position))))))
             (byte-col (string-bytes (buffer-substring-no-properties row-beg p))))
        (cons row byte-col)))))

(defun omnidiff-dual--row-byte-col-to-point (buf line-starts row byte-col &optional tick)
  "Convert 0-based (ROW, BYTE-COL) to a clamped buffer position in BUF.
Falls back to `goto-char' + `forward-line' when LINE-STARTS is stale."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (let* ((pmin (point-min))
             (pmax (point-max))
             (n (if line-starts (length line-starts) 0))
             (eff-tick (omnidiff-dual--resolve-line-starts-tick buf line-starts tick))
             (use-table (and (> n 0)
                             (integerp eff-tick)
                             (= eff-tick (buffer-chars-modified-tick buf))
                             (<= (aref line-starts (1- n)) pmax))))
        (if use-table
            (let* ((r (max 0 (min (or row 0) (1- n))))
                   (row-beg (max pmin (min pmax (aref line-starts r))))
                   (row-end (max row-beg
                                 (min pmax
                                      (if (< (1+ r) n)
                                          (1- (aref line-starts (1+ r)))
                                        pmax))))
                   (line (buffer-substring-no-properties row-beg row-end))
                   (char-off (omnidiff-engine-line-byte-to-char line (or byte-col 0))))
              (max pmin (min pmax (min row-end (+ row-beg char-off)))))
          (save-excursion
            (goto-char pmin)
            (when (and (integerp row) (> row 0))
              (forward-line row))
            (let* ((row-beg (max pmin (min pmax (line-beginning-position))))
                   (row-end (max row-beg (min pmax (line-end-position))))
                   (line (buffer-substring-no-properties row-beg row-end))
                   (char-off (omnidiff-engine-line-byte-to-char line (or byte-col 0))))
              (max pmin (min pmax (min row-end (+ row-beg char-off)))))))))))

(defun omnidiff-dual--range-last-row (range num-rows)
  "Return inclusive 0-based last row of RANGE within NUM-ROWS, or nil."
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

(defun omnidiff-dual--range-nonempty-p (range)
  "Return non-nil when `omnidiff-json-range' RANGE covers at least one byte."
  (when range
    (let ((sr (omnidiff-json-range-start-row range))
          (sc (omnidiff-json-range-start-col range))
          (er (omnidiff-json-range-end-row range))
          (ec (omnidiff-json-range-end-col range)))
      (or (> er sr) (and (= er sr) (> ec sc))))))

(defun omnidiff-dual--range-contains-p (range row byte-col)
  "Return non-nil if right-open RANGE contains (ROW, BYTE-COL)."
  (when range
    (let ((sr (omnidiff-json-range-start-row range))
          (sc (omnidiff-json-range-start-col range))
          (er (omnidiff-json-range-end-row range))
          (ec (omnidiff-json-range-end-col range)))
      (and (or (< sr row) (and (= sr row) (<= sc byte-col)))
           (or (< row er) (and (= row er) (< byte-col ec)))))))

(defun omnidiff-dual--range-covers-row-p (range row)
  "Return non-nil if non-empty RANGE covers 0-based ROW."
  (when (omnidiff-dual--range-nonempty-p range)
    (let* ((sr (omnidiff-json-range-start-row range))
           (er (omnidiff-json-range-end-row range))
           (ec (omnidiff-json-range-end-col range))
           (lr (if (and (> er sr) (= ec 0)) (1- er) er)))
      (and (<= sr row) (<= row lr)))))

(defun omnidiff-dual--hunk-at (hunks row byte-col)
  "Return the most specific hunk in HUNKS covering (ROW, BYTE-COL), or on ROW.
Exact span containment is preferred; if point lies outside all spans on ROW,
falls back to the nearest hunk on ROW."
  (let ((exact nil)
        (exact-span most-positive-fixnum)
        (on-row nil)
        (on-row-dist most-positive-fixnum))
    (dolist (h hunks)
      (let ((r (omnidiff-json-hunk-range h)))
        (cond
         ((omnidiff-dual--range-contains-p r row byte-col)
          (let ((span (+ (* 100000 (- (omnidiff-json-range-end-row r)
                                      (omnidiff-json-range-start-row r)))
                         (abs (- (omnidiff-json-range-end-col r)
                                 (omnidiff-json-range-start-col r))))))
            (when (< span exact-span)
              (setq exact h
                    exact-span span))))
         ((and (null exact) (omnidiff-dual--range-covers-row-p r row))
          (let ((dist (if (= (omnidiff-json-range-start-row r) row)
                          (abs (- (omnidiff-json-range-start-col r) byte-col))
                        0)))
            (when (< dist on-row-dist)
              (setq on-row h
                    on-row-dist dist)))))))
    (or exact on-row)))

(defun omnidiff-dual--invoke-callback (session key &optional missing-msg)
  "Invoke callback KEY from SESSION's `:nav-callbacks' plist."
  (let ((fn (plist-get (omnidiff-dual--session-nav-callbacks session) key)))
    (cond
     ((functionp fn)
      (let ((arity (func-arity fn)))
        (if (and (consp arity) (eql (cdr arity) 0))
            (funcall fn)
          (funcall fn session))))
     (missing-msg
      (message "%s" missing-msg)))))

;;; Fringe, faces & overlays

(defun omnidiff-dual--token-face (op)
  "Return semantic token face for hunk operation OP."
  (pcase op
    ('insert 'omnidiff-token-insert)
    ('delete 'omnidiff-token-delete)
    ('update 'omnidiff-token-update)
    ('move 'omnidiff-token-move)
    (_ 'omnidiff-token-update)))

(defun omnidiff-dual--gutter-face (op)
  "Return dual gutter face for hunk operation OP."
  (pcase op
    ('insert 'omnidiff-dual-gutter-insert)
    ('delete 'omnidiff-dual-gutter-delete)
    ('update 'omnidiff-dual-gutter-update)
    ('move 'omnidiff-dual-gutter-move)
    (_ 'omnidiff-dual-gutter-update)))

(defun omnidiff-dual--op-priority (op)
  "Return visual prominence rank for operation OP (higher wins per row)."
  (pcase op
    ('update 3)
    ('insert 2)
    ('delete 2)
    ('move 1)
    (_ 0)))

(defun omnidiff-dual--ensure-fringe-bitmap ()
  "Register distinct accessible left-fringe bitmaps per diff operation."
  (when (and (not omnidiff-dual--fringe-bitmap-defined)
             (fboundp 'define-fringe-bitmap))
    (ignore-errors
      ;; Solid 3px bar (default / insert).
      (define-fringe-bitmap 'omnidiff-dual-bar [#b11100000] nil 8 '(top t))
      (define-fringe-bitmap 'omnidiff-dual-bar-insert [#b11100000] nil 8 '(top t))
      ;; Bold dashed bar (delete).
      (define-fringe-bitmap 'omnidiff-dual-bar-delete
                            [#b11100000 #b11100000 #b11100000 #b00000000 #b00000000]
                            nil 8 '(top t))
      ;; Double hairline rail (update).
      (define-fringe-bitmap 'omnidiff-dual-bar-update [#b10100000] nil 8 '(top t))
      ;; Subtle 1px dotted bar (move).
      (define-fringe-bitmap 'omnidiff-dual-bar-move
                            [#b01000000 #b00000000] nil 8 '(top t))
      (setq omnidiff-dual--fringe-bitmap-defined t)))
  omnidiff-dual--fringe-bitmap-defined)

(defun omnidiff-dual--fringe-bitmap (op)
  "Return the fringe bitmap symbol corresponding to diff operation OP."
  (pcase op
    ('insert 'omnidiff-dual-bar-insert)
    ('delete 'omnidiff-dual-bar-delete)
    ('update 'omnidiff-dual-bar-update)
    ('move 'omnidiff-dual-bar-move)
    (_ 'omnidiff-dual-bar)))

(defun omnidiff-dual--clear-overlays (buf &optional kind)
  "Remove dual viewer overlays from BUF.
When KIND is non-nil, only overlays whose `omnidiff-dual-kind' matches KIND
are removed; otherwise all `omnidiff-dual-ov' overlays are deleted."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (save-restriction
        (widen)
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (and (overlay-get ov 'omnidiff-dual-ov)
                     (or (null kind)
                         (eq (overlay-get ov 'omnidiff-dual-kind) kind)))
            (delete-overlay ov)))))))

(defun omnidiff-dual--clear-focus-overlays (session)
  "Delete active-stop and counterpart cross-highlight overlays for SESSION."
  (dolist (ov (omnidiff-dual--session-active-ovs session))
    (when (overlayp ov) (delete-overlay ov)))
  (dolist (ov (omnidiff-dual--session-cross-ovs session))
    (when (overlayp ov) (delete-overlay ov)))
  (setf (omnidiff-dual--session-active-ovs session) nil
        (omnidiff-dual--session-cross-ovs session) nil))

(defun omnidiff-dual--make-range-overlays (buf line-starts range face priority kind &optional tick)
  "Create per-line overlays for RANGE in BUF with FACE, PRIORITY, and KIND.
Returns the list of created overlays, or nil if LINE-STARTS is stale."
  (when (and (buffer-live-p buf)
             line-starts
             (omnidiff-dual--range-nonempty-p range))
    (let ((eff-tick (omnidiff-dual--resolve-line-starts-tick buf line-starts tick)))
      (when (if (integerp eff-tick)
                (omnidiff-dual--line-starts-fresh-p buf line-starts eff-tick)
              (and (> (length line-starts) 0)
                   (<= (aref line-starts (1- (length line-starts)))
                       (with-current-buffer buf
                         (save-restriction (widen) (point-max))))))
        (let ((created nil))
          (dolist (reg (omnidiff-engine-range-to-buffer-regions range line-starts buf))
            (let ((beg (car reg))
                  (end (cdr reg)))
              (when (< beg end)
                (let ((ov (make-overlay beg end buf)))
                  (overlay-put ov 'face face)
                  (overlay-put ov 'priority priority)
                  (overlay-put ov 'omnidiff-dual-ov t)
                  (overlay-put ov 'omnidiff-dual-kind kind)
                  (overlay-put ov 'evaporate t)
                  (push ov created)))))
          (nreverse created))))))

(defun omnidiff-dual--apply-side-overlays (session side)
  "Apply semantic token and left-fringe overlays on SIDE for SESSION."
  (let* ((buf (omnidiff-dual--side-buf session side))
         (line-starts (omnidiff-dual--side-line-starts session side))
         (json-side (omnidiff-dual--side-json session side))
         (hunks (and json-side (omnidiff-json-side-hunks json-side)))
         (num-rows (if line-starts (length line-starts) 0))
         (use-fringe (and omnidiff-dual-show-fringe
                          (display-graphic-p)
                          (omnidiff-dual--ensure-fringe-bitmap)))
         (row-ops (when (and use-fringe (> num-rows 0))
                    (make-vector num-rows nil))))
    (when (buffer-live-p buf)
      (omnidiff-dual--clear-overlays buf 'token)
      (omnidiff-dual--clear-overlays buf 'fringe)
      (dolist (hunk hunks)
        (let* ((op (omnidiff-json-hunk-operation hunk))
               (rng (omnidiff-json-hunk-range hunk))
               (face (omnidiff-dual--token-face op)))
          (dolist (reg (omnidiff-engine-range-to-buffer-regions rng line-starts buf))
            (let ((beg (car reg))
                  (end (cdr reg)))
              (when (< beg end)
                (let ((ov (make-overlay beg end buf)))
                  (overlay-put ov 'face face)
                  (overlay-put ov 'priority 10)
                  (overlay-put ov 'omnidiff-dual-ov t)
                  (overlay-put ov 'omnidiff-dual-kind 'token)
                  (overlay-put ov 'omnidiff-dual-hunk hunk)
                  (overlay-put ov 'evaporate t)))))
          (when row-ops
            (when-let* ((last-row (omnidiff-dual--range-last-row rng num-rows)))
              (let ((r (omnidiff-json-range-start-row rng))
                    (prio (omnidiff-dual--op-priority op)))
                (while (<= r last-row)
                  (let ((cur (aref row-ops r)))
                    (when (or (null cur) (> prio (omnidiff-dual--op-priority cur)))
                      (aset row-ops r op)))
                  (setq r (1+ r))))))))
      (when row-ops
        (dotimes (r num-rows)
          (when-let* ((op (aref row-ops r)))
            (let* ((pos (aref line-starts r))
                   (ov (make-overlay pos pos buf))
                   (gface (omnidiff-dual--gutter-face op)))
              (overlay-put ov 'before-string
                           (propertize " " 'display
                                       (list 'left-fringe
                                             (omnidiff-dual--fringe-bitmap op)
                                             gface)))
              (overlay-put ov 'priority 5)
              (overlay-put ov 'omnidiff-dual-ov t)
              (overlay-put ov 'omnidiff-dual-kind 'fringe))))))))

;;; Folding unchanged line runs

(defun omnidiff-dual--fold-overlay-at (pos)
  "Return the fold overlay covering POS in the current buffer, or nil.
Also matches when POS sits on the visible trailing newline (or end boundary)
immediately following a fold overlay."
  (or (cl-loop for ov in (overlays-at pos)
               when (and (overlay-get ov 'omnidiff-dual-ov)
                         (eq (overlay-get ov 'omnidiff-dual-kind) 'fold))
               return ov)
      (when (> pos (point-min))
        (cl-loop for ov in (overlays-at (1- pos))
                 when (and (overlay-get ov 'omnidiff-dual-ov)
                           (eq (overlay-get ov 'omnidiff-dual-kind) 'fold)
                           (= (overlay-end ov) pos))
                 return ov))))

(defun omnidiff-dual--delete-fold-overlay (ov)
  "Delete fold overlay OV and its paired trailing-newline overlay, if any."
  (when (overlayp ov)
    (let ((nl-ov (overlay-get ov 'omnidiff-dual-fold-nl-ov)))
      (when (overlayp nl-ov)
        (delete-overlay nl-ov)))
    (delete-overlay ov)))

(defun omnidiff-dual--expand-fold-at (pos)
  "Expand any fold overlay covering POS.  Return non-nil if a fold was opened."
  (when-let* ((ov (omnidiff-dual--fold-overlay-at pos)))
    (omnidiff-dual--delete-fold-overlay ov)
    t))

(defun omnidiff-dual--isearch-open-fold (ov)
  "Delete fold overlay OV when incremental search matches inside it."
  (omnidiff-dual--delete-fold-overlay ov))

(defun omnidiff-dual--mouse-expand-fold (event)
  "Expand the unchanged-lines fold banner clicked by mouse EVENT."
  (interactive "e")
  (let* ((start (event-start event))
         (win (posn-window start))
         (pos (posn-point start)))
    (when (and (window-live-p win) (integerp pos))
      (select-window win)
      (with-current-buffer (window-buffer win)
        (goto-char pos)
        (when (omnidiff-dual--expand-fold-at pos)
          (message "Expanded fold"))))))

(defconst omnidiff-dual--fold-banner-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'omnidiff-dual--mouse-expand-fold)
    map)
  "Keymap allowing mouse clicks to expand dual-viewer fold banners.")

(defun omnidiff-dual--fold-decl-for-run (hunks lines run-end)
  "Return trimmed enclosing declaration for a fold ending at RUN-END, or nil."
  (let ((num-lines (length lines))
        (max-sr (+ run-end omnidiff-dual-context-lines 1))
        (found nil))
    (dolist (h hunks)
      (unless found
        (let ((sr (omnidiff-json-range-start-row (omnidiff-json-hunk-range h)))
              (ref (omnidiff-json-hunk-reference-line h)))
          (when (and (> sr run-end)
                     (<= sr max-sr)
                     (integerp ref)
                     (>= ref 0)
                     (<= ref run-end)
                     (< ref num-lines))
            (let ((text (string-trim (aref lines ref))))
              (unless (string-empty-p text)
                (setq found (if (> (length text) 64)
                                (concat (substring text 0 61) "...")
                              text))))))))
    found))

(defun omnidiff-dual--apply-folds-side (session side)
  "Apply or clear unchanged-region fold overlays on SIDE for SESSION."
  (let* ((buf (omnidiff-dual--side-buf session side))
         (line-starts (omnidiff-dual--side-line-starts session side))
         (lines (omnidiff-dual--side-lines session side))
         (fold-p (omnidiff-dual--session-fold-unchanged session)))
    (when (buffer-live-p buf)
      (omnidiff-dual--clear-overlays buf 'fold)
      (omnidiff-dual--clear-overlays buf 'fold-nl)
      (with-current-buffer buf
        (if (not fold-p)
            (when (listp buffer-invisibility-spec)
              (remove-from-invisibility-spec 'omnidiff-dual-fold))
          (when (omnidiff-dual--line-starts-fresh-p
                 buf line-starts (omnidiff-dual--side-lines-tick session side))
            (let* ((num-lines (omnidiff-dual--logical-line-count buf line-starts))
                 (json-side (omnidiff-dual--side-json session side))
                 (other-side (omnidiff-dual--other-side side))
                 (other-json (omnidiff-dual--side-json session other-side))
                 (hunks (and json-side (omnidiff-json-side-hunks json-side)))
                 (other-hunks (and other-json (omnidiff-json-side-hunks other-json)))
                 (alignment (omnidiff-dual--session-alignment session))
                 (ctx omnidiff-dual-context-lines))
            (when (and (> num-lines 2) (or hunks other-hunks))
              (let ((visible (make-vector num-lines nil))
                    (any-marked nil))
                (cl-flet ((mark-range (r0 r1)
                            (let ((lo (max 0 (- r0 ctx)))
                                  (hi (min (1- num-lines) (+ r1 ctx))))
                              (setq any-marked t)
                              (while (<= lo hi)
                                (aset visible lo t)
                                (setq lo (1+ lo))))))
                  (dolist (h hunks)
                    (when-let* ((lr (omnidiff-dual--range-last-row
                                     (omnidiff-json-hunk-range h) num-lines)))
                      (mark-range (omnidiff-json-range-start-row
                                   (omnidiff-json-hunk-range h))
                                  lr)))
                  (when alignment
                    (let ((other-n (omnidiff-dual--logical-line-count
                                    (omnidiff-dual--side-buf session other-side)
                                    (omnidiff-dual--side-line-starts session other-side))))
                      (dolist (h other-hunks)
                        (when-let* ((lr (omnidiff-dual--range-last-row
                                         (omnidiff-json-hunk-range h) (max 1 other-n))))
                          (let* ((sr (omnidiff-json-range-start-row
                                      (omnidiff-json-hunk-range h)))
                                 (m0 (omnidiff-engine-map-row alignment other-side sr))
                                 (m1 (omnidiff-engine-map-row alignment other-side lr)))
                            (mark-range (min m0 m1) (max m0 m1))))))))
                (when any-marked
                  (when (listp buffer-invisibility-spec)
                    (add-to-invisibility-spec 'omnidiff-dual-fold))
                  (let ((row 0)
                        (starts-len (length line-starts))
                        (pmax (save-restriction (widen) (point-max))))
                    (while (< row num-lines)
                      (if (aref visible row)
                          (setq row (1+ row))
                        (let ((run-start row))
                          (while (and (< row num-lines) (not (aref visible row)))
                            (setq row (1+ row)))
                          (let* ((run-end (1- row))
                                 (count (1+ (- run-end run-start))))
                            (when (>= count 2)
                              (let* ((beg (aref line-starts run-start))
                                     (has-nl (< (1+ run-end) starts-len))
                                     (end (if has-nl
                                              (aref line-starts (1+ run-end))
                                            pmax))
                                     (fold-end (if (and (> end beg)
                                                        (eq (char-before end) ?\n))
                                                   (1- end)
                                                 end))
                                     (decl (omnidiff-dual--fold-decl-for-run
                                            hunks lines run-end))
                                     (banner-text
                                      (format "  ··· %d unchanged %s ···%s"
                                              count
                                              (if (= count 1) "line" "lines")
                                              (if decl (format "  @ %s" decl) "")))
                                     (ov (make-overlay beg fold-end buf)))
                                (overlay-put ov 'invisible 'omnidiff-dual-fold)
                                (overlay-put ov 'before-string
                                             (propertize banner-text
                                                         'face 'omnidiff-dual-fold-banner
                                                         'mouse-face 'highlight
                                                         'help-echo "Click or press RET to expand unchanged lines"
                                                         'keymap omnidiff-dual--fold-banner-map))
                                (overlay-put ov 'isearch-open-invisible
                                             #'omnidiff-dual--isearch-open-fold)
                                (overlay-put ov 'priority 40)
                                (overlay-put ov 'omnidiff-dual-ov t)
                                (overlay-put ov 'omnidiff-dual-kind 'fold)
                                (when (< fold-end end)
                                  (let ((nl-ov (make-overlay fold-end end buf)))
                                    (overlay-put nl-ov 'face 'omnidiff-dual-fold-banner)
                                    (overlay-put nl-ov 'priority 40)
                                    (overlay-put nl-ov 'omnidiff-dual-ov t)
                                    (overlay-put nl-ov 'omnidiff-dual-kind 'fold-nl)
                                    (overlay-put ov 'omnidiff-dual-fold-nl-ov nl-ov))))))))))))))))))))

(defun omnidiff-dual--apply-folds (session)
  "Apply or clear fold overlays on both panes of SESSION."
  (omnidiff-dual--apply-folds-side session 'before)
  (omnidiff-dual--apply-folds-side session 'after))

;;; Header-line minimap & status chrome

(defun omnidiff-dual--minimap-fill-buckets (buckets total hunks)
  "Populate BUCKETS vector across TOTAL rows with dominant op from HUNKS."
  (let ((nb (length buckets)))
    (dolist (h hunks)
      (let* ((op (omnidiff-json-hunk-operation h))
             (prio (omnidiff-dual--op-priority op))
             (rng (omnidiff-json-hunk-range h))
             (sr (omnidiff-json-range-start-row rng))
             (lr (or (omnidiff-dual--range-last-row rng total) sr))
             (b0 (min (1- nb) (max 0 (/ (* sr nb) total))))
             (b1 (min (1- nb) (max b0 (/ (* lr nb) total))))
             (bi b0))
        (while (<= bi b1)
          (let ((cur (aref buckets bi)))
            (if (null cur)
                (aset buckets bi (cons op 1))
              (let ((cur-op (car cur))
                    (cnt (cdr cur)))
                (aset buckets bi
                      (cons (if (> prio (omnidiff-dual--op-priority cur-op)) op cur-op)
                            (1+ cnt))))))
          (setq bi (1+ bi)))))))

(defun omnidiff-dual--minimap-svg-rects (buckets colors)
  "Build SVG tick `<rect>' fragments from BUCKETS and semantic COLORS plist."
  (let ((parts nil)
        (nb (length buckets)))
    (dotimes (i nb)
      (when-let* ((cell (aref buckets i)))
        (let* ((op (car cell))
               (fill (pcase op
                       ('insert (plist-get colors :gutter-insert))
                       ('delete (plist-get colors :gutter-delete))
                       ('update (plist-get colors :gutter-update))
                       ('move (plist-get colors :gutter-move))
                       (_ (plist-get colors :accent))))
               (x (+ 2 (* i 2))))
          (when fill
            (push (format "<rect x=\"%d\" y=\"1.5\" width=\"2\" height=\"7\" rx=\"0.5\" fill=\"%s\"/>"
                          x fill)
                  parts)))))
    (apply #'concat (nreverse parts))))

(defun omnidiff-dual--ensure-minimap-cache (session)
  "Return up-to-date minimap cache plist for SESSION."
  (let* ((diff (omnidiff-dual--session-json-diff session))
         (b-hunks (and diff (omnidiff-json-side-hunks (omnidiff-json-diff-before diff))))
         (a-hunks (and diff (omnidiff-json-side-hunks (omnidiff-json-diff-after diff))))
         (b-count (omnidiff-dual--logical-line-count
                   (omnidiff-dual--session-before-buf session)
                   (omnidiff-dual--session-before-line-starts session)))
         (a-count (omnidiff-dual--logical-line-count
                   (omnidiff-dual--session-after-buf session)
                   (omnidiff-dual--session-after-line-starts session)))
         (total (max 1 b-count a-count))
         (cache (omnidiff-dual--session-minimap-cache session)))
    (if (and cache
             (eql (plist-get cache :rev) omnidiff-theme--revision)
             (eql (plist-get cache :total) total)
             (eq (plist-get cache :b-hunks) b-hunks)
             (eq (plist-get cache :a-hunks) a-hunks))
        cache
      (let* ((colors (omnidiff-theme-semantic-colors))
             (gui-buckets (make-vector 50 nil))
             (tty-buckets (make-vector 16 nil)))
        (omnidiff-dual--minimap-fill-buckets gui-buckets total b-hunks)
        (omnidiff-dual--minimap-fill-buckets gui-buckets total a-hunks)
        (omnidiff-dual--minimap-fill-buckets tty-buckets total b-hunks)
        (omnidiff-dual--minimap-fill-buckets tty-buckets total a-hunks)
        (let ((new-cache
               (list :rev omnidiff-theme--revision
                     :total total
                     :b-hunks b-hunks
                     :a-hunks a-hunks
                     :colors colors
                     :gui-rects (omnidiff-dual--minimap-svg-rects gui-buckets colors)
                     :tty-buckets tty-buckets
                     :last-gui-p nil
                     :last-cursor-bucket nil
                     :rendered nil)))
          (setf (omnidiff-dual--session-minimap-cache session) new-cache)
          new-cache)))))

(defun omnidiff-dual--minimap-click (event)
  "Jump proportionally within the pane when clicking the header-line minimap."
  (interactive "e")
  (let* ((start (event-start event))
         (win (posn-window start))
         (buf (and (window-live-p win) (window-buffer win))))
    (when (buffer-live-p buf)
      (select-window win)
      (with-current-buffer buf
        (when-let* ((session (omnidiff-dual-active-session))
                    (side omnidiff-dual--side))
          (let* ((starts (omnidiff-dual--side-line-starts session side))
                 (tick (omnidiff-dual--side-lines-tick session side))
                 (total (max 1 (omnidiff-dual--logical-line-count buf starts)))
                 (obj-xy (posn-object-x-y start))
                 (obj-wh (posn-object-width-height start))
                 (frac
                  (cond
                   ((and (consp obj-xy) (consp obj-wh)
                         (numberp (car obj-xy)) (numberp (car obj-wh))
                         (> (car obj-wh) 0))
                    (/ (float (car obj-xy)) (float (car obj-wh))))
                   ((and (consp (posn-string start))
                         (integerp (cdr (posn-string start)))
                         (stringp (car (posn-string start)))
                         (> (length (car (posn-string start))) 0))
                    (/ (float (cdr (posn-string start)))
                       (float (length (car (posn-string start))))))
                   (t 0.5)))
                 (clamped (max 0.0 (min 1.0 frac)))
                 (target-row (min (1- total) (max 0 (round (* clamped (1- total))))))
                 (pos (omnidiff-dual--row-byte-col-to-point buf starts target-row 0 tick)))
            (setf (omnidiff-dual--session-active-side session) side)
            (omnidiff-dual--expand-fold-at pos)
            (goto-char pos)
            (ignore-errors (recenter))
            (omnidiff-dual--sync-focus-and-counterpart session nil t)))))))

(defconst omnidiff-dual--minimap-map
  (let ((map (make-sparse-keymap)))
    (define-key map [header-line mouse-1] #'omnidiff-dual--minimap-click)
    (define-key map [mouse-1] #'omnidiff-dual--minimap-click)
    map)
  "Keymap making the dual header-line minimap clickable to scrub/jump.")

(defun omnidiff-dual--render-minimap (session)
  "Render cached SVG or TTY change-distribution minimap string for SESSION."
  (when (omnidiff-dual--session-show-minimap session)
    (let* ((cache (omnidiff-dual--ensure-minimap-cache session))
           (total (plist-get cache :total))
           (gui-p (and (display-graphic-p)
                       (fboundp 'image-type-available-p)
                       (image-type-available-p 'svg)
                       (fboundp 'create-image)))
           (nb (if gui-p 50 16))
           (cur-side (omnidiff-dual--session-active-side session))
           (cur-buf (omnidiff-dual--side-buf session cur-side))
           (cur-row (if (buffer-live-p cur-buf)
                        (car (omnidiff-dual--point-to-row-byte-col
                              cur-buf
                              (omnidiff-dual--side-line-starts session cur-side)
                              (with-current-buffer cur-buf (point))
                              (omnidiff-dual--side-lines-tick session cur-side)))
                      0))
           (cursor-bucket (min (1- nb) (max 0 (/ (* cur-row nb) total)))))
      (if (and (plist-get cache :rendered)
               (eq (plist-get cache :last-gui-p) gui-p)
               (eql (plist-get cache :last-cursor-bucket) cursor-bucket))
          (plist-get cache :rendered)
        (let ((rendered
               (if gui-p
                   (let* ((colors (plist-get cache :colors))
                          (bg (or (plist-get colors :bg-chrome) "#262636"))
                          (border (or (plist-get colors :border) "#4d4e5b"))
                          (accent (or (plist-get colors :accent) "#61afef"))
                          (cx (+ 1 (* cursor-bucket 2)))
                          (svg (concat
                                "<svg xmlns=\"http://www.w3.org/2000/svg\" "
                                "width=\"104\" height=\"10\" viewBox=\"0 0 104 10\">"
                                (format "<rect x=\"0.5\" y=\"0.5\" width=\"103\" height=\"9\" rx=\"3\" ry=\"3\" fill=\"%s\" stroke=\"%s\" stroke-width=\"1\"/>"
                                        bg border)
                                (plist-get cache :gui-rects)
                                (format "<rect x=\"%d\" y=\"0.5\" width=\"3\" height=\"9\" rx=\"1\" fill=\"%s\" fill-opacity=\"0.9\"/>"
                                        cx accent)
                                "</svg>")))
                     (propertize " "
                                 'face 'omnidiff-dual-header-meta
                                 'display (create-image svg 'svg t :ascent 'center)
                                 'mouse-face 'highlight
                                 'help-echo "Click minimap to jump to position in file"
                                 'keymap omnidiff-dual--minimap-map))
                 (let ((buckets (plist-get cache :tty-buckets))
                       (parts nil))
                   (dotimes (i 16)
                     (let* ((cell (aref buckets i))
                            (ch (cond
                                 ((null cell) "·")
                                 ((>= (cdr cell) 2) "█")
                                 (t "▄")))
                            (base-face (if cell
                                           (omnidiff-dual--gutter-face (car cell))
                                         'omnidiff-dual-header-meta))
                            (face (if (= i cursor-bucket)
                                      (list :inherit base-face :underline t)
                                    base-face)))
                       (push (propertize ch
                                         'face face
                                         'mouse-face 'highlight
                                         'help-echo "Click minimap to jump to position in file"
                                         'keymap omnidiff-dual--minimap-map)
                             parts)))
                   (apply #'concat (nreverse parts))))))
          (plist-put cache :last-gui-p gui-p)
          (plist-put cache :last-cursor-bucket cursor-bucket)
          (plist-put cache :rendered rendered)
          rendered)))))

(defun omnidiff-dual--preset-pill (session)
  "Format preset and flag pill string for SESSION."
  (let* ((preset (or (omnidiff-dual--session-preset session) 'default))
         (parts (list (symbol-name preset))))
    (when (omnidiff-dual--session-whole-updates session)
      (push "+whole" parts))
    (when (omnidiff-dual--session-paint-reindent-moves session)
      (push "+reindent" parts))
    (propertize (concat " " (string-join (nreverse parts) " ") " ")
                'face 'omnidiff-dual-header-badge)))

(defun omnidiff-dual--truncate-label (label max-cols)
  "Truncate LABEL to at most MAX-COLS display columns, preserving tail path."
  (let ((w (string-width label)))
    (cond
     ((or (<= max-cols 0) (<= w max-cols)) label)
     ((<= max-cols 4) (truncate-string-to-width label max-cols nil nil "…"))
     ((string-search "/" label)
      (let* ((parts (split-string label "/"))
             (tail (car (last parts)))
             (rest (nreverse (butlast parts)))
             (acc tail))
        (while (and rest
                    (<= (+ 2 (string-width (car rest)) 1 (string-width acc)) max-cols))
          (setq acc (concat (pop rest) "/" acc)))
        (let ((cand (concat "…/" acc)))
          (if (<= (string-width cand) max-cols)
              cand
            (truncate-string-to-width cand max-cols nil nil "…")))))
     (t
      (truncate-string-to-width label max-cols nil nil "…")))))

(defun omnidiff-dual--header-line ()
  "Build dynamic `header-line-format' string for the current dual pane."
  (ignore-errors
    (let ((session (omnidiff-dual-active-session))
          (side omnidiff-dual--side))
      (when (and session side)
        (let* ((diff (omnidiff-dual--session-json-diff session))
               (stops (omnidiff-dual--session-stops session))
               (total-stops (length stops))
               (stop-idx (omnidiff-dual--session-stop-index session))
               (single-p (eq (omnidiff-dual--effective-layout session) 'single))
               (active-p (or single-p
                             (eq side (omnidiff-dual--session-active-side session))))
               (win (get-buffer-window (current-buffer)))
               (win-w (if (window-live-p win) (window-body-width win) 80))
               (sp (propertize "  " 'face 'omnidiff-dual-header-meta))
               (sep (propertize " · " 'face 'omnidiff-dual-header-meta))
               (file-face (if active-p
                              'omnidiff-dual-header-file
                            'omnidiff-dual-header-file-inactive))
               (lang (or (and diff (omnidiff-json-side-language
                                    (omnidiff-json-diff-before diff)))
                         (and diff (omnidiff-json-side-language
                                    (omnidiff-json-diff-after diff)))))
               (summary (and diff (omnidiff-engine-summary-label
                                   (omnidiff-json-diff-summary diff))))
               (minimap (when (or (eq side 'before) single-p)
                          (omnidiff-dual--render-minimap session)))
               (minimap-cols (if minimap (if (display-graphic-p) 15 16) 0))
               (counter (if (> total-stops 0)
                            (format "change %d/%d"
                                    (max 1 (min total-stops stop-idx))
                                    total-stops)
                          "no changes"))
               (parts nil))
          (if (eq side 'before)
              (let* ((raw-label (or (omnidiff-dual--session-before-label session)
                                    (omnidiff-dual--session-display-path session)
                                    "before"))
                     (pill-face (if active-p
                                    'omnidiff-dual-header-before
                                  'omnidiff-dual-header-before-inactive))
                     (tail-cols (+ 10 ; pill + leading space
                                   (if lang (+ 3 (string-width lang)) 0)
                                   (if summary (+ 4 (string-width summary)) 0)
                                   (if minimap (+ 2 minimap-cols) 0)
                                   (+ 2 (string-width counter))
                                   (if single-p 18 0)))
                     (label (omnidiff-dual--truncate-label
                             raw-label (max 14 (- win-w tail-cols 2)))))
                (push (propertize " BEFORE " 'face pill-face) parts)
                (push (propertize (concat " " label) 'face file-face) parts)
                (when (and lang (>= win-w 62))
                  (push sep parts)
                  (push (propertize lang 'face 'omnidiff-dual-header-meta) parts))
                (when summary
                  (push sp parts)
                  (push (propertize (concat " " summary " ")
                                    'face 'omnidiff-dual-header-badge)
                        parts))
                (when minimap
                  (push sp parts)
                  (push minimap parts))
                (push sp parts)
                (push (propertize counter 'face 'omnidiff-dual-header-meta) parts)
                (when single-p
                  (push sep parts)
                  (push (propertize "single (o:flip)" 'face 'omnidiff-dual-header-meta)
                        parts)))
            (let* ((raw-label (or (omnidiff-dual--session-after-label session)
                                  (omnidiff-dual--session-display-path session)
                                  "after"))
                   (pill-face (if active-p
                                  'omnidiff-dual-header-after
                                'omnidiff-dual-header-after-inactive))
                   (editing-p (omnidiff-dual--session-editing-p session))
                   (nav (omnidiff-dual--session-nav-callbacks session))
                   (f-idx (plist-get nav :file-index))
                   (f-tot (plist-get nav :file-total))
                   (rev-fn (plist-get nav :reviewed-p))
                   (reviewed-p (and (functionp rev-fn) (funcall rev-fn)))
                   (prog-fn (plist-get nav :review-progress))
                   (prog (when (functionp prog-fn) (funcall prog-fn)))
                   (rev-cnt (or (car-safe prog) 0))
                   (rev-tot (or (cdr-safe prog) 0))
                   (file-str (cond
                              ((and (integerp f-idx) (integerp f-tot) (> f-tot 0)
                                    (> rev-cnt 0) (> rev-tot 0))
                               (format "file %d/%d · reviewed %d/%d" f-idx f-tot rev-cnt rev-tot))
                              ((and (integerp f-idx) (integerp f-tot) (> f-tot 0))
                               (format "file %d/%d" f-idx f-tot))
                              ((and (> rev-cnt 0) (> rev-tot 0))
                               (format "reviewed %d/%d" rev-cnt rev-tot))))
                   (preset-pill (omnidiff-dual--preset-pill session))
                   (hints (if editing-p
                              "M-n/p C-c r C-c C-c:review C-c ?"
                            (if (omnidiff-dual--session-editable-after-p session)
                                "n/p RET r/u z v e ? q"
                              "n/p RET z v ? q")))
                   (show-hints (>= win-w 72))
                   (tail-cols (+ 10
                                 (if editing-p 9 0)
                                 (if reviewed-p 5 0)
                                 (+ 2 (string-width preset-pill))
                                 (if file-str (+ 2 (string-width file-str)) 0)
                                 (if (and single-p summary) (+ 4 (string-width summary)) 0)
                                 (if (and single-p minimap) (+ 2 minimap-cols) 0)
                                 (if single-p (+ 12 (string-width counter)) 0)
                                 (if show-hints (+ 2 (string-width hints)) 0)))
                   (label (omnidiff-dual--truncate-label
                           raw-label (max 14 (- win-w tail-cols 2)))))
              (push (propertize " AFTER  " 'face pill-face) parts)
              (push (propertize (concat " " label) 'face file-face) parts)
              (when reviewed-p
                (push (propertize " " 'face 'omnidiff-dual-header-meta) parts)
                (push (propertize " ✓ " 'face 'omnidiff-dual-header-badge) parts))
              (when editing-p
                (push (propertize " " 'face 'omnidiff-dual-header-meta) parts)
                (push (propertize " EDIT " 'face 'omnidiff-dual-header-badge) parts))
              (when (and single-p summary)
                (push sp parts)
                (push (propertize (concat " " summary " ")
                                  'face 'omnidiff-dual-header-badge)
                      parts))
              (push sp parts)
              (push preset-pill parts)
              (when file-str
                (push sp parts)
                (push (propertize file-str 'face 'omnidiff-dual-header-meta) parts))
              (when (and single-p minimap)
                (push sp parts)
                (push minimap parts))
              (when single-p
                (push sep parts)
                (push (propertize (concat counter " · o:flip")
                                  'face 'omnidiff-dual-header-meta)
                      parts))
              (when show-hints
                (push sp parts)
                (push (propertize hints 'face 'omnidiff-dual-header-meta) parts))))
          (apply #'concat (nreverse parts)))))))

(defun omnidiff-dual--refresh-headers (session)
  "Force header-line redisplay on both panes of SESSION."
  (dolist (side '(before after))
    (when-let* ((buf (omnidiff-dual--side-buf session side)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (force-mode-line-update))))))

(defun omnidiff-dual--on-theme-apply ()
  "Invalidate minimap caches and refresh headers after theme changes."
  (dolist (session omnidiff-dual--sessions)
    (setf (omnidiff-dual--session-minimap-cache session) nil)
    (omnidiff-dual--refresh-headers session)))

(add-hook 'omnidiff-theme-after-apply-hook #'omnidiff-dual--on-theme-apply)

;;; Layout & window management

(defun omnidiff-dual--effective-layout (session)
  "Return resolved layout (`dual' or `single') for SESSION."
  (pcase (omnidiff-dual--session-layout session)
    ('dual 'dual)
    ('single 'single)
    (_ (if (>= (frame-width) omnidiff-dual-min-width) 'dual 'single))))

(defun omnidiff-dual--dual-visible-p (session)
  "Return non-nil when SESSION's BEFORE and AFTER panes are both visible."
  (let ((bw (omnidiff-dual--session-before-win session))
        (aw (omnidiff-dual--session-after-win session))
        (bb (omnidiff-dual--session-before-buf session))
        (ab (omnidiff-dual--session-after-buf session)))
    (and (window-live-p bw)
         (window-live-p aw)
         (not (eq bw aw))
         (buffer-live-p bb)
         (buffer-live-p ab)
         (eq (window-buffer bw) bb)
         (eq (window-buffer aw) ab))))

(defun omnidiff-dual--apply-layout (session)
  "Arrange windows for SESSION according to its effective layout."
  (let* ((eff (omnidiff-dual--effective-layout session))
         (bb (omnidiff-dual--session-before-buf session))
         (ab (omnidiff-dual--session-after-buf session))
         (bw (omnidiff-dual--session-before-win session))
         (aw (omnidiff-dual--session-after-win session))
         (active (omnidiff-dual--session-active-side session))
         (anchor (cond
                  ((and (eq active 'before) (window-live-p bw)) bw)
                  ((and (eq active 'after) (window-live-p aw)) aw)
                  ((window-live-p bw) bw)
                  ((window-live-p aw) aw)
                  (t (selected-window)))))
    (if (eq eff 'dual)
        (if (and (window-live-p bw) (window-live-p aw) (not (eq bw aw)))
            (progn
              (set-window-buffer bw bb)
              (set-window-buffer aw ab))
          (select-window anchor)
          (ignore-errors (delete-other-windows anchor))
          (set-window-buffer anchor bb)
          (let ((right (ignore-errors (split-window-right))))
            (if (window-live-p right)
                (progn
                  (set-window-buffer right ab)
                  (setf (omnidiff-dual--session-before-win session) anchor
                        (omnidiff-dual--session-after-win session) right))
              ;; Frame too narrow to split: fall back gracefully to single pane.
              (set-window-buffer anchor (omnidiff-dual--side-buf session active))
              (setf (omnidiff-dual--session-before-win session)
                    (when (eq active 'before) anchor)
                    (omnidiff-dual--session-after-win session)
                    (when (eq active 'after) anchor)))))
      ;; Single-pane layout.
      (when (and (window-live-p bw) (window-live-p aw) (not (eq bw aw)))
        (let ((keep anchor)
              (drop (if (eq anchor bw) aw bw)))
          (when (window-live-p drop)
            (ignore-errors (delete-window drop)))
          (setq anchor keep)))
      (set-window-buffer anchor (omnidiff-dual--side-buf session active))
      (setf (omnidiff-dual--session-before-win session)
            (when (eq active 'before) anchor)
            (omnidiff-dual--session-after-win session)
            (when (eq active 'after) anchor)))
    (let ((target-win (or (omnidiff-dual--side-win session active) anchor)))
      (when (window-live-p target-win)
        (select-window target-win)))))

(defun omnidiff-dual--focus-side (session side)
  "Focus SIDE (`before' or `after') for SESSION, switching pane or buffer."
  (setf (omnidiff-dual--session-active-side session) side)
  (let ((buf (omnidiff-dual--side-buf session side)))
    (when (buffer-live-p buf)
      (if (eq (omnidiff-dual--effective-layout session) 'dual)
          (let ((win (omnidiff-dual--side-win session side)))
            (if (and (window-live-p win) (eq (window-buffer win) buf))
                (select-window win)
              (omnidiff-dual--apply-layout session)))
        (let ((bw (omnidiff-dual--session-before-win session))
              (aw (omnidiff-dual--session-after-win session)))
          (if (and (window-live-p bw) (window-live-p aw) (not (eq bw aw)))
              (omnidiff-dual--apply-layout session)
            (let ((win (or (omnidiff-dual--side-win session side)
                           (omnidiff-dual--side-win session (omnidiff-dual--other-side side))
                           (selected-window))))
              (when (window-live-p win)
                (set-window-buffer win buf)
                (setf (omnidiff-dual--session-before-win session)
                      (when (eq side 'before) win)
                      (omnidiff-dual--session-after-win session)
                      (when (eq side 'after) win))
                (select-window win)))))))))

(defun omnidiff-dual--setup-windows (session)
  "Initialize windows for a newly created SESSION."
  (setf (omnidiff-dual--session-saved-winconf session)
        (current-window-configuration))
  (cond
   ((eq omnidiff-dual-window-setup 'new-tab)
    (when (fboundp 'tab-bar-new-tab)
      (tab-bar-new-tab)
      (let* ((frame (selected-frame))
             (tab-id (omnidiff-dual--session-id session))
             (cur (when (fboundp 'tab-bar-tabs)
                    (assq 'current-tab (tab-bar-tabs frame)))))
        (when cur
          (setcdr cur (cons (cons 'omnidiff-dual-tab-id tab-id) (cdr cur))))
        (setf (omnidiff-dual--session-created-tab-p session) t
              (omnidiff-dual--session-created-tab-frame session) frame
              (omnidiff-dual--session-created-tab-id session) tab-id)))
    (omnidiff-dual--apply-layout session))
   ((functionp omnidiff-dual-window-setup)
    (let* ((bb (omnidiff-dual--session-before-buf session))
           (ab (omnidiff-dual--session-after-buf session))
           (res (funcall omnidiff-dual-window-setup bb ab session)))
      (if (and (consp res) (window-live-p (car res)) (window-live-p (cdr res)))
          (setf (omnidiff-dual--session-before-win session) (car res)
                (omnidiff-dual--session-after-win session) (cdr res))
        (setf (omnidiff-dual--session-before-win session) (get-buffer-window bb)
              (omnidiff-dual--session-after-win session) (get-buffer-window ab)))
      (when-let* ((w (omnidiff-dual--side-win
                      session (omnidiff-dual--session-active-side session))))
        (when (window-live-p w)
          (select-window w)))))
   (t
    (omnidiff-dual--apply-layout session))))

;;; Active focus, counterpart cross-highlight & synchronized scrolling

(defun omnidiff-dual--find-stop-at (session side row byte-col)
  "Return the `omnidiff-change-stop' matching (SIDE, ROW, BYTE-COL), or nil."
  (cl-loop for stop in (omnidiff-dual--session-stops session)
           when (or (and (eq (omnidiff-change-stop-side stop) side)
                         (omnidiff-dual--range-contains-p
                          (omnidiff-change-stop-range stop) row byte-col))
                    (and (eq (omnidiff-change-stop-counterpart-side stop) side)
                         (omnidiff-dual--range-contains-p
                          (omnidiff-change-stop-counterpart-range stop) row byte-col)))
           return stop))

(defun omnidiff-dual--sync-other-window (session other-side target-pos &optional center-p)
  "Scroll and position OTHER-SIDE window at TARGET-POS when dual panes show."
  (when (and omnidiff-dual-sync-scroll
             (not (minibuffer-window-active-p (selected-window)))
             (omnidiff-dual--dual-visible-p session))
    (let* ((active-win (omnidiff-dual--side-win
                        session (omnidiff-dual--other-side other-side)))
           (other-win (omnidiff-dual--side-win session other-side))
           (other-buf (omnidiff-dual--side-buf session other-side)))
      (when (and (window-live-p active-win)
                 (window-live-p other-win)
                 (buffer-live-p other-buf)
                 (not (eq active-win other-win)))
        (let ((screen-line
               (unless center-p
                 (ignore-errors
                   (with-current-buffer (window-buffer active-win)
                     (max 0 (1- (count-screen-lines
                                 (window-start active-win)
                                 (window-point active-win)
                                 nil
                                 active-win))))))))
          (with-current-buffer other-buf
            (omnidiff-dual--expand-fold-at target-pos))
          (save-selected-window
            (select-window other-win 'norecord)
            (goto-char target-pos)
            (ignore-errors
              (if center-p
                  (recenter)
                (let ((max-line (max 0 (1- (window-body-height other-win)))))
                  (recenter (min max-line (or screen-line (/ max-line 2)))))))))))))

(defun omnidiff-dual--sync-focus-and-counterpart (session &optional explicit-stop center-p skip-other-scroll)
  "Update active overlay, counterpart cross-highlight, and sync-scroll.
When EXPLICIT-STOP is an `omnidiff-change-stop', highlight its exact range
and counterpart; otherwise inspect the cursor position in the active buffer.
When SKIP-OTHER-SCROLL is non-nil, do not reposition the opposite window."
  (let* ((diff (omnidiff-dual--session-json-diff session))
         (alignment (omnidiff-dual--session-alignment session))
         (side (omnidiff-dual--session-active-side session))
         (other-side (omnidiff-dual--other-side side))
         (buf (omnidiff-dual--side-buf session side))
         (other-buf (omnidiff-dual--side-buf session other-side)))
    (when (and diff alignment (buffer-live-p buf) (buffer-live-p other-buf))
      (omnidiff-dual--clear-focus-overlays session)
      (let* ((starts (omnidiff-dual--side-line-starts session side))
             (other-starts (omnidiff-dual--side-line-starts session other-side))
             (tick (omnidiff-dual--side-lines-tick session side))
             (other-tick (omnidiff-dual--side-lines-tick session other-side))
             (pos (with-current-buffer buf (point)))
             (rc (omnidiff-dual--point-to-row-byte-col buf starts pos tick))
             (row (car rc))
             (bcol (cdr rc))
             (active-range nil)
             (cross-range nil)
             (scroll-row nil)
             (scroll-col 0))
        (if explicit-stop
            (progn
              (setq active-range (omnidiff-change-stop-range explicit-stop))
              (if (and (eq (omnidiff-change-stop-counterpart-side explicit-stop) other-side)
                       (omnidiff-dual--range-nonempty-p
                        (omnidiff-change-stop-counterpart-range explicit-stop)))
                  (setq cross-range (omnidiff-change-stop-counterpart-range explicit-stop))
                (let ((cp (omnidiff-engine-counterpart-at
                           diff alignment side
                           (omnidiff-json-range-start-row active-range)
                           (omnidiff-json-range-start-col active-range))))
                  (when (and cp (eq (car cp) other-side))
                    (if (omnidiff-dual--range-nonempty-p (cdr cp))
                        (setq cross-range (cdr cp))
                      (setq scroll-row (omnidiff-json-range-start-row (cdr cp))))))))
          (let* ((json-side (omnidiff-dual--side-json session side))
                 (hunks (and json-side (omnidiff-json-side-hunks json-side)))
                 (hunk (omnidiff-dual--hunk-at hunks row bcol))
                 (matched-stop (omnidiff-dual--find-stop-at session side row bcol)))
            (when matched-stop
              (setf (omnidiff-dual--session-stop-index session)
                    (omnidiff-change-stop-index matched-stop)))
            (if hunk
                (let* ((hrng (omnidiff-json-hunk-range hunk))
                       (q-row (if (omnidiff-dual--range-contains-p hrng row bcol)
                                  row
                                (omnidiff-json-range-start-row hrng)))
                       (q-col (if (omnidiff-dual--range-contains-p hrng row bcol)
                                  bcol
                                (omnidiff-json-range-start-col hrng)))
                       (cp (omnidiff-engine-counterpart-at
                            diff alignment side q-row q-col)))
                  (setq active-range hrng)
                  (when (and cp (eq (car cp) other-side))
                    (if (omnidiff-dual--range-nonempty-p (cdr cp))
                        (setq cross-range (cdr cp))
                      (setq scroll-row (omnidiff-json-range-start-row (cdr cp))))))
              (setq scroll-row (omnidiff-engine-map-row alignment side row)))))
        (when active-range
          (setf (omnidiff-dual--session-active-ovs session)
                (omnidiff-dual--make-range-overlays
                 buf starts active-range 'omnidiff-token-active 25 'active tick)))
        (when (and cross-range (omnidiff-dual--session-show-cross-highlight session))
          (setf (omnidiff-dual--session-cross-ovs session)
                (omnidiff-dual--make-range-overlays
                 other-buf other-starts cross-range
                 'omnidiff-token-cross-highlight 30 'cross other-tick)))
        (cond
         (cross-range
          (setq scroll-row (omnidiff-json-range-start-row cross-range)
                scroll-col (omnidiff-json-range-start-col cross-range)))
         ((null scroll-row)
          (setq scroll-row (omnidiff-engine-map-row alignment side row))))
        (unless skip-other-scroll
          (let ((target-pos (omnidiff-dual--row-byte-col-to-point
                             other-buf other-starts scroll-row scroll-col other-tick)))
            (omnidiff-dual--sync-other-window session other-side target-pos center-p)))
        (setf (omnidiff-dual--session-last-cursor-state session) (cons side pos))
        (omnidiff-dual--refresh-headers session)))))

(defun omnidiff-dual--post-command ()
  "Sync focus, cross-highlight, and counterpart scroll from `post-command-hook'."
  (unless omnidiff-dual--in-post-command
    (let ((omnidiff-dual--in-post-command t))
      (condition-case nil
          (when-let* ((session (omnidiff-dual-active-session))
                      (side omnidiff-dual--side))
            (when (and (eq (current-buffer) (window-buffer (selected-window)))
                       (not (omnidiff-dual--session-tearing-down-p session)))
              (let ((state (cons side (point))))
                (unless (equal state (omnidiff-dual--session-last-cursor-state session))
                  (setf (omnidiff-dual--session-active-side session) side)
                  (omnidiff-dual--sync-focus-and-counterpart session nil nil)))))
        (error nil)))))

;;; Buffer creation, syntax highlighting, and attachment

(defun omnidiff-dual--apply-safe-major-mode (buf path-hint show-syntax)
  "Enable native syntax highlighting in ephemeral BUF without running mode hooks.
Uses PATH-HINT against `auto-mode-alist' while guarding size against
`omnidiff-dual-syntax-max-bytes'."
  (with-current-buffer buf
    (when (and (stringp path-hint)
               (not (string-empty-p path-hint))
               (not (string= path-hint "/dev/null"))
               (<= (buffer-size) omnidiff-dual-syntax-max-bytes))
      (let ((buffer-file-name (expand-file-name path-hint))
            (enable-local-variables nil)
            (enable-dir-local-variables nil))
        (ignore-errors
          (delay-mode-hooks
            (set-auto-mode t)))
        (setq delayed-mode-hooks nil))
      ;; Never let viewer major modes (e.g., `image-mode' on `.svg') hide source text.
      (when (derived-mode-p 'image-mode 'archive-mode 'tar-mode 'doc-view-mode)
        (let ((enable-local-variables nil)
              (enable-dir-local-variables nil))
          (ignore-errors
            (delay-mode-hooks
              (if (string-match-p "\\.svgz?\\'" path-hint)
                  (xml-mode)
                (fundamental-mode))))
          (setq delayed-mode-hooks nil)))
      (when (and show-syntax (not (eq major-mode 'fundamental-mode)))
        (ignore-errors
          (font-lock-mode 1)
          (font-lock-ensure))))))

(defun omnidiff-dual--make-ephemeral-buffer (side display-name content path-hint show-syntax)
  "Create and populate an owned ephemeral buffer for SIDE."
  (let* ((tag (if (eq side 'before) "BEFORE" "AFTER"))
         (buf (generate-new-buffer (format "*omnidiff %s: %s*" tag display-name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (insert (or content "")))
      (goto-char (point-min))
      (set-buffer-modified-p nil)
      (setq buffer-undo-list t)
      (omnidiff-dual--apply-safe-major-mode buf path-hint show-syntax))
    buf))

(defun omnidiff-dual--writable-file-p (path)
  "Return non-nil when PATH is a regular readable and writable file on disk."
  (and (stringp path)
       (not (string-empty-p path))
       (not (string= path "/dev/null"))
       (file-regular-p path)
       (file-readable-p path)
       (file-writable-p path)))

(defun omnidiff-dual--attach-buffer (session side buf owns-p)
  "Attach BUF on SIDE to SESSION, recording original state for restoration."
  (with-current-buffer buf
    ;; If BUF is still attached to another session, tear down that session first
    ;; so BUF's true original state is restored before we record `orig-*'.
    (when (and (omnidiff-dual--session-p omnidiff-dual--session)
               (not (eq omnidiff-dual--session session)))
      (save-current-buffer
        (omnidiff-dual--teardown omnidiff-dual--session nil nil)))
    (let ((orig-ro buffer-read-only)
          (orig-ln (bound-and-true-p display-line-numbers-mode))
          (orig-hl-local (local-variable-p 'header-line-format))
          (orig-hl header-line-format)
          (orig-fl (bound-and-true-p font-lock-mode)))
      (if (eq side 'before)
          (setf (omnidiff-dual--session-before-buf session) buf
                (omnidiff-dual--session-before-owns-buf-p session) owns-p
                (omnidiff-dual--session-before-orig-read-only session) orig-ro
                (omnidiff-dual--session-before-orig-line-numbers session) orig-ln
                (omnidiff-dual--session-before-orig-header-line-local-p session) orig-hl-local
                (omnidiff-dual--session-before-orig-header-line session) orig-hl
                (omnidiff-dual--session-before-orig-font-lock session) orig-fl)
        (setf (omnidiff-dual--session-after-buf session) buf
              (omnidiff-dual--session-after-owns-buf-p session) owns-p
              (omnidiff-dual--session-after-orig-read-only session) orig-ro
              (omnidiff-dual--session-after-orig-line-numbers session) orig-ln
              (omnidiff-dual--session-after-orig-header-line-local-p session) orig-hl-local
              (omnidiff-dual--session-after-orig-header-line session) orig-hl
              (omnidiff-dual--session-after-orig-font-lock session) orig-fl)))
    (setq-local omnidiff-dual--session session)
    (setq-local omnidiff-dual--side side)
    (when (and (fboundp 'display-line-numbers-mode)
               (not (bound-and-true-p display-line-numbers-mode)))
      (display-line-numbers-mode 1))
    (setq-local header-line-format '(:eval (omnidiff-dual--header-line)))
    (let ((cookie (face-remap-add-relative 'header-line 'omnidiff-dual-header-meta)))
      (if (eq side 'before)
          (setf (omnidiff-dual--session-before-hl-remap-cookie session) cookie)
        (setf (omnidiff-dual--session-after-hl-remap-cookie session) cookie)))
    (setq buffer-read-only t)
    (omnidiff-dual-edit-mode -1)
    (omnidiff-dual-mode 1)
    (add-hook 'post-command-hook #'omnidiff-dual--post-command nil t)
    (add-hook 'kill-buffer-hook #'omnidiff-dual--on-kill-buffer nil t)
    (add-hook 'change-major-mode-hook #'omnidiff-dual--on-change-major-mode nil t)))

(defun omnidiff-dual--detach-buffer (session side &optional dying-buf)
  "Detach SIDE buffer from SESSION, killing owned buffers or restoring user ones.
If the buffer is `eq' to DYING-BUF, do not call `kill-buffer' on it."
  (let* ((before-p (eq side 'before))
         (buf (if before-p
                  (omnidiff-dual--session-before-buf session)
                (omnidiff-dual--session-after-buf session)))
         (owns-p (if before-p
                     (omnidiff-dual--session-before-owns-buf-p session)
                   (omnidiff-dual--session-after-owns-buf-p session)))
         (orig-ro (if before-p
                      (omnidiff-dual--session-before-orig-read-only session)
                    (omnidiff-dual--session-after-orig-read-only session)))
         (orig-ln (if before-p
                      (omnidiff-dual--session-before-orig-line-numbers session)
                    (omnidiff-dual--session-after-orig-line-numbers session)))
         (orig-hl-local (if before-p
                            (omnidiff-dual--session-before-orig-header-line-local-p session)
                          (omnidiff-dual--session-after-orig-header-line-local-p session)))
         (orig-hl (if before-p
                      (omnidiff-dual--session-before-orig-header-line session)
                    (omnidiff-dual--session-after-orig-header-line session)))
         (orig-fl (if before-p
                      (omnidiff-dual--session-before-orig-font-lock session)
                    (omnidiff-dual--session-after-orig-font-lock session)))
         (hl-cookie (if before-p
                        (omnidiff-dual--session-before-hl-remap-cookie session)
                      (omnidiff-dual--session-after-hl-remap-cookie session))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (remove-hook 'kill-buffer-hook #'omnidiff-dual--on-kill-buffer t)
        (remove-hook 'change-major-mode-hook #'omnidiff-dual--on-change-major-mode t)
        (remove-hook 'post-command-hook #'omnidiff-dual--post-command t)
        (remove-hook 'after-save-hook #'omnidiff-dual--on-after-save t)
        (remove-hook 'after-change-functions #'omnidiff-dual--on-after-change t)
        (omnidiff-dual--clear-overlays buf)
        (when (listp buffer-invisibility-spec)
          (remove-from-invisibility-spec 'omnidiff-dual-fold))
        (if (and owns-p (not (eq buf dying-buf)))
            (kill-buffer buf)
          (when (bound-and-true-p omnidiff-dual-mode)
            (omnidiff-dual-mode -1))
          (when (bound-and-true-p omnidiff-dual-edit-mode)
            (omnidiff-dual-edit-mode -1))
          (setq buffer-read-only orig-ro)
          (when (and (fboundp 'display-line-numbers-mode)
                     (not orig-ln)
                     (bound-and-true-p display-line-numbers-mode))
            (display-line-numbers-mode -1))
          (if orig-hl-local
              (setq-local header-line-format orig-hl)
            (kill-local-variable 'header-line-format))
          (when hl-cookie
            (ignore-errors (face-remap-remove-relative hl-cookie)))
          (when (and (boundp 'font-lock-mode)
                     (not (eq (and orig-fl t) (and font-lock-mode t))))
            (ignore-errors (font-lock-mode (if orig-fl 1 -1))))
          (setq-local omnidiff-dual--session nil)
          (setq-local omnidiff-dual--side nil))))))

(defun omnidiff-dual--ensure-tmpdir (session)
  "Return SESSION's temporary directory, creating it lazily if needed."
  (let ((dir (omnidiff-dual--session-tmpdir session)))
    (unless (and dir (file-directory-p dir))
      (setq dir (make-temp-file "omnidiff-dual-" t))
      (setf (omnidiff-dual--session-tmpdir session) dir))
    dir))

(defun omnidiff-dual--delete-temp-files (paths)
  "Silently remove temporary snapshot files in PATHS and empty parent subdirs."
  (dolist (p paths)
    (when (and (stringp p) (file-exists-p p))
      (ignore-errors (delete-file p))
      (let ((parent (file-name-directory p)))
        (when (and parent (file-directory-p parent))
          (ignore-errors (delete-directory parent nil)))))))

(defun omnidiff-dual--write-string-temp (content tmpdir name-hint)
  "Write string CONTENT into a unique temp file under TMPDIR preserving NAME-HINT."
  (with-temp-buffer
    (insert (or content ""))
    (omnidiff-engine-write-temp-for-buffer (current-buffer) tmpdir name-hint)))

;;; Diff application & asynchronous recompute

(defun omnidiff-dual--initial-fold-p (session)
  "Compute initial folding boolean for SESSION from `omnidiff-dual-fold-unchanged'."
  (pcase omnidiff-dual-fold-unchanged
    ('nil nil)
    ('auto
     (let ((nb (omnidiff-dual--logical-line-count
                (omnidiff-dual--session-before-buf session)
                (omnidiff-dual--session-before-line-starts session)))
           (na (omnidiff-dual--logical-line-count
                (omnidiff-dual--session-after-buf session)
                (omnidiff-dual--session-after-line-starts session))))
       (> (max nb na) omnidiff-dual-auto-fold-lines)))
    (_ t)))

(defun omnidiff-dual--file-has-bom-p (path)
  "Return non-nil when regular file PATH starts with a UTF-8/UTF-16/UTF-32 BOM."
  (when (and (stringp path)
             (not (string-empty-p path))
             (not (string= path "/dev/null"))
             (file-regular-p path)
             (file-readable-p path))
    (ignore-errors
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally path nil 0 4)
        (let ((len (buffer-size)))
          (or (and (>= len 3)
                   (= (char-after 1) #xef)
                   (= (char-after 2) #xbb)
                   (= (char-after 3) #xbf))
              (and (>= len 2)
                   (or (and (= (char-after 1) #xff) (= (char-after 2) #xfe))
                       (and (= (char-after 1) #xfe) (= (char-after 2) #xff))))
              (and (>= len 4)
                   (= (char-after 1) #x00) (= (char-after 2) #x00)
                   (= (char-after 3) #xfe) (= (char-after 4) #xff))))))))

(defun omnidiff-dual--buffer-bom-stripped-p (buf)
  "Return non-nil when BUF is a visited buffer whose coding system stripped a BOM."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((cs buffer-file-coding-system))
        (and (symbolp cs)
             (coding-system-p cs)
             (or (coding-system-get cs :bom)
                 (string-match-p "with-signature" (symbol-name cs)))
             (not (eql (save-restriction (widen) (char-after (point-min)))
                       #xfeff)))))))

(defun omnidiff-dual--shift-row0-range (range delta)
  "Return RANGE with row-0 byte columns shifted by DELTA (clamped >= 0)."
  (when range
    (let ((sr (omnidiff-json-range-start-row range))
          (sc (omnidiff-json-range-start-col range))
          (er (omnidiff-json-range-end-row range))
          (ec (omnidiff-json-range-end-col range)))
      (if (and (/= sr 0) (/= er 0))
          range
        (make-omnidiff-json-range
         :start-row sr
         :start-col (if (= sr 0) (max 0 (+ sc delta)) sc)
         :end-row er
         :end-col (if (= er 0) (max 0 (+ ec delta)) ec))))))

(defun omnidiff-dual--adjust-side-bom (side-json own-delta other-delta)
  "Return a copy of SIDE-JSON with row-0 byte columns shifted by OWN-DELTA.
`move-target' ranges are shifted by OTHER-DELTA."
  (when side-json
    (if (and (= own-delta 0) (= other-delta 0))
        side-json
      (let ((new-hunks nil))
        (dolist (h (omnidiff-json-side-hunks side-json))
          (let* ((rng (omnidiff-json-hunk-range h))
                 (new-rng (if (/= own-delta 0)
                              (omnidiff-dual--shift-row0-range rng own-delta)
                            rng)))
            (when (omnidiff-dual--range-nonempty-p new-rng)
              (let* ((mt (omnidiff-json-hunk-move-target h))
                     (new-mt (when mt
                               (let ((shifted (if (/= other-delta 0)
                                                  (omnidiff-dual--shift-row0-range mt other-delta)
                                                mt)))
                                 (when (omnidiff-dual--range-nonempty-p shifted)
                                   shifted)))))
                (push (make-omnidiff-json-hunk
                       :operation (omnidiff-json-hunk-operation h)
                       :range new-rng
                       :move-target new-mt
                       :reference-line (omnidiff-json-hunk-reference-line h))
                      new-hunks)))))
        (make-omnidiff-json-side
         :path (omnidiff-json-side-path side-json)
         :language (omnidiff-json-side-language side-json)
         :encoding (omnidiff-json-side-encoding side-json)
         :hunks (nreverse new-hunks))))))

(defun omnidiff-dual--side-json-has-bom-p (side-json fallback-path)
  "Return non-nil when SIDE-JSON was computed from a BOM-prefixed disk file."
  (when side-json
    (let ((jpath (omnidiff-json-side-path side-json)))
      (or (omnidiff-dual--file-has-bom-p jpath)
          (and (stringp jpath)
               (not (string-empty-p jpath))
               (not (file-name-absolute-p jpath))
               (stringp fallback-path)
               (equal (file-name-nondirectory jpath)
                      (file-name-nondirectory fallback-path))
               (omnidiff-dual--file-has-bom-p fallback-path))))))

(defun omnidiff-dual--normalize-bom-diff (session json-diff before-buf after-buf)
  "Adjust row-0 byte columns in JSON-DIFF for BOM-stripped visited buffers.
Non-destructively shifts row-0 columns by -3 on any side whose Emacs buffer
stripped a leading U+FEFF while `omnidiff' read the BOM-prefixed file from disk."
  (if (null json-diff)
      nil
    (let* ((b-side (omnidiff-json-diff-before json-diff))
           (a-side (omnidiff-json-diff-after json-diff))
           (b-shift (if (and (omnidiff-dual--buffer-bom-stripped-p before-buf)
                             (omnidiff-dual--side-json-has-bom-p
                              b-side (omnidiff-dual--session-before-path session)))
                        -3
                      0))
           (a-shift (if (and (omnidiff-dual--buffer-bom-stripped-p after-buf)
                             (omnidiff-dual--side-json-has-bom-p
                              a-side (omnidiff-dual--session-after-path session)))
                        -3
                      0)))
      (if (and (= b-shift 0) (= a-shift 0))
          json-diff
        (make-omnidiff-json-diff
         :before (omnidiff-dual--adjust-side-bom b-side b-shift a-shift)
         :after (omnidiff-dual--adjust-side-bom a-side a-shift b-shift)
         :large-residual (omnidiff-json-diff-large-residual json-diff)
         :summary (omnidiff-json-diff-summary json-diff)
         :binary-p (omnidiff-json-diff-binary-p json-diff)
         :picture (omnidiff-json-diff-picture json-diff)
         :container (omnidiff-json-diff-container json-diff))))))

(defun omnidiff-dual--goto-stop (session stop &optional center-p)
  "Focus and move point to `omnidiff-change-stop' STOP in SESSION."
  (let* ((side (omnidiff-change-stop-side stop))
         (range (omnidiff-change-stop-range stop))
         (buf (omnidiff-dual--side-buf session side))
         (starts (omnidiff-dual--side-line-starts session side))
         (tick (omnidiff-dual--side-lines-tick session side)))
    (when (buffer-live-p buf)
      (setf (omnidiff-dual--session-stop-index session)
            (omnidiff-change-stop-index stop))
      (omnidiff-dual--focus-side session side)
      (let ((pos (omnidiff-dual--row-byte-col-to-point
                  buf starts
                  (omnidiff-json-range-start-row range)
                  (omnidiff-json-range-start-col range)
                  tick)))
        (with-current-buffer buf
          (omnidiff-dual--expand-fold-at pos)
          (goto-char pos)
          (when center-p
            (ignore-errors (recenter)))))
      (omnidiff-dual--sync-focus-and-counterpart session stop center-p))))

(defun omnidiff-dual--apply-diff (session json-diff &optional preserve-cursor)
  "Install JSON-DIFF on SESSION, computing alignment, stops, and overlays.
When PRESERVE-CURSOR is non-nil, keep cursor positions in place instead of
jumping to the first change stop."
  (let* ((bb (omnidiff-dual--session-before-buf session))
         (ab (omnidiff-dual--session-after-buf session))
         (json-diff (omnidiff-dual--normalize-bom-diff session json-diff bb ab))
         (b-tick (buffer-chars-modified-tick bb))
         (a-tick (buffer-chars-modified-tick ab))
         (b-starts (omnidiff-engine-buffer-line-offsets bb))
         (a-starts (omnidiff-engine-buffer-line-offsets ab))
         (b-lines (omnidiff-dual--buffer-lines bb b-starts))
         (a-lines (omnidiff-dual--buffer-lines ab a-starts))
         (alignment (omnidiff-engine-compute-alignment b-lines a-lines json-diff))
         (stops (omnidiff-engine-build-change-stops json-diff alignment)))
    (setf (omnidiff-dual--session-json-diff session) json-diff
          (omnidiff-dual--session-before-line-starts session) b-starts
          (omnidiff-dual--session-after-line-starts session) a-starts
          (omnidiff-dual--session-before-lines-tick session) b-tick
          (omnidiff-dual--session-after-lines-tick session) a-tick
          (omnidiff-dual--session-before-lines session) b-lines
          (omnidiff-dual--session-after-lines session) a-lines
          (omnidiff-dual--session-alignment session) alignment
          (omnidiff-dual--session-stops session) stops
          (omnidiff-dual--session-minimap-cache session) nil
          (omnidiff-dual--session-last-cursor-state session) nil)
    (unless preserve-cursor
      (setf (omnidiff-dual--session-fold-unchanged session)
            (omnidiff-dual--initial-fold-p session)
            (omnidiff-dual--session-stop-index session) (if stops 1 0)
            (omnidiff-dual--session-last-jump session) nil))
    (omnidiff-dual--apply-side-overlays session 'before)
    (omnidiff-dual--apply-side-overlays session 'after)
    (unless (and preserve-cursor (omnidiff-dual--session-editing-p session))
      (omnidiff-dual--apply-folds session)
      (when preserve-cursor
        (let ((active-buf (omnidiff-dual--side-buf
                           session (omnidiff-dual--session-active-side session))))
          (when (buffer-live-p active-buf)
            (with-current-buffer active-buf
              (omnidiff-dual--expand-fold-at (point)))))))
    (cond
     (preserve-cursor
      (omnidiff-dual--sync-focus-and-counterpart session nil nil))
     (stops
      (omnidiff-dual--goto-stop session (car stops) t))
     (t
      (omnidiff-dual--clear-focus-overlays session)
      (omnidiff-dual--focus-side session 'after)
      (with-current-buffer ab (goto-char (point-min)))
      (with-current-buffer bb (goto-char (point-min)))
      (omnidiff-dual--refresh-headers session)))
    (run-hooks 'omnidiff-dual-after-diff-hook)))

(defun omnidiff-dual--snapshot-paths (session)
  "Return `(BEFORE-FILE AFTER-FILE TEMP-FILES)' for invoking `omnidiff'."
  (let* ((bb (omnidiff-dual--session-before-buf session))
         (ab (omnidiff-dual--session-after-buf session))
         (bp (omnidiff-dual--session-before-path session))
         (ap (omnidiff-dual--session-after-path session))
         (hint (or (omnidiff-dual--session-display-path session)
                   ap bp "file"))
         (temps nil)
         (b-file (if (and (stringp bp)
                          (or (string= bp "/dev/null") (file-exists-p bp))
                          (not (buffer-modified-p bb)))
                     bp
                   (let ((f (omnidiff-engine-write-temp-for-buffer
                             bb (omnidiff-dual--ensure-tmpdir session) hint)))
                     (push f temps)
                     f)))
         (a-file (if (and (stringp ap)
                          (or (string= ap "/dev/null") (file-exists-p ap))
                          (not (buffer-modified-p ab)))
                     ap
                   (let ((f (omnidiff-engine-write-temp-for-buffer
                             ab (omnidiff-dual--ensure-tmpdir session) hint)))
                     (push f temps)
                     f))))
    (list b-file a-file temps)))

(defun omnidiff-dual--refresh-async (session)
  "Asynchronously re-run `omnidiff --mode json' for SESSION preserving cursor."
  (when (and (memq session omnidiff-dual--sessions)
             (buffer-live-p (omnidiff-dual--session-before-buf session))
             (buffer-live-p (omnidiff-dual--session-after-buf session)))
    ;; Bump request-seq before killing old-proc so its sentinel drops silently.
    (let* ((seq (cl-incf (omnidiff-dual--session-request-seq session)))
           (old-proc (omnidiff-dual--session-active-proc session)))
      (when (and old-proc (process-live-p old-proc))
        (delete-process old-proc))
      (let* ((snap (omnidiff-dual--snapshot-paths session))
           (b-file (nth 0 snap))
           (a-file (nth 1 snap))
           (temps (nth 2 snap))
           (proc
            (omnidiff-engine-run-pair-async
             b-file a-file
             (lambda (new-diff)
               (omnidiff-dual--delete-temp-files temps)
               (when (and (memq session omnidiff-dual--sessions)
                          (= seq (omnidiff-dual--session-request-seq session))
                          (buffer-live-p (omnidiff-dual--session-before-buf session))
                          (buffer-live-p (omnidiff-dual--session-after-buf session)))
                 (setf (omnidiff-dual--session-active-proc session) nil)
                 (omnidiff-dual--apply-diff session new-diff t)))
             :preset (omnidiff-dual--session-preset session)
             :whole-updates (omnidiff-dual--session-whole-updates session)
             :paint-reindent-moves (omnidiff-dual--session-paint-reindent-moves session)
             :error-callback
             (lambda (err-msg)
               (omnidiff-dual--delete-temp-files temps)
               (when (and (memq session omnidiff-dual--sessions)
                          (= seq (omnidiff-dual--session-request-seq session)))
                 (setf (omnidiff-dual--session-active-proc session) nil)
                 (message "OmniDiff refresh failed: %s" err-msg))))))
      (setf (omnidiff-dual--session-active-proc session) proc)
      proc))))

;;; Live edit hooks

(defun omnidiff-dual--live-timer-fire (session)
  "Timer callback triggering idle live recompute for SESSION."
  (when (omnidiff-dual--session-p session)
    (setf (omnidiff-dual--session-live-timer session) nil)
    (when (and (memq session omnidiff-dual--sessions)
               (omnidiff-dual--session-editing-p session)
               (buffer-live-p (omnidiff-dual--session-before-buf session))
               (buffer-live-p (omnidiff-dual--session-after-buf session)))
      (omnidiff-dual--refresh-async session))))

(defun omnidiff-dual--on-after-save ()
  "Recompute semantic overlays after saving a live-edited AFTER buffer."
  (when-let* ((session (omnidiff-dual-active-session)))
    (when (eq omnidiff-dual--side 'after)
      (setf (omnidiff-dual--session-mutated-p session) t)
      (when (memq omnidiff-dual-live-update '(on-save idle))
        (when-let* ((timer (omnidiff-dual--session-live-timer session)))
          (cancel-timer timer)
          (setf (omnidiff-dual--session-live-timer session) nil))
        (omnidiff-dual--refresh-async session)))))

(defun omnidiff-dual--on-after-change (_beg _end _len)
  "Schedule debounced idle diff recompute after editing the AFTER buffer."
  (when-let* ((session (omnidiff-dual-active-session)))
    (when (and (eq omnidiff-dual--side 'after)
               (eq omnidiff-dual-live-update 'idle)
               (omnidiff-dual--session-editing-p session))
      (when-let* ((timer (omnidiff-dual--session-live-timer session)))
        (cancel-timer timer))
      (setf (omnidiff-dual--session-live-timer session)
            (run-with-idle-timer (max 0.05 omnidiff-dual-live-delay)
                                 nil
                                 #'omnidiff-dual--live-timer-fire
                                 session)))))

;;; Session teardown

(defun omnidiff-dual--teardown (session &optional dying-buf restore-windows)
  "Cleanly release all resources owned by SESSION.
DYING-BUF, if non-nil, is already being killed by `kill-buffer'.
When RESTORE-WINDOWS is non-nil, restore saved window configuration or tab."
  (unless (omnidiff-dual--session-tearing-down-p session)
    (setf (omnidiff-dual--session-tearing-down-p session) t)
    (setq omnidiff-dual--sessions (delq session omnidiff-dual--sessions))
    (when-let* ((timer (omnidiff-dual--session-live-timer session)))
      (when (timerp timer) (cancel-timer timer))
      (setf (omnidiff-dual--session-live-timer session) nil))
    (when-let* ((proc (omnidiff-dual--session-active-proc session)))
      (when (process-live-p proc) (delete-process proc))
      (setf (omnidiff-dual--session-active-proc session) nil))
    (omnidiff-dual--clear-focus-overlays session)
    (let ((saved-winconf (omnidiff-dual--session-saved-winconf session))
          (created-tab (omnidiff-dual--session-created-tab-p session))
          (created-tab-frame (omnidiff-dual--session-created-tab-frame session))
          (created-tab-id (omnidiff-dual--session-created-tab-id session))
          (tmpdir (omnidiff-dual--session-tmpdir session))
          (on-quit (plist-get (omnidiff-dual--session-nav-callbacks session) :on-quit)))
      (omnidiff-dual--detach-buffer session 'before dying-buf)
      (omnidiff-dual--detach-buffer session 'after dying-buf)
      (setf (omnidiff-dual--session-tmpdir session) nil)
      (when (and tmpdir (file-directory-p tmpdir))
        (ignore-errors (delete-directory tmpdir t)))
      (when restore-windows
        (cond
         ((and created-tab
               (fboundp 'tab-bar-close-tab)
               (fboundp 'tab-bar-tabs))
          (let ((frame (if (frame-live-p created-tab-frame)
                           created-tab-frame
                         (selected-frame))))
            (when (frame-live-p frame)
              (ignore-errors
                (let* ((tabs (tab-bar-tabs frame))
                       (idx (and created-tab-id
                                 (cl-position-if
                                  (lambda (tab)
                                    (eql (alist-get 'omnidiff-dual-tab-id (cdr tab))
                                         created-tab-id))
                                  tabs))))
                  (when (and idx (> (length tabs) 1))
                    (with-selected-frame frame
                      (tab-bar-close-tab (1+ idx)))))))))
         ((window-configuration-p saved-winconf)
          (ignore-errors (set-window-configuration saved-winconf)))))
      (when (functionp on-quit)
        (ignore-errors
          (let ((arity (func-arity on-quit)))
            (if (and (consp arity) (eql (cdr arity) 0))
                (funcall on-quit)
              (funcall on-quit session))))))))

(defun omnidiff-dual--on-kill-buffer ()
  "Clean up an active dual session if one of its buffers is killed directly."
  (when-let* ((session (omnidiff-dual-active-session)))
    (unless (omnidiff-dual--session-tearing-down-p session)
      (omnidiff-dual--teardown session (current-buffer) nil))))

(defun omnidiff-dual--on-change-major-mode ()
  "Clean up an active dual session before a major-mode switch or buffer revert."
  (when-let* ((session (omnidiff-dual-active-session)))
    (unless (omnidiff-dual--session-tearing-down-p session)
      (omnidiff-dual--teardown session nil nil))))

;;; Navigation & interactive commands

(defun omnidiff-dual--stop-sort-key (alignment stop)
  "Return reading-order sort key `(AFTER-ROW BEFORE-ROW COL)' for STOP."
  (let* ((side (omnidiff-change-stop-side stop))
         (rng (omnidiff-change-stop-range stop))
         (sr (omnidiff-json-range-start-row rng))
         (sc (omnidiff-json-range-start-col rng)))
    (if (eq side 'after)
        (list sr (omnidiff-engine-map-row alignment 'after sr) sc)
      (list (omnidiff-engine-map-row alignment 'before sr) sr sc))))

(defun omnidiff-dual--key-less-p (a b)
  "Return non-nil if 3-element integer tuple A precedes B lexicographically."
  (cond
   ((/= (nth 0 a) (nth 0 b)) (< (nth 0 a) (nth 0 b)))
   ((/= (nth 1 a) (nth 1 b)) (< (nth 1 a) (nth 1 b)))
   (t (< (nth 2 a) (nth 2 b)))))

(defun omnidiff-dual--step-change (session forward-p)
  "Move SESSION to the next (FORWARD-P) or previous change stop."
  (let ((stops (omnidiff-dual--session-stops session)))
    (if (null stops)
        (progn
          (message "No changes in this diff")
          nil)
      (let* ((len (length stops))
             (side (or omnidiff-dual--side (omnidiff-dual--session-active-side session)))
             (buf (omnidiff-dual--side-buf session side))
             (starts (omnidiff-dual--side-line-starts session side))
             (tick (omnidiff-dual--side-lines-tick session side))
             (rc (omnidiff-dual--point-to-row-byte-col
                  buf starts (with-current-buffer buf (point)) tick))
             (cur-stop (omnidiff-dual--find-stop-at session side (car rc) (cdr rc)))
             (wrapped nil)
             (target-idx
              (if cur-stop
                  (let ((idx (omnidiff-change-stop-index cur-stop)))
                    (if forward-p
                        (if (>= idx len) (progn (setq wrapped 'first) 1) (1+ idx))
                      (if (<= idx 1) (progn (setq wrapped 'last) len) (1- idx))))
                (let* ((alignment (omnidiff-dual--session-alignment session))
                       (row (car rc))
                       (col (cdr rc))
                       (cur-key (if (eq side 'after)
                                    (list row (omnidiff-engine-map-row alignment 'after row) col)
                                  (list (omnidiff-engine-map-row alignment 'before row) row col)))
                       (after-pos
                        (cl-position-if
                         (lambda (st)
                           (omnidiff-dual--key-less-p
                            cur-key (omnidiff-dual--stop-sort-key alignment st)))
                         stops)))
                  (if forward-p
                      (if after-pos (1+ after-pos) (setq wrapped 'first) 1)
                    (cond
                     ((null after-pos) len)
                     ((= after-pos 0) (setq wrapped 'last) len)
                     (t after-pos))))))
             (target (nth (1- target-idx) stops))
             (nav (omnidiff-dual--session-nav-callbacks session))
             (multi-file-p (and (integerp (plist-get nav :file-total))
                                (> (plist-get nav :file-total) 1))))
        (setf (omnidiff-dual--session-last-jump session) nil)
        (omnidiff-dual--goto-stop session target t)
        (when wrapped
          (message "Wrapped to %s change (%d/%d)%s"
                   (if (eq wrapped 'first) "first" "last")
                   target-idx len
                   (if multi-file-p
                       (if (eq wrapped 'first)
                           " — ] for next file"
                         " — [ for previous file")
                     "")))
        target))))

(defun omnidiff-dual-next-change ()
  "Jump to the next semantic change stop across both panes."
  (interactive)
  (omnidiff-dual--step-change (omnidiff-dual--require-session) t))

(defun omnidiff-dual-prev-change ()
  "Jump to the previous semantic change stop across both panes."
  (interactive)
  (omnidiff-dual--step-change (omnidiff-dual--require-session) nil))

(defun omnidiff-dual-jump-counterpart ()
  "Expand fold banner at point, or jump to counterpart in the opposite pane.
Pressing again immediately returns to the originating position."
  (interactive)
  (let ((session (omnidiff-dual--require-session)))
    (if (omnidiff-dual--expand-fold-at (point))
        (progn
          (message "Expanded fold")
          'expanded-fold)
      (let* ((side (or omnidiff-dual--side (omnidiff-dual--session-active-side session)))
             (other-side (omnidiff-dual--other-side side))
             (buf (omnidiff-dual--side-buf session side))
             (other-buf (omnidiff-dual--side-buf session other-side))
             (starts (omnidiff-dual--side-line-starts session side))
             (other-starts (omnidiff-dual--side-line-starts session other-side))
             (tick (omnidiff-dual--side-lines-tick session side))
             (other-tick (omnidiff-dual--side-lines-tick session other-side))
             (diff (omnidiff-dual--session-json-diff session))
             (alignment (omnidiff-dual--session-alignment session))
             (orig-pos (point))
             (rc (omnidiff-dual--point-to-row-byte-col buf starts orig-pos tick))
             (row (car rc))
             (bcol (cdr rc))
             (json-side (omnidiff-dual--side-json session side))
             (hunks (and json-side (omnidiff-json-side-hunks json-side)))
             (hunk (omnidiff-dual--hunk-at hunks row bcol))
             (last-jump (omnidiff-dual--session-last-jump session))
             (dest-side other-side)
             (dest-pos nil)
             (record-jump t))
        (cond
         ;; Pressing RET again without moving returns to originating position.
         ((and last-jump
               (eq side (nth 2 last-jump))
               (= orig-pos (nth 3 last-jump)))
          (setq dest-side (nth 0 last-jump)
                dest-pos (nth 1 last-jump)
                record-jump nil))
         ;; Hunk at point: use exact move_target or paired counterpart.
         (hunk
          (let* ((hrng (omnidiff-json-hunk-range hunk))
                 (q-row (if (omnidiff-dual--range-contains-p hrng row bcol)
                            row
                          (omnidiff-json-range-start-row hrng)))
                 (q-col (if (omnidiff-dual--range-contains-p hrng row bcol)
                            bcol
                          (omnidiff-json-range-start-col hrng)))
                 (cp (omnidiff-engine-counterpart-at diff alignment side q-row q-col))
                 (cp-range (cdr cp)))
            (setq dest-side (car cp)
                  dest-pos (omnidiff-dual--row-byte-col-to-point
                            other-buf other-starts
                            (omnidiff-json-range-start-row cp-range)
                            (omnidiff-json-range-start-col cp-range)
                            other-tick))))
         ;; Unchanged line: map row directly via alignment.
         (t
          (let ((mapped-row (omnidiff-engine-map-row alignment side row)))
            (setq dest-side other-side
                  dest-pos (omnidiff-dual--row-byte-col-to-point
                            other-buf other-starts mapped-row 0 other-tick)))))
        (when (and dest-pos (buffer-live-p (omnidiff-dual--side-buf session dest-side)))
          (setf (omnidiff-dual--session-last-jump session)
                (when record-jump (list side orig-pos dest-side dest-pos)))
          (omnidiff-dual--focus-side session dest-side)
          (with-current-buffer (omnidiff-dual--side-buf session dest-side)
            (omnidiff-dual--expand-fold-at dest-pos)
            (goto-char dest-pos)
            (ignore-errors (recenter)))
          (omnidiff-dual--sync-focus-and-counterpart session nil t t)
          (cons dest-side dest-pos))))))

(defun omnidiff-dual-switch-pane ()
  "Switch focus to the opposite pane (or flip displayed side in single layout)."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (cur (or omnidiff-dual--side (omnidiff-dual--session-active-side session)))
         (next (omnidiff-dual--other-side cur)))
    (omnidiff-dual--focus-side session next)
    (omnidiff-dual--sync-focus-and-counterpart session nil nil)
    next))

(defun omnidiff-dual-next-file ()
  "Step to the next file via the session's `:next-file' callback."
  (interactive)
  (omnidiff-dual--invoke-callback
   (omnidiff-dual--require-session) :next-file "No next file callback configured"))

(defun omnidiff-dual-prev-file ()
  "Step to the previous file via the session's `:prev-file' callback."
  (interactive)
  (omnidiff-dual--invoke-callback
   (omnidiff-dual--require-session) :prev-file "No previous file callback configured"))

(defun omnidiff-dual-open-overview ()
  "Open multi-file overview via the session's `:open-overview' callback."
  (interactive)
  (omnidiff-dual--invoke-callback
   (omnidiff-dual--require-session) :open-overview "No overview callback configured"))

(defun omnidiff-dual-toggle-reviewed ()
  "Toggle reviewed status for the current file in the linked Overview buffer.
When marked reviewed, automatically advances to the next unreviewed file if
one exists."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (nav (omnidiff-dual--session-nav-callbacks session))
         (toggle-fn (plist-get nav :toggle-reviewed)))
    (unless (functionp toggle-fn)
      (user-error "Review tracking requires a linked multi-file Overview session"))
    (let* ((disp (or (omnidiff-dual--session-display-path session) "file"))
           (new-state (omnidiff-dual--invoke-callback session :toggle-reviewed))
           (prog-fn (plist-get nav :review-progress))
           (prog (when (functionp prog-fn) (funcall prog-fn)))
           (rev (or (car-safe prog) 0))
           (tot (or (cdr-safe prog) 0)))
      (if new-state
          (let ((next-fn (plist-get nav :next-unreviewed))
                (moved nil))
            (when (functionp next-fn)
              (setq moved (omnidiff-dual--invoke-callback session :next-unreviewed)))
            (omnidiff-dual--refresh-headers session)
            (if moved
                (message "Reviewed %s (%d/%d) — next: %s"
                         disp rev tot
                         (or (omnidiff-dual--session-display-path session) ""))
              (message "All files reviewed (%d/%d)" rev tot)))
        (omnidiff-dual--refresh-headers session)
        (message "Unmarked %s (%d/%d reviewed)" disp rev tot))
      new-state)))

(defun omnidiff-dual-open-tui ()
  "Open external omnidiff TUI via the session's `:open-tui' callback."
  (interactive)
  (omnidiff-dual--invoke-callback
   (omnidiff-dual--require-session) :open-tui "No TUI callback configured"))

(defun omnidiff-dual-cycle-layout (&optional reset-auto-p)
  "Toggle pane layout between `dual' and `single' with immediate visual change.
With prefix argument RESET-AUTO-P, restore width-responsive `auto' layout."
  (interactive "P")
  (let* ((session (omnidiff-dual--require-session))
         (eff (omnidiff-dual--effective-layout session))
         (next (cond
                (reset-auto-p 'auto)
                ((eq eff 'dual) 'single)
                (t 'dual))))
    (setf (omnidiff-dual--session-layout session) next)
    (omnidiff-dual--apply-layout session)
    (omnidiff-dual--sync-focus-and-counterpart session nil nil)
    (message "OmniDiff layout: %s%s"
             next
             (if (eq next 'auto)
                 (format " (%s)" (omnidiff-dual--effective-layout session))
               ""))
    next))

(defun omnidiff-dual-toggle-fold ()
  "Toggle folding of unchanged line runs in both panes."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (next (not (omnidiff-dual--session-fold-unchanged session))))
    (setf (omnidiff-dual--session-fold-unchanged session) next)
    (omnidiff-dual--apply-folds session)
    (message "OmniDiff unchanged folding: %s" (if next "on" "off"))
    next))

(defun omnidiff-dual-cycle-preset ()
  "Cycle verbosity preset (`minimal' -> `full' -> `default') and recompute."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (next (pcase (omnidiff-dual--session-preset session)
                 ('minimal 'full)
                 ('full 'default)
                 (_ 'minimal))))
    (setf (omnidiff-dual--session-preset session) next)
    (omnidiff-dual--refresh-headers session)
    (omnidiff-dual--refresh-async session)
    (message "OmniDiff preset: %s" next)
    next))

(defun omnidiff-dual-toggle-whole-updates ()
  "Toggle `--whole-updates' for the current session and recompute overlays."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (next (not (omnidiff-dual--session-whole-updates session))))
    (setf (omnidiff-dual--session-whole-updates session) next)
    (omnidiff-dual--refresh-headers session)
    (omnidiff-dual--refresh-async session)
    (message "OmniDiff --whole-updates: %s" (if next "on" "off"))
    next))

(defun omnidiff-dual-toggle-paint-reindent-moves ()
  "Toggle `--paint-reindent-moves' for the current session and recompute."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (next (not (omnidiff-dual--session-paint-reindent-moves session))))
    (setf (omnidiff-dual--session-paint-reindent-moves session) next)
    (omnidiff-dual--refresh-headers session)
    (omnidiff-dual--refresh-async session)
    (message "OmniDiff --paint-reindent-moves: %s" (if next "on" "off"))
    next))

(defun omnidiff-dual-cycle-color-scheme ()
  "Cycle `omnidiff-color-scheme' and re-apply the semantic palette."
  (interactive)
  (omnidiff-dual--require-session)
  (let ((next (pcase omnidiff-color-scheme
                ('omni-semantic 'calm-modus)
                ('calm-modus 'neutral-paper)
                (_ 'omni-semantic))))
    (setq omnidiff-color-scheme next)
    (omnidiff-apply-theme)
    (message "OmniDiff color scheme: %s" next)
    next))

(defun omnidiff-dual-toggle-syntax ()
  "Toggle native `font-lock-mode' syntax highlighting on both panes."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (next (not (omnidiff-dual--session-show-syntax session))))
    (setf (omnidiff-dual--session-show-syntax session) next)
    (dolist (side '(before after))
      (when-let* ((buf (omnidiff-dual--side-buf session side)))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (if next
                (when (<= (buffer-size) omnidiff-dual-syntax-max-bytes)
                  (ignore-errors
                    (font-lock-mode 1)
                    (font-lock-ensure)))
              (ignore-errors
                (font-lock-mode -1)
                (when (fboundp 'font-lock-unfontify-buffer)
                  (font-lock-unfontify-buffer))))))))
    (message "OmniDiff syntax highlighting: %s" (if next "on" "off"))
    next))

(defun omnidiff-dual-toggle-counterpart-highlight ()
  "Toggle counterpart cross-highlight overlays in the opposite pane."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (next (not (omnidiff-dual--session-show-cross-highlight session))))
    (setf (omnidiff-dual--session-show-cross-highlight session) next)
    (omnidiff-dual--sync-focus-and-counterpart session nil nil)
    (message "OmniDiff counterpart highlight: %s" (if next "on" "off"))
    next))

(defun omnidiff-dual-toggle-minimap ()
  "Toggle the header-line change-distribution minimap."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (next (not (omnidiff-dual--session-show-minimap session))))
    (setf (omnidiff-dual--session-show-minimap session) next)
    (omnidiff-dual--refresh-headers session)
    (message "OmniDiff minimap: %s" (if next "on" "off"))
    next))

(defun omnidiff-dual-toggle-edit ()
  "Toggle live edit mode on the editable AFTER buffer."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (ab (omnidiff-dual--session-after-buf session)))
    (unless (and (omnidiff-dual--session-editable-after-p session)
                 (buffer-live-p ab))
      (user-error "AFTER pane is not backed by an editable buffer"))
    (if (omnidiff-dual--session-editing-p session)
        ;; Return to review mode and refresh overlays.
        (progn
          (when-let* ((timer (omnidiff-dual--session-live-timer session)))
            (when (timerp timer) (cancel-timer timer))
            (setf (omnidiff-dual--session-live-timer session) nil))
          (setf (omnidiff-dual--session-editing-p session) nil)
          (with-current-buffer ab
            (remove-hook 'after-save-hook #'omnidiff-dual--on-after-save t)
            (remove-hook 'after-change-functions #'omnidiff-dual--on-after-change t)
            (omnidiff-dual-edit-mode -1)
            (setq buffer-read-only t)
            (omnidiff-dual-mode 1))
          (omnidiff-dual--refresh-headers session)
          (omnidiff-dual--refresh-async session)
          (message "OmniDiff review mode")
          nil)
      ;; Unlock AFTER buffer into live edit mode.
      (setf (omnidiff-dual--session-editing-p session) t)
      (omnidiff-dual--focus-side session 'after)
      (with-current-buffer ab
        (omnidiff-dual-mode -1)
        (setq buffer-read-only nil)
        (omnidiff-dual-edit-mode 1)
        (add-hook 'after-save-hook #'omnidiff-dual--on-after-save nil t)
        (add-hook 'after-change-functions #'omnidiff-dual--on-after-change nil t))
      (omnidiff-dual--refresh-headers session)
      (message "OmniDiff live edit mode (C-c C-c to return to review)")
      t)))

(defun omnidiff-dual-refresh ()
  "Asynchronously re-run `omnidiff --mode json' and refresh overlays."
  (interactive)
  (omnidiff-dual--refresh-async (omnidiff-dual--require-session)))

;;; Semantic change-stop revert & undo

(defun omnidiff-dual--find-stop-at-or-on-row (session side row byte-col)
  "Return `omnidiff-change-stop' containing (SIDE, ROW, BYTE-COL) or on ROW.
Exact span containment is preferred; otherwise picks the nearest stop whose
range (or counterpart range) on SIDE covers ROW."
  (or (omnidiff-dual--find-stop-at session side row byte-col)
      (let ((best nil)
            (best-dist most-positive-fixnum))
        (dolist (stop (omnidiff-dual--session-stops session))
          (let ((rng (cond
                      ((eq (omnidiff-change-stop-side stop) side)
                       (omnidiff-change-stop-range stop))
                      ((eq (omnidiff-change-stop-counterpart-side stop) side)
                       (omnidiff-change-stop-counterpart-range stop)))))
            (when (and rng (omnidiff-dual--range-covers-row-p rng row))
              (let ((dist (if (= (omnidiff-json-range-start-row rng) row)
                              (abs (- (omnidiff-json-range-start-col rng) byte-col))
                            0)))
                (when (< dist best-dist)
                  (setq best stop
                        best-dist dist))))))
        best)))

(defun omnidiff-dual--buffer-ends-with-newline-p (buf)
  "Return non-nil when non-empty BUF ends with a newline character."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (and (> (point-max) (point-min))
           (eq (char-before (point-max)) ?\n)))))

(defun omnidiff-dual--line-range-bounds (buf line-starts r0 r1)
  "Return `(BEG . END)' covering 0-based rows R0..R1 in BUF without trailing NL."
  (let* ((num-lines (omnidiff-dual--logical-line-count buf line-starts))
         (beg (aref line-starts r0))
         (end (if (< (1+ r1) num-lines)
                  (1- (aref line-starts (1+ r1)))
                (with-current-buffer buf
                  (save-restriction
                    (widen)
                    (let ((pmax (point-max)))
                      (if (and (> pmax beg) (eq (char-before pmax) ?\n))
                          (1- pmax)
                        pmax)))))))
    (cons beg (max beg end))))

(defun omnidiff-dual--direct-anchor (alignment side row)
  "Return counterpart row if ROW on SIDE is directly anchored in ALIGNMENT."
  (let* ((avec (omnidiff-engine--ensure-anchor-vec alignment))
         (before-p (eq side 'before))
         (lo 0)
         (hi (length avec))
         (found nil))
    (while (and (< lo hi) (null found))
      (let* ((mid (/ (+ lo hi) 2))
             (pair (aref avec mid))
             (s (if before-p (car pair) (cdr pair))))
        (cond
         ((= s row) (setq found (if before-p (cdr pair) (car pair))))
         ((< s row) (setq lo (1+ mid)))
         (t (setq hi mid)))))
    found))

(defun omnidiff-dual--surrounding-anchors (alignment side r0 r1)
  "Return `(PREV . NEXT)' anchor pairs strictly surrounding [R0, R1] on SIDE."
  (let* ((avec (omnidiff-engine--ensure-anchor-vec alignment))
         (alen (length avec))
         (before-p (eq side 'before))
         (prev nil)
         (next nil))
    ;; Binary search for first anchor with src-row >= r0.
    (let ((lo 0) (hi alen))
      (while (< lo hi)
        (let* ((mid (/ (+ lo hi) 2))
               (s (if before-p (car (aref avec mid)) (cdr (aref avec mid)))))
          (if (< s r0) (setq lo (1+ mid)) (setq hi mid))))
      (when (> lo 0)
        (setq prev (aref avec (1- lo)))))
    ;; Binary search for first anchor with src-row > r1.
    (let ((lo 0) (hi alen))
      (while (< lo hi)
        (let* ((mid (/ (+ lo hi) 2))
               (s (if before-p (car (aref avec mid)) (cdr (aref avec mid)))))
          (if (<= s r1) (setq lo (1+ mid)) (setq hi mid))))
      (when (< lo alen)
        (setq next (aref avec lo))))
    (cons prev next)))

(defun omnidiff-dual--range-partial-p (buf line-starts range r1)
  "Return non-nil when RANGE starts mid-line or ends before end of row R1 in BUF."
  (or (> (omnidiff-json-range-start-col range) 0)
      (and (= (omnidiff-json-range-end-row range) r1)
           (let* ((bounds (omnidiff-dual--line-range-bounds buf line-starts r1 r1))
                  (line-bytes
                   (with-current-buffer buf
                     (save-restriction
                       (widen)
                       (string-bytes
                        (buffer-substring-no-properties (car bounds) (cdr bounds)))))))
             (< (omnidiff-json-range-end-col range) line-bytes)))))

(defun omnidiff-dual--plan-stop-revert (session stop)
  "Compute a revert edit plan plist for non-move `omnidiff-change-stop' STOP.
Returns one of:
- `(:kind replace :a0 A0 :a1 A1 :b0 B0 :b1 B1)'
- `(:kind delete :a0 A0 :a1 A1)'
- `(:kind insert :insert-row ROW :b0 B0 :b1 B1)'."
  (let* ((bb (omnidiff-dual--session-before-buf session))
         (ab (omnidiff-dual--session-after-buf session))
         (b-starts (omnidiff-dual--session-before-line-starts session))
         (a-starts (omnidiff-dual--session-after-line-starts session))
         (nb (omnidiff-dual--logical-line-count bb b-starts))
         (na (omnidiff-dual--logical-line-count ab a-starts))
         (alignment (omnidiff-dual--session-alignment session))
         (side (omnidiff-change-stop-side stop))
         (rng (omnidiff-change-stop-range stop))
         (cp-side (omnidiff-change-stop-counterpart-side stop))
         (cp-rng (omnidiff-change-stop-counterpart-range stop)))
    (cond
     ;; 1. Paired edit: replace AFTER rows with corresponding BEFORE rows.
     ((and cp-rng (omnidiff-dual--range-nonempty-p cp-rng))
      (let* ((a-rng (if (eq side 'after) rng cp-rng))
             (b-rng (if (eq side 'before) rng cp-rng))
             (a0 (omnidiff-json-range-start-row a-rng))
             (a1 (or (omnidiff-dual--range-last-row a-rng na) a0))
             (b0 (omnidiff-json-range-start-row b-rng))
             (b1 (or (omnidiff-dual--range-last-row b-rng nb) b0)))
        (list :kind 'replace :a0 a0 :a1 a1 :b0 b0 :b1 b1)))
     ;; 2. Unpaired AFTER stop (inline addition on existing line, or pure line insert).
     ((eq side 'after)
      (let* ((a0 (omnidiff-json-range-start-row rng))
             (a1 (or (omnidiff-dual--range-last-row rng na) a0))
             (b-direct (when (= a0 a1)
                         (omnidiff-dual--direct-anchor alignment 'after a0))))
        (if (and (integerp b-direct) (< b-direct nb))
            (list :kind 'replace :a0 a0 :a1 a0 :b0 b-direct :b1 b-direct)
          (pcase-let* ((`(,prev . ,next)
                        (omnidiff-dual--surrounding-anchors alignment 'after a0 a1))
                       (b-lo (if prev (1+ (car prev)) 0))
                       (b-hi (if next (1- (car next)) (1- nb)))
                       (a-lo (if prev (1+ (cdr prev)) 0))
                       (a-hi (if next (1- (cdr next)) (1- na))))
            (if (and (<= b-lo b-hi)
                     (omnidiff-dual--range-partial-p ab a-starts rng a1))
                (if (and (= a-lo a0) (= a-hi a1))
                    (list :kind 'replace :a0 a0 :a1 a1 :b0 b-lo :b1 b-hi)
                  (let ((b-map (max b-lo (min b-hi (omnidiff-engine-map-row
                                                    alignment 'after a0)))))
                    (list :kind 'replace :a0 a0 :a1 a1 :b0 b-map :b1 b-map)))
              (list :kind 'delete :a0 a0 :a1 a1))))))
     ;; 3. Unpaired BEFORE stop (inline deletion on existing line, or pure line delete).
     (t
      (let* ((b0 (omnidiff-json-range-start-row rng))
             (b1 (or (omnidiff-dual--range-last-row rng nb) b0))
             (a-direct (when (and (= b0 b1) (eq cp-side 'after))
                         (omnidiff-dual--direct-anchor alignment 'before b0))))
        (if (and (integerp a-direct) (< a-direct na))
            (list :kind 'replace :a0 a-direct :a1 a-direct :b0 b0 :b1 b0)
          (pcase-let* ((`(,prev . ,next)
                        (omnidiff-dual--surrounding-anchors alignment 'before b0 b1))
                       (b-lo (if prev (1+ (car prev)) 0))
                       (b-hi (if next (1- (car next)) (1- nb)))
                       (a-lo (if prev (1+ (cdr prev)) 0))
                       (a-hi (if next (1- (cdr next)) (1- na))))
            (if (and (<= a-lo a-hi)
                     (omnidiff-dual--range-partial-p bb b-starts rng b1))
                (if (and (= b-lo b0) (= b-hi b1))
                    (list :kind 'replace :a0 a-lo :a1 a-hi :b0 b0 :b1 b1)
                  (let ((a-map (max a-lo (min a-hi (omnidiff-engine-map-row
                                                    alignment 'before b0)))))
                    (list :kind 'replace :a0 a-map :a1 a-map :b0 b0 :b1 b1)))
              (let ((insert-row
                     (cond
                      ((<= na 0) 0)
                      ((> a-lo a-hi) (if next (cdr next) na))
                      (t
                       (let* ((b-span (max 1 (1+ (- b-hi b-lo))))
                              (a-span (1+ (- a-hi a-lo)))
                              (off (round (* (float (max 0 (- b0 b-lo))) a-span) b-span))
                              (hi-limit (if next (cdr next) na)))
                         (max a-lo (min hi-limit (+ a-lo off))))))))
                (list :kind 'insert :insert-row insert-row :b0 b0 :b1 b1))))))))))

(defun omnidiff-dual--apply-revert-plan (session plan)
  "Apply revert PLAN to SESSION's AFTER buffer as one atomic undo step.
Returns the buffer position in AFTER where the edit started."
  (let* ((bb (omnidiff-dual--session-before-buf session))
         (ab (omnidiff-dual--session-after-buf session))
         (b-starts (omnidiff-dual--session-before-line-starts session))
         (a-starts (omnidiff-dual--session-after-line-starts session))
         (nb (omnidiff-dual--logical-line-count bb b-starts))
         (na (omnidiff-dual--logical-line-count ab a-starts))
         (b-ends-nl (omnidiff-dual--buffer-ends-with-newline-p bb))
         (kind (plist-get plan :kind))
         (edit-pos nil))
    (cl-flet ((before-text (b0 b1)
                (let ((bounds (omnidiff-dual--line-range-bounds bb b-starts b0 b1)))
                  (with-current-buffer bb
                    (save-restriction
                      (widen)
                      (buffer-substring-no-properties (car bounds) (cdr bounds)))))))
      (with-current-buffer ab
        (save-restriction
          (widen)
          (undo-boundary)
          (let ((inhibit-read-only t))
            (pcase kind
              ('replace
               (let* ((a0 (plist-get plan :a0))
                      (a1 (plist-get plan :a1))
                      (b0 (plist-get plan :b0))
                      (b1 (plist-get plan :b1))
                      (bounds (omnidiff-dual--line-range-bounds ab a-starts a0 a1))
                      (beg (car bounds))
                      (end (cdr bounds))
                      (txt (before-text b0 b1)))
                 (omnidiff-dual--expand-fold-at beg)
                 (goto-char beg)
                 (delete-region beg end)
                 (insert txt)
                 (setq edit-pos beg)))
              ('delete
               (let* ((a0 (plist-get plan :a0))
                      (a1 (plist-get plan :a1))
                      (beg (aref a-starts a0))
                      (pmax (point-max))
                      (del-beg beg)
                      (del-end (cond
                                ((< (1+ a1) na)
                                 (aref a-starts (1+ a1)))
                                ((= a0 0)
                                 pmax)
                                (t
                                 (unless (and (> nb 0) b-ends-nl)
                                   (setq del-beg (max (point-min) (1- beg))))
                                 pmax))))
                 (omnidiff-dual--expand-fold-at del-beg)
                 (delete-region del-beg del-end)
                 (setq edit-pos (min del-beg (point-max)))))
              ('insert
               (let* ((insert-row (plist-get plan :insert-row))
                      (b0 (plist-get plan :b0))
                      (b1 (plist-get plan :b1))
                      (txt (before-text b0 b1))
                      (b-has-nl (or (< (1+ b1) nb) b-ends-nl)))
                 (cond
                  ((<= na 0)
                   (goto-char (point-min))
                   (setq edit-pos (point))
                   (insert txt)
                   (when b-has-nl (insert "\n")))
                  ((< insert-row na)
                   (let ((pos (aref a-starts insert-row)))
                     (omnidiff-dual--expand-fold-at pos)
                     (goto-char pos)
                     (setq edit-pos pos)
                     (insert txt "\n")))
                  (t
                   (goto-char (point-max))
                   (let ((a-ends-nl (and (> (point-max) (point-min))
                                         (eq (char-before (point-max)) ?\n))))
                     (unless a-ends-nl
                       (insert "\n"))
                     (setq edit-pos (point))
                     (insert txt)
                     (when b-has-nl
                       (insert "\n")))))))))
          (undo-boundary)
          (when edit-pos
            (goto-char (max (point-min) (min edit-pos (point-max))))))))
    edit-pos))

(defun omnidiff-dual-revert-change ()
  "Revert the semantic change stop at point into the editable AFTER buffer.
Works directly from read-only review mode (`r') or live edit mode (`C-c r')
when AFTER is backed by an editable buffer.  Refuses `move' stops to avoid
duplicating moved code.  Leaves the buffer unsaved with an atomic Emacs undo
boundary (`u' to undo) and immediately triggers an asynchronous diff refresh."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (bb (omnidiff-dual--session-before-buf session))
         (ab (omnidiff-dual--session-after-buf session)))
    (unless (and (omnidiff-dual--session-editable-after-p session)
                 (buffer-live-p ab)
                 (buffer-live-p bb))
      (user-error "AFTER pane is not backed by an editable buffer"))
    (when (let ((proc (omnidiff-dual--session-active-proc session)))
            (and proc (process-live-p proc)))
      (user-error "Diff refresh is in progress; wait a moment before reverting"))
    (unless (and (omnidiff-dual--line-starts-fresh-p
                  bb
                  (omnidiff-dual--session-before-line-starts session)
                  (omnidiff-dual--session-before-lines-tick session))
                 (omnidiff-dual--line-starts-fresh-p
                  ab
                  (omnidiff-dual--session-after-line-starts session)
                  (omnidiff-dual--session-after-lines-tick session)))
      (user-error "Diff is out of date; press g (or C-c C-r) to refresh before reverting"))
    (let* ((side (or omnidiff-dual--side (omnidiff-dual--session-active-side session)))
           (buf (omnidiff-dual--side-buf session side))
           (starts (omnidiff-dual--side-line-starts session side))
           (tick (omnidiff-dual--side-lines-tick session side))
           (rc (omnidiff-dual--point-to-row-byte-col
                buf starts (with-current-buffer buf (point)) tick))
           (stop (omnidiff-dual--find-stop-at-or-on-row session side (car rc) (cdr rc))))
      (unless stop
        (user-error "No change stop at point (use n/p to jump to a change)"))
      (when (eq (omnidiff-change-stop-operation stop) 'move)
        (user-error "Cannot revert a moved block automatically (press RET to inspect counterpart, or e to edit)"))
      (let* ((plan (omnidiff-dual--plan-stop-revert session stop))
             (idx (omnidiff-change-stop-index stop))
             (total (length (omnidiff-dual--session-stops session)))
             (op (omnidiff-change-stop-operation stop)))
        (omnidiff-dual--apply-revert-plan session plan)
        (when-let* ((timer (omnidiff-dual--session-live-timer session)))
          (when (timerp timer) (cancel-timer timer))
          (setf (omnidiff-dual--session-live-timer session) nil))
        (setf (omnidiff-dual--session-mutated-p session) t
              (omnidiff-dual--session-last-jump session) nil)
        (omnidiff-dual--refresh-async session)
        (message "Reverted change %d/%d (%s)%s"
                 idx total op
                 (if (omnidiff-dual--session-editing-p session)
                     ""
                   " — u to undo"))
        plan))))

(defun omnidiff-dual-undo ()
  "Undo the last edit or revert in the editable AFTER buffer and refresh."
  (interactive)
  (let* ((session (omnidiff-dual--require-session))
         (ab (omnidiff-dual--session-after-buf session)))
    (unless (and (omnidiff-dual--session-editable-after-p session)
                 (buffer-live-p ab))
      (user-error "AFTER pane is not backed by an editable buffer"))
    (with-current-buffer ab
      (when (eq buffer-undo-list t)
        (user-error "Undo is disabled in AFTER buffer"))
      (omnidiff-dual--expand-fold-at (point))
      (let ((inhibit-read-only t)
            (last-command (if (eq last-command 'omnidiff-dual-undo)
                              'undo
                            last-command)))
        (undo-only 1)))
    (when-let* ((timer (omnidiff-dual--session-live-timer session)))
      (when (timerp timer) (cancel-timer timer))
      (setf (omnidiff-dual--session-live-timer session) nil))
    (setf (omnidiff-dual--session-mutated-p session) t
          (omnidiff-dual--session-last-jump session) nil)
    (omnidiff-dual--refresh-async session)
    (message "Undid last change in AFTER buffer")
    t))

(defun omnidiff-dual--menu-flag (label active-p)
  "Format transient menu toggle LABEL showing live ACTIVE-P state."
  (format "%s [%s]" label (if active-p "on" "off")))

(transient-define-prefix omnidiff-dual-menu ()
  "Interactive command dispatch menu for the OmniDiff Dual Semantic Viewer."
  [:description
   (lambda ()
     (if-let* ((s (omnidiff-dual-active-session)))
         (format "OmniDiff Dual — %s (%s%s)"
                 (or (omnidiff-dual--session-display-path s) "diff")
                 (omnidiff-dual--effective-layout s)
                 (if (omnidiff-dual--session-editing-p s) " · EDIT" ""))
       "OmniDiff Dual"))
   ["Navigate"
    ("n" "Next change" omnidiff-dual-next-change)
    ("p" "Previous change" omnidiff-dual-prev-change)
    ("RET" "Jump counterpart / expand fold" omnidiff-dual-jump-counterpart)
    ("o" "Switch / flip pane" omnidiff-dual-switch-pane)
    ("]" "Next file" omnidiff-dual-next-file)
    ("[" "Previous file" omnidiff-dual-prev-file)]
   ["View & Display"
    ("v" (lambda ()
           (let ((s (omnidiff-dual-active-session)))
             (format "Layout [%s]" (if s (omnidiff-dual--effective-layout s) "auto"))))
     omnidiff-dual-cycle-layout)
    ("z" (lambda ()
           (let ((s (omnidiff-dual-active-session)))
             (omnidiff-dual--menu-flag
              "Fold unchanged" (and s (omnidiff-dual--session-fold-unchanged s)))))
     omnidiff-dual-toggle-fold)
    ("c" (lambda () (format "Color scheme [%s]" omnidiff-color-scheme))
     omnidiff-dual-cycle-color-scheme)
    ("S" (lambda ()
           (let ((s (omnidiff-dual-active-session)))
             (omnidiff-dual--menu-flag
              "Syntax highlight" (and s (omnidiff-dual--session-show-syntax s)))))
     omnidiff-dual-toggle-syntax)
    ("H" (lambda ()
           (let ((s (omnidiff-dual-active-session)))
             (omnidiff-dual--menu-flag
              "Cross-highlight" (and s (omnidiff-dual--session-show-cross-highlight s)))))
     omnidiff-dual-toggle-counterpart-highlight)
    ("M" (lambda ()
           (let ((s (omnidiff-dual-active-session)))
             (omnidiff-dual--menu-flag
              "Minimap" (and s (omnidiff-dual--session-show-minimap s)))))
     omnidiff-dual-toggle-minimap)]
   ["Diff & Actions"
    ("m" (lambda ()
           (let ((s (omnidiff-dual-active-session)))
             (format "Preset [%s]" (if s (omnidiff-dual--session-preset s) "default"))))
     omnidiff-dual-cycle-preset)
    ("W" (lambda ()
           (let ((s (omnidiff-dual-active-session)))
             (omnidiff-dual--menu-flag
              "--whole-updates" (and s (omnidiff-dual--session-whole-updates s)))))
     omnidiff-dual-toggle-whole-updates)
    ("I" (lambda ()
           (let ((s (omnidiff-dual-active-session)))
             (omnidiff-dual--menu-flag
              "--paint-reindent-moves"
              (and s (omnidiff-dual--session-paint-reindent-moves s)))))
     omnidiff-dual-toggle-paint-reindent-moves)
    ("e" (lambda ()
           (let ((s (omnidiff-dual-active-session)))
             (omnidiff-dual--menu-flag
              "Live edit AFTER" (and s (omnidiff-dual--session-editing-p s)))))
     omnidiff-dual-toggle-edit)
    ("r" "Revert change stop" omnidiff-dual-revert-change)
    ("u" "Undo in AFTER" omnidiff-dual-undo)
    ("R" (lambda ()
           (let* ((s (omnidiff-dual-active-session))
                  (fn (and s (plist-get (omnidiff-dual--session-nav-callbacks s)
                                        :reviewed-p))))
             (omnidiff-dual--menu-flag
              "Reviewed" (and (functionp fn) (funcall fn)))))
     omnidiff-dual-toggle-reviewed)
    ("O" "Return to Overview" omnidiff-dual-open-overview)
    ("T" "Open in native TUI" omnidiff-dual-open-tui)
    ("g" "Refresh diff" omnidiff-dual-refresh)
    ("q" "Quit Dual session" omnidiff-dual-quit)]])

(defun omnidiff-dual-help ()
  "Open the interactive transient menu for OmniDiff Dual viewer.
In noninteractive sessions, displays a concise echo-area keybinding summary."
  (interactive)
  (if (not noninteractive)
      (call-interactively #'omnidiff-dual-menu)
    (let ((session (omnidiff-dual-active-session)))
      (if (and session (omnidiff-dual--session-editing-p session))
          (message "Edit: M-n/M-p change | C-c RET counterpart/fold | C-c r revert | C-c z fold | C-c C-c review | C-c C-r refresh | C-c C-q quit")
        (message "n/p change | RET/TAB counterpart/fold | o pane | ]/[ file | R reviewed | r revert | u undo | z fold | v layout | m preset | W whole | I reindent | c theme | S syntax | H x-hl | M minimap | e edit | g refresh | q quit")))))

(defun omnidiff-dual-quit ()
  "Close the current OmniDiff dual viewer session cleanly."
  (interactive)
  (omnidiff-dual--teardown (omnidiff-dual--require-session) nil t))

;;; Minor modes

(defvar omnidiff-dual-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'omnidiff-dual-next-change)
    (define-key map (kbd "p") #'omnidiff-dual-prev-change)
    (define-key map (kbd "RET") #'omnidiff-dual-jump-counterpart)
    (define-key map [return] #'omnidiff-dual-jump-counterpart)
    (define-key map (kbd "TAB") #'omnidiff-dual-jump-counterpart)
    (define-key map [tab] #'omnidiff-dual-jump-counterpart)
    (define-key map (kbd "o") #'omnidiff-dual-switch-pane)
    (define-key map (kbd "]") #'omnidiff-dual-next-file)
    (define-key map (kbd "[") #'omnidiff-dual-prev-file)
    (define-key map (kbd "N") #'omnidiff-dual-next-file)
    (define-key map (kbd "P") #'omnidiff-dual-prev-file)
    (define-key map (kbd "O") #'omnidiff-dual-open-overview)
    (define-key map (kbd "T") #'omnidiff-dual-open-tui)
    (define-key map (kbd "v") #'omnidiff-dual-cycle-layout)
    (define-key map (kbd "z") #'omnidiff-dual-toggle-fold)
    (define-key map (kbd "m") #'omnidiff-dual-cycle-preset)
    (define-key map (kbd "W") #'omnidiff-dual-toggle-whole-updates)
    (define-key map (kbd "I") #'omnidiff-dual-toggle-paint-reindent-moves)
    (define-key map (kbd "c") #'omnidiff-dual-cycle-color-scheme)
    (define-key map (kbd "S") #'omnidiff-dual-toggle-syntax)
    (define-key map (kbd "H") #'omnidiff-dual-toggle-counterpart-highlight)
    (define-key map (kbd "M") #'omnidiff-dual-toggle-minimap)
    (define-key map (kbd "e") #'omnidiff-dual-toggle-edit)
    (define-key map (kbd "r") #'omnidiff-dual-revert-change)
    (define-key map (kbd "u") #'omnidiff-dual-undo)
    (define-key map (kbd "R") #'omnidiff-dual-toggle-reviewed)
    (define-key map (kbd "g") #'omnidiff-dual-refresh)
    (define-key map (kbd "?") #'omnidiff-dual-help)
    (define-key map (kbd "q") #'omnidiff-dual-quit)
    map)
  "Keymap for `omnidiff-dual-mode' (read-only semantic review).")

(defvar omnidiff-dual--emulation-alist nil
  "Emulation keymap alist giving `omnidiff-dual-mode' priority over modal maps.")

(define-minor-mode omnidiff-dual-mode
  "Minor mode for read-only semantic diff review in real source buffers."
  :lighter " OmniDiff"
  :keymap omnidiff-dual-mode-map
  :group 'omnidiff
  (when omnidiff-dual-mode
    (setq omnidiff-dual--emulation-alist
          `((omnidiff-dual-mode . ,omnidiff-dual-mode-map))
          emulation-mode-map-alists
          (cons 'omnidiff-dual--emulation-alist
                (delq 'omnidiff-dual--emulation-alist
                      emulation-mode-map-alists)))))

(defvar omnidiff-dual-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") #'omnidiff-dual-next-change)
    (define-key map (kbd "M-p") #'omnidiff-dual-prev-change)
    (define-key map (kbd "C-c RET") #'omnidiff-dual-jump-counterpart)
    (define-key map (kbd "C-c r") #'omnidiff-dual-revert-change)
    (define-key map (kbd "C-c z") #'omnidiff-dual-toggle-fold)
    (define-key map (kbd "C-c ?") #'omnidiff-dual-help)
    (define-key map (kbd "C-c C-c") #'omnidiff-dual-toggle-edit)
    (define-key map (kbd "C-c C-r") #'omnidiff-dual-refresh)
    (define-key map (kbd "C-c C-q") #'omnidiff-dual-quit)
    map)
  "Keymap for `omnidiff-dual-edit-mode' (live editing without key shadowing).")

(define-minor-mode omnidiff-dual-edit-mode
  "Minor mode for live-editing the AFTER worktree buffer in a dual session."
  :lighter " OmniDiff[Edit]"
  :keymap omnidiff-dual-edit-mode-map
  :group 'omnidiff)

;;; Public entry point

(defun omnidiff-dual--resolve-display-name (display-path before-path after-path
                                                        before-buffer after-buffer)
  "Resolve a concise display label from caller inputs."
  (cond
   ((and (stringp display-path) (not (string-empty-p display-path)))
    display-path)
   ((and (stringp after-path) (not (string-empty-p after-path))
         (not (string= after-path "/dev/null")))
    (file-name-nondirectory after-path))
   ((and (stringp before-path) (not (string-empty-p before-path))
         (not (string= before-path "/dev/null")))
    (file-name-nondirectory before-path))
   ((buffer-live-p after-buffer)
    (buffer-name after-buffer))
   ((buffer-live-p before-buffer)
    (buffer-name before-buffer))
   (t "diff")))

(defun omnidiff-dual--resolve-syntax-hint (side-path display-path fallback-path)
  "Pick best path hint for `auto-mode-alist' detection."
  (cl-find-if
   (lambda (p)
     (and (stringp p) (not (string-empty-p p)) (not (string= p "/dev/null"))))
   (list side-path display-path fallback-path)))

;;;###autoload
(cl-defun omnidiff-dual-open (&key before-path after-path
                                   before-source after-source
                                   before-buffer after-buffer
                                   before-label after-label
                                   display-path json-diff
                                   preset
                                   (whole-updates omnidiff-whole-updates)
                                   (paint-reindent-moves omnidiff-paint-reindent-moves)
                                   (editable-after nil editable-after-supplied-p)
                                   nav-callbacks
                                   reuse-session)
  "Open or update a native Dual Semantic Source Viewer session.
Returns the active `omnidiff-dual--session'.

If JSON-DIFF is already provided (an `omnidiff-json-diff' struct), renders
immediately with zero subprocesses.  Otherwise runs `omnidiff-engine-run-pair'
to compute the semantic diff.

When AFTER-BUFFER is provided, or EDITABLE-AFTER is non-nil with a readable
and writable AFTER-PATH, attaches directly to the user's real buffer without
overwriting it or killing it on quit.

When REUSE-SESSION is a live `omnidiff-dual--session', swaps its contents
in place without churning the window configuration."
  (let* ((reusing-p (and (omnidiff-dual--session-p reuse-session)
                         (memq reuse-session omnidiff-dual--sessions)))
         (session (if reusing-p
                      reuse-session
                    (make-omnidiff-dual--session
                     :id (cl-incf omnidiff-dual--next-id)
                     :layout omnidiff-dual-layout
                     :show-minimap omnidiff-dual-show-minimap
                     :show-syntax t
                     :show-cross-highlight omnidiff-dual-highlight-counterpart)))
         (eff-preset (or preset 'default))
         (disp-name (omnidiff-dual--resolve-display-name
                     display-path before-path after-path
                     before-buffer after-buffer))
         (b-hint (omnidiff-dual--resolve-syntax-hint
                  before-path display-path after-path))
         (a-hint (omnidiff-dual--resolve-syntax-hint
                  after-path display-path before-path))
         (old-before-buf (when reusing-p (omnidiff-dual--session-before-buf session)))
         (old-after-buf (when reusing-p (omnidiff-dual--session-after-buf session)))
         (old-before-owns (when reusing-p (omnidiff-dual--session-before-owns-buf-p session)))
         (old-after-owns (when reusing-p (omnidiff-dual--session-after-owns-buf-p session)))
         (old-b-ro (when reusing-p (omnidiff-dual--session-before-orig-read-only session)))
         (old-a-ro (when reusing-p (omnidiff-dual--session-after-orig-read-only session)))
         (old-b-ln (when reusing-p (omnidiff-dual--session-before-orig-line-numbers session)))
         (old-a-ln (when reusing-p (omnidiff-dual--session-after-orig-line-numbers session)))
         (old-b-hl-loc (when reusing-p (omnidiff-dual--session-before-orig-header-line-local-p session)))
         (old-a-hl-loc (when reusing-p (omnidiff-dual--session-after-orig-header-line-local-p session)))
         (old-b-hl (when reusing-p (omnidiff-dual--session-before-orig-header-line session)))
         (old-a-hl (when reusing-p (omnidiff-dual--session-after-orig-header-line session)))
         (old-b-fl (when reusing-p (omnidiff-dual--session-before-orig-font-lock session)))
         (old-a-fl (when reusing-p (omnidiff-dual--session-after-orig-font-lock session)))
         (old-b-hl-remap (when reusing-p (omnidiff-dual--session-before-hl-remap-cookie session)))
         (old-a-hl-remap (when reusing-p (omnidiff-dual--session-after-hl-remap-cookie session)))
         (temp-files nil)
         (resolved-diff json-diff)
         (new-before-buf nil)
         (new-before-owns nil)
         (new-after-buf nil)
         (new-after-owns nil)
         (can-edit-after nil)
         (committed nil))
    (unwind-protect
        (progn
          ;; Cancel any in-flight async work when reusing a session.
          (when reusing-p
            (cl-incf (omnidiff-dual--session-request-seq session))
            (when-let* ((timer (omnidiff-dual--session-live-timer session)))
              (when (timerp timer) (cancel-timer timer))
              (setf (omnidiff-dual--session-live-timer session) nil))
            (when-let* ((proc (omnidiff-dual--session-active-proc session)))
              (when (process-live-p proc) (delete-process proc))
              (setf (omnidiff-dual--session-active-proc session) nil))
            (omnidiff-dual--clear-focus-overlays session))
          ;; Compute JSON diff when caller did not supply one.
          (unwind-protect
              (unless resolved-diff
                (let* ((b-disk (cond
                                ((and (buffer-live-p before-buffer)
                                      (or (null before-path)
                                          (not (file-exists-p before-path))
                                          (buffer-modified-p before-buffer)))
                                 (let ((f (omnidiff-engine-write-temp-for-buffer
                                           before-buffer
                                           (omnidiff-dual--ensure-tmpdir session)
                                           (or b-hint disp-name))))
                                   (push f temp-files)
                                   f))
                                ((and (stringp before-path)
                                      (or (string= before-path "/dev/null")
                                          (file-exists-p before-path)))
                                 before-path)
                                ((stringp before-source)
                                 (let ((f (omnidiff-dual--write-string-temp
                                           before-source
                                           (omnidiff-dual--ensure-tmpdir session)
                                           (or b-hint disp-name))))
                                   (push f temp-files)
                                   f))
                                (t "/dev/null")))
                       (a-disk (cond
                                ((and (buffer-live-p after-buffer)
                                      (or (null after-path)
                                          (not (file-exists-p after-path))
                                          (buffer-modified-p after-buffer)))
                                 (let ((f (omnidiff-engine-write-temp-for-buffer
                                           after-buffer
                                           (omnidiff-dual--ensure-tmpdir session)
                                           (or a-hint disp-name))))
                                   (push f temp-files)
                                   f))
                                ((and (stringp after-path)
                                      (or (string= after-path "/dev/null")
                                          (file-exists-p after-path)))
                                 after-path)
                                ((stringp after-source)
                                 (let ((f (omnidiff-dual--write-string-temp
                                           after-source
                                           (omnidiff-dual--ensure-tmpdir session)
                                           (or a-hint disp-name))))
                                   (push f temp-files)
                                   f))
                                (t "/dev/null"))))
                  (setq resolved-diff
                        (omnidiff-engine-run-pair
                         b-disk a-disk eff-preset nil whole-updates paint-reindent-moves))))
            (omnidiff-dual--delete-temp-files temp-files))
          ;; Acquire BEFORE buffer.
          (let ((b-enc (and resolved-diff
                            (omnidiff-json-side-encoding
                             (omnidiff-json-diff-before resolved-diff))))
                (show-syn (omnidiff-dual--session-show-syntax session)))
            (if (buffer-live-p before-buffer)
                (setq new-before-buf before-buffer
                      new-before-owns nil)
              (let ((text (cond
                           ((stringp before-source) before-source)
                           ((stringp before-path)
                            (omnidiff-engine-read-file-decoded before-path b-enc))
                           (t ""))))
                (setq new-before-buf
                      (omnidiff-dual--make-ephemeral-buffer
                       'before disp-name text b-hint show-syn)
                      new-before-owns t))))
          ;; Acquire AFTER buffer.
          (let ((a-enc (and resolved-diff
                            (omnidiff-json-side-encoding
                             (omnidiff-json-diff-after resolved-diff))))
                (show-syn (omnidiff-dual--session-show-syntax session)))
            (cond
             ((buffer-live-p after-buffer)
              (setq new-after-buf after-buffer
                    new-after-owns nil
                    can-edit-after (if editable-after-supplied-p
                                       (and editable-after t)
                                     t)))
             ((and editable-after (omnidiff-dual--writable-file-p after-path))
              (setq new-after-buf (find-file-noselect after-path)
                    new-after-owns nil
                    can-edit-after t))
             (t
              (let ((text (cond
                           ((stringp after-source) after-source)
                           ((stringp after-path)
                            (omnidiff-engine-read-file-decoded after-path a-enc))
                           (t ""))))
                (setq new-after-buf
                      (omnidiff-dual--make-ephemeral-buffer
                       'after disp-name text a-hint show-syn)
                      new-after-owns t
                      can-edit-after nil)))))
          ;; Never double-attach the same live buffer as both BEFORE and AFTER.
          (when (and (buffer-live-p new-before-buf)
                     (eq new-before-buf new-after-buf))
            (let ((copy-text (with-current-buffer new-before-buf
                               (save-restriction
                                 (widen)
                                 (buffer-substring-no-properties
                                  (point-min) (point-max))))))
              (setq new-before-buf
                    (omnidiff-dual--make-ephemeral-buffer
                     'before disp-name copy-text b-hint
                     (omnidiff-dual--session-show-syntax session))
                    new-before-owns t)))
          ;; Register session metadata.
          (setf (omnidiff-dual--session-before-path session) before-path
                (omnidiff-dual--session-after-path session) after-path
                (omnidiff-dual--session-before-label session)
                (or before-label
                    (if (and (stringp before-path) (string= before-path "/dev/null"))
                        "/dev/null"
                      disp-name))
                (omnidiff-dual--session-after-label session)
                (or after-label
                    (if (and (stringp after-path) (string= after-path "/dev/null"))
                        "/dev/null"
                      disp-name))
                (omnidiff-dual--session-display-path session) disp-name
                (omnidiff-dual--session-preset session) eff-preset
                (omnidiff-dual--session-whole-updates session) (and whole-updates t)
                (omnidiff-dual--session-paint-reindent-moves session) (and paint-reindent-moves t)
                (omnidiff-dual--session-editable-after-p session) can-edit-after
                (omnidiff-dual--session-editing-p session) nil
                (omnidiff-dual--session-nav-callbacks session) nav-callbacks)
          (unless (memq session omnidiff-dual--sessions)
            (push session omnidiff-dual--sessions))
          ;; Attach new buffers (preserving original saved states if reusing same buffer).
          (if (and reusing-p (eq new-before-buf old-before-buf))
              (progn
                (omnidiff-dual--clear-overlays new-before-buf)
                (with-current-buffer new-before-buf
                  (setq buffer-read-only t)
                  (omnidiff-dual-edit-mode -1)
                  (omnidiff-dual-mode 1)))
            (omnidiff-dual--attach-buffer session 'before new-before-buf new-before-owns))
          (if (and reusing-p (eq new-after-buf old-after-buf))
              (progn
                (omnidiff-dual--clear-overlays new-after-buf)
                (with-current-buffer new-after-buf
                  (remove-hook 'after-save-hook #'omnidiff-dual--on-after-save t)
                  (remove-hook 'after-change-functions #'omnidiff-dual--on-after-change t)
                  (setq buffer-read-only t)
                  (omnidiff-dual-edit-mode -1)
                  (omnidiff-dual-mode 1)))
            (omnidiff-dual--attach-buffer session 'after new-after-buf new-after-owns))
          ;; Arrange or swap windows cleanly before disposing old buffers.
          (if reusing-p
              (omnidiff-dual--apply-layout session)
            (omnidiff-dual--setup-windows session))
          ;; Dispose replaced buffers from a reused session cleanly.
          (when reusing-p
            (when (and (buffer-live-p old-before-buf)
                       (not (eq old-before-buf new-before-buf))
                       (not (eq old-before-buf new-after-buf)))
              (let ((tmp (make-omnidiff-dual--session
                          :before-buf old-before-buf
                          :before-owns-buf-p old-before-owns
                          :before-orig-read-only old-b-ro
                          :before-orig-line-numbers old-b-ln
                          :before-orig-header-line-local-p old-b-hl-loc
                          :before-orig-header-line old-b-hl
                          :before-orig-font-lock old-b-fl
                          :before-hl-remap-cookie old-b-hl-remap)))
                (omnidiff-dual--detach-buffer tmp 'before)))
            (when (and (buffer-live-p old-after-buf)
                       (not (eq old-after-buf new-after-buf))
                       (not (eq old-after-buf new-before-buf)))
              (let ((tmp (make-omnidiff-dual--session
                          :after-buf old-after-buf
                          :after-owns-buf-p old-after-owns
                          :after-orig-read-only old-a-ro
                          :after-orig-line-numbers old-a-ln
                          :after-orig-header-line-local-p old-a-hl-loc
                          :after-orig-header-line old-a-hl
                          :after-orig-font-lock old-a-fl
                          :after-hl-remap-cookie old-a-hl-remap)))
                (omnidiff-dual--detach-buffer tmp 'after))))
          ;; Apply diff, alignment, overlays, folds, and jump to first change.
          (omnidiff-dual--apply-diff session resolved-diff nil)
          (setq committed t)
          session)
      (unless committed
        (if (not reusing-p)
            (progn
              (when (or (omnidiff-dual--session-before-buf session)
                        (omnidiff-dual--session-after-buf session)
                        (omnidiff-dual--session-created-tab-p session))
                (omnidiff-dual--teardown session nil t))
              (setq omnidiff-dual--sessions (delq session omnidiff-dual--sessions))
              (when (and new-before-owns (buffer-live-p new-before-buf))
                (kill-buffer new-before-buf))
              (when (and new-after-owns (buffer-live-p new-after-buf))
                (kill-buffer new-after-buf))
              (let ((dir (omnidiff-dual--session-tmpdir session)))
                (setf (omnidiff-dual--session-tmpdir session) nil)
                (when (and dir (file-directory-p dir))
                  (ignore-errors (delete-directory dir t)))))
          (when (and new-before-owns
                     (buffer-live-p new-before-buf)
                     (not (eq new-before-buf (omnidiff-dual--session-before-buf session))))
            (kill-buffer new-before-buf))
          (when (and new-after-owns
                     (buffer-live-p new-after-buf)
                     (not (eq new-after-buf (omnidiff-dual--session-after-buf session))))
            (kill-buffer new-after-buf)))))))

(provide 'omnidiff-dual)
;;; omnidiff-dual.el ends here
