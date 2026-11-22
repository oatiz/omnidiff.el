;;; omnidiff-engine.el --- JSON diff engine and alignment model for omnidiff -*- lexical-binding: t; -*-

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

;; Core engine consuming `omnidiff --mode json' output (compatible with
;; upstream `omnidiff' 0.5.0+).  Provides:
;;
;;  - Strongly-typed `cl-defstruct' representations for diff metadata,
;;    JSON diff output, picture diffs, and recursive container diffs
;;    (archives, fonts, cursors, catalogs, documents).
;;  - Exact UTF-8 byte-column to Emacs character/buffer-position conversion
;;    with O(N) buffer line-offset tables and EOF-normalized range handling.
;;  - Pure-Elisp Patience + bounded LCS line alignment, row interpolation,
;;    counterpart lookup, and merged reading-order change stops.
;;  - Synchronous and asynchronous `omnidiff --mode json' invocation and
;;    decoded file/buffer helpers.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

;;; Shared protocol sentinels & core diff model

(defconst omnidiff-engine-file-sentinel "\x1cOMNIDIFF-FILE\x00"
  "Per-file sentinel emitted by the `GIT_EXTERNAL_DIFF' shell wrapper.")

(defconst omnidiff-engine-mode-sentinel "\x1cOMNIDIFF-MODE\x00"
  "Optional mode-change marker emitted after file metadata when modes differ.")

(defconst omnidiff-engine-oid-sentinel "\x1cOMNIDIFF-OID\x00"
  "Optional Git OID marker emitted after file metadata for blob retrieval.")

(defconst omnidiff-engine-end-sentinel "\x1cOMNIDIFF-END\x00"
  "End-of-record sentinel emitted after JSON output under `GIT_EXTERNAL_DIFF'.")

(defun omnidiff-engine--normalize-mode (mode)
  "Return Git MODE string if meaningful, else nil."
  (when (and (stringp mode)
             (not (string-empty-p mode))
             (not (string= mode "."))
             (not (string= mode "000000")))
    mode))

(defun omnidiff-engine--normalize-oid (oid)
  "Return Git OID string if it is a non-zero SHA-1/SHA-256 hex ID, else nil."
  (when (and (stringp oid)
             (or (= (length oid) 40) (= (length oid) 64))
             (string-match-p "\\`[0-9a-f]+\\'" oid)
             (not (string-match-p "\\`0+\\'" oid)))
    oid))

(cl-defstruct (omnidiff-scope-item (:copier nil))
  "Enclosing syntax scope anchor line (`@')."
  (line 0 :type integer)
  (text "" :type string))

(cl-defstruct (omnidiff-picture-info (:copier nil))
  "Format and dimension metadata for one side of a picture diff."
  (format "" :type string)
  (width 0 :type integer)
  (height 0 :type integer)
  (color nil)
  (bytes 0 :type integer))

(cl-defstruct (omnidiff-picture-region (:copier nil))
  "Bounding box of one changed pixel region reported by `omnidiff'."
  (x 0 :type integer)
  (y 0 :type integer)
  (width 0 :type integer)
  (height 0 :type integer))

(cl-defstruct (omnidiff-picture (:copier nil))
  "Structured picture diff metadata parsed from `omnidiff' output."
  (before nil) ; omnidiff-picture-info | nil
  (after nil)  ; omnidiff-picture-info | nil
  (comparison 'one-sided :type symbol) ; one-sided | identical | pixels | resized | resized-scaled | svg | unknown
  (raw-comparison nil)
  (changed-pct nil)
  (changed-pixels nil)
  (total-pixels nil)
  (region-count 0 :type integer)
  (regions nil :type list)
  (more-regions 0 :type integer)
  (svg-p nil :type boolean))

(cl-defstruct (omnidiff-file (:copier nil))
  "Parsed diff entry for one file."
  (path "" :type string)
  (target-path "" :type string)
  (status 'modified :type symbol) ; added | deleted | renamed | modified
  (preamble nil) ; optional commit metadata preceding this file (multi-commit `git show')
  (old-mode nil)
  (new-mode nil)
  (old-oid nil)
  (new-oid nil)
  (picture nil)
  (note nil)
  (added 0 :type integer)
  (removed 0 :type integer)
  (updated 0 :type integer)
  (moved 0 :type integer)
  (max-line 0 :type integer)
  (json-diff nil) ; omnidiff-json-diff | nil
  (before-source nil) ; decoded BEFORE text string | nil
  (after-source nil) ; decoded AFTER text string | nil
  (before-disk-path nil) ; real or temp filesystem path for BEFORE | nil
  (after-disk-path nil) ; real or temp filesystem path for AFTER | nil
  (summary nil) ; nil | no-changes | new-file | deleted-file | whitespace-only | comment-only | refactor-moved-only
  (language nil) ; detected language string | nil
  (large-residual nil :type boolean)
  (binary-p nil :type boolean)
  (container nil) ; omnidiff-json-container | nil
  (json-render-plan nil) ; cached `omnidiff-json-render-plan' | nil
  (json-render-plan-key nil)) ; cache key validating `json-render-plan'

(cl-defstruct (omnidiff-diff (:copier nil))
  "Top-level parsed `git --ext-diff' + `omnidiff' output."
  (preamble nil)
  (files nil :type list))

(defvar omnidiff-executable)

;;; Customization

(defcustom omnidiff-whole-updates nil
  "When non-nil, pass `--whole-updates' to `omnidiff --mode json'."
  :type 'boolean
  :group 'omnidiff)

(defcustom omnidiff-paint-reindent-moves nil
  "When non-nil, pass `--paint-reindent-moves' to `omnidiff --mode json'."
  :type 'boolean
  :group 'omnidiff)

(defcustom omnidiff-alignment-max-lines 8000
  "Maximum line count per side for full Patience/LCS line alignment.
Files exceeding this threshold skip interior LCS matching and fall back
to fast common prefix/suffix anchors plus linear interpolation."
  :type 'natnum
  :group 'omnidiff)

;;; Data structures

(cl-defstruct (omnidiff-json-range (:copier nil))
  "Right-open 0-based source range whose columns are UTF-8 byte offsets."
  (start-row 0 :type integer)
  (start-col 0 :type integer)
  (end-row 0 :type integer)
  (end-col 0 :type integer))

(cl-defstruct (omnidiff-json-hunk (:copier nil))
  "Single changed range reported by `omnidiff --mode json' on one side."
  (operation 'update :type symbol) ; insert | delete | update | move
  (range nil)                      ; omnidiff-json-range
  (move-target nil)                ; omnidiff-json-range | nil
  (reference-line nil))            ; 0-based integer | nil

(cl-defstruct (omnidiff-json-side (:copier nil))
  "One side (`before' or `after') of a JSON diff.
`hunks' is always sorted by `(start-row, start-col, end-row, end-col)'."
  (path "" :type string)
  (language nil)
  (encoding nil)
  (hunks nil :type list))

(cl-defstruct (omnidiff-json-container-side (:copier nil))
  "Format and size metadata for one side of a container diff."
  (format "" :type string)
  (bytes 0 :type integer)
  (members 0 :type integer))

(cl-defstruct (omnidiff-json-container-member (:copier nil))
  "One changed, added, or removed member inside a container diff."
  (key "" :type string)
  (status 'changed :type symbol) ; same | changed | added | removed
  (detail-kind nil)              ; picture | content | text | binary | nil
  (detail nil))                  ; omnidiff-picture | omnidiff-json-container | plist | nil

(cl-defstruct (omnidiff-json-container (:copier nil))
  "Structured diff for a container pair (archive, font, cursor, catalog, PDF)."
  (family 'archives :type symbol) ; archives | cursors | fonts | catalogs | documents
  (before nil)                    ; omnidiff-json-container-side | nil
  (after nil)                     ; omnidiff-json-container-side | nil
  (unchanged 0 :type integer)
  (members nil :type list))

(cl-defstruct (omnidiff-json-diff (:copier nil))
  "Top-level parsed result of `omnidiff --mode json'."
  (before nil) ; omnidiff-json-side
  (after nil)  ; omnidiff-json-side
  (large-residual nil :type boolean)
  (summary nil) ; nil | no-changes | new-file | deleted-file | whitespace-only | comment-only | refactor-moved-only
  (binary-p nil :type boolean)
  (picture nil)   ; omnidiff-picture | nil
  (container nil)) ; omnidiff-json-container | nil

(cl-defstruct (omnidiff-change-stop (:copier nil))
  "Single navigable logical change stop across both sides of a diff."
  (index 1 :type integer)
  (side 'after :type symbol) ; before | after
  (operation 'update :type symbol)
  (range nil)            ; omnidiff-json-range
  (counterpart-side nil) ; before | after | nil
  (counterpart-range nil) ; omnidiff-json-range | nil
  (reference-line nil))   ; 0-based integer | nil

(cl-defstruct (omnidiff-line-alignment (:copier nil))
  "Bidirectional 0-based row mapping and monotonic anchors between two files."
  (before-to-after nil) ; vector of (integer | nil)
  (after-to-before nil) ; vector of (integer | nil)
  (anchors nil :type list) ; strictly increasing list of (before-row . after-row)
  (anchor-vec nil)) ; internal vector mirror of `anchors' for O(log A) lookup

;;; JSON parsing

(defun omnidiff-engine--json-bool (val)
  "Convert parsed JSON boolean VAL (`t', `:false', or nil) to an Elisp boolean."
  (eq val t))

(defun omnidiff-engine--parse-summary (str)
  "Parse JSON `summary' snake_case STR into a Lisp symbol, or nil."
  (pcase str
    ('nil nil)
    ("no_changes" 'no-changes)
    ("new_file" 'new-file)
    ("deleted_file" 'deleted-file)
    ("whitespace_only" 'whitespace-only)
    ("comment_only" 'comment-only)
    ("refactor_moved_only" 'refactor-moved-only)
    ((pred stringp) (intern (string-replace "_" "-" str)))
    (_ (error "Invalid omnidiff JSON summary: %S" str))))

(defun omnidiff-engine--parse-operation (str)
  "Parse JSON hunk `operation' STR into a Lisp symbol."
  (pcase str
    ("insert" 'insert)
    ("delete" 'delete)
    ("update" 'update)
    ("move" 'move)
    (_ (error "Invalid omnidiff JSON hunk operation: %S" str))))

(defun omnidiff-engine--parse-range (ht)
  "Parse JSON range hash table HT into an `omnidiff-json-range'."
  (unless (hash-table-p ht)
    (error "Expected JSON range object, got: %S" ht))
  (make-omnidiff-json-range
   :start-row (or (gethash "start_row" ht) 0)
   :start-col (or (gethash "start_column" ht) 0)
   :end-row (or (gethash "end_row" ht) 0)
   :end-col (or (gethash "end_column" ht) 0)))

(defun omnidiff-engine--range-less-p (a b)
  "Return non-nil if `omnidiff-json-range' A precedes B lexicographically."
  (let ((asr (omnidiff-json-range-start-row a))
        (bsr (omnidiff-json-range-start-row b)))
    (cond
     ((/= asr bsr) (< asr bsr))
     ((/= (omnidiff-json-range-start-col a) (omnidiff-json-range-start-col b))
      (< (omnidiff-json-range-start-col a) (omnidiff-json-range-start-col b)))
     ((/= (omnidiff-json-range-end-row a) (omnidiff-json-range-end-row b))
      (< (omnidiff-json-range-end-row a) (omnidiff-json-range-end-row b)))
     (t
      (< (omnidiff-json-range-end-col a) (omnidiff-json-range-end-col b))))))

(defun omnidiff-engine--hunk-less-p (a b)
  "Return non-nil if `omnidiff-json-hunk' A precedes B by range."
  (omnidiff-engine--range-less-p
   (omnidiff-json-hunk-range a)
   (omnidiff-json-hunk-range b)))

(defun omnidiff-engine--parse-hunk (ht)
  "Parse JSON hunk hash table HT into an `omnidiff-json-hunk'."
  (unless (hash-table-p ht)
    (error "Expected JSON hunk object, got: %S" ht))
  (let ((move-target (gethash "move_target" ht))
        (ref-line (gethash "reference_line" ht)))
    (make-omnidiff-json-hunk
     :operation (omnidiff-engine--parse-operation (gethash "operation" ht))
     :range (omnidiff-engine--parse-range (gethash "range" ht))
     :move-target (when move-target (omnidiff-engine--parse-range move-target))
     :reference-line (when (integerp ref-line) ref-line))))

(defun omnidiff-engine--parse-side (ht)
  "Parse JSON side hash table HT into an `omnidiff-json-side' with sorted hunks."
  (unless (hash-table-p ht)
    (error "Expected JSON side object, got: %S" ht))
  (let* ((raw-hunks (gethash "hunks" ht))
         (hunks (mapcar #'omnidiff-engine--parse-hunk raw-hunks)))
    (make-omnidiff-json-side
     :path (or (gethash "path" ht) "")
     :language (gethash "language" ht)
     :encoding (gethash "encoding" ht)
     :hunks (sort hunks #'omnidiff-engine--hunk-less-p))))

(defun omnidiff-engine--parse-picture-info (ht)
  "Parse JSON `picture_info' hash table HT into `omnidiff-picture-info', or nil."
  (when (hash-table-p ht)
    (make-omnidiff-picture-info
     :format (or (gethash "format" ht) "")
     :width (or (gethash "width" ht) 0)
     :height (or (gethash "height" ht) 0)
     :color (gethash "color" ht)
     :bytes (or (gethash "bytes" ht) 0))))

(defun omnidiff-engine--parse-picture-regions (raw-regions)
  "Parse JSON list RAW-REGIONS into a list of `omnidiff-picture-region's."
  (mapcar (lambda (r)
            (make-omnidiff-picture-region
             :x (or (gethash "x" r) 0)
             :y (or (gethash "y" r) 0)
             :width (or (gethash "width" r) 0)
             :height (or (gethash "height" r) 0)))
          raw-regions))

(defun omnidiff-engine--parse-picture (ht)
  "Parse JSON picture / tagged_picture hash table HT into `omnidiff-picture'."
  (let* ((before (omnidiff-engine--parse-picture-info (gethash "before" ht)))
         (after (omnidiff-engine--parse-picture-info (gethash "after" ht)))
         (cmp (gethash "comparison" ht))
         (kind (and (hash-table-p cmp) (gethash "kind" cmp)))
         (comparison 'unknown)
         (raw-comparison nil)
         (changed-pct nil)
         (changed-pixels nil)
         (total-pixels nil)
         (region-count 0)
         (regions nil))
    (pcase kind
      ("one_sided"
       (setq comparison 'one-sided))
      ("pixels"
       (let* ((regs (omnidiff-engine--parse-picture-regions (gethash "regions" cmp)))
              (cpix (or (gethash "changed_pixels" cmp) 0))
              (tpix (or (gethash "total_pixels" cmp) 0)))
         (setq total-pixels tpix)
         (if (null regs)
             (setq comparison 'identical
                   changed-pct 0.0
                   changed-pixels 0
                   region-count 0
                   regions nil)
           (setq comparison 'pixels
                 changed-pixels cpix
                 changed-pct (* 100.0 (/ (float cpix) (max 1 tpix)))
                 region-count (length regs)
                 regions regs))))
      ("resized"
       (let ((scaled (gethash "scaled" cmp)))
         (if (hash-table-p scaled)
             (let* ((regs (omnidiff-engine--parse-picture-regions (gethash "regions" scaled)))
                    (cpix (or (gethash "changed_pixels" scaled) 0))
                    (tpix (or (gethash "total_pixels" scaled) 0)))
               (setq comparison 'resized-scaled
                     total-pixels tpix
                     changed-pixels (if (null regs) 0 cpix)
                     changed-pct (if (null regs)
                                     0.0
                                   (* 100.0 (/ (float cpix) (max 1 tpix))))
                     region-count (length regs)
                     regions regs))
           (setq comparison 'resized))))
      ("frames"
       (let ((changed 0)
             (added 0)
             (removed 0)
             (retimed (omnidiff-engine--json-bool (gethash "retimed" cmp)))
             (faint (omnidiff-engine--json-bool (gethash "faint" cmp))))
         (dolist (step (gethash "steps" cmp))
           (pcase (gethash "kind" step)
             ("changed" (setq changed (1+ changed)))
             ("inserted" (setq added (+ added (or (gethash "frames" step) 1))))
             ("deleted" (setq removed (+ removed (or (gethash "frames" step) 1))))))
         (setq comparison 'unknown
               total-pixels (gethash "total_pixels" cmp)
               raw-comparison (format "animation: %d changed, %d added, %d removed frames%s%s"
                                      changed added removed
                                      (if retimed " (retimed)" "")
                                      (if faint " (faint)" "")))))
      (_
       (setq comparison 'unknown
             raw-comparison (if (stringp kind) kind "unknown"))))
    (make-omnidiff-picture
     :before before
     :after after
     :comparison comparison
     :raw-comparison raw-comparison
     :changed-pct changed-pct
     :changed-pixels changed-pixels
     :total-pixels total-pixels
     :region-count region-count
     :regions regions
     :more-regions 0
     :svg-p nil)))

(defun omnidiff-engine--parse-container-side (ht)
  "Parse JSON `container_side' HT into `omnidiff-json-container-side', or nil."
  (when (hash-table-p ht)
    (make-omnidiff-json-container-side
     :format (or (gethash "format" ht) "")
     :bytes (or (gethash "bytes" ht) 0)
     :members (or (gethash "members" ht) 0))))

(defun omnidiff-engine--parse-container-family (str)
  "Parse JSON container `family' STR into a Lisp symbol."
  (pcase str
    ("archives" 'archives)
    ("cursors" 'cursors)
    ("fonts" 'fonts)
    ("catalogs" 'catalogs)
    ("documents" 'documents)
    ((pred stringp) (intern (string-replace "_" "-" str)))
    (_ 'archives)))

(defun omnidiff-engine--parse-member-status (str)
  "Parse JSON container member `status' STR into a Lisp symbol."
  (pcase str
    ("same" 'same)
    ("changed" 'changed)
    ("added" 'added)
    ("removed" 'removed)
    ((pred stringp) (intern (string-replace "_" "-" str)))
    (_ 'changed)))

(defun omnidiff-engine--parse-content (ht)
  "Parse tagged JSON `content' HT into a picture or container struct."
  (when (hash-table-p ht)
    (pcase (gethash "kind" ht)
      ("picture" (omnidiff-engine--parse-picture ht))
      ("container" (omnidiff-engine--parse-container ht))
      (_ nil))))

(defun omnidiff-engine--parse-container-member (ht)
  "Parse JSON container `member' HT into `omnidiff-json-container-member'."
  (let* ((raw-detail (gethash "detail" ht))
         (detail-kind nil)
         (detail nil))
    (when (hash-table-p raw-detail)
      (pcase (gethash "kind" raw-detail)
        ("picture"
         (setq detail-kind 'picture
               detail (omnidiff-engine--parse-picture raw-detail)))
        ("content"
         (setq detail-kind 'content
               detail (omnidiff-engine--parse-content (gethash "content" raw-detail))))
        ("text"
         (let ((ws (gethash "whitespace" raw-detail)))
           (setq detail-kind 'text
                 detail (list :before-lines (or (gethash "before_lines" raw-detail) 0)
                              :after-lines (or (gethash "after_lines" raw-detail) 0)
                              :removed (or (gethash "removed" raw-detail) 0)
                              :added (or (gethash "added" raw-detail) 0)
                              :whitespace (pcase ws
                                            ("trailing" 'trailing)
                                            ("anywhere" 'anywhere)
                                            (_ nil))
                              :words-kept (omnidiff-engine--json-bool
                                           (gethash "words_kept" raw-detail))))))
        ("binary"
         (setq detail-kind 'binary
               detail (list :before-bytes (or (gethash "before_bytes" raw-detail) 0)
                            :after-bytes (or (gethash "after_bytes" raw-detail) 0))))))
    (make-omnidiff-json-container-member
     :key (or (gethash "key" ht) "")
     :status (omnidiff-engine--parse-member-status (gethash "status" ht))
     :detail-kind detail-kind
     :detail detail)))

(defun omnidiff-engine--parse-container (ht)
  "Parse JSON `container' HT into an `omnidiff-json-container'."
  (make-omnidiff-json-container
   :family (omnidiff-engine--parse-container-family (gethash "family" ht))
   :before (omnidiff-engine--parse-container-side (gethash "before" ht))
   :after (omnidiff-engine--parse-container-side (gethash "after" ht))
   :unchanged (or (gethash "unchanged" ht) 0)
   :members (mapcar #'omnidiff-engine--parse-container-member
                    (gethash "members" ht))))

(defun omnidiff-engine-parse-json (json-str)
  "Parse `omnidiff --mode json' output string JSON-STR into `omnidiff-json-diff'."
  (let ((obj (json-parse-string json-str
                                :object-type 'hash-table
                                :array-type 'list
                                :null-object nil
                                :false-object :false)))
    (unless (hash-table-p obj)
      (error "Expected top-level JSON object from omnidiff"))
    (let* ((before (omnidiff-engine--parse-side (gethash "before" obj)))
           (after (omnidiff-engine--parse-side (gethash "after" obj)))
           (content-ht (gethash "content" obj))
           (picture-ht (gethash "picture" obj))
           (picture nil)
           (container nil))
      (when (hash-table-p content-ht)
        (pcase (gethash "kind" content-ht)
          ("picture" (setq picture (omnidiff-engine--parse-picture content-ht)))
          ("container" (setq container (omnidiff-engine--parse-container content-ht)))))
      (when (and (null picture) (hash-table-p picture-ht))
        (setq picture (omnidiff-engine--parse-picture picture-ht)))
      (make-omnidiff-json-diff
       :before before
       :after after
       :large-residual (omnidiff-engine--json-bool (gethash "large_residual" obj))
       :summary (omnidiff-engine--parse-summary (gethash "summary" obj))
       :binary-p (omnidiff-engine--json-bool (gethash "binary" obj))
       :picture picture
       :container container))))

;;; UTF-8 byte-offset <-> Emacs char & buffer position conversion

(defsubst omnidiff-engine--utf8-char-bytes (ch)
  "Return the UTF-8 byte length of Emacs character CH."
  (cond
   ((< ch #x80) 1)
   ((< ch #x800) 2)
   ((< ch #x10000) 3)
   ((<= ch #x10ffff) 4)
   ;; Raw eight-bit bytes (#x3fff80..#x3fffff) occupy 1 byte in source files.
   (t 1)))

(defsubst omnidiff-engine--unicode-ws-p (ch)
  "Return non-nil if CH has the Unicode White_Space property."
  (if (< ch 128)
      (or (eq ch ?\s) (and (>= ch ?\t) (<= ch ?\r)))
    (or (eq ch #x0085)
        (eq ch #x00a0)
        (eq ch #x1680)
        (and (>= ch #x2000) (<= ch #x200a))
        (eq ch #x2028)
        (eq ch #x2029)
        (eq ch #x202f)
        (eq ch #x205f)
        (eq ch #x3000))))

(defun omnidiff-engine-line-byte-to-char (line-str byte-col)
  "Convert 0-based UTF-8 BYTE-COL within LINE-STR to an Emacs character offset.
Uses an O(1) fast path for single-byte ASCII lines, clamps out-of-bounds
offsets to `[0, (length LINE-STR)]', and floors mid-character byte offsets
to the preceding character boundary."
  (let ((len (length line-str)))
    (cond
     ((or (<= byte-col 0) (= len 0)) 0)
     ((= len (string-bytes line-str))
      (min len byte-col))
     (t
      (let ((idx 0)
            (bytes 0)
            (done nil))
        (while (and (< idx len) (not done))
          (let ((next (+ bytes (omnidiff-engine--utf8-char-bytes (aref line-str idx)))))
            (if (> next byte-col)
                (setq done t)
              (setq bytes next
                    idx (1+ idx))
              (when (= bytes byte-col)
                (setq done t)))))
        idx)))))

(defun omnidiff-engine-paint-row-byte-len (line-str)
  "Return UTF-8 byte length of LINE-STR after stripping trailing whitespace.
Matches Rust `str::trim_end()' / `paint_row_len', including trailing `\\r'
and Unicode White_Space characters."
  (let* ((len (length line-str))
         (end len))
    (while (and (> end 0) (omnidiff-engine--unicode-ws-p (aref line-str (1- end))))
      (setq end (1- end)))
    (cond
     ((= end 0) 0)
     ((= len (string-bytes line-str)) end)
     (t
      (let ((bytes 0)
            (i 0))
        (while (< i end)
          (setq bytes (+ bytes (omnidiff-engine--utf8-char-bytes (aref line-str i)))
                i (1+ i)))
        bytes)))))

(defun omnidiff-engine--range-last-row (range num-rows)
  "Return inclusive 0-based last row covered by RANGE within NUM-ROWS, or nil."
  (let ((sr (omnidiff-json-range-start-row range))
        (sc (omnidiff-json-range-start-col range))
        (er (omnidiff-json-range-end-row range))
        (ec (omnidiff-json-range-end-col range)))
    (when (and (> num-rows 0)
               (>= sr 0)
               (< sr num-rows)
               (<= sr er)
               (or (> er sr) (> ec sc)))
      (let ((last (if (and (> er sr) (= ec 0))
                      (1- (min er num-rows))
                    (min er (1- num-rows)))))
        (when (>= last sr)
          last)))))

(defun omnidiff-engine-range-line-spans (range lines-vec)
  "Split RANGE over LINES-VEC into `(ROW START-CHAR END-CHAR)' tuples.
Handles EOF-normalized `(LINE-COUNT, 0)' ranges, clamps intermediate rows
to `omnidiff-engine-paint-row-byte-len', and preserves full end-of-line
columns on the final row so trailing-whitespace diffs stay visible."
  (let* ((num-rows (length lines-vec))
         (sr (omnidiff-json-range-start-row range))
         (sc (omnidiff-json-range-start-col range))
         (er (omnidiff-json-range-end-row range))
         (ec (omnidiff-json-range-end-col range))
         (last-row (omnidiff-engine--range-last-row range num-rows))
         (spans nil))
    (when last-row
      (let ((row sr))
        (while (<= row last-row)
          (let* ((line (aref lines-vec row))
                 (s-byte (if (= row sr) sc 0))
                 (e-byte (if (< row er)
                             (omnidiff-engine-paint-row-byte-len line)
                           ec))
                 (s-char (omnidiff-engine-line-byte-to-char line s-byte))
                 (e-char (omnidiff-engine-line-byte-to-char line e-byte)))
            (when (< s-char e-char)
              (push (list row s-char e-char) spans)))
          (setq row (1+ row)))))
    (nreverse spans)))

(defun omnidiff-engine-buffer-line-offsets (buf)
  "Return a vector of 0-based line-start buffer positions for BUF in O(N)."
  (with-current-buffer buf
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let ((starts (list (point))))
          (while (search-forward "\n" nil t)
            (push (point) starts))
          (vconcat (nreverse starts)))))))

(defun omnidiff-engine-range-to-buffer-regions (range line-starts buf)
  "Convert `omnidiff-json-range' RANGE to `(BEG . END)' regions in BUF.
LINE-STARTS must be a vector from `omnidiff-engine-buffer-line-offsets'."
  (let* ((num-rows (length line-starts))
         (sr (omnidiff-json-range-start-row range))
         (sc (omnidiff-json-range-start-col range))
         (er (omnidiff-json-range-end-row range))
         (ec (omnidiff-json-range-end-col range))
         (last-row (omnidiff-engine--range-last-row range num-rows))
         (regions nil))
    (when last-row
      (with-current-buffer buf
        (save-restriction
          (widen)
          (let ((pmin (point-min))
                (pmax (point-max))
                (row sr))
            (while (<= row last-row)
              (let ((raw-beg (aref line-starts row))
                    (raw-end (if (< (1+ row) num-rows)
                                 (1- (aref line-starts (1+ row)))
                               pmax)))
                (when (and (<= raw-beg pmax) (<= raw-beg raw-end))
                  (let ((row-beg (max pmin raw-beg))
                        (row-end (min pmax raw-end)))
                    (when (<= row-beg row-end)
                      (let* ((line (buffer-substring-no-properties row-beg row-end))
                             (s-byte (if (= row sr) sc 0))
                             (e-byte (if (< row er)
                                         (omnidiff-engine-paint-row-byte-len line)
                                       ec))
                             (s-char (omnidiff-engine-line-byte-to-char line s-byte))
                             (e-char (omnidiff-engine-line-byte-to-char line e-byte)))
                        (when (< s-char e-char)
                          (push (cons (+ row-beg s-char) (+ row-beg e-char)) regions)))))))
              (setq row (1+ row)))))))
    (nreverse regions)))

;;; Line alignment & change navigation model

(defun omnidiff-engine--patience-lis (pairs)
  "Return the longest increasing subsequence of PAIRS by `cdr' in O(K log K)."
  (let ((k (length pairs)))
    (if (<= k 1)
        pairs
      (let ((pvec (vconcat pairs))
            (tails-ai (make-vector k 0))
            (tails-idx (make-vector k 0))
            (prev (make-vector k -1))
            (size 0))
        (dotimes (i k)
          (let ((ai (cdr (aref pvec i)))
                (lo 0)
                (hi size))
            (while (< lo hi)
              (let ((mid (/ (+ lo hi) 2)))
                (if (< (aref tails-ai mid) ai)
                    (setq lo (1+ mid))
                  (setq hi mid))))
            (aset prev i (if (> lo 0) (aref tails-idx (1- lo)) -1))
            (aset tails-ai lo ai)
            (aset tails-idx lo i)
            (when (= lo size)
              (setq size (1+ size)))))
        (let ((res nil)
              (cur (aref tails-idx (1- size))))
          (while (>= cur 0)
            (push (aref pvec cur) res)
            (setq cur (aref prev cur)))
          res)))))

(defun omnidiff-engine--lcs-gap (bvec b-rows bi0 bi1 avec a-rows ai0 ai1)
  "Compute exact LCS matching pairs between B-ROWS[BI0..BI1) and A-ROWS[AI0..AI1)."
  (let* ((blen (- bi1 bi0))
         (alen (- ai1 ai0))
         (stride (1+ alen))
         (dp (make-vector (* (1+ blen) stride) 0)))
    (let ((i (1- blen)))
      (while (>= i 0)
        (let ((bline (aref bvec (aref b-rows (+ bi0 i))))
              (row-off (* i stride))
              (next-off (* (1+ i) stride))
              (j (1- alen)))
          (while (>= j 0)
            (aset dp (+ row-off j)
                  (if (equal bline (aref avec (aref a-rows (+ ai0 j))))
                      (1+ (aref dp (+ next-off j 1)))
                    (max (aref dp (+ next-off j))
                         (aref dp (+ row-off j 1)))))
            (setq j (1- j))))
        (setq i (1- i))))
    (let ((i 0)
          (j 0)
          (res nil))
      (while (and (< i blen) (< j alen))
        (let ((br (aref b-rows (+ bi0 i)))
              (ar (aref a-rows (+ ai0 j))))
          (cond
           ((equal (aref bvec br) (aref avec ar))
            (push (cons br ar) res)
            (setq i (1+ i)
                  j (1+ j)))
           ((>= (aref dp (+ (* (1+ i) stride) j))
                (aref dp (+ (* i stride) (1+ j))))
            (setq i (1+ i)))
           (t
            (setq j (1+ j))))))
      (nreverse res))))

(defun omnidiff-engine--greedy-gap (bvec b-rows bi0 bi1 avec a-rows ai0 ai1)
  "Greedily match exact equal lines across a large non-unique gap in O(N)."
  (let* ((alen (- ai1 ai0))
         (window (max 64 (/ alen 4)))
         (positions (make-hash-table :test #'equal :size (max 16 alen)))
         (res nil))
    (let ((ai (1- ai1)))
      (while (>= ai ai0)
        (let ((line (aref avec (aref a-rows ai))))
          (puthash line (cons ai (gethash line positions)) positions))
        (setq ai (1- ai))))
    (let ((bi bi0)
          (next-ai ai0))
      (while (and (< bi bi1) (< next-ai ai1))
        (let* ((br (aref b-rows bi))
               (line (aref bvec br))
               (cands (gethash line positions)))
          (while (and cands (< (car cands) next-ai))
            (setq cands (cdr cands)))
          (puthash line cands positions)
          (when (and cands (<= (- (car cands) next-ai) window))
            (let ((matched-ai (car cands)))
              (push (cons br (aref a-rows matched-ai)) res)
              (puthash line (cdr cands) positions)
              (setq next-ai (1+ matched-ai)))))
        (setq bi (1+ bi))))
    (nreverse res)))

(defun omnidiff-engine--match-unchanged-anchors (bvec b-rows avec a-rows)
  "Compute strictly increasing `(BEFORE . AFTER)' anchors via Patience+LCS."
  (let ((anchors nil)
        (stack (list (list 0 (length b-rows) 0 (length a-rows)))))
    (while stack
      (pcase-let ((`(,bi0 ,bi1 ,ai0 ,ai1) (pop stack)))
        ;; Trim matching prefix.
        (while (and (< bi0 bi1)
                    (< ai0 ai1)
                    (equal (aref bvec (aref b-rows bi0))
                           (aref avec (aref a-rows ai0))))
          (push (cons (aref b-rows bi0) (aref a-rows ai0)) anchors)
          (setq bi0 (1+ bi0)
                ai0 (1+ ai0)))
        ;; Trim matching suffix.
        (while (and (< bi0 bi1)
                    (< ai0 ai1)
                    (equal (aref bvec (aref b-rows (1- bi1)))
                           (aref avec (aref a-rows (1- ai1)))))
          (setq bi1 (1- bi1)
                ai1 (1- ai1))
          (push (cons (aref b-rows bi1) (aref a-rows ai1)) anchors))
        (when (and (< bi0 bi1) (< ai0 ai1))
          ;; Patience: find lines unique to both slices, then take LIS.
          (let* ((blen (- bi1 bi0))
                 (alen (- ai1 ai0))
                 (b-pos (make-hash-table :test #'equal :size (max 16 blen)))
                 (a-pos (make-hash-table :test #'equal :size (max 16 alen)))
                 (unique-pairs nil))
            (let ((bi bi0))
              (while (< bi bi1)
                (let* ((line (aref bvec (aref b-rows bi)))
                       (seen (gethash line b-pos)))
                  (puthash line (if seen :dup bi) b-pos))
                (setq bi (1+ bi))))
            (let ((ai ai0))
              (while (< ai ai1)
                (let* ((line (aref avec (aref a-rows ai)))
                       (seen (gethash line a-pos)))
                  (puthash line (if seen :dup ai) a-pos))
                (setq ai (1+ ai))))
            (let ((bi bi0))
              (while (< bi bi1)
                (let* ((line (aref bvec (aref b-rows bi)))
                       (b-idx (gethash line b-pos))
                       (a-idx (and (integerp b-idx) (gethash line a-pos))))
                  (when (integerp a-idx)
                    (push (cons bi a-idx) unique-pairs)))
                (setq bi (1+ bi))))
            (let ((lis (omnidiff-engine--patience-lis (nreverse unique-pairs))))
              (if lis
                  (let ((prev-bi bi0)
                        (prev-ai ai0))
                    (dolist (pair lis)
                      (let ((cur-bi (car pair))
                            (cur-ai (cdr pair)))
                        (push (cons (aref b-rows cur-bi) (aref a-rows cur-ai)) anchors)
                        (when (and (< prev-bi cur-bi) (< prev-ai cur-ai))
                          (push (list prev-bi cur-bi prev-ai cur-ai) stack))
                        (setq prev-bi (1+ cur-bi)
                              prev-ai (1+ cur-ai))))
                    (when (and (< prev-bi bi1) (< prev-ai ai1))
                      (push (list prev-bi bi1 prev-ai ai1) stack)))
                ;; No unique lines in slice: bounded exact LCS or O(N) greedy fallback.
                (let ((matched (if (<= (* blen alen) 250000)
                                   (omnidiff-engine--lcs-gap
                                    bvec b-rows bi0 bi1 avec a-rows ai0 ai1)
                                 (omnidiff-engine--greedy-gap
                                  bvec b-rows bi0 bi1 avec a-rows ai0 ai1))))
                  (dolist (m matched)
                    (push m anchors)))))))))
    (sort anchors (lambda (x y) (< (car x) (car y))))))

(defun omnidiff-engine--mark-hunk-rows (hunks num-rows)
  "Return boolean vector marking rows touched by any hunk in HUNKS."
  (let ((marked (make-vector num-rows nil)))
    (dolist (h hunks)
      (when-let* ((last (omnidiff-engine--range-last-row
                         (omnidiff-json-hunk-range h) num-rows)))
        (let ((r (omnidiff-json-range-start-row (omnidiff-json-hunk-range h))))
          (while (<= r last)
            (aset marked r t)
            (setq r (1+ r))))))
    marked))

(defun omnidiff-engine--record-move-mappings (hunks src-len dst-len src-to-dst dst-to-src)
  "Populate SRC-TO-DST and DST-TO-SRC with `move-target' row mappings for HUNKS.
Never overwrites already populated unchanged anchor slots."
  (dolist (h hunks)
    (when (and (eq (omnidiff-json-hunk-operation h) 'move)
               (omnidiff-json-hunk-move-target h))
      (let* ((srng (omnidiff-json-hunk-range h))
             (drng (omnidiff-json-hunk-move-target h))
             (s0 (omnidiff-json-range-start-row srng))
             (s1 (omnidiff-engine--range-last-row srng src-len))
             (d0 (omnidiff-json-range-start-row drng))
             (d1 (omnidiff-engine--range-last-row drng dst-len)))
        (when (and s1 d1)
          (let ((s-span (- s1 s0))
                (d-span (- d1 d0))
                (r s0))
            (while (<= r s1)
              (let ((mapped (if (= s-span 0)
                                d0
                              (min d1 (+ d0 (round (* (float (- r s0)) d-span) s-span))))))
                (when (null (aref src-to-dst r))
                  (aset src-to-dst r mapped))
                (when (null (aref dst-to-src mapped))
                  (aset dst-to-src mapped r)))
              (setq r (1+ r)))))))))

(defun omnidiff-engine--collect-move-pairs (b-hunks a-hunks nb na)
  "Collect deduplicated `(BEFORE . AFTER)' move-target pairs sorted by `car'."
  (let ((by-b (make-hash-table :test #'eql)))
    (cl-flet ((add-pair (b a)
                (when (and (>= b 0) (< b nb) (>= a 0) (< a na)
                           (null (gethash b by-b)))
                  (puthash b a by-b))))
      (dolist (h b-hunks)
        (when (and (eq (omnidiff-json-hunk-operation h) 'move)
                   (omnidiff-json-hunk-move-target h))
          (let* ((srng (omnidiff-json-hunk-range h))
                 (drng (omnidiff-json-hunk-move-target h))
                 (s0 (omnidiff-json-range-start-row srng))
                 (s1 (omnidiff-engine--range-last-row srng nb))
                 (d0 (omnidiff-json-range-start-row drng))
                 (d1 (omnidiff-engine--range-last-row drng na)))
            (when (and s1 d1)
              (let ((s-span (- s1 s0))
                    (d-span (- d1 d0))
                    (r s0))
                (while (<= r s1)
                  (let ((mapped (if (= s-span 0)
                                    d0
                                  (min d1 (+ d0 (round (* (float (- r s0)) d-span) s-span))))))
                    (add-pair r mapped))
                  (setq r (1+ r))))))))
      (dolist (h a-hunks)
        (when (and (eq (omnidiff-json-hunk-operation h) 'move)
                   (omnidiff-json-hunk-move-target h))
          (let* ((srng (omnidiff-json-hunk-range h))
                 (drng (omnidiff-json-hunk-move-target h))
                 (a0 (omnidiff-json-range-start-row srng))
                 (a1 (omnidiff-engine--range-last-row srng na))
                 (b0 (omnidiff-json-range-start-row drng))
                 (b1 (omnidiff-engine--range-last-row drng nb)))
            (when (and a1 b1)
              (let ((a-span (- a1 a0))
                    (b-span (- b1 b0))
                    (r a0))
                (while (<= r a1)
                  (let ((mapped (if (= a-span 0)
                                    b0
                                  (min b1 (+ b0 (round (* (float (- r a0)) b-span) a-span))))))
                    (add-pair mapped r))
                  (setq r (1+ r)))))))))
    (let ((pairs nil))
      (maphash (lambda (b a) (push (cons b a) pairs)) by-b)
      (sort pairs (lambda (x y) (< (car x) (car y)))))))

(defun omnidiff-engine--insert-monotonic-move-anchors (anchors move-pairs nb na)
  "Insert monotonic MOVE-PAIRS into gaps of ANCHORS while keeping strict order."
  (if (null move-pairs)
      anchors
    (let ((extra nil)
          (cur-b -1)
          (cur-a -1)
          (rem move-pairs))
      (dolist (bound (append anchors (list (cons nb na))))
        (let ((next-b (car bound))
              (next-a (cdr bound))
              (cands nil))
          (while (and rem (<= (caar rem) cur-b))
            (setq rem (cdr rem)))
          (let ((scan rem))
            (while (and scan (< (caar scan) next-b))
              (let ((a (cdar scan)))
                (when (and (> a cur-a) (< a next-a))
                  (push (car scan) cands)))
              (setq scan (cdr scan)))
            (setq rem scan))
          (when cands
            (dolist (p (omnidiff-engine--patience-lis (nreverse cands)))
              (push p extra)))
          (setq cur-b next-b
                cur-a next-a)))
      (if extra
          (sort (nconc (copy-sequence anchors) extra)
                (lambda (x y) (< (car x) (car y))))
        anchors))))

(defun omnidiff-engine--row-skeleton (line spans)
  "Return trimmed LINE with painted char SPANS `(START . END)' removed."
  (if (null spans)
      (string-trim line)
    (let ((len (length line))
          (pos 0)
          (parts nil))
      (dolist (sp spans)
        (let ((s (max pos (car sp)))
              (e (min len (cdr sp))))
          (when (> s pos)
            (push (substring line pos s) parts))
          (when (> e pos)
            (setq pos e))))
      (when (< pos len)
        (push (substring line pos) parts))
      (string-trim (apply #'concat (nreverse parts))))))

(defun omnidiff-engine--build-row-non-move-spans (hunks lines-vec)
  "Map 0-based rows to sorted non-move `(START . END)' char spans."
  (let ((table (make-hash-table :test #'eql)))
    (dolist (h hunks)
      (unless (eq (omnidiff-json-hunk-operation h) 'move)
        (dolist (seg (omnidiff-engine-range-line-spans
                      (omnidiff-json-hunk-range h) lines-vec))
          (pcase-let ((`(,row ,sc ,ec) seg))
            (puthash row (cons (cons sc ec) (gethash row table)) table)))))
    (maphash (lambda (row spans)
               (puthash row (sort spans (lambda (x y) (< (car x) (car y)))) table))
             table)
    table))

(defun omnidiff-engine--refine-gaps-with-skeletons (anchors bvec avec b-hunks a-hunks)
  "Match partially edited lines with identical unpainted skeletons inside gaps."
  (let* ((nb (length bvec))
         (na (length avec))
         (b-spans nil)
         (a-spans nil)
         (b-skels nil)
         (a-skels nil)
         (extra nil)
         (cur-b -1)
         (cur-a -1))
    (dolist (bound (append anchors (list (cons nb na))))
      (let ((next-b (car bound))
            (next-a (cdr bound)))
        (when (and (> (- next-b cur-b) 1) (> (- next-a cur-a) 1))
          (unless b-spans
            (setq b-spans (omnidiff-engine--build-row-non-move-spans b-hunks bvec)
                  a-spans (omnidiff-engine--build-row-non-move-spans a-hunks avec)
                  b-skels (make-vector nb nil)
                  a-skels (make-vector na nil)))
          (let ((b-rows nil)
                (a-rows nil))
            (let ((r (1+ cur-b)))
              (while (< r next-b)
                (let ((skel (omnidiff-engine--row-skeleton
                             (aref bvec r) (gethash r b-spans))))
                  (when (>= (length skel) 2)
                    (aset b-skels r skel)
                    (push r b-rows)))
                (setq r (1+ r))))
            (let ((r (1+ cur-a)))
              (while (< r next-a)
                (let ((skel (omnidiff-engine--row-skeleton
                             (aref avec r) (gethash r a-spans))))
                  (when (>= (length skel) 2)
                    (aset a-skels r skel)
                    (push r a-rows)))
                (setq r (1+ r))))
            (when (and b-rows a-rows)
              (dolist (m (omnidiff-engine--match-unchanged-anchors
                          b-skels (vconcat (nreverse b-rows))
                          a-skels (vconcat (nreverse a-rows))))
                (push m extra)))))
        (setq cur-b next-b
              cur-a next-a)))
    (if extra
        (sort (nconc (copy-sequence anchors) extra)
              (lambda (x y) (< (car x) (car y))))
      anchors)))

(defun omnidiff-engine--normalize-lines-vec (lines)
  "Convert LINES (vector or list of strings) to a vector without trailing `\\r'."
  (let* ((src (if (vectorp lines) lines (vconcat lines)))
         (n (length src))
         (out nil))
    (dotimes (i n)
      (let* ((s (aref src i))
             (len (length s)))
        (when (and (> len 0) (= (aref s (1- len)) ?\r))
          (unless out
            (setq out (copy-sequence src)))
          (aset out i (substring s 0 (1- len))))))
    (or out src)))

(defun omnidiff-engine-compute-alignment (before-lines after-lines json-diff)
  "Compute an `omnidiff-line-alignment' between BEFORE-LINES and AFTER-LINES.
JSON-DIFF is an `omnidiff-json-diff' identifying changed and moved ranges.
BEFORE-LINES and AFTER-LINES may be vectors or lists of 0-based line strings."
  (let* ((bvec (omnidiff-engine--normalize-lines-vec before-lines))
         (avec (omnidiff-engine--normalize-lines-vec after-lines))
         (nb (length bvec))
         (na (length avec))
         (b2a (make-vector nb nil))
         (a2b (make-vector na nil))
         (b-hunks (omnidiff-json-side-hunks (omnidiff-json-diff-before json-diff)))
         (a-hunks (omnidiff-json-side-hunks (omnidiff-json-diff-after json-diff)))
         (anchors nil))
    (cond
     ((or (= nb 0) (= na 0))
      nil)
     ;; Fast path: no hunks at all and equal lengths.
     ((and (null b-hunks) (null a-hunks) (= nb na))
      (let ((acc nil))
        (dotimes (i nb)
          (when (equal (aref bvec i) (aref avec i))
            (push (cons i i) acc)))
        (setq anchors (nreverse acc))))
     ;; Large-file cap: fast O(N) common unchanged prefix/suffix + linear interpolation.
     ((> (max nb na) omnidiff-alignment-max-lines)
      (let* ((b-marked (omnidiff-engine--mark-hunk-rows b-hunks nb))
             (a-marked (omnidiff-engine--mark-hunk-rows a-hunks na))
             (pre nil)
             (suf nil)
             (bi 0)
             (ai 0)
             (bj (1- nb))
             (aj (1- na)))
        (while (and (< bi nb) (< ai na)
                    (not (aref b-marked bi))
                    (not (aref a-marked ai))
                    (equal (aref bvec bi) (aref avec ai)))
          (push (cons bi ai) pre)
          (setq bi (1+ bi)
                ai (1+ ai)))
        (while (and (>= bj bi) (>= aj ai)
                    (not (aref b-marked bj))
                    (not (aref a-marked aj))
                    (equal (aref bvec bj) (aref avec aj)))
          (push (cons bj aj) suf)
          (setq bj (1- bj)
                aj (1- aj)))
        (setq anchors (nconc (nreverse pre) suf))))
     ;; Standard files: Patience unique anchors + bounded LCS over hunk-free rows.
     (t
      (let* ((b-marked (omnidiff-engine--mark-hunk-rows b-hunks nb))
             (a-marked (omnidiff-engine--mark-hunk-rows a-hunks na))
             (b-clean-list nil)
             (a-clean-list nil))
        (dotimes (i nb)
          (unless (aref b-marked i)
            (push i b-clean-list)))
        (dotimes (i na)
          (unless (aref a-marked i)
            (push i a-clean-list)))
        (let ((b-rows (vconcat (nreverse b-clean-list)))
              (a-rows (vconcat (nreverse a-clean-list)))
              (move-pairs (omnidiff-engine--collect-move-pairs b-hunks a-hunks nb na)))
          (when (and (> (length b-rows) 0) (> (length a-rows) 0))
            (setq anchors (omnidiff-engine--match-unchanged-anchors
                           bvec b-rows avec a-rows)))
          (setq anchors (omnidiff-engine--insert-monotonic-move-anchors
                         anchors move-pairs nb na))
          (setq anchors (omnidiff-engine--refine-gaps-with-skeletons
                         anchors bvec avec b-hunks a-hunks))))))
    ;; Populate exact unchanged anchors first.
    (dolist (pair anchors)
      (aset b2a (car pair) (cdr pair))
      (aset a2b (cdr pair) (car pair)))
    ;; Overlay exact move_target row mappings for moved lines without disturbing anchors.
    (omnidiff-engine--record-move-mappings b-hunks nb na b2a a2b)
    (omnidiff-engine--record-move-mappings a-hunks na nb a2b b2a)
    (make-omnidiff-line-alignment
     :before-to-after b2a
     :after-to-before a2b
     :anchors anchors
     :anchor-vec (vconcat anchors))))

(defun omnidiff-engine--ensure-anchor-vec (alignment)
  "Return vector of anchors for ALIGNMENT, lazily caching it if needed."
  (or (omnidiff-line-alignment-anchor-vec alignment)
      (setf (omnidiff-line-alignment-anchor-vec alignment)
            (vconcat (omnidiff-line-alignment-anchors alignment)))))

(defun omnidiff-engine-map-row (alignment side row)
  "Map 0-based ROW on SIDE (`before' or `after') to its counterpart 0-based row.
Returns the exact mapped row from ALIGNMENT when available, otherwise
linearly interpolates between the nearest surrounding monotonic anchors."
  (let* ((before-p (eq side 'before))
         (vec (if before-p
                  (omnidiff-line-alignment-before-to-after alignment)
                (omnidiff-line-alignment-after-to-before alignment)))
         (other-vec (if before-p
                        (omnidiff-line-alignment-after-to-before alignment)
                      (omnidiff-line-alignment-before-to-after alignment)))
         (src-len (length vec))
         (dst-len (length other-vec)))
    (if (<= dst-len 0)
        0
      (let* ((max-src (max 0 (1- src-len)))
             (max-dst (1- dst-len))
             (clamped (max 0 (min row max-src)))
             (direct (and (< clamped src-len) (aref vec clamped))))
        (if (integerp direct)
            (max 0 (min max-dst direct))
          (let* ((avec (omnidiff-engine--ensure-anchor-vec alignment))
                 (alen (length avec)))
            (if (= alen 0)
                (if (<= max-src 0)
                    0
                  (max 0 (min max-dst (round (* (float clamped) max-dst) max-src))))
              ;; Binary search for first anchor with src-row > clamped.
              (let ((lo 0)
                    (hi alen))
                (while (< lo hi)
                  (let* ((mid (/ (+ lo hi) 2))
                         (pair (aref avec mid))
                         (s (if before-p (car pair) (cdr pair))))
                    (if (<= s clamped)
                        (setq lo (1+ mid))
                      (setq hi mid))))
                (let* ((prev (when (> lo 0) (aref avec (1- lo))))
                       (next (when (< lo alen) (aref avec lo)))
                       (s-lo (if prev (1+ (if before-p (car prev) (cdr prev))) 0))
                       (d-lo (if prev (1+ (if before-p (cdr prev) (car prev))) 0))
                       (s-hi (if next (1- (if before-p (car next) (cdr next))) max-src))
                       (d-hi (if next (1- (if before-p (cdr next) (car next))) max-dst)))
                  (cond
                   ((> d-lo d-hi)
                    ;; Pure deletion/insertion gap with zero destination rows.
                    (max 0 (min max-dst (if next
                                            (if before-p (cdr next) (car next))
                                          (if prev (if before-p (cdr prev) (car prev)) 0)))))
                   ((<= s-hi s-lo)
                    (max 0 (min max-dst d-lo)))
                   (t
                    (let ((interp (+ d-lo (round (* (float (- clamped s-lo))
                                                    (- d-hi d-lo))
                                                 (- s-hi s-lo)))))
                      (max d-lo (min d-hi (max 0 (min max-dst interp))))))))))))))))

(defun omnidiff-engine--pos-le-p (r1 c1 r2 c2)
  "Return non-nil if (R1, C1) <= (R2, C2)."
  (or (< r1 r2) (and (= r1 r2) (<= c1 c2))))

(defun omnidiff-engine--pos-lt-p (r1 c1 r2 c2)
  "Return non-nil if (R1, C1) < (R2, C2)."
  (or (< r1 r2) (and (= r1 r2) (< c1 c2))))

(defun omnidiff-engine--range-contains-p (range row byte-col)
  "Return non-nil if right-open RANGE contains (ROW, BYTE-COL)."
  (and (omnidiff-engine--pos-le-p
        (omnidiff-json-range-start-row range)
        (omnidiff-json-range-start-col range)
        row byte-col)
       (omnidiff-engine--pos-lt-p
        row byte-col
        (omnidiff-json-range-end-row range)
        (omnidiff-json-range-end-col range))))

(defun omnidiff-engine--range-effective-last-row (range)
  "Return inclusive 0-based last row of RANGE without needing file length."
  (let ((sr (omnidiff-json-range-start-row range))
        (er (omnidiff-json-range-end-row range))
        (ec (omnidiff-json-range-end-col range)))
    (if (and (> er sr) (= ec 0))
        (1- er)
      er)))

(defun omnidiff-engine--hunk-at (hunks row byte-col)
  "Return the most specific hunk in HUNKS containing (ROW, BYTE-COL)."
  (let ((best nil)
        (best-span most-positive-fixnum))
    (dolist (h hunks)
      (let ((r (omnidiff-json-hunk-range h)))
        (when (omnidiff-engine--range-contains-p r row byte-col)
          (let ((span (+ (* 100000 (- (omnidiff-json-range-end-row r)
                                      (omnidiff-json-range-start-row r)))
                         (abs (- (omnidiff-json-range-end-col r)
                                 (omnidiff-json-range-start-col r))))))
            (when (< span best-span)
              (setq best h
                    best-span span))))))
    best))

(defun omnidiff-engine--op-counterpart-priority (src-op dst-op)
  "Return numeric preference (lower is better) for pairing SRC-OP with DST-OP."
  (pcase (cons src-op dst-op)
    ('(update . update) 0)
    ('(delete . insert) 0)
    ('(insert . delete) 0)
    ('(update . insert) 1)
    ('(update . delete) 1)
    ('(delete . update) 1)
    ('(insert . update) 1)
    ('(move . move) 0)
    ((pred (lambda (p) (eq (cdr p) 'move))) 3)
    (_ 2)))

(defun omnidiff-engine-counterpart-at (json-diff alignment side row byte-col)
  "Return `(OTHER-SIDE . COUNTERPART-RANGE)' for (ROW, BYTE-COL) on SIDE.
JSON-DIFF is an `omnidiff-json-diff' and ALIGNMENT an `omnidiff-line-alignment'.
SIDE is `before' or `after', ROW is 0-based, and BYTE-COL is a 0-based UTF-8
byte column (nil defaults to 0)."
  (let* ((col (or byte-col 0))
         (before-p (eq side 'before))
         (other-side (if before-p 'after 'before))
         (src-hunks (omnidiff-json-side-hunks
                     (if before-p
                         (omnidiff-json-diff-before json-diff)
                       (omnidiff-json-diff-after json-diff))))
         (dst-hunks (omnidiff-json-side-hunks
                     (if before-p
                         (omnidiff-json-diff-after json-diff)
                       (omnidiff-json-diff-before json-diff))))
         (hunk (omnidiff-engine--hunk-at src-hunks row col)))
    (cond
     ;; Exact move target.
     ((and hunk
           (eq (omnidiff-json-hunk-operation hunk) 'move)
           (omnidiff-json-hunk-move-target hunk))
      (cons other-side (omnidiff-json-hunk-move-target hunk)))
     ;; Non-move hunk (or move without target): locate opposing hunk on mapped row(s).
     (hunk
      (let* ((src-op (omnidiff-json-hunk-operation hunk))
             (srng (omnidiff-json-hunk-range hunk))
             (s0 (omnidiff-json-range-start-row srng))
             (s1 (omnidiff-engine--range-effective-last-row srng))
             (dst-vec (if before-p
                          (omnidiff-line-alignment-after-to-before alignment)
                        (omnidiff-line-alignment-before-to-after alignment)))
             (dst-len (length dst-vec))
             (mapped-row (omnidiff-engine-map-row alignment side row))
             (m-start (omnidiff-engine-map-row alignment side s0))
             (m-end (omnidiff-engine-map-row alignment side s1))
             (lo (min mapped-row m-start m-end))
             (hi (max mapped-row m-start m-end))
             (best nil)
             (best-prio 99)
             (best-rdist most-positive-fixnum)
             (best-cdist most-positive-fixnum))
        (dolist (dh dst-hunks)
          (let* ((dr (omnidiff-json-hunk-range dh))
                 (d0 (omnidiff-json-range-start-row dr))
                 (d1 (omnidiff-engine--range-effective-last-row dr)))
            (when (and (<= d0 hi) (<= lo d1))
              ;; Reject opposing hunks whose overlapped rows are all anchored
              ;; to source rows outside `srng' (e.g. zero-width gap clamping).
              (let ((ov-r (max lo d0))
                    (ov-hi (min hi d1))
                    (compatible nil))
                (while (and (<= ov-r ov-hi) (not compatible))
                  (let ((back (when (and (>= ov-r 0) (< ov-r dst-len))
                                (aref dst-vec ov-r))))
                    (when (or (null back) (and (<= s0 back) (<= back s1)))
                      (setq compatible t)))
                  (setq ov-r (1+ ov-r)))
                (when compatible
                  (let* ((prio (omnidiff-engine--op-counterpart-priority
                                src-op (omnidiff-json-hunk-operation dh)))
                         (rdist (cond
                                 ((< mapped-row d0) (- d0 mapped-row))
                                 ((> mapped-row d1) (- mapped-row d1))
                                 (t 0)))
                         (cdist (abs (- (omnidiff-json-range-start-col dr) col))))
                    (when (and (< prio 3)
                               (or (< prio best-prio)
                                   (and (= prio best-prio) (< rdist best-rdist))
                                   (and (= prio best-prio) (= rdist best-rdist)
                                        (< cdist best-cdist))))
                      (setq best dh
                            best-prio prio
                            best-rdist rdist
                            best-cdist cdist))))))))
        (cons other-side
              (if best
                  (omnidiff-json-hunk-range best)
                (make-omnidiff-json-range
                 :start-row mapped-row :start-col 0
                 :end-row mapped-row :end-col 0)))))
     ;; Unchanged text: map line directly.
     (t
      (let ((mapped-row (omnidiff-engine-map-row alignment side row)))
        (cons other-side
              (make-omnidiff-json-range
               :start-row mapped-row :start-col 0
               :end-row (1+ mapped-row) :end-col 0)))))))

(cl-defstruct (omnidiff-engine--hunk-group (:copier nil))
  "Internal coalesced hunk cluster on one side for change-stop construction."
  (operation 'update :type symbol)
  (range nil)
  (move-target nil)
  (reference-line nil)
  (start-row 0 :type integer)
  (last-row 0 :type integer))

(defun omnidiff-engine--union-ranges (r1 r2)
  "Return the minimal `omnidiff-json-range' spanning both R1 and R2."
  (let* ((r1-sr (omnidiff-json-range-start-row r1))
         (r1-sc (omnidiff-json-range-start-col r1))
         (r2-sr (omnidiff-json-range-start-row r2))
         (r2-sc (omnidiff-json-range-start-col r2))
         (r1-er (omnidiff-json-range-end-row r1))
         (r1-ec (omnidiff-json-range-end-col r1))
         (r2-er (omnidiff-json-range-end-row r2))
         (r2-ec (omnidiff-json-range-end-col r2))
         (s-first (omnidiff-engine--pos-le-p r1-sr r1-sc r2-sr r2-sc))
         (e-second (omnidiff-engine--pos-le-p r1-er r1-ec r2-er r2-ec)))
    (make-omnidiff-json-range
     :start-row (if s-first r1-sr r2-sr)
     :start-col (if s-first r1-sc r2-sc)
     :end-row (if e-second r2-er r1-er)
     :end-col (if e-second r2-ec r1-ec))))

(defun omnidiff-engine--coalesce-side-hunks (hunks anchored-vec)
  "Coalesce sorted HUNKS into `(NON-MOVE-GROUPS . MOVE-GROUPS)'.
Non-move hunks on the same row always merge; consecutive rows of the same
operation merge when neither row is pinned in ANCHORED-VEC.  Move hunks
merge only when both source rows and `move-target' rows are contiguous."
  (let ((non-moves nil)
        (moves nil))
    (dolist (h hunks)
      (let* ((op (omnidiff-json-hunk-operation h))
             (rng (omnidiff-json-hunk-range h))
             (sr (omnidiff-json-range-start-row rng))
             (lr (omnidiff-engine--range-effective-last-row rng))
             (ref (omnidiff-json-hunk-reference-line h)))
        (if (eq op 'move)
            (let* ((mt (omnidiff-json-hunk-move-target h))
                   (prev (car moves))
                   (prev-mt (and prev (omnidiff-engine--hunk-group-move-target prev))))
              (if (and prev
                       (<= sr (1+ (omnidiff-engine--hunk-group-last-row prev)))
                       (if (and mt prev-mt)
                           (let ((mt-sr (omnidiff-json-range-start-row mt))
                                 (pm-sr (omnidiff-json-range-start-row prev-mt))
                                 (pm-lr (omnidiff-engine--range-effective-last-row prev-mt)))
                             (and (>= mt-sr pm-sr) (<= mt-sr (1+ pm-lr))))
                         (and (null mt) (null prev-mt))))
                  (progn
                    (setf (omnidiff-engine--hunk-group-range prev)
                          (omnidiff-engine--union-ranges
                           (omnidiff-engine--hunk-group-range prev) rng)
                          (omnidiff-engine--hunk-group-last-row prev)
                          (max (omnidiff-engine--hunk-group-last-row prev) lr))
                    (when (and mt prev-mt)
                      (setf (omnidiff-engine--hunk-group-move-target prev)
                            (omnidiff-engine--union-ranges prev-mt mt)))
                    (unless (omnidiff-engine--hunk-group-reference-line prev)
                      (setf (omnidiff-engine--hunk-group-reference-line prev) ref)))
                (push (make-omnidiff-engine--hunk-group
                       :operation 'move
                       :range rng
                       :move-target mt
                       :reference-line ref
                       :start-row sr
                       :last-row lr)
                      moves)))
          (let* ((prev (car non-moves))
                 (prev-lr (and prev (omnidiff-engine--hunk-group-last-row prev)))
                 (alen (length anchored-vec)))
            (if (and prev
                     (or (<= sr prev-lr)
                         (and (= sr (1+ prev-lr))
                              (eq op (omnidiff-engine--hunk-group-operation prev))
                              (not (and (< prev-lr alen) (aref anchored-vec prev-lr)))
                              (not (and (< sr alen) (aref anchored-vec sr))))))
                (progn
                  (when (eq op 'update)
                    (setf (omnidiff-engine--hunk-group-operation prev) 'update))
                  (setf (omnidiff-engine--hunk-group-range prev)
                        (omnidiff-engine--union-ranges
                         (omnidiff-engine--hunk-group-range prev) rng)
                        (omnidiff-engine--hunk-group-last-row prev)
                        (max (omnidiff-engine--hunk-group-last-row prev) lr))
                  (unless (omnidiff-engine--hunk-group-reference-line prev)
                    (setf (omnidiff-engine--hunk-group-reference-line prev) ref)))
              (push (make-omnidiff-engine--hunk-group
                     :operation op
                     :range rng
                     :reference-line ref
                     :start-row sr
                     :last-row lr)
                    non-moves))))))
    (cons (nreverse non-moves) (nreverse moves))))

(defun omnidiff-engine--rows-overlap-p (a0 a1 b0 b1)
  "Return non-nil when closed integer intervals [A0, A1] and [B0, B1] overlap."
  (and (<= a0 b1) (<= b0 a1)))

(defun omnidiff-engine--group-mapped-bounds (alignment side grp)
  "Return `(LO . HI)' mapped row interval for GRP on SIDE through ALIGNMENT."
  (let ((m0 (omnidiff-engine-map-row alignment side (omnidiff-engine--hunk-group-start-row grp)))
        (m1 (omnidiff-engine-map-row alignment side (omnidiff-engine--hunk-group-last-row grp))))
    (cons (min m0 m1) (max m0 m1))))

(defun omnidiff-engine--group-anchored-p (grp anc-vec)
  "Return non-nil if any row covered by GRP is marked in ANC-VEC."
  (let ((r (omnidiff-engine--hunk-group-start-row grp))
        (last (min (omnidiff-engine--hunk-group-last-row grp)
                   (1- (length anc-vec))))
        (found nil))
    (while (and (<= r last) (not found))
      (when (and (>= r 0) (aref anc-vec r))
        (setq found t))
      (setq r (1+ r)))
    found))

(defun omnidiff-engine-build-change-stops (json-diff alignment)
  "Build an ordered list of `omnidiff-change-stop's for JSON-DIFF and ALIGNMENT.
Merges paired before+after edits on aligned rows into one logical stop so
navigation does not double-stop on replacements, while keeping pure inserts
on `after', pure deletes on `before', and moves clearly navigable."
  (let* ((nb (length (omnidiff-line-alignment-before-to-after alignment)))
         (na (length (omnidiff-line-alignment-after-to-before alignment)))
         (b-anc (make-vector nb nil))
         (a-anc (make-vector na nil))
         (_ (dolist (pair (omnidiff-line-alignment-anchors alignment))
              (when (< (car pair) nb) (aset b-anc (car pair) t))
              (when (< (cdr pair) na) (aset a-anc (cdr pair) t))))
         (b-split (omnidiff-engine--coalesce-side-hunks
                   (omnidiff-json-side-hunks (omnidiff-json-diff-before json-diff))
                   b-anc))
         (a-split (omnidiff-engine--coalesce-side-hunks
                   (omnidiff-json-side-hunks (omnidiff-json-diff-after json-diff))
                   a-anc))
         (b-edits (vconcat (car b-split)))
         (a-edits (vconcat (car a-split)))
         (b-moves (vconcat (cdr b-split)))
         (a-moves (vconcat (cdr a-split)))
         (nb-e (length b-edits))
         (na-e (length a-edits))
         (b-e-used (make-vector nb-e nil))
         (b-e-mapped (make-vector nb-e nil))
         (b-e-anc (make-vector nb-e nil))
         (paired-edit-boxes nil)
         (entries nil))
    ;; Precompute before-edit mapped bounds onto `after' and anchor flags.
    (dotimes (bi nb-e)
      (let ((bg (aref b-edits bi)))
        (aset b-e-mapped bi
              (omnidiff-engine--group-mapped-bounds alignment 'before bg))
        (aset b-e-anc bi (omnidiff-engine--group-anchored-p bg b-anc))))
    ;; 1. Pair non-move edits across `after' and `before'.
    (dotimes (ai na-e)
      (let* ((ag (aref a-edits ai))
             (a0 (omnidiff-engine--hunk-group-start-row ag))
             (a1 (omnidiff-engine--hunk-group-last-row ag))
             (a-anc-p (omnidiff-engine--group-anchored-p ag a-anc))
             (ab (omnidiff-engine--group-mapped-bounds alignment 'after ag))
             (ab-lo (car ab))
             (ab-hi (cdr ab))
             (best-bi nil)
             (best-prio 99)
             (best-dist most-positive-fixnum))
        (dotimes (bi nb-e)
          (unless (aref b-e-used bi)
            (let* ((bg (aref b-edits bi))
                   (b0 (omnidiff-engine--hunk-group-start-row bg))
                   (b1 (omnidiff-engine--hunk-group-last-row bg))
                   (b-anc-p (aref b-e-anc bi))
                   (ba (aref b-e-mapped bi))
                   (ba-lo (car ba))
                   (ba-hi (cdr ba))
                   (ab-ov (omnidiff-engine--rows-overlap-p ab-lo ab-hi b0 b1))
                   (ba-ov (omnidiff-engine--rows-overlap-p ba-lo ba-hi a0 a1)))
              ;; Do not let an unanchored zero-width gap clamp onto an adjacent
              ;; anchored group whose own anchor maps elsewhere.
              (when (cond
                     ((and a-anc-p b-anc-p) (and ab-ov ba-ov))
                     (a-anc-p ab-ov)
                     (b-anc-p ba-ov)
                     (t (or ab-ov ba-ov)))
                (let ((prio (omnidiff-engine--op-counterpart-priority
                             (omnidiff-engine--hunk-group-operation ag)
                             (omnidiff-engine--hunk-group-operation bg)))
                      (dist (+ (abs (- ab-lo b0)) (abs (- ba-lo a0)))))
                  (when (or (< prio best-prio)
                            (and (= prio best-prio) (< dist best-dist)))
                    (setq best-bi bi
                          best-prio prio
                          best-dist dist)))))))
        (if best-bi
            (let* ((bg (aref b-edits best-bi))
                   (b0 (omnidiff-engine--hunk-group-start-row bg))
                   (b1 (omnidiff-engine--hunk-group-last-row bg)))
              (aset b-e-used best-bi t)
              (push (list a0 a1 b0 b1) paired-edit-boxes)
              (push (list (list a0 b0 (omnidiff-json-range-start-col
                                       (omnidiff-engine--hunk-group-range ag)))
                          (make-omnidiff-change-stop
                           :side 'after
                           :operation 'update
                           :range (omnidiff-engine--hunk-group-range ag)
                           :counterpart-side 'before
                           :counterpart-range (omnidiff-engine--hunk-group-range bg)
                           :reference-line (or (omnidiff-engine--hunk-group-reference-line ag)
                                               (omnidiff-engine--hunk-group-reference-line bg))))
                    entries))
          (push (list (list a0 ab-lo (omnidiff-json-range-start-col
                                      (omnidiff-engine--hunk-group-range ag)))
                      (make-omnidiff-change-stop
                       :side 'after
                       :operation (omnidiff-engine--hunk-group-operation ag)
                       :range (omnidiff-engine--hunk-group-range ag)
                       :counterpart-side 'before
                       :counterpart-range nil
                       :reference-line (omnidiff-engine--hunk-group-reference-line ag)))
                entries))))
    ;; Remaining unpaired `before' non-move edits (pure deletes).
    (dotimes (bi nb-e)
      (unless (aref b-e-used bi)
        (let* ((bg (aref b-edits bi))
               (b0 (omnidiff-engine--hunk-group-start-row bg))
               (ba-lo (car (aref b-e-mapped bi))))
          (push (list (list ba-lo b0 (omnidiff-json-range-start-col
                                      (omnidiff-engine--hunk-group-range bg)))
                      (make-omnidiff-change-stop
                       :side 'before
                       :operation (omnidiff-engine--hunk-group-operation bg)
                       :range (omnidiff-engine--hunk-group-range bg)
                       :counterpart-side 'after
                       :counterpart-range nil
                       :reference-line (omnidiff-engine--hunk-group-reference-line bg)))
                entries))))
    ;; 2. Pair move groups (suppressing inline syntax-shift moves subsumed by a paired edit).
    (let* ((nb-m (length b-moves))
           (na-m (length a-moves))
           (b-m-used (make-vector nb-m nil)))
      (cl-flet ((subsumed-by-paired-edit-p (a0 a1 b0 b1)
                  (cl-some (lambda (box)
                             (pcase-let ((`(,ea0 ,ea1 ,eb0 ,eb1) box))
                               (and (>= a0 ea0) (<= a1 ea1)
                                    (>= b0 eb0) (<= b1 eb1))))
                           paired-edit-boxes)))
        (dotimes (ai na-m)
          (let* ((ag (aref a-moves ai))
                 (a0 (omnidiff-engine--hunk-group-start-row ag))
                 (a1 (omnidiff-engine--hunk-group-last-row ag))
                 (amt (omnidiff-engine--hunk-group-move-target ag))
                 (amt0 (when amt (omnidiff-json-range-start-row amt)))
                 (amt1 (when amt (omnidiff-engine--range-effective-last-row amt)))
                 (best-bi nil)
                 (best-dist most-positive-fixnum))
            (dotimes (bi nb-m)
              (unless (aref b-m-used bi)
                (let* ((bg (aref b-moves bi))
                       (b0 (omnidiff-engine--hunk-group-start-row bg))
                       (b1 (omnidiff-engine--hunk-group-last-row bg))
                       (bmt (omnidiff-engine--hunk-group-move-target bg))
                       (bmt0 (when bmt (omnidiff-json-range-start-row bmt)))
                       (bmt1 (when bmt (omnidiff-engine--range-effective-last-row bmt))))
                  (when (or (and amt0 amt1 (omnidiff-engine--rows-overlap-p amt0 amt1 b0 b1))
                            (and bmt0 bmt1 (omnidiff-engine--rows-overlap-p bmt0 bmt1 a0 a1)))
                    (let ((dist (if amt0 (abs (- amt0 b0)) 0)))
                      (when (< dist best-dist)
                        (setq best-bi bi
                              best-dist dist)))))))
            (if best-bi
                (let* ((bg (aref b-moves best-bi))
                       (b0 (omnidiff-engine--hunk-group-start-row bg))
                       (b1 (omnidiff-engine--hunk-group-last-row bg)))
                  (aset b-m-used best-bi t)
                  (unless (subsumed-by-paired-edit-p a0 a1 b0 b1)
                    (push (list (list a0 b0 (omnidiff-json-range-start-col
                                             (omnidiff-engine--hunk-group-range ag)))
                                (make-omnidiff-change-stop
                                 :side 'after
                                 :operation 'move
                                 :range (omnidiff-engine--hunk-group-range ag)
                                 :counterpart-side 'before
                                 :counterpart-range (omnidiff-engine--hunk-group-range bg)
                                 :reference-line (or (omnidiff-engine--hunk-group-reference-line ag)
                                                     (omnidiff-engine--hunk-group-reference-line bg))))
                          entries)))
              (unless (and amt0 amt1 (subsumed-by-paired-edit-p a0 a1 amt0 amt1))
                (push (list (list a0 (or amt0 (omnidiff-engine-map-row alignment 'after a0))
                                  (omnidiff-json-range-start-col
                                   (omnidiff-engine--hunk-group-range ag)))
                            (make-omnidiff-change-stop
                             :side 'after
                             :operation 'move
                             :range (omnidiff-engine--hunk-group-range ag)
                             :counterpart-side 'before
                             :counterpart-range amt
                             :reference-line (omnidiff-engine--hunk-group-reference-line ag)))
                      entries)))))
        (dotimes (bi nb-m)
          (unless (aref b-m-used bi)
            (let* ((bg (aref b-moves bi))
                   (b0 (omnidiff-engine--hunk-group-start-row bg))
                   (b1 (omnidiff-engine--hunk-group-last-row bg))
                   (bmt (omnidiff-engine--hunk-group-move-target bg))
                   (bmt0 (when bmt (omnidiff-json-range-start-row bmt)))
                   (bmt1 (when bmt (omnidiff-engine--range-effective-last-row bmt))))
              (unless (and bmt0 bmt1 (subsumed-by-paired-edit-p bmt0 bmt1 b0 b1))
                (push (list (list (or bmt0 (omnidiff-engine-map-row alignment 'before b0))
                                  b0
                                  (omnidiff-json-range-start-col
                                   (omnidiff-engine--hunk-group-range bg)))
                            (make-omnidiff-change-stop
                             :side 'before
                             :operation 'move
                             :range (omnidiff-engine--hunk-group-range bg)
                             :counterpart-side 'after
                             :counterpart-range bmt
                             :reference-line (omnidiff-engine--hunk-group-reference-line bg)))
                      entries)))))))
    ;; 3. Sort in reading order and assign 1-based indices.
    (setq entries
          (sort entries
                (lambda (x y)
                  (pcase-let ((`(,xa ,xb ,xc) (car x))
                              (`(,ya ,yb ,yc) (car y)))
                    (cond
                     ((/= xa ya) (< xa ya))
                     ((/= xb yb) (< xb yb))
                     (t (< xc yc)))))))
    (let ((idx 0)
          (stops nil))
      (dolist (e entries)
        (setq idx (1+ idx))
        (let ((stop (cadr e)))
          (setf (omnidiff-change-stop-index stop) idx)
          (push stop stops)))
      (nreverse stops))))

(defun omnidiff-engine-summary-label (summary)
  "Return a concise human-readable badge string for SUMMARY symbol, or nil."
  (pcase summary
    ('nil nil)
    ('no-changes "no changes")
    ('new-file "new file")
    ('deleted-file "deleted file")
    ('whitespace-only "whitespace only")
    ('comment-only "comment only")
    ('refactor-moved-only "moved only")
    ((pred symbolp) (string-replace "-" " " (symbol-name summary)))
    (_ nil)))

;;; CLI invocation & file helpers

(cl-defun omnidiff-engine-build-args (&optional (preset 'default)
                                                (whole-updates omnidiff-whole-updates)
                                                (paint-reindent-moves omnidiff-paint-reindent-moves))
  "Build CLI flag list for `omnidiff --mode json'.
PRESET may be `default' (or nil), `minimal', or `full'.  WHOLE-UPDATES and
PAINT-REINDENT-MOVES default to `omnidiff-whole-updates' and
`omnidiff-paint-reindent-moves' when omitted."
  (let ((args (list "--mode" "json")))
    (pcase (or preset 'default)
      ('default nil)
      ('minimal (setq args (nconc args (list "--minimal"))))
      ('full (setq args (nconc args (list "--full"))))
      (other (error "Unknown omnidiff preset: %S" other)))
    (when whole-updates
      (setq args (nconc args (list "--whole-updates"))))
    (when paint-reindent-moves
      (setq args (nconc args (list "--paint-reindent-moves"))))
    args))

(defconst omnidiff-engine--raw-byte-regexp
  (string ?\[ #x3fff80 ?- #x3fffff ?\])
  "Regexp matching Emacs raw-byte characters produced by invalid UTF-8.")

(defun omnidiff-engine--sniff-bom-encoding (raw)
  "Return BOM encoding name for unibyte RAW bytes, or nil for UTF-8."
  (let ((len (length raw)))
    (cond
     ((and (>= len 4)
           (= (aref raw 0) #xff) (= (aref raw 1) #xfe)
           (= (aref raw 2) #x00) (= (aref raw 3) #x00))
      "UTF-32LE")
     ((and (>= len 4)
           (= (aref raw 0) #x00) (= (aref raw 1) #x00)
           (= (aref raw 2) #xfe) (= (aref raw 3) #xff))
      "UTF-32BE")
     ((and (>= len 2)
           (= (aref raw 0) #xff) (= (aref raw 1) #xfe))
      "UTF-16LE")
     ((and (>= len 2)
           (= (aref raw 0) #xfe) (= (aref raw 1) #xff))
      "UTF-16BE")
     (t nil))))

(defun omnidiff-engine--decode-utf32 (raw big-endian-p)
  "Decode unibyte RAW bytes as UTF-32 (big-endian if BIG-ENDIAN-P, else LE).
Preserves any leading U+FEFF byte order mark to match `omnidiff' byte columns."
  (let* ((count (/ (length raw) 4))
         (chars (make-vector count 0)))
    (dotimes (i count)
      (let* ((base (* i 4))
             (b0 (aref raw base))
             (b1 (aref raw (+ base 1)))
             (b2 (aref raw (+ base 2)))
             (b3 (aref raw (+ base 3)))
             (cp (if big-endian-p
                     (logior (ash b0 24) (ash b1 16) (ash b2 8) b3)
                   (logior b0 (ash b1 8) (ash b2 16) (ash b3 24)))))
        (aset chars i (if (and (<= cp #x10ffff)
                               (not (and (>= cp #xd800) (<= cp #xdfff))))
                          cp
                        #xfffd))))
    (concat chars)))

(defun omnidiff-engine-decode-bytes (raw &optional encoding)
  "Decode unibyte string RAW into an Emacs string matching `omnidiff --mode json'.
Honors ENCODING (`UTF-16LE', `UTF-16BE', `UTF-32LE', `UTF-32BE') or BOM
auto-detection while preserving leading U+FEFF so UTF-8 byte offsets match
`omnidiff --mode json'.  Falls back to `utf-8-unix' (or `undecided' on
non-UTF-8 text), and normalizes CRLF (`\\r\\n') line endings to LF (`\\n')."
  (if (or (null raw) (string-empty-p raw))
      ""
    (let* ((enc (or encoding (omnidiff-engine--sniff-bom-encoding raw)))
           (text (pcase enc
                   ("UTF-16LE" (decode-coding-string raw 'utf-16le-unix))
                   ("UTF-16BE" (decode-coding-string raw 'utf-16be-unix))
                   ("UTF-32LE" (omnidiff-engine--decode-utf32 raw nil))
                   ("UTF-32BE" (omnidiff-engine--decode-utf32 raw t))
                   (_
                    (let ((decoded (decode-coding-string raw 'utf-8-unix)))
                      (if (string-match-p omnidiff-engine--raw-byte-regexp decoded)
                          (decode-coding-string raw 'undecided)
                        decoded))))))
      (if (string-search "\r\n" text)
          (string-replace "\r\n" "\n" text)
        text))))

(defun omnidiff-engine-read-file-decoded (path &optional encoding)
  "Read file at PATH and return its decoded Emacs string.
Returns `\"\"' for `/dev/null'.  Decodes bytes via `omnidiff-engine-decode-bytes'."
  (if (or (null path) (string-empty-p path) (string= path "/dev/null"))
      ""
    (omnidiff-engine-decode-bytes
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (insert-file-contents-literally path)
       (buffer-string))
     encoding)))

(defun omnidiff-engine--resolve-executable (&optional executable)
  "Resolve `omnidiff' CLI path from EXECUTABLE, `omnidiff-executable', or PATH."
  (let ((bin (or executable
                 (and (boundp 'omnidiff-executable) omnidiff-executable)
                 "omnidiff")))
    (or (executable-find bin)
        (error "Cannot find omnidiff executable: %s" bin))))

(cl-defun omnidiff-engine-run-pair (before-path after-path
                                                &optional (preset 'default)
                                                executable
                                                (whole-updates omnidiff-whole-updates)
                                                (paint-reindent-moves omnidiff-paint-reindent-moves))
  "Synchronously run `omnidiff --mode json' on BEFORE-PATH and AFTER-PATH.
Returns a parsed `omnidiff-json-diff', or signals an error on non-zero exit."
  (let* ((bin (omnidiff-engine--resolve-executable executable))
         (args (nconc (omnidiff-engine-build-args preset whole-updates paint-reindent-moves)
                      (list "--" before-path after-path)))
         (err-file (make-temp-file "omnidiff-json-err-")))
    (unwind-protect
        (with-temp-buffer
          (let* ((coding-system-for-read 'utf-8-unix)
                 (status (apply #'call-process bin nil (list (current-buffer) err-file) nil args)))
            (if (and (integerp status) (zerop status))
                (omnidiff-engine-parse-json (buffer-string))
              (let ((stderr (string-trim
                             (with-temp-buffer
                               (let ((coding-system-for-read 'utf-8-unix))
                                 (insert-file-contents err-file))
                               (buffer-string)))))
                (error "omnidiff exited with %s%s"
                       status
                       (if (string-empty-p stderr) "" (concat ": " stderr)))))))
      (when (file-exists-p err-file)
        (ignore-errors (delete-file err-file))))))

(cl-defun omnidiff-engine-run-pair-async (before-path after-path callback
                                                      &key (preset 'default)
                                                      executable
                                                      (whole-updates omnidiff-whole-updates)
                                                      (paint-reindent-moves omnidiff-paint-reindent-moves)
                                                      error-callback)
  "Asynchronously run `omnidiff --mode json' on BEFORE-PATH and AFTER-PATH.
Returns the spawned process object.  Invokes CALLBACK with the parsed
`omnidiff-json-diff' on success, or ERROR-CALLBACK with an error message
string on failure (falling back to signaling an error if nil)."
  (let* ((bin (omnidiff-engine--resolve-executable executable))
         (args (nconc (omnidiff-engine-build-args preset whole-updates paint-reindent-moves)
                      (list "--" before-path after-path)))
         (out-buf (generate-new-buffer " *omnidiff-json-stdout*"))
         (err-buf (generate-new-buffer " *omnidiff-json-stderr*"))
         (err-proc (make-pipe-process
                    :name "omnidiff-json-stderr"
                    :buffer err-buf
                    :coding 'utf-8-unix
                    :sentinel #'ignore
                    :noquery t)))
    (cl-flet ((cleanup ()
                (when (process-live-p err-proc)
                  (delete-process err-proc))
                (when (buffer-live-p out-buf)
                  (kill-buffer out-buf))
                (when (buffer-live-p err-buf)
                  (kill-buffer err-buf))))
      (condition-case err
          (make-process
           :name "omnidiff-json"
           :buffer out-buf
           :command (cons bin args)
           :stderr err-proc
           :connection-type 'pipe
           :coding 'utf-8-unix
           :noquery t
           :sentinel
           (lambda (proc _event)
             (unless (process-live-p proc)
               (if (and (eq (process-status proc) 'exit)
                        (zerop (process-exit-status proc)))
                   (let ((raw (when (buffer-live-p out-buf)
                                (with-current-buffer out-buf (buffer-string)))))
                     (cleanup)
                     (condition-case parse-err
                         (funcall callback (omnidiff-engine-parse-json (or raw "")))
                       (error
                        (if error-callback
                            (funcall error-callback (error-message-string parse-err))
                          (signal (car parse-err) (cdr parse-err))))))
                 (when (process-live-p err-proc)
                   (accept-process-output err-proc 0.05))
                 (let* ((stderr (string-trim
                                 (if (buffer-live-p err-buf)
                                     (with-current-buffer err-buf (buffer-string))
                                   "")))
                        (msg (format "omnidiff exited with %s (%s)%s"
                                     (process-exit-status proc)
                                     (process-status proc)
                                     (if (string-empty-p stderr) "" (concat ": " stderr)))))
                   (cleanup)
                   (if error-callback
                       (funcall error-callback msg)
                     (error "%s" msg)))))))
        (error
         (cleanup)
         (signal (car err) (cdr err)))))))

(defun omnidiff-engine--sanitize-filename-hint (hint)
  "Extract a safe single filename component from HINT preserving extension."
  (let* ((stripped (replace-regexp-in-string "<[0-9]+>\\'" "" (or hint "")))
         (base (file-name-nondirectory stripped))
         (clean (replace-regexp-in-string "[/\\\x00]" "_" base)))
    (cond
     ((or (string-empty-p clean) (string= clean ".") (string= clean ".."))
      "buffer")
     ((string-prefix-p "~" clean)
      (concat "_" clean))
     (t clean))))

(defun omnidiff-engine-write-temp-for-buffer (buffer &optional tmpdir name-hint)
  "Write BUFFER contents to a temp file preserving extension/basename.
When TMPDIR is non-nil, writes inside a unique subdirectory of TMPDIR so
exact basenames (e.g. `Makefile', `Dockerfile') are preserved without
collisions.  When TMPDIR is nil, preserves file extensions directly (or
uses a temp subdirectory for extensionless basenames).  Writes UTF-8 with
Unix newlines so `omnidiff --mode json' columns match BUFFER directly.
Returns the written file path."
  (let* ((raw-hint (or (and (stringp name-hint) (not (string-empty-p name-hint)) name-hint)
                       (buffer-file-name buffer)
                       (buffer-name buffer)))
         (base (omnidiff-engine--sanitize-filename-hint raw-hint))
         (ext (file-name-extension base t))
         (dest (cond
                (tmpdir
                 (make-directory tmpdir t)
                 (let* ((sub (file-name-as-directory
                              (expand-file-name
                               (make-temp-file (expand-file-name "buf-" tmpdir) t))))
                        (cand (expand-file-name (concat "./" base) sub)))
                   (if (string-prefix-p sub cand)
                       cand
                     (expand-file-name "./buffer" sub))))
                ((and ext (not (string-empty-p ext)))
                 (make-temp-file "omnidiff-buf-" nil ext))
                (t
                 (let* ((sub (file-name-as-directory
                              (expand-file-name
                               (make-temp-file "omnidiff-buf-" t))))
                        (cand (expand-file-name (concat "./" base) sub)))
                   (if (string-prefix-p sub cand)
                       cand
                     (expand-file-name "./buffer" sub)))))))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let ((coding-system-for-write 'utf-8-unix)
              (write-region-inhibit-fsync t))
          (write-region (point-min) (point-max) dest nil 'silent))))
    dest))

(provide 'omnidiff-engine)
;;; omnidiff-engine.el ends here
