;;; omnidiff-git.el --- Async Git diff pipeline for omnidiff -*- lexical-binding: t; -*-

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

;; Asynchronous Git diff pipeline for `omnidiff.el'.  Coordinates:
;;
;;  - Parallel `git --raw --no-abbrev -z' metadata discovery, bounded
;;    `git cat-file --batch-check' / `--batch' blob materialization, and
;;    concurrent `omnidiff --mode json' worker processes.
;;  - Automatic fallback to serial `GIT_EXTERNAL_DIFF' invocation for
;;    multi-commit `git show', combined merge diffs, unmerged paths, and
;;    oversized blobs exceeding `omnidiff-max-parallel-blob-bytes'.
;;  - Request lifecycle ownership, cancellation, stale-completion rejection,
;;    and Overview/Dual post-render commit dispatch via `omnidiff-run'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'omnidiff-engine)
(require 'omnidiff-image)
(require 'omnidiff-render)

(declare-function magit-toplevel "ext:magit-git" (&optional directory))
(declare-function omnidiff-mode "omnidiff" ())
(declare-function omnidiff-finalize-git-render "omnidiff"
                  (buffer diff saved-loc entry-policy
                          &optional target-file target-line target-side final-msg))

;;; Protocol sentinels

(defconst omnidiff-git-file-sentinel "\x1cOMNIDIFF-FILE\x00"
  "Per-file sentinel emitted by the `GIT_EXTERNAL_DIFF' shell wrapper.")

(defconst omnidiff-git-mode-sentinel "\x1cOMNIDIFF-MODE\x00"
  "Optional mode-change marker emitted after file metadata when modes differ.")

(defconst omnidiff-git-oid-sentinel "\x1cOMNIDIFF-OID\x00"
  "Optional Git OID marker emitted after file metadata for blob retrieval.")

(defconst omnidiff-git-end-sentinel "\x1cOMNIDIFF-END\x00"
  "End-of-record sentinel emitted after JSON output under `GIT_EXTERNAL_DIFF'.")

;;; Customization & request state

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

(defcustom omnidiff-after-diff-hook nil
  "Normal hook run in the Overview buffer after `omnidiff-run' renders a diff."
  :type 'hook
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
fall back to Git's serial `GIT_EXTERNAL_DIFF' invocation (and skips reading
oversized blobs into Emacs memory)."
  :type 'natnum
  :group 'omnidiff)

(cl-defstruct (omnidiff--request (:copier nil))
  "In-flight asynchronous `omnidiff' request bound to a buffer."
  (id 0 :type integer)
  (buffer nil)
  (proc nil)
  (stderr-proc nil)
  (workers nil :type list)
  (tmpdir nil)
  (bin nil)
  (worker-flags nil :type list)
  (command nil :type list)
  (par-kind nil) ; nil | diff | show
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
  (err-chunks nil :type list)
  (stderr-done nil :type boolean)
  (cancelled nil :type boolean)
  (preserve-state nil :type boolean)
  (image-blobs nil)
  ;; Parallel worker stage state.
  (preamble nil)
  (results nil)
  (queue nil :type list)
  (active 0 :type integer)
  (done-jobs 0 :type integer)
  (total-jobs 0 :type integer)
  (done-files 0 :type integer)
  (total-files 0 :type integer)
  (last-progress-time 0.0 :type float)
  ;; Multi-batch `git cat-file --batch' streaming state.
  (cat-overflow nil :type boolean)
  (cat-rem-batches nil :type list)
  (cat-entries nil :type list)
  (cat-sha-targets nil)
  (cat-visual-shas nil)
  (cat-blob-files nil)
  (cat-seeded-cell nil)
  (cat-visual-limit 0 :type integer))

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

(defvar-local omnidiff--origin-buffer nil
  "Originating buffer (e.g. a Magit buffer) from which Overview was launched.")

;;; Request accessors & cancellation

(defun omnidiff-git-pending-request ()
  "Return the active unfailed `omnidiff--request' for this buffer, or nil."
  (when (and omnidiff--active-request
             (null (omnidiff--request-failed-event omnidiff--active-request)))
    omnidiff--active-request))

(defun omnidiff-git-effective-command ()
  "Return pending Git command if an unfailed request is active, else committed."
  (if-let* ((req (omnidiff-git-pending-request)))
      (omnidiff--request-command req)
    omnidiff--command))

(defun omnidiff-git-effective-label ()
  "Return the pending label if an unfailed request is in flight, else committed."
  (if-let* ((req (omnidiff-git-pending-request)))
      (omnidiff--request-label req)
    omnidiff--label))

(defun omnidiff-git-effective-repo-dir ()
  "Return the pending repo dir if an unfailed request is in flight, else committed."
  (if-let* ((req (omnidiff-git-pending-request)))
      (omnidiff--request-repo-dir req)
    omnidiff--repo-dir))

(defun omnidiff-git-effective-layout ()
  "Return the pending layout if an unfailed request is in flight, else committed."
  (or (when-let* ((req (omnidiff-git-pending-request)))
        (omnidiff--request-layout req))
      omnidiff--layout
      omnidiff-default-layout))

(defun omnidiff-git-update-pending-layout (layout &optional msg)
  "Update the in-flight request's target LAYOUT and optional status MSG.
Return non-nil if an unfailed pending request was updated, else nil."
  (when-let* ((req (omnidiff-git-pending-request)))
    (setf (omnidiff--request-layout req) layout)
    (when msg
      (setf (omnidiff--request-last-msg req) msg))
    t))

(defun omnidiff-git-effective-preset ()
  "Return the pending preset if an unfailed request is in flight, else committed."
  (or (when-let* ((req (omnidiff-git-pending-request)))
        (omnidiff--request-preset req))
      omnidiff--preset
      omnidiff-default-preset))

(defun omnidiff-git-effective-whole-updates ()
  "Return pending or committed `--whole-updates' setting for this buffer."
  (if-let* ((req (omnidiff-git-pending-request)))
      (omnidiff--request-whole-updates req)
    (if (local-variable-p 'omnidiff--whole-updates)
        omnidiff--whole-updates
      omnidiff-whole-updates)))

(defun omnidiff-git-effective-paint-reindent-moves ()
  "Return pending or committed `--paint-reindent-moves' setting for this buffer."
  (if-let* ((req (omnidiff-git-pending-request)))
      (omnidiff--request-paint-reindent-moves req)
    (if (local-variable-p 'omnidiff--paint-reindent-moves)
        omnidiff--paint-reindent-moves
      omnidiff-paint-reindent-moves)))

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

(defun omnidiff-git-cancel-request ()
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

;;; Git paths, OIDs & blob materialization helpers

(defun omnidiff--repo-root ()
  "Return the Git repository root for `default-directory'."
  (or (and (fboundp 'magit-toplevel) (magit-toplevel))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun omnidiff-git-toplevel (&optional dir)
  "Return the Git repository root directory for DIR, or nil."
  (let ((default-directory (or dir default-directory)))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "rev-parse" "--show-toplevel"))
        (let ((out (string-trim (buffer-string))))
          (unless (string-empty-p out)
            (file-name-as-directory out)))))))

(defun omnidiff--decode-git-path (bytes)
  "Decode unibyte Git path BYTES without converting embedded CRLF endings."
  (decode-coding-string bytes 'utf-8-unix))

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

(defun omnidiff--blob-key (sha path)
  "Return cache key `(SHA . HINT)' preserving PATH's extension or basename.
`omnidiff' infers parser language from the temp file's extension or exact
basename (e.g. `Makefile', `Dockerfile')."
  (let* ((base (file-name-nondirectory path))
         (ext (file-name-extension base t)))
    (cons sha (if (and ext (not (string-empty-p ext))) ext base))))

(defun omnidiff-git-blob-dest (sha path tmpdir)
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

(defun omnidiff-git-write-small-temp (dest content)
  "Write unibyte/UTF-8 string CONTENT to DEST without fsync or handlers."
  (let ((file-name-handler-alist nil)
        (write-region-inhibit-fsync t)
        (coding-system-for-write 'no-conversion))
    (write-region content nil dest nil 'silent)
    dest))

(defun omnidiff-git-cat-blob-to-file (oid path repo-dir tmpdir tag)
  "Write Git blob OID from REPO-DIR into TMPDIR preserving PATH's hint.
Returns the written file path, or nil if `git cat-file' fails."
  (when (and oid repo-dir (not (omnidiff--zero-oid-p oid)))
    (let* ((default-directory repo-dir)
           (dest (omnidiff-git-blob-dest (concat tag "-" oid) path tmpdir)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'no-conversion))
          (when (zerop (call-process "git" nil '(t nil) nil "cat-file" "blob" oid))
            (omnidiff-git-write-small-temp dest (buffer-string))))))))

(defun omnidiff-git-show-to-file (rev relpath repo-dir tmpdir tag)
  "Write `REV:RELPATH' from Git REPO-DIR into TMPDIR preserving RELPATH's hint.
Returns the written file path, or nil when `REV:RELPATH' does not exist."
  (when (and rev relpath repo-dir)
    (let* ((default-directory repo-dir)
           (dest (omnidiff-git-blob-dest tag relpath tmpdir)))
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (let ((coding-system-for-read 'no-conversion))
          (when (zerop (call-process "git" nil '(t nil) nil "show" (concat rev ":" relpath)))
            (omnidiff-git-write-small-temp dest (buffer-string))))))))

;;; `GIT_EXTERNAL_DIFF' wrapper & `--raw' parsing

(defun omnidiff--build-ext-diff-wrapper (bin flags &optional end-sentinel-p)
  "Build a `GIT_EXTERNAL_DIFF' shell command invoking BIN with FLAGS.
When END-SENTINEL-P is non-nil, emit `omnidiff-git-end-sentinel' after
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

;;; File construction & `cat-file --batch' helpers

(defun omnidiff-git--source-line-count (source)
  "Return logical line count of decoded string SOURCE without allocating lines."
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

(defun omnidiff-git--file-status (path old-file new-file new-path)
  "Derive `(STATUS . TARGET-PATH)' for a diff entry."
  (let* ((renamed-p (and (stringp new-path)
                         (not (string-empty-p new-path))
                         (not (string= new-path path))))
         (status (cond
                  ((string= old-file "/dev/null") 'added)
                  ((string= new-file "/dev/null") 'deleted)
                  (renamed-p 'renamed)
                  (t 'modified))))
    (cons status (if (eq status 'renamed) new-path path))))

(defun omnidiff-git--make-error-file (path old-file new-file new-path
                                           old-mode new-mode old-oid new-oid
                                           note &optional preamble)
  "Construct an error/fallback `omnidiff-file' carrying NOTE."
  (pcase-let ((`(,status . ,target)
               (omnidiff-git--file-status path old-file new-file new-path)))
    (make-omnidiff-file
     :path path
     :target-path target
     :status status
     :preamble preamble
     :old-mode (omnidiff-engine-normalize-mode old-mode)
     :new-mode (omnidiff-engine-normalize-mode new-mode)
     :old-oid (omnidiff-engine-normalize-oid old-oid)
     :new-oid (omnidiff-engine-normalize-oid new-oid)
     :note note)))

(defun omnidiff-git-file-from-json (path old-file new-file new-path json-diff
                                      &optional old-mode new-mode old-oid new-oid
                                      before-disk-path after-disk-path
                                      pre-before-src pre-after-src preamble)
  "Construct a full `omnidiff-file' from JSON-DIFF and file metadata.
Decodes BEFORE and AFTER sources from OLD-FILE and NEW-FILE using the JSON
side encodings whenever the entry is not binary and PRE-BEFORE-SRC /
PRE-AFTER-SRC are not already supplied."
  (pcase-let* ((`(,status . ,target)
                (omnidiff-git--file-status path old-file new-file new-path))
               (b-side (omnidiff-json-diff-before json-diff))
               (a-side (omnidiff-json-diff-after json-diff))
               (binary-p (omnidiff-json-diff-binary-p json-diff))
               (picture (omnidiff-json-diff-picture json-diff))
               (container (omnidiff-json-diff-container json-diff))
               (summary (omnidiff-json-diff-summary json-diff))
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
      (let ((stats (omnidiff-render-compute-json-file-stats
                    json-diff
                    (omnidiff-git--source-line-count before-src)
                    (omnidiff-git--source-line-count after-src))))
        (setq added (plist-get stats :added)
              removed (plist-get stats :removed)
              updated (plist-get stats :updated)
              moved (plist-get stats :moved)
              max-line (plist-get stats :max-line)))))
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
       :old-mode (omnidiff-engine-normalize-mode old-mode)
       :new-mode (omnidiff-engine-normalize-mode new-mode)
       :old-oid (omnidiff-engine-normalize-oid old-oid)
       :new-oid (omnidiff-engine-normalize-oid new-oid)
       :json-diff json-diff
       :before-source before-src
       :after-source after-src
       :before-disk-path before-disk-path
       :after-disk-path after-disk-path
       :note note
       :added added
       :removed removed
       :updated updated
       :moved moved
       :max-line max-line))))

(defun omnidiff-git--for-each-cat-batch-entry (cat-buf oids fn)
  "Iterate over `git cat-file --batch' entries for OIDS in unibyte CAT-BUF.
For each present (non-missing) entry, call `(FN OID START END SIZE)' with
CAT-BUF current, where START and END delimit the raw blob bytes and SIZE
is the declared byte length."
  (when (and cat-buf (buffer-live-p cat-buf) oids)
    (with-current-buffer cat-buf
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
                (funcall fn oid start end sz)
                (goto-char (min (point-max) (1+ end)))))))))))

(defun omnidiff-git--maybe-seed-visual-blob (image-blobs sha cat-buf start end
                                                         seeded-cell visual-limit)
  "Seed raw bytes [START, END) in CAT-BUF for visual SHA into IMAGE-BLOBS.
Respects `omnidiff-image-max-bytes', VISUAL-LIMIT (counter in cons cell
SEEDED-CELL), and skips Git LFS pointers via `omnidiff-image-lfs-pointer-p'."
  (let ((len (- end start)))
    (when (and (hash-table-p image-blobs)
               (not (omnidiff--zero-oid-p sha))
               (< (car seeded-cell) visual-limit)
               (not (gethash sha image-blobs))
               (> len 0)
               (or (<= omnidiff-image-max-bytes 0)
                   (<= len omnidiff-image-max-bytes)))
      (let ((raw (with-current-buffer cat-buf
                   (buffer-substring-no-properties start end))))
        (unless (omnidiff-image-lfs-pointer-p raw)
          (puthash sha raw image-blobs)
          (setcar seeded-cell (1+ (car seeded-cell))))))))

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

;;; Serial fallback blob reading & JSON parser

(defun omnidiff--batch-read-git-blobs (oids repo-dir &optional skipped-out)
  "Batch-read Git blob OIDS in REPO-DIR into a hash table `OID -> UNIBYTE'.
Checks sizes via `git cat-file --batch-check' first and skips any blob
whose size exceeds `omnidiff-max-parallel-blob-bytes' (marking skipped OIDs
in optional hash table SKIPPED-OUT)."
  (let ((table (make-hash-table :test #'equal :size (max 16 (length oids)))))
    (when (and oids repo-dir (file-directory-p repo-dir))
      (let ((default-directory repo-dir)
            (coding-system-for-read 'no-conversion)
            (coding-system-for-write 'no-conversion)
            (max-bytes omnidiff-max-parallel-blob-bytes)
            (allowed-oids oids))
        (when (and (integerp max-bytes) (> max-bytes 0))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (if (zerop (call-process-region
                        (concat (string-join oids "\n") "\n")
                        nil "git" nil '(t nil) nil "cat-file" "--batch-check"))
                (let ((sizes (omnidiff--parse-batch-check-sizes (buffer-string)))
                      (kept nil))
                  (dolist (oid oids)
                    (let ((sz (gethash oid sizes)))
                      (if (and (integerp sz) (> sz max-bytes))
                          (when (hash-table-p skipped-out)
                            (puthash oid t skipped-out))
                        (push oid kept))))
                  (setq allowed-oids (nreverse kept)))
              (setq allowed-oids nil))))
        (when allowed-oids
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (when (zerop (call-process-region
                          (concat (string-join allowed-oids "\n") "\n")
                          nil "git" nil '(t nil) nil "cat-file" "--batch"))
              (omnidiff-git--for-each-cat-batch-entry
               (current-buffer) allowed-oids
               (lambda (oid start end _sz)
                 (puthash oid (buffer-substring-no-properties start end) table))))))))
    table))

(defun omnidiff--read-serial-side-source (oid mode rel-path raw-file encoding
                                              repo-dir blob-cache
                                              &optional skipped-oids)
  "Resolve decoded text source for one side of a serial JSON diff entry.
Returns the symbol `too-large' when the blob or working-tree file exceeds
`omnidiff-max-parallel-blob-bytes'."
  (let ((norm-oid (omnidiff-engine-normalize-oid oid))
        (max-bytes omnidiff-max-parallel-blob-bytes))
    (cond
     ((or (null raw-file) (string= raw-file "/dev/null")) "")
     ((equal mode "160000")
      (format "Subproject commit %s\n" (or norm-oid oid "")))
     ((and norm-oid (hash-table-p skipped-oids) (gethash norm-oid skipped-oids))
      'too-large)
     ((when-let* ((raw (and norm-oid (gethash norm-oid blob-cache))))
        (omnidiff-engine-decode-bytes raw encoding)))
     ((and repo-dir rel-path (not (string-empty-p rel-path)))
      (let ((full (omnidiff-engine-expand-repo-path rel-path repo-dir)))
        (cond
         ((when-let* ((link (file-symlink-p full)))
            link))
         ((file-regular-p full)
          (let* ((attrs (file-attributes full))
                 (sz (and attrs (file-attribute-size attrs))))
            (if (and (integerp max-bytes) (> max-bytes 0)
                     (integerp sz) (> sz max-bytes))
                'too-large
              (or (ignore-errors (omnidiff-engine-read-file-decoded full encoding)) ""))))
         (norm-oid
          (format "Subproject commit %s\n" norm-oid))
         (t ""))))
     (t ""))))

(defun omnidiff--parse-serial-json-output (raw repo-dir)
  "Parse serial `GIT_EXTERNAL_DIFF' JSON output RAW in REPO-DIR."
  (let* ((sentinel omnidiff-git-file-sentinel)
         (slen (length sentinel))
         (msent omnidiff-git-mode-sentinel)
         (mlen (length msent))
         (osent omnidiff-git-oid-sentinel)
         (olen (length osent))
         (esent omnidiff-git-end-sentinel)
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
                      (let ((oid (omnidiff-engine-normalize-oid (car pair)))
                            (mode (cdr pair)))
                        (when (and oid (not (equal mode "160000"))
                                   (not (gethash oid seen-oids)))
                          (puthash oid t seen-oids)
                          (push oid needed-oids)))))
                  (push (list path old-file new-file new-path old-mode new-mode
                              old-oid new-oid parsed-jd parse-err-msg rec-pre)
                        records))
                (setq pos next)))))
        (let* ((skipped-oids (make-hash-table :test #'equal))
               (blob-cache (omnidiff--batch-read-git-blobs
                            (nreverse needed-oids) repo-dir skipped-oids))
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
                         (b-res (unless bin-p
                                  (omnidiff--read-serial-side-source
                                   old-oid old-mode path old-file
                                   (and b-side (omnidiff-json-side-encoding b-side))
                                   repo-dir blob-cache skipped-oids)))
                         (a-res (unless bin-p
                                  (omnidiff--read-serial-side-source
                                   new-oid new-mode target new-file
                                   (and a-side (omnidiff-json-side-encoding a-side))
                                   repo-dir blob-cache skipped-oids)))
                         (too-large-p (or (eq b-res 'too-large) (eq a-res 'too-large)))
                         (b-src (if too-large-p "" b-res))
                         (a-src (if too-large-p "" a-res))
                         (file (omnidiff-git-file-from-json
                                path old-file new-file new-path jd
                                old-mode new-mode old-oid new-oid nil nil
                                b-src a-src rec-pre)))
                    (when too-large-p
                      (let ((stats (omnidiff-render-compute-json-file-stats jd)))
                        (setf (omnidiff-file-too-large-p file) t
                              (omnidiff-file-added file) (plist-get stats :added)
                              (omnidiff-file-removed file) (plist-get stats :removed)
                              (omnidiff-file-updated file) (plist-get stats :updated)
                              (omnidiff-file-moved file) (plist-get stats :moved)
                              (omnidiff-file-max-line file) (plist-get stats :max-line)
                              (omnidiff-file-note file)
                              (format "File exceeds blob size limit (%s) — press T for native omnidiff TUI"
                                      (omnidiff-engine-format-bytes
                                       omnidiff-max-parallel-blob-bytes)))))
                    (push file files))
                (push (omnidiff-git--make-error-file
                       path old-file new-file new-path
                       old-mode new-mode old-oid new-oid
                       err-msg rec-pre)
                      files))))
          ;; Flush any trailing empty commit / clean merge header in multi-commit `git show'.
          (when (and pending-pre (not (string-blank-p pending-pre)))
            (push (make-omnidiff-file
                   :path ""
                   :target-path ""
                   :status 'modified
                   :preamble pending-pre
                   :note "No file changes in commit")
                  files))
          (make-omnidiff-diff :preamble preamble :files (nreverse files)))))))

;;; Parallel blob materialization & work preparation

(defun omnidiff--materialize-cat-batch (cat-buf batch-shas sha-targets visual-shas
                                                ensure-tmpdir image-blobs blob-files
                                                seeded-cell visual-limit)
  "Extract blobs in BATCH-SHAS from CAT-BUF onto disk in BLOB-FILES.
Also seeds IMAGE-BLOBS for visual OIDs in VISUAL-SHAS up to VISUAL-LIMIT,
updating the counter stored in SEEDED-CELL."
  (let ((file-name-handler-alist nil)
        (write-region-inhibit-fsync t)
        (coding-system-for-write 'no-conversion))
    (omnidiff-git--for-each-cat-batch-entry
     cat-buf batch-shas
     (lambda (sha start end _sz)
       (when (gethash sha visual-shas)
         (omnidiff-git--maybe-seed-visual-blob
          image-blobs sha cat-buf start end seeded-cell visual-limit))
       (dolist (kp (gethash sha sha-targets))
         (let ((key (car kp))
               (path (cdr kp)))
           (unless (gethash key blob-files)
             (let ((dest (omnidiff-git-blob-dest sha path (funcall ensure-tmpdir))))
               (write-region start end dest nil 'silent)
               (puthash key dest blob-files)))))))))

(cl-defun omnidiff--prepare-parallel-work (cat-buf sha-list entries ensure-tmpdir dir
                                                   &optional image-blobs
                                                   pre-blob-files)
  "Build results vector and worker job list for Git raw ENTRIES.
Materializes blobs from CAT-BUF via ENSURE-TMPDIR when not already present
in PRE-BLOB-FILES.  When IMAGE-BLOBS is a hash table, seeds visual image/SVG
blobs from CAT-BUF so inline image rendering avoids duplicate `git cat-file'
calls."
  (let ((spans (make-hash-table :test #'equal :size (max 16 (length sha-list))))
        (blob-files (or pre-blob-files
                        (make-hash-table :test #'equal :size (max 16 (length sha-list)))))
        (sub-files (make-hash-table :test #'equal))
        (results (make-vector (length entries) nil))
        (jobs nil)
        (idx 0)
        (file-name-handler-alist nil)
        (write-region-inhibit-fsync t)
        (coding-system-for-write 'no-conversion))
    (when cat-buf
      (omnidiff-git--for-each-cat-batch-entry
       cat-buf sha-list
       (lambda (sha start end _sz)
         (puthash sha (cons start end) spans))))
    (when (and cat-buf (hash-table-p image-blobs))
      (let ((visual-limit (if (> omnidiff-image-auto-load-limit 0)
                              (* 2 omnidiff-image-auto-load-limit)
                            most-positive-fixnum))
            (seeded-cell (list 0)))
        (dolist (e entries)
          (when (and (< (car seeded-cell) visual-limit)
                     (or (omnidiff-image-path-looks-visual-p (omnidiff--git-entry-path e))
                         (omnidiff-image-path-looks-visual-p (omnidiff--git-entry-new-path e))))
            (dolist (sha (list (omnidiff--git-entry-old-sha e)
                               (omnidiff--git-entry-new-sha e)))
              (when-let* ((span (and (not (omnidiff--zero-oid-p sha))
                                     (gethash sha spans))))
                (omnidiff-git--maybe-seed-visual-blob
                 image-blobs sha cat-buf (car span) (cdr span)
                 seeded-cell visual-limit)))))))
    (cl-flet ((write-blob (sha path)
                (let ((key (omnidiff--blob-key sha path)))
                  (or (gethash key blob-files)
                      (when-let* ((span (and cat-buf (gethash sha spans))))
                        (let ((dest (omnidiff-git-blob-dest sha path (funcall ensure-tmpdir))))
                          (with-current-buffer cat-buf
                            (write-region (car span) (cdr span) dest nil 'silent))
                          (puthash key dest blob-files))))))
              (sub-file (sha)
                (or (gethash sha sub-files)
                    (puthash sha
                             (omnidiff-git-write-small-temp
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
               (old-file (cond
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
                           (let ((full (omnidiff-engine-expand-repo-path new-path dir)))
                             (cond
                              ((equal nm "120000")
                               (omnidiff-git-write-small-temp
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
          (push (list idx path old-file new-file new-path cli-args om nm os ns) jobs)
          (setq idx (1+ idx)))))
    (cons results (nreverse jobs))))

;;; Flattened async pipeline stages

(defun omnidiff-git--request-current-p (req)
  "Return non-nil when REQ is still the active live request for its buffer."
  (let ((buffer (omnidiff--request-buffer req)))
    (and (not (omnidiff--request-cancelled req))
         (buffer-live-p buffer)
         (eq req (buffer-local-value 'omnidiff--active-request buffer))
         (eql (omnidiff--request-id req)
              (buffer-local-value 'omnidiff--request-id buffer)))))

(defun omnidiff-git--ensure-tmpdir (req)
  "Return REQ's temporary blob directory, creating it lazily if needed."
  (or (omnidiff--request-tmpdir req)
      (setf (omnidiff--request-tmpdir req)
            (make-temp-file "omnidiff-blobs-" t))))

(defun omnidiff-git--ensure-image-blobs (req)
  "Return REQ's seeded visual blob table, creating it lazily if needed."
  (or (omnidiff--request-image-blobs req)
      (setf (omnidiff--request-image-blobs req)
            (make-hash-table :test #'equal))))

(defun omnidiff-git--fail-request (req msg)
  "Abort REQ if still current, release resources, and report MSG."
  (when (omnidiff-git--request-current-p req)
    (setf (omnidiff--request-failed-event req) nil)
    (omnidiff--cleanup-request-resources req)
    (with-current-buffer (omnidiff--request-buffer req)
      (setq omnidiff--active-request nil))
    (message "%s" msg)))

(defun omnidiff-git--report-failure (req)
  "Report REQ failure once both main process exit and stderr EOF have arrived."
  (when (and (omnidiff-git--request-current-p req)
             (omnidiff--request-failed-event req)
             (omnidiff--request-stderr-done req))
    (let ((ev (omnidiff--request-failed-event req))
          (diag (string-trim (apply #'concat (nreverse (omnidiff--request-err-chunks req))))))
      (omnidiff-git--fail-request
       req
       (format "Omnidiff failed (%s)%s"
               ev
               (if (string-empty-p diag) "" (concat ": " diag)))))))

(defun omnidiff-git--commit-and-render (req raw-or-diff)
  "Commit completed REQ diff RAW-OR-DIFF to its Overview buffer and render."
  (omnidiff--cleanup-request-resources req)
  (when (omnidiff-git--request-current-p req)
    (let* ((buffer (omnidiff--request-buffer req))
           (dir (omnidiff--request-repo-dir req))
           (command (omnidiff--request-command req))
           (diff (if (omnidiff-diff-p raw-or-diff)
                     raw-or-diff
                   (omnidiff--parse-serial-json-output raw-or-diff dir)))
           (final-layout (omnidiff--request-layout req))
           (final-msg (omnidiff--request-last-msg req))
           (preserve-p (omnidiff--request-preserve-state req))
           (seeded (omnidiff--request-image-blobs req))
           (saved-loc nil))
      (setf (omnidiff--request-image-blobs req) nil)
      (with-current-buffer buffer
        (when preserve-p
          (setq saved-loc
                (list (get-text-property (point) 'omnidiff-file)
                      (get-text-property (point) 'omnidiff-old-line)
                      (get-text-property (point) 'omnidiff-new-line)
                      (get-text-property (point) 'omnidiff-side)
                      (get-text-property (point) 'omnidiff-line))))
        (omnidiff-image-cleanup preserve-p)
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
              omnidiff--label (omnidiff--request-label req)
              omnidiff--preset (omnidiff--request-preset req)
              omnidiff--layout final-layout
              omnidiff--whole-updates (omnidiff--request-whole-updates req)
              omnidiff--paint-reindent-moves (omnidiff--request-paint-reindent-moves req)
              omnidiff--origin-buffer (or (omnidiff--request-origin-buf req)
                                          omnidiff--origin-buffer)
              omnidiff--diff diff))
      (omnidiff-finalize-git-render
       buffer diff saved-loc
       (omnidiff--request-auto-dual req)
       (omnidiff--request-target-file req)
       (omnidiff--request-target-line req)
       (omnidiff--request-target-side req)
       final-msg))))

(defun omnidiff-git--spawn-command (req cmd serial-p on-success)
  "Spawn Git command CMD for REQ and invoke ON-SUCCESS with stdout on exit 0.
When SERIAL-P is non-nil, binds `GIT_EXTERNAL_DIFF' to the JSON wrapper.
Localizes stderr chunks per stage and checks stderr process identity so a
late close event from an earlier stage cannot pollute a fallback stage."
  (let* ((buffer (omnidiff--request-buffer req))
         (default-directory (omnidiff--request-repo-dir req))
         (wrapper (when serial-p
                    (omnidiff--build-ext-diff-wrapper
                     (omnidiff--request-bin req)
                     (omnidiff--request-worker-flags req)
                     t)))
         (process-environment
          (if wrapper
              (cons (concat "GIT_EXTERNAL_DIFF=" wrapper) process-environment)
            process-environment))
         (raw-chunks nil)
         (stage-err-chunks nil)
         (stderr-proc nil)
         (proc nil)
         (created-ok nil))
    (setf (omnidiff--request-failed-event req) nil
          (omnidiff--request-stderr-done req) nil
          (omnidiff--request-err-chunks req) nil)
    (unwind-protect
        (progn
          (setq stderr-proc
                (make-pipe-process
                 :name (concat (buffer-name buffer) " stderr")
                 :buffer nil
                 :noquery t
                 :filter (lambda (p chunk)
                           (when (and (not (omnidiff--request-cancelled req))
                                      (eq p (omnidiff--request-stderr-proc req)))
                             (push chunk stage-err-chunks)))
                 :sentinel
                 (lambda (p _event)
                   (when (and (memq (process-status p) '(closed exit signal))
                              (eq p (omnidiff--request-stderr-proc req)))
                     (setf (omnidiff--request-stderr-done req) t
                           (omnidiff--request-err-chunks req) stage-err-chunks)
                     (omnidiff-git--report-failure req)))))
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
                     (let ((current-p (and (omnidiff-git--request-current-p req)
                                           (eq p (omnidiff--request-proc req)))))
                       (cond
                        ((not current-p)
                         (when (process-live-p stderr-proc)
                           (delete-process stderr-proc))
                         (omnidiff--cleanup-request-resources req))
                        ((and (eq (process-status p) 'exit)
                              (zerop (process-exit-status p)))
                         (when (eq stderr-proc (omnidiff--request-stderr-proc req))
                           (setf (omnidiff--request-stderr-proc req) nil))
                         (when (process-live-p stderr-proc)
                           (delete-process stderr-proc))
                         (condition-case err
                             (funcall on-success
                                      (apply #'concat (nreverse raw-chunks)))
                           (error
                            (omnidiff-git--fail-request
                             req
                             (format "Omnidiff failed: %s"
                                     (error-message-string err))))))
                        (t
                         (setf (omnidiff--request-failed-event req)
                               (string-trim event)
                               (omnidiff--request-err-chunks req)
                               stage-err-chunks)
                         (omnidiff-git--report-failure req))))))))
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
        (when (and (buffer-live-p buffer)
                   (eq req (buffer-local-value 'omnidiff--active-request buffer)))
          (with-current-buffer buffer
            (setq omnidiff--active-request nil)))
        (omnidiff--cleanup-request-resources req)))
    proc))

(defun omnidiff-git--on-worker-exit (req job proc ev chunks)
  "Handle completion of parallel `omnidiff' worker PROC for JOB in REQ."
  (setf (omnidiff--request-workers req)
        (delq proc (omnidiff--request-workers req)))
  (if (not (omnidiff-git--request-current-p req))
      (omnidiff--cleanup-request-resources req)
    (pcase-let* ((`(,ji ,jpath ,jfa ,jfb ,jnew _ ,jom ,jnm ,jos ,jns) job)
                 (results (omnidiff--request-results req)))
      (let ((gc-cons-threshold most-positive-fixnum)
            (gc-cons-percentage 0.8))
        (aset results ji
              (if (and (eq (process-status proc) 'exit)
                       (zerop (process-exit-status proc)))
                  (condition-case parse-err
                      (let ((payload (apply #'concat (nreverse chunks))))
                        (omnidiff-git-file-from-json
                         jpath jfa jfb jnew
                         (omnidiff-engine-parse-json payload)
                         jom jnm jos jns))
                    (error
                     (omnidiff-git--make-error-file
                      jpath jfa jfb jnew jom jnm jos jns
                      (format "omnidiff parse error: %s"
                              (error-message-string parse-err)))))
                (omnidiff-git--make-error-file
                 jpath jfa jfb jnew jom jnm jos jns
                 (format "omnidiff failed (%s)" (string-trim ev))))))
      (cl-decf (omnidiff--request-active req))
      (cl-incf (omnidiff--request-done-jobs req))
      (cl-incf (omnidiff--request-done-files req))
      (if (= (omnidiff--request-done-jobs req) (omnidiff--request-total-jobs req))
          (condition-case render-err
              (omnidiff-git--commit-and-render
               req (make-omnidiff-diff
                    :preamble (omnidiff--request-preamble req)
                    :files (append results nil)))
            (error
             (omnidiff-git--fail-request
              req (format "Omnidiff render failed: %s"
                          (error-message-string render-err)))))
        (let ((now (float-time))
              (total-files (omnidiff--request-total-files req)))
          (when (and (>= total-files 16)
                     (>= (- now (omnidiff--request-last-progress-time req)) 0.35))
            (setf (omnidiff--request-last-progress-time req) now)
            (let ((message-log-max nil)
                  (msg (format "Running omnidiff (%s): %d/%d..."
                               (omnidiff--request-label req)
                               (omnidiff--request-done-files req)
                               total-files)))
              (setf (omnidiff--request-last-msg req) msg)
              (message "%s" msg))))
        (omnidiff-git--pump-workers req)))))

(defun omnidiff-git--pump-workers (req)
  "Spawn queued parallel `omnidiff' workers for REQ up to `omnidiff-max-parallel'."
  (let ((max-w (max 1 omnidiff-max-parallel))
        (bin (omnidiff--request-bin req))
        (worker-flags (omnidiff--request-worker-flags req))
        (default-directory (omnidiff--request-repo-dir req)))
    (while (and (omnidiff-git--request-current-p req)
                (< (omnidiff--request-active req) max-w)
                (omnidiff--request-queue req))
      (let* ((job (pop (omnidiff--request-queue req)))
             (ji (nth 0 job))
             (jargs (nth 5 job))
             (chunks nil)
             (wp nil))
        (condition-case err
            (progn
              (setq wp
                    (make-process
                     :name (format "omnidiff-%d" ji)
                     :buffer nil
                     :command (cons bin (append worker-flags (cons "--" jargs)))
                     :connection-type 'pipe
                     :coding 'utf-8-unix
                     :noquery t
                     :filter (lambda (_p chunk)
                               (unless (omnidiff--request-cancelled req)
                                 (push chunk chunks)))
                     :sentinel
                     (lambda (p ev)
                       (when (memq (process-status p) '(exit signal))
                         (omnidiff-git--on-worker-exit req job p ev chunks)))))
              (cl-incf (omnidiff--request-active req))
              (push wp (omnidiff--request-workers req)))
          (error
           (omnidiff-git--fail-request
            req (format "Omnidiff failed to start worker: %s"
                        (error-message-string err)))))))))

(defun omnidiff-git--launch-workers (req preamble results jobs)
  "Begin parallel worker execution for REQ over RESULTS and JOBS."
  (if (not (omnidiff-git--request-current-p req))
      (omnidiff--cleanup-request-resources req)
    (if (null jobs)
        (omnidiff-git--commit-and-render
         req (make-omnidiff-diff :preamble preamble :files (append results nil)))
      (let ((total-files (length results))
            (total-jobs (length jobs)))
        (setf (omnidiff--request-preamble req) preamble
              (omnidiff--request-results req) results
              (omnidiff--request-queue req) jobs
              (omnidiff--request-active req) 0
              (omnidiff--request-done-jobs req) 0
              (omnidiff--request-total-jobs req) total-jobs
              (omnidiff--request-done-files req) (- total-files total-jobs)
              (omnidiff--request-total-files req) total-files
              (omnidiff--request-last-progress-time req) (float-time))
        (omnidiff-git--pump-workers req)))))

(defun omnidiff-git--run-cat-batch (req batch wire-cap on-done)
  "Run `git cat-file --batch' for REQ on SHA list BATCH bounded by WIRE-CAP.
Calls `(ON-DONE CAT-BUF)' on success, or falls back to serial mode on overflow."
  (let ((cat-buf (generate-new-buffer " *omnidiff-cat-file*"))
        (default-directory (omnidiff--request-repo-dir req))
        (cat-bytes 0)
        (cat-proc nil))
    (setf (omnidiff--request-cat-overflow req) nil)
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
                   (unless (or (omnidiff--request-cancelled req)
                               (omnidiff--request-cat-overflow req))
                     (setq cat-bytes (+ cat-bytes (length chunk)))
                     (if (and wire-cap (> wire-cap 0) (> cat-bytes wire-cap))
                         (progn
                           (setf (omnidiff--request-cat-overflow req) t)
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
                         (when (omnidiff-git--request-current-p req)
                           (condition-case stage-err
                               (cond
                                ((omnidiff--request-cat-overflow req)
                                 (when (buffer-live-p cat-buf)
                                   (kill-buffer cat-buf))
                                 (omnidiff-git--spawn-command
                                  req (omnidiff--request-command req) t
                                  (lambda (raw)
                                    (omnidiff-git--commit-and-render req raw))))
                                ((and (eq (process-status cp) 'exit)
                                      (zerop (process-exit-status cp)))
                                 (funcall on-done cat-buf))
                                (t
                                 (omnidiff-git--fail-request
                                  req
                                  (format "Omnidiff failed (git cat-file exited with status %s)"
                                          (process-exit-status cp)))))
                             (error
                              (omnidiff-git--fail-request
                               req (format "Omnidiff failed: %s"
                                           (error-message-string stage-err))))))
                       (when (buffer-live-p cat-buf)
                         (kill-buffer cat-buf)))))))
          (push cat-proc (omnidiff--request-workers req))
          (when (and (not (omnidiff--request-cat-overflow req))
                     (process-live-p cat-proc))
            (process-send-string cat-proc (concat (string-join batch "\n") "\n")))
          (when (and (not (omnidiff--request-cat-overflow req))
                     (process-live-p cat-proc))
            (process-send-eof cat-proc)))
      (error
       ;; If filter already triggered `cat-overflow', let the sentinel perform
       ;; serial fallback instead of failing the request.
       (unless (omnidiff--request-cat-overflow req)
         (when (and cat-proc (process-live-p cat-proc))
           (delete-process cat-proc))
         (when (buffer-live-p cat-buf)
           (kill-buffer cat-buf))
         (omnidiff-git--fail-request
          req (format "Omnidiff failed to start git cat-file: %s"
                      (error-message-string err))))))))

(defun omnidiff-git--run-single-batch (req entries batch budget)
  "Fetch single SHA BATCH for REQ and launch workers over ENTRIES."
  (omnidiff-git--run-cat-batch
   req batch
   (when (> budget 0) (+ budget (* 128 (length batch))))
   (lambda (cat-buf)
     (pcase-let ((`(,results . ,jobs)
                  (omnidiff--prepare-parallel-work
                   cat-buf batch entries
                   (lambda () (omnidiff-git--ensure-tmpdir req))
                   (omnidiff--request-repo-dir req)
                   (omnidiff-git--ensure-image-blobs req))))
       (omnidiff-git--launch-workers
        req (omnidiff--request-preamble req) results jobs)))))

(defun omnidiff-git--step-multi-batch (req budget)
  "Process the next queued `cat-file --batch' chunk for REQ, or launch workers."
  (let ((rem (omnidiff--request-cat-rem-batches req)))
    (if (null rem)
        (pcase-let ((`(,results . ,jobs)
                     (omnidiff--prepare-parallel-work
                      nil nil
                      (omnidiff--request-cat-entries req)
                      (lambda () (omnidiff-git--ensure-tmpdir req))
                      (omnidiff--request-repo-dir req)
                      (omnidiff--request-image-blobs req)
                      (omnidiff--request-cat-blob-files req))))
          (omnidiff-git--launch-workers
           req (omnidiff--request-preamble req) results jobs))
      (let ((batch (pop (omnidiff--request-cat-rem-batches req))))
        (omnidiff-git--run-cat-batch
         req batch
         (when (> budget 0) (+ budget (* 128 (length batch))))
         (lambda (cat-buf)
           (omnidiff--materialize-cat-batch
            cat-buf batch
            (omnidiff--request-cat-sha-targets req)
            (omnidiff--request-cat-visual-shas req)
            (lambda () (omnidiff-git--ensure-tmpdir req))
            (omnidiff--request-image-blobs req)
            (omnidiff--request-cat-blob-files req)
            (omnidiff--request-cat-seeded-cell req)
            (omnidiff--request-cat-visual-limit req))
           (omnidiff-git--step-multi-batch req budget)))))))

(defun omnidiff-git--run-multi-batches (req entries sha-count batches budget)
  "Stream BATCHES of blobs for REQ onto disk and launch parallel workers."
  (let ((maps (omnidiff--collect-sha-batch-maps entries)))
    (omnidiff-git--ensure-image-blobs req)
    (setf (omnidiff--request-cat-entries req) entries
          (omnidiff--request-cat-rem-batches req) batches
          (omnidiff--request-cat-sha-targets req) (car maps)
          (omnidiff--request-cat-visual-shas req) (cdr maps)
          (omnidiff--request-cat-blob-files req)
          (make-hash-table :test #'equal :size (max 16 sha-count))
          (omnidiff--request-cat-seeded-cell req) (list 0)
          (omnidiff--request-cat-visual-limit req)
          (if (> omnidiff-image-auto-load-limit 0)
              (* 2 omnidiff-image-auto-load-limit)
            most-positive-fixnum))
    (omnidiff-git--step-multi-batch req budget)))

(defun omnidiff-git--start-parallel-stage (req raw-meta)
  "Parse RAW-META from `git --raw' for REQ and schedule blob fetch + workers."
  (let ((parsed (omnidiff--parse-raw-git-entries
                 raw-meta (omnidiff--request-par-kind req))))
    (cond
     ((eq parsed 'fallback-serial)
      (omnidiff-git--spawn-command
       req (omnidiff--request-command req) t
       (lambda (raw) (omnidiff-git--commit-and-render req raw))))
     ((null (cdr parsed))
      (omnidiff-git--commit-and-render
       req (make-omnidiff-diff :preamble (car parsed) :files nil)))
     (t
      (let* ((preamble (car parsed))
             (entries (cdr parsed))
             (repo-dir (omnidiff--request-repo-dir req))
             (budget omnidiff-max-parallel-blob-bytes)
             (wt-overflow-p
              (and (integerp budget) (> budget 0) repo-dir
                   (cl-some
                    (lambda (e)
                      (let ((st (omnidiff--git-entry-status e))
                            (nm (omnidiff--git-entry-new-mode e))
                            (ns (omnidiff--git-entry-new-sha e))
                            (new-path (omnidiff--git-entry-new-path e)))
                        (when (and (omnidiff--zero-oid-p ns)
                                   (not (string-prefix-p "D" st))
                                   (not (equal nm "160000"))
                                   (not (equal nm "120000"))
                                   (stringp new-path)
                                   (not (string-empty-p new-path)))
                          (let ((full (omnidiff-engine-expand-repo-path new-path repo-dir)))
                            (when (and (not (file-symlink-p full))
                                       (file-regular-p full))
                              (let* ((attrs (file-attributes full))
                                     (sz (and attrs (file-attribute-size attrs))))
                                (and (integerp sz) (> sz budget))))))))
                    entries))))
        (if wt-overflow-p
            (omnidiff-git--spawn-command
             req (omnidiff--request-command req) t
             (lambda (raw) (omnidiff-git--commit-and-render req raw)))
          (let ((seen (make-hash-table :test #'equal))
                (sha-list nil))
            (setf (omnidiff--request-preamble req) preamble)
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
            (cond
             ((null sha-list)
              (pcase-let ((`(,results . ,jobs)
                           (omnidiff--prepare-parallel-work
                            nil nil entries
                            (lambda () (omnidiff-git--ensure-tmpdir req))
                            repo-dir
                            nil)))
                (omnidiff-git--launch-workers req preamble results jobs)))
             ((or (not (integerp budget)) (<= budget 0))
              (omnidiff-git--run-single-batch req entries sha-list budget))
             (t
              (let ((check-chunks nil)
                    (default-directory repo-dir)
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
                                 (when (omnidiff-git--request-current-p req)
                                   (condition-case stage-err
                                       (if (and (eq (process-status cp) 'exit)
                                                (zerop (process-exit-status cp)))
                                           (let* ((sizes (omnidiff--parse-batch-check-sizes
                                                          (apply #'concat (nreverse check-chunks))))
                                                  (batches (omnidiff--partition-sha-batches
                                                            sha-list sizes budget)))
                                             (cond
                                              ((eq batches 'overflow)
                                               (omnidiff-git--spawn-command
                                                req (omnidiff--request-command req) t
                                                (lambda (raw)
                                                  (omnidiff-git--commit-and-render req raw))))
                                              ((<= (length batches) 1)
                                               (omnidiff-git--run-single-batch
                                                req entries (or (car batches) sha-list) budget))
                                              (t
                                               (omnidiff-git--run-multi-batches
                                                req entries (length sha-list) batches budget))))
                                         (omnidiff-git--fail-request
                                          req
                                          (format "Omnidiff failed (git cat-file --batch-check exited with status %s)"
                                                  (process-exit-status cp))))
                                     (error
                                      (omnidiff-git--fail-request
                                       req (format "Omnidiff failed: %s"
                                                   (error-message-string stage-err))))))))))
                      (push check-proc (omnidiff--request-workers req))
                      (when (process-live-p check-proc)
                        (process-send-string check-proc (concat (string-join sha-list "\n") "\n")))
                      (when (process-live-p check-proc)
                        (process-send-eof check-proc)))
                  (error
                   (omnidiff-git--fail-request
                    req (format "Omnidiff failed to start git cat-file --batch-check: %s"
                                (error-message-string err)))))))))))))))

;;; Public async runner entrypoint

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
                            (with-current-buffer buffer (omnidiff-git-effective-layout))))
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
         (req nil)
         (start-msg (or status-message
                        (format "Running omnidiff (%s)..." active-label))))
    (with-current-buffer buffer
      (unless (derived-mode-p 'omnidiff-mode)
        (omnidiff-mode))
      ;; Cancel any superseded request and advance request id before spawning.
      (omnidiff-git-cancel-request)
      (let ((req-id (1+ omnidiff--request-id)))
        (setq omnidiff--request-id req-id
              req (make-omnidiff--request
                   :id req-id
                   :buffer buffer
                   :bin omnidiff-bin
                   :worker-flags worker-flags
                   :command command
                   :par-kind (car par-spec)
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
                   :preserve-state (and preserve-state-p t)))))
    (let ((proc (if par-spec
                    (omnidiff-git--spawn-command
                     req (cdr par-spec) nil
                     (lambda (raw-meta)
                       (omnidiff-git--start-parallel-stage req raw-meta)))
                  (omnidiff-git--spawn-command
                   req command t
                   (lambda (raw)
                     (omnidiff-git--commit-and-render req raw))))))
      (message "%s" start-msg)
      proc)))

(provide 'omnidiff-git)
;;; omnidiff-git.el ends here
