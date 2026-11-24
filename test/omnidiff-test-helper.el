;;; omnidiff-test-helper.el --- Shared ERT fixtures and helpers for omnidiff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; This file is part of omnidiff.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Shared test fixtures, binary PNG samples, temp-repo/buffer lifecycle macros,
;; async polling helpers, and buffer/overlay inspectors used by the modular
;; `test/omnidiff-*-test.el' ERT suites.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'omnidiff)

;;; Package paths & protocol fixtures

(defconst omnidiff-test--pkg-dir
  (let ((this-file (or load-file-name buffer-file-name)))
    (if this-file
        (file-name-directory
         (directory-file-name
          (file-name-directory (file-truename this-file))))
      default-directory))
  "Root directory of the `omnidiff.el' package.")

(defun omnidiff-test--file-meta (path old-file new-file &optional new-path)
  "Format a NUL-delimited file sentinel header for parser tests."
  (concat omnidiff-git-file-sentinel
          path "\x00" old-file "\x00" new-file "\x00" (or new-path "") "\x00"))

;;; Diff model builders

(defun omnidiff-test--make-hunk (op sr sc er ec &optional move-target ref-line)
  "Build an `omnidiff-json-hunk' with OP over (SR,SC)..(ER,EC).
Optional MOVE-TARGET is an `omnidiff-json-range' and REF-LINE is a 0-based
scope reference row."
  (make-omnidiff-json-hunk
   :operation op
   :range (make-omnidiff-json-range
           :start-row sr :start-col sc :end-row er :end-col ec)
   :move-target move-target
   :reference-line ref-line))

(cl-defun omnidiff-test--make-file
    (&key (path "") target-path (status 'modified) preamble
          old-mode new-mode old-oid new-oid note
          (added 0) (removed 0) (updated 0) (moved 0) (max-line 0)
          json-diff before-source after-source
          before-disk-path after-disk-path
          before-hunks after-hunks language encoding
          picture summary binary-p container large-residual)
  "Build an `omnidiff-file' storing diff metadata inside `:json-diff'.
When JSON-DIFF is nil and any JSON-level field (PICTURE, SUMMARY, BINARY-P,
CONTAINER, LARGE-RESIDUAL, BEFORE-HUNKS, AFTER-HUNKS, LANGUAGE, ENCODING) is
supplied, synthesize an `omnidiff-json-diff' automatically.  TARGET-PATH
defaults to PATH."
  (let* ((target (or target-path path))
         (need-sides (or before-hunks after-hunks language encoding
                         (and (or before-source after-source)
                              (not picture) (not container) (not binary-p))))
         (jd (or json-diff
                 (when (or picture summary binary-p container large-residual
                           need-sides)
                   (make-omnidiff-json-diff
                    :before (when need-sides
                              (make-omnidiff-json-side
                               :path (if (eq status 'added) "/dev/null" path)
                               :language language
                               :encoding encoding
                               :hunks before-hunks))
                    :after (when need-sides
                             (make-omnidiff-json-side
                              :path (if (eq status 'deleted) "/dev/null" target)
                              :language language
                              :encoding encoding
                              :hunks after-hunks))
                    :picture picture
                    :summary summary
                    :binary-p (and binary-p t)
                    :container container
                    :large-residual (and large-residual t))))))
    (make-omnidiff-file
     :path path
     :target-path target
     :status status
     :preamble preamble
     :old-mode old-mode
     :new-mode new-mode
     :old-oid old-oid
     :new-oid new-oid
     :note note
     :added added
     :removed removed
     :updated updated
     :moved moved
     :max-line max-line
     :json-diff jd
     :before-source before-source
     :after-source after-source
     :before-disk-path before-disk-path
     :after-disk-path after-disk-path)))

(cl-defun omnidiff-test--make-json-file
    (path before-src after-src
          &key (status 'modified) new-path
          before-hunks after-hunks
          old-mode new-mode old-oid new-oid
          summary language encoding large-residual)
  "Build a JSON-backed `omnidiff-file' with computed stats via git pipeline."
  (let* ((target (or new-path path))
         (old-file (if (eq status 'added) "/dev/null" path))
         (new-file (if (eq status 'deleted) "/dev/null" target))
         (jd (make-omnidiff-json-diff
              :before (make-omnidiff-json-side
                       :path old-file
                       :language language
                       :encoding encoding
                       :hunks before-hunks)
              :after (make-omnidiff-json-side
                      :path new-file
                      :language language
                      :encoding encoding
                      :hunks after-hunks)
              :summary summary
              :large-residual (and large-residual t))))
    (omnidiff-git-file-from-json
     path old-file new-file target jd
     old-mode new-mode old-oid new-oid nil nil
     before-src after-src)))

(defun omnidiff-test--make-diff (files &optional preamble)
  "Wrap FILES (a single `omnidiff-file' or list) into an `omnidiff-diff'."
  (make-omnidiff-diff
   :preamble preamble
   :files (if (omnidiff-file-p files) (list files) files)))

;;; Binary PNG & archive fixtures

(defconst omnidiff-test--png-red-2x2
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
   #x00 #x00 #x00 #x02 #x00 #x00 #x00 #x02 #x08 #x02 #x00 #x00 #x00 #xfd #xd4 #x9a
   #x73 #x00 #x00 #x00 #x10 #x49 #x44 #x41 #x54 #x78 #x9c #x63 #xb8 #xa3 #x61 #x03
   #x44 #x0c #x10 #x0a #x00 #x25 #x8e #x05 #x01 #xf5 #x5e #xb0 #x29 #x00 #x00 #x00
   #x00 #x49 #x45 #x4e #x44 #xae #x42 #x60 #x82)
  "Minimal valid 2x2 opaque red RGB8 PNG for image diff tests.")

(defconst omnidiff-test--png-blue-2x2
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
   #x00 #x00 #x00 #x02 #x00 #x00 #x00 #x02 #x08 #x02 #x00 #x00 #x00 #xfd #xd4 #x9a
   #x73 #x00 #x00 #x00 #x10 #x49 #x44 #x41 #x54 #x78 #x9c #x63 #xd0 #xa8 #x78 #x06
   #x44 #x0c #x10 #x0a #x00 #x27 #xbe #x06 #x19 #x55 #x8d #xca #x90 #x00 #x00 #x00
   #x00 #x49 #x45 #x4e #x44 #xae #x42 #x60 #x82)
  "Minimal valid 2x2 opaque blue RGB8 PNG for image diff tests.")

(defconst omnidiff-test--png-wide-4x2
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
   #x00 #x00 #x00 #x04 #x00 #x00 #x00 #x02 #x08 #x02 #x00 #x00 #x00 #xf0 #xca #xea
   #x34 #x00 #x00 #x00 #x10 #x49 #x44 #x41 #x54 #x78 #x9c #x63 #xd0 #x38 #x91 #x02
   #x47 #x0c #xc8 #x1c #x00 #x88 #x5a #x0a #xa1 #xca #x03 #xe0 #x28 #x00 #x00 #x00
   #x00 #x49 #x45 #x4e #x44 #xae #x42 #x60 #x82)
  "Minimal valid 4x2 opaque green RGB8 PNG with different aspect ratio.")

(defconst omnidiff-test--png-rgba-base-4x4
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
   #x00 #x00 #x00 #x04 #x00 #x00 #x00 #x04 #x08 #x06 #x00 #x00 #x00 #xa9 #xf1 #x9e
   #x7e #x00 #x00 #x00 #x12 #x49 #x44 #x41 #x54 #x78 #xda #x63 #x68 #x70 #x50 #x68
   #x40 #xc6 #x0c #xa4 #x0b #x00 #x00 #xed #x62 #x16 #x01 #xec #xa6 #x0e #xe9 #x00
   #x00 #x00 #x00 #x49 #x45 #x4e #x44 #xae #x42 #x60 #x82)
  "Valid 4x4 semi-transparent RGBA8 PNG filled with rgba(128, 64, 32, 128).")

(defconst omnidiff-test--png-rgba-alpha-4x4
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
   #x00 #x00 #x00 #x04 #x00 #x00 #x00 #x04 #x08 #x06 #x00 #x00 #x00 #xa9 #xf1 #x9e
   #x7e #x00 #x00 #x00 #x12 #x49 #x44 #x41 #x54 #x78 #xda #x63 #x68 #x70 #x50 #x38
   #x81 #x8c #x19 #x48 #x17 #x00 #x00 #x7f #xb1 #x1a #x81 #x72 #xbf #x9f #xfc #x00
   #x00 #x00 #x00 #x49 #x45 #x4e #x44 #xae #x42 #x60 #x82)
  "Valid 4x4 RGBA8 PNG filled with rgba(128, 64, 32, 200) (alpha changed).")

(defconst omnidiff-test--png-rgba-rgb-4x4
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
   #x00 #x00 #x00 #x04 #x00 #x00 #x00 #x04 #x08 #x06 #x00 #x00 #x00 #xa9 #xf1 #x9e
   #x7e #x00 #x00 #x00 #x12 #x49 #x44 #x41 #x54 #x78 #xda #x63 #x38 #xe1 #xa0 #xd0
   #x80 #x8c #x19 #x48 #x17 #x00 #x00 #x8d #x31 #x1a #x81 #xdc #x3b #x7f #xdf #x00
   #x00 #x00 #x00 #x49 #x45 #x4e #x44 #xae #x42 #x60 #x82)
  "Valid 4x4 RGBA8 PNG filled with rgba(200, 64, 32, 128) (RGB changed).")

(defconst omnidiff-test--png-rgba-partial-alpha-4x4
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
   #x00 #x00 #x00 #x04 #x00 #x00 #x00 #x04 #x08 #x06 #x00 #x00 #x00 #xa9 #xf1 #x9e
   #x7e #x00 #x00 #x00 #x19 #x49 #x44 #x41 #x54 #x78 #xda #x63 #x68 #x70 #x50 #x38
   #x01 #xc5 #x0d #x20 #xcc #x80 #x4d #xa0 #x01 #x19 #x63 #x08 #x00 #x00 #x29 #xa1
   #x17 #x21 #xdc #x53 #x28 #x08 #x00 #x00 #x00 #x00 #x49 #x45 #x4e #x44 #xae #x42
   #x60 #x82)
  "Valid 4x4 RGBA8 PNG: top-left 2x2 alpha=200, remaining pixels alpha=128.")

(defun omnidiff-test--write-bin (path bytes)
  "Write unibyte BYTES verbatim to PATH."
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert bytes))))

(defun omnidiff-test--make-zip (zip-path entries)
  "Create a ZIP archive at ZIP-PATH from ENTRIES `((MEMBER . FILE) ...)'."
  (let ((python (or (executable-find "python3")
                    (error "python3 not found")))
        (args (list "-c"
                    (concat "import sys, zipfile\n"
                            "with zipfile.ZipFile(sys.argv[1], 'w') as zf:\n"
                            "    a = sys.argv[2:]\n"
                            "    for i in range(0, len(a), 2):\n"
                            "        zf.write(a[i + 1], a[i])\n")
                    zip-path)))
    (dolist (entry entries)
      (setq args (nconc args (list (car entry) (cdr entry)))))
    (unless (zerop (apply #'call-process python nil nil nil args))
      (error "Failed to build test zip %s" zip-path))
    zip-path))

;;; Temp directories, Git repos, GUI image simulation & cleanup

(defmacro omnidiff-test--with-temp-dir (dir &rest body)
  "Bind DIR to a fresh temporary directory, run BODY, and delete DIR."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,dir (make-temp-file "omnidiff-test-" t)))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p ,dir)
         (delete-directory ,dir t)))))

(defun omnidiff-test--git (dir &rest args)
  "Run `git' with ARGS in DIR, returning trimmed stdout or signaling error."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (let ((rc (apply #'call-process "git" nil t nil args)))
        (unless (zerop rc)
          (error "git %S failed in %s (exit %s): %s"
                 args dir rc (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defmacro omnidiff-test--with-git-repo (dir &rest body)
  "Create an initialized Git repo bound to DIR, run BODY, and clean up.
Configures `user.name' and `user.email', sets `default-directory' to DIR,
and removes DIR in `unwind-protect'."
  (declare (indent 1) (debug (symbolp body)))
  `(omnidiff-test--with-temp-dir ,dir
     (let ((default-directory (file-name-as-directory ,dir)))
       (omnidiff-test--git ,dir "init" "-q" "-b" "main")
       (omnidiff-test--git ,dir "config" "user.name" "Test")
       (omnidiff-test--git ,dir "config" "user.email" "test@example.com")
       ,@body)))

(defvar omnidiff-test--gui-viewport-px '(1200 . 600)
  "Default `(WIDTH . HEIGHT)' viewport for `omnidiff-test--with-gui-image'.")

(defmacro omnidiff-test--with-gui-image (&rest body)
  "Run BODY with GUI image rendering enabled in batch/headless Emacs.
Overrides `omnidiff-image--display-graphic-p' and
`omnidiff-image--viewport-px' using `omnidiff-test--gui-viewport-px'."
  (declare (indent 0) (debug (body)))
  `(cl-letf (((symbol-function 'omnidiff-image--display-graphic-p)
              (lambda () t))
             ((symbol-function 'omnidiff-image--viewport-px)
              (lambda () omnidiff-test--gui-viewport-px)))
     ,@body))

(defun omnidiff-test--cleanup-session (&optional session restore-windows)
  "Close dual SESSION via `omnidiff-dual-close' if still active.
When SESSION is nil, close all active sessions in `omnidiff-dual--sessions'.
RESTORE-WINDOWS defaults to nil so headless tests keep window state stable."
  (if session
      (when (and (omnidiff-dual-session-p session)
                 (memq session omnidiff-dual--sessions))
        (omnidiff-dual-close session restore-windows))
    (dolist (sess (copy-sequence omnidiff-dual--sessions))
      (when (and (omnidiff-dual-session-p sess)
                 (memq sess omnidiff-dual--sessions))
        (omnidiff-dual-close sess restore-windows)))))

(defmacro omnidiff-test--with-buffer-cleanup (&rest body)
  "Run BODY, then close newly opened dual sessions and kill created buffers."
  (declare (indent 0) (debug (body)))
  (let ((orig-bufs (make-symbol "orig-bufs"))
        (orig-sessions (make-symbol "orig-sessions")))
    `(let ((,orig-bufs (buffer-list))
           (,orig-sessions (copy-sequence omnidiff-dual--sessions)))
       (unwind-protect
           (progn ,@body)
         (dolist (sess (copy-sequence omnidiff-dual--sessions))
           (unless (memq sess ,orig-sessions)
             (omnidiff-test--cleanup-session sess nil)))
         (let ((kill-buffer-query-functions nil))
           (dolist (buf (buffer-list))
             (when (and (buffer-live-p buf)
                        (not (memq buf ,orig-bufs)))
               (with-current-buffer buf
                 (set-buffer-modified-p nil))
               (kill-buffer buf))))))))

;;; Async polling helpers

(defun omnidiff-test--wait-until (pred &optional timeout-secs)
  "Poll until zero-arg PRED returns non-nil or TIMEOUT-SECS (default 10.0).
Always calls `(accept-process-output nil 0.02)' so all child processes and
stderr pipes drain cleanly.  Return the truthy result of PRED, or nil."
  (let ((deadline (+ (float-time) (or timeout-secs 10.0)))
        (result (funcall pred)))
    (while (and (null result) (< (float-time) deadline))
      (accept-process-output nil 0.02)
      (setq result (funcall pred)))
    result))

(defun omnidiff-test--wait-request (buf &optional timeout-sec)
  "Wait until BUF has no active `omnidiff--active-request' or fail."
  (unless (omnidiff-test--wait-until
           (lambda ()
             (or (not (buffer-live-p buf))
                 (null (buffer-local-value 'omnidiff--active-request buf))))
           timeout-sec)
    (let ((req (and (buffer-live-p buf)
                    (buffer-local-value 'omnidiff--active-request buf))))
      (ert-fail
       (format "Timed out waiting for omnidiff request %S (proc=%S workers=%S)"
               req
               (and req (omnidiff--request-proc req))
               (and req (omnidiff--request-workers req)))))))

(defun omnidiff-test--wait-dual (session &optional timeout-sec)
  "Wait until dual SESSION has no live async refresh process or fail."
  (unless (omnidiff-test--wait-until
           (lambda ()
             (or (not (memq session omnidiff-dual--sessions))
                 (let ((proc (omnidiff-dual--session-active-proc session)))
                   (not (and proc (process-live-p proc))))))
           timeout-sec)
    (ert-fail
     (format "Timed out waiting for dual async refresh on %S" session))))

;;; Buffer & overlay inspectors

(defun omnidiff-test--collect-diff-faces ()
  "Return list of `(START END FACE)' for diff/refine faces in current buffer."
  (let ((pos (point-min))
        (faces nil))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'face nil (point-max)))
            (f (get-text-property pos 'face)))
        (when (memq f '(omnidiff-added omnidiff-removed omnidiff-changed
                        omnidiff-added-refine omnidiff-removed-refine
                        omnidiff-changed-refine))
          (push (list pos next f) faces))
        (setq pos next)))
    (nreverse faces)))

(defun omnidiff-test--collect-display-props ()
  "Return list of `display' text property values in the current buffer."
  (let ((pos (point-min))
        (vals nil))
    (while (< pos (point-max))
      (let ((next (next-single-property-change pos 'display nil (point-max)))
            (d (get-text-property pos 'display)))
        (when d (push d vals))
        (setq pos next)))
    (nreverse vals)))

(defun omnidiff-test--token-overlays (buf)
  "Return all semantic `token' overlays belonging to `omnidiff-dual' in BUF."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (cl-remove-if-not
       (lambda (ov)
         (and (overlay-get ov 'omnidiff-dual-ov)
              (eq (overlay-get ov 'omnidiff-dual-kind) 'token)))
       (overlays-in (point-min) (point-max))))))

(provide 'omnidiff-test-helper)
;;; omnidiff-test-helper.el ends here
