;;; omnidiff-test.el --- ERT tests for omnidiff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; This file is part of omnidiff.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Automated unit, engine, theme, dual viewer, image, rendering, folding,
;; and process-lifecycle tests for `omnidiff-engine',
;; `omnidiff-theme', `omnidiff-dual', `omnidiff-image', `omnidiff-render',
;; and `omnidiff'.

;;; Code:

(require 'ert)
(require 'omnidiff)

(defconst omnidiff-test--pkg-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (file-truename (or load-file-name buffer-file-name)))))
  "Root directory of the `omnidiff.el' package.")

(defun omnidiff-test--file-meta (path old-file new-file &optional new-path)
  "Format a NUL-delimited file sentinel header for parser tests."
  (concat omnidiff-engine-file-sentinel
          path "\x00" old-file "\x00" new-file "\x00" (or new-path "") "\x00"))

(defun omnidiff-test--make-hunk (op sr sc er ec &optional move-target ref-line)
  "Build an `omnidiff-json-hunk' with OP over (SR,SC)..(ER,EC)."
  (make-omnidiff-json-hunk
   :operation op
   :range (make-omnidiff-json-range
           :start-row sr :start-col sc :end-row er :end-col ec)
   :move-target move-target
   :reference-line ref-line))

(cl-defun omnidiff-test--make-json-file (path before-src after-src
                                              &key (status 'modified)
                                              new-path before-hunks after-hunks
                                              old-mode new-mode old-oid new-oid
                                              summary)
  "Build a JSON-backed `omnidiff-file' test fixture."
  (let* ((target (or new-path path))
         (jd (make-omnidiff-json-diff
              :before (make-omnidiff-json-side
                       :path (if (eq status 'added) "/dev/null" path)
                       :hunks before-hunks)
              :after (make-omnidiff-json-side
                      :path (if (eq status 'deleted) "/dev/null" target)
                      :hunks after-hunks)
              :summary summary)))
    (omnidiff--file-from-json
     path
     (if (eq status 'added) "/dev/null" path)
     (if (eq status 'deleted) "/dev/null" target)
     target jd old-mode new-mode old-oid new-oid nil nil
     before-src after-src)))

(defun omnidiff-test--wait-request (buf &optional timeout-sec)
  "Wait until BUF has no active `omnidiff--active-request' or fail after TIMEOUT-SEC."
  (let ((deadline (+ (float-time) (or timeout-sec 10.0))))
    (while (and (buffer-live-p buf)
                (buffer-local-value 'omnidiff--active-request buf))
      (when (> (float-time) deadline)
        (let ((req (buffer-local-value 'omnidiff--active-request buf)))
          (ert-fail
           (format "Timed out waiting for omnidiff request %S (proc=%S workers=%S)"
                   req
                   (omnidiff--request-proc req)
                   (omnidiff--request-workers req)))))
      (accept-process-output nil 0.02))))

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

;;; Theme tests

(ert-deftest omnidiff-test/theme-hex-blending ()
  "Hex parser and blender handle valid colors and pass through invalid values."
  (should (equal (omnidiff-theme--parse-hex "#102030") '(16 32 48)))
  (should-not (omnidiff-theme--parse-hex "invalid"))
  (should (equal (omnidiff-theme--blend-hex "#000000" "#ffffff" 0.5) "#808080"))
  (should (equal (omnidiff-theme--blend-hex "unspecified" "#ffffff" 0.5) "unspecified"))
  (should (omnidiff-theme--dark-color-p "#0d0e1c"))
  (should-not (omnidiff-theme--dark-color-p "#ffffff")))

(ert-deftest omnidiff-test/standalone-diff-faces-defined ()
  "Standalone `emacs -Q' load defines inherited `diff-mode' fallback faces."
  (dolist (face '(diff-added diff-removed diff-changed
                  diff-refine-added diff-refine-removed diff-refine-changed))
    (should (facep face))))

(ert-deftest omnidiff-test/cycle-color-scheme ()
  "Cycling color scheme rotates exhaustively across all three palettes."
  (let ((saved omnidiff-color-scheme))
    (unwind-protect
        (progn
          (setq omnidiff-color-scheme 'omni-semantic)
          (omnidiff-cycle-color-scheme)
          (should (eq omnidiff-color-scheme 'calm-modus))
          (omnidiff-cycle-color-scheme)
          (should (eq omnidiff-color-scheme 'neutral-paper))
          (omnidiff-cycle-color-scheme)
          (should (eq omnidiff-color-scheme 'omni-semantic)))
      (setq omnidiff-color-scheme saved)
      (omnidiff-apply-theme))))

;;; Rendering, navigation, folding, and source jump tests

(ert-deftest omnidiff-test/stacked-hunks-headers-and-folding ()
  "Stacked layout renders canvas spacers, structured hunk headers, and clean folding."
  (let* ((buf (generate-new-buffer " *omnidiff-stacked-test*"))
         (pre (mapconcat (lambda (i) (format "same-%d" i)) (number-sequence 1 10) "\n"))
         (mid (mapconcat (lambda (i) (format "mid-%d" i)) (number-sequence 1 10) "\n"))
         (b-src (concat pre "\nold-line\n" mid "\nold-upd\n"))
         (a-src (concat pre "\nnew-line\n" mid "\nnew-upd\n"))
         (f (omnidiff-test--make-json-file
             "src/demo.el" b-src a-src
             :before-hunks (list (omnidiff-test--make-hunk 'delete 10 0 10 8)
                                 (omnidiff-test--make-hunk 'update 21 0 21 7))
             :after-hunks (list (omnidiff-test--make-hunk 'insert 10 0 10 8)
                                (omnidiff-test--make-hunk 'update 21 0 21 7))))
         (diff (make-omnidiff-diff :files (list f))))
    (unwind-protect
        (with-current-buffer buf
          (omnidiff-mode)
          (should (eq buffer-undo-list t))
          (setq omnidiff--layout 'stacked)
          (omnidiff-render-buffer diff)
          (should-not (buffer-modified-p))
          (goto-char (point-min))
          ;; Jump to Hunk 1 header and inspect structure + canvas spacer above it.
          (omnidiff-next-hunk)
          (let ((h1-pos (point))
                (h1-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (should (string-search "▼ L8-14" h1-line))
            (should (string-search "+1" h1-line))
            (should (string-search "-1" h1-line))
            (should (string-search "7 unchanged lines" h1-line))
            (should (string-search "1/2" h1-line))
            (should (eql (get-text-property h1-pos 'omnidiff-line) 8))
            (save-excursion
              (forward-line -1)
              (should (string-empty-p
                       (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
            ;; Fold from inside hunk body -> collapses body, flips triangle, lands on hunk header.
            (search-forward "old-line")
            (should (= (omnidiff-render-file-header-pos-at (point)) (point-min)))
            (omnidiff-toggle-fold)
            (should (= (point) h1-pos))
            (let* ((hov (get-text-property h1-pos 'omnidiff-hunk-ov))
                   (fov (overlay-get hov 'omnidiff-fold-ov)))
              (should (eq (overlay-get hov 'invisible) 'omnidiff-hunk-fold))
              (should (equal (substring-no-properties (overlay-get fov 'display)) "▶"))
              ;; Unfold from hunk header -> restores body and triangle.
              (omnidiff-toggle-fold)
              (should-not (overlay-get hov 'invisible))
              (should-not (overlay-get fov 'display)))
            ;; Jump to Hunk 2 header and back to Hunk 1.
            (omnidiff-next-hunk)
            (let ((h2-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
              (should (string-search "▼ L19-22" h2-line))
              (should (string-search "~1" h2-line))
              (should (string-search "4 unchanged lines" h2-line))
              (should (string-search "2/2" h2-line))
              (should (eql (get-text-property (point) 'omnidiff-line) 19)))
            (omnidiff-prev-hunk)
            (should (= (point) h1-pos)))
          ;; File-level fold & fold-all.
          (goto-char (point-min))
          (omnidiff-toggle-fold)
          (let ((bov (get-text-property (point-min) 'omnidiff-body-ov)))
            (should (eq (overlay-get bov 'invisible) 'omnidiff-fold)))
          (omnidiff-toggle-fold-all)
          (let ((bov (get-text-property (point-min) 'omnidiff-body-ov)))
            (should-not (overlay-get bov 'invisible))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest omnidiff-test/fold-all-keeps-current-file ()
  "Folding all files from inside a file body lands on that same file's header."
  (let* ((buf (generate-new-buffer " *omnidiff-fold-all-test*"))
         (make-f (lambda (name line-text)
                   (omnidiff-test--make-json-file
                    name "" (concat line-text "\n")
                    :status 'added
                    :after-hunks (list (omnidiff-test--make-hunk 'insert 0 0 0 (string-bytes line-text))))))
         (diff (make-omnidiff-diff :files (list (funcall make-f "a.el" "alpha-body")
                                                (funcall make-f "b.el" "beta-body")))))
    (unwind-protect
        (with-current-buffer buf
          (omnidiff-mode)
          (omnidiff-render-buffer diff)
          ;; Fold all from inside file a.el -> lands on file a.el header.
          (goto-char (point-min))
          (search-forward "alpha-body")
          (omnidiff-toggle-fold-all)
          (should (string-search "a.el" (buffer-substring-no-properties
                                         (line-beginning-position) (line-end-position))))
          ;; Unfold all, then fold all from inside final file b.el -> lands on b.el header.
          (omnidiff-toggle-fold-all)
          (search-forward "beta-body")
          (omnidiff-toggle-fold-all)
          (should (string-search "b.el" (buffer-substring-no-properties
                                         (line-beginning-position) (line-end-position)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest omnidiff-test/unified-and-sbs-layouts ()
  "Unified and side-by-side layouts render refine highlights, continuations, and CJK alignment."
  (should (eq omnidiff-default-layout 'unified))
  (should (eq (keymap-lookup omnidiff-mode-map "v") #'omnidiff-cycle-layout))
  (let* ((pad (mapconcat (lambda (i) (format "// pad %d" i)) (number-sequence 1 11) "\n"))
         (b-src (concat "fn long_fn() {\n" pad "\n;; 中文上下文\n\nlet v = 15;\nend_fn();\n"))
         (a-src (concat "fn long_fn() {\n" pad "\n;; 中文上下文\n\nlet v = 1500;\nlet extra = 99; // cont-part\nend_fn();\n"))
         (f (omnidiff-test--make-json-file
             "src/demo.rs" b-src a-src
             :before-hunks (list (omnidiff-test--make-hunk 'update 14 8 14 10 nil 0))
             :after-hunks (list (omnidiff-test--make-hunk 'update 14 8 14 12 nil 0)
                                (omnidiff-test--make-hunk 'insert 15 0 15 28 nil 0))))
         (diff (make-omnidiff-diff :files (list f)))
         (buf (generate-new-buffer " *omnidiff-layouts-test*")))
    (unwind-protect
        (with-current-buffer buf
          (omnidiff-mode)
          ;; Unified layout.
          (setq omnidiff--layout 'unified
                omnidiff--highlight-style 'traditional)
          (omnidiff-render-buffer diff)
          (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-search "▼ L12-17" txt))
            (should (string-search "   14   14 │" txt))
            (should (string-search "11 unchanged lines" txt))
            (should (string-search "@ 1: fn long_fn() {" txt))
            (let ((first (string-search ";; 中文上下文" txt)))
              (should first)
              (should-not (string-search ";; 中文上下文" txt (1+ first))))
            (should (string-search "let v = 15;" txt))
            (should (string-search "let v = 1500;" txt))
            (should (string-search "let extra = 99; // cont-part" txt)))
          (goto-char (point-min))
          (search-forward "1500")
          (let ((fc (get-text-property (1- (point)) 'face)))
            (should (or (eq fc 'omnidiff-changed-refine)
                        (and (listp fc) (memq 'omnidiff-changed-refine fc)))))
          ;; Header line reflects file count and layout.
          (let ((hl (substring-no-properties (omnidiff-render-header-line))))
            (should (string-search "OMNIDIFF" hl))
            (should (string-search "1 file" hl))
            (should (string-search "unified · default" hl)))
          ;; Side-by-side layout.
          (setq omnidiff--layout 'side-by-side)
          (omnidiff-render-buffer diff)
          (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-search " BEFORE " txt))
            (should (string-search " AFTER " txt))
            (should (string-search "let v = 15;" txt))
            (should (string-search "let v = 1500;" txt)))
          (goto-char (point-min))
          (search-forward " BEFORE ")
          (search-forward " │ " (line-end-position))
          (should (eq (get-text-property (1- (point)) 'face) 'omnidiff-side-bar))
          ;; CJK row and ASCII row align their center `│' divider identically.
          (goto-char (point-min))
          (search-forward ";; 中文上下文")
          (let ((cjk-bar (save-excursion
                           (search-forward " │ " (line-end-position))
                           (string-width (buffer-substring-no-properties
                                          (line-beginning-position) (match-beginning 0))))))
            (search-forward "let v = 15;")
            (let ((ascii-bar (save-excursion
                               (search-forward " │ " (line-end-position))
                               (string-width (buffer-substring-no-properties
                                              (line-beginning-position) (match-beginning 0))))))
              (should (= cjk-bar ascii-bar)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest omnidiff-test/jump-to-source ()
  "RET jumps to the target source file and line in another window."
  (let* ((dir (make-temp-file "omnidiff-src-" t))
         (file (expand-file-name "demo.txt" dir))
         (buf (generate-new-buffer " *omnidiff-jump-test*"))
         (visited nil))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "line 1\nline 2\nline 3\n"))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir)
            (omnidiff-render-buffer
             (make-omnidiff-diff
              :files
              (list
               (omnidiff-test--make-json-file
                "demo.txt" "line 1\nline 2\n" "line 1\nline 2\nline 3\n"
                :after-hunks (list (omnidiff-test--make-hunk 'insert 2 0 2 6))))))
            (goto-char (point-min))
            (search-forward "line 3")
            (omnidiff-jump-to-source)
            (setq visited (current-buffer))
            (should (equal (file-truename (buffer-file-name)) (file-truename file)))
            (should (= (line-number-at-pos) 3))))
      (when (buffer-live-p visited) (kill-buffer visited))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest omnidiff-test/empty-diff ()
  "Empty diff renders a clean placeholder message."
  (let ((buf (generate-new-buffer " *omnidiff-empty-test*")))
    (unwind-protect
        (with-current-buffer buf
          (omnidiff-mode)
          (omnidiff-render-buffer "")
          (should (string-search "No changes to display." (buffer-string))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;; Runner & shell wrapper tests

(ert-deftest omnidiff-test/wrapper-shell-quoting ()
  "Wrapper shell-quotes flags containing spaces and glob characters safely."
  (let* ((cmd (omnidiff--build-ext-diff-wrapper
               "/bin/echo" '("--arg" "has space" "*.el")))
         (out (shell-command-to-string
               (concat cmd " path.el old 0 0 new 0 0")))
         (prefix (omnidiff-test--file-meta "path.el" "old" "new")))
    (should (string-prefix-p prefix out))
    (should (equal (string-trim (substring out (length prefix)))
                   "--arg has space *.el -- path.el old 0 0 new 0 0"))))

(ert-deftest omnidiff-test/runner-error-and-concurrency ()
  "Failed commands preserve existing view; superseded runs cannot overwrite newer ones."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true"))
        (buf (generate-new-buffer " *omnidiff-runner-test*"))
        (dir-good (make-temp-file "omnidiff-good-" t))
        (dir-bad (make-temp-file "omnidiff-bad-" t))
        (dir-stale (make-temp-file "omnidiff-stale-" t))
        (dir-fresh (make-temp-file "omnidiff-fresh-" t)))
    (unwind-protect
        (progn
          ;; 1. Seed buffer with a valid render, then run a failing command.
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir-good)
            (omnidiff-render-buffer "commit keep-me\n"))
          (let ((fail-proc (omnidiff-run buf '("sh" "-c" "echo bad rev >&2; exit 128")
                                         "bad" dir-bad)))
            (while (process-live-p fail-proc)
              (accept-process-output fail-proc 0.05))
            (with-current-buffer buf
              (should (string-search "commit keep-me" (buffer-string)))
              (should (equal omnidiff--repo-dir dir-good))))
          ;; 2. Launch a slow run, immediately supersede with a fast run.
          (let ((slow (omnidiff-run buf '("sh" "-c" "sleep 0.2; printf 'commit stale\\n'")
                                    "stale" dir-stale))
                (fast (omnidiff-run buf '("sh" "-c" "printf 'commit fresh\\n'")
                                    "fresh" dir-fresh)))
            (while (or (process-live-p slow) (process-live-p fast))
              (accept-process-output nil 0.05))
            (with-current-buffer buf
              (should (string-search "commit fresh" (buffer-string)))
              (should-not (string-search "commit stale" (buffer-string)))
              (should (equal omnidiff--repo-dir dir-fresh)))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (dolist (d (list dir-good dir-bad dir-stale dir-fresh))
        (when (file-directory-p d) (delete-directory d t))))))

(ert-deftest omnidiff-test/nul-framing-special-filenames ()
  "NUL metadata framing preserves tabs and newlines in Git file paths."
  (let* ((weird-old "dir\tname/old\nfile.rs")
         (weird-new "dir\tname/new\nfile.rs")
         (json-body "{\"before\":{\"path\":\"old\",\"hunks\":[{\"operation\":\"delete\",\"range\":{\"start_row\":0,\"start_column\":0,\"end_row\":0,\"end_column\":5}}]},\"after\":{\"path\":\"new\",\"hunks\":[{\"operation\":\"insert\",\"range\":{\"start_row\":0,\"start_column\":0,\"end_row\":0,\"end_column\":5}}]},\"large_residual\":false}")
         (raw (concat
               (omnidiff-test--file-meta weird-old "tmp-old" "tmp-new" weird-new)
               json-body omnidiff-engine-end-sentinel))
         (diff (omnidiff--parse-serial-json-output raw nil))
         (file (car (omnidiff-diff-files diff))))
    (should (= (length (omnidiff-diff-files diff)) 1))
    (should (equal (omnidiff-file-path file) weird-old))
    (should (equal (omnidiff-file-target-path file) weird-new))
    (should (eq (omnidiff-file-status file) 'renamed))
    (should (omnidiff-json-diff-p (omnidiff-file-json-diff file))))
  ;; Verify the actual shell wrapper emits exact NUL fields for tab/newline paths.
  (let* ((cmd (omnidiff--build-ext-diff-wrapper "/bin/echo" '("--flag") t))
         (out (shell-command-to-string
               (format "%s %s old 0 0 new 0 0 %s 100"
                       cmd
                       (shell-quote-argument "a\tb\nc.el")
                       (shell-quote-argument "x\ty\nz.el"))))
         (diff (omnidiff--parse-serial-json-output out nil))
         (file (car (omnidiff-diff-files diff))))
    (should (equal (omnidiff-file-path file) "a\tb\nc.el"))
    (should (equal (omnidiff-file-target-path file) "x\ty\nz.el"))
    (should (eq (omnidiff-file-status file) 'renamed)))
  ;; Pathname ending in sentinel body must not split its own metadata record.
  (let* ((tricky-path "foo\x1cOMNIDIFF-FILE")
         (json-body "{\"before\":{\"path\":\"old\",\"hunks\":[]},\"after\":{\"path\":\"new\",\"hunks\":[]},\"large_residual\":false,\"summary\":\"no_changes\"}")
         (raw (concat
               (omnidiff-test--file-meta tricky-path "old" "new")
               json-body omnidiff-engine-end-sentinel))
         (diff (omnidiff--parse-serial-json-output raw nil)))
    (should (= (length (omnidiff-diff-files diff)) 1))
    (should (equal (omnidiff-file-path (car (omnidiff-diff-files diff))) tricky-path)))
  ;; Newline in filename escapes cleanly to a single header line while keeping raw path for RET.
  (let ((buf (generate-new-buffer " *omnidiff-nl-title-test*"))
        (diff (make-omnidiff-diff
               :files (list (omnidiff-test--make-json-file
                             "dir/a\nb.el" "" "x\n" :status 'added
                             :after-hunks (list (omnidiff-test--make-hunk 'insert 0 0 0 1)))))))
    (unwind-protect
        (with-current-buffer buf
          (omnidiff-mode)
          (omnidiff-render-buffer diff)
          (goto-char (point-min))
          (let ((hdr-line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
            (should (string-search "dir/a\\nb.el" hdr-line))
            (should (equal (get-text-property (point-min) 'omnidiff-file) "dir/a\nb.el"))
            (should (= (omnidiff-render-file-header-pos-at (+ (point-min) 8)) (point-min)))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest omnidiff-test/autoload-no-recursive-load ()
  "Package autoloads load all public entrypoints and theme hooks without recursion."
  (let* ((tmp (make-temp-file "omnidiff-autoload-" t))
         (autoloads (expand-file-name "omnidiff-autoloads.el" tmp))
         (emacs-bin (expand-file-name invocation-name invocation-directory))
         (expected-cmds '(omnidiff-files
                          omnidiff-buffers
                          omnidiff-buffer-vs-head
                          omnidiff-magit-dual
                          omnidiff-diff
                          omnidiff-show
                          omnidiff-mode
                          omnidiff-magit-setup
                          omnidiff-apply-theme)))
    (unwind-protect
        (progn
          (require 'loaddefs-gen)
          (loaddefs-generate omnidiff-test--pkg-dir autoloads)
          ;; 1. Loading autoloads registers all commands, and `(require 'omnidiff)' does not recurse.
          (with-temp-buffer
            (let ((rc (call-process
                       emacs-bin nil '(t t) nil
                       "-Q" "--batch" "-L" omnidiff-test--pkg-dir
                       "--eval" (prin1-to-string
                                 `(progn
                                    (load ,autoloads nil t)
                                    (dolist (sym ',(append expected-cmds '(omnidiff-dual-open)))
                                      (unless (autoloadp (symbol-function sym))
                                        (error "Expected autoload for %s" sym)))
                                    (require 'omnidiff)
                                    (customize-set-variable 'omnidiff-color-scheme 'neutral-paper)
                                    (unless (eq omnidiff-color-scheme 'neutral-paper)
                                      (error "Expected updated color scheme")))))))
              (should (zerop rc))))
          ;; 2. Invoking `omnidiff-apply-theme' directly through its autoload must work.
          (with-temp-buffer
            (let ((rc (call-process
                       emacs-bin nil '(t t) nil
                       "-Q" "--batch" "-L" omnidiff-test--pkg-dir
                       "--eval" (prin1-to-string
                                 `(progn
                                    (load ,autoloads nil t)
                                    (omnidiff-apply-theme)
                                    (unless (featurep 'omnidiff-theme)
                                      (error "Expected omnidiff-theme to be loaded")))))))
              (should (zerop rc))))
          ;; 3. Each new public command autoload resolves cleanly with zero recursive load errors.
          (dolist (entry '(omnidiff-files
                           omnidiff-buffers
                           omnidiff-buffer-vs-head
                           omnidiff-magit-dual
                           omnidiff-dual-open))
            (with-temp-buffer
              (let ((rc (call-process
                         emacs-bin nil '(t t) nil
                         "-Q" "--batch" "-L" omnidiff-test--pkg-dir
                         "--eval" (prin1-to-string
                                   `(progn
                                      (load ,autoloads nil t)
                                      (unless (autoloadp (symbol-function ',entry))
                                        (error "Expected autoload for %s" ',entry))
                                      (autoload-do-load (symbol-function ',entry) ',entry)
                                      (when (autoloadp (symbol-function ',entry))
                                        (error "Autoload did not resolve for %s" ',entry))
                                      (unless (fboundp ',entry)
                                        (error "Symbol %s not fbound after autoload" ',entry)))))))
                (should (zerop rc))))))
      (delete-directory tmp t))))

(ert-deftest omnidiff-test/sbs-width-budget-and-narrow-fallback ()
  "Fixed `--width 320' output wraps losslessly in SBS and falls back to stacked when narrow."
  (let ((buf (generate-new-buffer " *omnidiff-sbs-width-test*"))
        (orig-width (frame-width (selected-frame))))
    (unwind-protect
        (with-current-buffer buf
          (omnidiff-mode)
          (set-frame-width (selected-frame) 80)
          (should (= (omnidiff-render--sbs-metrics-code-w (omnidiff-render--sbs-metrics 4)) 28))
          (set-frame-width (selected-frame) 68)
          (should (= (omnidiff-render--sbs-metrics-code-w (omnidiff-render--sbs-metrics 4)) 22))
          (set-frame-width (selected-frame) 60)
          (should (= (omnidiff-render--sbs-metrics-code-w (omnidiff-render--sbs-metrics 4)) 18))
          ;; End-to-end check against real `omnidiff' CLI with fixed `--width 320'.
          (when-let* ((bin (executable-find omnidiff-executable)))
            (let* ((dir (make-temp-file "omnidiff-sbs-cli-" t))
                   (fa (expand-file-name "a.rs" dir))
                   (fb (expand-file-name "b.rs" dir))
                   (long-a "fn demo() { let x = \"0123456789abcdefghijklmnopqrstuvwxyz\"; }")
                   (long-b "fn demo() { let y = \"0123456789abcdefghijklmnopqrstuvwxyz\"; }"))
              (unwind-protect
                  (progn
                    (with-temp-file fa (insert long-a "\n"))
                    (with-temp-file fb (insert long-b "\n"))
                    (let* ((jd (omnidiff-engine-run-pair fa fb 'default bin))
                           (diff (make-omnidiff-diff
                                  :files (list (omnidiff--file-from-json "a.rs" fa fb "a.rs" jd)))))
                      ;; At 80 and 68 cols: dynamic Elisp SBS wrapping loses zero characters.
                      (dolist (frame-w '(80 68))
                        (set-frame-width (selected-frame) frame-w)
                        (setq omnidiff--layout 'side-by-side)
                        (omnidiff-render-buffer diff)
                        (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                          (should (string-search " BEFORE " txt))
                          (should (string-search " AFTER " txt))
                          (should-not (string-search "…" txt))
                          (let (left-parts right-parts)
                            (dolist (row (split-string txt "\n"))
                              (when (string-match "\\`[0-9 ]+┃ [~ ] \\(.*?\\) │ [0-9 ]+┃ [~ ] \\(.*\\)\\'" row)
                                (push (string-trim-right (match-string 1 row) " +") left-parts)
                                (push (string-trim-right (match-string 2 row) " +") right-parts)))
                            (should (>= (length left-parts) 2))
                            (should (equal (apply #'concat (nreverse left-parts)) long-a))
                            (should (equal (apply #'concat (nreverse right-parts)) long-b)))))
                      ;; At 60 cols (code-w 18 < 20): auto-falls back to stacked without re-running CLI.
                      (set-frame-width (selected-frame) 60)
                      (setq omnidiff--layout 'side-by-side)
                      (omnidiff-render-buffer diff)
                      (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                        (should (string-search " BEFORE \n" txt))
                        (should (string-search " AFTER  \n" txt))
                        (should-not (string-search "…" txt))
                        (should (string-search long-a txt))
                        (should (string-search long-b txt)))))
                (delete-directory dir t)))))
      (set-frame-width (selected-frame) orig-width)
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest omnidiff-test/file-header-pos-inside-folded-body ()
  "Enclosing file header lookup resolves folded body positions to their own file."
  (let* ((buf (generate-new-buffer " *omnidiff-folded-pos-test*"))
         (make-f (lambda (name line-text)
                   (omnidiff-test--make-json-file
                    name "" (concat line-text "\n") :status 'added
                    :after-hunks (list (omnidiff-test--make-hunk 'insert 0 0 0 (string-bytes line-text))))))
         (diff (make-omnidiff-diff :files (list (funcall make-f "first.el" "first-body")
                                                (funcall make-f "last.el" "last-body")))))
    (unwind-protect
        (with-current-buffer buf
          (omnidiff-mode)
          (omnidiff-render-buffer diff)
          (goto-char (point-min))
          (let ((hdr1 (point))
                (body1 (progn (search-forward "first-body") (match-beginning 0)))
                (hdr2 (progn (omnidiff-next-file) (point)))
                (body2 (progn (search-forward "last-body") (match-beginning 0))))
            ;; Fold all files, then query positions inside each folded body.
            (goto-char hdr1)
            (omnidiff-toggle-fold-all)
            (should (invisible-p body1))
            (should (invisible-p body2))
            (should (= (omnidiff-render-file-header-pos-at body1) hdr1))
            (should (= (omnidiff-render-file-header-pos-at body2) hdr2))))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest omnidiff-test/moved-indicator-uses-named-face ()
  "Moved rails and move-refs use `omnidiff-moved-indicator' instead of literal hex."
  (let* ((buf (generate-new-buffer " *omnidiff-moved-face-test*"))
         (b-src "(foo)\nline2\nline3\nline4\nline5\n")
         (a-src "line2\nline3\nline4\nline5\n(bar)\n")
         (mt-b (make-omnidiff-json-range :start-row 4 :start-col 0 :end-row 4 :end-col 5))
         (mt-a (make-omnidiff-json-range :start-row 0 :start-col 0 :end-row 0 :end-col 5))
         (diff (make-omnidiff-diff
                :files
                (list
                 (omnidiff-test--make-json-file
                  "moved.el" b-src a-src
                  :before-hunks (list (omnidiff-test--make-hunk 'move 0 0 0 5 mt-b)
                                      (omnidiff-test--make-hunk 'update 0 1 0 4))
                  :after-hunks (list (omnidiff-test--make-hunk 'move 4 0 4 5 mt-a)
                                     (omnidiff-test--make-hunk 'update 4 1 4 4)))))))
    (unwind-protect
        (with-current-buffer buf
          (omnidiff-mode)
          (setq omnidiff--layout 'unified
                omnidiff--highlight-style 'traditional)
          (omnidiff-render-buffer diff)
          (goto-char (point-min))
          (search-forward "┃")
          (let ((rail-face (get-text-property (match-beginning 0) 'face)))
            (should (equal rail-face '(omnidiff-moved-indicator omnidiff-lineno-changed))))
          (search-forward "→ L5")
          (let ((ref-face (get-text-property (match-beginning 0) 'face)))
            (should (equal ref-face '(omnidiff-moved-indicator omnidiff-changed))))
          (should (eq (face-attribute 'omnidiff-moved-indicator :inherit nil) 'unspecified))
          (should (eq (face-attribute 'omnidiff-moved-indicator :background nil) 'unspecified))
          (should (eq (face-attribute 'omnidiff-moved-indicator :slant nil) 'unspecified)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest omnidiff-test/header-hints-cache-key ()
  "Header hints cache key tracks theme revision, frame, and header font."
  (let ((omnidiff-render--hints-cache nil))
    (let ((first (omnidiff-render--header-hints)))
      (should (equal (car omnidiff-render--hints-cache)
                     (list omnidiff-theme--revision
                           (selected-frame)
                           (display-graphic-p)
                           (frame-char-width)
                           (face-font 'omnidiff-header-line))))
      (should (eq (omnidiff-render--header-hints) first))
      (omnidiff-apply-theme)
      (should-not (eq (omnidiff-render--header-hints) first)))))

(ert-deftest omnidiff-test/mode-reentry-and-lifecycle-cleanup ()
  "Mode re-entry rejects stale runs; launch failure and kill-buffer clean up pipes."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true"))
        (buf (generate-new-buffer " *omnidiff-lifecycle-test*"))
        (dir-a (make-temp-file "omnidiff-life-a-" t))
        (dir-b (make-temp-file "omnidiff-life-b-" t)))
    (unwind-protect
        (progn
          ;; 1. Mode re-entry cancels active request; even if hook were bypassed,
          ;; process identity prevents a stale req-id 1 from overwriting a new req-id 1.
          (let ((slow (omnidiff-run buf '("sh" "-c" "sleep 0.25; printf 'commit STALE\\n'")
                                    "stale" dir-a)))
            (with-current-buffer buf
              (let ((change-major-mode-hook nil))
                (omnidiff-mode))
              (should (= omnidiff--request-id 0)))
            (let ((fast (omnidiff-run buf '("sh" "-c" "printf 'commit FRESH\\n'")
                                      "fresh" dir-b)))
              (while (or (process-live-p slow) (process-live-p fast))
                (accept-process-output nil 0.05))
              (with-current-buffer buf
                (should (string-search "commit FRESH" (buffer-string)))
                (should-not (string-search "commit STALE" (buffer-string)))
                (should (equal omnidiff--repo-dir dir-b)))))
          ;; 2. Launch failure cleans up the stderr pipe with no leak in `process-list'.
          (let ((stderr-name (concat (buffer-name buf) " stderr")))
            (should-error
             (omnidiff-run buf '("nonexistent-omnidiff-cmd-xyz") "bad" dir-a))
            (should-not (cl-find-if (lambda (p) (equal (process-name p) stderr-name))
                                    (process-list))))
          ;; 3. Delayed multi-chunk stderr is captured completely after failure.
          (let* ((msg-mark (with-current-buffer (messages-buffer) (point-max)))
                 (proc (omnidiff-run
                        buf
                        '("sh" "-c" "printf 'part-one\\n' >&2; sleep 0.05; printf 'part-two\\n' >&2; exit 7")
                        "err" dir-a))
                 (sp (with-current-buffer buf
                       (omnidiff--request-stderr-proc omnidiff--active-request))))
            (while (or (process-live-p proc) (process-live-p sp))
              (accept-process-output nil 0.02))
            (accept-process-output nil 0.02)
            (let ((logged (with-current-buffer (messages-buffer)
                            (buffer-substring-no-properties msg-mark (point-max)))))
              (should (string-search "Omnidiff failed (exited abnormally with code 7): part-one\npart-two"
                                     logged))))
          ;; 4. Killing the buffer cancels both the main process and its stderr pipe.
          (let* ((kill-buf (generate-new-buffer " *omnidiff-kill-test*"))
                 (proc (omnidiff-run kill-buf '("sh" "-c" "sleep 10") "slow" dir-a))
                 (sp (with-current-buffer kill-buf
                       (omnidiff--request-stderr-proc omnidiff--active-request))))
            (should (process-live-p proc))
            (should (process-live-p sp))
            (kill-buffer kill-buf)
            (should-not (process-live-p proc))
            (should-not (process-live-p sp)))
          ;; 5. Failed process whose child retains stderr keeps ownership until EOF
          ;; so killing the buffer closes the lingering pipe.
          (let* ((linger-buf (generate-new-buffer " *omnidiff-linger-stderr*"))
                 (proc (omnidiff-run linger-buf
                                     '("sh" "-c" "(trap '' HUP; sleep 0.5) >/dev/null & exit 7")
                                     "linger" dir-a))
                 (sp (with-current-buffer linger-buf
                       (omnidiff--request-stderr-proc omnidiff--active-request))))
            (while (process-live-p proc)
              (accept-process-output nil 0.02))
            (should (process-live-p sp))
            (with-current-buffer linger-buf
              (should omnidiff--active-request)
              (should-not (omnidiff--pending-request)))
            (kill-buffer linger-buf)
            (should-not (process-live-p sp))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (dolist (d (list dir-a dir-b))
        (when (file-directory-p d) (delete-directory d t))))))

(ert-deftest omnidiff-test/rapid-view-cycling ()
  "In-flight `v' mutates layout without restarting; cached `v' re-renders in memory."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true"))
        (buf (generate-new-buffer " *omnidiff-rapid-cycle-test*"))
        (dir (make-temp-file "omnidiff-rapid-" t)))
    (unwind-protect
        (with-current-buffer buf
          (omnidiff-mode)
          (setq omnidiff--command '("sh" "-c" "sleep 0.12; printf 'commit ok\\n'")
                omnidiff--label "rapid"
                omnidiff--repo-dir dir
                omnidiff--layout 'unified
                omnidiff--preset 'default)
          ;; First `v' with no cached diff launches a request for side-by-side.
          (omnidiff-cycle-layout)
          (should (eq omnidiff--layout 'unified))
          (let ((first-req omnidiff--active-request)
                (first-proc (omnidiff--request-proc omnidiff--active-request)))
            (should (eq (omnidiff--request-layout first-req) 'side-by-side))
            ;; Second `v' while in flight updates target layout IN PLACE without restarting.
            (omnidiff-cycle-layout)
            (should (eq omnidiff--active-request first-req))
            (should (eq (omnidiff--request-proc omnidiff--active-request) first-proc))
            (should (eq (omnidiff--request-layout omnidiff--active-request) 'stacked))
            ;; `m' still spawns a new request inheriting the updated stacked layout.
            (omnidiff-cycle-preset)
            (should-not (eq omnidiff--active-request first-req))
            (should (eq (omnidiff--request-layout omnidiff--active-request) 'stacked))
            (should (eq (omnidiff--request-preset omnidiff--active-request) 'minimal)))
          ;; Wait for the final request to commit and cache `omnidiff--diff'.
          (let ((final-proc (omnidiff--request-proc omnidiff--active-request)))
            (while (process-live-p final-proc)
              (accept-process-output nil 0.03)))
          (should-not omnidiff--active-request)
          (should (omnidiff-diff-p omnidiff--diff))
          (should (eq omnidiff--layout 'stacked))
          (should (eq omnidiff--preset 'minimal))
          ;; With cached `omnidiff--diff', pressing `v' switches instantly with 0 subprocesses.
          (let ((cached omnidiff--diff))
            (omnidiff-cycle-layout)
            (should-not omnidiff--active-request)
            (should (eq omnidiff--layout 'unified))
            (should (eq omnidiff--diff cached)))
          ;; Failed `m' (or `g') leaves committed state and cached diff intact.
          (setq omnidiff--command '("sh" "-c" "exit 1"))
          (omnidiff-cycle-preset)
          (let ((fail-proc (omnidiff--request-proc omnidiff--active-request))
                (fail-sp (omnidiff--request-stderr-proc omnidiff--active-request)))
            (while (or (process-live-p fail-proc) (process-live-p fail-sp))
              (accept-process-output nil 0.03))
            (accept-process-output nil 0.02))
          (should-not omnidiff--active-request)
          (should (eq omnidiff--layout 'unified))
          (should (eq omnidiff--preset 'minimal))
          (should (omnidiff-diff-p omnidiff--diff))
          ;; Cycling `v' during an in-flight request for repo B mutates layout in place on repo B.
          (let ((dir-b (make-temp-file "omnidiff-rapid-b-" t)))
            (unwind-protect
                (progn
                  (omnidiff-run buf '("sh" "-c" "sleep 0.12; printf 'commit repo-b\\n'")
                                "repo-b" dir-b 'default 'unified)
                  (let ((b-proc (omnidiff--request-proc omnidiff--active-request)))
                    (omnidiff-cycle-layout)
                    (should (eq (omnidiff--request-proc omnidiff--active-request) b-proc))
                    (should (equal (omnidiff--request-repo-dir omnidiff--active-request) dir-b))
                    (should (equal (omnidiff--request-label omnidiff--active-request) "repo-b"))
                    (while (process-live-p b-proc)
                      (accept-process-output nil 0.03)))
                  (should (equal omnidiff--repo-dir dir-b))
                  (should (equal omnidiff--label "repo-b"))
                  (should (eq omnidiff--layout 'side-by-side)))
              (when (file-directory-p dir-b) (delete-directory dir-b t)))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest omnidiff-test/runner-pipe-and-progress-message ()
  "`omnidiff-run' uses a pipe connection and reports a start progress message."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true"))
        (buf (generate-new-buffer " *omnidiff-pipe-test*"))
        (dir (make-temp-file "omnidiff-pipe-" t)))
    (unwind-protect
        (let* ((msg-mark (with-current-buffer (messages-buffer) (point-max)))
               (proc (omnidiff-run buf '("sh" "-c" "printf 'commit ok\\n'")
                                   "origin/main" dir)))
          (should-not (process-tty-name proc))
          (while (process-live-p proc)
            (accept-process-output proc 0.05))
          (let ((logged (with-current-buffer (messages-buffer)
                          (buffer-substring-no-properties msg-mark (point-max)))))
            (should (string-search "Running omnidiff (origin/main)..." logged))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (when (file-directory-p dir) (delete-directory dir t)))))

(ert-deftest omnidiff-test/parse-raw-git-entries ()
  "Raw Git NUL-delimited parser handles diff entries, renames, and show preambles."
  (let* ((sha-a (make-string 40 ?a))
         (sha-b (make-string 40 ?b))
         (raw-diff (concat ":100644 100644 " sha-a " " sha-b " M\0dir/中文 file.rs\0"
                           ":100644 100644 " sha-a " " sha-b " R095\0old name.el\0new name.el\0"))
         (parsed-diff (omnidiff--parse-raw-git-entries (encode-coding-string raw-diff 'utf-8) 'diff))
         (entries (cdr parsed-diff)))
    (should-not (car parsed-diff))
    (should (= (length entries) 2))
    (should (equal (omnidiff--git-entry-status (nth 0 entries)) "M"))
    (should (equal (omnidiff--git-entry-path (nth 0 entries)) "dir/中文 file.rs"))
    (should (equal (omnidiff--git-entry-status (nth 1 entries)) "R095"))
    (should (equal (omnidiff--git-entry-path (nth 1 entries)) "old name.el"))
    (should (equal (omnidiff--git-entry-new-path (nth 1 entries)) "new name.el")))
  (let* ((sha-a (make-string 40 ?1))
         (sha-b (make-string 40 ?2))
         (raw-show (concat "commit " sha-a "\nAuthor: Alice\n\n    Subject\n\n"
                           ":100644 100644 " sha-a " " sha-b " M\0Makefile\0"))
         (parsed-show (omnidiff--parse-raw-git-entries raw-show 'show)))
    (should (string-search "Author: Alice" (car parsed-show)))
    (should (= (length (cdr parsed-show)) 1))
    (should (equal (omnidiff--git-entry-path (car (cdr parsed-show))) "Makefile")))
  ;; Merge/empty commit with no raw entries still preserves commit preamble.
  (let* ((raw-empty "commit deadbeef\nMerge: 111 222\n\n    Merge branch\n\0")
         (parsed-empty (omnidiff--parse-raw-git-entries raw-empty 'show)))
    (should (string-search "Merge branch" (car parsed-empty)))
    (should-not (cdr parsed-empty))))

(ert-deftest omnidiff-test/parallel-git-diff-and-show-parity ()
  "Parallel Git diff/show pipeline matches serial `GIT_EXTERNAL_DIFF' identically."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (progn
    (let* ((repo (make-temp-file "omnidiff-par-repo-" t))
           (default-directory repo))
      (cl-flet ((git (&rest args)
                  (unless (zerop (apply #'call-process "git" nil nil nil args))
                    (error "git %S failed" args)))
                (run-cmd (cmd label max-par)
                  (let ((omnidiff-max-parallel max-par)
                        (buf (generate-new-buffer (format " *omnidiff-par-%s-%d*" label max-par))))
                    (unwind-protect
                        (progn
                          (omnidiff-run buf cmd label repo 'default 'unified)
                          (omnidiff-test--wait-request buf)
                          (with-current-buffer buf
                            (should-not omnidiff--active-request)
                            (list (buffer-substring-no-properties (point-min) (point-max))
                                  (omnidiff-test--collect-diff-faces)
                                  omnidiff--file-count)))
                      (when (buffer-live-p buf) (kill-buffer buf))))))
        (unwind-protect
            (progn
              (git "init" "-q")
              (git "config" "user.name" "Test")
              (git "config" "user.email" "test@example.com")
              (with-temp-file (expand-file-name "Makefile" repo)
                (insert "all:\n\techo old\n"))
              (with-temp-file (expand-file-name "keep.rs" repo)
                (insert "fn demo() {\n    let x = 1;\n}\n"))
              (with-temp-file (expand-file-name "gone.rs" repo)
                (insert "fn gone() {}\n"))
              (with-temp-file (expand-file-name "old_name.rs" repo)
                (insert "fn renamed() {\n    let a = 1;\n    let b = 2;\n}\n"))
              (git "add" ".")
              (git "commit" "-q" "-m" "initial")
              ;; Second commit: modify Makefile, modify keep.rs, delete gone.rs, rename old_name.rs.
              (with-temp-file (expand-file-name "Makefile" repo)
                (insert "all:\n\techo new\n"))
              (with-temp-file (expand-file-name "keep.rs" repo)
                (insert "fn demo() {\n    let x = 2;\n}\n"))
              (delete-file (expand-file-name "gone.rs" repo))
              (rename-file (expand-file-name "old_name.rs" repo)
                           (expand-file-name "new_name.rs" repo))
              (with-temp-file (expand-file-name "new_name.rs" repo)
                (insert "fn renamed() {\n    let a = 1;\n    let b = 3;\n}\n"))
              (git "add" "-A")
              (git "commit" "-q" "-m" "second commit")
              ;; Working-tree dirty edits + intent-to-add new file (unpersisted SHA).
              (with-temp-file (expand-file-name "keep.rs" repo)
                (insert "fn demo() {\n    let x = 3;\n}\n"))
              (with-temp-file (expand-file-name "added_wt.rs" repo)
                (insert "fn brand_new() { let v = 42; }\n"))
              (git "add" "-N" "added_wt.rs")
              ;; 1. Strict JSON parity (parallel vs serial `GIT_EXTERNAL_DIFF').
              (let ((s1 (run-cmd '("git" "--no-pager" "show" "--ext-diff" "HEAD") "show HEAD" 1))
                    (s4 (run-cmd '("git" "--no-pager" "show" "--ext-diff" "HEAD") "show HEAD" 4)))
                (should (equal s4 s1))
                (should (= (nth 2 s4) 4))
                (should (string-search "second commit" (car s4))))
              (let ((d1 (run-cmd '("git" "--no-pager" "diff" "--ext-diff" "HEAD~1") "HEAD~1" 1))
                    (d4 (run-cmd '("git" "--no-pager" "diff" "--ext-diff" "HEAD~1") "HEAD~1" 4)))
                (should (equal d4 d1))
                (should (= (nth 2 d4) 5))
                (should (string-search "brand_new" (car d4))))
              (let ((u1 (run-cmd '("git" "--no-pager" "diff" "--ext-diff") "unstaged" 1))
                    (u4 (run-cmd '("git" "--no-pager" "diff" "--ext-diff") "unstaged" 4)))
                (should (equal u4 u1))
                (should (= (nth 2 u4) 2)))
              ;; 2. JSON parallel mode produces semantic structs, summaries, and accurate file counts.
              (let ((omnidiff-max-parallel 4))
                (dolist (spec '(("show HEAD" ("git" "--no-pager" "show" "--ext-diff" "HEAD") 4 "second commit")
                                ("HEAD~1" ("git" "--no-pager" "diff" "--ext-diff" "HEAD~1") 5 "brand_new")
                                ("unstaged" ("git" "--no-pager" "diff" "--ext-diff") 2 "brand_new")))
                  (let ((buf (generate-new-buffer (format " *omnidiff-par-json-%s*" (nth 0 spec)))))
                    (unwind-protect
                        (progn
                          (omnidiff-run buf (nth 1 spec) (nth 0 spec) repo 'default 'unified)
                          (omnidiff-test--wait-request buf)
                          (with-current-buffer buf
                            (should-not omnidiff--active-request)
                            (should (= omnidiff--file-count (nth 2 spec)))
                            (should (string-search (nth 3 spec)
                                                   (buffer-substring-no-properties (point-min) (point-max))))
                            (should (cl-every #'omnidiff-file-json-diff
                                              (omnidiff-diff-files omnidiff--diff)))))
                      (when (buffer-live-p buf) (kill-buffer buf)))))))
          (delete-directory repo t))))))

(ert-deftest omnidiff-test/fast-whole-file-ad-and-fallbacks ()
  "P1 A/D fast path matches serial CLI across text, blank-run, binary, and preset edge cases."
  ;; 1. Pure unit checks on `omnidiff--fast-parse-whole-file' & admission filters.
  (let ((empty (omnidiff--fast-parse-whole-file "empty.txt" 'added "")))
    (should (equal (omnidiff-file-note empty) "No changes - files are identical"))
    (should (eq (omnidiff-file-summary empty) 'no-changes))
    (should (= (omnidiff-file-added empty) 0)))
  (let ((f (omnidiff--fast-parse-whole-file
            "tabs.rs" 'added (encode-coding-string "a\tb\r\n中\tx\r\n\r\n   \r\nend" 'utf-8))))
    (should (equal (omnidiff-file-note f) "New file - everything inserted"))
    (should (eq (omnidiff-file-summary f) 'new-file))
    (should (= (omnidiff-file-added f) 3))
    (should (= (omnidiff-file-max-line f) 5))
    (should (omnidiff-json-diff-p (omnidiff-file-json-diff f))))
  (let ((del (omnidiff--fast-parse-whole-file "del.rs" 'deleted "fn x() {}   \n\n")))
    (should (equal (omnidiff-file-note del) "Deleted file - everything removed"))
    (should (eq (omnidiff-file-summary del) 'deleted-file))
    (should (= (omnidiff-file-removed del) 1)))
  (should-not (omnidiff--fast-parse-whole-file "onlyblank.txt" 'added "\n\n"))
  ;; Binary / PDF / BOM / invalid UTF-8 rejected by admission filter.
  (should-not (omnidiff--fast-parse-whole-file "a.png" 'added (unibyte-string #x89 ?P ?N ?G 0)))
  (should-not (omnidiff--fast-parse-whole-file "a.pdf" 'added (string-to-unibyte "%PDF-1.7\n")))
  (should-not (omnidiff--fast-parse-whole-file "u16.txt" 'added (unibyte-string #xff #xfe ?a 0)))
  (should-not (omnidiff--fast-parse-whole-file "lat1.txt" 'added (unibyte-string ?c ?a ?f #xe9 ?\n)))
  ;; 2. Real Git parity for A/D text, empty, >6 blank lines, binary, and --minimal preset.
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (progn
    (let* ((repo (make-temp-file "omnidiff-p1-repo-" t))
           (default-directory repo))
      (cl-flet ((git (&rest args)
                  (unless (zerop (apply #'call-process "git" nil nil nil args))
                    (error "git %S failed" args)))
                (run-show (preset max-par)
                  (let ((omnidiff-max-parallel max-par)
                        (buf (generate-new-buffer (format " *omnidiff-p1-%s-%d*" preset max-par))))
                    (unwind-protect
                        (progn
                          (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                        "show HEAD" repo preset 'unified)
                          (omnidiff-test--wait-request buf)
                          (with-current-buffer buf
                            (list (buffer-substring-no-properties (point-min) (point-max))
                                  (omnidiff-test--collect-diff-faces)
                                  omnidiff--file-count)))
                      (when (buffer-live-p buf) (kill-buffer buf))))))
        (unwind-protect
            (progn
              (git "init" "-q" "-b" "main")
              (git "config" "user.name" "Test")
              (git "config" "user.email" "test@example.com")
              (with-temp-file (expand-file-name "del_plain.rs" repo)
                (insert "fn gone() {\r\n\tlet x = 1;   \r\n\r\n\tlet y = 2;\r\n}\r\n"))
              (git "add" ".")
              (git "commit" "-q" "-m" "seed")
              ;; Second commit: only A/D files covering fast path + fallbacks.
              (delete-file (expand-file-name "del_plain.rs" repo))
              (with-temp-file (expand-file-name "add_plain.rs" repo)
                (insert "fn added() {\r\n\tlet a = \"中文\t对齐\";\r\n\r\n    \r\n\treturn a\r\n}"))
              (with-temp-file (expand-file-name "add_empty.txt" repo)
                (insert ""))
              (with-temp-file (expand-file-name "add_blanks7.txt" repo)
                (insert "top\n\n\n\n\n\n\n\nbottom\n"))
              (with-temp-file (expand-file-name "add_trailsp.rs" repo)
                (insert "  foo   \n"))
              (with-temp-file (expand-file-name "add_nbsp.txt" repo)
                (insert "a\n\u00a0\nb\n"))
              (let ((coding-system-for-write 'no-conversion))
                (with-temp-file (expand-file-name "add_bin.dat" repo)
                  (set-buffer-multibyte nil)
                  (insert (unibyte-string #x89 ?P ?N ?G #x0d #x0a #x1a #x0a 0 1 2 3))))
              (git "add" "-A")
              (git "commit" "-q" "-m" "ad mix")
              ;; JSON parallel mode handles the same mixed A/D + binary commit cleanly.
              (let ((omnidiff-max-parallel 4)
                    (buf (generate-new-buffer " *omnidiff-p1-json*")))
                (unwind-protect
                    (progn
                      (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                    "show HEAD" repo 'default 'unified)
                      (omnidiff-test--wait-request buf)
                      (with-current-buffer buf
                        (should (= omnidiff--file-count 7))
                        (let ((files (omnidiff-diff-files omnidiff--diff)))
                          (should (cl-every #'omnidiff-file-json-diff files))
                          (let ((bin (cl-find "add_bin.dat" files :key #'omnidiff-file-path :test #'equal))
                                (emp (cl-find "add_empty.txt" files :key #'omnidiff-file-path :test #'equal))
                                (add (cl-find "add_plain.rs" files :key #'omnidiff-file-path :test #'equal))
                                (del (cl-find "del_plain.rs" files :key #'omnidiff-file-path :test #'equal)))
                            (should (omnidiff-file-binary-p bin))
                            (should (eq (omnidiff-file-summary emp) 'no-changes))
                            (should (eq (omnidiff-file-summary add) 'new-file))
                            (should (eq (omnidiff-file-summary del) 'deleted-file))))))
                  (when (buffer-live-p buf) (kill-buffer buf))))
              ;; Deleting an empty file matches serial parity cleanly.
              (delete-file (expand-file-name "add_empty.txt" repo))
              (git "add" "-A")
              (git "commit" "-q" "-m" "del empty")
              (should (equal (run-show 'default 4) (run-show 'default 1)))
              ;; Pure text A/D commit spawns zero worker jobs in JSON mode.
              (with-temp-file (expand-file-name "only_fast.rs" repo)
                (insert "fn fast_only() {\n    let ok = true;\n}\n"))
              (git "add" "only_fast.rs")
              (git "commit" "-q" "-m" "pure fast ad")
              (let* ((raw-meta
                      (with-temp-buffer
                        (set-buffer-multibyte nil)
                        (call-process "git" nil t nil "--no-pager" "show" "--raw" "--no-abbrev" "-z" "HEAD")
                        (buffer-string)))
                     (entries (cdr (omnidiff--parse-raw-git-entries raw-meta 'show)))
                     (sha (omnidiff--git-entry-new-sha (car entries)))
                     (cat-buf (generate-new-buffer " *p1-cat*")))
                (unwind-protect
                    (progn
                      (with-current-buffer cat-buf
                        (set-buffer-multibyte nil)
                        (call-process-region (concat sha "\n") nil "git" nil t nil "cat-file" "--batch"))
                      (let ((tmpdir-called nil))
                        (pcase-let ((`(,results . ,jobs)
                                     (omnidiff--prepare-parallel-work
                                      cat-buf (list sha) entries
                                      (lambda () (setq tmpdir-called t) repo)
                                      repo 'default)))
                          (should-not tmpdir-called)
                          (should-not jobs)
                          (should (= (length results) 1))
                          (let ((f (aref results 0)))
                            (should (omnidiff-file-p f))
                            (should (omnidiff-json-diff-p (omnidiff-file-json-diff f)))
                            (should (string-search "fast_only" (omnidiff-file-after-source f))))))
                      (should (equal (run-show 'default 4) (run-show 'default 1))))
                  (when (buffer-live-p cat-buf) (kill-buffer cat-buf)))))
          (delete-directory repo t))))))

(ert-deftest omnidiff-test/cached-layout-switching-cjk-wrap-and-hooks ()
  "Cached diff cycles all 3 layouts in memory, wraps CJK+spans accurately, and preserves overlays/hooks."
  (let* ((b-src "fn demo() {\nlet msg = \"旧中文内容\";\n}\n")
         (a-src "fn demo() {\nlet msg = \"新中文跨行高亮内容测试\";\n    let partial_refine_long_line_0123456789_continued_part;\n}\n")
         (f (omnidiff-test--make-json-file
             "src/demo.rs" b-src a-src
             :before-hunks (list (omnidiff-test--make-hunk 'update 1 10 1 (string-bytes "let msg = \"旧中文内容\"")))
             :after-hunks (list (omnidiff-test--make-hunk 'update 1 10 1 (string-bytes "let msg = \"新中文跨行高亮内容测试\""))
                                (omnidiff-test--make-hunk 'insert 2 4 2 (string-bytes "    let partial_refine_long_line_0123456789_continued_part;")))))
         (diff (make-omnidiff-diff :files (list f)))
         (buf (generate-new-buffer " *omnidiff-p2-cache-test*"))
         (orig-width (frame-width (selected-frame))))
    (unwind-protect
        (with-current-buffer buf
          (omnidiff-mode)
          (set-frame-width (selected-frame) 80)
          (setq omnidiff--diff diff
                omnidiff--layout 'unified
                omnidiff--highlight-style 'traditional)
          ;; Track change hooks and an external (e.g. scrollview) overlay across renders.
          (let ((before-calls 0)
                (after-calls nil)
                (ext-ov nil))
            (add-hook 'before-change-functions
                      (lambda (_s _e) (setq before-calls (1+ before-calls)))
                      nil t)
            (add-hook 'after-change-functions
                      (lambda (s e old-len) (push (list s e old-len) after-calls))
                      nil t)
            (omnidiff-render-buffer diff)
            (should (= before-calls 1))
            (should (= (length after-calls) 1))
            (setq ext-ov (make-overlay (point-min) (point-min) buf))
            (overlay-put ext-ov 'external-marker t)
            ;; Cycle unified -> side-by-side in memory (0 subprocesses).
            (goto-char (point-min))
            (search-forward "新中文跨行高亮内容测试")
            (let ((prev-size (buffer-size)))
              (omnidiff-cycle-layout)
              (should-not omnidiff--active-request)
              (should (eq omnidiff--layout 'side-by-side))
              ;; External overlay survived re-render; hooks fired once with accurate old-size.
              (should (eq (overlay-buffer ext-ov) buf))
              (should (= before-calls 2))
              (should (= (length after-calls) 2))
              (should (= (nth 2 (car after-calls)) prev-size))
              ;; Cursor stayed on line 2 of src/demo.rs.
              (should (equal (get-text-property (point) 'omnidiff-file) "src/demo.rs"))
              (should (eql (get-text-property (point) 'omnidiff-line) 2)))
            ;; In SBS, partial-refine wrapped line 3 preserves refine face on continuation row.
            (save-excursion
              (goto-char (point-min))
              (search-forward "_continued_pa")
              (let ((f1 (get-text-property (match-beginning 0) 'face)))
                (should (or (eq f1 'omnidiff-added-refine)
                            (and (listp f1) (memq 'omnidiff-added-refine f1)))))
              (search-forward "rt;")
              (let ((f2 (get-text-property (match-beginning 0) 'face)))
                (should (or (eq f2 'omnidiff-added-refine)
                            (and (listp f2) (memq 'omnidiff-added-refine f2))))))
            ;; Cycle side-by-side -> stacked in memory.
            (omnidiff-cycle-layout)
            (should-not omnidiff--active-request)
            (should (eq omnidiff--layout 'stacked))
            (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-search " BEFORE \n" txt))
              (should (string-search " AFTER  \n" txt))
              (should (string-search "let partial_refine_long_line_0123456789_continued_part;" txt)))
            ;; Cycle stacked -> unified in memory.
            (omnidiff-cycle-layout)
            (should-not omnidiff--active-request)
            (should (eq omnidiff--layout 'unified))))
      (set-frame-width (selected-frame) orig-width)
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest omnidiff-test/parallel-pipeline-edge-cases ()
  "Combined diffs, multi-commit `git show' ranges, and worker errors are handled cleanly."
  ;; 1. Unit checks for `git show' range & option gating.
  (should (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "HEAD")))
  (should (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "HEAD~1..HEAD")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "main...feat")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "HEAD" "HEAD~1")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "--stat" "HEAD")))
  ;; 2. Unit checks for combined diff (`::') and unmerged (`U') fallback detection.
  (let ((sha (make-string 40 ?a)))
    (should (eq (omnidiff--parse-raw-git-entries
                 (concat "commit " sha "\n\n    merge\n\0::100644 100644 100644 "
                         sha " " sha " " sha " MM\0f.txt\0")
                 'show)
                'fallback-serial))
    (should (eq (omnidiff--parse-raw-git-entries
                 (concat ":000000 100644 " omnidiff--zero-sha " " omnidiff--zero-sha " U\0f.txt\0")
                 'diff)
                'fallback-serial)))
  ;; 3. End-to-end checks on a real Git repo (merge commit, show A..B, failing worker).
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (progn
    (let* ((repo (make-temp-file "omnidiff-edge-repo-" t))
           (default-directory repo))
      (cl-flet ((git (&rest args)
                  (apply #'call-process "git" nil nil nil args))
                (run-buf (cmd label max-par &optional bin-override)
                  (let ((omnidiff-max-parallel max-par)
                        (omnidiff-executable (or bin-override omnidiff-executable))
                        (buf (generate-new-buffer (format " *omnidiff-edge-%s-%d*" label max-par))))
                    (unwind-protect
                        (progn
                          (omnidiff-run buf cmd label repo 'default 'unified)
                          (omnidiff-test--wait-request buf)
                          (with-current-buffer buf
                            (buffer-substring-no-properties (point-min) (point-max))))
                      (when (buffer-live-p buf) (kill-buffer buf))))))
        (unwind-protect
            (progn
              (should (zerop (git "init" "-q" "-b" "main")))
              (should (zerop (git "config" "user.name" "Test")))
              (should (zerop (git "config" "user.email" "test@example.com")))
              (with-temp-file (expand-file-name "conflict.txt" repo) (insert "base\n"))
              (with-temp-file (expand-file-name "ok.txt" repo) (insert "v1\n"))
              (should (zerop (git "add" ".")))
              (should (zerop (git "commit" "-q" "-m" "base commit")))
              (should (zerop (git "checkout" "-q" "-b" "side")))
              (with-temp-file (expand-file-name "conflict.txt" repo) (insert "side\n"))
              (should (zerop (git "commit" "-q" "-am" "side commit")))
              (should (zerop (git "checkout" "-q" "main")))
              (with-temp-file (expand-file-name "conflict.txt" repo) (insert "main\n"))
              (with-temp-file (expand-file-name "ok.txt" repo) (insert "v2\n"))
              (should (zerop (git "commit" "-q" "-am" "main commit")))
              (git "merge" "--no-commit" "side")
              (with-temp-file (expand-file-name "conflict.txt" repo) (insert "resolved\n"))
              (should (zerop (git "add" "conflict.txt")))
              (should (zerop (git "commit" "-q" "-m" "merge conflict resolved")))
              ;; 3a. Combined diff merge commit falls back cleanly and matches serial.
              (let ((s1 (run-buf '("git" "--no-pager" "show" "--ext-diff" "HEAD") "merge" 1))
                    (s4 (run-buf '("git" "--no-pager" "show" "--ext-diff" "HEAD") "merge" 4)))
                (should (equal s4 s1))
                (should (string-search "merge conflict resolved" s4))
                (should (string-search "resolved" s4)))
              ;; 3b. `git show A..B' falls back to serial and shows multiple commits.
              (let ((range-out (run-buf '("git" "--no-pager" "show" "--ext-diff" "HEAD~2..HEAD")
                                        "range" 4)))
                (should (string-search "merge conflict resolved" range-out))
                (should (string-search "main commit" range-out)))
              ;; 3c. Failing worker records diagnostic `:note' without breaking sibling file.
              (with-temp-file (expand-file-name "conflict.txt" repo) (insert "dirty-a\n"))
              (with-temp-file (expand-file-name "ok.txt" repo) (insert "dirty-b\n"))
              (let* ((real-bin (executable-find omnidiff-executable))
                     (fake-bin (expand-file-name "fake-omnidiff.sh" repo)))
                (with-temp-file fake-bin
                  (insert "#!/bin/sh\n"
                          "for arg in \"$@\"; do\n"
                          "  if [ \"$arg\" = \"conflict.txt\" ]; then exit 2; fi\n"
                          "done\n"
                          (format "exec %s \"$@\"\n" (shell-quote-argument real-bin))))
                (set-file-modes fake-bin #o755)
                (let ((out (run-buf '("git" "--no-pager" "diff" "--ext-diff")
                                    "worker-err" 4 fake-bin)))
                  (should (string-search "conflict.txt" out))
                  (should (string-search "omnidiff failed (exited abnormally with code 2)" out))
                  (should (string-search "ok.txt" out))
                  (should (string-search "dirty-b" out)))))
          (delete-directory repo t))))))

(ert-deftest omnidiff-test/async-error-always-terminates-request ()
  "Parse or stage exceptions never leave `omnidiff--active-request' stuck."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (let* ((repo (make-temp-file "omnidiff-err-term-" t))
         (default-directory repo))
    (cl-flet ((git (&rest args)
                (unless (zerop (apply #'call-process "git" nil nil nil args))
                  (error "git %S failed" args))))
      (unwind-protect
          (progn
            (git "init" "-q" "-b" "main")
            (git "config" "user.name" "Test")
            (git "config" "user.email" "test@example.com")
            (with-temp-file (expand-file-name "a.txt" repo) (insert "old-a\n"))
            (with-temp-file (expand-file-name "b.txt" repo) (insert "old-b\n"))
            (git "add" ".")
            (git "commit" "-q" "-m" "init")
            (with-temp-file (expand-file-name "a.txt" repo) (insert "new-a\n"))
            (with-temp-file (expand-file-name "b.txt" repo) (insert "new-b\n"))
            ;; 1. Single-worker JSON parse exception produces error card and finishes cleanly.
            (let ((buf (generate-new-buffer " *omnidiff-parse-err-json*"))
                  (orig-json-parse (symbol-function 'omnidiff-engine-parse-json)))
              (unwind-protect
                  (cl-letf (((symbol-function 'omnidiff-engine-parse-json)
                             (lambda (json-str)
                               (if (string-search "a.txt" json-str)
                                   (error "synthetic json parse boom")
                                 (funcall orig-json-parse json-str)))))
                    (omnidiff-run buf '("git" "--no-pager" "diff" "--ext-diff")
                                  "parse-err-json" repo 'default 'unified)
                    (omnidiff-test--wait-request buf 5.0)
                    (with-current-buffer buf
                      (should-not omnidiff--active-request)
                      (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                        (should (string-search "omnidiff parse error: synthetic json parse boom" txt))
                        (should (string-search "new-b" txt)))))
                (when (buffer-live-p buf) (kill-buffer buf))))
            ;; 2. Stage preparation exception fails cleanly and clears pending request.
            (let ((buf (generate-new-buffer " *omnidiff-prep-err*")))
              (unwind-protect
                  (cl-letf (((symbol-function 'omnidiff--prepare-parallel-work)
                             (lambda (&rest _) (error "synthetic prep boom"))))
                    (omnidiff-run buf '("git" "--no-pager" "diff" "--ext-diff")
                                  "prep-err" repo 'default 'unified)
                    (omnidiff-test--wait-request buf 5.0)
                    (with-current-buffer buf
                      (should-not omnidiff--active-request)))
                (when (buffer-live-p buf) (kill-buffer buf)))))
        (delete-directory repo t)))))

(ert-deftest omnidiff-test/git-parallel-gating-sha256-and-blob-budget ()
  "Parallel gating rejects unsafe flags, handles SHA-256 OIDs, and enforces blob byte budget."
  ;; 1. `git diff' & `git show' flag gating.
  (should (omnidiff--git-parallel-spec '("git" "--no-pager" "diff" "--ext-diff")))
  (should (omnidiff--git-parallel-spec '("git" "--no-pager" "diff" "--ext-diff" "--cached")))
  (should (omnidiff--git-parallel-spec '("git" "--no-pager" "diff" "--ext-diff" "HEAD" "--" "a.el")))
  (should (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "HEAD" "--" "a.el")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "diff" "--ext-diff" "--name-only")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "diff" "--ext-diff" "--stat")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "diff" "--ext-diff" "-Sfoo")))
  ;; Malformed non-raw output triggers fallback instead of empty diff.
  (should (eq (omnidiff--parse-raw-git-entries "foo.el\0bar.el\0" 'diff) 'fallback-serial))
  ;; 2. SHA-256 64-char OID support (show preamble + zero-OID detection).
  (let* ((z64 (make-string 64 ?0))
         (oid64 (make-string 64 ?a))
         (raw-show (concat "commit " oid64 "\nAuthor: Bob\n\n    sha256 commit\n\n"
                           ":000000 100644 " z64 " " oid64 " A\0sha256.rs\0"))
         (parsed (omnidiff--parse-raw-git-entries raw-show 'show)))
    (should (consp parsed))
    (should (string-search "sha256 commit" (car parsed)))
    (should (= (length (cdr parsed)) 1))
    (should (omnidiff--zero-oid-p (omnidiff--git-entry-old-sha (cadr parsed))))
    (should (equal (omnidiff--git-entry-new-sha (cadr parsed)) oid64))
    (should (equal (omnidiff--git-entry-path (cadr parsed)) "sha256.rs")))
  ;; 3. `omnidiff-max-parallel-blob-bytes' overflow falls back cleanly to serial `GIT_EXTERNAL_DIFF'.
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (let* ((repo (make-temp-file "omnidiff-budget-" t))
         (default-directory repo))
    (cl-flet ((git (&rest args)
                (unless (zerop (apply #'call-process "git" nil nil nil args))
                  (error "git %S failed" args))))
      (unwind-protect
          (progn
            (git "init" "-q" "-b" "main")
            (git "config" "user.name" "Test")
            (git "config" "user.email" "test@example.com")
            (with-temp-file (expand-file-name "big.txt" repo)
              (insert (make-string 512 ?a) "\n"))
            (git "add" ".")
            (git "commit" "-q" "-m" "v1")
            (with-temp-file (expand-file-name "big.txt" repo)
              (insert (make-string 512 ?b) "\n"))
            (git "commit" "-q" "-am" "v2")
            (let ((buf-cap (generate-new-buffer " *omnidiff-budget-cap*"))
                  (buf-ser (generate-new-buffer " *omnidiff-budget-ser*")))
              (unwind-protect
                  (progn
                    (let ((omnidiff-max-parallel 4)
                          (omnidiff-max-parallel-blob-bytes 32))
                      (omnidiff-run buf-cap '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                    "cap" repo 'default 'unified)
                      (omnidiff-test--wait-request buf-cap))
                    (let ((omnidiff-max-parallel 1))
                      (omnidiff-run buf-ser '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                    "ser" repo 'default 'unified)
                      (omnidiff-test--wait-request buf-ser))
                    (should (equal (with-current-buffer buf-cap
                                     (buffer-substring-no-properties (point-min) (point-max)))
                                   (with-current-buffer buf-ser
                                     (buffer-substring-no-properties (point-min) (point-max))))))
                (when (buffer-live-p buf-cap) (kill-buffer buf-cap))
                (when (buffer-live-p buf-ser) (kill-buffer buf-ser)))))
        (delete-directory repo t)))))

(ert-deftest omnidiff-test/paths-modes-navigation-theme-and-trim ()
  "Verify literal paths, mode changes, dual-side restore/RET, folded nav, SBS window, Magit, light theme, and O(n) trim."
  ;; 1. Literal CRLF path decoding & repo-relative `~/' protection.
  (let* ((sha-a (make-string 40 ?a))
         (sha-b (make-string 40 ?b))
         (raw (concat ":100644 100644 " sha-a " " sha-b " M\0dir/a\r\nb.el\0"))
         (parsed (omnidiff--parse-raw-git-entries (string-to-unibyte raw) 'diff)))
    (should (equal (omnidiff--git-entry-path (cadr parsed)) "dir/a\r\nb.el")))
  (let ((repo-dir (make-temp-file "omnidiff-tilde-" t)))
    (unwind-protect
        (should (string-prefix-p (file-name-as-directory repo-dir)
                                 (omnidiff--expand-repo-path "~/local.el" repo-dir)))
      (delete-directory repo-dir t)))
  ;; 3. Light theme `neutral-paper' differs from `calm-modus'.
  (let* ((light-base (make-omnidiff-theme--base
                      :bg-main "#ffffff" :bg-dim "#f2f2f2" :bg-inactive "#e6e6e6"
                      :bg-active "#d7d7d7" :bg-card "#f0f0f0" :bg-chrome "#f5f5f5"
                      :border "#cccccc" :accent "#0031a9" :dark-p nil
                      :fg-main "#000000" :fg-dim "#595959"
                      :bg-added-faint "#e0f6e0" :bg-added-refine "#bae0ba" :fg-added "#005000"
                      :bg-removed-faint "#ffe8e8" :bg-removed-refine "#f5c2c2" :fg-removed "#8f1313"
                      :bg-changed-faint "#fff3d0" :bg-changed-refine "#ebd080" :fg-changed "#553d00"
                      :blue-cooler "#0000b0" :green-faint "#2a5045" :red-faint "#7f0000"
                      :yellow-faint "#624416" :indigo "#4a3ab0"
                      :bg-green-nuanced "#ecf7ed" :bg-sage "#c1e7c1" :bg-ochre "#f0d890"
                      :fg-ochre "#624416" :bg-lavender "#ece8ff" :raw-moved "#ece8ff"))
         (p-calm (omnidiff-theme--resolve-palette light-base 'calm-modus))
         (p-paper (omnidiff-theme--resolve-palette light-base 'neutral-paper)))
    (should-not (equal (omnidiff-theme--palette-add-fg p-calm)
                       (omnidiff-theme--palette-add-fg p-paper)))
    (should-not (equal (omnidiff-theme--palette-add-bg p-calm)
                       (omnidiff-theme--palette-add-bg p-paper)))
    (should (eq (omnidiff-theme--palette-ref-weight p-paper) 'normal)))
  ;; 4. Dual-side cursor restoration (BEFORE vs AFTER on same line number) & folded hunk nav & RET.
  (let* ((fa (omnidiff-test--make-json-file
              "a.rs"
              "keep_top();\nlet old_only = 1;\nremoved_line();\nkeep_bot();\n"
              "keep_top();\nlet new_only = 2;\nkeep_bot();\n"
              :before-hunks (list (omnidiff-test--make-hunk 'update 1 4 1 12)
                                  (omnidiff-test--make-hunk 'delete 2 0 2 15))
              :after-hunks (list (omnidiff-test--make-hunk 'update 1 4 1 12))))
         (fb (omnidiff-test--make-json-file
              "b.rs" "second_old\n" "second_new\n"
              :before-hunks (list (omnidiff-test--make-hunk 'update 0 0 0 10))
              :after-hunks (list (omnidiff-test--make-hunk 'update 0 0 0 10))))
         (diff (make-omnidiff-diff :files (list fa fb)))
         (buf (generate-new-buffer " *omnidiff-nav-restore*"))
         (dir (make-temp-file "omnidiff-ret-del-" t))
         (visited nil))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.rs" dir)
            (insert "keep_top();\nlet new_only = 2;\nkeep_bot();\n"))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--diff diff
                  omnidiff--repo-dir dir
                  omnidiff--layout 'unified)
            (omnidiff-render-buffer diff)
            ;; 4a. Put point on AFTER line 2 ("new_only"), cycle unified -> sbs -> stacked.
            (goto-char (point-min))
            (search-forward "new_only")
            (should (eq (get-text-property (point) 'omnidiff-side) 'after))
            (omnidiff-cycle-layout) ; -> side-by-side (lands in right cell)
            (should (eq (get-text-property (point) 'omnidiff-side) 'after))
            (should (eql (get-text-property (point) 'omnidiff-new-line) 2))
            (omnidiff-cycle-layout) ; -> stacked (lands on AFTER line 2, not BEFORE line 2!)
            (should (eq (get-text-property (point) 'omnidiff-side) 'after))
            (should (string-search "new_only"
                                   (buffer-substring-no-properties
                                    (line-beginning-position) (line-end-position))))
            ;; 4b. Put point on BEFORE line 2 ("old_only") in stacked -> stays on BEFORE side.
            (goto-char (point-min))
            (search-forward "old_only")
            (should (eq (get-text-property (point) 'omnidiff-side) 'before))
            (omnidiff-cycle-layout) ; -> unified
            (should (eq (get-text-property (point) 'omnidiff-side) 'before))
            (should (string-search "old_only"
                                   (buffer-substring-no-properties
                                    (line-beginning-position) (line-end-position))))
            ;; 4c. RET on deleted-only line 3 jumps to nearest working-tree new line (2 or 3).
            (goto-char (point-min))
            (search-forward "removed_line")
            (omnidiff-jump-to-source)
            (setq visited (current-buffer))
            (should (memq (line-number-at-pos) '(2 3))))
          ;; 4d. Fold first file -> `n' skips invisible hunk and lands on visible second file's hunk.
          (with-current-buffer buf
            (goto-char (point-min))
            (omnidiff-toggle-fold)
            (omnidiff-next-hunk)
            (should-not (invisible-p (point)))
            (should (equal (get-text-property (point) 'omnidiff-file) "b.rs"))
            (omnidiff-prev-hunk)
            (should-not (invisible-p (point)))
            (should (equal (get-text-property (point) 'omnidiff-file) "b.rs"))))
      (when (buffer-live-p visited) (kill-buffer visited))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory dir t)))
  ;; 5. `commit-and-render' displays buffer before rendering so SBS measures actual target window.
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true"))
        (buf (generate-new-buffer " *omnidiff-pop-order*"))
        (dir (make-temp-file "omnidiff-pop-" t))
        (events nil)
        (orig-render (symbol-function 'omnidiff-render-buffer)))
    (unwind-protect
        (cl-letf (((symbol-function 'pop-to-buffer)
                   (lambda (b &rest _) (push 'pop events) b))
                  ((symbol-function 'omnidiff-render-buffer)
                   (lambda (d) (push 'render events) (funcall orig-render d))))
          (omnidiff-run buf '("sh" "-c" "printf 'commit ok\\n'") "pop" dir 'default 'side-by-side)
          (omnidiff-test--wait-request buf)
          (should (equal (nreverse events) '(pop render))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory dir t)))
  ;; 6. Mode-only Git change + Magit file filter + root commit DWIM.
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (let* ((repo (make-temp-file "omnidiff-mode-magit-" t))
         (default-directory repo))
    (cl-flet ((git (&rest args)
                (unless (zerop (apply #'call-process "git" nil nil nil args))
                  (error "git %S failed" args))))
      (unwind-protect
          (progn
            (git "init" "-q" "-b" "main")
            (git "config" "user.name" "Test")
            (git "config" "user.email" "test@example.com")
            (with-temp-file (expand-file-name "run.sh" repo) (insert "#!/bin/sh\necho hi\n"))
            (with-temp-file (expand-file-name "other.txt" repo) (insert "v1\n"))
            (git "add" ".")
            (git "commit" "-q" "-m" "root commit")
            ;; 6a. Root commit via `(commit . HEAD)' delegates to `omnidiff-show' and succeeds.
            (let ((omnidiff-single-file-entry 'overview)
                  (buf (get-buffer-create "*git show omnidiff HEAD*")))
              (unwind-protect
                  (progn
                    (omnidiff-diff '(commit . "HEAD") '("run.sh"))
                    (omnidiff-test--wait-request buf)
                    (with-current-buffer buf
                      (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                        (should (string-search "root commit" txt))
                        (should (string-search "run.sh" txt))
                        (should-not (string-search "other.txt" txt)))))
                (when (buffer-live-p buf) (kill-buffer buf))))
            ;; 6b. Mode-only change (`chmod +x`) matches serial/parallel in headless and renders cleanly in JSON.
            (set-file-modes (expand-file-name "run.sh" repo) #o755)
            (git "add" "run.sh")
            (git "commit" "-q" "-m" "chmod +x")
            (let ((buf-s (generate-new-buffer " *omnidiff-mode-s*"))
                  (buf-p (generate-new-buffer " *omnidiff-mode-p*"))
                  (buf-j (generate-new-buffer " *omnidiff-mode-j*")))
              (unwind-protect
                  (progn
                    (let ((omnidiff-max-parallel 1))
                      (omnidiff-run buf-s '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                    "mode" repo 'default 'unified)
                      (omnidiff-test--wait-request buf-s))
                    (let ((omnidiff-max-parallel 4))
                      (omnidiff-run buf-p '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                    "mode" repo 'default 'unified)
                      (omnidiff-test--wait-request buf-p))
                    (let ((ts (with-current-buffer buf-s
                                (buffer-substring-no-properties (point-min) (point-max))))
                          (tp (with-current-buffer buf-p
                                (buffer-substring-no-properties (point-min) (point-max)))))
                      (should (equal tp ts))
                      (should (string-search "100644→100755" tp))
                      (should (string-search "Mode changed: 100644 → 100755" tp))))
                (when (buffer-live-p buf-s) (kill-buffer buf-s))
                (when (buffer-live-p buf-p) (kill-buffer buf-p))
                (when (buffer-live-p buf-j) (kill-buffer buf-j))))
            ;; 6c. `omnidiff-diff' with file filter limits unstaged diff to requested path.
            (with-temp-file (expand-file-name "run.sh" repo) (insert "#!/bin/sh\necho changed\n"))
            (with-temp-file (expand-file-name "other.txt" repo) (insert "v2\n"))
            (let ((omnidiff-single-file-entry 'overview)
                  (buf (get-buffer-create "*git diff omnidiff*")))
              (unwind-protect
                  (progn
                    (omnidiff-diff nil '("other.txt"))
                    (omnidiff-test--wait-request buf)
                    (with-current-buffer buf
                      (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                        (should (string-search "other.txt" txt))
                        (should-not (string-search "run.sh" txt)))))
                (when (buffer-live-p buf) (kill-buffer buf)))))
        (delete-directory repo t)))))

(defconst omnidiff-test--png-red-2x2
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
   #x00 #x00 #x00 #x02 #x00 #x00 #x00 #x02 #x08 #x02 #x00 #x00 #x00 #xfd #xd4 #x9a
   #x73 #x00 #x00 #x00 #x10 #x49 #x44 #x41 #x54 #x78 #x9c #x63 #xb8 #xa3 #x61 #x03
   #x44 #x0c #x10 #x0a #x00 #x25 #x8e #x05 #x01 #xf5 #x5e #xb0 #x29 #x00 #x00 #x00
   #x00 #x49 #x45 #x4e #x44 #xae #x42 #x60 #x82)
  "Minimal valid 2x2 red RGB PNG for image diff tests.")

(defconst omnidiff-test--png-blue-2x2
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
   #x00 #x00 #x00 #x02 #x00 #x00 #x00 #x02 #x08 #x02 #x00 #x00 #x00 #xfd #xd4 #x9a
   #x73 #x00 #x00 #x00 #x10 #x49 #x44 #x41 #x54 #x78 #x9c #x63 #xd0 #xa8 #x78 #x06
   #x44 #x0c #x10 #x0a #x00 #x27 #xbe #x06 #x19 #x55 #x8d #xca #x90 #x00 #x00 #x00
   #x00 #x49 #x45 #x4e #x44 #xae #x42 #x60 #x82)
  "Minimal valid 2x2 blue RGB PNG for image diff tests.")

(defconst omnidiff-test--png-wide-4x2
  (unibyte-string
   #x89 #x50 #x4e #x47 #x0d #x0a #x1a #x0a #x00 #x00 #x00 #x0d #x49 #x48 #x44 #x52
   #x00 #x00 #x00 #x04 #x00 #x00 #x00 #x02 #x08 #x02 #x00 #x00 #x00 #xf0 #xca #xea
   #x34 #x00 #x00 #x00 #x10 #x49 #x44 #x41 #x54 #x78 #x9c #x63 #xd0 #x38 #x91 #x02
   #x47 #x0c #xc8 #x1c #x00 #x88 #x5a #x0a #xa1 #xca #x03 #xe0 #x28 #x00 #x00 #x00
   #x00 #x49 #x45 #x4e #x44 #xae #x42 #x60 #x82)
  "Minimal valid 4x2 green RGB PNG with different aspect ratio.")

(defun omnidiff-test--write-bin (path bytes)
  "Write unibyte BYTES verbatim to PATH."
  (let ((coding-system-for-write 'no-conversion))
    (with-temp-file path
      (set-buffer-multibyte nil)
      (insert bytes))))

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

(ert-deftest omnidiff-test/image-modes-svg-preview-limits-and-cleanup ()
  "Verify TTY fallback, all 6 GUI visual modes, SVG preview, limits, RET, and cleanup."
  (skip-unless (executable-find "git"))
  (let* ((repo (make-temp-file "omnidiff-img-ui-" t))
         (default-directory repo)
         (buf (generate-new-buffer " *omnidiff-img-ui*"))
         (opened-buf nil))
    (cl-flet ((git-out (&rest args)
                (with-temp-buffer
                  (unless (zerop (apply #'call-process "git" nil t nil args))
                    (error "git %S failed" args))
                  (string-trim (buffer-string)))))
      (unwind-protect
          (progn
            (git-out "init" "-q" "-b" "main")
            (git-out "config" "user.name" "Test")
            (git-out "config" "user.email" "test@example.com")
            (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-red-2x2)
            (omnidiff-test--write-bin (expand-file-name "shape.png" repo) omnidiff-test--png-red-2x2)
            (omnidiff-test--write-bin (expand-file-name "gone.png" repo) omnidiff-test--png-red-2x2)
            (with-temp-file (expand-file-name "icon.svg" repo)
              (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\"><rect width=\"24\" height=\"24\" fill=\"red\"/></svg>\n"))
            (git-out "add" ".")
            (git-out "commit" "-q" "-m" "v1")
            (let ((oid-red (git-out "rev-parse" "HEAD:mod.png"))
                  (oid-svg-old (git-out "rev-parse" "HEAD:icon.svg")))
              (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-blue-2x2)
              (omnidiff-test--write-bin (expand-file-name "shape.png" repo) omnidiff-test--png-wide-4x2)
              (delete-file (expand-file-name "gone.png" repo))
              (with-temp-file (expand-file-name "icon.svg" repo)
                (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\"><rect width=\"24\" height=\"24\" fill=\"blue\"/></svg>\n"))
              (git-out "add" "-A")
              (git-out "commit" "-q" "-m" "v2")
              (let* ((oid-blue (git-out "rev-parse" "HEAD:mod.png"))
                     (oid-wide (git-out "rev-parse" "HEAD:shape.png"))
                     (oid-svg-new (git-out "rev-parse" "HEAD:icon.svg"))
                     (diff
                      (make-omnidiff-diff
                       :files
                       (list
                        (make-omnidiff-file
                         :path "mod.png" :target-path "mod.png" :status 'modified
                         :old-oid oid-red :new-oid oid-blue
                         :picture (make-omnidiff-picture
                                   :before (make-omnidiff-picture-info
                                            :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                                   :after (make-omnidiff-picture-info
                                           :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                                   :comparison 'pixels :changed-pct 100.0
                                   :changed-pixels 4 :total-pixels 4 :region-count 1
                                   :regions (list (make-omnidiff-picture-region
                                                   :x 0 :y 0 :width 2 :height 2))))
                        (make-omnidiff-file
                         :path "shape.png" :target-path "shape.png" :status 'modified
                         :old-oid oid-red :new-oid oid-wide
                         :picture (make-omnidiff-picture
                                   :before (make-omnidiff-picture-info
                                            :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                                   :after (make-omnidiff-picture-info
                                           :format "PNG" :width 4 :height 2 :color "RGB8" :bytes 73)
                                   :comparison 'resized))
                        (make-omnidiff-file
                         :path "gone.png" :target-path "gone.png" :status 'deleted
                         :old-oid oid-red :new-oid nil
                         :picture (make-omnidiff-picture
                                   :before (make-omnidiff-picture-info
                                            :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                                   :after nil :comparison 'one-sided))
                        (omnidiff-test--make-json-file
                         "icon.svg" "fill=\"red\"\n" "fill=\"blue\"\n"
                         :old-oid oid-svg-old :new-oid oid-svg-new
                         :before-hunks (list (omnidiff-test--make-hunk 'update 0 6 0 9))
                         :after-hunks (list (omnidiff-test--make-hunk 'update 0 6 0 10)))))))
                (with-current-buffer buf
                  (omnidiff-mode)
                  (setq omnidiff--repo-dir repo
                        omnidiff--diff diff
                        omnidiff--layout 'unified)
                  ;; 1. TTY mode: clean metadata cards, delta bars, regions, and no redundant SVG preview.
                  (omnidiff-render-buffer diff)
                  (let ((tty (buffer-substring-no-properties (point-min) (point-max))))
                    (should (string-search "PNG 2×2" tty))
                    (should (string-search "100.0% Δ" tty))
                    ;; Raster cards omit the redundant 1/1 "Image diff" hunk header.
                    (should-not (string-search "Image diff" tty))
                    (should (string-search "Changed regions (showing 1 / 1):" tty))
                    (should (string-search "2×2 at (0, 0)" tty))
                    (should (string-search "Resized to a different aspect ratio" tty))
                    (should-not (string-search "SVG preview" tty))
                    (should (string-search "fill=\"blue\"" tty)))
                  ;; TTY `r' toggles changed-regions visibility; sub-cell bar never lies for tiny %.
                  (goto-char (point-min))
                  (omnidiff-image-toggle-regions)
                  (should-not (string-search "Changed regions"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                  (omnidiff-image-toggle-regions)
                  (should (string-prefix-p "▏" (omnidiff-image--format-tty-bar 0.1 20)))
                  (should (equal (omnidiff-theme-color-hex "blue") "#0000ff"))
                  ;; 2. RET on deleted image opens read-only before-blob buffer.
                  (goto-char (point-min))
                  (search-forward "gone.png")
                  (save-current-buffer
                    (omnidiff-jump-to-source)
                    (setq opened-buf (current-buffer))
                    (should (string-search "gone.png (before)" (buffer-name opened-buf)))
                    (should buffer-read-only)
                    (kill-buffer opened-buf)
                    (setq opened-buf nil))
                  ;; 2b. RET on historical `git show' modified image opens exact commit blob, not worktree edits.
                  (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-wide-4x2)
                  (let ((omnidiff--command '("git" "--no-pager" "show" "--ext-diff" "HEAD")))
                    (goto-char (point-min))
                    (save-current-buffer
                      (omnidiff-jump-to-source)
                      (setq opened-buf (current-buffer))
                      (should (string-search "mod.png (after)" (buffer-name opened-buf)))
                      (should (= (buffer-size opened-buf) (length omnidiff-test--png-blue-2x2)))
                      (kill-buffer opened-buf)
                      (setq opened-buf nil)))
                  (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-blue-2x2)
                  ;; 3. GUI rendering & all interactive visual modes (when PNG/SVG support exists).
                  (when (and (image-type-available-p 'png)
                             (image-type-available-p 'svg))
                    (cl-letf (((symbol-function 'omnidiff-image--display-graphic-p) (lambda () t))
                              ((symbol-function 'omnidiff-image--viewport-px) (lambda () '(1200 . 600))))
                      (omnidiff-render-buffer diff)
                      ;; 3a. Unified defaults to swipe (native slice specs) for mod.png,
                      ;; degrades to side-by-side for resized shape.png, and adds SVG preview above XML.
                      (let ((txt (buffer-substring-no-properties (point-min) (point-max)))
                            (displays (omnidiff-test--collect-display-props)))
                        (should (string-search "3:swipe 50%" txt))
                        (should (string-search "SVG preview" txt))
                        (should (string-search "fill=\"blue\"" txt))
                        (should (cl-some (lambda (d)
                                           (and (consp d) (consp (car d))
                                                (eq (caar d) 'slice)))
                                         displays)))
                      ;; 3b. Navigate with `n' onto mod.png card -> image commands work directly!
                      (goto-char (point-min))
                      (omnidiff-next-hunk)
                      (should (omnidiff-image--card-ov-at))
                      ;; Fold flattened raster card -> background rerender stays folded, user `l' unfolds!
                      (omnidiff-toggle-fold)
                      (let ((fov (get-text-property (point) 'omnidiff-body-ov))
                            (cov (omnidiff-image--card-ov-at)))
                        (should (eq (overlay-get fov 'invisible) 'omnidiff-fold))
                        (omnidiff-image--rerender-card cov nil)
                        (should (eq (overlay-get fov 'invisible) 'omnidiff-fold))
                        (omnidiff-image-adjust-increase)
                        (should-not (overlay-get fov 'invisible))
                        (should (string-search "3:swipe 55%"
                                               (buffer-substring-no-properties (point-min) (point-max)))))
                      ;; Fine step, reset swipe to 50%, regions loupe + live multi-file header tracking!
                      (omnidiff-image-adjust-fine-increase)
                      (should (string-search "3:swipe 56%"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                      (omnidiff-image-adjust-reset)
                      (should (string-search "3:swipe 50%"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                      (omnidiff-image-toggle-regions)
                      (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                        (should (string-search "Changed regions (showing 1 / 1):" txt))
                        (should (string-search "Region 1/1" txt))
                        (should (string-search "loupe" txt)))
                      ;; Expanding card #1 must NOT corrupt file header resolution on card #2!
                      (save-excursion
                        (goto-char (point-min))
                        (omnidiff-next-file)
                        (let ((hdr2 (line-beginning-position)))
                          (forward-line 1)
                          (should (= (omnidiff-render-file-header-pos-at (point)) hdr2))))
                      (omnidiff-image-toggle-regions)
                      ;; Zoom + backdrop cycle + non-destructive blink peek (`b' then `DEL').
                      (omnidiff-image-zoom-in)
                      (should (omnidiff-image--state-zoom
                               (omnidiff-image--state-for (car (omnidiff-diff-files diff)))))
                      (omnidiff-image-zoom-reset)
                      (should-not (omnidiff-image--state-zoom
                                   (omnidiff-image--state-for (car (omnidiff-diff-files diff)))))
                      (omnidiff-image-cycle-backdrop)
                      (should (eq (omnidiff-image--state-backdrop
                                   (omnidiff-image--state-for (car (omnidiff-diff-files diff))))
                                  'checker))
                      ;; Blink peek from swipe -> `DEL' returns cleanly to swipe.
                      (omnidiff-image-toggle-side)
                      (should (string-search "6:blink (old)"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                      (omnidiff-image-return-from-peek)
                      (should (string-search "3:swipe 50%"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                      ;; Onion steps cleanly by 0.1 and returns to 50%.
                      (omnidiff-image-mode-onion)
                      (omnidiff-image-adjust-increase)
                      (should (string-search "5:onion 60%"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                      (omnidiff-image-adjust-decrease)
                      (should (string-search "5:onion 50%"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                      ;; Diff mode: spotlight 2-up by default when ImageMagick is installed,
                      ;; cycling cleanly through spotlight -> mask -> delta via `s'.
                      (omnidiff-image-mode-diff)
                      (should (string-search "4:diff"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                      (cl-flet ((wait-workers ()
                                  (let ((deadline (+ (float-time) 5.0)))
                                    (while (and omnidiff-image--workers (< (float-time) deadline))
                                      (accept-process-output nil 0.05)))
                                  (should-not omnidiff-image--workers)))
                        (wait-workers)
                        (when (omnidiff-image--magick-convert-command)
                          (let ((sbs-txt (buffer-substring-no-properties (point-min) (point-max))))
                            (should (string-search "spotlight 2-up" sbs-txt))
                            (should (string-search "BEFORE" sbs-txt))
                            (should (string-search "AFTER" sbs-txt)))
                          ;; Cycle -> single spotlight
                          (omnidiff-image-cycle-diff-style)
                          (wait-workers)
                          (should (string-search "spotlight (s)"
                                                 (buffer-substring-no-properties (point-min) (point-max)))))
                        (when (omnidiff-image--magick-command)
                          ;; Cycle -> classic magick compare mask
                          (omnidiff-image-cycle-diff-style)
                          (wait-workers)
                          (should (string-search "mask (magick)"
                                                 (buffer-substring-no-properties (point-min) (point-max)))))
                        ;; Cycle -> built-in SVG delta
                        (when (> (length (omnidiff-image--available-diff-styles
                                          (and (omnidiff-image--magick-command) t)))
                                 1)
                          (omnidiff-image-cycle-diff-style)
                          (should (string-search "delta ×4.0 (RGB+α)"
                                                 (buffer-substring-no-properties (point-min) (point-max))))
                          ;; Cycle wraps back to spotlight 2-up
                          (omnidiff-image-cycle-diff-style)
                          (wait-workers)
                          (when (omnidiff-image--magick-convert-command)
                            (should (string-search "spotlight 2-up"
                                                   (buffer-substring-no-properties (point-min) (point-max)))))))
                      ;; 3c. Resized image refuses swipe & `='; single-sided image refuses `b'.
                      (omnidiff-next-file)
                      (should-error (omnidiff-image-mode-swipe) :type 'user-error)
                      (should-error (omnidiff-image-adjust-reset) :type 'user-error)
                      (omnidiff-next-file) ; gone.png (deleted)
                      (should-error (omnidiff-image-toggle-side) :type 'user-error)
                      ;; 3d. Plain `v' preserves pinned per-card mode; `C-u v' resets all overrides.
                      (goto-char (point-min))
                      (omnidiff-cycle-layout) ; -> side-by-side
                      (should (eq omnidiff--layout 'side-by-side))
                      (should (eq (omnidiff-image--state-mode
                                   (omnidiff-image--state-for (car (omnidiff-diff-files diff))))
                                  'diff))
                      (omnidiff-cycle-layout t) ; C-u v -> stacked + reset pinned overrides
                      (should (eq omnidiff--layout 'stacked))
                      (should-not (omnidiff-image--state-mode
                                   (omnidiff-image--state-for (car (omnidiff-diff-files diff)))))
                      ;; 3e. Narrow viewport honestly degrades 2-up toolbar to `2:stack · narrow window'.
                      (cl-letf (((symbol-function 'omnidiff-image--viewport-px) (lambda () '(280 . 400))))
                        (setq omnidiff--layout 'side-by-side)
                        (omnidiff-render-buffer diff)
                        (let ((narrow (buffer-substring-no-properties (point-min) (point-max))))
                          (should (string-search "2:stack" narrow))
                          (should (string-search "narrow window" narrow))))
                      ;; 3f. Auto-load limit defers extra cards; `C-u L' loads all deferred cards.
                      (omnidiff-image-cleanup)
                      (let ((omnidiff-image-auto-load-limit 1))
                        (setq omnidiff--layout 'unified)
                        (omnidiff-render-buffer diff)
                        (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                          (should (string-search "Preview deferred (> 1 auto-loaded images)" txt)))
                        (goto-char (point-min))
                        (omnidiff-image-force-load t)
                        (should (omnidiff-image--state-force-load
                                 (omnidiff-image--state-for (nth 1 (omnidiff-diff-files diff))))))
                      ;; 3g. Max-bytes guard defers oversized image until `L'.
                      (omnidiff-image-cleanup)
                      (let ((omnidiff-image-max-bytes 10))
                        (omnidiff-render-buffer diff)
                        (should (string-search "exceeds 10 B auto-load limit"
                                               (buffer-substring-no-properties (point-min) (point-max))))))
                    ;; 4. Verify alpha-correct SVG compositing, region auto-aim, and compact identical card.
                    (let* ((svg-diff (omnidiff-image--build-svg-diff
                                      'b omnidiff-test--png-red-2x2 "image/png"
                                      'a omnidiff-test--png-blue-2x2 "image/png" 64 64 4.0))
                           (svg-onion-100 (omnidiff-image--build-svg-onion
                                           'b omnidiff-test--png-red-2x2 "image/png"
                                           'a omnidiff-test--png-blue-2x2 "image/png"
                                           64 64 1.0 'checker t))
                           (aim-pic (make-omnidiff-picture
                                     :regions (list (make-omnidiff-picture-region
                                                     :x 76 :y 10 :width 8 :height 8)))))
                      (should (string-search "#808080" svg-diff))
                      (should (string-search "isolation: isolate; mix-blend-mode: difference" svg-diff))
                      ;; At 100% onion, BEFORE must not be embedded at all so erased alpha never leaks.
                      (should-not (string-search (base64-encode-string omnidiff-test--png-red-2x2 t)
                                                 svg-onion-100))
                      ;; Region centered at x=80 of 100px width auto-aims swipe to 0.8.
                      (should (= (omnidiff-image--default-swipe aim-pic 100) 0.8)))
                    (let ((ident-diff
                           (make-omnidiff-diff
                            :files
                            (list (make-omnidiff-file
                                   :path "same.png" :target-path "same.png" :status 'modified
                                   :old-oid oid-red :new-oid oid-red
                                   :picture (make-omnidiff-picture
                                             :before (make-omnidiff-picture-info
                                                      :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                                             :after (make-omnidiff-picture-info
                                                     :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                                             :comparison 'identical :changed-pct 0.0))))))
                      (cl-letf (((symbol-function 'omnidiff-image--display-graphic-p) (lambda () t))
                                ((symbol-function 'omnidiff-image--viewport-px) (lambda () '(1200 . 600))))
                        (omnidiff-image-cleanup)
                        (omnidiff-render-buffer ident-diff)
                        (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                          (should (string-search "IDENTICAL" txt))
                          (should (string-search "i:compare" txt)))
                        (goto-char (point-min))
                        (omnidiff-image-cycle-mode)
                        (should (string-search "3:swipe"
                                               (buffer-substring-no-properties (point-min) (point-max)))))))))))
        (when (buffer-live-p opened-buf) (kill-buffer opened-buf))
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-directory repo t)))))

(ert-deftest omnidiff-test/image-git-parallel-and-serial-parity ()
  "Parallel and serial Git pipelines produce identical picture/SVG diffs and OIDs."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (let* ((repo (make-temp-file "omnidiff-img-git-" t))
         (default-directory repo))
    (cl-flet ((git (&rest args)
                (unless (zerop (apply #'call-process "git" nil nil nil args))
                  (error "git %S failed" args)))
              (run-show (max-par)
                (let ((omnidiff-max-parallel max-par)
                      (buf (generate-new-buffer (format " *omnidiff-img-par-%d*" max-par))))
                  (unwind-protect
                      (progn
                        (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                      "show HEAD" repo 'default 'unified)
                        (omnidiff-test--wait-request buf)
                        (with-current-buffer buf
                          (list (buffer-substring-no-properties (point-min) (point-max))
                                (mapcar (lambda (f)
                                          (list (omnidiff-file-path f)
                                                (omnidiff-file-target-path f)
                                                (omnidiff-file-status f)
                                                (omnidiff-file-old-oid f)
                                                (omnidiff-file-new-oid f)
                                                (and (omnidiff-file-picture f)
                                                     (omnidiff-picture-comparison
                                                      (omnidiff-file-picture f)))))
                                        (omnidiff-diff-files omnidiff--diff)))))
                    (when (buffer-live-p buf) (kill-buffer buf))))))
      (unwind-protect
          (progn
            (git "init" "-q" "-b" "main")
            (git "config" "user.name" "Test")
            (git "config" "user.email" "test@example.com")
            (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-red-2x2)
            (omnidiff-test--write-bin (expand-file-name "del.png" repo) omnidiff-test--png-red-2x2)
            (with-temp-file (expand-file-name "vec.svg" repo)
              (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\"><rect width=\"10\" height=\"10\" fill=\"red\"/></svg>\n"))
            (git "add" ".")
            (git "commit" "-q" "-m" "seed images")
            (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-blue-2x2)
            (delete-file (expand-file-name "del.png" repo))
            (omnidiff-test--write-bin (expand-file-name "add.png" repo) omnidiff-test--png-wide-4x2)
            (with-temp-file (expand-file-name "vec.svg" repo)
              (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\"><rect width=\"10\" height=\"10\" fill=\"blue\"/></svg>\n"))
            (git "add" "-A")
            (git "commit" "-q" "-m" "update images")
            (let ((ser (run-show 1))
                  (par (run-show 4)))
              (should (equal par ser))
              (should (= (length (cadr par)) 4))
              (let ((mod (assoc "mod.png" (cadr par)))
                    (add (assoc "add.png" (cadr par)))
                    (del (assoc "del.png" (cadr par)))
                    (vec (assoc "vec.svg" (cadr par))))
                (should (nth 3 mod))
                (should (nth 4 mod))
                (should (eq (nth 5 mod) 'pixels))
                (should-not (nth 3 add))
                (should (nth 4 add))
                (should (eq (nth 5 add) 'one-sided))
                (should (nth 3 del))
                (should-not (nth 4 del))
                (should (eq (nth 5 del) 'one-sided))
                (should (nth 3 vec))
                (should (nth 4 vec))
                (should-not (nth 5 vec)))))
        (delete-directory repo t)))))

(ert-deftest omnidiff-test/image-ergonomics-endpoints-diff-and-refresh ()
  "Verify zero-offset swipe endpoints, diff gain/fuzz, multi-region loupe, partial decode, and refresh state."
  (skip-unless (and (executable-find "git")
                    (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (let* ((repo (make-temp-file "omnidiff-img-ergo-" t))
         (default-directory repo)
         (buf (generate-new-buffer " *omnidiff-img-ergo*")))
    (cl-flet ((git-out (&rest args)
                (with-temp-buffer
                  (unless (zerop (apply #'call-process "git" nil t nil args))
                    (error "git %S failed" args))
                  (string-trim (buffer-string)))))
      (unwind-protect
          (progn
            (git-out "init" "-q" "-b" "main")
            (git-out "config" "user.name" "Test")
            (git-out "config" "user.email" "test@example.com")
            (omnidiff-test--write-bin (expand-file-name "a.png" repo) omnidiff-test--png-red-2x2)
            (git-out "add" ".")
            (git-out "commit" "-q" "-m" "v1")
            (let ((oid-red (git-out "rev-parse" "HEAD:a.png")))
              (omnidiff-test--write-bin (expand-file-name "a.png" repo) omnidiff-test--png-blue-2x2)
              (git-out "add" ".")
              (git-out "commit" "-q" "-m" "v2")
              (let* ((oid-blue (git-out "rev-parse" "HEAD:a.png"))
                     (diff
                      (make-omnidiff-diff
                       :files
                       (list
                        (make-omnidiff-file
                         :path "a.png" :target-path "a.png" :status 'modified
                         :old-oid oid-red :new-oid oid-blue
                         :picture (make-omnidiff-picture
                                   :before (make-omnidiff-picture-info
                                            :format "PNG" :width 100 :height 100 :color "RGBA8" :bytes 73)
                                   :after (make-omnidiff-picture-info
                                           :format "PNG" :width 100 :height 100 :color "RGBA8" :bytes 73)
                                   :comparison 'pixels :changed-pct 2.0
                                   :changed-pixels 200 :total-pixels 10000 :region-count 2
                                   :regions (list (make-omnidiff-picture-region :x 10 :y 10 :width 10 :height 10)
                                                  (make-omnidiff-picture-region :x 70 :y 70 :width 10 :height 10))))
                        (make-omnidiff-file
                         :path "partial.png" :target-path "partial.png" :status 'modified
                         :old-oid (make-string 40 ?f) :new-oid oid-blue
                         :picture (make-omnidiff-picture
                                   :before (make-omnidiff-picture-info
                                            :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                                   :after (make-omnidiff-picture-info
                                           :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                                   :comparison 'unknown :raw-comparison "backend note"))))))
                (with-current-buffer buf
                  (omnidiff-mode)
                  (setq omnidiff--repo-dir repo
                        omnidiff--diff diff
                        omnidiff--layout 'unified)
                  (cl-letf (((symbol-function 'omnidiff-image--display-graphic-p) (lambda () t))
                            ((symbol-function 'omnidiff-image--viewport-px) (lambda () '(1200 . 600))))
                    (omnidiff-render-buffer diff)
                    ;; 1. Partial-decode modified file explicitly warns about missing BEFORE side.
                    (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                      (should (string-search "BEFORE unavailable — showing AFTER only" txt))
                      (should (string-search "unverified" txt)))
                    ;; 2. Zero-Vernier swipe slice math at 50% on 100px width (handle = 3px -> half = 0.015).
                    (goto-char (point-min))
                    (omnidiff-image-adjust-reset)
                    (let* ((displays (omnidiff-test--collect-display-props))
                           (slices (cl-remove-if-not
                                    (lambda (d) (and (consp d) (consp (car d)) (eq (caar d) 'slice)))
                                    displays)))
                      (should (= (length slices) 2))
                      (should (< (abs (- (nth 3 (car (nth 0 slices))) 0.485)) 0.001))
                      (should (< (abs (- (nth 1 (car (nth 1 slices))) 0.515)) 0.001)))
                    ;; 3. Full [0.0, 1.0] endpoints emit no zero-width slice.
                    (let ((st (omnidiff-image--state-for (car (omnidiff-diff-files diff)))))
                      (setf (omnidiff-image--state-swipe st) 0.0)
                      (omnidiff-image--rerender-card (omnidiff-image--card-ov-at) t)
                      (should (string-search "3:swipe 0%"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                      (should-not (cl-some (lambda (d) (and (consp d) (consp (car d)) (eq (caar d) 'slice)))
                                           (omnidiff-test--collect-display-props)))
                      (setf (omnidiff-image--state-swipe st) 1.0)
                      (omnidiff-image--rerender-card (omnidiff-image--card-ov-at) t)
                      (should (string-search "3:swipe 100%"
                                             (buffer-substring-no-properties (point-min) (point-max)))))
                    ;; 4. Stepping regions `>' / `<' moves active region 1/2 -> 2/2 -> 1/2.
                    (omnidiff-image-toggle-regions)
                    (should (string-search "Region 1/2"
                                           (buffer-substring-no-properties (point-min) (point-max))))
                    (omnidiff-image-next-region)
                    (should (string-search "Region 2/2"
                                           (buffer-substring-no-properties (point-min) (point-max))))
                    (omnidiff-image-prev-region)
                    (should (string-search "Region 1/2"
                                           (buffer-substring-no-properties (point-min) (point-max))))
                    ;; 5. Interactive gain/fuzz stepping in diff mode + `=' reset.
                    (let ((omnidiff-image-diff-backend 'svg))
                      (omnidiff-image-mode-diff)
                      (should (string-search "delta ×4.0"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                      (omnidiff-image-adjust-increase)
                      (should (string-search "delta ×8.0"
                                             (buffer-substring-no-properties (point-min) (point-max))))
                      (omnidiff-image-adjust-reset)
                      (should (string-search "delta ×4.0"
                                             (buffer-substring-no-properties (point-min) (point-max)))))
                    ;; 6. Refresh (`g') preserves per-card state across full re-run.
                    (when (executable-find omnidiff-executable)
                      (setq omnidiff--command '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                            omnidiff--label "show HEAD")
                      (omnidiff-image-mode-onion)
                      (omnidiff-image-adjust-increase) ; -> 60%
                      (omnidiff-refresh)
                      (omnidiff-test--wait-request buf)
                      (let ((st (omnidiff-image--state-for (car (omnidiff-diff-files omnidiff--diff)))))
                        (should (eq (omnidiff-image--state-mode st) 'onion))
                        (should (= (round (* 100.0 (omnidiff-image--state-opacity st))) 60)))))))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-directory repo t)))))

;;; Semantic engine, universal theme, dual viewer, JSON overview, and entrypoint tests

(defun omnidiff-test--token-overlays (buf)
  "Return all semantic `'token' overlays belonging to `omnidiff-dual' in BUF."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (cl-remove-if-not
       (lambda (ov)
         (and (overlay-get ov 'omnidiff-dual-ov)
              (eq (overlay-get ov 'omnidiff-dual-kind) 'token)))
       (overlays-in (point-min) (point-max))))))

(defun omnidiff-test--make-zip (zip-path entries)
  "Create a ZIP archive at ZIP-PATH from ENTRIES `((MEMBER-NAME . FILE-PATH) ...)'."
  (let ((python (or (executable-find "python3")
                    (error "python3 not found")))
        (args (list "-c"
                    "import sys, zipfile\nwith zipfile.ZipFile(sys.argv[1], 'w') as zf:\n    a = sys.argv[2:]\n    for i in range(0, len(a), 2):\n        zf.write(a[i + 1], a[i])\n"
                    zip-path)))
    (dolist (entry entries)
      (setq args (nconc args (list (car entry) (cdr entry)))))
    (unless (zerop (apply #'call-process python nil nil nil args))
      (error "Failed to build test zip %s" zip-path))
    zip-path))

(ert-deftest omnidiff-test/engine-json-parsing-utf8-and-edge-cases ()
  "Verify JSON parsing, UTF-8 byte offsets, EOF ranges, and real sync/async engine runs."
  ;; 1. Unsorted hunks get sorted; summary, encoding, and large-residual parse cleanly.
  (let* ((json-unsorted
          (concat
           "{"
           "\"before\":{\"path\":\"a.py\",\"language\":\"Python\",\"encoding\":\"UTF-16LE\",\"hunks\":["
           "{\"operation\":\"delete\",\"range\":{\"start_row\":5,\"start_column\":0,\"end_row\":5,\"end_column\":4}},"
           "{\"operation\":\"update\",\"range\":{\"start_row\":2,\"start_column\":8,\"end_row\":2,\"end_column\":12},\"reference_line\":1},"
           "{\"operation\":\"move\",\"range\":{\"start_row\":2,\"start_column\":1,\"end_row\":2,\"end_column\":5},"
           "\"move_target\":{\"start_row\":9,\"start_column\":0,\"end_row\":10,\"end_column\":0}}"
           "]},"
           "\"after\":{\"path\":\"b.py\",\"language\":\"Python\",\"encoding\":\"UTF-8\",\"hunks\":[]},"
           "\"large_residual\":true,"
           "\"summary\":\"whitespace_only\""
           "}"))
         (parsed (omnidiff-engine-parse-json json-unsorted))
         (b-side (omnidiff-json-diff-before parsed))
         (hunks (omnidiff-json-side-hunks b-side)))
    (should (omnidiff-json-diff-p parsed))
    (should (omnidiff-json-diff-large-residual parsed))
    (should (eq (omnidiff-json-diff-summary parsed) 'whitespace-only))
    (should (equal (omnidiff-engine-summary-label 'whitespace-only) "whitespace only"))
    (should (equal (omnidiff-engine-summary-label 'comment-only) "comment only"))
    (should (equal (omnidiff-engine-summary-label 'refactor-moved-only) "moved only"))
    (should (equal (omnidiff-json-side-encoding b-side) "UTF-16LE"))
    (should (= (length hunks) 3))
    (should (equal (mapcar (lambda (h)
                             (let ((r (omnidiff-json-hunk-range h)))
                               (list (omnidiff-json-hunk-operation h)
                                     (omnidiff-json-range-start-row r)
                                     (omnidiff-json-range-start-col r))))
                           hunks)
                   '((move 2 1) (update 2 8) (delete 5 0))))
    (should (= (omnidiff-json-range-start-row (omnidiff-json-hunk-move-target (nth 0 hunks))) 9))
    (should (= (omnidiff-json-hunk-reference-line (nth 1 hunks)) 1)))
  ;; 2. Picture JSON parsing: pixels, identical, resized-scaled, resized, and animation frames.
  (cl-flet ((wrap-pic (cmp-json)
              (omnidiff-engine-parse-json
               (format
                (concat "{\"before\":{\"path\":\"a.png\",\"hunks\":[]},"
                        "\"after\":{\"path\":\"b.png\",\"hunks\":[]},"
                        "\"binary\":true,"
                        "\"content\":{\"kind\":\"picture\","
                        "\"before\":{\"format\":\"PNG\",\"width\":10,\"height\":10,\"color\":\"RGB8\",\"bytes\":100},"
                        "\"after\":{\"format\":\"PNG\",\"width\":20,\"height\":20,\"color\":\"RGBA8\",\"bytes\":200},"
                        "\"comparison\":%s}}")
                cmp-json))))
    (let ((p-pix (omnidiff-json-diff-picture
                  (wrap-pic "{\"kind\":\"pixels\",\"changed_pixels\":25,\"total_pixels\":100,\"regions\":[{\"x\":1,\"y\":2,\"width\":5,\"height\":5}]}"))))
      (should (omnidiff-picture-p p-pix))
      (should (eq (omnidiff-picture-comparison p-pix) 'pixels))
      (should (= (omnidiff-picture-changed-pixels p-pix) 25))
      (should (= (omnidiff-picture-total-pixels p-pix) 100))
      (should (< (abs (- (omnidiff-picture-changed-pct p-pix) 25.0)) 0.001))
      (should (= (omnidiff-picture-region-count p-pix) 1))
      (should (= (omnidiff-picture-region-y (car (omnidiff-picture-regions p-pix))) 2)))
    (let ((p-ident (omnidiff-json-diff-picture
                    (wrap-pic "{\"kind\":\"pixels\",\"changed_pixels\":0,\"total_pixels\":100,\"regions\":[]}"))))
      (should (eq (omnidiff-picture-comparison p-ident) 'identical))
      (should (= (omnidiff-picture-changed-pct p-ident) 0.0))
      (should (= (omnidiff-picture-changed-pixels p-ident) 0)))
    (let ((p-scaled (omnidiff-json-diff-picture
                     (wrap-pic "{\"kind\":\"resized\",\"scaled\":{\"changed_pixels\":10,\"total_pixels\":200,\"regions\":[{\"x\":0,\"y\":0,\"width\":2,\"height\":5}]}}"))))
      (should (eq (omnidiff-picture-comparison p-scaled) 'resized-scaled))
      (should (< (abs (- (omnidiff-picture-changed-pct p-scaled) 5.0)) 0.001))
      (should (= (omnidiff-picture-region-count p-scaled) 1)))
    (let ((p-resized (omnidiff-json-diff-picture (wrap-pic "{\"kind\":\"resized\"}"))))
      (should (eq (omnidiff-picture-comparison p-resized) 'resized)))
    (let ((p-frames (omnidiff-json-diff-picture
                     (wrap-pic "{\"kind\":\"frames\",\"total_pixels\":400,\"retimed\":true,\"faint\":true,\"steps\":[{\"kind\":\"changed\"},{\"kind\":\"inserted\",\"frames\":2},{\"kind\":\"deleted\",\"frames\":1}]}"))))
      (should (eq (omnidiff-picture-comparison p-frames) 'unknown))
      (should (= (omnidiff-picture-total-pixels p-frames) 400))
      (should (equal (omnidiff-picture-raw-comparison p-frames)
                     "animation: 1 changed, 2 added, 1 removed frames (retimed) (faint)"))))
  ;; 3. Recursive container JSON parsing (text, binary, picture, and nested container).
  (let* ((json-cont
          (concat
           "{\"before\":{\"path\":\"a.zip\",\"hunks\":[]},"
           "\"after\":{\"path\":\"b.zip\",\"hunks\":[]},"
           "\"binary\":true,"
           "\"content\":{\"kind\":\"container\",\"family\":\"archives\","
           "\"before\":{\"format\":\"ZIP\",\"bytes\":500,\"members\":5},"
           "\"after\":{\"format\":\"ZIP\",\"bytes\":620,\"members\":5},"
           "\"unchanged\":1,"
           "\"members\":["
           "{\"key\":\"src/app.py\",\"status\":\"changed\",\"detail\":{\"kind\":\"text\",\"before_lines\":10,\"after_lines\":12,\"removed\":1,\"added\":3,\"whitespace\":\"trailing\",\"words_kept\":true}},"
           "{\"key\":\"blob.bin\",\"status\":\"changed\",\"detail\":{\"kind\":\"binary\",\"before_bytes\":16,\"after_bytes\":32}},"
           "{\"key\":\"icon.png\",\"status\":\"changed\",\"detail\":{\"kind\":\"picture\",\"before\":{\"format\":\"PNG\",\"width\":2,\"height\":2,\"color\":\"RGB8\",\"bytes\":73},\"after\":{\"format\":\"PNG\",\"width\":2,\"height\":2,\"color\":\"RGB8\",\"bytes\":73},\"comparison\":{\"kind\":\"pixels\",\"changed_pixels\":4,\"total_pixels\":4,\"regions\":[{\"x\":0,\"y\":0,\"width\":2,\"height\":2}]}}},"
           "{\"key\":\"nested.pdf\",\"status\":\"changed\",\"detail\":{\"kind\":\"content\",\"content\":{\"kind\":\"container\",\"family\":\"documents\",\"before\":{\"format\":\"PDF\",\"bytes\":900,\"members\":2},\"after\":{\"format\":\"PDF\",\"bytes\":950,\"members\":2},\"unchanged\":1,\"members\":[{\"key\":\"page 2\",\"status\":\"changed\",\"detail\":{\"kind\":\"text\",\"before_lines\":5,\"after_lines\":6,\"removed\":1,\"added\":2,\"words_kept\":false}}]}}}"
           "]}}"))
         (parsed (omnidiff-engine-parse-json json-cont))
         (cont (omnidiff-json-diff-container parsed))
         (members (omnidiff-json-container-members cont)))
    (should (omnidiff-json-container-p cont))
    (should (eq (omnidiff-json-container-family cont) 'archives))
    (should (= (omnidiff-json-container-unchanged cont) 1))
    (should (equal (omnidiff-json-container-side-format (omnidiff-json-container-before cont)) "ZIP"))
    (should (= (length members) 4))
    (let ((m-txt (nth 0 members))
          (m-bin (nth 1 members))
          (m-pic (nth 2 members))
          (m-doc (nth 3 members)))
      (should (eq (omnidiff-json-container-member-detail-kind m-txt) 'text))
      (should (eq (plist-get (omnidiff-json-container-member-detail m-txt) :whitespace) 'trailing))
      (should (plist-get (omnidiff-json-container-member-detail m-txt) :words-kept))
      (should (eq (omnidiff-json-container-member-detail-kind m-bin) 'binary))
      (should (= (plist-get (omnidiff-json-container-member-detail m-bin) :after-bytes) 32))
      (should (eq (omnidiff-json-container-member-detail-kind m-pic) 'picture))
      (should (omnidiff-picture-p (omnidiff-json-container-member-detail m-pic)))
      (should (eq (omnidiff-json-container-member-detail-kind m-doc) 'content))
      (let ((nested (omnidiff-json-container-member-detail m-doc)))
        (should (omnidiff-json-container-p nested))
        (should (eq (omnidiff-json-container-family nested) 'documents))
        (should (equal (omnidiff-json-container-member-key
                        (car (omnidiff-json-container-members nested)))
                       "page 2")))))
  ;; 4. UTF-8 ASCII, CJK (3-byte), and emoji (4-byte) byte-to-char & paint-row-byte-len.
  (should (= (omnidiff-engine-line-byte-to-char "abcdef" -2) 0))
  (should (= (omnidiff-engine-line-byte-to-char "abcdef" 3) 3))
  (should (= (omnidiff-engine-line-byte-to-char "abcdef" 99) 6))
  (let ((cjk "a中b文"))
    (should (= (omnidiff-engine-line-byte-to-char cjk 1) 1))
    (should (= (omnidiff-engine-line-byte-to-char cjk 2) 1)) ; mid-char floors
    (should (= (omnidiff-engine-line-byte-to-char cjk 3) 1))
    (should (= (omnidiff-engine-line-byte-to-char cjk 4) 2))
    (should (= (omnidiff-engine-line-byte-to-char cjk 5) 3))
    (should (= (omnidiff-engine-line-byte-to-char cjk 8) 4)))
  (let ((emoji (concat "x" (string #x1f600) "y"))) ; x(1) + 😀(4) + y(1) = 6 bytes
    (should (= (omnidiff-engine-line-byte-to-char emoji 1) 1))
    (should (= (omnidiff-engine-line-byte-to-char emoji 2) 1))
    (should (= (omnidiff-engine-line-byte-to-char emoji 4) 1))
    (should (= (omnidiff-engine-line-byte-to-char emoji 5) 2))
    (should (= (omnidiff-engine-line-byte-to-char emoji 6) 3)))
  (should (= (omnidiff-engine-paint-row-byte-len "abc   \r") 3))
  (should (= (omnidiff-engine-paint-row-byte-len
              (concat "中" (string #x1f600) " \u00a0\u3000\r"))
             7))
  (should (= (omnidiff-engine-paint-row-byte-len "  \t\u00a0") 0))
  ;; 5. EOF-normalized (line_count, 0) & trailing-whitespace spans and buffer regions.
  (let* ((emoji (string #x1f600))
         (lines (vector "a中  " (concat "b" emoji "  ") "end"))
         (eof-rng (make-omnidiff-json-range :start-row 0 :start-col 0 :end-row 3 :end-col 0))
         (ws-rng (make-omnidiff-json-range :start-row 0 :start-col 4 :end-row 0 :end-col 6)))
    (should (equal (omnidiff-engine-range-line-spans eof-rng lines)
                   '((0 0 2) (1 0 2) (2 0 3))))
    (should (equal (omnidiff-engine-range-line-spans ws-rng lines)
                   '((0 2 4))))
    (with-temp-buffer
      (insert (aref lines 0) "\n" (aref lines 1) "\n" (aref lines 2) "\n")
      (let* ((starts (omnidiff-engine-buffer-line-offsets (current-buffer)))
             (eof-regs (omnidiff-engine-range-to-buffer-regions eof-rng starts (current-buffer)))
             (ws-regs (omnidiff-engine-range-to-buffer-regions ws-rng starts (current-buffer))))
        (should (equal (mapcar (lambda (r) (buffer-substring-no-properties (car r) (cdr r)))
                               eof-regs)
                       (list "a中" (concat "b" emoji) "end")))
        (should (equal (mapcar (lambda (r) (buffer-substring-no-properties (car r) (cdr r)))
                               ws-regs)
                       '("  "))))))
  ;; 6. Real sync & async engine runs on Python with CJK + moves + updates + /dev/null.
  (skip-unless (executable-find omnidiff-executable))
  (let* ((dir (make-temp-file "omnidiff-engine-py-" t))
         (fa (expand-file-name "before.py" dir))
         (fb (expand-file-name "after.py" dir)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'utf-8-unix))
            (with-temp-file fa
              (insert "def helper(x):\n    # 注释\n    return x + 1\n\n"
                      "def keep_a():\n    return 1\n\n"
                      "def keep_b():\n    return 2\n\n"
                      "def main():\n    msg = \"你好世界\"\n    return helper(10)\n"))
            (with-temp-file fb
              (insert "def keep_a():\n    return 1\n\n"
                      "def keep_b():\n    return 2\n\n"
                      "def main():\n    msg = \"你好宇宙\"\n    return helper(20)\n\n"
                      "def helper(x):\n    # 注释\n    return x + 1\n")))
          ;; Sync run: CJK updates + move with move_target.
          (let* ((sync-diff (omnidiff-engine-run-pair fa fb 'default))
                 (bh (omnidiff-json-side-hunks (omnidiff-json-diff-before sync-diff)))
                 (ah (omnidiff-json-side-hunks (omnidiff-json-diff-after sync-diff))))
            (should (omnidiff-json-diff-p sync-diff))
            (should (equal (omnidiff-json-side-language (omnidiff-json-diff-after sync-diff)) "Python"))
            (should (cl-some (lambda (h)
                               (and (eq (omnidiff-json-hunk-operation h) 'move)
                                    (omnidiff-json-range-p (omnidiff-json-hunk-move-target h))))
                             bh))
            (should (cl-some (lambda (h) (eq (omnidiff-json-hunk-operation h) 'update)) ah))
            ;; /dev/null new-file and deleted-file runs.
            (let ((new-d (omnidiff-engine-run-pair "/dev/null" fb 'default))
                  (del-d (omnidiff-engine-run-pair fa "/dev/null" 'default)))
              (should (eq (omnidiff-json-diff-summary new-d) 'new-file))
              (should-not (omnidiff-json-side-hunks (omnidiff-json-diff-before new-d)))
              (should (omnidiff-json-side-hunks (omnidiff-json-diff-after new-d)))
              (should (eq (omnidiff-json-diff-summary del-d) 'deleted-file))
              (should (omnidiff-json-side-hunks (omnidiff-json-diff-before del-d)))
              (should-not (omnidiff-json-side-hunks (omnidiff-json-diff-after del-d))))
            ;; Async run success callback matches sync hunk counts.
            (let ((async-res nil)
                  (async-err nil))
              (let ((proc (omnidiff-engine-run-pair-async
                           fa fb
                           (lambda (d) (setq async-res d))
                           :preset 'default
                           :error-callback (lambda (e) (setq async-err e))))
                    (deadline (+ (float-time) 10.0)))
                (while (and (null async-res) (null async-err) (< (float-time) deadline))
                  (accept-process-output proc 0.02))
                (should-not async-err)
                (should (omnidiff-json-diff-p async-res))
                (should (= (length (omnidiff-json-side-hunks (omnidiff-json-diff-before async-res)))
                           (length bh)))
                (should (= (length (omnidiff-json-side-hunks (omnidiff-json-diff-after async-res)))
                           (length ah)))))
            ;; Async run error callback fires on nonexistent input file.
            (let ((ok-called nil)
                  (err-msg nil)
                  (missing (expand-file-name "does-not-exist-xyz.py" dir)))
              (let ((proc (omnidiff-engine-run-pair-async
                           missing fb
                           (lambda (_d) (setq ok-called t))
                           :error-callback (lambda (msg) (setq err-msg msg))))
                    (deadline (+ (float-time) 10.0)))
                (while (and (null ok-called) (null err-msg) (< (float-time) deadline))
                  (accept-process-output proc 0.02))
                (should-not ok-called)
                (should (stringp err-msg))
                (should-not (string-empty-p err-msg))))))
      (delete-directory dir t))))

(ert-deftest omnidiff-test/theme-universal-and-semantic-tokens ()
  "Non-Modus theme resolution, display-guarded specs, foreground modes, overrides, and recalc."
  (let ((saved-bg-mode (frame-parameter nil 'background-mode))
        (saved-scheme omnidiff-color-scheme)
        (saved-fg-mode omnidiff-dual-overlay-foreground)
        (saved-custom omnidiff-custom-palette)
        (dark-display '((class color) (min-colors 88) (background dark)))
        (light-display '((class color) (min-colors 88) (background light)))
        (rich-display '((class color) (min-colors 88)))
        (token-faces '(omnidiff-token-insert
                       omnidiff-token-delete
                       omnidiff-token-update
                       omnidiff-token-move
                       omnidiff-token-cross-highlight
                       omnidiff-token-active))
        (token-bg-keys '((omnidiff-token-insert . :insert-bg)
                         (omnidiff-token-delete . :delete-bg)
                         (omnidiff-token-update . :update-bg)
                         (omnidiff-token-move . :move-bg)
                         (omnidiff-token-cross-highlight . :cross-bg)
                         (omnidiff-token-active . :active-bg)))
        (semantic-keys '(:insert-bg :delete-bg :update-bg :move-bg :cross-bg :active-bg
                         :overlay-fg :bg-main :bg-chrome :bg-card :border :fg-main :fg-dim
                         :accent :gutter-insert :gutter-delete :gutter-update :gutter-move))
        (dummy-theme 'omnidiff-test--dummy-theme))
    (cl-flet ((clause-attrs (spec display)
                (cdr (assoc display spec))))
      (unwind-protect
          (cl-letf (((symbol-function 'omnidiff-theme--active-modus-theme) (lambda () nil)))
            (setq omnidiff-color-scheme 'omni-semantic
                  omnidiff-custom-palette nil
                  omnidiff-dual-overlay-foreground 'preserve-syntax)
            ;; 1. No-theme Emacs: emits dark + light `((class color) (min-colors 88) ...)' clauses
            ;;    plus `(t ...)' fallback; zero `:extend t' on every token clause; valid semantic hexes.
            (let ((custom-enabled-themes nil)
                  dark-colors light-colors)
              (dolist (mode '(dark light))
                (set-frame-parameter nil 'background-mode mode)
                (let ((prev-rev omnidiff-theme--revision))
                  (should (omnidiff-apply-theme))
                  (should (> omnidiff-theme--revision prev-rev)))
                (let ((colors (omnidiff-theme-semantic-colors)))
                  (if (eq mode 'dark)
                      (setq dark-colors colors)
                    (setq light-colors colors))
                  (dolist (k semantic-keys)
                    (let ((hex (plist-get colors k)))
                      (should (stringp hex))
                      (should (omnidiff-theme--parse-hex hex))))
                  (should (eq (omnidiff-theme--dark-color-p (plist-get colors :bg-main))
                              (eq mode 'dark)))))
              (should-not (equal (plist-get dark-colors :bg-main)
                                 (plist-get light-colors :bg-main)))
              (should-not (equal (plist-get dark-colors :insert-bg)
                                 (plist-get light-colors :insert-bg)))
              (dolist (f token-faces)
                (let* ((spec (get f 'face-defface-spec))
                       (dark-attrs (clause-attrs spec dark-display))
                       (light-attrs (clause-attrs spec light-display))
                       (fb-attrs (clause-attrs spec t))
                       (bg-key (cdr (assq f token-bg-keys))))
                  (should (= (length spec) 3))
                  (should dark-attrs)
                  (should light-attrs)
                  (should fb-attrs)
                  ;; Dark and light clauses match semantic hexes and differ from each other.
                  (should (equal (plist-get dark-attrs :background)
                                 (plist-get dark-colors bg-key)))
                  (should (equal (plist-get light-attrs :background)
                                 (plist-get light-colors bg-key)))
                  (should-not (equal (plist-get dark-attrs :background)
                                     (plist-get light-attrs :background)))
                  ;; 8-color/mono `(t ...)' fallback uses non-hex attributes.
                  (should-not (omnidiff-theme--parse-hex (plist-get fb-attrs :background)))
                  (should-not (omnidiff-theme--parse-hex (plist-get fb-attrs :foreground)))
                  ;; Zero `:extend t' and no `:foreground' in any clause under `preserve-syntax'.
                  (should-not (eq (face-attribute f :extend nil) t))
                  (dolist (clause spec)
                    (let ((attrs (cdr clause)))
                      (should-not (eq (plist-get attrs :extend) t))
                      (should-not (plist-get attrs :foreground))))))
              ;; 2. `omnidiff-dual-overlay-foreground' switches between `uniform' and `preserve-syntax'.
              (let ((omnidiff-dual-overlay-foreground 'uniform))
                (omnidiff-apply-theme)
                (let ((dark-fg (plist-get dark-colors :overlay-fg))
                      (light-fg (plist-get light-colors :overlay-fg)))
                  (should (omnidiff-theme--parse-hex dark-fg))
                  (should (omnidiff-theme--parse-hex light-fg))
                  (dolist (f token-faces)
                    (let* ((spec (get f 'face-defface-spec))
                           (dark-attrs (clause-attrs spec dark-display))
                           (light-attrs (clause-attrs spec light-display)))
                      (should (equal (plist-get dark-attrs :foreground) dark-fg))
                      (should (equal (plist-get light-attrs :foreground) light-fg))
                      (dolist (clause spec)
                        (should-not (eq (plist-get (cdr clause) :extend) t)))))))
              (let ((omnidiff-dual-overlay-foreground 'preserve-syntax))
                (omnidiff-apply-theme)
                (dolist (f token-faces)
                  (dolist (clause (get f 'face-defface-spec))
                    (should-not (plist-get (cdr clause) :foreground))
                    (should-not (eq (plist-get (cdr clause) :extend) t)))))
              ;; 3. `omnidiff-custom-palette' applies valid overrides to both dark/light clauses
              ;;    and ignores invalid entries gracefully.
              (set-frame-parameter nil 'background-mode 'dark)
              (let ((baseline (omnidiff-theme-semantic-colors)))
                (let ((omnidiff-custom-palette '((insert-bg . "#112233")
                                                 (delete-bg . "blue")
                                                 (accent . "#abcdef"))))
                  (omnidiff-apply-theme)
                  (let ((ov-colors (omnidiff-theme-semantic-colors)))
                    (should (equal (plist-get ov-colors :insert-bg) "#112233"))
                    (should (equal (plist-get ov-colors :delete-bg) "#0000ff"))
                    (should (equal (plist-get ov-colors :accent) "#abcdef"))
                    (let ((ins-spec (get 'omnidiff-token-insert 'face-defface-spec))
                          (del-spec (get 'omnidiff-token-delete 'face-defface-spec))
                          (act-spec (get 'omnidiff-token-active 'face-defface-spec)))
                      (should (equal (plist-get (clause-attrs ins-spec dark-display) :background)
                                     "#112233"))
                      (should (equal (plist-get (clause-attrs ins-spec light-display) :background)
                                     "#112233"))
                      (should (equal (plist-get (clause-attrs del-spec dark-display) :background)
                                     "#0000ff"))
                      (should (equal (plist-get (plist-get (clause-attrs act-spec dark-display) :box)
                                                :color)
                                     "#abcdef")))))
                (let ((omnidiff-custom-palette '((insert-bg . "not-a-valid-color-xyz")
                                                 (delete-bg . nil)
                                                 (update-bg . 12345))))
                  (should (omnidiff-apply-theme))
                  (let ((fallback-colors (omnidiff-theme-semantic-colors)))
                    (should (equal (plist-get fallback-colors :insert-bg)
                                   (plist-get baseline :insert-bg)))
                    (should (equal (plist-get fallback-colors :delete-bg)
                                   (plist-get baseline :delete-bg)))
                    (should (equal (plist-get fallback-colors :update-bg)
                                   (plist-get baseline :update-bg)))
                    (should (equal (plist-get (clause-attrs
                                               (get 'omnidiff-token-insert 'face-defface-spec)
                                               dark-display)
                                              :background)
                                   (plist-get baseline :insert-bg)))))))
            ;; 4. Enabled custom theme: emits `((class color) (min-colors 88))' + `(t ...)' fallback
            ;;    and invokes `custom-theme-recalc-face' for all theme faces.
            (custom-declare-theme dummy-theme nil)
            (let ((custom-enabled-themes (list dummy-theme))
                  (recalced nil)
                  (orig-recalc (symbol-function 'custom-theme-recalc-face)))
              (unwind-protect
                  (progn
                    (cl-letf (((symbol-function 'custom-theme-recalc-face)
                               (lambda (face)
                                 (push face recalced)
                                 (funcall orig-recalc face))))
                      (should (omnidiff-apply-theme)))
                    (should (memq 'omnidiff-ui recalced))
                    (let ((settings (get dummy-theme 'theme-settings)))
                      (dolist (f token-faces)
                        (should (memq f recalced))
                        (let* ((entry (cl-find-if
                                       (lambda (e)
                                         (and (eq (car e) 'theme-face)
                                              (eq (cadr e) f)))
                                       settings))
                               (spec (nth 3 entry))
                               (rich-attrs (clause-attrs spec rich-display))
                               (fb-attrs (clause-attrs spec t)))
                          (should entry)
                          (should (= (length spec) 2))
                          (should (omnidiff-theme--parse-hex (plist-get rich-attrs :background)))
                          (should fb-attrs)
                          (should-not (omnidiff-theme--parse-hex (plist-get fb-attrs :background)))
                          (dolist (clause spec)
                            (should-not (eq (plist-get (cdr clause) :extend) t)))))))
                (disable-theme dummy-theme)
                (setq custom-known-themes (delq dummy-theme custom-known-themes))
                (setplist dummy-theme nil))))
        (set-frame-parameter nil 'background-mode saved-bg-mode)
        (setq omnidiff-color-scheme saved-scheme
              omnidiff-dual-overlay-foreground saved-fg-mode
              omnidiff-custom-palette saved-custom)
        (omnidiff-apply-theme)))))

(ert-deftest omnidiff-test/dual-viewer-navigation-folding-edit-and-cleanup ()
  "Dual viewer handles Python syntax+tokens, navigation, moves, folding, layout, live edit, reuse, and teardown."
  (skip-unless (executable-find omnidiff-executable))
  (let* ((dir (make-temp-file "omnidiff-dual-e2e-" t))
         (fa (expand-file-name "before.py" dir))
         (fb (expand-file-name "after.py" dir))
         (fa2 (expand-file-name "swap_before.py" dir))
         (fb2 (expand-file-name "swap_after.py" dir))
         (orig-width (frame-width (selected-frame)))
         (user-buf nil)
         (user-buf2 nil)
         (sess nil))
    (cl-flet ((wait-dual (s)
                (let ((deadline (+ (float-time) 10.0)))
                  (while (and (memq s omnidiff-dual--sessions)
                              (omnidiff-dual--session-active-proc s))
                    (when (> (float-time) deadline)
                      (ert-fail "Timed out waiting for dual async refresh"))
                    (accept-process-output nil 0.02)))))
      (unwind-protect
          (let ((omnidiff-dual-layout 'auto)
                (omnidiff-dual-min-width 100)
                (omnidiff-dual-fold-unchanged nil)
                (omnidiff-dual-context-lines 2)
                (omnidiff-dual-live-update 'on-save)
                (coding-system-for-write 'utf-8-unix))
            (set-frame-width (selected-frame) 140)
            (with-temp-file fa
              (insert "def helper(x):\n    # 注释 alpha\n    total = x + 1\n    return total\n\n"
                      "def keep_one():\n    return 1\n\n"
                      "def keep_two():\n    return 2\n\n"
                      "def long_section():\n"
                      "    a0 = 0\n    a1 = 1\n    a2 = 2\n    a3 = 3\n    a4 = 4\n"
                      "    a5 = 5\n    a6 = 6\n    a7 = 7\n    a8 = 8\n    a9 = 9\n"
                      "    msg = \"你好世界\"\n"
                      "    old_only = compute_old()\n"
                      "    return helper(10)\n"))
            (with-temp-file fb
              (insert "def keep_one():\n    return 1\n\n"
                      "def keep_two():\n    return 2\n\n"
                      "def long_section():\n"
                      "    a0 = 0\n    a1 = 1\n    a2 = 2\n    a3 = 3\n    a4 = 4\n"
                      "    a5 = 5\n    a6 = 6\n    a7 = 7\n    a8 = 8\n    a9 = 9\n"
                      "    msg = \"你好宇宙\"\n"
                      "    return helper(20)\n\n"
                      "def added_fn():\n    return 42\n\n"
                      "def helper(x):\n    # 注释 alpha\n    total = x + 1\n    return total\n"))
            (with-temp-file fa2 (insert "def swapped():\n    return 100\n"))
            (with-temp-file fb2 (insert "def swapped():\n    return 234\n"))
            ;; Visit AFTER as a real non-owned user buffer prior to opening Dual.
            (setq user-buf (find-file-noselect fb))
            (setq sess (omnidiff-dual-open :before-path fa
                                           :after-path fb
                                           :editable-after t))
            (should (omnidiff-dual--session-p sess))
            (let ((bb (omnidiff-dual--session-before-buf sess))
                  (ab (omnidiff-dual--session-after-buf sess)))
              (should (eq ab user-buf))
              (should (omnidiff-dual--session-before-owns-buf-p sess))
              (should-not (omnidiff-dual--session-after-owns-buf-p sess))
              ;; 1. Native python-mode syntax highlighting + semantic token overlays.
              (with-current-buffer bb
                (should (derived-mode-p 'python-mode))
                (save-excursion
                  (goto-char (point-min))
                  (should (eq (get-text-property (point) 'face) 'font-lock-keyword-face))))
              (with-current-buffer ab
                (should (derived-mode-p 'python-mode)))
              (dolist (buf (list bb ab))
                (let ((tok-ovs (omnidiff-test--token-overlays buf)))
                  (should (> (length tok-ovs) 0))
                  (with-current-buffer buf
                    (dolist (ov tok-ovs)
                      (let ((face (overlay-get ov 'face)))
                        (should (memq face '(omnidiff-token-insert omnidiff-token-delete
                                             omnidiff-token-update omnidiff-token-move)))
                        (should-not (eq (face-attribute face :extend nil) t))
                        (should-not (string-search "\n" (buffer-substring-no-properties
                                                         (overlay-start ov) (overlay-end ov)))))))))
              ;; CJK token update covers exact characters "世界" / "宇宙".
              (should (cl-some (lambda (ov)
                                 (with-current-buffer bb
                                   (equal (buffer-substring-no-properties
                                           (overlay-start ov) (overlay-end ov))
                                          "世界")))
                               (omnidiff-test--token-overlays bb)))
              (should (cl-some (lambda (ov)
                                 (with-current-buffer ab
                                   (equal (buffer-substring-no-properties
                                           (overlay-start ov) (overlay-end ov))
                                          "宇宙")))
                               (omnidiff-test--token-overlays ab)))
              ;; 2. `n'/`p' change stepping and wrap-around.
              (let* ((stops (omnidiff-dual--session-stops sess))
                     (total (length stops)))
                (should (>= total 4))
                (should (= (omnidiff-dual--session-stop-index sess) 1))
                (dotimes (i (1- total))
                  (omnidiff-dual-next-change)
                  (should (= (omnidiff-dual--session-stop-index sess) (+ i 2))))
                ;; Stepping past the last stop wraps cleanly back to 1.
                (omnidiff-dual-next-change)
                (should (= (omnidiff-dual--session-stop-index sess) 1))
                ;; Stepping backward from 1 wraps cleanly to `total'.
                (omnidiff-dual-prev-change)
                (should (= (omnidiff-dual--session-stop-index sess) total))
                (omnidiff-dual-prev-change)
                (should (= (omnidiff-dual--session-stop-index sess) (1- total))))
              ;; 3. RET exact `move_target' jump & round-trip return.
              (omnidiff-dual--focus-side sess 'before)
              (with-current-buffer bb (goto-char (point-min)))
              (let ((orig-pt (with-current-buffer bb (point))))
                (with-current-buffer bb
                  (let ((dest (omnidiff-dual-jump-counterpart)))
                    (should (eq (car dest) 'after))))
                (should (eq (omnidiff-dual--session-active-side sess) 'after))
                (with-current-buffer ab
                  (should (string-prefix-p "def helper(x):"
                                           (buffer-substring-no-properties
                                            (line-beginning-position) (line-end-position))))
                  ;; Second RET without moving returns to exact originating BEFORE position.
                  (let ((ret (omnidiff-dual-jump-counterpart)))
                    (should (eq (car ret) 'before))
                    (should (= (cdr ret) orig-pt))))
                (should (eq (omnidiff-dual--session-active-side sess) 'before))
                (should (= (with-current-buffer bb (point)) orig-pt)))
              ;; 4. `z' context folding with `@' breadcrumb + RET single-fold expand.
              (omnidiff-dual-toggle-fold)
              (should (omnidiff-dual--session-fold-unchanged sess))
              (let* ((folds (with-current-buffer ab
                              (cl-remove-if-not
                               (lambda (ov)
                                 (and (overlay-get ov 'omnidiff-dual-ov)
                                      (eq (overlay-get ov 'omnidiff-dual-kind) 'fold)))
                               (overlays-in (point-min) (point-max)))))
                     (target-fold (cl-find-if
                                   (lambda (ov)
                                     (let ((bs (overlay-get ov 'before-string)))
                                       (and (stringp bs)
                                            (string-search "@ def long_section():" bs))))
                                   folds)))
                (should target-fold)
                (let ((fold-pos (overlay-start target-fold))
                      (initial-count (length folds)))
                  (omnidiff-dual--focus-side sess 'after)
                  (with-current-buffer ab
                    (goto-char fold-pos)
                    (should (eq (omnidiff-dual-jump-counterpart) 'expanded-fold))
                    (should-not (overlay-buffer target-fold))
                    (let ((remaining (cl-remove-if-not
                                      (lambda (ov)
                                        (and (overlay-get ov 'omnidiff-dual-ov)
                                             (eq (overlay-get ov 'omnidiff-dual-kind) 'fold)))
                                      (overlays-in (point-min) (point-max)))))
                      (should (= (length remaining) (1- initial-count)))))))
              (omnidiff-dual-toggle-fold)
              (should-not (omnidiff-dual--session-fold-unchanged sess))
              ;; 5. `v' toggles directly between effective dual <-> single (no dead keypress);
              ;;    `C-u v' restores `auto'; `o' flips panes in single layout.
              (should (omnidiff-dual--dual-visible-p sess))
              (should (eq (omnidiff-dual-cycle-layout) 'single))
              (should-not (omnidiff-dual--dual-visible-p sess))
              (let ((before-flip (omnidiff-dual--session-active-side sess)))
                (let ((after-flip (omnidiff-dual-switch-pane)))
                  (should-not (eq after-flip before-flip))
                  (should (eq (omnidiff-dual--session-active-side sess) after-flip))
                  (should (eq (window-buffer (selected-window))
                              (omnidiff-dual--side-buf sess after-flip))))
                (should (eq (omnidiff-dual-switch-pane) before-flip)))
              (should (eq (omnidiff-dual-cycle-layout) 'dual))
              (should (omnidiff-dual--dual-visible-p sess))
              (should (eq (omnidiff-dual-cycle-layout t) 'auto))
              (should (omnidiff-dual--dual-visible-p sess))
              ;; 6. `e' live edit unlock -> save triggers async refresh -> lock back to review.
              (should (omnidiff-dual-toggle-edit))
              (should (omnidiff-dual--session-editing-p sess))
              (with-current-buffer ab
                (should-not buffer-read-only)
                (should (bound-and-true-p omnidiff-dual-edit-mode))
                (should-not (bound-and-true-p omnidiff-dual-mode))
                (goto-char (point-max))
                (insert "\nlive_added_var = 777\n")
                (let ((inhibit-message t))
                  (save-buffer)))
              (wait-dual sess)
              (should (cl-some (lambda (ov)
                                 (with-current-buffer ab
                                   (and (eq (overlay-get ov 'face) 'omnidiff-token-insert)
                                        (string-search "live_added_var"
                                                       (buffer-substring-no-properties
                                                        (overlay-start ov) (overlay-end ov))))))
                               (omnidiff-test--token-overlays ab)))
              ;; Lock back to read-only review mode.
              (should-not (omnidiff-dual-toggle-edit))
              (wait-dual sess)
              (should-not (omnidiff-dual--session-editing-p sess))
              (with-current-buffer ab
                (should buffer-read-only)
                (should (bound-and-true-p omnidiff-dual-mode))
                (should-not (bound-and-true-p omnidiff-dual-edit-mode)))
              ;; 7. `:reuse-session' swaps files in place, kills old owned BEFORE, preserves user-buf.
              (setq user-buf2 (find-file-noselect fb2))
              (let ((old-bb bb)
                    (old-id (omnidiff-dual--session-id sess)))
                (let ((reused (omnidiff-dual-open :before-path fa2
                                                  :after-path fb2
                                                  :editable-after t
                                                  :reuse-session sess)))
                  (should (eq reused sess))
                  (should (= (omnidiff-dual--session-id reused) old-id))
                  (should-not (buffer-live-p old-bb))
                  ;; Previous non-owned user buffer is preserved and restored cleanly.
                  (should (buffer-live-p user-buf))
                  (with-current-buffer user-buf
                    (should-not buffer-read-only)
                    (should-not (bound-and-true-p omnidiff-dual-mode))
                    (should-not omnidiff-dual--session)
                    (should-not (omnidiff-test--token-overlays user-buf)))
                  ;; Swapped pair has its own token overlays.
                  (should (eq (omnidiff-dual--session-after-buf sess) user-buf2))
                  (should (cl-some (lambda (ov)
                                     (with-current-buffer user-buf2
                                       (equal (buffer-substring-no-properties
                                               (overlay-start ov) (overlay-end ov))
                                              "234")))
                                   (omnidiff-test--token-overlays user-buf2)))))
              ;; 8. `q' teardown kills owned BEFORE, preserves non-owned user-buf2, and cleans up.
              (let ((cur-bb (omnidiff-dual--session-before-buf sess))
                    (tmpdir (omnidiff-dual--session-tmpdir sess)))
                (with-current-buffer user-buf2
                  (omnidiff-dual-quit))
                (should-not (memq sess omnidiff-dual--sessions))
                (should-not (buffer-live-p cur-bb))
                (should-not (and tmpdir (file-directory-p tmpdir)))
                (should-not (omnidiff-dual--session-live-timer sess))
                (should-not (omnidiff-dual--session-active-proc sess))
                (should (buffer-live-p user-buf2))
                (with-current-buffer user-buf2
                  (should-not buffer-read-only)
                  (should-not (bound-and-true-p omnidiff-dual-mode))
                  (should-not omnidiff-dual--session)
                  (should-not (omnidiff-test--token-overlays user-buf2)))
                (setq sess nil))))
        (when (and sess (memq sess omnidiff-dual--sessions))
          (omnidiff-dual--teardown sess nil t))
        (when (buffer-live-p user-buf)
          (with-current-buffer user-buf (set-buffer-modified-p nil))
          (kill-buffer user-buf))
        (when (buffer-live-p user-buf2)
          (with-current-buffer user-buf2 (set-buffer-modified-p nil))
          (kill-buffer user-buf2))
        (set-frame-width (selected-frame) orig-width)
        (delete-directory dir t)))))

(ert-deftest omnidiff-test/overview-json-syntax-token-style-and-containers ()
  "JSON Overview renders syntax faces, 3 styles x 3 layouts, trailing-comma safety, badges, auto-fold, and containers."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "python3")))
  (let* ((dir (make-temp-file "omnidiff-ov-json-" t))
         (demo-a (expand-file-name "demo_a.py" dir))
         (demo-b (expand-file-name "demo_b.py" dir))
         (ws-a (expand-file-name "ws_a.py" dir))
         (ws-b (expand-file-name "ws_b.py" dir))
         (z-src-1 (expand-file-name "z1.py" dir))
         (z-src-2 (expand-file-name "z2.py" dir))
         (z-keep (expand-file-name "keep.txt" dir))
         (z-gone (expand-file-name "gone.txt" dir))
         (z-add (expand-file-name "added.txt" dir))
         (zip-a (expand-file-name "bundle_a.zip" dir))
         (zip-b (expand-file-name "bundle_b.zip" dir))
         (buf (generate-new-buffer " *omnidiff-ov-json*"))
         (orig-width (frame-width (selected-frame))))
    (cl-flet ((faces-at (pos)
                (let ((f (get-text-property pos 'face)))
                  (cond ((null f) nil)
                        ((symbolp f) (list f))
                        ((listp f) (cl-remove-if-not #'symbolp f))
                        (t nil))))
              (collect-faces-in (beg end)
                (let ((pos beg)
                      (seen nil))
                  (while (< pos end)
                    (let ((next (next-single-property-change pos 'face nil end)))
                      (dolist (f (let ((fp (get-text-property pos 'face)))
                                   (cond ((null fp) nil)
                                         ((symbolp fp) (list fp))
                                         ((listp fp) (cl-remove-if-not #'symbolp fp))
                                         (t nil))))
                        (cl-pushnew f seen))
                      (setq pos next)))
                  seen)))
      (unwind-protect
          (progn
            (set-frame-width (selected-frame) 140)
            ;; Build real Python, whitespace-only, and ZIP fixtures.
            (with-temp-file demo-a
              (insert "def compute(x):\n"
                      "    items = [\n"
                      "        'alpha',\n"
                      "        'beta'\n"
                      "    ]\n"
                      "    return x + 1\n"))
            (with-temp-file demo-b
              (insert "def compute(x):\n"
                      "    items = [\n"
                      "        'alpha',\n"
                      "        'beta',\n"
                      "        'gamma',\n"
                      "    ]\n"
                      "    return x + 2\n"))
            (with-temp-file ws-a (insert "val = 1 + 2\n"))
            (with-temp-file ws-b (insert "val = 1   +   2\n"))
            (with-temp-file z-src-1 (insert "x = 1\n"))
            (with-temp-file z-src-2 (insert "x = 2\n"))
            (with-temp-file z-keep (insert "unchanged\n"))
            (with-temp-file z-gone (insert "bye\n"))
            (with-temp-file z-add (insert "hello\n"))
            (omnidiff-test--make-zip zip-a (list (cons "mod.py" z-src-1)
                                                 (cons "keep.txt" z-keep)
                                                 (cons "gone.txt" z-gone)))
            (omnidiff-test--make-zip zip-b (list (cons "mod.py" z-src-2)
                                                 (cons "keep.txt" z-keep)
                                                 (cons "added.txt" z-add)))
            (let* ((f-demo (omnidiff--file-from-json
                            "demo.py" demo-a demo-b "demo.py"
                            (omnidiff-engine-run-pair demo-a demo-b 'default)))
                   (f-ws (omnidiff--file-from-json
                          "ws.py" ws-a ws-b "ws.py"
                          (omnidiff-engine-run-pair ws-a ws-b 'default)))
                   (f-zip (omnidiff--file-from-json
                           "bundle.zip" zip-a zip-b "bundle.zip"
                           (omnidiff-engine-run-pair zip-a zip-b 'default)
                           nil nil nil nil zip-a zip-b))
                   (diff (make-omnidiff-diff :files (list f-demo f-ws f-zip))))
              (should (eq (omnidiff-file-summary f-ws) 'whitespace-only))
              (should (omnidiff-json-container-p (omnidiff-file-container f-zip)))
              (with-current-buffer buf
                (omnidiff-mode)
                (setq omnidiff--diff diff
                      omnidiff--repo-dir dir)
                ;; 1. Verify all 3 layouts x all 3 highlight styles.
                (dolist (layout '(unified side-by-side stacked))
                  (dolist (style '(token subtle-line traditional))
                    (setq omnidiff--layout layout
                          omnidiff--highlight-style style)
                    (let ((omnidiff-overview-auto-fold-trivial nil))
                      (omnidiff-render-buffer diff))
                    (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                      (should (string-search "demo.py" txt))
                      (should (string-search "ws.py" txt))
                      (should (string-search "bundle.zip" txt))
                      (should (string-search "'gamma'" txt))
                      (when (memq layout '(side-by-side stacked))
                        (should (string-search " BEFORE " txt))
                        (should (string-search " AFTER " txt))))
                    ;; Locate demo.py body region.
                    (goto-char (point-min))
                    (let* ((demo-ov (get-text-property (point-min) 'omnidiff-body-ov))
                           (body-faces (collect-faces-in (overlay-start demo-ov) (overlay-end demo-ov))))
                      (pcase style
                        ('token
                         ;; Native font-lock syntax faces + semantic token faces, ZERO :extend t diff wash!
                         (should (cl-some (lambda (f) (string-prefix-p "font-lock-" (symbol-name f)))
                                          body-faces))
                         (should (memq 'omnidiff-token-update body-faces))
                         (should (memq 'omnidiff-token-insert body-faces))
                         (dolist (wash '(omnidiff-added omnidiff-removed omnidiff-changed omnidiff-moved))
                           (should-not (memq wash body-faces))))
                        ('subtle-line
                         (should (cl-some (lambda (f) (string-prefix-p "font-lock-" (symbol-name f)))
                                          body-faces))
                         (should (memq 'omnidiff-token-update body-faces))
                         (should (memq 'omnidiff-changed body-faces)))
                        ('traditional
                         (should (memq 'omnidiff-changed body-faces))
                         (should (memq 'omnidiff-changed-refine body-faces)))))
                    ;; 2. In unified layout, single-sided trailing-comma edit does NOT paint unchanged
                    ;; left `'beta'` line red across any highlight style.
                    (when (eq layout 'unified)
                      (goto-char (point-min))
                      (let ((found-left-beta nil))
                        (while (and (not found-left-beta) (search-forward "'beta'" nil t))
                          (let ((bol (line-beginning-position)))
                            (when (eq (get-text-property bol 'omnidiff-side) 'before)
                              (setq found-left-beta t)
                              (let ((line-faces (collect-faces-in bol (line-end-position))))
                                (dolist (red '(omnidiff-removed omnidiff-removed-refine
                                               omnidiff-token-delete omnidiff-lineno-removed))
                                  (should-not (memq red line-faces)))))))
                        (should found-left-beta)))))
                ;; 3. Summary badge face & `omnidiff-overview-auto-fold-trivial'.
                (setq omnidiff--layout 'unified
                      omnidiff--highlight-style 'token)
                (let ((omnidiff-overview-auto-fold-trivial 'whitespace-and-comments))
                  (omnidiff-render-buffer diff)
                  (goto-char (point-min))
                  (let ((demo-ov (get-text-property (point-min) 'omnidiff-body-ov)))
                    (should-not (overlay-get demo-ov 'invisible)))
                  (search-forward "whitespace only")
                  (should (memq 'omnidiff-summary-badge (faces-at (match-beginning 0))))
                  (let ((ws-ov (get-text-property (line-beginning-position) 'omnidiff-body-ov)))
                    (should (eq (overlay-get ws-ov 'invisible) 'omnidiff-fold))))
                (let ((omnidiff-overview-auto-fold-trivial nil))
                  (omnidiff-render-buffer diff)
                  (goto-char (point-min))
                  (search-forward "ws.py")
                  (let ((ws-ov (get-text-property (line-beginning-position) 'omnidiff-body-ov)))
                    (should-not (overlay-get ws-ov 'invisible))))
                ;; 4. Structured container card & member text properties.
                (goto-char (point-min))
                (search-forward "bundle.zip")
                (let ((card-text (buffer-substring-no-properties (line-beginning-position) (point-max))))
                  (should (string-search "Archive" card-text))
                  (should (string-search "1 changed" card-text))
                  (should (string-search "1 added" card-text))
                  (should (string-search "1 removed" card-text))
                  (should (string-search "1 unchanged" card-text)))
                (search-forward "mod.py")
                (let ((cfile (get-text-property (match-beginning 0) 'omnidiff-container-file))
                      (cmem (get-text-property (match-beginning 0) 'omnidiff-container-member)))
                  (should (eq cfile f-zip))
                  (should (omnidiff-json-container-member-p cmem))
                  (should (equal (omnidiff-json-container-member-key cmem) "mod.py"))
                  (should (eq (omnidiff-json-container-member-status cmem) 'changed))))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (set-frame-width (selected-frame) orig-width)
        (delete-directory dir t)))))

(ert-deftest omnidiff-test/entrypoints-files-buffers-head-git-and-containers ()
  "Verify omnidiff-files (py/png/zip+drilldown), omnidiff-buffers, buffer-vs-head, single/multi git diff, and TUI."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")
                    (executable-find "python3")))
  (let* ((dir (make-temp-file "omnidiff-entry-" t))
         (repo1 (make-temp-file "omnidiff-repo1-" t))
         (repo-multi (make-temp-file "omnidiff-repom-" t))
         (orig-width (frame-width (selected-frame)))
         (cleanup-bufs nil)
         (cleanup-sessions nil))
    (cl-flet ((git-in (repo &rest args)
                (let ((default-directory repo))
                  (unless (zerop (apply #'call-process "git" nil nil nil args))
                    (error "git %S failed in %s" args repo))))
              (close-session (s)
                (when (and s (memq s omnidiff-dual--sessions))
                  (omnidiff-dual--teardown s nil t))))
      (unwind-protect
          (progn
            (set-frame-width (selected-frame) 140)
            ;; 1. `omnidiff-files' on Python pair -> Dual viewer with token overlays.
            (let ((py-a (expand-file-name "left.py" dir))
                  (py-b (expand-file-name "right.py" dir)))
              (with-temp-file py-a (insert "def greet():\n    return \"old\"\n"))
              (with-temp-file py-b (insert "def greet():\n    return \"new\"\n"))
              (let ((s (omnidiff-files py-a py-b)))
                (push s cleanup-sessions)
                (should (omnidiff-dual--session-p s))
                (should (> (length (omnidiff-test--token-overlays
                                    (omnidiff-dual--session-after-buf s)))
                           0))
                (with-current-buffer (omnidiff-dual--session-after-buf s)
                  (omnidiff-dual-quit))
                (should-not (memq s omnidiff-dual--sessions))
                ;; Clean up the visited `right.py' buffer if `omnidiff-files' opened it.
                (when-let* ((vb (find-buffer-visiting py-b)))
                  (kill-buffer vb))))
            ;; 2. `omnidiff-files' on PNG pair -> visual image viewer loading BOTH sides without Git/OIDs.
            (let ((png-a (expand-file-name "before.png" dir))
                  (png-b (expand-file-name "after.png" dir)))
              (omnidiff-test--write-bin png-a omnidiff-test--png-red-2x2)
              (omnidiff-test--write-bin png-b omnidiff-test--png-blue-2x2)
              (let ((img-buf
                     (if (and (image-type-available-p 'png)
                              (image-type-available-p 'svg))
                         (cl-letf (((symbol-function 'omnidiff-image--display-graphic-p) (lambda () t))
                                   ((symbol-function 'omnidiff-image--viewport-px) (lambda () '(1200 . 600))))
                           (omnidiff-files png-a png-b))
                       (omnidiff-files png-a png-b))))
                (push img-buf cleanup-bufs)
                (should (buffer-live-p img-buf))
                (with-current-buffer img-buf
                  (should (derived-mode-p 'omnidiff-mode))
                  (let* ((f (car (omnidiff-diff-files omnidiff--diff)))
                         (pic (omnidiff-file-picture f)))
                    (should (omnidiff-picture-p pic))
                    (should-not (omnidiff-file-old-oid f))
                    (should-not (omnidiff-file-new-oid f))
                    (should (equal (omnidiff-image--fetch-side-bytes f pic 'before dir t)
                                   omnidiff-test--png-red-2x2))
                    (should (equal (omnidiff-image--fetch-side-bytes f pic 'after dir t)
                                   omnidiff-test--png-blue-2x2))
                    (when (and (image-type-available-p 'png)
                               (image-type-available-p 'svg))
                      (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                        (should (string-search "3:swipe" txt))
                        (should-not (string-search "BEFORE unavailable" txt))))))
                (kill-buffer img-buf)))
            ;; 3. `omnidiff-files' on ZIP pair -> container Overview + RET drilldown to Dual (.py) & image (.png).
            (let ((app1 (expand-file-name "app1.py" dir))
                  (app2 (expand-file-name "app2.py" dir))
                  (logo1 (expand-file-name "logo1.png" dir))
                  (logo2 (expand-file-name "logo2.png" dir))
                  (keep (expand-file-name "keep.txt" dir))
                  (z1 (expand-file-name "pkg_v1.zip" dir))
                  (z2 (expand-file-name "pkg_v2.zip" dir)))
              (with-temp-file app1 (insert "def run():\n    return 1\n"))
              (with-temp-file app2 (insert "def run():\n    return 2\n"))
              (omnidiff-test--write-bin logo1 omnidiff-test--png-red-2x2)
              (omnidiff-test--write-bin logo2 omnidiff-test--png-blue-2x2)
              (with-temp-file keep (insert "same\n"))
              (omnidiff-test--make-zip z1 (list (cons "app.py" app1)
                                                (cons "logo.png" logo1)
                                                (cons "keep.txt" keep)))
              (omnidiff-test--make-zip z2 (list (cons "app.py" app2)
                                                (cons "logo.png" logo2)
                                                (cons "keep.txt" keep)))
              (let ((zip-buf (omnidiff-files z1 z2)))
                (push zip-buf cleanup-bufs)
                (should (buffer-live-p zip-buf))
                (with-current-buffer zip-buf
                  (should (derived-mode-p 'omnidiff-mode))
                  ;; Drill down `.py' member -> Dual viewer.
                  (goto-char (point-min))
                  (search-forward "app.py")
                  (goto-char (match-beginning 0))
                  (should (omnidiff-jump-to-source))
                  (let ((py-sess (car omnidiff-dual--sessions)))
                    (push py-sess cleanup-sessions)
                    (should (omnidiff-dual--session-p py-sess))
                    (should (string-search "app.py" (omnidiff-dual--session-display-path py-sess)))
                    (should (> (length (omnidiff-test--token-overlays
                                        (omnidiff-dual--session-after-buf py-sess)))
                               0))
                    (close-session py-sess))
                  ;; Drill down `.png' member -> standalone visual image viewer + tmpdir cleanup on kill.
                  (goto-char (point-min))
                  (search-forward "logo.png")
                  (goto-char (match-beginning 0))
                  (should (omnidiff-jump-to-source))
                  (let ((png-buf (window-buffer (selected-window))))
                    (push png-buf cleanup-bufs)
                    (should (buffer-live-p png-buf))
                    (should-not (eq png-buf zip-buf))
                    (should (string-search "logo.png" (buffer-name png-buf)))
                    (let ((owned-dirs nil))
                      (with-current-buffer png-buf
                        (should (derived-mode-p 'omnidiff-mode))
                        (let* ((f (car (omnidiff-diff-files omnidiff--diff)))
                               (pic (omnidiff-file-picture f)))
                          (should (omnidiff-picture-p pic))
                          (should (equal (omnidiff-image--fetch-side-bytes f pic 'before dir t)
                                         omnidiff-test--png-red-2x2))
                          (should (equal (omnidiff-image--fetch-side-bytes f pic 'after dir t)
                                         omnidiff-test--png-blue-2x2)))
                        (setq owned-dirs (copy-sequence omnidiff--owned-tmpdirs))
                        (should owned-dirs)
                        (should (cl-every #'file-directory-p owned-dirs)))
                      (kill-buffer png-buf)
                      (should-not (cl-some #'file-directory-p owned-dirs)))))
                (kill-buffer zip-buf)))
            ;; 4. `omnidiff-buffers' on two unsaved buffers.
            (let ((ub1 (generate-new-buffer "unsaved-a.py"))
                  (ub2 (generate-new-buffer "unsaved-b.py")))
              (push ub1 cleanup-bufs)
              (push ub2 cleanup-bufs)
              (with-current-buffer ub1 (insert "x = 10\n"))
              (with-current-buffer ub2 (insert "x = 20\n"))
              (let ((bsess (omnidiff-buffers ub1 ub2)))
                (push bsess cleanup-sessions)
                (should (omnidiff-dual--session-p bsess))
                (should (eq (omnidiff-dual--session-before-buf bsess) ub1))
                (should (eq (omnidiff-dual--session-after-buf bsess) ub2))
                (should (> (length (omnidiff-test--token-overlays ub2)) 0))
                (with-current-buffer ub2 (omnidiff-dual-quit))
                (should (buffer-live-p ub1))
                (should (buffer-live-p ub2))
                (should-not (omnidiff-test--token-overlays ub1))
                (should-not (omnidiff-test--token-overlays ub2)))
              (kill-buffer ub1)
              (kill-buffer ub2))
            ;; 5. `omnidiff-buffer-vs-head' in a real Git repo (modified live buffer + untracked file)
            ;;    and `omnidiff-diff' on a 1-file Python repo (`auto' -> Dual -> `O', `overview' -> `d' 0ms).
            (git-in repo1 "init" "-q" "-b" "main")
            (git-in repo1 "config" "user.name" "Test")
            (git-in repo1 "config" "user.email" "test@example.com")
            (let ((tracked (expand-file-name "single.py" repo1))
                  (untracked (expand-file-name "brand_new.py" repo1)))
              (with-temp-file tracked (insert "def calc():\n    return 1\n"))
              (git-in repo1 "add" "single.py")
              (git-in repo1 "commit" "-q" "-m" "initial")
              ;; 5a. Modified unsaved buffer vs HEAD.
              (let ((live-buf (find-file-noselect tracked)))
                (push live-buf cleanup-bufs)
                (with-current-buffer live-buf
                  (goto-char (point-min))
                  (search-forward "1")
                  (replace-match "99")
                  (should (buffer-modified-p))
                  (let ((hsess (omnidiff-buffer-vs-head)))
                    (push hsess cleanup-sessions)
                    (should (omnidiff-dual--session-p hsess))
                    (should (eq (omnidiff-dual--session-after-buf hsess) live-buf))
                    (should (string-search "HEAD:single.py"
                                           (omnidiff-dual--session-before-label hsess)))
                    (should (cl-some (lambda (ov)
                                       (with-current-buffer live-buf
                                         (equal (buffer-substring-no-properties
                                                 (overlay-start ov) (overlay-end ov))
                                                "99")))
                                     (omnidiff-test--token-overlays live-buf)))
                    (with-current-buffer live-buf (omnidiff-dual-quit)))
                  (should (buffer-live-p live-buf))
                  (should (buffer-modified-p))
                  (set-buffer-modified-p nil))
                (kill-buffer live-buf))
              ;; 5b. Untracked file vs HEAD -> `/dev/null' before side and `new-file' summary.
              (with-temp-file untracked (insert "def fresh():\n    return 7\n"))
              (let ((untr-buf (find-file-noselect untracked)))
                (push untr-buf cleanup-bufs)
                (with-current-buffer untr-buf
                  (let ((usess (omnidiff-buffer-vs-head)))
                    (push usess cleanup-sessions)
                    (should (omnidiff-dual--session-p usess))
                    (should (equal (omnidiff-dual--session-before-path usess) "/dev/null"))
                    (should (equal (omnidiff-dual--session-before-label usess) "/dev/null (new file)"))
                    (should (= (buffer-size (omnidiff-dual--session-before-buf usess)) 0))
                    (should (eq (omnidiff-dual--session-after-buf usess) untr-buf))
                    (let ((ins-ovs (omnidiff-test--token-overlays untr-buf)))
                      (should (> (length ins-ovs) 0))
                      (should (cl-every (lambda (ov) (eq (overlay-get ov 'face) 'omnidiff-token-insert))
                                        ins-ovs)))
                    (with-current-buffer untr-buf (omnidiff-dual-quit))))
                (kill-buffer untr-buf))
              (delete-file untracked)
              ;; 5c. `omnidiff-diff' on 1-file Python repo: `auto' opens Dual -> `O' returns to Overview.
              (with-temp-file tracked (insert "def calc():\n    return 42\n"))
              (let ((default-directory repo1)
                    (ov-buf (get-buffer-create "*git diff omnidiff*")))
                (push ov-buf cleanup-bufs)
                (let ((omnidiff-single-file-entry 'auto))
                  (omnidiff-diff)
                  (omnidiff-test--wait-request ov-buf)
                  (let ((auto-sess (car omnidiff-dual--sessions)))
                    (push auto-sess cleanup-sessions)
                    (should (omnidiff-dual--session-p auto-sess))
                    (should (equal (omnidiff-dual--session-display-path auto-sess) "single.py"))
                    ;; `O' tears down Dual and opens Overview buffer.
                    (with-current-buffer (omnidiff-dual--session-after-buf auto-sess)
                      (omnidiff-dual-open-overview))
                    (should-not (memq auto-sess omnidiff-dual--sessions))
                    (should (eq (window-buffer (selected-window)) ov-buf))))
                (when-let* ((vb (find-buffer-visiting tracked)))
                  (kill-buffer vb))
                ;; 5d. `omnidiff-single-file-entry 'overview' stays in Overview -> `d' opens Dual in 0ms (0 subprocesses).
                (let ((omnidiff-single-file-entry 'overview))
                  (omnidiff-diff)
                  (omnidiff-test--wait-request ov-buf)
                  (should-not omnidiff-dual--sessions)
                  (with-current-buffer ov-buf
                    (goto-char (point-min))
                    (cl-letf (((symbol-function 'omnidiff-engine-run-pair)
                               (lambda (&rest _)
                                 (error "Expected 0ms cached Dual handoff without running omnidiff"))))
                      (let ((cached-sess (omnidiff-open-dual-at-point)))
                        (push cached-sess cleanup-sessions)
                        (should (omnidiff-dual--session-p cached-sess))
                        (should (> (length (omnidiff-test--token-overlays
                                            (omnidiff-dual--session-after-buf cached-sess)))
                                   0))
                        (close-session cached-sess)))))
                (when-let* ((vb (find-buffer-visiting tracked)))
                  (kill-buffer vb))
                (kill-buffer ov-buf)))
            ;; 6. Multi-file Git repo: Overview (`H`, `F`, `T`), Dual `]`/`[` cycling, TUI tmpdir cleanup.
            (git-in repo-multi "init" "-q" "-b" "main")
            (git-in repo-multi "config" "user.name" "Test")
            (git-in repo-multi "config" "user.email" "test@example.com")
            (with-temp-file (expand-file-name "alpha.py" repo-multi) (insert "def alpha():\n    return 1\n"))
            (with-temp-file (expand-file-name "beta.py" repo-multi) (insert "def beta():\n    return 10\n"))
            (with-temp-file (expand-file-name "ws_only.py" repo-multi) (insert "x = 1 + 2\n"))
            (git-in repo-multi "add" ".")
            (git-in repo-multi "commit" "-q" "-m" "base")
            (with-temp-file (expand-file-name "alpha.py" repo-multi) (insert "def alpha():\n    return 2\n"))
            (with-temp-file (expand-file-name "beta.py" repo-multi) (insert "def beta():\n    return 20\n"))
            (with-temp-file (expand-file-name "ws_only.py" repo-multi) (insert "x = 1   +   2\n"))
            (let ((default-directory repo-multi)
                  (ov-buf (get-buffer-create "*git diff omnidiff*")))
              (push ov-buf cleanup-bufs)
              (let ((omnidiff-single-file-entry 'auto)
                    (omnidiff-overview-auto-fold-trivial nil))
                (omnidiff-diff)
                (omnidiff-test--wait-request ov-buf)
                ;; Multi-file stays in Overview even under `'auto'.
                (should-not omnidiff-dual--sessions)
                (with-current-buffer ov-buf
                  (should (= omnidiff--file-count 3))
                  ;; 6a. `H' cycles highlight style in memory (`token' -> `subtle-line' -> `traditional' -> `token').
                  (should (eq (omnidiff-render-effective-highlight-style) 'token))
                  (omnidiff-cycle-highlight-style)
                  (should (eq omnidiff--highlight-style 'subtle-line))
                  (omnidiff-cycle-highlight-style)
                  (should (eq omnidiff--highlight-style 'traditional))
                  (omnidiff-cycle-highlight-style)
                  (should (eq omnidiff--highlight-style 'token))
                  (should-not omnidiff--active-request)
                  ;; 6b. `F' folds/unfolds trivial `ws_only.py' card while keeping `alpha.py' open.
                  (omnidiff-toggle-fold-trivial)
                  (goto-char (point-min))
                  (search-forward "alpha.py")
                  (should-not (overlay-get (get-text-property (line-beginning-position) 'omnidiff-body-ov)
                                           'invisible))
                  (search-forward "ws_only.py")
                  (should (eq (overlay-get (get-text-property (line-beginning-position) 'omnidiff-body-ov)
                                           'invisible)
                              'omnidiff-fold))
                  (omnidiff-toggle-fold-trivial)
                  (goto-char (point-min))
                  (search-forward "ws_only.py")
                  (should-not (overlay-get (get-text-property (line-beginning-position) 'omnidiff-body-ov)
                                           'invisible))
                  ;; 6c. `T' custom TUI launcher + automatic tempdir cleanup on kill-buffer.
                  (goto-char (point-min))
                  (search-forward "alpha.py")
                  (let* ((captured nil)
                         (omnidiff-tui-launcher
                          (lambda (argv buf-name dir-arg)
                            (setq captured (list argv buf-name dir-arg))
                            (generate-new-buffer buf-name)))
                         (tui-buf (omnidiff-open-in-tui)))
                    (push tui-buf cleanup-bufs)
                    (should (buffer-live-p tui-buf))
                    (should (member "--mode" (nth 0 captured)))
                    (should (member "tui" (nth 0 captured)))
                    (let ((tui-tmp (buffer-local-value 'omnidiff--tui-tmpdir tui-buf)))
                      (should (and (stringp tui-tmp) (file-directory-p tui-tmp)))
                      (kill-buffer tui-buf)
                      (should-not (file-directory-p tui-tmp))))
                  ;; 6d. Dual `]'/`[' file cycling reuses the Dual session across `alpha.py' <-> `beta.py'.
                  (goto-char (point-min))
                  (search-forward "alpha.py")
                  (let ((dsess (omnidiff-open-dual-at-point)))
                    (push dsess cleanup-sessions)
                    (should (equal (omnidiff-dual--session-display-path dsess) "alpha.py"))
                    (with-current-buffer (omnidiff-dual--session-after-buf dsess)
                      (omnidiff-dual-next-file))
                    (should (memq dsess omnidiff-dual--sessions))
                    (should (equal (omnidiff-dual--session-display-path dsess) "beta.py"))
                    (with-current-buffer (omnidiff-dual--session-after-buf dsess)
                      (omnidiff-dual-prev-file))
                    (should (equal (omnidiff-dual--session-display-path dsess) "alpha.py"))
                    ;; `T' from inside Dual also launches custom TUI and cleans up tmpdir.
                    (let* ((dual-cap nil)
                           (omnidiff-tui-launcher
                            (lambda (argv buf-name dir-arg)
                              (setq dual-cap (list argv buf-name dir-arg))
                              (generate-new-buffer buf-name)))
                           (dtui-buf (with-current-buffer (omnidiff-dual--session-after-buf dsess)
                                       (omnidiff-dual-open-tui))))
                      (push dtui-buf cleanup-bufs)
                      (should (buffer-live-p dtui-buf))
                      (should (member "--mode" (nth 0 dual-cap)))
                      (let ((dtmp (buffer-local-value 'omnidiff--tui-tmpdir dtui-buf)))
                        (should (and (stringp dtmp) (file-directory-p dtmp)))
                        (kill-buffer dtui-buf)
                        (should-not (file-directory-p dtmp))))
                    (close-session dsess))))
              (dolist (rel '("alpha.py" "beta.py" "ws_only.py"))
                (when-let* ((vb (find-buffer-visiting (expand-file-name rel repo-multi))))
                  (kill-buffer vb)))
              (kill-buffer ov-buf)))
        (dolist (s cleanup-sessions)
          (close-session s))
        (dolist (b cleanup-bufs)
          (when (buffer-live-p b)
            (with-current-buffer b (set-buffer-modified-p nil))
            (kill-buffer b)))
        (set-frame-width (selected-frame) orig-width)
        (dolist (d (list dir repo1 repo-multi))
          (when (file-directory-p d) (delete-directory d t)))))))

;;; Wave 7 regression tests

(ert-deftest omnidiff-test/regressions-engine-dual-and-lifecycle ()
  "Regression suite for sanitization, zero-width gaps, stale tables, re-attach, folds, EOF, and async stderr."
  (let* ((dir (make-temp-file "omnidiff-reg-edl-" t))
         (orig-width (frame-width (selected-frame)))
         (cleanup-sessions nil)
         (cleanup-bufs nil)
         (cleanup-paths nil))
    (cl-flet ((close-session (s)
                (when (and (omnidiff-dual--session-p s)
                           (memq s omnidiff-dual--sessions))
                  (omnidiff-dual--teardown s nil t))))
      (unwind-protect
          (progn
            (set-frame-width (selected-frame) 140)
            ;; 1. Filename sanitization & `write-temp-for-buffer' containment for tricky hints.
            (let ((src-buf (generate-new-buffer " *omnidiff-san-buf*"))
                  (tmp-sub (expand-file-name "san-tmp" dir)))
              (push src-buf cleanup-bufs)
              (with-current-buffer src-buf (insert "payload = 1\n"))
              (dolist (hint '("~" "dir/~" "~root" "a\\b" ".." "foo\x00bar.py"))
                (let* ((clean (omnidiff-engine--sanitize-filename-hint hint))
                       (dest (omnidiff-engine-write-temp-for-buffer src-buf tmp-sub hint)))
                  (should-not (string-empty-p clean))
                  (should-not (string-prefix-p "~" clean))
                  (should-not (member clean '("." "..")))
                  (should-not (string-search "/" clean))
                  (should-not (string-search "\\" clean))
                  (should-not (string-search "\x00" clean))
                  (should (file-exists-p dest))
                  (should (file-in-directory-p dest tmp-sub))
                  (should-not (string-prefix-p "~" (file-name-nondirectory dest)))
                  (should (equal (omnidiff-engine-read-file-decoded dest) "payload = 1\n"))))
              ;; Also verify `tmpdir = nil' containment for `~root' (no HOME expansion).
              (let ((nil-dest (omnidiff-engine-write-temp-for-buffer src-buf nil "~root")))
                (push (directory-file-name (file-name-directory nil-dest)) cleanup-paths)
                (should (file-exists-p nil-dest))
                (should (file-in-directory-p nil-dest temporary-file-directory))
                (should-not (string-prefix-p "~" (file-name-nondirectory nil-dest))))
              (kill-buffer src-buf))
            ;; 2. Zero-width gap delete (`obsolete()' before edited `return compute(x, debug=True)')
            ;;    yields 2 separate stops and `counterpart-at' does not cross-highlight `, debug=True'.
            (let* ((b-lines ["def run(x):" "    obsolete()" "    return compute(x)"])
                   (a-lines ["def run(x):" "    return compute(x, debug=True)"])
                   (jdiff (make-omnidiff-json-diff
                           :before (make-omnidiff-json-side
                                    :path "a.py"
                                    :hunks (list (make-omnidiff-json-hunk
                                                  :operation 'delete
                                                  :range (make-omnidiff-json-range
                                                          :start-row 1 :start-col 0
                                                          :end-row 1 :end-col 14)
                                                  :reference-line 0)))
                           :after (make-omnidiff-json-side
                                   :path "b.py"
                                   :hunks (list (make-omnidiff-json-hunk
                                                 :operation 'insert
                                                 :range (make-omnidiff-json-range
                                                         :start-row 1 :start-col 20
                                                         :end-row 1 :end-col 32)
                                                 :reference-line 0)))))
                   (verify-gap
                    (lambda (diff)
                      (let* ((aln (omnidiff-engine-compute-alignment b-lines a-lines diff))
                             (stops (omnidiff-engine-build-change-stops diff aln))
                             (cp-del (omnidiff-engine-counterpart-at diff aln 'before 1 4))
                             (cp-ins (omnidiff-engine-counterpart-at diff aln 'after 1 20)))
                        (should (equal (omnidiff-line-alignment-anchors aln) '((0 . 0) (2 . 1))))
                        (should (= (length stops) 2))
                        (let ((s1 (nth 0 stops))
                              (s2 (nth 1 stops)))
                          (should (eq (omnidiff-change-stop-side s1) 'before))
                          (should (eq (omnidiff-change-stop-operation s1) 'delete))
                          (should-not (omnidiff-change-stop-counterpart-range s1))
                          (should (eq (omnidiff-change-stop-side s2) 'after))
                          (should (eq (omnidiff-change-stop-operation s2) 'insert))
                          (should-not (omnidiff-change-stop-counterpart-range s2)))
                        ;; Counterpart on deleted `obsolete()' is empty (must NOT highlight `, debug=True').
                        (should (eq (car cp-del) 'after))
                        (should-not (omnidiff-dual--range-nonempty-p (cdr cp-del)))
                        (should (eq (car cp-ins) 'before))
                        (should-not (omnidiff-dual--range-nonempty-p (cdr cp-ins)))))))
              (funcall verify-gap jdiff)
              (when (executable-find omnidiff-executable)
                (let ((fa (expand-file-name "gap_a.py" dir))
                      (fb (expand-file-name "gap_b.py" dir)))
                  (with-temp-file fa (insert (string-join (append b-lines nil) "\n") "\n"))
                  (with-temp-file fb (insert (string-join (append a-lines nil) "\n") "\n"))
                  (funcall verify-gap (omnidiff-engine-run-pair fa fb 'minimal)))))
            ;; 3. Stale `line-starts' after deleting lines during live edit never throws
            ;;    `args-out-of-range' and keeps `post-command-hook' intact.
            (let* ((stale-b (generate-new-buffer " *omnidiff-stale-b*"))
                   (stale-a (generate-new-buffer " *omnidiff-stale-a*"))
                   (lines-10 (cl-loop for i from 1 to 10 collect (format "v%d = %d" i i)))
                   (alines-10 (append (butlast lines-10) (list "v10 = 999")))
                   (jdiff (make-omnidiff-json-diff
                           :before (make-omnidiff-json-side
                                    :path "a.py"
                                    :hunks (list (make-omnidiff-json-hunk
                                                  :operation 'update
                                                  :range (make-omnidiff-json-range
                                                          :start-row 9 :start-col 6
                                                          :end-row 9 :end-col 8))))
                           :after (make-omnidiff-json-side
                                   :path "b.py"
                                   :hunks (list (make-omnidiff-json-hunk
                                                 :operation 'update
                                                 :range (make-omnidiff-json-range
                                                         :start-row 9 :start-col 6
                                                         :end-row 9 :end-col 9)))))))
              (push stale-b cleanup-bufs)
              (push stale-a cleanup-bufs)
              (with-current-buffer stale-b (insert (string-join lines-10 "\n") "\n"))
              (with-current-buffer stale-a (insert (string-join alines-10 "\n") "\n"))
              (let* ((omnidiff-dual-live-update nil)
                     (omnidiff-dual-fold-unchanged nil)
                     (sess (omnidiff-dual-open :before-buffer stale-b
                                               :after-buffer stale-a
                                               :json-diff jdiff
                                               :editable-after t)))
                (push sess cleanup-sessions)
                (with-current-buffer stale-a
                  (omnidiff-dual-toggle-edit)
                  ;; Delete last 6 lines so cached `after-line-starts' extends far past `point-max'.
                  (goto-char (point-min))
                  (forward-line 4)
                  (delete-region (point) (point-max))
                  (let ((stale-starts (omnidiff-dual--session-after-line-starts sess)))
                    (should (> (aref stale-starts (1- (length stale-starts))) (point-max)))
                    (should (consp (omnidiff-dual--point-to-row-byte-col
                                    stale-a stale-starts (point-max))))
                    (should (<= (omnidiff-dual--row-byte-col-to-point
                                 stale-a stale-starts 9 0)
                                (point-max)))
                    (should (listp (omnidiff-engine-range-to-buffer-regions
                                    (make-omnidiff-json-range
                                     :start-row 6 :start-col 0 :end-row 9 :end-col 5)
                                    stale-starts stale-a)))
                    (omnidiff-dual--sync-focus-and-counterpart sess nil nil)
                    (goto-char (point-min))
                    (omnidiff-dual--post-command)
                    (should (memq #'omnidiff-dual--post-command post-command-hook))
                    (omnidiff-dual-toggle-fold)))
                (close-session sess))
              (kill-buffer stale-b)
              (kill-buffer stale-a))
            ;; 4. Re-attaching Dual to an already-attached user buffer tears down Session 1
            ;;    cleanly and restores `buffer-read-only', `header-line-format', and
            ;;    `buffer-invisibility-spec = t' on `q'.
            (let* ((ubuf (generate-new-buffer " *omnidiff-reattach-user*"))
                   (ubuf-nonlocal (generate-new-buffer " *omnidiff-reattach-nonlocal*"))
                   (body (string-join (cl-loop for i from 1 to 12 collect (format "x%d = %d" i i)) "\n"))
                   (jdiff (make-omnidiff-json-diff
                           :before (make-omnidiff-json-side
                                    :path "a.py"
                                    :hunks (list (make-omnidiff-json-hunk
                                                  :operation 'update
                                                  :range (make-omnidiff-json-range
                                                          :start-row 0 :start-col 5
                                                          :end-row 0 :end-col 6))))
                           :after (make-omnidiff-json-side
                                   :path "b.py"
                                   :hunks (list (make-omnidiff-json-hunk
                                                 :operation 'update
                                                 :range (make-omnidiff-json-range
                                                         :start-row 0 :start-col 5
                                                         :end-row 0 :end-col 6)))))))
              (push ubuf cleanup-bufs)
              (push ubuf-nonlocal cleanup-bufs)
              (with-current-buffer ubuf
                (insert body "\n")
                (setq buffer-read-only nil)
                (setq-local header-line-format "USER-ORIGINAL-HL")
                (setq buffer-invisibility-spec t))
              (let* ((omnidiff-dual-fold-unchanged t)
                     (omnidiff-dual-context-lines 1)
                     (s1 (omnidiff-dual-open :before-source (concat body "\n")
                                             :after-buffer ubuf
                                             :json-diff jdiff))
                     (s1-before (omnidiff-dual--session-before-buf s1)))
                (push s1 cleanup-sessions)
                (should (memq s1 omnidiff-dual--sessions))
                (should (buffer-live-p s1-before))
                (with-current-buffer ubuf
                  (should buffer-read-only)
                  (should (eq buffer-invisibility-spec t)))
                ;; Re-attach a brand new Session 2 to the same user buffer without quitting S1.
                (let ((s2 (omnidiff-dual-open :before-source (concat body "\n")
                                              :after-buffer ubuf
                                              :json-diff jdiff)))
                  (push s2 cleanup-sessions)
                  (should-not (memq s1 omnidiff-dual--sessions))
                  (should-not (buffer-live-p s1-before))
                  (should (memq s2 omnidiff-dual--sessions))
                  (with-current-buffer ubuf
                    (should (eq buffer-invisibility-spec t))
                    (omnidiff-dual-quit))
                  (should-not (memq s2 omnidiff-dual--sessions))
                  (with-current-buffer ubuf
                    (should-not buffer-read-only)
                    (should (local-variable-p 'header-line-format))
                    (should (equal header-line-format "USER-ORIGINAL-HL"))
                    (should (eq buffer-invisibility-spec t))
                    (should-not (cl-some (lambda (o) (overlay-get o 'omnidiff-dual-ov))
                                         (overlays-in (point-min) (point-max)))))))
              ;; Also verify non-local `header-line-format' stays non-local after re-attach + quit.
              (with-current-buffer ubuf-nonlocal
                (insert body "\n")
                (setq buffer-read-only nil)
                (kill-local-variable 'header-line-format)
                (setq buffer-invisibility-spec t))
              (let* ((s3 (omnidiff-dual-open :before-source (concat body "\n")
                                             :after-buffer ubuf-nonlocal
                                             :json-diff jdiff))
                     (s4 (omnidiff-dual-open :before-source (concat body "\n")
                                             :after-buffer ubuf-nonlocal
                                             :json-diff jdiff)))
                (push s3 cleanup-sessions)
                (push s4 cleanup-sessions)
                (should-not (memq s3 omnidiff-dual--sessions))
                (with-current-buffer ubuf-nonlocal
                  (omnidiff-dual-quit)
                  (should-not buffer-read-only)
                  (should-not (local-variable-p 'header-line-format))
                  (should (eq buffer-invisibility-spec t))))
              (kill-buffer ubuf)
              (kill-buffer ubuf-nonlocal))
            ;; 5. Fold banner is reachable via `next-line' (col 0 & col > 0) and `previous-line',
            ;;    `RET' expands it, and moving down never skips the next visible line.
            (let* ((fb-b (generate-new-buffer " *omnidiff-foldnav-b*"))
                   (fb-a (generate-new-buffer " *omnidiff-foldnav-a*"))
                   (base-lines (cl-loop for i from 1 to 20 collect (format "line_%02d = %d" i i)))
                   (after-lines (copy-sequence base-lines)))
              (setf (nth 0 after-lines) "line_01 = 999")
              (setf (nth 19 after-lines) "line_20 = 999")
              (push fb-b cleanup-bufs)
              (push fb-a cleanup-bufs)
              (with-current-buffer fb-b (insert (string-join base-lines "\n") "\n"))
              (with-current-buffer fb-a (insert (string-join after-lines "\n") "\n"))
              (let* ((jdiff (make-omnidiff-json-diff
                             :before (make-omnidiff-json-side
                                      :path "a.py"
                                      :hunks (list (make-omnidiff-json-hunk
                                                    :operation 'update
                                                    :range (make-omnidiff-json-range
                                                            :start-row 0 :start-col 10
                                                            :end-row 0 :end-col 11))
                                                   (make-omnidiff-json-hunk
                                                    :operation 'update
                                                    :range (make-omnidiff-json-range
                                                            :start-row 19 :start-col 10
                                                            :end-row 19 :end-col 12))))
                             :after (make-omnidiff-json-side
                                     :path "b.py"
                                     :hunks (list (make-omnidiff-json-hunk
                                                   :operation 'update
                                                   :range (make-omnidiff-json-range
                                                           :start-row 0 :start-col 10
                                                           :end-row 0 :end-col 13))
                                                  (make-omnidiff-json-hunk
                                                   :operation 'update
                                                   :range (make-omnidiff-json-range
                                                           :start-row 19 :start-col 10
                                                           :end-row 19 :end-col 13))))))
                     (omnidiff-dual-fold-unchanged t)
                     (omnidiff-dual-context-lines 2)
                     (sess (omnidiff-dual-open :before-buffer fb-b
                                               :after-buffer fb-a
                                               :json-diff jdiff)))
                (push sess cleanup-sessions)
                (omnidiff-dual--focus-side sess 'after)
                (with-current-buffer fb-a
                  (let ((ov (cl-find-if (lambda (o)
                                          (and (overlay-get o 'omnidiff-dual-ov)
                                               (eq (overlay-get o 'omnidiff-dual-kind) 'fold)))
                                        (overlays-in (point-min) (point-max)))))
                    (should ov)
                    (should (overlayp (overlay-get ov 'omnidiff-dual-fold-nl-ov)))
                    ;; From L3 col 0 -> `next-line' lands on fold -> `next-line' lands on L18 (no skip).
                    (goto-char (point-min))
                    (forward-line 2)
                    (setq last-command nil)
                    (call-interactively #'next-line)
                    (should (eq (omnidiff-dual--fold-overlay-at (point)) ov))
                    (setq last-command 'next-line)
                    (call-interactively #'next-line)
                    (should (= (line-number-at-pos) 18))
                    (should-not (omnidiff-dual--fold-overlay-at (point)))
                    ;; From L3 col 5 -> `next-line' lands on fold -> `next-line' lands on L18 (no skip).
                    (goto-char (point-min))
                    (forward-line 2)
                    (forward-char 5)
                    (setq last-command nil)
                    (call-interactively #'next-line)
                    (should (eq (omnidiff-dual--fold-overlay-at (point)) ov))
                    (setq last-command 'next-line)
                    (call-interactively #'next-line)
                    (should (= (line-number-at-pos) 18))
                    ;; From L18 -> `previous-line' lands on fold -> `RET' expands both fold & nl overlays.
                    (goto-char (point-min))
                    (forward-line 17)
                    (setq last-command nil)
                    (call-interactively #'previous-line)
                    (should (eq (omnidiff-dual--fold-overlay-at (point)) ov))
                    (let ((nl-ov (overlay-get ov 'omnidiff-dual-fold-nl-ov)))
                      (should (eq (omnidiff-dual-jump-counterpart) 'expanded-fold))
                      (should-not (overlay-buffer ov))
                      (should-not (overlay-buffer nl-ov)))))
                (close-session sess))
              (kill-buffer fb-b)
              (kill-buffer fb-a))
            ;; 6. `omnidiff-dual--buffer-lines' excludes phantom EOF empty line;
            ;;    `change-major-mode-hook' (`revert-buffer') tears down Dual cleanly without leaks.
            (with-temp-buffer
              (insert "a\nb\nc\n")
              (let ((starts (omnidiff-engine-buffer-line-offsets (current-buffer))))
                (should (= (length starts) 4))
                (should (equal (omnidiff-dual--buffer-lines (current-buffer) starts)
                               ["a" "b" "c"]))))
            (with-temp-buffer
              (insert "a\nb\nc")
              (let ((starts (omnidiff-engine-buffer-line-offsets (current-buffer))))
                (should (equal (omnidiff-dual--buffer-lines (current-buffer) starts)
                               ["a" "b" "c"]))))
            (with-temp-buffer
              (let ((starts (omnidiff-engine-buffer-line-offsets (current-buffer))))
                (should (equal (omnidiff-dual--buffer-lines (current-buffer) starts) []))))
            (with-temp-buffer
              (insert "a\n\n")
              (let ((starts (omnidiff-engine-buffer-line-offsets (current-buffer))))
                (should (equal (omnidiff-dual--buffer-lines (current-buffer) starts)
                               ["a" ""]))))
            (let* ((rev-file (expand-file-name "revert_me.py" dir))
                   (rev-buf nil)
                   (jdiff (make-omnidiff-json-diff
                           :before (make-omnidiff-json-side
                                    :path "a.py"
                                    :hunks (list (make-omnidiff-json-hunk
                                                  :operation 'update
                                                  :range (make-omnidiff-json-range
                                                          :start-row 0 :start-col 4
                                                          :end-row 0 :end-col 5))))
                           :after (make-omnidiff-json-side
                                   :path "b.py"
                                   :hunks (list (make-omnidiff-json-hunk
                                                 :operation 'update
                                                 :range (make-omnidiff-json-range
                                                         :start-row 0 :start-col 4
                                                         :end-row 0 :end-col 5)))))))
              (with-temp-file rev-file (insert "x = 2\n"))
              (setq rev-buf (find-file-noselect rev-file))
              (push rev-buf cleanup-bufs)
              (let* ((sess (omnidiff-dual-open :before-source "x = 1\n"
                                               :after-path rev-file
                                               :after-buffer rev-buf
                                               :json-diff jdiff
                                               :editable-after t))
                     (owned-before (omnidiff-dual--session-before-buf sess))
                     (sess-tmp (omnidiff-dual--ensure-tmpdir sess)))
                (push sess cleanup-sessions)
                (should (file-directory-p sess-tmp))
                (should (buffer-live-p owned-before))
                (with-current-buffer rev-buf
                  (revert-buffer t t)
                  (should-not (memq sess omnidiff-dual--sessions))
                  (should-not (buffer-live-p owned-before))
                  (should-not (file-directory-p sess-tmp))
                  (should-not omnidiff-dual--session)
                  (should-not omnidiff-dual--side)
                  (should-not (bound-and-true-p omnidiff-dual-mode))
                  (should-not buffer-read-only)
                  (should-not (memq #'omnidiff-dual--post-command post-command-hook))
                  (should-not (cl-some (lambda (o) (overlay-get o 'omnidiff-dual-ov))
                                       (overlays-in (point-min) (point-max))))))
              (kill-buffer rev-buf))
            ;; 7. Async stderr error callback contains stderr text and NO
            ;;    `"Process omnidiff-json-stderr finished"'.
            (let* ((fake-bin (expand-file-name "fake-omnidiff-err.sh" dir))
                   (fa (expand-file-name "err_a.txt" dir))
                   (fb (expand-file-name "err_b.txt" dir))
                   (ok-called nil)
                   (err-msg nil))
              (with-temp-file fake-bin
                (insert "#!/bin/sh\necho 'synthetic stderr boom' >&2\nexit 2\n"))
              (set-file-modes fake-bin #o755)
              (with-temp-file fa (insert "a\n"))
              (with-temp-file fb (insert "b\n"))
              (let ((proc (omnidiff-engine-run-pair-async
                           fa fb
                           (lambda (_d) (setq ok-called t))
                           :executable fake-bin
                           :error-callback (lambda (msg) (setq err-msg msg))))
                    (deadline (+ (float-time) 10.0)))
                (while (and (null ok-called) (null err-msg) (< (float-time) deadline))
                  (accept-process-output proc 0.02))
                (should-not ok-called)
                (should (stringp err-msg))
                (should (string-search "synthetic stderr boom" err-msg))
                (should-not (string-search "Process omnidiff-json-stderr finished" err-msg))
                (should-not (string-search "Process omnidiff-json-stderr" err-msg)))))
        (dolist (s cleanup-sessions)
          (close-session s))
        (dolist (b cleanup-bufs)
          (when (buffer-live-p b)
            (with-current-buffer b (set-buffer-modified-p nil))
            (kill-buffer b)))
        (dolist (p cleanup-paths)
          (cond
           ((file-directory-p p) (ignore-errors (delete-directory p t)))
           ((file-exists-p p) (ignore-errors (delete-file p)))))
        (set-frame-width (selected-frame) orig-width)
        (when (file-directory-p dir)
          (delete-directory dir t))))))

(ert-deftest omnidiff-test/regressions-security-containers-image-and-ux ()
  "Regression suite for archive security, PDF regex, binary PNG git blobs, fontify hooks, and UX."
  (let* ((dir (make-temp-file "omnidiff-reg-sec-" t))
         (orig-width (frame-width (selected-frame)))
         (cleanup-sessions nil)
         (cleanup-bufs nil))
    (cl-flet ((close-session (s)
                (when (and (omnidiff-dual--session-p s)
                           (memq s omnidiff-dual--sessions))
                  (omnidiff-dual--teardown s nil t)))
              (git-in (repo &rest args)
                (let ((default-directory repo))
                  (unless (zerop (apply #'call-process "git" nil nil nil args))
                    (error "git %S failed in %s" args repo)))))
      (unwind-protect
          (progn
            (set-frame-width (selected-frame) 140)
            ;; 1. Archive bomb limit, `.env' vs `env' dotfiles, ZIP dir skip,
            ;;    NUL / `~root' key containment, and `omnidiff--blob-dest' containment.
            (let* ((python (executable-find "python3"))
                   (blob-tmp (expand-file-name "blob-tmp" dir)))
              (make-directory blob-tmp t)
              ;; 1a. `omnidiff--blob-dest' keeps tricky filenames contained inside TMPDIR.
              (dolist (p '("~" "dir/~" "~root" "~root.py" "a\\b" ".." "bad\x00name" "Makefile"))
                (let ((dest (omnidiff--blob-dest "abcd1234" p blob-tmp)))
                  (should (file-in-directory-p dest blob-tmp))
                  (should (string-prefix-p (file-name-as-directory (expand-file-name blob-tmp))
                                           dest))
                  (should-not (string-prefix-p "~" (file-name-nondirectory dest)))
                  (should-not (string-search "\x00" dest))))
              (should (string-suffix-p ".py" (omnidiff--blob-dest "abcd1234" "~root.py" blob-tmp)))
              (should (equal (file-name-nondirectory
                              (omnidiff--blob-dest "abcd1234" "Makefile" blob-tmp))
                             "Makefile"))
              ;; 1b. NUL in archive key is rejected immediately.
              (let ((dummy-arch (make-omnidiff-file :path "pkg.zip" :target-path "pkg.zip"
                                                    :status 'modified)))
                (should-error
                 (omnidiff--drilldown-archive-member dummy-arch "bad\x00key" dir nil)
                 :type 'user-error))
              (when python
                ;; 1c. Archive bomb exceeding `omnidiff-archive-extract-max-bytes' signals
                ;;     `user-error' and leaves no partial file (both ZIP header and `.gz' stream).
                (let* ((bomb-zip (expand-file-name "bomb.zip" dir))
                       (bomb-gz (expand-file-name "bomb.txt.gz" dir))
                       (stream-out-a (expand-file-name "gz-out-a" dir))
                       (stream-out-b (expand-file-name "gz-out-b" dir)))
                  (unless (zerop
                           (call-process
                            python nil nil nil "-c"
                            "import gzip, sys, zipfile
with zipfile.ZipFile(sys.argv[1], 'w', compression=zipfile.ZIP_DEFLATED) as zf:
    zf.writestr('big.txt', 'A' * 4096)
with gzip.open(sys.argv[2], 'wb') as gz:
    gz.write(b'B' * 4096)
"
                            bomb-zip bomb-gz))
                    (error "Failed to build bomb test archives"))
                  ;; Streaming `.gz' bomb leaves no partial output file and exits 3.
                  (should (= (call-process python nil nil nil
                                           "-c" omnidiff--archive-extract-py
                                           bomb-gz bomb-gz "(content)"
                                           stream-out-a stream-out-b "1024")
                             3))
                  (should-not (file-exists-p stream-out-a))
                  (should-not (file-exists-p stream-out-b))
                  ;; Full Elisp drilldown signals `user-error' and leaves no temp dir.
                  (let* ((omnidiff-archive-extract-max-bytes 1024)
                         (before-tmps (directory-files temporary-file-directory nil
                                                       "\\`omnidiff-archive-"))
                         (f-zip (make-omnidiff-file :path "bomb.zip" :target-path "bomb.zip"
                                                    :status 'modified
                                                    :before-disk-path bomb-zip
                                                    :after-disk-path bomb-zip))
                         (f-gz (make-omnidiff-file :path "bomb.txt.gz" :target-path "bomb.txt.gz"
                                                   :status 'modified
                                                   :before-disk-path bomb-gz
                                                   :after-disk-path bomb-gz)))
                    (dolist (pair (list (cons f-zip "big.txt") (cons f-gz "(content)")))
                      (let ((err (should-error
                                  (omnidiff--drilldown-archive-member
                                   (car pair) (cdr pair) dir nil)
                                  :type 'user-error)))
                        (should (string-search "exceeds extraction size limit"
                                               (error-message-string err)))))
                    (should (equal (directory-files temporary-file-directory nil
                                                    "\\`omnidiff-archive-")
                                   before-tmps))))
                ;; 1d. `.env' vs `env' dotfiles never mis-match; ZIP directory entry is skipped.
                (let* ((z-dot (expand-file-name "dotonly.zip" dir))
                       (z-plain (expand-file-name "plainonly.zip" dir))
                       (z-dir (expand-file-name "direntry.zip" dir))
                       (out-a (expand-file-name "dot-a" dir))
                       (out-b (expand-file-name "dot-b" dir)))
                  (unless (zerop
                           (call-process
                            python nil nil nil "-c"
                            "import sys, zipfile
with zipfile.ZipFile(sys.argv[1], 'w') as zf:
    zf.writestr('.env', 'SECRET=1\\n')
with zipfile.ZipFile(sys.argv[2], 'w') as zf:
    zf.writestr('env', 'PUBLIC=2\\n')
with zipfile.ZipFile(sys.argv[3], 'w') as zf:
    zf.writestr('subdir/', '')
    zf.writestr('subdir/file.txt', 'ok\\n')
"
                            z-dot z-plain z-dir))
                    (error "Failed to build dotfile/dir test zips"))
                  ;; Querying `env' against `.env'-only zip extracts nothing; querying `.env'
                  ;; against `env'-only zip extracts nothing.
                  (should (zerop (call-process python nil nil nil
                                               "-c" omnidiff--archive-extract-py
                                               z-dot z-dot "env" out-a out-b "0")))
                  (should-not (file-exists-p out-a))
                  (should-not (file-exists-p out-b))
                  (should (zerop (call-process python nil nil nil
                                               "-c" omnidiff--archive-extract-py
                                               z-plain z-plain ".env" out-a out-b "0")))
                  (should-not (file-exists-p out-a))
                  (should-not (file-exists-p out-b))
                  ;; `./.env' still matches `.env'.
                  (should (zerop (call-process python nil nil nil
                                               "-c" omnidiff--archive-extract-py
                                               z-dot "/dev/null" "./.env" out-a out-b "0")))
                  (should (file-exists-p out-a))
                  (should (equal (omnidiff-engine-read-file-decoded out-a) "SECRET=1\n"))
                  (delete-file out-a)
                  ;; ZIP directory entry `subdir/' is skipped (no file written, drilldown errors).
                  (should (zerop (call-process python nil nil nil
                                               "-c" omnidiff--archive-extract-py
                                               z-dir z-dir "subdir/" out-a out-b "0")))
                  (should-not (file-exists-p out-a))
                  (should-not (file-exists-p out-b))
                  (let ((f-dir (make-omnidiff-file :path "direntry.zip" :target-path "direntry.zip"
                                                   :status 'modified
                                                   :before-disk-path z-dir
                                                   :after-disk-path z-dir)))
                    (should-error
                     (omnidiff--drilldown-archive-member f-dir "subdir/" dir nil)
                     :type 'user-error)))
                ;; 1e. `~root' archive member key stays contained and never expands HOME/user dir.
                (when (executable-find omnidiff-executable)
                  (let ((z-tilde-a (expand-file-name "tilde_a.zip" dir))
                        (z-tilde-b (expand-file-name "tilde_b.zip" dir)))
                    (unless (zerop
                             (call-process
                              python nil nil nil "-c"
                              "import sys, zipfile
with zipfile.ZipFile(sys.argv[1], 'w') as zf:
    zf.writestr('~root', 'alpha = 1\\n')
with zipfile.ZipFile(sys.argv[2], 'w') as zf:
    zf.writestr('~root', 'alpha = 2\\n')
"
                              z-tilde-a z-tilde-b))
                      (error "Failed to build tilde test zips"))
                    (let* ((f-tilde (make-omnidiff-file :path "t.zip" :target-path "t.zip"
                                                        :status 'modified
                                                        :before-disk-path z-tilde-a
                                                        :after-disk-path z-tilde-b))
                           (sess (omnidiff--drilldown-archive-member f-tilde "~root" dir nil)))
                      (push sess cleanup-sessions)
                      (should (omnidiff-dual--session-p sess))
                      (should (string-search "~root" (omnidiff-dual--session-display-path sess)))
                      (close-session sess))))))
            ;; 2. PDF page key regex accepts `"page 3"' / `"3"' and rejects `"attachment/q1-2026.csv"'.
            (let* ((empty-bin (expand-file-name "empty-bin" dir))
                   (fake-bin (expand-file-name "fake-pdf-bin" dir))
                   (pdf-log (expand-file-name "pdftoppm.log" dir))
                   (dummy-pdf (expand-file-name "doc.pdf" dir))
                   (pdf-file (make-omnidiff-file
                              :path "doc.pdf" :target-path "doc.pdf" :status 'modified
                              :before-disk-path dummy-pdf :after-disk-path dummy-pdf
                              :container (make-omnidiff-json-container
                                          :family 'documents
                                          :before (make-omnidiff-json-container-side :format "PDF")
                                          :after (make-omnidiff-json-container-side :format "PDF")))))
              (make-directory empty-bin t)
              (make-directory fake-bin t)
              (with-temp-file dummy-pdf (insert "%PDF-1.4\n"))
              (let ((fake-pdftoppm (expand-file-name "pdftoppm" fake-bin)))
                (with-temp-file fake-pdftoppm
                  (insert (format "#!/bin/sh\nprintf '%%s\\n' \"$*\" >> %s\nexit 0\n"
                                  (shell-quote-argument pdf-log))))
                (set-file-modes fake-pdftoppm #o755))
              ;; Without rasterizer: valid page keys reach rasterizer-required message;
              ;; attachment key is rejected as a non-page PDF member.
              (let ((exec-path (list empty-bin)))
                (dolist (valid-key '("page 3" "3"))
                  (let ((msg (omnidiff--drilldown-pdf-page pdf-file valid-key dir nil)))
                    (should (string-search "PDF page preview requires" msg))
                    (should (string-search "pdftoppm" msg))))
                (let ((rej (omnidiff--drilldown-pdf-page
                            pdf-file "attachment/q1-2026.csv" dir nil)))
                  (should (string-search "PDF member" rej))
                  (should (string-search "attachment/q1-2026.csv" rej))
                  (should-not (string-search "pdftoppm" rej))))
              ;; With fake `pdftoppm': `"page 3"' and `"3"' invoke `-f 3 -l 3';
              ;; `"attachment/q1-2026.csv"' never spawns `pdftoppm'.
              (let ((exec-path (list fake-bin)))
                (omnidiff--drilldown-pdf-page pdf-file "attachment/q1-2026.csv" dir nil)
                (should-not (file-exists-p pdf-log))
                (dolist (valid-key '("page 3" "3"))
                  (when (file-exists-p pdf-log) (delete-file pdf-log))
                  (let ((err (should-error
                              (omnidiff--drilldown-pdf-page pdf-file valid-key dir nil)
                              :type 'user-error)))
                    (should (string-search "page 3" (error-message-string err))))
                  (should (file-exists-p pdf-log))
                  (should (string-search "-f 3 -l 3"
                                         (omnidiff-engine-read-file-decoded pdf-log))))))
            ;; 3. `omnidiff-image--read-git-blob' preserves `\r\n' PNG header bytes
            ;;    under `default-process-coding-system = (utf-8-dos . utf-8-dos)'.
            (let ((img-repo (expand-file-name "img-repo" dir)))
              (make-directory img-repo t)
              (git-in img-repo "init" "-q" "-b" "main")
              (git-in img-repo "config" "user.name" "Test")
              (git-in img-repo "config" "user.email" "test@example.com")
              (omnidiff-test--write-bin (expand-file-name "dot.png" img-repo)
                                        omnidiff-test--png-red-2x2)
              (git-in img-repo "add" "dot.png")
              (git-in img-repo "commit" "-q" "-m" "add png")
              (let* ((default-directory img-repo)
                     (oid (string-trim
                           (with-temp-buffer
                             (call-process "git" nil t nil "rev-parse" "HEAD:dot.png")
                             (buffer-string))))
                     (default-process-coding-system '(utf-8-dos . utf-8-dos))
                     (raw (omnidiff-image--read-git-blob oid "dot.png" img-repo nil)))
                (should (stringp raw))
                (should-not (multibyte-string-p raw))
                (should (string-prefix-p omnidiff-image--png-magic raw))
                (should (= (aref raw 4) #x0d))
                (should (= (aref raw 5) #x0a))
                (should (equal raw omnidiff-test--png-red-2x2))))
            ;; 4. `omnidiff-render--fontify-lines' does not fire `font-lock-mode-hook'
            ;;    or `change-major-mode-hook'.
            (let ((fl-fired 0)
                  (cmm-fired 0))
              (let ((omnidiff-overview-syntax-highlight t)
                    (font-lock-mode-hook (list (lambda () (setq fl-fired (1+ fl-fired)))))
                    (change-major-mode-hook (list (lambda () (setq cmm-fired (1+ cmm-fired))))))
                (let ((lines (omnidiff-render--fontify-lines
                              "def demo(x):\n    return x + 1\n" "demo.py")))
                  (should (= fl-fired 0))
                  (should (= cmm-fired 0))
                  (should (= (length lines) 2))
                  (should (eq (get-text-property 0 'face (aref lines 0))
                              'font-lock-keyword-face)))))
            ;; 5. `omnidiff-files' on a dirty visited `after' buffer diffs its live
            ;;    modified contents and cleans up temp snapshot on `q'.
            (when (executable-find omnidiff-executable)
              (let* ((fa (expand-file-name "dirty_a.py" dir))
                     (fb (expand-file-name "dirty_b.py" dir))
                     (before-snaps (directory-files temporary-file-directory t
                                                    "\\`omnidiff-files-"))
                     (after-buf nil))
                (with-temp-file fa (insert "def val():\n    return 1\n"))
                (with-temp-file fb (insert "def val():\n    return 1\n"))
                (setq after-buf (find-file-noselect fb))
                (push after-buf cleanup-bufs)
                (with-current-buffer after-buf
                  (goto-char (point-min))
                  (search-forward "1")
                  (replace-match "777")
                  (should (buffer-modified-p)))
                (let* ((sess (omnidiff-files fa fb))
                       (after-snaps (directory-files temporary-file-directory t
                                                     "\\`omnidiff-files-"))
                       (new-snaps (cl-set-difference after-snaps before-snaps :test #'equal)))
                  (push sess cleanup-sessions)
                  (should (omnidiff-dual--session-p sess))
                  (should (eq (omnidiff-dual--session-after-buf sess) after-buf))
                  (should new-snaps)
                  (should (cl-some (lambda (ov)
                                     (with-current-buffer after-buf
                                       (equal (buffer-substring-no-properties
                                               (overlay-start ov) (overlay-end ov))
                                              "777")))
                                   (omnidiff-test--token-overlays after-buf)))
                  (with-current-buffer after-buf
                    (omnidiff-dual-quit))
                  (should (buffer-live-p after-buf))
                  (should (with-current-buffer after-buf (buffer-modified-p)))
                  (dolist (snap new-snaps)
                    (should-not (file-directory-p snap))))
                (with-current-buffer after-buf (set-buffer-modified-p nil))
                (kill-buffer after-buf)))
            ;; 6. `]' / `[' in Dual does not error after Overview buffer is killed.
            (let* ((ov-buf (generate-new-buffer " *omnidiff-killed-ov*"))
                   (mk-file (lambda (name old-val new-val)
                              (make-omnidiff-file
                               :path name :target-path name :status 'modified
                               :before-source (format "x = %s\n" old-val)
                               :after-source (format "x = %s\n" new-val)
                               :json-diff (make-omnidiff-json-diff
                                           :before (make-omnidiff-json-side
                                                    :path name
                                                    :hunks (list (make-omnidiff-json-hunk
                                                                  :operation 'update
                                                                  :range (make-omnidiff-json-range
                                                                          :start-row 0 :start-col 4
                                                                          :end-row 0 :end-col (+ 4 (length old-val))))))
                                           :after (make-omnidiff-json-side
                                                   :path name
                                                   :hunks (list (make-omnidiff-json-hunk
                                                                 :operation 'update
                                                                 :range (make-omnidiff-json-range
                                                                         :start-row 0 :start-col 4
                                                                         :end-row 0 :end-col (+ 4 (length new-val))))))))))
                   (diff (make-omnidiff-diff :files (list (funcall mk-file "one.py" "1" "2")
                                                          (funcall mk-file "two.py" "3" "4")))))
              (push ov-buf cleanup-bufs)
              (with-current-buffer ov-buf
                (omnidiff-mode)
                (setq omnidiff--repo-dir dir
                      omnidiff--diff diff)
                (omnidiff-render-buffer diff)
                (goto-char (point-min))
                (let ((dsess (omnidiff-open-dual-at-point)))
                  (push dsess cleanup-sessions)
                  (should (omnidiff-dual--session-p dsess))
                  (kill-buffer ov-buf)
                  (should-not (buffer-live-p ov-buf))
                  (with-current-buffer (omnidiff-dual--session-after-buf dsess)
                    (should (string-search "no longer open" (omnidiff-dual-next-file)))
                    (should (string-search "no longer open" (omnidiff-dual-prev-file))))
                  (should (memq dsess omnidiff-dual--sessions))
                  (close-session dsess))))
            ;; 7. `omnidiff-open-dual-at-point' on a `'deleted' file hunk header
            ;;    focuses `'before' (not empty `'after').
            (let* ((ov-del (generate-new-buffer " *omnidiff-del-hunk-ov*"))
                   (del-src "def legacy():\n    return 1\n    # end\n")
                   (del-file (make-omnidiff-file
                              :path "gone.py" :target-path "gone.py" :status 'deleted
                              :removed 3 :max-line 3
                              :before-source del-src
                              :after-source ""
                              :json-diff (make-omnidiff-json-diff
                                          :before (make-omnidiff-json-side
                                                   :path "gone.py"
                                                   :hunks (list (make-omnidiff-json-hunk
                                                                 :operation 'delete
                                                                 :range (make-omnidiff-json-range
                                                                         :start-row 0 :start-col 0
                                                                         :end-row 2 :end-col 9))))
                                          :after (make-omnidiff-json-side :path "/dev/null")
                                          :summary 'deleted-file)))
                   (diff (make-omnidiff-diff :files (list del-file))))
              (push ov-del cleanup-bufs)
              (with-current-buffer ov-del
                (omnidiff-mode)
                (setq omnidiff--repo-dir dir
                      omnidiff--diff diff)
                (omnidiff-render-buffer diff)
                (goto-char (point-min))
                (omnidiff-next-hunk)
                (should (get-text-property (point) 'omnidiff-hunk))
                (should (integerp (get-text-property (point) 'omnidiff-line)))
                (let ((dsess (omnidiff-open-dual-at-point)))
                  (push dsess cleanup-sessions)
                  (should (omnidiff-dual--session-p dsess))
                  (should (eq (omnidiff-dual--session-active-side dsess) 'before))
                  (should (eq (window-buffer (selected-window))
                              (omnidiff-dual--session-before-buf dsess)))
                  (should (> (buffer-size (omnidiff-dual--session-before-buf dsess)) 0))
                  (should (= (buffer-size (omnidiff-dual--session-after-buf dsess)) 0))
                  (close-session dsess)))
              (kill-buffer ov-del)))
        (dolist (s cleanup-sessions)
          (close-session s))
        (dolist (b cleanup-bufs)
          (when (buffer-live-p b)
            (with-current-buffer b (set-buffer-modified-p nil))
            (kill-buffer b)))
        (set-frame-width (selected-frame) orig-width)
        (when (file-directory-p dir)
          (delete-directory dir t))))))

(ert-deftest omnidiff-test/ergonomics-aesthetics-and-menus ()
  "Verify perceptual theme luma, Dual header/fringe/click/layout polish, image map safety, SBS reflow, and transient menus."
  ;; 1. Rec. 709 relative luminance: pure green is light, pure blue is dark.
  (should-not (omnidiff-theme--dark-color-p "#00ff00"))
  (should (omnidiff-theme--dark-color-p "#0000ff"))
  ;; Header badge underline, cross-highlight underline, and Dual active/inactive faces exist.
  (dolist (f '(omnidiff-dual-header-file
               omnidiff-dual-header-file-inactive
               omnidiff-dual-header-before-inactive
               omnidiff-dual-header-after-inactive
               omnidiff-dual-header-badge))
    (should (facep f)))
  (let* ((attrs (omnidiff-theme--face-color-attrs
                 (omnidiff-theme--resolve-palette
                  (omnidiff-theme--generic-base 'dark) 'omni-semantic)))
         (badge-attrs (cdr (assq 'omnidiff-header-badge attrs)))
         (cross-attrs (cdr (assq 'omnidiff-token-cross-highlight attrs))))
    (should (plist-get badge-attrs :underline))
    (should (plist-get cross-attrs :underline))
    (should-not (plist-get cross-attrs :foreground)))
  ;; 2. Transient menus are defined for Overview, Dual, and Image.
  (should (commandp 'omnidiff-overview-menu))
  (should (commandp 'omnidiff-dual-menu))
  (should (commandp 'omnidiff-image-menu))
  ;; 3. Edit-mode map exposes fold, counterpart, and help bindings without shadowing editing.
  (should (eq (lookup-key omnidiff-dual-edit-mode-map (kbd "C-c z")) #'omnidiff-dual-toggle-fold))
  (should (eq (lookup-key omnidiff-dual-edit-mode-map (kbd "C-c RET")) #'omnidiff-dual-jump-counterpart))
  (should (eq (lookup-key omnidiff-dual-edit-mode-map (kbd "C-c ?")) #'omnidiff-dual-help))
  ;; 4. Image header map keeps horizontal motion free; image card map keeps h/l/<left>/<right>.
  (should-not (lookup-key omnidiff-image-header-map (kbd "<left>")))
  (should-not (lookup-key omnidiff-image-header-map (kbd "<right>")))
  (should-not (lookup-key omnidiff-image-header-map (kbd "h")))
  (should-not (lookup-key omnidiff-image-header-map (kbd "l")))
  (should (eq (lookup-key omnidiff-image-card-map (kbd "<left>")) #'omnidiff-image-adjust-decrease))
  (should (eq (lookup-key omnidiff-image-card-map (kbd "h")) #'omnidiff-image-adjust-decrease))
  ;; 5. Distinct fringe bitmap symbols per diff operation.
  (should (= (length (cl-delete-duplicates
                      (mapcar #'omnidiff-dual--fringe-bitmap '(insert delete update move))))
             4))
  ;; 6. Smart path truncation preserves filename & handles renames.
  (let ((short (omnidiff-render--truncate-path "src/very/deep/nested/component/widget.tsx" 24))
        (ren (omnidiff-render--truncate-path
              "src/deep/old_dir/old_name.py → src/deep/new_dir/new_name.py" 32)))
    (should (<= (string-width short) 24))
    (should (string-suffix-p "widget.tsx" short))
    (should (<= (string-width ren) 32))
    (should (string-search " → " ren))
    (should (string-suffix-p "new_name.py" ren)))
  ;; 7. Overview context-aware header hints include `? help' and vary by context.
  (let ((def-h (car (omnidiff-render--header-hints 'default)))
        (img-h (car (omnidiff-render--header-hints 'image)))
        (cnt-h (car (omnidiff-render--header-hints 'container)))
        (std-h (car (omnidiff-render--header-hints 'standalone))))
    (dolist (h (list def-h img-h cnt-h std-h))
      (should (string-search "? help" h)))
    (should (string-search "d dual" def-h))
    (should-not (string-search "d dual" img-h))
    (should (string-search "1-6 mode" img-h))
    (should (string-search "RET inspect" cnt-h))
    (should-not (string-search "d dual" std-h)))
  ;; 8. Overview tab-width customization, mouse click maps on headers & containers, and SBS resize reflow.
  (let ((buf (generate-new-buffer " *omnidiff-ergo-ov*"))
        (orig-width (frame-width (selected-frame))))
    (unwind-protect
        (let* ((omnidiff-overview-tab-width 2)
               (jdiff (make-omnidiff-json-diff
                       :before (make-omnidiff-json-side
                                :path "src/alpha.py"
                                :hunks (list (make-omnidiff-json-hunk
                                              :operation 'update
                                              :range (make-omnidiff-json-range
                                                      :start-row 0 :start-col 1
                                                      :end-row 0 :end-col 4))))
                       :after (make-omnidiff-json-side
                               :path "src/alpha.py"
                               :hunks (list (make-omnidiff-json-hunk
                                             :operation 'update
                                             :range (make-omnidiff-json-range
                                                     :start-row 0 :start-col 1
                                                     :end-row 0 :end-col 4))))))
               (f-text (make-omnidiff-file
                        :path "src/alpha.py" :target-path "src/alpha.py" :status 'modified
                        :before-source "\told()\n" :after-source "\tnew()\n"
                        :json-diff jdiff))
               (f-cont (make-omnidiff-file
                        :path "pkg.zip" :target-path "pkg.zip" :status 'modified
                        :container (make-omnidiff-json-container
                                    :family 'archives :unchanged 1
                                    :members (list (make-omnidiff-json-container-member
                                                    :key "inside.txt" :status 'changed)))))
               (diff (make-omnidiff-diff :files (list f-text f-cont))))
          (with-current-buffer buf
            (omnidiff-mode)
            (set-window-buffer (selected-window) buf)
            (set-frame-width (selected-frame) 120)
            (setq omnidiff--diff diff
                  omnidiff--layout 'side-by-side)
            (omnidiff-render-buffer diff)
            ;; Custom 2-col tab expansion applied.
            (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-search "  old()" txt))
              (should (string-search "  new()" txt))
              (should (string-search " BEFORE " txt))
              (should (string-search " AFTER  " txt)))
            ;; File header, hunk header, and container member rows have mouse-1 keymaps.
            (goto-char (point-min))
            (should (eq (lookup-key (get-text-property (point) 'keymap) [mouse-1])
                        #'omnidiff-render--mouse-toggle-fold))
            (omnidiff-next-hunk)
            (should (eq (lookup-key (get-text-property (point) 'keymap) [mouse-1])
                        #'omnidiff-render--mouse-toggle-fold))
            (search-forward "inside.txt")
            (should (eq (lookup-key (get-text-property (match-beginning 0) 'keymap) [mouse-1])
                        #'omnidiff-render--mouse-container-jump))
            ;; Fold first file, resize window narrower, trigger reflow, and verify fold is preserved!
            (goto-char (point-min))
            (omnidiff-toggle-fold)
            (should (overlay-get (get-text-property (point) 'omnidiff-body-ov) 'invisible))
            (set-frame-width (selected-frame) 90)
            (omnidiff--reflow-sbs-buffer buf)
            (should (= omnidiff--last-sbs-width (window-body-width (selected-window))))
            (goto-char (point-min))
            (should (overlay-get (get-text-property (point) 'omnidiff-body-ov) 'invisible))))
      (set-frame-width (selected-frame) orig-width)
      (when (buffer-live-p buf) (kill-buffer buf))))
  ;; 9. Image card: DEL outside blink peek scrolls cleanly without error; h/l on 2-up doesn't hijack to swipe.
  (when (and (image-type-available-p 'png) (image-type-available-p 'svg))
    (let ((buf (generate-new-buffer " *omnidiff-ergo-img*"))
          (dir (make-temp-file "omnidiff-ergo-img-" t)))
      (unwind-protect
          (let ((fa (expand-file-name "before.png" dir))
                (fb (expand-file-name "after.png" dir)))
            (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
            (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
            (let* ((pic (make-omnidiff-picture
                         :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                         :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                         :comparison 'pixels :changed-pct 100.0 :changed-pixels 4 :total-pixels 4))
                   (file (omnidiff-image-make-file-from-paths fa fb pic "icon.png"))
                   (diff (make-omnidiff-diff :files (list file))))
              (with-current-buffer buf
                (omnidiff-mode)
                (setq omnidiff--diff diff
                      omnidiff--layout 'side-by-side)
                (cl-letf (((symbol-function 'omnidiff-image--display-graphic-p) (lambda () t))
                          ((symbol-function 'omnidiff-image--viewport-px) (lambda () '(1000 . 600))))
                  (omnidiff-render-buffer diff)
                  (goto-char (point-min))
                  ;; File header uses image-header-map + mouse fold (no horizontal shadow).
                  (let ((hmap (get-text-property (point) 'keymap)))
                    (should (eq (lookup-key hmap [mouse-1]) #'omnidiff-render--mouse-toggle-fold))
                    (should-not (lookup-key hmap (kbd "<left>"))))
                  ;; On 2-up mode, adjust-increase does NOT hijack into swipe!
                  (omnidiff-image-adjust-increase)
                  (let ((st (omnidiff-image--state-for file)))
                    (should-not (eq (omnidiff-image--state-mode st) 'swipe))
                    (should-not (omnidiff-image--state-mode-pinned st)))
                  ;; DEL when not in blink peek delegates to scroll-down-command without error.
                  (let ((scrolled nil))
                    (cl-letf (((symbol-function 'scroll-down-command)
                               (lambda (&optional _arg) (interactive) (setq scrolled t))))
                      (omnidiff-image-return-from-peek)
                      (should scrolled)))))))
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-directory dir t))))
  ;; 10. Dual header-line: unified face-remap attached & cleaned up, active/inactive faces,
  ;;     single-pane AFTER preserves minimap, and clickable minimap + fold banners.
  (let* ((dir (make-temp-file "omnidiff-ergo-dual-" t))
         (fa (expand-file-name "before.txt" dir))
         (fb (expand-file-name "after.txt" dir))
         (orig-width (frame-width (selected-frame)))
         (sess nil))
    (unwind-protect
        (let ((omnidiff-dual-layout 'auto)
              (omnidiff-dual-min-width 100)
              (omnidiff-dual-fold-unchanged t)
              (omnidiff-dual-context-lines 1))
          (set-frame-width (selected-frame) 140)
          (with-temp-file fa
            (insert "old-head\n" (mapconcat (lambda (i) (format "same-%d" i)) (number-sequence 1 12) "\n") "\n"))
          (with-temp-file fb
            (insert "new-head\n" (mapconcat (lambda (i) (format "same-%d" i)) (number-sequence 1 12) "\n") "\n"))
          (let* ((jdiff (make-omnidiff-json-diff
                         :before (make-omnidiff-json-side
                                  :path fa :language "Text"
                                  :hunks (list (make-omnidiff-json-hunk
                                                :operation 'update
                                                :range (make-omnidiff-json-range
                                                        :start-row 0 :start-col 0
                                                        :end-row 0 :end-col 8))))
                         :after (make-omnidiff-json-side
                                 :path fb :language "Text"
                                 :hunks (list (make-omnidiff-json-hunk
                                               :operation 'update
                                               :range (make-omnidiff-json-range
                                                       :start-row 0 :start-col 0
                                                       :end-row 0 :end-col 8))))
                         :summary 'modified))
                 (user-after (find-file-noselect fb)))
            (unwind-protect
                (progn
                  (setq sess (omnidiff-dual-open
                              :before-path fa :after-path fb
                              :after-buffer user-after
                              :before-label "src/deep/nested/dir/before.txt"
                              :after-label "src/deep/nested/dir/after.txt"
                              :json-diff jdiff))
                  (let ((bb (omnidiff-dual--session-before-buf sess))
                        (ab (omnidiff-dual--session-after-buf sess)))
                    ;; Header-line face-remap is active on both panes.
                    (should (omnidiff-dual--session-before-hl-remap-cookie sess))
                    (should (omnidiff-dual--session-after-hl-remap-cookie sess))
                    ;; Active/inactive faces reflect focused pane.
                    (omnidiff-dual--focus-side sess 'after)
                    (let ((bh (with-current-buffer bb (omnidiff-dual--header-line)))
                          (ah (with-current-buffer ab (omnidiff-dual--header-line))))
                      (should (eq (get-text-property 0 'face bh) 'omnidiff-dual-header-before-inactive))
                      (should (eq (get-text-property 0 'face ah) 'omnidiff-dual-header-after))
                      (should (string-prefix-p " AFTER  " ah)))
                    ;; Fold banner has mouse-1 click keymap.
                    (with-current-buffer ab
                      (let ((fold-ov (cl-find-if
                                      (lambda (o)
                                        (and (overlay-get o 'omnidiff-dual-ov)
                                             (eq (overlay-get o 'omnidiff-dual-kind) 'fold)))
                                      (overlays-in (point-min) (point-max)))))
                        (should fold-ov)
                        (let ((bs (overlay-get fold-ov 'before-string)))
                          (should (eq (lookup-key (get-text-property 0 'keymap bs) [mouse-1])
                                      #'omnidiff-dual--mouse-expand-fold)))))
                    ;; Minimap is clickable and stays visible on AFTER in single layout.
                    (omnidiff-dual-cycle-layout) ; dual -> single
                    (omnidiff-dual--focus-side sess 'after)
                    (let ((mm (omnidiff-dual--render-minimap sess)))
                      (should mm)
                      (should (eq (lookup-key (get-text-property 0 'keymap mm) [header-line mouse-1])
                                  #'omnidiff-dual--minimap-click)))
                    ;; Teardown removes header-line face-remap cleanly from user buffer.
                    (with-current-buffer ab
                      (omnidiff-dual-quit))
                    (setq sess nil)
                    (with-current-buffer user-after
                      (should-not (assq 'header-line face-remapping-alist)))))
              (when (and sess (memq sess omnidiff-dual--sessions))
                (omnidiff-dual--teardown sess nil t))
              (when (buffer-live-p user-after)
                (kill-buffer user-after)))))
      (set-frame-width (selected-frame) orig-width)
      (delete-directory dir t))))

(ert-deftest omnidiff-test/json-render-plan-cache-and-lazy-folding ()
  "Overview caches `omnidiff-json-render-plan' across layout/style cycles and lazily materializes folded cards."
  (skip-unless (executable-find omnidiff-executable))
  (let ((dir (make-temp-file "omnidiff-phase1-lazy-" t))
        (buf (generate-new-buffer " *omnidiff-phase1-lazy*")))
    (unwind-protect
        (let ((a1 (expand-file-name "code-before.py" dir))
              (b1 (expand-file-name "code-after.py" dir))
              (a2 (expand-file-name "ws-before.py" dir))
              (b2 (expand-file-name "ws-after.py" dir)))
          (with-temp-file a1 (insert "def greet(name):\n    return name\n"))
          (with-temp-file b1 (insert "def greet(user):\n    return user\n"))
          (with-temp-file a2 (insert "def noop():\n    return 1\n"))
          (with-temp-file b2 (insert "def noop():\n        return 1\n"))
          (let* ((jd1 (omnidiff-engine-run-pair a1 b1 'default))
                 (jd2 (omnidiff-engine-run-pair a2 b2 'default))
                 (f1 (omnidiff--file-from-json "code.py" a1 b1 "code.py" jd1))
                 (f2 (omnidiff--file-from-json "ws.py" a2 b2 "ws.py" jd2))
                 (diff (make-omnidiff-diff :files (list f1 f2)))
                 (orig-build (symbol-function 'omnidiff-render--build-json-plan))
                 (built-paths nil))
            (should (eq (omnidiff-file-summary f2) 'whitespace-only))
            (cl-letf (((symbol-function 'omnidiff-render--build-json-plan)
                       (lambda (file jd)
                         (push (omnidiff-file-target-path file) built-paths)
                         (funcall orig-build file jd))))
              (with-current-buffer buf
                (omnidiff-mode)
                (setq omnidiff--diff diff
                      omnidiff--layout 'unified
                      omnidiff--highlight-style 'token
                      omnidiff--repo-dir dir)
                ;; 1. Auto-folded trivial file skips plan construction on initial render.
                (let ((omnidiff-overview-auto-fold-trivial 'whitespace-and-comments))
                  (omnidiff-render-buffer diff)
                  (should (equal built-paths '("code.py")))
                  (goto-char (point-min))
                  (search-forward "ws.py")
                  (let ((ws-ov (get-text-property (line-beginning-position) 'omnidiff-body-ov)))
                    (should (eq (overlay-get ws-ov 'invisible) 'omnidiff-fold))
                    (should (eq (overlay-get ws-ov 'omnidiff-lazy-file) f2))
                    ;; 2. Cycling layouts and highlight styles hits cache and keeps ws.py lazy.
                    (omnidiff-cycle-layout) ; -> side-by-side
                    (omnidiff-cycle-layout) ; -> stacked
                    (omnidiff-cycle-layout) ; -> unified
                    (omnidiff-cycle-highlight-style) ; -> subtle-line
                    (omnidiff-cycle-highlight-style) ; -> traditional
                    (omnidiff-cycle-highlight-style) ; -> token
                    (should (equal built-paths '("code.py")))
                    ;; 3. Switch to side-by-side while ws.py is still folded, then expand it on demand.
                    (omnidiff-cycle-layout) ; -> side-by-side
                    (goto-char (point-min))
                    (search-forward "ws.py")
                    (beginning-of-line)
                    (let ((ws-ov2 (get-text-property (point) 'omnidiff-body-ov)))
                      (should (eq (overlay-get ws-ov2 'invisible) 'omnidiff-fold))
                      (should (eq (overlay-get ws-ov2 'omnidiff-lazy-file) f2))
                      (omnidiff-toggle-fold)
                      (should-not (overlay-get ws-ov2 'invisible))
                      (should-not (overlay-get ws-ov2 'omnidiff-lazy-file))
                      (should (equal (length built-paths) 2))
                      (should (equal (car built-paths) "ws.py"))
                      (let ((body-txt (buffer-substring-no-properties
                                       (overlay-start ws-ov2) (overlay-end ws-ov2))))
                        (should (string-search "return 1" body-txt))))
                    ;; 4. Re-folding and cycling layout reuses cached plan on next unfold.
                    (omnidiff-toggle-fold)
                    (omnidiff-cycle-layout) ; -> stacked
                    (goto-char (point-min))
                    (search-forward "ws.py")
                    (beginning-of-line)
                    (omnidiff-toggle-fold)
                    (should (equal (length built-paths) 2))))
                ;; 5. Changing context-lines invalidates the cached plan and rebuilds.
                (let ((omnidiff-overview-context-lines 1)
                      (omnidiff-overview-auto-fold-trivial nil))
                  (omnidiff-render-buffer diff)
                  (should (equal (length built-paths) 4)))))))
      (when (buffer-live-p buf) (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest omnidiff-test/batched-git-cat-file-preserves-parallel-json ()
  "Multi-file diffs exceeding `omnidiff-max-parallel-blob-bytes' stream in batches without falling back to serial."
  ;; 1. Pure batch-check parser and partitioner unit checks.
  (let* ((s1 (make-string 40 ?1))
         (s2 (make-string 40 ?2))
         (s3 (make-string 40 ?3))
         (s4 (make-string 40 ?4))
         (raw (format "%s blob 200\n%s blob 200\n%s blob 200\n%s missing\n"
                      s1 s2 s3 s4))
         (sizes (omnidiff--parse-batch-check-sizes raw)))
    (should (= (gethash s1 sizes) 200))
    (should (= (gethash s4 sizes) 0))
    (should (equal (omnidiff--partition-sha-batches (list s1 s2 s3 s4) sizes 450)
                   (list (list s1 s2) (list s3 s4))))
    (should (equal (omnidiff--partition-sha-batches (list s1 s2 s3) sizes 200)
                   (list (list s1) (list s2) (list s3))))
    (should (eq (omnidiff--partition-sha-batches (list s1 s2) sizes 150) 'overflow))
    (should (equal (omnidiff--partition-sha-batches (list s1 s2 s3) sizes 0)
                   (list (list s1 s2 s3)))))
  ;; 2. Real Git repo: 6 files x ~220B (> 500B budget) runs across batches in parallel JSON mode.
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (let* ((repo (make-temp-file "omnidiff-batched-cat-" t))
         (default-directory repo))
    (cl-flet ((git (&rest args)
                (unless (zerop (apply #'call-process "git" nil nil nil args))
                  (error "git %S failed" args))))
      (unwind-protect
          (progn
            (git "init" "-q" "-b" "main")
            (git "config" "user.name" "Test")
            (git "config" "user.email" "test@example.com")
            (dotimes (i 6)
              (with-temp-file (expand-file-name (format "f%d.txt" i) repo)
                (insert (format "header-%d\n%s\n" i (make-string 200 (+ ?a i))))))
            (git "add" ".")
            (git "commit" "-q" "-m" "v1")
            (dotimes (i 6)
              (with-temp-file (expand-file-name (format "f%d.txt" i) repo)
                (insert (format "header-%d\n%s\n" i (make-string 200 (+ ?A i))))))
            (git "commit" "-q" "-am" "v2")
            (let ((buf-batched (generate-new-buffer " *omnidiff-batched*"))
                  (buf-full (generate-new-buffer " *omnidiff-unbatched*"))
                  (batch-calls 0)
                  (orig-mat (symbol-function 'omnidiff--materialize-cat-batch)))
              (unwind-protect
                  (progn
                    (cl-letf (((symbol-function 'omnidiff--materialize-cat-batch)
                               (lambda (&rest args)
                                 (setq batch-calls (1+ batch-calls))
                                 (apply orig-mat args))))
                      (let ((omnidiff-max-parallel 4)
                            (omnidiff-max-parallel-blob-bytes 500))
                        (omnidiff-run buf-batched '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                      "batched" repo 'default 'unified)
                        (omnidiff-test--wait-request buf-batched)))
                    (let ((omnidiff-max-parallel 4)
                          (omnidiff-max-parallel-blob-bytes (* 32 1024 1024)))
                      (omnidiff-run buf-full '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                    "full" repo 'default 'unified)
                      (omnidiff-test--wait-request buf-full))
                    ;; Confirmed multiple batches executed AND stayed in parallel JSON mode.
                    (should (> batch-calls 1))
                    (with-current-buffer buf-batched
                      (should omnidiff--diff)
                      (should (= (length (omnidiff-diff-files omnidiff--diff)) 6))
                      (dolist (f (omnidiff-diff-files omnidiff--diff))
                        (should (omnidiff-file-json-diff f))))
                    (should (equal (with-current-buffer buf-batched
                                     (buffer-substring-no-properties (point-min) (point-max)))
                                   (with-current-buffer buf-full
                                     (buffer-substring-no-properties (point-min) (point-max))))))
                (when (buffer-live-p buf-batched) (kill-buffer buf-batched))
                (when (buffer-live-p buf-full) (kill-buffer buf-full)))))
        (delete-directory repo t)))))

(ert-deftest omnidiff-test/serial-json-parity-and-multi-commit-show ()
  "Serial `GIT_EXTERNAL_DIFF' runs `--mode json' with full parity and multi-commit `git show' preambles."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (let* ((repo (make-temp-file "omnidiff-serial-json-" t))
         (default-directory repo))
    (cl-flet ((git (&rest args)
                (unless (zerop (apply #'call-process "git" nil nil nil args))
                  (error "git %S failed" args)))
              (run-one (cmd label max-par &optional budget)
                (let ((buf (generate-new-buffer (format " *omnidiff-sjson-%s-%d*" label max-par))))
                  (unwind-protect
                      (let ((omnidiff-max-parallel max-par)
                            (omnidiff-max-parallel-blob-bytes
                             (or budget omnidiff-max-parallel-blob-bytes)))
                        (omnidiff-run buf cmd label repo 'default 'unified)
                        (omnidiff-test--wait-request buf)
                        (with-current-buffer buf
                          (list (buffer-substring-no-properties (point-min) (point-max))
                                (mapcar (lambda (f)
                                          (list (omnidiff-file-target-path f)
                                                (not (null (omnidiff-file-json-diff f)))
                                                (omnidiff-file-status f)))
                                        (omnidiff-diff-files omnidiff--diff)))))
                    (when (buffer-live-p buf) (kill-buffer buf))))))
      (unwind-protect
          (progn
            (git "init" "-q" "-b" "main")
            (git "config" "user.name" "Test")
            (git "config" "user.email" "test@example.com")
            (with-temp-file (expand-file-name "alpha.py" repo)
              (insert "def f():\n    return 1\n"))
            (with-temp-file (expand-file-name "old.txt" repo)
              (insert "rename me\n"))
            (git "add" ".")
            (git "commit" "-q" "-m" "commit-one")
            ;; Commit 2: modify alpha.py + rename old.txt -> renamed.txt.
            (with-temp-file (expand-file-name "alpha.py" repo)
              (insert "def f():\n    return 2\n"))
            (git "add" "alpha.py")
            (git "mv" "old.txt" "renamed.txt")
            (git "commit" "-q" "-m" "commit-two-alpha")
            ;; Commit 3: add beta.py.
            (with-temp-file (expand-file-name "beta.py" repo)
              (insert "def g():\n    return 3\n"))
            (git "add" "beta.py")
            (git "commit" "-q" "-m" "commit-three-beta")
            ;; Unstaged worktree modification.
            (with-temp-file (expand-file-name "alpha.py" repo)
              (insert "def f():\n    return 99\n"))
            ;; 1. Serial JSON (`max-parallel=1') matches parallel JSON (`max-parallel=4').
            (let ((p-show (run-one '("git" "--no-pager" "show" "--ext-diff" "HEAD~1") "show" 4))
                  (s-show (run-one '("git" "--no-pager" "show" "--ext-diff" "HEAD~1") "show" 1))
                  (p-wt (run-one '("git" "--no-pager" "diff" "--ext-diff") "wt" 4))
                  (s-wt (run-one '("git" "--no-pager" "diff" "--ext-diff") "wt" 1)))
              (should (equal (car s-show) (car p-show)))
              (should (equal (cadr s-show) (cadr p-show)))
              (should (cl-every #'cadr (cadr s-show)))
              (should (equal (car s-wt) (car p-wt)))
              (should (equal (cadr s-wt) (cadr p-wt)))
              (should (cl-every #'cadr (cadr s-wt)))
              (should (string-search "99" (car s-wt))))
            ;; 2. Single-blob overflow falls back to serial JSON (never downgrades to headless).
            (let ((ov-res (run-one '("git" "--no-pager" "show" "--ext-diff" "HEAD~1") "ov" 4 8)))
              (should (cl-every #'cadr (cadr ov-res)))
              (should (string-search "return 2" (car ov-res))))
            ;; 3. Multi-commit `git show' preserves both commit preambles and JSON diffs.
            (let ((multi (run-one '("git" "--no-pager" "show" "--ext-diff" "HEAD~2..HEAD") "multi" 4)))
              (should (string-search "commit-three-beta" (car multi)))
              (should (string-search "commit-two-alpha" (car multi)))
              (should (string-search "return 3" (car multi)))
              (should (string-search "return 2" (car multi)))
              (should (= (length (cadr multi)) 3))
              (should (cl-every #'cadr (cadr multi)))))
        (delete-directory repo t)))))

(ert-deftest omnidiff-test/dual-semantic-revert-and-undo ()
  "Dual viewer reverts semantic stops atomically with undo, refresh, and move guard."
  (skip-unless (executable-find omnidiff-executable))
  (should (eq (keymap-lookup omnidiff-dual-mode-map "r") #'omnidiff-dual-revert-change))
  (should (eq (keymap-lookup omnidiff-dual-mode-map "u") #'omnidiff-dual-undo))
  (should (eq (keymap-lookup omnidiff-dual-edit-mode-map "C-c r") #'omnidiff-dual-revert-change))
  (let ((wait-dual
         (lambda (session)
           (let ((deadline (+ (float-time) 5.0)))
             (while (and (memq session omnidiff-dual--sessions)
                         (let ((p (omnidiff-dual--session-active-proc session)))
                           (and p (process-live-p p))))
               (when (> (float-time) deadline)
                 (ert-fail "Timed out waiting for dual async refresh"))
               (accept-process-output nil 0.02))))))
    ;; 1. Exhaustive stop-by-stop revert parity across real CLI edit patterns.
    (let ((dir (make-temp-file "omnidiff-dual-revert-" t)))
      (unwind-protect
          (dolist (spec
                   '(("inline_upd" "rs" "let x = 1;\n" "let x = 2;\n")
                     ("inline_ins" "rs" "foo(a);\n" "foo(a, b);\n")
                     ("inline_del" "rs" "foo(a, b);\n" "foo(a);\n")
                     ("inline_upd_del" "rs" "call(alpha, beta, gamma);\n" "call(ALPHA, gamma);\n")
                     ("elisp_arg" "el" "(defun f (a)\n  (+ a 1))\n" "(defun f (a b)\n  (+ a b 1))\n")
                     ("line_ins_mid" "rs" "a\nc\n" "a\nb1\nb2\nc\n")
                     ("line_ins_eof_nl" "rs" "a\nb\n" "a\nb\nc\n")
                     ("line_ins_eof_nonl" "rs" "a\nb" "a\nb\nc")
                     ("line_del_top" "rs" "x\na\nb\n" "a\nb\n")
                     ("line_del_mid" "rs" "a\nb1\nb2\nc\n" "a\nc\n")
                     ("line_del_eof_nl" "rs" "a\nb\nc\n" "a\nb\n")
                     ("line_del_eof_nonl" "rs" "a\nb\nc" "a\nb")
                     ("split_call" "rs" "fn f() {\n    call(a);\n}\n" "fn f() {\n    call(a,\n         b);\n}\n")
                     ("join_call" "rs" "foo(a,\n    b);\n" "foo(a);\n")
                     ("new_file" "rs" "" "line1\nline2\n")
                     ("deleted_file" "rs" "line1\nline2\n" "")))
            (pcase-let* ((`(,name ,ext ,before-txt ,after-txt) spec)
                         (bp (expand-file-name (format "%s-b.%s" name ext) dir))
                         (ap (expand-file-name (format "%s-a.%s" name ext) dir))
                         (abuf nil)
                         (session nil))
              (with-temp-file bp (insert before-txt))
              (with-temp-file ap (insert after-txt))
              (unwind-protect
                  (progn
                    (setq abuf (find-file-noselect ap)
                          session (omnidiff-dual-open
                                   :before-path bp
                                   :after-path ap
                                   :after-buffer abuf
                                   :display-path (format "%s.%s" name ext)
                                   :editable-after t))
                    (should (omnidiff-dual--session-stops session))
                    (let ((guard 0))
                      (while (and (omnidiff-dual--session-stops session) (< guard 10))
                        (setq guard (1+ guard))
                        (omnidiff-dual--goto-stop
                         session (car (omnidiff-dual--session-stops session)) nil)
                        (let ((cur-buf (omnidiff-dual--side-buf
                                        session (omnidiff-dual--session-active-side session))))
                          (with-current-buffer cur-buf
                            (omnidiff-dual-revert-change)))
                        (funcall wait-dual session)))
                    (should-not (omnidiff-dual--session-stops session))
                    (with-current-buffer abuf
                      (should (equal (buffer-string) before-txt))
                      (should (buffer-modified-p))
                      ;; Disk file was NOT auto-saved.
                      (should (equal (with-temp-buffer
                                       (insert-file-contents ap)
                                       (buffer-string))
                                     after-txt))))
                (when (and session (memq session omnidiff-dual--sessions))
                  (omnidiff-dual--teardown session nil nil))
                (when (buffer-live-p abuf)
                  (with-current-buffer abuf (set-buffer-modified-p nil))
                  (kill-buffer abuf)))))
        (delete-directory dir t)))
    ;; 2. Consecutive `u' undo (including from BEFORE pane), live edit `C-c r', stale guard,
    ;;    read-only guard, and `move' refusal.
    (let* ((dir (make-temp-file "omnidiff-dual-revert-ux-" t))
           (bp (expand-file-name "demo.rs" dir))
           (ap (expand-file-name "demo-after.rs" dir))
           (before-txt "fn alpha() {\n    let a = 1;\n    removed_call();\n}\n\nfn beta() {\n    let b = 2;\n}\n")
           (after-txt "fn alpha() {\n    let a = 10;\n}\n\nfn beta() {\n    let b = 20;\n}\n")
           (abuf nil)
           (session nil)
           (ro-session nil)
           (move-session nil))
      (unwind-protect
          (progn
            (with-temp-file bp (insert before-txt))
            (with-temp-file ap (insert after-txt))
            (setq abuf (find-file-noselect ap)
                  session (omnidiff-dual-open
                           :before-path bp
                           :after-path ap
                           :after-buffer abuf
                           :display-path "demo.rs"
                           :editable-after t))
            (let ((init-stops (length (omnidiff-dual--session-stops session))))
              (should (>= init-stops 3))
              ;; Header-line shows r/u hint for editable AFTER.
              (with-current-buffer abuf
                (should (string-search "r/u" (substring-no-properties (omnidiff-dual--header-line)))))
              ;; Revert stop #1 (update `let a = 10;' -> `let a = 1;') from AFTER pane.
              (omnidiff-dual--goto-stop session (car (omnidiff-dual--session-stops session)) nil)
              (with-current-buffer (omnidiff-dual--side-buf
                                    session (omnidiff-dual--session-active-side session))
                (omnidiff-dual-revert-change))
              (funcall wait-dual session)
              (let ((after-first (with-current-buffer abuf (buffer-string))))
                (should (string-search "let a = 1;" after-first))
                (should (string-search "let b = 20;" after-first))
                ;; Revert stop #1 now (pure delete `removed_call();') from BEFORE pane.
                (omnidiff-dual--goto-stop session (car (omnidiff-dual--session-stops session)) nil)
                (should (eq (omnidiff-dual--session-active-side session) 'before))
                (with-current-buffer (omnidiff-dual--session-before-buf session)
                  (omnidiff-dual-revert-change))
                (funcall wait-dual session)
                (should (string-search "removed_call();" (with-current-buffer abuf (buffer-string))))
                ;; Press `u' twice consecutively from BEFORE pane -> undoes 2nd, then 1st revert!
                (with-current-buffer (omnidiff-dual--session-before-buf session)
                  (let ((last-command 'omnidiff-dual-revert-change))
                    (omnidiff-dual-undo))
                  (funcall wait-dual session)
                  (should (equal (with-current-buffer abuf (buffer-string)) after-first))
                  (let ((last-command 'omnidiff-dual-undo))
                    (omnidiff-dual-undo))
                  (funcall wait-dual session)
                  (should (equal (with-current-buffer abuf (buffer-string)) after-txt)))
                (should (= (length (omnidiff-dual--session-stops session)) init-stops))
                ;; Live edit mode: `C-c r' works, and stale edit without refresh is refused.
                (with-current-buffer abuf
                  (omnidiff-dual-toggle-edit)
                  (should (omnidiff-dual--session-editing-p session))
                  (omnidiff-dual--goto-stop session (car (omnidiff-dual--session-stops session)) nil)
                  (omnidiff-dual-revert-change))
                (funcall wait-dual session)
                (should (string-search "let a = 1;" (with-current-buffer abuf (buffer-string))))
                (with-current-buffer abuf
                  (goto-char (point-min))
                  (insert "// unsaved stale edit\n")
                  (let ((err (should-error (omnidiff-dual-revert-change) :type 'user-error)))
                    (should (string-search "out of date" (error-message-string err)))))))
            (omnidiff-dual--teardown session nil nil)
            (setq session nil)
            ;; Read-only AFTER session refuses `r' and `u' and omits `r/u' from header hint.
            (setq ro-session (omnidiff-dual-open
                              :before-source "let x = 1;\n"
                              :after-source "let x = 2;\n"
                              :display-path "ro.rs"
                              :editable-after nil))
            (with-current-buffer (omnidiff-dual--session-after-buf ro-session)
              (should-not (string-search "r/u" (substring-no-properties (omnidiff-dual--header-line))))
              (should-error (omnidiff-dual-revert-change) :type 'user-error)
              (should-error (omnidiff-dual-undo) :type 'user-error))
            (omnidiff-dual--teardown ro-session nil nil)
            (setq ro-session nil)
            ;; Move stop refuses automatic revert without modifying AFTER buffer.
            (let* ((mb (expand-file-name "move-b.rs" dir))
                   (ma (expand-file-name "move-a.rs" dir))
                   (mb-txt "fn first() {\n    let x = 1;\n    let y = 2;\n    x + y\n}\n\nfn second() {\n    let p = 3;\n    let q = 4;\n    p * q\n}\n")
                   (ma-txt "fn second() {\n    let p = 3;\n    let q = 4;\n    p * q\n}\n\nfn first() {\n    let x = 1;\n    let y = 2;\n    x + y\n}\n")
                   (mabuf nil))
              (with-temp-file mb (insert mb-txt))
              (with-temp-file ma (insert ma-txt))
              (unwind-protect
                  (progn
                    (setq mabuf (find-file-noselect ma)
                          move-session (omnidiff-dual-open
                                        :before-path mb
                                        :after-path ma
                                        :after-buffer mabuf
                                        :display-path "move.rs"
                                        :editable-after t))
                    (let ((move-stop (cl-find-if
                                      (lambda (st) (eq (omnidiff-change-stop-operation st) 'move))
                                      (omnidiff-dual--session-stops move-session))))
                      (should move-stop)
                      (omnidiff-dual--goto-stop move-session move-stop nil)
                      (let ((cur-buf (omnidiff-dual--side-buf
                                      move-session
                                      (omnidiff-dual--session-active-side move-session))))
                        (with-current-buffer cur-buf
                          (let ((err (should-error (omnidiff-dual-revert-change) :type 'user-error)))
                            (should (string-search "moved block" (error-message-string err))))))
                      (with-current-buffer mabuf
                        (should (equal (buffer-string) ma-txt))
                        (should-not (buffer-modified-p)))))
                (when (and move-session (memq move-session omnidiff-dual--sessions))
                  (omnidiff-dual--teardown move-session nil nil))
                (when (buffer-live-p mabuf)
                  (with-current-buffer mabuf (set-buffer-modified-p nil))
                  (kill-buffer mabuf)))))
        (when (and session (memq session omnidiff-dual--sessions))
          (omnidiff-dual--teardown session nil nil))
        (when (and ro-session (memq ro-session omnidiff-dual--sessions))
          (omnidiff-dual--teardown ro-session nil nil))
        (when (buffer-live-p abuf)
          (with-current-buffer abuf (set-buffer-modified-p nil))
          (kill-buffer abuf))
        (delete-directory dir t)))))

(ert-deftest omnidiff-test/multi-file-review-workflow-and-oid-invalidation ()
  "Multi-file reviewed state syncs between Overview and Dual and invalidates on change."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (should (eq (keymap-lookup omnidiff-mode-map "R") #'omnidiff-toggle-reviewed))
  (should (eq (keymap-lookup omnidiff-dual-mode-map "R") #'omnidiff-dual-toggle-reviewed))
  (let* ((repo (make-temp-file "omnidiff-review-repo-" t))
         (default-directory repo)
         (buf (generate-new-buffer " *omnidiff-review-ov*"))
         (dual-sess nil)
         (standalone-sess nil))
    (cl-flet ((git (&rest args)
                (unless (zerop (apply #'call-process "git" nil nil nil args))
                  (error "git %S failed" args))))
      (unwind-protect
          (progn
            (git "init" "-q" "-b" "main")
            (git "config" "user.name" "Test")
            (git "config" "user.email" "test@example.com")
            (with-temp-file (expand-file-name "a.rs" repo) (insert "fn a() { 1 }\n"))
            (with-temp-file (expand-file-name "b.rs" repo) (insert "fn b() { 1 }\n"))
            (with-temp-file (expand-file-name "c.rs" repo) (insert "fn c() { 1 }\n"))
            (git "add" ".")
            (git "commit" "-q" "-m" "base")
            ;; Unstaged worktree edits on all 3 files.
            (with-temp-file (expand-file-name "a.rs" repo) (insert "fn a() { 2 }\n"))
            (with-temp-file (expand-file-name "b.rs" repo) (insert "fn b() { 2 }\n"))
            (with-temp-file (expand-file-name "c.rs" repo) (insert "fn c() { 2 }\n"))
            (omnidiff-run buf '("git" "--no-pager" "diff" "--ext-diff")
                          "unstaged" repo 'default 'unified)
            (omnidiff-test--wait-request buf)
            (with-current-buffer buf
              (should (= omnidiff--file-count 3))
              (should (= (omnidiff-render-reviewed-count) 0))
              (should-not (string-search "reviewed" (substring-no-properties (omnidiff-render-header-line))))
              ;; 1. Overview `R': mark a.rs reviewed -> shows ✓, folds a.rs, jumps to b.rs.
              (let ((pos-a (omnidiff--find-file-header-pos "a.rs"))
                    (pos-b (omnidiff--find-file-header-pos "b.rs")))
                (should pos-a)
                (should pos-b)
                (goto-char pos-a)
                (should (eq (omnidiff-toggle-reviewed) t))
                (should (omnidiff--file-reviewed-p "a.rs"))
                (should (= (omnidiff-render-reviewed-count) 1))
                (should (equal (substring-no-properties
                                (overlay-get (get-text-property pos-a 'omnidiff-review-ov) 'before-string))
                               " ✓"))
                (should (eq (overlay-get (get-text-property pos-a 'omnidiff-body-ov) 'invisible)
                            'omnidiff-fold))
                (should (= (point) pos-b))
                (should (string-search "reviewed 1/3"
                                       (substring-no-properties (omnidiff-render-header-line))))
                ;; Layout cycle preserves reviewed ✓, fold state, and header progress.
                (omnidiff-cycle-layout)
                (omnidiff-cycle-layout)
                (omnidiff-cycle-layout)
                (should (omnidiff--file-reviewed-p "a.rs"))
                (let ((pos-a2 (omnidiff--find-file-header-pos "a.rs")))
                  (should (equal (substring-no-properties
                                  (overlay-get (get-text-property pos-a2 'omnidiff-review-ov) 'before-string))
                                 " ✓"))
                  (should (eq (overlay-get (get-text-property pos-a2 'omnidiff-body-ov) 'invisible)
                              'omnidiff-fold)))
                ;; Unmark and re-mark a.rs cleanly.
                (goto-char (omnidiff--find-file-header-pos "a.rs"))
                (should-not (omnidiff-toggle-reviewed))
                (should-not (omnidiff--file-reviewed-p "a.rs"))
                (should-not (overlay-get (get-text-property (omnidiff--find-file-header-pos "a.rs")
                                                            'omnidiff-body-ov)
                                         'invisible))
                (should-not (string-search "reviewed"
                                           (substring-no-properties (omnidiff-render-header-line))))
                (should (eq (omnidiff-toggle-reviewed) t)))
              ;; 2. Bidirectional sync with Dual viewer: open Dual on b.rs.
              (goto-char (omnidiff--find-file-header-pos "b.rs"))
              (setq dual-sess (omnidiff-open-dual-at-point))
              (should (equal (omnidiff-dual--session-display-path dual-sess) "b.rs")))
            ;; Dual header reflects Overview progress (1/3) and unreviewed b.rs.
            (with-current-buffer (omnidiff-dual--session-after-buf dual-sess)
              (let ((hl (substring-no-properties (omnidiff-dual--header-line))))
                (should (string-search "file 2/3 · reviewed 1/3" hl))
                (should-not (string-search " ✓ " hl)))
              ;; Press `R' in Dual -> marks b.rs reviewed in Overview and jumps Dual to c.rs!
              (should (eq (omnidiff-dual-toggle-reviewed) t)))
            (should (equal (omnidiff-dual--session-display-path dual-sess) "c.rs"))
            (with-current-buffer buf
              (should (omnidiff--file-reviewed-p "b.rs"))
              (should (= (omnidiff-render-reviewed-count) 2))
              (should (eq (overlay-get (get-text-property (omnidiff--find-file-header-pos "b.rs")
                                                          'omnidiff-body-ov)
                                       'invisible)
                          'omnidiff-fold)))
            ;; Press `R' on c.rs (last unreviewed) -> stays on c.rs with ✓ badge and 3/3 progress.
            (with-current-buffer (omnidiff-dual--session-after-buf dual-sess)
              (should (string-search "file 3/3 · reviewed 2/3"
                                     (substring-no-properties (omnidiff-dual--header-line))))
              (should (eq (omnidiff-dual-toggle-reviewed) t)))
            (should (equal (omnidiff-dual--session-display-path dual-sess) "c.rs"))
            (with-current-buffer (omnidiff-dual--session-after-buf dual-sess)
              (let ((hl (substring-no-properties (omnidiff-dual--header-line))))
                (should (string-search " ✓ " hl))
                (should (string-search "file 3/3 · reviewed 3/3" hl)))
              ;; Unmark c.rs from Dual -> removes ✓ badge and unfolds c.rs in Overview.
              (should-not (omnidiff-dual-toggle-reviewed))
              (let ((hl (substring-no-properties (omnidiff-dual--header-line))))
                (should-not (string-search " ✓ " hl))
                (should (string-search "file 3/3 · reviewed 2/3" hl))))
            (omnidiff-dual--teardown dual-sess nil nil)
            (setq dual-sess nil)
            ;; Standalone Dual without Overview refuses `R'.
            (setq standalone-sess (omnidiff-dual-open
                                   :before-source "a\n" :after-source "b\n"
                                   :display-path "solo.rs"))
            (with-current-buffer (omnidiff-dual--session-after-buf standalone-sess)
              (should-error (omnidiff-dual-toggle-reviewed) :type 'user-error))
            (omnidiff-dual--teardown standalone-sess nil nil)
            (setq standalone-sess nil)
            ;; 3. Worktree content invalidation on `g' refresh: edit b.rs, leave a.rs unchanged.
            (with-temp-file (expand-file-name "b.rs" repo) (insert "fn b() { 999 }\n"))
            (with-current-buffer buf
              (omnidiff-refresh))
            (omnidiff-test--wait-request buf)
            (with-current-buffer buf
              (should (omnidiff--file-reviewed-p "a.rs"))
              (should-not (omnidiff--file-reviewed-p "b.rs"))
              (should (= (omnidiff-render-reviewed-count) 1))
              (should (eq (overlay-get (get-text-property (omnidiff--find-file-header-pos "a.rs")
                                                          'omnidiff-body-ov)
                                       'invisible)
                          'omnidiff-fold))
              (should-not (overlay-get (get-text-property (omnidiff--find-file-header-pos "b.rs")
                                                          'omnidiff-body-ov)
                                       'invisible)))
            ;; 4. Committed OID invalidation on `git show HEAD' + amend + `g'.
            (git "add" ".")
            (git "commit" "-q" "-m" "commit-v1")
            (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                          "HEAD" repo 'default 'unified)
            (omnidiff-test--wait-request buf)
            (with-current-buffer buf
              ;; Switching to a new command cleared old worktree review state.
              (should (= (omnidiff-render-reviewed-count) 0))
              (omnidiff--set-file-reviewed "a.rs" t)
              (omnidiff--set-file-reviewed "b.rs" t)
              (should (= (omnidiff-render-reviewed-count) 2)))
            ;; Amend commit modifying only b.rs -> b.rs OID changes, a.rs OID unchanged.
            (with-temp-file (expand-file-name "b.rs" repo) (insert "fn b() { 2026 }\n"))
            (git "add" "b.rs")
            (git "commit" "-q" "--amend" "--no-edit")
            (with-current-buffer buf
              (omnidiff-refresh))
            (omnidiff-test--wait-request buf)
            (with-current-buffer buf
              (should (omnidiff--file-reviewed-p "a.rs"))
              (should-not (omnidiff--file-reviewed-p "b.rs"))
              (should (= (omnidiff-render-reviewed-count) 1))
              (should (string-search "reviewed 1/3"
                                     (substring-no-properties (omnidiff-render-header-line))))))
        (when (and dual-sess (memq dual-sess omnidiff-dual--sessions))
          (omnidiff-dual--teardown dual-sess nil nil))
        (when (and standalone-sess (memq standalone-sess omnidiff-dual--sessions))
          (omnidiff-dual--teardown standalone-sess nil nil))
        (when (buffer-live-p buf) (kill-buffer buf))
        (delete-directory repo t)))))

(ert-deftest omnidiff-test/dual-mode-emulation-keymap-precedence ()
  "Review keys in `omnidiff-dual-mode' take precedence over modal emulation maps.
Unbound keys fall through to the modal map, and disabling `omnidiff-dual-mode'
(e.g. entering live edit mode) immediately restores full modal bindings."
  (let* ((modal-map (let ((m (make-sparse-keymap)))
                      (define-key m (kbd "n") #'forward-char)
                      (define-key m (kbd "p") #'backward-char)
                      (define-key m (kbd "j") #'next-line)
                      m))
         (saved-emul emulation-mode-map-alists))
    (unwind-protect
        (progn
          (setq emulation-mode-map-alists
                (cons (list (cons 'omnidiff-test--modal-active modal-map))
                      emulation-mode-map-alists))
          (with-temp-buffer
            (setq-local omnidiff-test--modal-active t)
            ;; Before dual mode: modal emulation map owns `n', `p', and `j'.
            (should (eq (key-binding (kbd "n")) #'forward-char))
            (should (eq (key-binding (kbd "p")) #'backward-char))
            (should (eq (key-binding (kbd "j")) #'next-line))
            ;; In dual review mode: review keys win while unbound `j' falls through.
            (omnidiff-dual-mode 1)
            (should (eq (key-binding (kbd "n")) #'omnidiff-dual-next-change))
            (should (eq (key-binding (kbd "p")) #'omnidiff-dual-prev-change))
            (should (eq (key-binding (kbd "j")) #'next-line))
            ;; Leaving dual review mode restores full modal precedence immediately.
            (omnidiff-dual-mode -1)
            (should (eq (key-binding (kbd "n")) #'forward-char))
            (should (eq (key-binding (kbd "p")) #'backward-char))))
      (setq emulation-mode-map-alists saved-emul))))

(ert-deftest omnidiff-test/overview-auto-fold-threshold-ret-dual-and-magit-workflow ()
  "Test multi-file auto-fold checklist, RET on text header, stash/untracked,
Magit file+line Dual entry with multi-file context, and Magit refresh on quit."
  (let ((repo (make-temp-file "omnidiff-magit-wf-" t))
        (buf nil)
        (dual-sess nil)
        (untracked-sess nil)
        (stash-buf nil))
    (cl-flet ((git (&rest args)
                (let ((default-directory repo))
                  (should (zerop (apply #'call-process "git" nil nil nil args))))))
      (unwind-protect
          (progn
            (git "init" "-q" "-b" "main")
            (git "config" "user.name" "Test")
            (git "config" "user.email" "test@example.com")
            (dolist (name '("f1.py" "f2.py" "f3.py" "f4.py"))
              (with-temp-file (expand-file-name name repo)
                (insert "def f():\n    x = 1\n    y = 2\n    return x + y\n")))
            (git "add" ".")
            (git "commit" "-q" "-m" "init")
            ;; Create a stash modifying f1.py, then restore working changes across all 4 files.
            (with-temp-file (expand-file-name "f1.py" repo)
              (insert "def f():\n    x = 99\n    y = 2\n    return x + y\n"))
            (git "stash" "push" "-q" "-m" "wip-stash")
            (dolist (pair '(("f1.py" . 10) ("f2.py" . 20) ("f3.py" . 30) ("f4.py" . 40)))
              (with-temp-file (expand-file-name (car pair) repo)
                (insert (format "def f():\n    x = %d\n    y = 2\n    return x + y\n"
                                (cdr pair)))))
            ;; Untracked file for untracked DWIM test.
            (with-temp-file (expand-file-name "untracked.py" repo)
              (insert "print('hello')\n"))
            ;; 1. Auto-fold threshold folds >= 4 files into a scannable checklist,
            ;;    and RET on a folded text file header opens Dual directly.
            (let ((omnidiff-overview-auto-fold-threshold 4)
                  (omnidiff-single-file-entry 'overview)
                  (default-directory repo))
              (setq buf (get-buffer-create "*omnidiff-wf-overview*"))
              (omnidiff-run buf '("git" "--no-pager" "diff" "--ext-diff")
                            "unstaged" repo 'minimal 'unified nil nil nil)
              (omnidiff-test--wait-request buf)
              (with-current-buffer buf
                ;; All 4 file cards start folded by threshold.
                (dolist (name '("f1.py" "f2.py" "f3.py" "f4.py"))
                  (let* ((hdr (omnidiff--find-file-header-pos name))
                         (ov (and hdr (get-text-property hdr 'omnidiff-body-ov))))
                    (should hdr)
                    (should ov)
                    (should (eq (overlay-get ov 'invisible) 'omnidiff-fold))))
                ;; RET on f2.py file header opens Dual directly on f2.py (with 4-file nav).
                (goto-char (omnidiff--find-file-header-pos "f2.py"))
                (setq dual-sess (omnidiff-jump-to-source))
                (should (omnidiff-dual--session-p dual-sess))
                (should (equal (omnidiff-dual--session-display-path dual-sess) "f2.py"))
                (should (= (plist-get (omnidiff-dual--session-nav-callbacks dual-sess) :file-total)
                           4)))
              (omnidiff-dual--teardown dual-sess nil nil)
              (setq dual-sess nil))
            ;; 2. Magit DWIM with target-spec lands directly in Dual on targeted file + line,
            ;;    retaining multi-file nav and triggering soft `magit-refresh' on quit after revert.
            (let* ((omnidiff-overview-auto-fold-threshold 4)
                   (omnidiff-magit-file-entry 'dual)
                   (omnidiff-single-file-entry 'auto)
                   (default-directory repo)
                   (refreshed nil)
                   (fake-magit (generate-new-buffer " *fake-magit*")))
              (unwind-protect
                  (cl-letf (((symbol-function 'magit-refresh)
                             (lambda () (setq refreshed t))))
                    (with-current-buffer fake-magit
                      (put 'omnidiff-test--fake-magit-mode 'derived-mode-parent 'magit-mode)
                      (setq major-mode 'omnidiff-test--fake-magit-mode))
                    (omnidiff-diff nil nil
                                   (list :file "f3.py" :line 2 :side 'after
                                         :origin-buf fake-magit))
                    (omnidiff-test--wait-request (get-buffer "*git diff omnidiff*"))
                    (setq dual-sess (car omnidiff-dual--sessions))
                    (should (omnidiff-dual--session-p dual-sess))
                    (should (equal (omnidiff-dual--session-display-path dual-sess) "f3.py"))
                    (should (= (plist-get (omnidiff-dual--session-nav-callbacks dual-sess) :file-total)
                               4))
                    (with-current-buffer (omnidiff-dual--session-after-buf dual-sess)
                      (should (= (line-number-at-pos) 2)))
                    ;; Simulate worktree mutation + quit -> triggers soft Magit refresh.
                    (setf (omnidiff-dual--session-mutated-p dual-sess) t)
                    (omnidiff-dual--teardown dual-sess nil nil)
                    (setq dual-sess nil)
                    (should refreshed))
                (when (buffer-live-p fake-magit) (kill-buffer fake-magit))
                (when-let* ((db (get-buffer "*git diff omnidiff*")))
                  (kill-buffer db))))
            ;; 3. Magit stash diff runs cleanly via `(stash . "stash@{0}")'.
            (let ((omnidiff-single-file-entry 'overview)
                  (default-directory repo))
              (omnidiff-diff (cons 'stash "stash@{0}"))
              (setq stash-buf (get-buffer "*git diff omnidiff stash@{0}*"))
              (should (buffer-live-p stash-buf))
              (omnidiff-test--wait-request stash-buf)
              (with-current-buffer stash-buf
                (should (string-search "f1.py" (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-search "99" (buffer-substring-no-properties (point-min) (point-max))))))
            ;; 4. Untracked file DWIM opens Dual directly comparing /dev/null vs untracked.py.
            (let ((default-directory repo))
              (setq untracked-sess
                    (omnidiff-diff nil nil (list :file "untracked.py" :untracked t)))
              (should (omnidiff-dual--session-p untracked-sess))
              (should (equal (omnidiff-dual--session-before-label untracked-sess) "/dev/null"))
              (should (string-search "untracked.py"
                                     (omnidiff-dual--session-display-path untracked-sess)))))
        (when (and dual-sess (memq dual-sess omnidiff-dual--sessions))
          (omnidiff-dual--teardown dual-sess nil nil))
        (when (and untracked-sess (memq untracked-sess omnidiff-dual--sessions))
          (omnidiff-dual--teardown untracked-sess nil nil))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (buffer-live-p stash-buf) (kill-buffer stash-buf))
        (delete-directory repo t)))))

(provide 'omnidiff-test)
;;; omnidiff-test.el ends here
