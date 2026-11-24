;;; omnidiff-git-test.el --- ERT tests for omnidiff-git and omnidiff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; This file is part of omnidiff.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Focused ERT test suite for `omnidiff-git.el' and `omnidiff.el': shell
;; wrapper framing, async request lifecycle, parallel/serial Git pipelines,
;; blob byte budgets, archive/PDF drilldown, review tracking, Magit workflows,
;; TUI launchers, and P0/P1 regression invariants.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'omnidiff-test-helper)

(declare-function loaddefs-generate "loaddefs-gen"
                  (dirs output-file &optional excluded-files extra-data
                        include-package-version generate-full))

;;; Shell wrapper, runner lifecycle & autoload tests

(ert-deftest omnidiff-git-test/wrapper-shell-quoting ()
  "Wrapper shell-quotes flags containing spaces and glob characters safely."
  (let* ((cmd (omnidiff--build-ext-diff-wrapper
               "/bin/echo" '("--arg" "has space" "*.el")))
         (out (shell-command-to-string
               (concat cmd " path.el old 0 0 new 0 0")))
         (prefix (omnidiff-test--file-meta "path.el" "old" "new")))
    (should (string-prefix-p prefix out))
    (should (equal (string-trim (substring out (length prefix)))
                   "--arg has space *.el -- path.el old 0 0 new 0 0"))))

(ert-deftest omnidiff-git-test/runner-error-and-concurrency ()
  "Failed runs preserve existing view; newer runs supersede stale ones."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true")))
    (omnidiff-test--with-temp-dir root
      (omnidiff-test--with-buffer-cleanup
        (let ((buf (generate-new-buffer " *omnidiff-runner-test*"))
              (dir-good (expand-file-name "good" root))
              (dir-bad (expand-file-name "bad" root))
              (dir-stale (expand-file-name "stale" root))
              (dir-fresh (expand-file-name "fresh" root)))
          (dolist (d (list dir-good dir-bad dir-stale dir-fresh))
            (make-directory d t))
          ;; 1. Seed buffer with a valid render, then run a failing command.
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir-good)
            (omnidiff-render-buffer (omnidiff-test--make-diff nil "commit keep-me\n")))
          (let ((fail-proc (omnidiff-run buf '("sh" "-c" "echo bad rev >&2; exit 128")
                                         "bad" dir-bad)))
            (should (omnidiff-test--wait-until
                     (lambda () (not (process-live-p fail-proc))) 5.0))
            (omnidiff-test--wait-request buf 5.0)
            (with-current-buffer buf
              (should (string-search "commit keep-me" (buffer-string)))
              (should (equal omnidiff--repo-dir dir-good))))
          ;; 2. Launch a slow run, immediately supersede with a fast run.
          (let ((slow (omnidiff-run buf '("sh" "-c" "sleep 0.2; printf 'commit stale\\n'")
                                    "stale" dir-stale))
                (fast (omnidiff-run buf '("sh" "-c" "printf 'commit fresh\\n'")
                                    "fresh" dir-fresh)))
            (should (omnidiff-test--wait-until
                     (lambda () (and (not (process-live-p slow))
                                     (not (process-live-p fast))))
                     5.0))
            (omnidiff-test--wait-request buf 5.0)
            (with-current-buffer buf
              (should (string-search "commit fresh" (buffer-string)))
              (should-not (string-search "commit stale" (buffer-string)))
              (should (equal omnidiff--repo-dir dir-fresh)))))))))

(ert-deftest omnidiff-git-test/nul-framing-special-filenames ()
  "NUL metadata framing preserves tabs, newlines, and sentinel-like names."
  (let* ((weird-old "dir\tname/old\nfile.rs")
         (weird-new "dir\tname/new\nfile.rs")
         (json-body "{\"before\":{\"path\":\"old\",\"hunks\":[{\"operation\":\"delete\",\"range\":{\"start_row\":0,\"start_column\":0,\"end_row\":0,\"end_column\":5}}]},\"after\":{\"path\":\"new\",\"hunks\":[{\"operation\":\"insert\",\"range\":{\"start_row\":0,\"start_column\":0,\"end_row\":0,\"end_column\":5}}]},\"large_residual\":false}")
         (raw (concat
               (omnidiff-test--file-meta weird-old "tmp-old" "tmp-new" weird-new)
               json-body omnidiff-git-end-sentinel))
         (diff (omnidiff--parse-serial-json-output raw nil))
         (file (car (omnidiff-diff-files diff))))
    (should (= (length (omnidiff-diff-files diff)) 1))
    (should (equal (omnidiff-file-path file) weird-old))
    (should (equal (omnidiff-file-target-path file) weird-new))
    (should (eq (omnidiff-file-status file) 'renamed))
    (should (omnidiff-json-diff-p (omnidiff-file-json-diff file))))
  ;; Shell wrapper emits exact NUL fields for tab/newline paths.
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
               json-body omnidiff-git-end-sentinel))
         (diff (omnidiff--parse-serial-json-output raw nil)))
    (should (= (length (omnidiff-diff-files diff)) 1))
    (should (equal (omnidiff-file-path (car (omnidiff-diff-files diff))) tricky-path))))

(ert-deftest omnidiff-git-test/autoload-no-recursive-load ()
  "Package autoloads resolve all public entrypoints without recursion."
  (omnidiff-test--with-temp-dir tmp
    (let* ((autoloads (expand-file-name "omnidiff-autoloads.el" tmp))
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
      (require 'loaddefs-gen)
      (loaddefs-generate omnidiff-test--pkg-dir autoloads)
      ;; 1. Loading autoloads registers all commands and `(require 'omnidiff)' succeeds.
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
      ;; 2. Invoking `omnidiff-apply-theme' directly through its autoload works.
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
      ;; 3. Each public command autoload resolves cleanly with zero recursive load errors.
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
            (should (zerop rc))))))))

(ert-deftest omnidiff-git-test/mode-reentry-and-lifecycle-cleanup ()
  "Mode re-entry rejects stale runs; launch failure and kill clean up pipes."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true")))
    (omnidiff-test--with-temp-dir root
      (omnidiff-test--with-buffer-cleanup
        (let ((buf (generate-new-buffer " *omnidiff-lifecycle-test*"))
              (dir-a (expand-file-name "life-a" root))
              (dir-b (expand-file-name "life-b" root)))
          (make-directory dir-a t)
          (make-directory dir-b t)
          ;; 1. Mode re-entry with bypassed hook still rejects stale req-id 1 via proc identity.
          (let ((slow (omnidiff-run buf '("sh" "-c" "sleep 0.25; printf 'commit STALE\\n'")
                                    "stale" dir-a)))
            (with-current-buffer buf
              (let ((change-major-mode-hook nil))
                (omnidiff-mode))
              (should (= omnidiff--request-id 0)))
            (let ((fast (omnidiff-run buf '("sh" "-c" "printf 'commit FRESH\\n'")
                                      "fresh" dir-b)))
              (should (omnidiff-test--wait-until
                       (lambda () (and (not (process-live-p slow))
                                       (not (process-live-p fast))))
                       5.0))
              (omnidiff-test--wait-request buf 5.0)
              (with-current-buffer buf
                (should (string-search "commit FRESH" (buffer-string)))
                (should-not (string-search "commit STALE" (buffer-string)))
                (should (equal omnidiff--repo-dir dir-b)))))
          ;; 2. Launch failure cleans up stderr pipe with no leak in `process-list'.
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
            (should (omnidiff-test--wait-until
                     (lambda () (and (not (process-live-p proc))
                                     (not (process-live-p sp))))
                     5.0))
            (omnidiff-test--wait-request buf 5.0)
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
          ;;    so killing the buffer closes the lingering pipe.
          (let* ((linger-buf (generate-new-buffer " *omnidiff-linger-stderr*"))
                 (proc (omnidiff-run linger-buf
                                     '("sh" "-c" "(trap '' HUP; sleep 0.5) >/dev/null & exit 7")
                                     "linger" dir-a))
                 (sp (with-current-buffer linger-buf
                       (omnidiff--request-stderr-proc omnidiff--active-request))))
            (should (omnidiff-test--wait-until
                     (lambda () (not (process-live-p proc))) 5.0))
            (should (process-live-p sp))
            (with-current-buffer linger-buf
              (should omnidiff--active-request)
              (should-not (omnidiff-git-pending-request)))
            (kill-buffer linger-buf)
            (should-not (process-live-p sp))))))))

(ert-deftest omnidiff-git-test/rapid-view-cycling ()
  "In-flight `v' updates layout in place; cached `v' renders with 0 spawns."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true")))
    (omnidiff-test--with-temp-dir dir
      (omnidiff-test--with-buffer-cleanup
        (let ((buf (generate-new-buffer " *omnidiff-rapid-cycle-test*")))
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
            (omnidiff-test--wait-request buf 5.0)
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
            ;; Failed `m' leaves committed state and cached diff intact.
            (setq omnidiff--command '("sh" "-c" "exit 1"))
            (omnidiff-cycle-preset)
            (omnidiff-test--wait-request buf 5.0)
            (should-not omnidiff--active-request)
            (should (eq omnidiff--layout 'unified))
            (should (eq omnidiff--preset 'minimal))
            (should (omnidiff-diff-p omnidiff--diff))
            ;; Cycling `v' during an in-flight request for repo B preserves repo B metadata.
            (omnidiff-test--with-temp-dir dir-b
              (omnidiff-run buf '("sh" "-c" "sleep 0.12; printf 'commit repo-b\\n'")
                            "repo-b" dir-b 'default 'unified)
              (let ((b-proc (omnidiff--request-proc omnidiff--active-request)))
                (omnidiff-cycle-layout)
                (should (eq (omnidiff--request-proc omnidiff--active-request) b-proc))
                (should (equal (omnidiff--request-repo-dir omnidiff--active-request) dir-b))
                (should (equal (omnidiff--request-label omnidiff--active-request) "repo-b"))
                (omnidiff-test--wait-request buf 5.0))
              (should (equal omnidiff--repo-dir dir-b))
              (should (equal omnidiff--label "repo-b"))
              (should (eq omnidiff--layout 'side-by-side)))))))))

(ert-deftest omnidiff-git-test/runner-pipe-and-progress-message ()
  "`omnidiff-run' uses a pipe connection and logs a start progress message."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true")))
    (omnidiff-test--with-temp-dir dir
      (omnidiff-test--with-buffer-cleanup
        (let* ((buf (generate-new-buffer " *omnidiff-pipe-test*"))
               (msg-mark (with-current-buffer (messages-buffer) (point-max)))
               (proc (omnidiff-run buf '("sh" "-c" "printf 'commit ok\\n'")
                                   "origin/main" dir)))
          (should-not (process-tty-name proc))
          (should (omnidiff-test--wait-until
                   (lambda () (not (process-live-p proc))) 5.0))
          (omnidiff-test--wait-request buf 5.0)
          (let ((logged (with-current-buffer (messages-buffer)
                          (buffer-substring-no-properties msg-mark (point-max)))))
            (should (string-search "Running omnidiff (origin/main)..." logged))))))))

;;; Raw Git parser, parallel vs serial parity & blob budget tests

(ert-deftest omnidiff-git-test/parse-raw-git-entries ()
  "Raw Git NUL parser handles UTF-8 paths, renames, and show preambles."
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

(ert-deftest omnidiff-git-test/parallel-git-diff-and-show-parity ()
  "Parallel Git diff/show pipeline matches serial `GIT_EXTERNAL_DIFF'."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (cl-flet ((run-cmd (cmd label max-par)
                  (let ((omnidiff-max-parallel max-par)
                        (buf (generate-new-buffer (format " *omnidiff-par-%s-%d*" label max-par))))
                    (omnidiff-run buf cmd label repo 'default 'unified)
                    (omnidiff-test--wait-request buf)
                    (with-current-buffer buf
                      (should-not omnidiff--active-request)
                      (list (buffer-substring-no-properties (point-min) (point-max))
                            (omnidiff-test--collect-diff-faces)
                            omnidiff--file-count)))))
        (with-temp-file (expand-file-name "Makefile" repo)
          (insert "all:\n\techo old\n"))
        (with-temp-file (expand-file-name "keep.rs" repo)
          (insert "fn demo() {\n    let x = 1;\n}\n"))
        (with-temp-file (expand-file-name "gone.rs" repo)
          (insert "fn gone() {}\n"))
        (with-temp-file (expand-file-name "old_name.rs" repo)
          (insert "fn renamed() {\n    let a = 1;\n    let b = 2;\n}\n"))
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "initial")
        (with-temp-file (expand-file-name "Makefile" repo)
          (insert "all:\n\techo new\n"))
        (with-temp-file (expand-file-name "keep.rs" repo)
          (insert "fn demo() {\n    let x = 2;\n}\n"))
        (delete-file (expand-file-name "gone.rs" repo))
        (rename-file (expand-file-name "old_name.rs" repo)
                     (expand-file-name "new_name.rs" repo))
        (with-temp-file (expand-file-name "new_name.rs" repo)
          (insert "fn renamed() {\n    let a = 1;\n    let b = 3;\n}\n"))
        (omnidiff-test--git repo "add" "-A")
        (omnidiff-test--git repo "commit" "-q" "-m" "second commit")
        ;; Working-tree dirty edits + intent-to-add new file (unpersisted SHA).
        (with-temp-file (expand-file-name "keep.rs" repo)
          (insert "fn demo() {\n    let x = 3;\n}\n"))
        (with-temp-file (expand-file-name "added_wt.rs" repo)
          (insert "fn brand_new() { let v = 42; }\n"))
        (omnidiff-test--git repo "add" "-N" "added_wt.rs")
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
        ;; 2. JSON parallel mode produces semantic structs and accurate file counts.
        (let ((omnidiff-max-parallel 4))
          (dolist (spec '(("show HEAD" ("git" "--no-pager" "show" "--ext-diff" "HEAD") 4 "second commit")
                          ("HEAD~1" ("git" "--no-pager" "diff" "--ext-diff" "HEAD~1") 5 "brand_new")
                          ("unstaged" ("git" "--no-pager" "diff" "--ext-diff") 2 "brand_new")))
            (let ((buf (generate-new-buffer (format " *omnidiff-par-json-%s*" (nth 0 spec)))))
              (omnidiff-run buf (nth 1 spec) (nth 0 spec) repo 'default 'unified)
              (omnidiff-test--wait-request buf)
              (with-current-buffer buf
                (should-not omnidiff--active-request)
                (should (= omnidiff--file-count (nth 2 spec)))
                (should (string-search (nth 3 spec)
                                       (buffer-substring-no-properties (point-min) (point-max))))
                (should (cl-every #'omnidiff-file-json-diff
                                  (omnidiff-diff-files omnidiff--diff)))))))))))

(ert-deftest omnidiff-git-test/mixed-ad-binary-and-empty-delete-parity ()
  "Mixed A/D text, empty, blank-run, NBSP, binary, and empty-delete match parity."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (cl-flet ((run-show (preset max-par)
                  (let ((omnidiff-max-parallel max-par)
                        (buf (generate-new-buffer (format " *omnidiff-ad-%s-%d*" preset max-par))))
                    (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                  "show HEAD" repo preset 'unified)
                    (omnidiff-test--wait-request buf)
                    (with-current-buffer buf
                      (list (buffer-substring-no-properties (point-min) (point-max))
                            (omnidiff-test--collect-diff-faces)
                            omnidiff--file-count)))))
        (with-temp-file (expand-file-name "del_plain.rs" repo)
          (insert "fn gone() {\r\n\tlet x = 1;   \r\n\r\n\tlet y = 2;\r\n}\r\n"))
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "seed")
        ;; Second commit: mixed A/D files covering CRLF, empty, >6 blanks, NBSP, binary.
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
        (omnidiff-test--write-bin
         (expand-file-name "add_bin.dat" repo)
         (unibyte-string #x89 ?P ?N ?G #x0d #x0a #x1a #x0a 0 1 2 3))
        (omnidiff-test--git repo "add" "-A")
        (omnidiff-test--git repo "commit" "-q" "-m" "ad mix")
        (let ((omnidiff-max-parallel 4)
              (buf (generate-new-buffer " *omnidiff-ad-json*")))
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
                (should (omnidiff-engine-file-binary-p bin))
                (should (eq (omnidiff-engine-file-summary emp) 'no-changes))
                (should (eq (omnidiff-engine-file-summary add) 'new-file))
                (should (eq (omnidiff-engine-file-summary del) 'deleted-file))))))
        ;; Deleting an empty file matches serial parity cleanly.
        (delete-file (expand-file-name "add_empty.txt" repo))
        (omnidiff-test--git repo "add" "-A")
        (omnidiff-test--git repo "commit" "-q" "-m" "del empty")
        (should (equal (run-show 'default 4) (run-show 'default 1)))))))

(ert-deftest omnidiff-git-test/parallel-spec-and-fallback-detection ()
  "Parallel gating rejects show ranges/flags and raw parser detects fallbacks."
  (should (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "HEAD")))
  (should (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "HEAD~1..HEAD")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "main...feat")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "HEAD" "HEAD~1")))
  (should-not (omnidiff--git-parallel-spec '("git" "--no-pager" "show" "--ext-diff" "--stat" "HEAD")))
  ;; Combined diff (`::') and unmerged (`U') trigger `fallback-serial'.
  (let ((sha (make-string 40 ?a))
        (zero-sha (make-string 40 ?0)))
    (should (eq (omnidiff--parse-raw-git-entries
                 (concat "commit " sha "\n\n    merge\n\0::100644 100644 100644 "
                         sha " " sha " " sha " MM\0f.txt\0")
                 'show)
                'fallback-serial))
    (should (eq (omnidiff--parse-raw-git-entries
                 (concat ":000000 100644 " zero-sha " " zero-sha " U\0f.txt\0")
                 'diff)
                'fallback-serial))))

(ert-deftest omnidiff-git-test/merge-range-and-worker-error ()
  "Merge commits, `git show A..B', and failing workers behave cleanly."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (cl-flet ((run-buf (cmd label max-par &optional bin-override)
                  (let ((omnidiff-max-parallel max-par)
                        (omnidiff-executable (or bin-override omnidiff-executable))
                        (buf (generate-new-buffer (format " *omnidiff-edge-%s-%d*" label max-par))))
                    (omnidiff-run buf cmd label repo 'default 'unified)
                    (omnidiff-test--wait-request buf)
                    (with-current-buffer buf
                      (buffer-substring-no-properties (point-min) (point-max))))))
        (with-temp-file (expand-file-name "conflict.txt" repo) (insert "base\n"))
        (with-temp-file (expand-file-name "ok.txt" repo) (insert "v1\n"))
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "base commit")
        (omnidiff-test--git repo "checkout" "-q" "-b" "side")
        (with-temp-file (expand-file-name "conflict.txt" repo) (insert "side\n"))
        (omnidiff-test--git repo "commit" "-q" "-am" "side commit")
        (omnidiff-test--git repo "checkout" "-q" "main")
        (with-temp-file (expand-file-name "conflict.txt" repo) (insert "main\n"))
        (with-temp-file (expand-file-name "ok.txt" repo) (insert "v2\n"))
        (omnidiff-test--git repo "commit" "-q" "-am" "main commit")
        ;; Expected merge conflict exit status 1, so invoke `call-process' directly.
        (call-process "git" nil nil nil "merge" "--no-commit" "side")
        (with-temp-file (expand-file-name "conflict.txt" repo) (insert "resolved\n"))
        (omnidiff-test--git repo "add" "conflict.txt")
        (omnidiff-test--git repo "commit" "-q" "-m" "merge conflict resolved")
        ;; 1. Combined diff merge commit falls back cleanly and matches serial.
        (let ((s1 (run-buf '("git" "--no-pager" "show" "--ext-diff" "HEAD") "merge" 1))
              (s4 (run-buf '("git" "--no-pager" "show" "--ext-diff" "HEAD") "merge" 4)))
          (should (equal s4 s1))
          (should (string-search "merge conflict resolved" s4))
          (should (string-search "resolved" s4)))
        ;; 2. `git show A..B' falls back to serial and shows multiple commits.
        (let ((range-out (run-buf '("git" "--no-pager" "show" "--ext-diff" "HEAD~2..HEAD")
                                  "range" 4)))
          (should (string-search "merge conflict resolved" range-out))
          (should (string-search "main commit" range-out)))
        ;; 3. Failing worker records diagnostic `:note' without breaking sibling file.
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
            (should (string-search "dirty-b" out))))))))

(ert-deftest omnidiff-git-test/async-error-always-terminates-request ()
  "Parse or stage exceptions never leave `omnidiff--active-request' stuck."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (with-temp-file (expand-file-name "a.txt" repo) (insert "old-a\n"))
      (with-temp-file (expand-file-name "b.txt" repo) (insert "old-b\n"))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "init")
      (with-temp-file (expand-file-name "a.txt" repo) (insert "new-a\n"))
      (with-temp-file (expand-file-name "b.txt" repo) (insert "new-b\n"))
      ;; 1. Single-worker JSON parse exception produces error card and finishes cleanly.
      (let ((buf (generate-new-buffer " *omnidiff-parse-err-json*"))
            (orig-json-parse (symbol-function 'omnidiff-engine-parse-json)))
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
              (should (string-search "new-b" txt))))))
      ;; 2. Stage preparation exception fails cleanly and clears pending request.
      (let ((buf (generate-new-buffer " *omnidiff-prep-err*")))
        (cl-letf (((symbol-function 'omnidiff--prepare-parallel-work)
                   (lambda (&rest _) (error "synthetic prep boom"))))
          (omnidiff-run buf '("git" "--no-pager" "diff" "--ext-diff")
                        "prep-err" repo 'default 'unified)
          (omnidiff-test--wait-request buf 5.0)
          (with-current-buffer buf
            (should-not omnidiff--active-request)))))))

(ert-deftest omnidiff-git-test/git-parallel-gating-sha256-and-blob-budget ()
  "Parallel gating validates flags, handles SHA-256 OIDs, and caps blob RAM."
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
  ;; 3. Oversized blob triggers serial fallback AND enforces `omnidiff-max-parallel-blob-bytes'.
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (with-temp-file (expand-file-name "big.txt" repo)
        (insert (make-string 512 ?a) "\n"))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "v1")
      (with-temp-file (expand-file-name "big.txt" repo)
        (insert (make-string 512 ?b) "\n"))
      (omnidiff-test--git repo "commit" "-q" "-am" "v2")
      (let ((buf-cap (generate-new-buffer " *omnidiff-budget-cap*"))
            (buf-ser (generate-new-buffer " *omnidiff-budget-ser*")))
        (let ((omnidiff-max-parallel 4)
              (omnidiff-max-parallel-blob-bytes 32))
          (omnidiff-run buf-cap '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                        "cap" repo 'default 'unified)
          (omnidiff-test--wait-request buf-cap))
        (let ((omnidiff-max-parallel 1)
              (omnidiff-max-parallel-blob-bytes 32))
          (omnidiff-run buf-ser '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                        "ser" repo 'default 'unified)
          (omnidiff-test--wait-request buf-ser))
        (with-current-buffer buf-cap
          (let ((f (car (omnidiff-diff-files omnidiff--diff)))
                (txt (buffer-substring-no-properties (point-min) (point-max))))
            (should (omnidiff-file-json-diff f))
            (should (equal (omnidiff-file-before-source f) ""))
            (should (equal (omnidiff-file-after-source f) ""))
            (should (string-search "File exceeds blob size limit" (omnidiff-file-note f)))
            (should (string-search "File exceeds blob size limit" txt))
            (should (equal txt (with-current-buffer buf-ser
                                 (buffer-substring-no-properties (point-min) (point-max)))))))))))

(ert-deftest omnidiff-git-test/crlf-raw-path-and-pop-before-render ()
  "Raw CRLF paths decode literally and `pop-to-buffer' runs before render."
  ;; 1. Literal CRLF path decoding in raw Git metadata.
  (let* ((sha-a (make-string 40 ?a))
         (sha-b (make-string 40 ?b))
         (raw (concat ":100644 100644 " sha-a " " sha-b " M\0dir/a\r\nb.el\0"))
         (parsed (omnidiff--parse-raw-git-entries (string-to-unibyte raw) 'diff)))
    (should (equal (omnidiff--git-entry-path (cadr parsed)) "dir/a\r\nb.el")))
  ;; 2. `commit-and-render' displays buffer before rendering so SBS measures target window.
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true")))
    (omnidiff-test--with-temp-dir dir
      (omnidiff-test--with-buffer-cleanup
        (let ((buf (generate-new-buffer " *omnidiff-pop-order*"))
              (events nil)
              (orig-render (symbol-function 'omnidiff-render-buffer)))
          (cl-letf (((symbol-function 'pop-to-buffer)
                     (lambda (b &rest _) (push 'pop events) b))
                    ((symbol-function 'omnidiff-render-buffer)
                     (lambda (&rest args) (push 'render events) (apply orig-render args))))
            (omnidiff-run buf '("sh" "-c" "printf 'commit ok\\n'")
                          "pop" dir 'default 'side-by-side)
            (omnidiff-test--wait-request buf)
            (should (equal (nreverse events) '(pop render)))))))))

(ert-deftest omnidiff-git-test/root-commit-mode-only-and-file-filter ()
  "Root commit DWIM, mode-only `chmod +x' parity, and file filters work."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (with-temp-file (expand-file-name "run.sh" repo) (insert "#!/bin/sh\necho hi\n"))
      (with-temp-file (expand-file-name "other.txt" repo) (insert "v1\n"))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "root commit")
      ;; 1. Root commit via `(commit . HEAD)' delegates to `omnidiff-show' with filter.
      (let ((omnidiff-single-file-entry 'overview)
            (buf (get-buffer-create "*git show omnidiff HEAD*")))
        (omnidiff-diff '(commit . "HEAD") '("run.sh"))
        (omnidiff-test--wait-request buf)
        (with-current-buffer buf
          (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-search "root commit" txt))
            (should (string-search "run.sh" txt))
            (should-not (string-search "other.txt" txt)))))
      ;; 2. Mode-only change (`chmod +x') matches serial/parallel and renders mode change.
      (set-file-modes (expand-file-name "run.sh" repo) #o755)
      (omnidiff-test--git repo "add" "run.sh")
      (omnidiff-test--git repo "commit" "-q" "-m" "chmod +x")
      (let ((buf-s (generate-new-buffer " *omnidiff-mode-s*"))
            (buf-p (generate-new-buffer " *omnidiff-mode-p*")))
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
      ;; 3. `omnidiff-diff' with file filter limits unstaged diff to requested path.
      (with-temp-file (expand-file-name "run.sh" repo) (insert "#!/bin/sh\necho changed\n"))
      (with-temp-file (expand-file-name "other.txt" repo) (insert "v2\n"))
      (let ((omnidiff-single-file-entry 'overview)
            (buf (get-buffer-create "*git diff omnidiff*")))
        (omnidiff-diff nil '("other.txt"))
        (omnidiff-test--wait-request buf)
        (with-current-buffer buf
          (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-search "other.txt" txt))
            (should-not (string-search "run.sh" txt))))))))

;;; Container drilldown, standalone entrypoints, Dual handoff & TUI launchers

(ert-deftest omnidiff-git-test/zip-container-drilldown ()
  "`omnidiff-files' on ZIP archives drills into Dual (.py) and image (.png)."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "python3")))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
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
          (should (buffer-live-p zip-buf))
          (with-current-buffer zip-buf
            (should (derived-mode-p 'omnidiff-mode))
            ;; Drill down `.py' member -> Dual viewer.
            (goto-char (point-min))
            (search-forward "app.py")
            (goto-char (match-beginning 0))
            (should (omnidiff-jump-to-source))
            (let ((py-sess (car omnidiff-dual--sessions)))
              (should (omnidiff-dual-session-p py-sess))
              (should (string-search "app.py" (omnidiff-dual-session-display-path py-sess)))
              (should (> (length (omnidiff-test--token-overlays
                                  (omnidiff-dual-session-after-buf py-sess)))
                         0))
              (omnidiff-test--cleanup-session py-sess))
            ;; Drill down `.png' member -> standalone visual image viewer + tmpdir cleanup.
            (goto-char (point-min))
            (search-forward "logo.png")
            (goto-char (match-beginning 0))
            (should (omnidiff-jump-to-source))
            (let ((png-buf (window-buffer (selected-window)))
                  (owned-dirs nil))
              (should (buffer-live-p png-buf))
              (should-not (eq png-buf zip-buf))
              (should (string-search "logo.png" (buffer-name png-buf)))
              (with-current-buffer png-buf
                (should (derived-mode-p 'omnidiff-mode))
                (let* ((f (car (omnidiff-diff-files omnidiff--diff)))
                       (pic (omnidiff-engine-file-picture f)))
                  (should (omnidiff-picture-p pic))
                  (should (equal (omnidiff-image--fetch-side-bytes f pic 'before dir t)
                                 omnidiff-test--png-red-2x2))
                  (should (equal (omnidiff-image--fetch-side-bytes f pic 'after dir t)
                                 omnidiff-test--png-blue-2x2)))
                (setq owned-dirs (copy-sequence omnidiff--owned-tmpdirs))
                (should owned-dirs)
                (should (cl-every #'file-directory-p owned-dirs)))
              (kill-buffer png-buf)
              (should-not (cl-some #'file-directory-p owned-dirs)))))))))

(ert-deftest omnidiff-git-test/buffer-vs-head-and-single-file-entry ()
  "`omnidiff-buffer-vs-head' and 1-file `auto'->Dual->`O' / `d' 0ms handoff work."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (let ((tracked (expand-file-name "single.py" repo))
            (untracked (expand-file-name "brand_new.py" repo)))
        (with-temp-file tracked (insert "def calc():\n    return 1\n"))
        (omnidiff-test--git repo "add" "single.py")
        (omnidiff-test--git repo "commit" "-q" "-m" "initial")
        ;; 1. Modified unsaved buffer vs HEAD.
        (let ((live-buf (find-file-noselect tracked)))
          (with-current-buffer live-buf
            (goto-char (point-min))
            (search-forward "1")
            (replace-match "99")
            (should (buffer-modified-p))
            (let ((hsess (omnidiff-buffer-vs-head)))
              (should (omnidiff-dual-session-p hsess))
              (should (eq (omnidiff-dual-session-after-buf hsess) live-buf))
              (should (string-search "HEAD:single.py"
                                     (omnidiff-dual-session-before-label hsess)))
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
        ;; 2. Untracked file vs HEAD -> `/dev/null' before side and insert tokens.
        (with-temp-file untracked (insert "def fresh():\n    return 7\n"))
        (let ((untr-buf (find-file-noselect untracked)))
          (with-current-buffer untr-buf
            (let ((usess (omnidiff-buffer-vs-head)))
              (should (omnidiff-dual-session-p usess))
              (should (equal (omnidiff-dual-session-before-path usess) "/dev/null"))
              (should (equal (omnidiff-dual-session-before-label usess) "/dev/null (new file)"))
              (should (= (buffer-size (omnidiff-dual-session-before-buf usess)) 0))
              (should (eq (omnidiff-dual-session-after-buf usess) untr-buf))
              (let ((ins-ovs (omnidiff-test--token-overlays untr-buf)))
                (should (> (length ins-ovs) 0))
                (should (cl-every (lambda (ov) (eq (overlay-get ov 'face) 'omnidiff-token-insert))
                                  ins-ovs)))
              (with-current-buffer untr-buf (omnidiff-dual-quit))))
          (kill-buffer untr-buf))
        (delete-file untracked)
        ;; 3. 1-file Python repo: `auto' opens Dual -> `O' returns to Overview.
        (with-temp-file tracked (insert "def calc():\n    return 42\n"))
        (let ((ov-buf (get-buffer-create "*git diff omnidiff*")))
          (let ((omnidiff-single-file-entry 'auto))
            (omnidiff-diff)
            (omnidiff-test--wait-request ov-buf)
            (let ((auto-sess (car omnidiff-dual--sessions)))
              (should (omnidiff-dual-session-p auto-sess))
              (should (equal (omnidiff-dual-session-display-path auto-sess) "single.py"))
              (with-current-buffer (omnidiff-dual-session-after-buf auto-sess)
                (omnidiff-dual-open-overview))
              (should-not (memq auto-sess omnidiff-dual--sessions))
              (should (eq (window-buffer (selected-window)) ov-buf))))
          (when-let* ((vb (find-buffer-visiting tracked)))
            (kill-buffer vb))
          ;; 4. `overview' stays in Overview -> `d' opens Dual in 0ms (0 subprocesses).
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
                  (should (omnidiff-dual-session-p cached-sess))
                  (should (> (length (omnidiff-test--token-overlays
                                      (omnidiff-dual-session-after-buf cached-sess)))
                             0))
                  (omnidiff-test--cleanup-session cached-sess))))))))))

(ert-deftest omnidiff-git-test/multi-file-overview-controls-dual-cycle-and-tui ()
  "Multi-file Overview (`H', `F', `T'), Dual `]'/`[' cycling, and TUI cleanup."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (with-temp-file (expand-file-name "alpha.py" repo) (insert "def alpha():\n    return 1\n"))
      (with-temp-file (expand-file-name "beta.py" repo) (insert "def beta():\n    return 10\n"))
      (with-temp-file (expand-file-name "ws_only.py" repo) (insert "x = 1 + 2\n"))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "base")
      (with-temp-file (expand-file-name "alpha.py" repo) (insert "def alpha():\n    return 2\n"))
      (with-temp-file (expand-file-name "beta.py" repo) (insert "def beta():\n    return 20\n"))
      (with-temp-file (expand-file-name "ws_only.py" repo) (insert "x = 1   +   2\n"))
      (let ((ov-buf (get-buffer-create "*git diff omnidiff*"))
            (omnidiff-single-file-entry 'auto)
            (omnidiff-overview-auto-fold-trivial nil))
        (omnidiff-diff)
        (omnidiff-test--wait-request ov-buf)
        (should-not omnidiff-dual--sessions)
        (with-current-buffer ov-buf
          (should (= omnidiff--file-count 3))
          ;; 1. `H' cycles highlight style in memory.
          (should (eq (omnidiff-render-effective-highlight-style) 'token))
          (omnidiff-cycle-highlight-style)
          (should (eq omnidiff--highlight-style 'subtle-line))
          (omnidiff-cycle-highlight-style)
          (should (eq omnidiff--highlight-style 'traditional))
          (omnidiff-cycle-highlight-style)
          (should (eq omnidiff--highlight-style 'token))
          (should-not omnidiff--active-request)
          ;; 2. `F' folds/unfolds trivial `ws_only.py' card while keeping `alpha.py' open.
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
          ;; 3. `T' custom TUI launcher + automatic `omnidiff--tui-tmpdirs' cleanup on kill.
          (goto-char (point-min))
          (search-forward "alpha.py")
          (let* ((captured nil)
                 (omnidiff-tui-launcher
                  (lambda (argv buf-name dir-arg)
                    (setq captured (list argv buf-name dir-arg))
                    (generate-new-buffer buf-name)))
                 (tui-buf (omnidiff-open-in-tui)))
            (should (buffer-live-p tui-buf))
            (should (member "--mode" (nth 0 captured)))
            (should (member "tui" (nth 0 captured)))
            (let ((tui-tmp (car (buffer-local-value 'omnidiff--tui-tmpdirs tui-buf))))
              (should (and (stringp tui-tmp) (file-directory-p tui-tmp)))
              (kill-buffer tui-buf)
              (should-not (file-directory-p tui-tmp))))
          ;; 4. Dual `]'/`[' file cycling reuses session across `alpha.py' <-> `beta.py' + `T'.
          (goto-char (point-min))
          (search-forward "alpha.py")
          (let ((dsess (omnidiff-open-dual-at-point)))
            (should (equal (omnidiff-dual-session-display-path dsess) "alpha.py"))
            (with-current-buffer (omnidiff-dual-session-after-buf dsess)
              (omnidiff-dual-next-file))
            (should (memq dsess omnidiff-dual--sessions))
            (should (equal (omnidiff-dual-session-display-path dsess) "beta.py"))
            (with-current-buffer (omnidiff-dual-session-after-buf dsess)
              (omnidiff-dual-prev-file))
            (should (equal (omnidiff-dual-session-display-path dsess) "alpha.py"))
            (let* ((dual-cap nil)
                   (omnidiff-tui-launcher
                    (lambda (argv buf-name dir-arg)
                      (setq dual-cap (list argv buf-name dir-arg))
                      (generate-new-buffer buf-name)))
                   (dtui-buf (with-current-buffer (omnidiff-dual-session-after-buf dsess)
                               (omnidiff-dual-open-tui))))
              (should (buffer-live-p dtui-buf))
              (should (member "--mode" (nth 0 dual-cap)))
              (let ((dtmp (car (buffer-local-value 'omnidiff--tui-tmpdirs dtui-buf))))
                (should (and (stringp dtmp) (file-directory-p dtmp)))
                (kill-buffer dtui-buf)
                (should-not (file-directory-p dtmp))))
            (omnidiff-test--cleanup-session dsess)))))))

;;; Security hardening: blob-dest containment, archive bombs, dotfiles & PDF keys

(ert-deftest omnidiff-git-test/blob-dest-and-nul-archive-key ()
  "`omnidiff-git-blob-dest' keeps tricky names contained; NUL archive key errors."
  (omnidiff-test--with-temp-dir dir
    (let ((blob-tmp (expand-file-name "blob-tmp" dir)))
      (make-directory blob-tmp t)
      (dolist (p '("~" "dir/~" "~root" "~root.py" "a\\b" ".." "bad\x00name" "Makefile"))
        (let ((dest (omnidiff-git-blob-dest "abcd1234" p blob-tmp)))
          (should (file-in-directory-p dest blob-tmp))
          (should (string-prefix-p (file-name-as-directory (expand-file-name blob-tmp))
                                   dest))
          (should-not (string-prefix-p "~" (file-name-nondirectory dest)))
          (should-not (string-search "\x00" dest))))
      (should (string-suffix-p ".py" (omnidiff-git-blob-dest "abcd1234" "~root.py" blob-tmp)))
      (should (equal (file-name-nondirectory
                      (omnidiff-git-blob-dest "abcd1234" "Makefile" blob-tmp))
                     "Makefile"))
      (let ((dummy-arch (omnidiff-test--make-file :path "pkg.zip" :target-path "pkg.zip"
                                                  :status 'modified)))
        (should-error
         (omnidiff--drilldown-archive-member dummy-arch "bad\x00key" dir nil)
         :type 'user-error)))))

(ert-deftest omnidiff-git-test/archive-bomb-dotfiles-and-dir-skip ()
  "Archive extraction enforces byte limits, dotfile exactness, and skips dirs."
  (skip-unless (executable-find "python3"))
  (omnidiff-test--with-temp-dir dir
    (let* ((python (executable-find "python3"))
           (bomb-zip (expand-file-name "bomb.zip" dir))
           (bomb-gz (expand-file-name "bomb.txt.gz" dir))
           (stream-out-a (expand-file-name "gz-out-a" dir))
           (stream-out-b (expand-file-name "gz-out-b" dir)))
      (should (zerop
               (call-process
                python nil nil nil "-c"
                "import gzip, sys, zipfile
with zipfile.ZipFile(sys.argv[1], 'w', compression=zipfile.ZIP_DEFLATED) as zf:
    zf.writestr('big.txt', 'A' * 4096)
with gzip.open(sys.argv[2], 'wb') as gz:
    gz.write(b'B' * 4096)
"
                bomb-zip bomb-gz)))
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
             (f-zip (omnidiff-test--make-file :path "bomb.zip" :target-path "bomb.zip"
                                              :status 'modified
                                              :before-disk-path bomb-zip
                                              :after-disk-path bomb-zip))
             (f-gz (omnidiff-test--make-file :path "bomb.txt.gz" :target-path "bomb.txt.gz"
                                             :status 'modified
                                             :before-disk-path bomb-gz
                                             :after-disk-path bomb-gz)))
        (dolist (pair (list (cons f-zip "big.txt") (cons f-gz "(content)")))
          (let ((err (should-error
                      (omnidiff--drilldown-archive-member (car pair) (cdr pair) dir nil)
                      :type 'user-error)))
            (should (string-search "exceeds extraction size limit"
                                   (error-message-string err)))))
        (should (equal (directory-files temporary-file-directory nil
                                        "\\`omnidiff-archive-")
                       before-tmps)))
      ;; `.env' vs `env' dotfiles never mismatch; ZIP directory entry is skipped.
      (let ((z-dot (expand-file-name "dotonly.zip" dir))
            (z-plain (expand-file-name "plainonly.zip" dir))
            (z-dir (expand-file-name "direntry.zip" dir))
            (out-a (expand-file-name "dot-a" dir))
            (out-b (expand-file-name "dot-b" dir)))
        (should (zerop
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
                  z-dot z-plain z-dir)))
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
        (let ((f-dir (omnidiff-test--make-file :path "direntry.zip" :target-path "direntry.zip"
                                               :status 'modified
                                               :before-disk-path z-dir
                                               :after-disk-path z-dir)))
          (should-error
           (omnidiff--drilldown-archive-member f-dir "subdir/" dir nil)
           :type 'user-error))))))

(ert-deftest omnidiff-git-test/archive-tilde-root-key-drilldown ()
  "`~root' archive member key stays contained and opens Dual cleanly."
  (skip-unless (and (executable-find "python3")
                    (executable-find omnidiff-executable)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((python (executable-find "python3"))
            (z-tilde-a (expand-file-name "tilde_a.zip" dir))
            (z-tilde-b (expand-file-name "tilde_b.zip" dir)))
        (should (zerop
                 (call-process
                  python nil nil nil "-c"
                  "import sys, zipfile
with zipfile.ZipFile(sys.argv[1], 'w') as zf:
    zf.writestr('~root', 'alpha = 1\\n')
with zipfile.ZipFile(sys.argv[2], 'w') as zf:
    zf.writestr('~root', 'alpha = 2\\n')
"
                  z-tilde-a z-tilde-b)))
        (let* ((f-tilde (omnidiff-test--make-file :path "t.zip" :target-path "t.zip"
                                                  :status 'modified
                                                  :before-disk-path z-tilde-a
                                                  :after-disk-path z-tilde-b))
               (sess (omnidiff--drilldown-archive-member f-tilde "~root" dir nil)))
          (should (omnidiff-dual-session-p sess))
          (should (string-search "~root" (omnidiff-dual-session-display-path sess)))
          (omnidiff-test--cleanup-session sess))))))

(ert-deftest omnidiff-git-test/pdf-page-regex-and-pdftoppm ()
  "PDF page regex accepts `page 3'/`3' and rejects attachment keys."
  (omnidiff-test--with-temp-dir dir
    (let* ((empty-bin (expand-file-name "empty-bin" dir))
           (fake-bin (expand-file-name "fake-pdf-bin" dir))
           (pdf-log (expand-file-name "pdftoppm.log" dir))
           (dummy-pdf (expand-file-name "doc.pdf" dir))
           (pdf-file (omnidiff-test--make-file
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
      ;; Without rasterizer: valid page keys mention `pdftoppm'; attachment is rejected.
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
      ;; With fake `pdftoppm': `page 3'/`3' pass `-f 3 -l 3'; attachment never spawns it.
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
                                 (omnidiff-engine-read-file-decoded pdf-log))))))))

;;; Multi-batch cat-file, serial JSON parity, multi-file review & Magit workflow

(ert-deftest omnidiff-git-test/batched-git-cat-file-preserves-parallel-json ()
  "Multi-file diffs exceeding blob budget stream across batches in parallel JSON."
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
  ;; 2. Real Git repo: 6 files x ~220B (> 500B budget) runs across batches in parallel JSON.
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (dotimes (i 6)
        (with-temp-file (expand-file-name (format "f%d.txt" i) repo)
          (insert (format "header-%d\n%s\n" i (make-string 200 (+ ?a i))))))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "v1")
      (dotimes (i 6)
        (with-temp-file (expand-file-name (format "f%d.txt" i) repo)
          (insert (format "header-%d\n%s\n" i (make-string 200 (+ ?A i))))))
      (omnidiff-test--git repo "commit" "-q" "-am" "v2")
      (let ((buf-batched (generate-new-buffer " *omnidiff-batched*"))
            (buf-full (generate-new-buffer " *omnidiff-unbatched*"))
            (batch-calls 0)
            (orig-mat (symbol-function 'omnidiff--materialize-cat-batch)))
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
        (should (> batch-calls 1))
        (with-current-buffer buf-batched
          (should omnidiff--diff)
          (should (= (length (omnidiff-diff-files omnidiff--diff)) 6))
          (dolist (f (omnidiff-diff-files omnidiff--diff))
            (should (omnidiff-file-json-diff f))))
        (should (equal (with-current-buffer buf-batched
                         (buffer-substring-no-properties (point-min) (point-max)))
                       (with-current-buffer buf-full
                         (buffer-substring-no-properties (point-min) (point-max)))))))))

(ert-deftest omnidiff-git-test/serial-json-parity-and-multi-commit-show ()
  "Serial JSON matches parallel JSON and preserves multi-commit preambles."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (cl-flet ((run-one (cmd label max-par)
                  (let ((buf (generate-new-buffer (format " *omnidiff-sjson-%s-%d*" label max-par)))
                        (omnidiff-max-parallel max-par))
                    (omnidiff-run buf cmd label repo 'default 'unified)
                    (omnidiff-test--wait-request buf)
                    (with-current-buffer buf
                      (list (buffer-substring-no-properties (point-min) (point-max))
                            (mapcar (lambda (f)
                                      (list (omnidiff-file-target-path f)
                                            (not (null (omnidiff-file-json-diff f)))
                                            (omnidiff-file-status f)))
                                    (omnidiff-diff-files omnidiff--diff)))))))
        (with-temp-file (expand-file-name "alpha.py" repo)
          (insert "def f():\n    return 1\n"))
        (with-temp-file (expand-file-name "old.txt" repo)
          (insert "rename me\n"))
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "commit-one")
        (with-temp-file (expand-file-name "alpha.py" repo)
          (insert "def f():\n    return 2\n"))
        (omnidiff-test--git repo "add" "alpha.py")
        (omnidiff-test--git repo "mv" "old.txt" "renamed.txt")
        (omnidiff-test--git repo "commit" "-q" "-m" "commit-two-alpha")
        (with-temp-file (expand-file-name "beta.py" repo)
          (insert "def g():\n    return 3\n"))
        (omnidiff-test--git repo "add" "beta.py")
        (omnidiff-test--git repo "commit" "-q" "-m" "commit-three-beta")
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
        ;; 2. Multi-commit `git show' preserves both commit preambles and JSON diffs.
        (let ((multi (run-one '("git" "--no-pager" "show" "--ext-diff" "HEAD~2..HEAD") "multi" 4)))
          (should (string-search "commit-three-beta" (car multi)))
          (should (string-search "commit-two-alpha" (car multi)))
          (should (string-search "return 3" (car multi)))
          (should (string-search "return 2" (car multi)))
          (should (= (length (cadr multi)) 3))
          (should (cl-every #'cadr (cadr multi))))))))

(ert-deftest omnidiff-git-test/serial-json-single-blob-budget-cap ()
  "Single-blob overflow falls back to serial JSON and enforces blob byte cap."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (with-temp-file (expand-file-name "alpha.py" repo)
        (insert "def f():\n    return 1\n"))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "v1")
      (with-temp-file (expand-file-name "alpha.py" repo)
        (insert "def f():\n    return 2\n"))
      (omnidiff-test--git repo "commit" "-q" "-am" "v2")
      (let ((buf (generate-new-buffer " *omnidiff-sjson-cap*"))
            (omnidiff-max-parallel 4)
            (omnidiff-max-parallel-blob-bytes 8))
        (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                      "cap" repo 'default 'unified)
        (omnidiff-test--wait-request buf)
        (with-current-buffer buf
          (let ((f (car (omnidiff-diff-files omnidiff--diff)))
                (txt (buffer-substring-no-properties (point-min) (point-max))))
            (should (omnidiff-file-json-diff f))
            (should (string-search "File exceeds blob size limit" (omnidiff-file-note f)))
            (should (string-search "File exceeds blob size limit" txt))
            (should (equal (omnidiff-file-before-source f) ""))
            (should (equal (omnidiff-file-after-source f) ""))))))))

(ert-deftest omnidiff-git-test/multi-file-review-workflow-and-oid-invalidation ()
  "Multi-file reviewed state syncs Overview<->Dual and invalidates on edits."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (should (eq (keymap-lookup omnidiff-mode-map "R") #'omnidiff-toggle-reviewed))
  (should (eq (keymap-lookup omnidiff-dual-mode-map "R") #'omnidiff-dual-toggle-reviewed))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (let ((buf (generate-new-buffer " *omnidiff-review-ov*"))
            (dual-sess nil)
            (standalone-sess nil))
        (with-temp-file (expand-file-name "a.rs" repo) (insert "fn a() { 1 }\n"))
        (with-temp-file (expand-file-name "b.rs" repo) (insert "fn b() { 1 }\n"))
        (with-temp-file (expand-file-name "c.rs" repo) (insert "fn c() { 1 }\n"))
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "base")
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
          ;; 1. Overview `R': mark a.rs reviewed -> ✓, fold a.rs, jump to b.rs.
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
          (should (equal (omnidiff-dual-session-display-path dual-sess) "b.rs")))
        (with-current-buffer (omnidiff-dual-session-after-buf dual-sess)
          (let ((hl (substring-no-properties (omnidiff-dual--header-line))))
            (should (string-search "file 2/3 · reviewed 1/3" hl))
            (should-not (string-search " ✓ " hl)))
          (should (eq (omnidiff-dual-toggle-reviewed) t)))
        (should (equal (omnidiff-dual-session-display-path dual-sess) "c.rs"))
        (with-current-buffer buf
          (should (omnidiff--file-reviewed-p "b.rs"))
          (should (= (omnidiff-render-reviewed-count) 2))
          (should (eq (overlay-get (get-text-property (omnidiff--find-file-header-pos "b.rs")
                                                      'omnidiff-body-ov)
                                   'invisible)
                      'omnidiff-fold)))
        (with-current-buffer (omnidiff-dual-session-after-buf dual-sess)
          (should (string-search "file 3/3 · reviewed 2/3"
                                 (substring-no-properties (omnidiff-dual--header-line))))
          (should (eq (omnidiff-dual-toggle-reviewed) t)))
        (should (equal (omnidiff-dual-session-display-path dual-sess) "c.rs"))
        (with-current-buffer (omnidiff-dual-session-after-buf dual-sess)
          (let ((hl (substring-no-properties (omnidiff-dual--header-line))))
            (should (string-search " ✓ " hl))
            (should (string-search "file 3/3 · reviewed 3/3" hl)))
          (should-not (omnidiff-dual-toggle-reviewed))
          (let ((hl (substring-no-properties (omnidiff-dual--header-line))))
            (should-not (string-search " ✓ " hl))
            (should (string-search "file 3/3 · reviewed 2/3" hl))))
        (omnidiff-test--cleanup-session dual-sess)
        ;; Standalone Dual without Overview refuses `R'.
        (setq standalone-sess (omnidiff-dual-open
                               :before-source "a\n" :after-source "b\n"
                               :display-path "solo.rs"))
        (with-current-buffer (omnidiff-dual-session-after-buf standalone-sess)
          (should-error (omnidiff-dual-toggle-reviewed) :type 'user-error))
        (omnidiff-test--cleanup-session standalone-sess)
        ;; 3. Worktree content invalidation on `g' refresh.
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
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "commit-v1")
        (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                      "HEAD" repo 'default 'unified)
        (omnidiff-test--wait-request buf)
        (with-current-buffer buf
          (should (= (omnidiff-render-reviewed-count) 0))
          (omnidiff--set-file-reviewed "a.rs" t)
          (omnidiff--set-file-reviewed "b.rs" t)
          (should (= (omnidiff-render-reviewed-count) 2)))
        (with-temp-file (expand-file-name "b.rs" repo) (insert "fn b() { 2026 }\n"))
        (omnidiff-test--git repo "add" "b.rs")
        (omnidiff-test--git repo "commit" "-q" "--amend" "--no-edit")
        (with-current-buffer buf
          (omnidiff-refresh))
        (omnidiff-test--wait-request buf)
        (with-current-buffer buf
          (should (omnidiff--file-reviewed-p "a.rs"))
          (should-not (omnidiff--file-reviewed-p "b.rs"))
          (should (= (omnidiff-render-reviewed-count) 1))
          (should (string-search "reviewed 1/3"
                                 (substring-no-properties (omnidiff-render-header-line)))))))))

(ert-deftest omnidiff-git-test/overview-auto-fold-threshold-ret-dual-and-magit-workflow ()
  "Auto-fold threshold, RET->Dual, Magit target/refresh, stash, and untracked."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (dolist (name '("f1.py" "f2.py" "f3.py" "f4.py"))
        (with-temp-file (expand-file-name name repo)
          (insert "def f():\n    x = 1\n    y = 2\n    return x + y\n")))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "init")
      ;; Create a stash modifying f1.py, then restore working edits across all 4 files.
      (with-temp-file (expand-file-name "f1.py" repo)
        (insert "def f():\n    x = 99\n    y = 2\n    return x + y\n"))
      (omnidiff-test--git repo "stash" "push" "-q" "-m" "wip-stash")
      (dolist (pair '(("f1.py" . 10) ("f2.py" . 20) ("f3.py" . 30) ("f4.py" . 40)))
        (with-temp-file (expand-file-name (car pair) repo)
          (insert (format "def f():\n    x = %d\n    y = 2\n    return x + y\n"
                          (cdr pair)))))
      (with-temp-file (expand-file-name "untracked.py" repo)
        (insert "print('hello')\n"))
      ;; 1. Auto-fold threshold folds >= 4 files; RET on folded header opens Dual.
      (let ((omnidiff-overview-auto-fold-threshold 4)
            (omnidiff-single-file-entry 'overview)
            (buf (get-buffer-create "*omnidiff-wf-overview*")))
        (omnidiff-run buf '("git" "--no-pager" "diff" "--ext-diff")
                      "unstaged" repo 'minimal 'unified nil nil nil)
        (omnidiff-test--wait-request buf)
        (with-current-buffer buf
          (dolist (name '("f1.py" "f2.py" "f3.py" "f4.py"))
            (let* ((hdr (omnidiff--find-file-header-pos name))
                   (ov (and hdr (get-text-property hdr 'omnidiff-body-ov))))
              (should hdr)
              (should ov)
              (should (eq (overlay-get ov 'invisible) 'omnidiff-fold))))
          (goto-char (omnidiff--find-file-header-pos "f2.py"))
          (let ((dual-sess (omnidiff-jump-to-source)))
            (should (omnidiff-dual-session-p dual-sess))
            (should (equal (omnidiff-dual-session-display-path dual-sess) "f2.py"))
            (should (= (plist-get (omnidiff-dual--session-nav-callbacks dual-sess) :file-total)
                       4))
            (omnidiff-test--cleanup-session dual-sess))))
      ;; 2. Magit DWIM with target-spec lands in Dual on file+line & refreshes Magit on quit.
      (let* ((omnidiff-overview-auto-fold-threshold 4)
             (omnidiff-magit-file-entry 'dual)
             (omnidiff-single-file-entry 'auto)
             (refreshed nil)
             (fake-magit (generate-new-buffer " *fake-magit*")))
        (cl-letf (((symbol-function 'magit-refresh)
                   (lambda () (setq refreshed t))))
          (with-current-buffer fake-magit
            (put 'omnidiff-git-test--fake-magit-mode 'derived-mode-parent 'magit-mode)
            (setq major-mode 'omnidiff-git-test--fake-magit-mode))
          (omnidiff-diff nil nil
                         (list :file "f3.py" :line 2 :side 'after
                               :origin-buf fake-magit))
          (omnidiff-test--wait-request (get-buffer "*git diff omnidiff*"))
          (let ((dual-sess (car omnidiff-dual--sessions)))
            (should (omnidiff-dual-session-p dual-sess))
            (should (equal (omnidiff-dual-session-display-path dual-sess) "f3.py"))
            (should (= (plist-get (omnidiff-dual--session-nav-callbacks dual-sess) :file-total)
                       4))
            (with-current-buffer (omnidiff-dual-session-after-buf dual-sess)
              (should (= (line-number-at-pos) 2)))
            (setf (omnidiff-dual--session-mutated-p dual-sess) t)
            (omnidiff-test--cleanup-session dual-sess)
            (should refreshed))))
      ;; 3. Magit stash diff runs cleanly via `(stash . "stash@{0}")'.
      (let ((omnidiff-single-file-entry 'overview))
        (omnidiff-diff (cons 'stash "stash@{0}"))
        (let ((stash-buf (get-buffer "*git diff omnidiff stash@{0}*")))
          (should (buffer-live-p stash-buf))
          (omnidiff-test--wait-request stash-buf)
          (with-current-buffer stash-buf
            (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-search "f1.py" txt))
              (should (string-search "99" txt))))))
      ;; 4. Untracked file DWIM opens Dual comparing `/dev/null' vs `untracked.py'.
      (let ((untracked-sess
             (omnidiff-diff nil nil (list :file "untracked.py" :untracked t))))
        (should (omnidiff-dual-session-p untracked-sess))
        (should (equal (omnidiff-dual-session-before-label untracked-sess) "/dev/null"))
        (should (string-search "untracked.py"
                               (omnidiff-dual-session-display-path untracked-sess)))
        (omnidiff-test--cleanup-session untracked-sess)))))

;;; New P0/P1 regression tests for `omnidiff-git.el' & `omnidiff.el'

(ert-deftest omnidiff-git-test/serial-batch-read-memory-cap ()
  "`omnidiff--batch-read-git-blobs' skips oversized blobs via `--batch-check'."
  (skip-unless (executable-find "git"))
  (omnidiff-test--with-git-repo repo
    (with-temp-file (expand-file-name "small.txt" repo) (insert "hello\n"))
    (with-temp-file (expand-file-name "large.txt" repo)
      (insert (make-string 2048 ?x) "\n"))
    (omnidiff-test--git repo "add" ".")
    (omnidiff-test--git repo "commit" "-q" "-m" "seed")
    (let ((oid-small (omnidiff-test--git repo "rev-parse" "HEAD:small.txt"))
          (oid-large (omnidiff-test--git repo "rev-parse" "HEAD:large.txt")))
      ;; 1. Mixed small + large: small is read, large is skipped and marked in SKIPPED-OUT.
      (let* ((omnidiff-max-parallel-blob-bytes 256)
             (skipped (make-hash-table :test #'equal))
             (blobs (omnidiff--batch-read-git-blobs (list oid-small oid-large) repo skipped)))
        (should (equal (gethash oid-small blobs) "hello\n"))
        (should-not (gethash oid-large blobs))
        (should (gethash oid-large skipped))
        (should-not (gethash oid-small skipped))
        ;; `--read-serial-side-source' returns `too-large' for skipped OIDs and big worktree files.
        (should (eq (omnidiff--read-serial-side-source
                     oid-large "100644" "large.txt" "tmp" "utf-8" repo blobs skipped)
                    'too-large))
        (should (eq (omnidiff--read-serial-side-source
                     nil "100644" "large.txt" "tmp" "utf-8" repo blobs skipped)
                    'too-large))
        (should (equal (omnidiff--read-serial-side-source
                        oid-small "100644" "small.txt" "tmp" "utf-8" repo blobs skipped)
                       "hello\n")))
      ;; 2. When every blob exceeds budget, `--batch' is never even spawned!
      (omnidiff-test--with-temp-dir wrap-dir
        (let* ((real-git (executable-find "git"))
               (git-log (expand-file-name "git.log" wrap-dir))
               (fake-git (expand-file-name "git" wrap-dir)))
          (with-temp-file fake-git
            (insert (format "#!/bin/sh\nprintf '%%s\\n' \"$*\" >> %s\nexec %s \"$@\"\n"
                            (shell-quote-argument git-log)
                            (shell-quote-argument real-git))))
          (set-file-modes fake-git #o755)
          (let* ((exec-path (cons wrap-dir exec-path))
                 (omnidiff-max-parallel-blob-bytes 256)
                 (skipped (make-hash-table :test #'equal))
                 (blobs (omnidiff--batch-read-git-blobs (list oid-large) repo skipped))
                 (log-txt (omnidiff-engine-read-file-decoded git-log)))
            (should (= (hash-table-count blobs) 0))
            (should (gethash oid-large skipped))
            (should (string-search "cat-file --batch-check" log-txt))
            (should-not (string-search "cat-file --batch\n" log-txt))))))))

(ert-deftest omnidiff-git-test/stderr-proc-cross-stage-isolation ()
  "Stage 1 stderr output/close never pollutes stage 2 failure diagnostics."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true")))
    (omnidiff-test--with-temp-dir dir
      (omnidiff-test--with-buffer-cleanup
        (let ((buf (generate-new-buffer " *omnidiff-stderr-iso*"))
              (msg-mark (with-current-buffer (messages-buffer) (point-max)))
              (stage1-stderr nil)
              (stage2-proc nil))
          (with-current-buffer buf
            (omnidiff-mode)
            (let* ((req-id (setq omnidiff--request-id (1+ omnidiff--request-id)))
                   (req (make-omnidiff--request
                         :id req-id :buffer buf :bin omnidiff-executable
                         :command '("sh" "-c" "exit 1") :label "iso" :repo-dir dir)))
              ;; Stage 1 prints noise to stderr and succeeds, transitioning to stage 2
              ;; which fails with its own delayed stderr message.
              (omnidiff-git--spawn-command
               req
               '("sh" "-c" "echo STAGE1-NOISE >&2; printf 'ok'")
               nil
               (lambda (_raw)
                 (setq stage2-proc
                       (omnidiff-git--spawn-command
                        req
                        '("sh" "-c" "sleep 0.06; echo STAGE2-REAL-ERR >&2; exit 9")
                        nil
                        (lambda (_raw2)
                          (ert-fail "Stage 2 should have failed"))))
                 ;; Simulate a late close/chunk event on stage 1's old stderr pipe while
                 ;; stage 2 is still running: must NOT mark stage 2 `stderr-done' or pollute chunks.
                 (when (and stage1-stderr (functionp (process-filter stage1-stderr)))
                   (funcall (process-filter stage1-stderr) stage1-stderr "LATE-STAGE1\n"))
                 (when (and stage1-stderr (functionp (process-sentinel stage1-stderr)))
                   (funcall (process-sentinel stage1-stderr) stage1-stderr "finished\n"))
                 (should-not (omnidiff--request-stderr-done req))
                 (should-not (omnidiff--request-err-chunks req))))
              (setq stage1-stderr (omnidiff--request-stderr-proc req))))
          (should (omnidiff-test--wait-until (lambda () stage2-proc) 5.0))
          (omnidiff-test--wait-request buf 5.0)
          (let ((logged (with-current-buffer (messages-buffer)
                          (buffer-substring-no-properties msg-mark (point-max)))))
            (should (string-search "STAGE2-REAL-ERR" logged))
            (should-not (string-search "STAGE1-NOISE" logged))
            (should-not (string-search "LATE-STAGE1" logged))))))))

(ert-deftest omnidiff-git-test/cat-overflow-mid-send-serial-fallback ()
  "`cat-overflow' during send/EOF falls back to serial instead of failing."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true")))
    (omnidiff-test--with-temp-dir dir
      (omnidiff-test--with-buffer-cleanup
        (let* ((fake-bin (expand-file-name "bin" dir))
               (fake-git (expand-file-name "git" fake-bin))
               (buf (generate-new-buffer " *omnidiff-cat-ovf*"))
               (fail-called nil)
               (orig-fail (symbol-function 'omnidiff-git--fail-request))
               ;; Large SHA batch (> 128 KB) ensures pipe write encounters immediate
               ;; overflow output and early EOF from fake `git cat-file --batch'.
               (big-batch (make-list 3500 (make-string 40 ?a))))
          (make-directory fake-bin t)
          (with-temp-file fake-git
            (insert "#!/bin/sh\n"
                    "dd if=/dev/zero bs=65536 count=3 2>/dev/null\n"
                    "exit 0\n"))
          (set-file-modes fake-git #o755)
          (with-current-buffer buf
            (omnidiff-mode)
            (let* ((req-id (setq omnidiff--request-id (1+ omnidiff--request-id)))
                   (req (make-omnidiff--request
                         :id req-id :buffer buf :bin omnidiff-executable
                         :command '("sh" "-c" "printf 'commit serial-fallback-ok\\n'")
                         :label "ovf" :repo-dir dir)))
              (setq omnidiff--active-request req)
              (cl-letf (((symbol-function 'omnidiff-git--fail-request)
                         (lambda (&rest args)
                           (setq fail-called t)
                           (apply orig-fail args))))
                (let ((exec-path (cons fake-bin exec-path)))
                  (omnidiff-git--run-cat-batch
                   req big-batch 16
                   (lambda (_cat-buf)
                     (ert-fail "Expected overflow serial fallback, not on-done"))))
                (omnidiff-test--wait-request buf 5.0)
                (should (omnidiff--request-cat-overflow req)))))
          (should-not fail-called)
          (with-current-buffer buf
            (should-not omnidiff--active-request)
            (should (string-search "commit serial-fallback-ok"
                                   (buffer-substring-no-properties (point-min) (point-max)))))
          (should-not (cl-some (lambda (b) (string-prefix-p " *omnidiff-cat-file*" (buffer-name b)))
                               (buffer-list))))))))

(ert-deftest omnidiff-git-test/trailing-empty-commit-in-multi-commit-show ()
  "Trailing empty commits in multi-commit `git show' render as placeholders."
  ;; 1. Parser unit check on serial output with trailing empty commit header.
  (let* ((json-body "{\"before\":{\"path\":\"old\",\"hunks\":[]},\"after\":{\"path\":\"new\",\"hunks\":[]},\"large_residual\":false,\"summary\":\"no_changes\"}")
         (raw (concat
               "commit 1111111\nAuthor: Alice\n\n    first commit\n\n"
               (omnidiff-test--file-meta "a.txt" "old" "new")
               json-body omnidiff-git-end-sentinel
               "\ncommit 2222222\nAuthor: Alice\n\n    trailing empty commit\n"))
         (diff (omnidiff--parse-serial-json-output raw nil))
         (files (omnidiff-diff-files diff)))
    (should (= (length files) 2))
    (should (equal (omnidiff-file-target-path (nth 0 files)) "a.txt"))
    (should (equal (omnidiff-file-target-path (nth 1 files)) ""))
    (should (string-search "trailing empty commit" (omnidiff-file-preamble (nth 1 files))))
    (should (equal (omnidiff-file-note (nth 1 files)) "No file changes in commit")))
  ;; 2. Real Git repo `git show HEAD~2..HEAD' with trailing empty commit.
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (with-temp-file (expand-file-name "a.txt" repo) (insert "v1\n"))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "base")
      (omnidiff-test--git repo "commit" "-q" "--allow-empty" "-m" "trailing-empty-commit")
      (with-temp-file (expand-file-name "a.txt" repo) (insert "v2\n"))
      (omnidiff-test--git repo "commit" "-q" "-am" "top-non-empty")
      (let ((buf (generate-new-buffer " *omnidiff-trailing-empty*")))
        (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD~2..HEAD")
                      "show range" repo 'default 'unified)
        (omnidiff-test--wait-request buf)
        (with-current-buffer buf
          (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-search "top-non-empty" txt))
            (should (string-search "trailing-empty-commit" txt))
            (should (string-search "No file changes in commit" txt))))))))

(ert-deftest omnidiff-git-test/multi-commit-same-path-collision ()
  "Same-path multi-commit cards resolve file-at-point, headers, fold, and `R' accurately."
  (omnidiff-test--with-buffer-cleanup
    (let* ((f1 (omnidiff-test--make-json-file
                "dup.rs" "let v = 1;\n" "let v = 2;\n"
                :old-oid (make-string 40 ?1) :new-oid (make-string 40 ?2)
                :before-hunks (list (omnidiff-test--make-hunk 'update 0 8 0 9))
                :after-hunks (list (omnidiff-test--make-hunk 'update 0 8 0 9))))
           (f2 (omnidiff-test--make-json-file
                "dup.rs" "let v = 2;\n" "let v = 3;\n"
                :old-oid (make-string 40 ?2) :new-oid (make-string 40 ?3)
                :before-hunks (list (omnidiff-test--make-hunk 'update 0 8 0 9))
                :after-hunks (list (omnidiff-test--make-hunk 'update 0 8 0 9))))
           (_ (setf (omnidiff-file-preamble f2) "commit second\n"))
           (diff (omnidiff-test--make-diff (list f1 f2) "commit first\n"))
           (buf (generate-new-buffer " *omnidiff-same-path*")))
      (with-current-buffer buf
        (omnidiff-mode)
        (setq omnidiff--diff diff
              omnidiff--layout 'unified)
        (omnidiff-render-buffer diff)
        (let ((pos1 (omnidiff--find-file-header-pos f1))
              (pos2 (omnidiff--find-file-header-pos f2)))
          (should (integerp pos1))
          (should (integerp pos2))
          (should (< pos1 pos2))
          ;; `omnidiff--file-at-point' returns exact commit instance on header & body.
          (goto-char pos1)
          (should (eq (omnidiff--file-at-point) f1))
          (forward-line 2)
          (should (eq (omnidiff--file-at-point) f1))
          (goto-char pos2)
          (should (eq (omnidiff--file-at-point) f2))
          (forward-line 2)
          (should (eq (omnidiff--file-at-point) f2))
          ;; Folding on pos2 folds ONLY f2, leaving f1 visible.
          (goto-char pos2)
          (omnidiff-toggle-fold)
          (should (eq (overlay-get (get-text-property pos2 'omnidiff-body-ov) 'invisible)
                      'omnidiff-fold))
          (should-not (overlay-get (get-text-property pos1 'omnidiff-body-ov) 'invisible))
          (omnidiff-toggle-fold)
          ;; Pressing `R' on pos2 marks & folds f2's exact header, not f1's!
          (goto-char pos2)
          (should (eq (omnidiff-toggle-reviewed) t))
          (should (equal (substring-no-properties
                          (overlay-get (get-text-property pos2 'omnidiff-review-ov) 'before-string))
                         " ✓"))
          (should (eq (overlay-get (get-text-property pos2 'omnidiff-body-ov) 'invisible)
                      'omnidiff-fold))
          (should-not (overlay-get (get-text-property pos1 'omnidiff-body-ov) 'invisible)))))))

(ert-deftest omnidiff-git-test/symlink-worktree-guard-120000 ()
  "`omnidiff--worktree-after-path' returns nil for symlinks and mode `120000'."
  (omnidiff-test--with-temp-dir dir
    (let ((target (expand-file-name "target.txt" dir))
          (link (expand-file-name "link.txt" dir))
          (wt-cmd '("git" "--no-pager" "diff" "--ext-diff")))
      (with-temp-file target (insert "real content\n"))
      (make-symbolic-link "target.txt" link)
      (should (file-symlink-p link))
      (should (file-regular-p link))
      ;; Symlink with mode 120000 -> nil.
      (let ((f-sym (omnidiff-test--make-file :path "link.txt" :target-path "link.txt"
                                             :status 'modified :new-mode "120000")))
        (should-not (omnidiff--worktree-after-path f-sym dir wt-cmd)))
      ;; Symlink even without explicit 120000 mode -> nil (`file-symlink-p' guard).
      (let ((f-sym-plain (omnidiff-test--make-file :path "link.txt" :target-path "link.txt"
                                                   :status 'modified :new-mode "100644")))
        (should-not (omnidiff--worktree-after-path f-sym-plain dir wt-cmd)))
      ;; Regular file marked as mode 120000 -> nil.
      (let ((f-mode-sym (omnidiff-test--make-file :path "target.txt" :target-path "target.txt"
                                                  :status 'modified :new-mode "120000")))
        (should-not (omnidiff--worktree-after-path f-mode-sym dir wt-cmd)))
      ;; Positive control: genuine regular file in unstaged worktree diff -> expanded path.
      (let ((f-reg (omnidiff-test--make-file :path "target.txt" :target-path "target.txt"
                                             :status 'modified :new-mode "100644")))
        (should (equal (omnidiff--worktree-after-path f-reg dir wt-cmd)
                       (omnidiff-engine-expand-repo-path "target.txt" dir)))))))

(ert-deftest omnidiff-git-test/toggle-flags-no-premature-mutation-on-failure ()
  "`W' and `I' do not mutate buffer flags when async `omnidiff-run' fails."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true")))
    (omnidiff-test--with-temp-dir dir
      (omnidiff-test--with-buffer-cleanup
        (let ((buf (generate-new-buffer " *omnidiff-toggle-fail*")))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--command '("sh" "-c" "sleep 0.05; exit 1")
                  omnidiff--label "fail-toggle"
                  omnidiff--repo-dir dir
                  omnidiff--whole-updates nil
                  omnidiff--paint-reindent-moves nil)
            ;; 1. Failed `omnidiff-toggle-whole-updates' leaves `omnidiff--whole-updates' nil.
            (omnidiff-toggle-whole-updates)
            (should (omnidiff-git-effective-whole-updates))
            (should-not omnidiff--whole-updates)
            (omnidiff-test--wait-request buf 5.0)
            (should-not omnidiff--whole-updates)
            (should-not (omnidiff-git-effective-whole-updates))
            ;; 2. Failed `omnidiff-toggle-paint-reindent-moves' leaves flag nil.
            (omnidiff-toggle-paint-reindent-moves)
            (should (omnidiff-git-effective-paint-reindent-moves))
            (should-not omnidiff--paint-reindent-moves)
            (omnidiff-test--wait-request buf 5.0)
            (should-not omnidiff--paint-reindent-moves)
            (should-not (omnidiff-git-effective-paint-reindent-moves))
            ;; 3. Succeeding command commits both toggled flags.
            (setq omnidiff--command '("sh" "-c" "printf 'commit ok\\n'"))
            (omnidiff-toggle-whole-updates)
            (omnidiff-test--wait-request buf 5.0)
            (should omnidiff--whole-updates)
            (omnidiff-toggle-paint-reindent-moves)
            (omnidiff-test--wait-request buf 5.0)
            (should omnidiff--paint-reindent-moves)))))))

(ert-deftest omnidiff-git-test/bounded-restore-location ()
  "`omnidiff--restore-location' stays bounded within its target file card."
  (omnidiff-test--with-buffer-cleanup
    (let* ((fa (omnidiff-test--make-json-file
                "a.rs" "line1\nline2\nline3\n" "line1\nLINE2\nline3\n"
                :before-hunks (list (omnidiff-test--make-hunk 'update 1 0 1 5))
                :after-hunks (list (omnidiff-test--make-hunk 'update 1 0 1 5))))
           (b-lines (mapconcat (lambda (n) (format "bline-%d" n))
                               (number-sequence 1 60) "\n"))
           (b-after (replace-regexp-in-string "bline-50" "BLINE-50" b-lines))
           (fb (omnidiff-test--make-json-file
                "b.rs" (concat b-lines "\n") (concat b-after "\n")
                :before-hunks (list (omnidiff-test--make-hunk 'update 49 0 49 8))
                :after-hunks (list (omnidiff-test--make-hunk 'update 49 0 49 8))))
           (diff (omnidiff-test--make-diff (list fa fb)))
           (buf (generate-new-buffer " *omnidiff-bounded-restore*")))
      (with-current-buffer buf
        (omnidiff-mode)
        (setq omnidiff--diff diff
              omnidiff--layout 'unified)
        (omnidiff-render-buffer diff)
        ;; Asking for line 50 in `a.rs' (which only has lines 1..3) MUST stay in `a.rs'
        ;; and never leak into `b.rs' line 50!
        (omnidiff--restore-location fa nil 50 'after)
        (should (equal (get-text-property (point) 'omnidiff-file) "a.rs"))
        (should (< (point) (omnidiff--find-file-header-pos fb)))
        ;; Restoring onto a lazy-folded file materializes & unfolds it cleanly.
        (let ((omnidiff-overview-auto-fold-threshold 2))
          (omnidiff-render-buffer diff))
        (let ((fb-ov (get-text-property (omnidiff--find-file-header-pos fb) 'omnidiff-body-ov)))
          (should (overlay-get fb-ov 'omnidiff-lazy-file))
          (omnidiff--restore-location fb nil 50 'after)
          (should-not (overlay-get fb-ov 'omnidiff-lazy-file))
          (should-not (invisible-p (point)))
          (should (equal (get-text-property (point) 'omnidiff-file) "b.rs"))
          (should (eql (get-text-property (point) 'omnidiff-new-line) 50)))))))

(ert-deftest omnidiff-git-test/pdf-rasterizer-exit-code-and-zero-byte-rejection ()
  "PDF drilldown rejects non-zero exit codes (even with output) and 0-byte PNGs."
  (omnidiff-test--with-temp-dir dir
    (let* ((dummy-pdf (expand-file-name "doc.pdf" dir))
           (valid-png (expand-file-name "valid.png" dir))
           (bin-err (expand-file-name "bin-err" dir))
           (bin-zero (expand-file-name "bin-zero" dir))
           (pdf-file (omnidiff-test--make-file
                      :path "doc.pdf" :target-path "doc.pdf" :status 'modified
                      :before-disk-path dummy-pdf :after-disk-path dummy-pdf
                      :container (make-omnidiff-json-container
                                  :family 'documents
                                  :before (make-omnidiff-json-container-side :format "PDF")
                                  :after (make-omnidiff-json-container-side :format "PDF")))))
      (make-directory bin-err t)
      (make-directory bin-zero t)
      (with-temp-file dummy-pdf (insert "%PDF-1.4\n"))
      (omnidiff-test--write-bin valid-png omnidiff-test--png-red-2x2)
      ;; Case 1: `pdftoppm' writes non-empty PNG bytes to `<prefix>.png' but exits 1.
      (let ((p-err (expand-file-name "pdftoppm" bin-err)))
        (with-temp-file p-err
          (insert (format "#!/bin/sh\nfor last; do :; done\ncp %s \"${last}.png\"\nexit 1\n"
                          (shell-quote-argument valid-png))))
        (set-file-modes p-err #o755))
      ;; Case 2: `pdftoppm' exits 0 but creates a 0-byte `<prefix>.png'.
      (let ((p-zero (expand-file-name "pdftoppm" bin-zero)))
        (with-temp-file p-zero
          (insert "#!/bin/sh\nfor last; do :; done\n: > \"${last}.png\"\nexit 0\n"))
        (set-file-modes p-zero #o755))
      (let ((before-tmps (directory-files temporary-file-directory nil "\\`omnidiff-pdf-")))
        (let ((exec-path (list bin-err)))
          (let ((err (should-error
                      (omnidiff--drilldown-pdf-page pdf-file "page 1" dir nil)
                      :type 'user-error)))
            (should (string-search "Could not rasterize PDF page 1"
                                   (error-message-string err)))))
        (let ((exec-path (list bin-zero)))
          (let ((err (should-error
                      (omnidiff--drilldown-pdf-page pdf-file "page 1" dir nil)
                      :type 'user-error)))
            (should (string-search "Could not rasterize PDF page 1"
                                   (error-message-string err)))))
        (should (equal (directory-files temporary-file-directory nil "\\`omnidiff-pdf-")
                       before-tmps))))))

(ert-deftest omnidiff-git-test/non-buffer-custom-tui-launcher-retains-tmpdir ()
  "Non-buffer custom TUI launcher keeps `tmpdir' alive until caller buffer dies."
  (let ((omnidiff-executable (or (executable-find "true") "/usr/bin/true"))
        (orig-ext omnidiff--ext-tui-tmpdirs)
        (captured-dir nil))
    (unwind-protect
        (omnidiff-test--with-temp-dir dir
          (omnidiff-test--with-buffer-cleanup
            (let* ((file (omnidiff-test--make-json-file
                          "demo.py" "x = 1\n" "x = 2\n"
                          :before-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))
                          :after-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))))
                   (diff (omnidiff-test--make-diff file))
                   (caller (generate-new-buffer " *omnidiff-ext-tui-caller*")))
              (with-current-buffer caller
                (omnidiff-mode)
                (setq omnidiff--repo-dir dir
                      omnidiff--diff diff)
                (omnidiff-render-buffer diff)
                (goto-char (point-min))
                (let ((omnidiff-tui-launcher
                       (lambda (argv _buf-name _dir)
                         ;; Return a non-buffer (e.g. external terminal spawned async).
                         (should (equal (nth 3 argv) "--"))
                         (should (file-exists-p (nth 4 argv)))
                         (should (file-exists-p (nth 5 argv)))
                         'external-spawned)))
                  (should (eq (omnidiff-open-in-tui) 'external-spawned)))
                ;; Tmpdir must remain alive on disk and tracked in caller's `--tui-tmpdirs'.
                (setq captured-dir (car omnidiff--tui-tmpdirs))
                (should (and (stringp captured-dir) (file-directory-p captured-dir)))
                (should (member captured-dir omnidiff--ext-tui-tmpdirs)))
              ;; Killing the caller buffer deletes tmpdir and removes it from `--ext-tui-tmpdirs'.
              (kill-buffer caller)
              (should-not (file-directory-p captured-dir))
              (should-not (member captured-dir omnidiff--ext-tui-tmpdirs)))))
      (setq omnidiff--ext-tui-tmpdirs orig-ext)
      (when (and captured-dir (file-directory-p captured-dir))
        (delete-directory captured-dir t)))))

(ert-deftest omnidiff-git-test/toggle-fold-all-batch-cache-single-hook-unfold ()
  "`omnidiff-toggle-fold-all' unfolds lazy files in one batch cache and one hook."
  (omnidiff-test--with-buffer-cleanup
    (let* ((files
            (mapcar (lambda (i)
                      (omnidiff-test--make-json-file
                       (format "f%d.py" i)
                       (format "old_%d = 1\n" i)
                       (format "new_%d = 2\n" i)
                       :before-hunks (list (omnidiff-test--make-hunk 'update 0 0 0 5))
                       :after-hunks (list (omnidiff-test--make-hunk 'update 0 0 0 5))))
                    '(1 2 3 4)))
           (diff (omnidiff-test--make-diff files))
           (buf (generate-new-buffer " *omnidiff-batch-unfold*"))
           (omnidiff-overview-auto-fold-threshold 2))
      (with-current-buffer buf
        (omnidiff-mode)
        (setq omnidiff--diff diff
              omnidiff--layout 'unified)
        (omnidiff-render-buffer diff)
        (let ((body-ovs (cl-remove-if-not
                         (lambda (ov) (overlay-get ov 'omnidiff-file-body))
                         (overlays-in (point-min) (point-max))))
              (batch-calls 0)
              (before-calls 0)
              (after-calls 0)
              (orig-batch (symbol-function 'omnidiff-render-call-with-batch-caches)))
          (should (= (length body-ovs) 4))
          (should (cl-every (lambda (ov) (overlay-get ov 'omnidiff-lazy-file)) body-ovs))
          (should (cl-every (lambda (ov) (eq (overlay-get ov 'invisible) 'omnidiff-fold))
                            body-ovs))
          (add-hook 'before-change-functions
                    (lambda (&rest _) (setq before-calls (1+ before-calls))) nil t)
          (add-hook 'after-change-functions
                    (lambda (&rest _) (setq after-calls (1+ after-calls))) nil t)
          (cl-letf (((symbol-function 'omnidiff-render-call-with-batch-caches)
                     (lambda (fn)
                       (setq batch-calls (1+ batch-calls))
                       (funcall orig-batch fn))))
            (omnidiff-toggle-fold-all))
          (should (= batch-calls 1))
          (should (= before-calls 1))
          (should (= after-calls 1))
          (should-not (cl-some (lambda (ov) (overlay-get ov 'omnidiff-lazy-file)) body-ovs))
          (should-not (cl-some (lambda (ov) (overlay-get ov 'invisible)) body-ovs))
          (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
            (dolist (i '(1 2 3 4))
              (should (string-search (format "new_%d = 2" i) txt)))))))))

(ert-deftest omnidiff-git-test/asymmetric-oversized-blob-note-and-no-dual ()
  "Asymmetric oversized blobs show TUI note (not fake diff) and block Dual."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      ;; Commit 1: small BEFORE; Commit 2: >budget AFTER; Commit 3: small AFTER.
      (with-temp-file (expand-file-name "asym.txt" repo)
        (insert "tiny-before-token\n"))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "small-v1")
      (with-temp-file (expand-file-name "asym.txt" repo)
        (insert (make-string 512 ?x) "\n"))
      (omnidiff-test--git repo "commit" "-q" "-am" "huge-v2")
      (with-temp-file (expand-file-name "asym.txt" repo)
        (insert "tiny-after-token\n"))
      (omnidiff-test--git repo "commit" "-q" "-am" "small-v3")
      ;; 1. Small BEFORE vs oversized AFTER (`HEAD~1`): stays in Overview with note.
      (let ((buf (generate-new-buffer " *omnidiff-asym-grow*"))
            (dual-opened nil)
            (omnidiff-max-parallel 4)
            (omnidiff-max-parallel-blob-bytes 64)
            (omnidiff-single-file-entry 'auto))
        (cl-letf (((symbol-function 'omnidiff-dual-open)
                   (lambda (&rest _)
                     (setq dual-opened t)
                     (error "Should not open Dual for oversized blob"))))
          (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD~1")
                        "asym-grow" repo 'default 'unified nil nil t)
          (omnidiff-test--wait-request buf))
        (should-not dual-opened)
        (with-current-buffer buf
          (let ((f (car (omnidiff-diff-files omnidiff--diff)))
                (txt (buffer-substring-no-properties (point-min) (point-max))))
            (should (omnidiff-file-too-large-p f))
            (should (equal (omnidiff-file-before-source f) ""))
            (should (equal (omnidiff-file-after-source f) ""))
            (should-not (omnidiff--file-text-dual-p f))
            (should (string-search "File exceeds blob size limit" (omnidiff-file-note f)))
            (should (string-search "File exceeds blob size limit" txt))
            (should-not (string-search "tiny-before-token" txt))
            (goto-char (omnidiff--find-file-header-pos f))
            (let ((err (should-error (omnidiff-open-dual-at-point) :type 'user-error)))
              (should (string-search "File exceeds blob size limit" (error-message-string err)))
              (should (string-search "T" (error-message-string err)))))))
      ;; 2. Reverse direction: oversized BEFORE vs small AFTER (`HEAD`).
      (let ((buf2 (generate-new-buffer " *omnidiff-asym-shrink*"))
            (omnidiff-max-parallel 4)
            (omnidiff-max-parallel-blob-bytes 64)
            (omnidiff-single-file-entry 'auto))
        (omnidiff-run buf2 '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                      "asym-shrink" repo 'default 'unified nil nil t)
        (omnidiff-test--wait-request buf2)
        (with-current-buffer buf2
          (let ((f (car (omnidiff-diff-files omnidiff--diff)))
                (txt (buffer-substring-no-properties (point-min) (point-max))))
            (should (omnidiff-file-too-large-p f))
            (should (equal (omnidiff-file-before-source f) ""))
            (should (equal (omnidiff-file-after-source f) ""))
            (should-not (omnidiff--file-text-dual-p f))
            (should (string-search "File exceeds blob size limit" txt))
            (should-not (string-search "tiny-after-token" txt))))))))

(ert-deftest omnidiff-git-test/unstaged-worktree-oversized-parallel-fallback ()
  "Oversized unstaged working-tree files trigger serial fallback under parallel mode."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (let ((wt-file (expand-file-name "wt-big.txt" repo))
            (ita-file (expand-file-name "ita-big.txt" repo))
            (orig-launch (symbol-function 'omnidiff-git--launch-workers))
            (orig-read (symbol-function 'omnidiff-engine-read-file-decoded)))
        (with-temp-file wt-file (insert "small-head-line\n"))
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "base")
        ;; 1. Unstaged modification: small index blob vs oversized working-tree file.
        (with-temp-file wt-file (insert (make-string 512 ?z) "\n"))
        (let ((buf (generate-new-buffer " *omnidiff-wt-big*"))
              (workers-launched nil)
              (read-paths nil)
              (omnidiff-max-parallel 4)
              (omnidiff-max-parallel-blob-bytes 64)
              (omnidiff-single-file-entry 'auto))
          (cl-letf (((symbol-function 'omnidiff-git--launch-workers)
                     (lambda (&rest args)
                       (setq workers-launched t)
                       (apply orig-launch args)))
                    ((symbol-function 'omnidiff-engine-read-file-decoded)
                     (lambda (path &rest args)
                       (push path read-paths)
                       (apply orig-read path args))))
            (omnidiff-run buf '("git" "--no-pager" "diff" "--ext-diff")
                          "wt-big" repo 'default 'unified nil nil t)
            (omnidiff-test--wait-request buf))
          (should-not workers-launched)
          (should-not (cl-some (lambda (p) (string-suffix-p "wt-big.txt" p)) read-paths))
          (with-current-buffer buf
            (let ((f (car (omnidiff-diff-files omnidiff--diff)))
                  (txt (buffer-substring-no-properties (point-min) (point-max))))
              (should (omnidiff-file-too-large-p f))
              (should (equal (omnidiff-file-before-source f) ""))
              (should (equal (omnidiff-file-after-source f) ""))
              (should-not (omnidiff--file-text-dual-p f))
              (should (string-search "File exceeds blob size limit" txt))
              (should-not (string-search "small-head-line" txt)))))
        ;; 2. Intent-to-add (`git add -N`) oversized file (zero old-sha & zero new-sha).
        (omnidiff-test--git repo "checkout" "--" "wt-big.txt")
        (with-temp-file ita-file (insert (make-string 512 ?q) "\n"))
        (omnidiff-test--git repo "add" "-N" "ita-big.txt")
        (let ((buf2 (generate-new-buffer " *omnidiff-ita-big*"))
              (workers-launched nil)
              (read-paths nil)
              (omnidiff-max-parallel 4)
              (omnidiff-max-parallel-blob-bytes 64)
              (omnidiff-single-file-entry 'auto))
          (cl-letf (((symbol-function 'omnidiff-git--launch-workers)
                     (lambda (&rest args)
                       (setq workers-launched t)
                       (apply orig-launch args)))
                    ((symbol-function 'omnidiff-engine-read-file-decoded)
                     (lambda (path &rest args)
                       (push path read-paths)
                       (apply orig-read path args))))
            (omnidiff-run buf2 '("git" "--no-pager" "diff" "--ext-diff")
                          "ita-big" repo 'default 'unified nil nil t)
            (omnidiff-test--wait-request buf2))
          (should-not workers-launched)
          (should-not (cl-some (lambda (p) (string-suffix-p "ita-big.txt" p)) read-paths))
          (with-current-buffer buf2
            (let ((f (car (omnidiff-diff-files omnidiff--diff)))
                  (txt (buffer-substring-no-properties (point-min) (point-max))))
              (should (omnidiff-file-too-large-p f))
              (should (string-search "File exceeds blob size limit" txt)))))))))

(ert-deftest omnidiff-git-test/trailing-empty-commit-file-count-and-review-progress ()
  "Trailing empty commit does not inflate `omnidiff--file-count' or review totals."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (with-temp-file (expand-file-name "a.txt" repo) (insert "v1\n"))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "base")
      (omnidiff-test--git repo "commit" "-q" "--allow-empty" "-m" "trailing-empty-commit")
      (with-temp-file (expand-file-name "a.txt" repo) (insert "v2\n"))
      (omnidiff-test--git repo "commit" "-q" "-am" "top-non-empty")
      (let ((buf (generate-new-buffer " *omnidiff-trailing-empty-rev*")))
        (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD~2..HEAD")
                      "show range" repo 'default 'unified)
        (omnidiff-test--wait-request buf)
        (with-current-buffer buf
          (should (= (length (omnidiff-diff-files omnidiff--diff)) 2))
          (should (= omnidiff--file-count 1))
          (let ((hdr0 (substring-no-properties (omnidiff-render-header-line))))
            (should (string-search "1 file" hdr0))
            (should-not (string-search "2 files" hdr0)))
          (let ((pos-a (omnidiff--find-file-header-pos "a.txt"))
                (msg-buf (messages-buffer)))
            (should pos-a)
            (goto-char pos-a)
            (let ((mark (with-current-buffer msg-buf (point-max))))
              (should (eq (omnidiff-toggle-reviewed) t))
              (should (= (omnidiff-render-reviewed-count) 1))
              (should (string-search "reviewed 1/1"
                                     (substring-no-properties (omnidiff-render-header-line))))
              (let ((out (with-current-buffer msg-buf
                           (buffer-substring-no-properties mark (point-max)))))
                (should (string-search "All 1 file reviewed!" out))))
            (let ((mark (with-current-buffer msg-buf (point-max))))
              (should-not (omnidiff-toggle-reviewed))
              (let ((out (with-current-buffer msg-buf
                           (buffer-substring-no-properties mark (point-max)))))
                (should (string-search "Unmarked a.txt (0/1 reviewed)" out))))))))))

(provide 'omnidiff-git-test)
;;; omnidiff-git-test.el ends here
