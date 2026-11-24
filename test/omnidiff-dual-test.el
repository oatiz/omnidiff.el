;;; omnidiff-dual-test.el --- ERT tests for omnidiff-dual -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; This file is part of omnidiff.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Focused ERT suite for `omnidiff-dual.el': Dual semantic viewer navigation,
;; syntax highlighting, folding, layouts, live editing, semantic revert/undo,
;; emulation keymap precedence, tab-bar restoration, BOM parity, async race
;; safety, and session lifecycle/rollback.

;;; Code:

(require 'omnidiff-test-helper)
(require 'tab-bar)

(defvar omnidiff-dual-test--modal-active nil
  "Buffer-local test flag activating a mock modal emulation keymap.")

;;; Shared fixture builder for Dual Python tests

(defun omnidiff-dual-test--write-python-pair (dir)
  "Write BEFORE and AFTER Python fixtures in DIR and return `(FA . FB)'."
  (let ((fa (expand-file-name "before.py" dir))
        (fb (expand-file-name "after.py" dir))
        (coding-system-for-write 'utf-8-unix))
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
    (cons fa fb)))

(defun omnidiff-dual-test--fold-overlays (buf)
  "Return all `omnidiff-dual' fold overlays in BUF."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (cl-remove-if-not
       (lambda (ov)
         (and (overlay-get ov 'omnidiff-dual-ov)
              (eq (overlay-get ov 'omnidiff-dual-kind) 'fold)))
       (overlays-in (point-min) (point-max))))))

;;; Migrated Dual viewer navigation, folding, live edit, and cleanup tests

(ert-deftest omnidiff-dual-test/syntax-tokens-and-cjk ()
  "Dual renders native Python syntax and exact CJK semantic token overlays."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (pcase-let* ((`(,fa . ,fb) (omnidiff-dual-test--write-python-pair dir))
                   (orig-width (frame-width (selected-frame)))
                   (omnidiff-dual-layout 'auto)
                   (omnidiff-dual-min-width 100)
                   (omnidiff-dual-fold-unchanged nil))
        (unwind-protect
            (progn
              (set-frame-width (selected-frame) 140)
              (let* ((user-buf (find-file-noselect fb))
                     (sess (omnidiff-dual-open :before-path fa
                                               :after-path fb
                                               :editable-after t))
                     (bb (omnidiff-dual-session-before-buf sess))
                     (ab (omnidiff-dual-session-after-buf sess)))
                (should (omnidiff-dual-session-p sess))
                (should (eq ab user-buf))
                (should (omnidiff-dual--pane-owns-buf-p (omnidiff-dual--pane sess 'before)))
                (should-not (omnidiff-dual--pane-owns-buf-p (omnidiff-dual--pane sess 'after)))
                ;; Native python-mode syntax highlighting + non-extending token overlays.
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
                                 (omnidiff-test--token-overlays ab)))))
          (set-frame-width (selected-frame) orig-width))))))

(ert-deftest omnidiff-dual-test/change-stepping-and-move-counterpart ()
  "`n'/`p' wrap across all stops and RET round-trips moved block counterparts."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (pcase-let* ((`(,fa . ,fb) (omnidiff-dual-test--write-python-pair dir))
                   (orig-width (frame-width (selected-frame)))
                   (omnidiff-dual-layout 'auto)
                   (omnidiff-dual-min-width 100)
                   (omnidiff-dual-fold-unchanged nil))
        (unwind-protect
            (progn
              (set-frame-width (selected-frame) 140)
              (let* ((sess (omnidiff-dual-open :before-path fa
                                               :after-path fb
                                               :editable-after t))
                     (bb (omnidiff-dual-session-before-buf sess))
                     (ab (omnidiff-dual-session-after-buf sess))
                     (stops (omnidiff-dual--session-stops sess))
                     (total (length stops)))
                ;; 1. `n'/`p' change stepping and wrap-around.
                (should (>= total 4))
                (should (= (omnidiff-dual--session-stop-index sess) 1))
                (dotimes (i (1- total))
                  (omnidiff-dual-next-change)
                  (should (= (omnidiff-dual--session-stop-index sess) (+ i 2))))
                (omnidiff-dual-next-change)
                (should (= (omnidiff-dual--session-stop-index sess) 1))
                (omnidiff-dual-prev-change)
                (should (= (omnidiff-dual--session-stop-index sess) total))
                (omnidiff-dual-prev-change)
                (should (= (omnidiff-dual--session-stop-index sess) (1- total)))
                ;; 2. RET exact `move_target' jump & round-trip return.
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
                    (let ((ret (omnidiff-dual-jump-counterpart)))
                      (should (eq (car ret) 'before))
                      (should (= (cdr ret) orig-pt))))
                  (should (eq (omnidiff-dual--session-active-side sess) 'before))
                  (should (= (with-current-buffer bb (point)) orig-pt)))))
          (set-frame-width (selected-frame) orig-width))))))

(ert-deftest omnidiff-dual-test/context-folding-and-layout-cycling ()
  "`z' folds unchanged runs with `@' breadcrumbs and `v'/`o' cycle layouts cleanly."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (pcase-let* ((`(,fa . ,fb) (omnidiff-dual-test--write-python-pair dir))
                   (orig-width (frame-width (selected-frame)))
                   (omnidiff-dual-layout 'auto)
                   (omnidiff-dual-min-width 100)
                   (omnidiff-dual-fold-unchanged nil)
                   (omnidiff-dual-context-lines 2))
        (unwind-protect
            (progn
              (set-frame-width (selected-frame) 140)
              (let* ((sess (omnidiff-dual-open :before-path fa
                                               :after-path fb
                                               :editable-after t))
                     (ab (omnidiff-dual-session-after-buf sess)))
                ;; 1. `z' context folding with `@' breadcrumb + RET single-fold expand.
                (omnidiff-dual-toggle-fold)
                (should (omnidiff-dual--session-fold-unchanged sess))
                (let* ((folds (omnidiff-dual-test--fold-overlays ab))
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
                      (should (= (length (omnidiff-dual-test--fold-overlays ab))
                                 (1- initial-count))))))
                (omnidiff-dual-toggle-fold)
                (should-not (omnidiff-dual--session-fold-unchanged sess))
                ;; 2. `v' toggles dual <-> single; `o' flips panes; `C-u v' restores `auto'.
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
                (should (omnidiff-dual--dual-visible-p sess))))
          (set-frame-width (selected-frame) orig-width))))))

(ert-deftest omnidiff-dual-test/live-edit-reuse-and-teardown ()
  "Live edit refresh, `:reuse-session' swap, and `q' teardown clean up resources."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (pcase-let* ((`(,fa . ,fb) (omnidiff-dual-test--write-python-pair dir))
                   (fa2 (expand-file-name "swap_before.py" dir))
                   (fb2 (expand-file-name "swap_after.py" dir))
                   (orig-width (frame-width (selected-frame)))
                   (omnidiff-dual-layout 'auto)
                   (omnidiff-dual-min-width 100)
                   (omnidiff-dual-fold-unchanged nil)
                   (omnidiff-dual-live-update 'on-save))
        (unwind-protect
            (progn
              (set-frame-width (selected-frame) 140)
              (with-temp-file fa2 (insert "def swapped():\n    return 100\n"))
              (with-temp-file fb2 (insert "def swapped():\n    return 234\n"))
              (let* ((user-buf (find-file-noselect fb))
                     (sess (omnidiff-dual-open :before-path fa
                                               :after-path fb
                                               :editable-after t))
                     (bb (omnidiff-dual-session-before-buf sess))
                     (ab (omnidiff-dual-session-after-buf sess)))
                (should (eq ab user-buf))
                ;; 1. `e' live edit unlock -> save triggers async refresh -> lock back.
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
                (omnidiff-test--wait-dual sess)
                (should (cl-some (lambda (ov)
                                   (with-current-buffer ab
                                     (and (eq (overlay-get ov 'face) 'omnidiff-token-insert)
                                          (string-search "live_added_var"
                                                         (buffer-substring-no-properties
                                                          (overlay-start ov) (overlay-end ov))))))
                                 (omnidiff-test--token-overlays ab)))
                (should-not (omnidiff-dual-toggle-edit))
                (omnidiff-test--wait-dual sess)
                (should-not (omnidiff-dual--session-editing-p sess))
                (with-current-buffer ab
                  (should buffer-read-only)
                  (should (bound-and-true-p omnidiff-dual-mode))
                  (should-not (bound-and-true-p omnidiff-dual-edit-mode)))
                ;; 2. `:reuse-session' swaps files in place, kills old BEFORE, preserves user-buf.
                (let ((user-buf2 (find-file-noselect fb2))
                      (old-id (omnidiff-dual--session-id sess)))
                  (let ((reused (omnidiff-dual-open :before-path fa2
                                                    :after-path fb2
                                                    :editable-after t
                                                    :reuse-session sess)))
                    (should (eq reused sess))
                    (should (= (omnidiff-dual--session-id reused) old-id))
                    (should-not (buffer-live-p bb))
                    (should (buffer-live-p user-buf))
                    (with-current-buffer user-buf
                      (should-not buffer-read-only)
                      (should-not (bound-and-true-p omnidiff-dual-mode))
                      (should-not omnidiff-dual--session)
                      (should-not (omnidiff-test--token-overlays user-buf)))
                    (should (eq (omnidiff-dual-session-after-buf sess) user-buf2))
                    (should (cl-some (lambda (ov)
                                       (with-current-buffer user-buf2
                                         (equal (buffer-substring-no-properties
                                                 (overlay-start ov) (overlay-end ov))
                                                "234")))
                                     (omnidiff-test--token-overlays user-buf2))))
                  ;; 3. `q' teardown kills owned BEFORE, cleans tmpdir, preserves user-buf2.
                  (let ((cur-bb (omnidiff-dual-session-before-buf sess))
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
                      (should-not (omnidiff-test--token-overlays user-buf2)))))))
          (set-frame-width (selected-frame) orig-width))))))

(ert-deftest omnidiff-dual-test/entrypoints-files-and-buffers ()
  "`omnidiff-files' on Python files and `omnidiff-buffers' open Dual sessions."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((orig-width (frame-width (selected-frame))))
        (unwind-protect
            (progn
              (set-frame-width (selected-frame) 140)
              ;; 1. `omnidiff-files' on Python pair -> Dual viewer with token overlays.
              (let ((py-a (expand-file-name "left.py" dir))
                    (py-b (expand-file-name "right.py" dir)))
                (with-temp-file py-a (insert "def greet():\n    return \"old\"\n"))
                (with-temp-file py-b (insert "def greet():\n    return \"new\"\n"))
                (let ((s (omnidiff-files py-a py-b)))
                  (should (omnidiff-dual-session-p s))
                  (should (> (length (omnidiff-test--token-overlays
                                      (omnidiff-dual-session-after-buf s)))
                             0))
                  (with-current-buffer (omnidiff-dual-session-after-buf s)
                    (omnidiff-dual-quit))
                  (should-not (memq s omnidiff-dual--sessions))))
              ;; 2. `omnidiff-buffers' on two unsaved buffers.
              (let ((ub1 (generate-new-buffer "unsaved-a.py"))
                    (ub2 (generate-new-buffer "unsaved-b.py")))
                (with-current-buffer ub1 (insert "x = 10\n"))
                (with-current-buffer ub2 (insert "x = 20\n"))
                (let ((bsess (omnidiff-buffers ub1 ub2)))
                  (should (omnidiff-dual-session-p bsess))
                  (should (eq (omnidiff-dual-session-before-buf bsess) ub1))
                  (should (eq (omnidiff-dual-session-after-buf bsess) ub2))
                  (should (> (length (omnidiff-test--token-overlays ub2)) 0))
                  (with-current-buffer ub2 (omnidiff-dual-quit))
                  (should (buffer-live-p ub1))
                  (should (buffer-live-p ub2))
                  (should-not (omnidiff-test--token-overlays ub1))
                  (should-not (omnidiff-test--token-overlays ub2)))))
          (set-frame-width (selected-frame) orig-width))))))

(ert-deftest omnidiff-dual-test/stale-line-starts-during-live-edit ()
  "Stale cached `line-starts' during live editing stays safe across Dual helpers."
  (omnidiff-test--with-buffer-cleanup
    (let* ((stale-b (generate-new-buffer " *omnidiff-stale-b*"))
           (stale-a (generate-new-buffer " *omnidiff-stale-a*"))
           (lines-10 (cl-loop for i from 1 to 10 collect (format "v%d = %d" i i)))
           (alines-10 (append (butlast lines-10) (list "v10 = 999")))
           (jdiff (make-omnidiff-json-diff
                   :before (make-omnidiff-json-side
                            :path "a.py"
                            :hunks (list (omnidiff-test--make-hunk 'update 9 6 9 8)))
                   :after (make-omnidiff-json-side
                           :path "b.py"
                           :hunks (list (omnidiff-test--make-hunk 'update 9 6 9 9))))))
      (with-current-buffer stale-b (insert (string-join lines-10 "\n") "\n"))
      (with-current-buffer stale-a (insert (string-join alines-10 "\n") "\n"))
      (let* ((omnidiff-dual-live-update nil)
             (omnidiff-dual-fold-unchanged nil)
             (sess (omnidiff-dual-open :before-buffer stale-b
                                       :after-buffer stale-a
                                       :json-diff jdiff
                                       :editable-after t)))
        (with-current-buffer stale-a
          (omnidiff-dual-toggle-edit)
          ;; Delete last 6 lines so cached `line-starts' extends past `point-max'.
          (goto-char (point-min))
          (forward-line 4)
          (delete-region (point) (point-max))
          (let ((stale-starts (omnidiff-dual--side-line-starts sess 'after)))
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
            (omnidiff-dual-toggle-fold)))))))

(ert-deftest omnidiff-dual-test/reattach-user-buffer-and-state-restoration ()
  "Re-attaching Dual to an attached user buffer tears down S1 and restores state."
  (omnidiff-test--with-buffer-cleanup
    (let* ((ubuf (generate-new-buffer " *omnidiff-reattach-user*"))
           (ubuf-nonlocal (generate-new-buffer " *omnidiff-reattach-nonlocal*"))
           (body (string-join (cl-loop for i from 1 to 12 collect (format "x%d = %d" i i)) "\n"))
           (jdiff (make-omnidiff-json-diff
                   :before (make-omnidiff-json-side
                            :path "a.py"
                            :hunks (list (omnidiff-test--make-hunk 'update 0 5 0 6)))
                   :after (make-omnidiff-json-side
                           :path "b.py"
                           :hunks (list (omnidiff-test--make-hunk 'update 0 5 0 6))))))
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
             (s1-before (omnidiff-dual-session-before-buf s1)))
        (should (memq s1 omnidiff-dual--sessions))
        (should (buffer-live-p s1-before))
        (with-current-buffer ubuf
          (should buffer-read-only)
          (should (eq buffer-invisibility-spec t)))
        ;; Re-attach Session 2 to the same user buffer without quitting S1.
        (let ((s2 (omnidiff-dual-open :before-source (concat body "\n")
                                      :after-buffer ubuf
                                      :json-diff jdiff)))
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
      ;; Non-local `header-line-format' remains non-local after re-attach + quit.
      (with-current-buffer ubuf-nonlocal
        (insert body "\n")
        (setq buffer-read-only nil)
        (kill-local-variable 'header-line-format)
        (setq buffer-invisibility-spec t))
      (let* ((omnidiff-dual-fold-unchanged t)
             (omnidiff-dual-context-lines 1)
             (s3 (omnidiff-dual-open :before-source (concat body "\n")
                                     :after-buffer ubuf-nonlocal
                                     :json-diff jdiff))
             (_s4 (omnidiff-dual-open :before-source (concat body "\n")
                                      :after-buffer ubuf-nonlocal
                                      :json-diff jdiff)))
        (should-not (memq s3 omnidiff-dual--sessions))
        (with-current-buffer ubuf-nonlocal
          (omnidiff-dual-quit)
          (should-not buffer-read-only)
          (should-not (local-variable-p 'header-line-format))
          (should (eq buffer-invisibility-spec t)))))))

(ert-deftest omnidiff-dual-test/fold-banner-vertical-navigation-and-expand ()
  "Fold banner is reachable by `next-line'/`previous-line' and expanded by RET."
  (omnidiff-test--with-buffer-cleanup
    (let* ((fb-b (generate-new-buffer " *omnidiff-foldnav-b*"))
           (fb-a (generate-new-buffer " *omnidiff-foldnav-a*"))
           (base-lines (cl-loop for i from 1 to 20 collect (format "line_%02d = %d" i i)))
           (after-lines (copy-sequence base-lines)))
      (setf (nth 0 after-lines) "line_01 = 999")
      (setf (nth 19 after-lines) "line_20 = 999")
      (with-current-buffer fb-b (insert (string-join base-lines "\n") "\n"))
      (with-current-buffer fb-a (insert (string-join after-lines "\n") "\n"))
      (let* ((jdiff (make-omnidiff-json-diff
                     :before (make-omnidiff-json-side
                              :path "a.py"
                              :hunks (list (omnidiff-test--make-hunk 'update 0 10 0 11)
                                           (omnidiff-test--make-hunk 'update 19 10 19 12)))
                     :after (make-omnidiff-json-side
                             :path "b.py"
                             :hunks (list (omnidiff-test--make-hunk 'update 0 10 0 13)
                                          (omnidiff-test--make-hunk 'update 19 10 19 13)))))
             (omnidiff-dual-fold-unchanged t)
             (omnidiff-dual-context-lines 2)
             (sess (omnidiff-dual-open :before-buffer fb-b
                                       :after-buffer fb-a
                                       :json-diff jdiff)))
        (omnidiff-dual--focus-side sess 'after)
        (with-current-buffer fb-a
          (let ((ov (car (omnidiff-dual-test--fold-overlays fb-a))))
            (should ov)
            (should (overlayp (overlay-get ov 'omnidiff-dual-fold-nl-ov)))
            ;; From L3 col 0 -> `next-line' lands on fold -> `next-line' lands on L18.
            (goto-char (point-min))
            (forward-line 2)
            (setq last-command nil)
            (call-interactively #'next-line)
            (should (eq (omnidiff-dual--fold-overlay-at (point)) ov))
            (setq last-command 'next-line)
            (call-interactively #'next-line)
            (should (= (line-number-at-pos) 18))
            (should-not (omnidiff-dual--fold-overlay-at (point)))
            ;; From L3 col 5 -> `next-line' lands on fold -> `next-line' lands on L18.
            (goto-char (point-min))
            (forward-line 2)
            (forward-char 5)
            (setq last-command nil)
            (call-interactively #'next-line)
            (should (eq (omnidiff-dual--fold-overlay-at (point)) ov))
            (setq last-command 'next-line)
            (call-interactively #'next-line)
            (should (= (line-number-at-pos) 18))
            ;; From L18 -> `previous-line' lands on fold -> RET expands both overlays.
            (goto-char (point-min))
            (forward-line 17)
            (setq last-command nil)
            (call-interactively #'previous-line)
            (should (eq (omnidiff-dual--fold-overlay-at (point)) ov))
            (let ((nl-ov (overlay-get ov 'omnidiff-dual-fold-nl-ov)))
              (should (eq (omnidiff-dual-jump-counterpart) 'expanded-fold))
              (should-not (overlay-buffer ov))
              (should-not (overlay-buffer nl-ov)))))))))

(ert-deftest omnidiff-dual-test/buffer-lines-eof-and-revert-teardown ()
  "`omnidiff-dual--buffer-lines' trims EOF phantom lines and `revert-buffer' tears down."
  ;; 1. EOF line-splitting behavior.
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
  ;; 2. `revert-buffer' (`change-major-mode-hook') tears down Dual cleanly without leaks.
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((rev-file (expand-file-name "revert_me.py" dir))
             (jdiff (make-omnidiff-json-diff
                     :before (make-omnidiff-json-side
                              :path "a.py"
                              :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5)))
                     :after (make-omnidiff-json-side
                             :path "b.py"
                             :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))))))
        (with-temp-file rev-file (insert "x = 2\n"))
        (let* ((rev-buf (find-file-noselect rev-file))
               (sess (omnidiff-dual-open :before-source "x = 1\n"
                                         :after-path rev-file
                                         :after-buffer rev-buf
                                         :json-diff jdiff
                                         :editable-after t))
               (owned-before (omnidiff-dual-session-before-buf sess))
               (sess-tmp (omnidiff-dual--ensure-tmpdir sess)))
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
                                 (overlays-in (point-min) (point-max))))))))))

(ert-deftest omnidiff-dual-test/files-dirty-visited-after-immediate-snap-cleanup ()
  "`omnidiff-files' diffs dirty visited buffer and removes `snap-tmpdir' immediately."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((fa (expand-file-name "dirty_a.py" dir))
             (fb (expand-file-name "dirty_b.py" dir))
             (before-snaps (directory-files temporary-file-directory t
                                            "\\`omnidiff-files-")))
        (with-temp-file fa (insert "def val():\n    return 1\n"))
        (with-temp-file fb (insert "def val():\n    return 1\n"))
        (let ((after-buf (find-file-noselect fb)))
          (with-current-buffer after-buf
            (goto-char (point-min))
            (search-forward "1")
            (replace-match "777")
            (should (buffer-modified-p)))
          (let* ((sess (omnidiff-files fa fb))
                 (after-snaps (directory-files temporary-file-directory t
                                               "\\`omnidiff-files-"))
                 (new-snaps (cl-set-difference after-snaps before-snaps :test #'equal)))
            (should (omnidiff-dual-session-p sess))
            (should (eq (omnidiff-dual-session-after-buf sess) after-buf))
            ;; Wave 3A: `snap-tmpdir' is deleted immediately once live buffer attaches!
            (should-not new-snaps)
            (should (cl-some (lambda (ov)
                               (with-current-buffer after-buf
                                 (equal (buffer-substring-no-properties
                                         (overlay-start ov) (overlay-end ov))
                                        "777")))
                             (omnidiff-test--token-overlays after-buf)))
            (with-current-buffer after-buf
              (omnidiff-dual-quit))
            (should (buffer-live-p after-buf))
            (should (with-current-buffer after-buf (buffer-modified-p)))))))))

(ert-deftest omnidiff-dual-test/nav-after-overview-killed ()
  "`]'/`[' in Dual handle a killed Overview buffer gracefully without error."
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((ov-buf (generate-new-buffer " *omnidiff-killed-ov*"))
             (mk-file (lambda (name old-val new-val)
                        (omnidiff-test--make-file
                         :path name
                         :before-source (format "x = %s\n" old-val)
                         :after-source (format "x = %s\n" new-val)
                         :before-hunks (list (omnidiff-test--make-hunk
                                              'update 0 4 0 (+ 4 (length old-val))))
                         :after-hunks (list (omnidiff-test--make-hunk
                                             'update 0 4 0 (+ 4 (length new-val)))))))
             (diff (omnidiff-test--make-diff (list (funcall mk-file "one.py" "1" "2")
                                                   (funcall mk-file "two.py" "3" "4")))))
        (with-current-buffer ov-buf
          (omnidiff-mode)
          (setq omnidiff--repo-dir dir
                omnidiff--diff diff)
          (omnidiff-render-buffer diff)
          (goto-char (point-min))
          (let ((dsess (omnidiff-open-dual-at-point)))
            (should (omnidiff-dual-session-p dsess))
            (kill-buffer ov-buf)
            (should-not (buffer-live-p ov-buf))
            (with-current-buffer (omnidiff-dual-session-after-buf dsess)
              (should (string-search "no longer open" (omnidiff-dual-next-file)))
              (should (string-search "no longer open" (omnidiff-dual-prev-file))))
            (should (memq dsess omnidiff-dual--sessions))))))))

(ert-deftest omnidiff-dual-test/open-dual-at-point-deleted-hunk-focuses-before ()
  "`omnidiff-open-dual-at-point' on a deleted file hunk header focuses `before'."
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((ov-del (generate-new-buffer " *omnidiff-del-hunk-ov*"))
             (del-src "def legacy():\n    return 1\n    # end\n")
             (del-file (omnidiff-test--make-file
                        :path "gone.py" :status 'deleted
                        :removed 3 :max-line 3
                        :before-source del-src
                        :after-source ""
                        :before-hunks (list (omnidiff-test--make-hunk 'delete 0 0 2 9))
                        :summary 'deleted-file))
             (diff (omnidiff-test--make-diff del-file)))
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
            (should (omnidiff-dual-session-p dsess))
            (should (eq (omnidiff-dual--session-active-side dsess) 'before))
            (should (eq (window-buffer (selected-window))
                        (omnidiff-dual-session-before-buf dsess)))
            (should (> (buffer-size (omnidiff-dual-session-before-buf dsess)) 0))
            (should (= (buffer-size (omnidiff-dual-session-after-buf dsess)) 0))))))))

(ert-deftest omnidiff-dual-test/menus-keymaps-and-fringe-bitmaps ()
  "Transient menus, edit-mode bindings, and per-operation fringe bitmaps exist."
  (should (commandp 'omnidiff-overview-menu))
  (should (commandp 'omnidiff-dual-menu))
  (should (commandp 'omnidiff-image-menu))
  (should (eq (lookup-key omnidiff-dual-edit-mode-map (kbd "C-c z"))
              #'omnidiff-dual-toggle-fold))
  (should (eq (lookup-key omnidiff-dual-edit-mode-map (kbd "C-c RET"))
              #'omnidiff-dual-jump-counterpart))
  (should (eq (lookup-key omnidiff-dual-edit-mode-map (kbd "C-c ?"))
              #'omnidiff-dual-help))
  (should (= (length (cl-delete-duplicates
                      (mapcar #'omnidiff-dual--fringe-bitmap
                              '(insert delete update move))))
             4)))

(ert-deftest omnidiff-dual-test/header-line-remap-and-clickable-controls ()
  "Dual header-line remap, active/inactive faces, and clickable fold/minimap work."
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((fa (expand-file-name "before.txt" dir))
             (fb (expand-file-name "after.txt" dir))
             (orig-width (frame-width (selected-frame)))
             (omnidiff-dual-layout 'auto)
             (omnidiff-dual-min-width 100)
             (omnidiff-dual-fold-unchanged t)
             (omnidiff-dual-context-lines 1))
        (unwind-protect
            (progn
              (set-frame-width (selected-frame) 140)
              (with-temp-file fa
                (insert "old-head\n"
                        (mapconcat (lambda (i) (format "same-%d" i))
                                   (number-sequence 1 12) "\n")
                        "\n"))
              (with-temp-file fb
                (insert "new-head\n"
                        (mapconcat (lambda (i) (format "same-%d" i))
                                   (number-sequence 1 12) "\n")
                        "\n"))
              (let* ((jdiff (make-omnidiff-json-diff
                             :before (make-omnidiff-json-side
                                      :path fa :language "Text"
                                      :hunks (list (omnidiff-test--make-hunk 'update 0 0 0 8)))
                             :after (make-omnidiff-json-side
                                     :path fb :language "Text"
                                     :hunks (list (omnidiff-test--make-hunk 'update 0 0 0 8)))
                             :summary 'modified))
                     (user-after (find-file-noselect fb))
                     (sess (omnidiff-dual-open
                            :before-path fa :after-path fb
                            :after-buffer user-after
                            :before-label "src/deep/nested/dir/before.txt"
                            :after-label "src/deep/nested/dir/after.txt"
                            :json-diff jdiff))
                     (bb (omnidiff-dual-session-before-buf sess))
                     (ab (omnidiff-dual-session-after-buf sess)))
                ;; Header-line face-remap cookie is recorded on both pane orig-states.
                (should (plist-get (omnidiff-dual--pane-orig-state
                                    (omnidiff-dual--pane sess 'before))
                                   :hl-remap-cookie))
                (should (plist-get (omnidiff-dual--pane-orig-state
                                    (omnidiff-dual--pane sess 'after))
                                   :hl-remap-cookie))
                ;; Active/inactive faces reflect focused pane.
                (omnidiff-dual--focus-side sess 'after)
                (let ((bh (with-current-buffer bb (omnidiff-dual--header-line)))
                      (ah (with-current-buffer ab (omnidiff-dual--header-line))))
                  (should (eq (get-text-property 0 'face bh)
                              'omnidiff-dual-header-before-inactive))
                  (should (eq (get-text-property 0 'face ah)
                              'omnidiff-dual-header-after))
                  (should (string-prefix-p " AFTER  " ah)))
                ;; Fold banner has mouse-1 click keymap.
                (let ((fold-ov (car (omnidiff-dual-test--fold-overlays ab))))
                  (should fold-ov)
                  (let ((bs (overlay-get fold-ov 'before-string)))
                    (should (eq (lookup-key (get-text-property 0 'keymap bs) [mouse-1])
                                #'omnidiff-dual--mouse-expand-fold))))
                ;; Minimap is clickable and stays visible on AFTER in single layout.
                (omnidiff-dual-cycle-layout) ; dual -> single
                (omnidiff-dual--focus-side sess 'after)
                (let ((mm (omnidiff-dual--render-minimap sess)))
                  (should mm)
                  (should (eq (lookup-key (get-text-property 0 'keymap mm)
                                          [header-line mouse-1])
                              #'omnidiff-dual--minimap-click)))
                ;; Teardown removes header-line face-remap cleanly from user buffer.
                (with-current-buffer ab
                  (omnidiff-dual-quit))
                (with-current-buffer user-after
                  (should-not (assq 'header-line face-remapping-alist)))))
          (set-frame-width (selected-frame) orig-width))))))

(ert-deftest omnidiff-dual-test/semantic-revert-16-patterns ()
  "Stop-by-stop semantic revert achieves exact BEFORE parity across 16 CLI patterns."
  (skip-unless (executable-find omnidiff-executable))
  (should (eq (keymap-lookup omnidiff-dual-mode-map "r") #'omnidiff-dual-revert-change))
  (should (eq (keymap-lookup omnidiff-dual-mode-map "u") #'omnidiff-dual-undo))
  (should (eq (keymap-lookup omnidiff-dual-edit-mode-map "C-c r")
              #'omnidiff-dual-revert-change))
  (omnidiff-test--with-temp-dir dir
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
               ("split_call" "rs" "fn f() {\n    call(a);\n}\n"
                "fn f() {\n    call(a,\n         b);\n}\n")
               ("join_call" "rs" "foo(a,\n    b);\n" "foo(a);\n")
               ("new_file" "rs" "" "line1\nline2\n")
               ("deleted_file" "rs" "line1\nline2\n" "")))
      (pcase-let* ((`(,name ,ext ,before-txt ,after-txt) spec)
                   (bp (expand-file-name (format "%s-b.%s" name ext) dir))
                   (ap (expand-file-name (format "%s-a.%s" name ext) dir)))
        (with-temp-file bp (insert before-txt))
        (with-temp-file ap (insert after-txt))
        (omnidiff-test--with-buffer-cleanup
          (let* ((abuf (find-file-noselect ap))
                 (session (omnidiff-dual-open
                           :before-path bp
                           :after-path ap
                           :after-buffer abuf
                           :display-path (format "%s.%s" name ext)
                           :editable-after t)))
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
                (omnidiff-test--wait-dual session 5.0)))
            (should-not (omnidiff-dual--session-stops session))
            (with-current-buffer abuf
              (should (equal (buffer-string) before-txt))
              (should (buffer-modified-p))
              ;; Disk file was NOT auto-saved.
              (should (equal (with-temp-buffer
                               (insert-file-contents ap)
                               (buffer-string))
                             after-txt)))))))))

(ert-deftest omnidiff-dual-test/semantic-revert-undo-and-guards ()
  "Consecutive `u' undo, live `C-c r', stale guard, read-only guard, and move refusal."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((bp (expand-file-name "demo.rs" dir))
             (ap (expand-file-name "demo-after.rs" dir))
             (before-txt (concat "fn alpha() {\n    let a = 1;\n    removed_call();\n}\n\n"
                                 "fn beta() {\n    let b = 2;\n}\n"))
             (after-txt (concat "fn alpha() {\n    let a = 10;\n}\n\n"
                                "fn beta() {\n    let b = 20;\n}\n")))
        (with-temp-file bp (insert before-txt))
        (with-temp-file ap (insert after-txt))
        (let* ((abuf (find-file-noselect ap))
               (session (omnidiff-dual-open
                         :before-path bp
                         :after-path ap
                         :after-buffer abuf
                         :display-path "demo.rs"
                         :editable-after t))
               (init-stops (length (omnidiff-dual--session-stops session))))
          (should (>= init-stops 3))
          (with-current-buffer abuf
            (should (string-search "r/u" (substring-no-properties (omnidiff-dual--header-line)))))
          ;; Revert stop #1 (`let a = 10;' -> `let a = 1;') from AFTER pane.
          (omnidiff-dual--goto-stop session (car (omnidiff-dual--session-stops session)) nil)
          (with-current-buffer (omnidiff-dual--side-buf
                                session (omnidiff-dual--session-active-side session))
            (omnidiff-dual-revert-change))
          (omnidiff-test--wait-dual session 5.0)
          (let ((after-first (with-current-buffer abuf (buffer-string))))
            (should (string-search "let a = 1;" after-first))
            (should (string-search "let b = 20;" after-first))
            ;; Revert pure delete (`removed_call();') from BEFORE pane.
            (omnidiff-dual--goto-stop session (car (omnidiff-dual--session-stops session)) nil)
            (should (eq (omnidiff-dual--session-active-side session) 'before))
            (with-current-buffer (omnidiff-dual-session-before-buf session)
              (omnidiff-dual-revert-change))
            (omnidiff-test--wait-dual session 5.0)
            (should (string-search "removed_call();" (with-current-buffer abuf (buffer-string))))
            ;; Press `u' twice consecutively from BEFORE pane -> undoes 2nd, then 1st.
            (with-current-buffer (omnidiff-dual-session-before-buf session)
              (let ((last-command 'omnidiff-dual-revert-change))
                (omnidiff-dual-undo))
              (omnidiff-test--wait-dual session 5.0)
              (should (equal (with-current-buffer abuf (buffer-string)) after-first))
              (let ((last-command 'omnidiff-dual-undo))
                (omnidiff-dual-undo))
              (omnidiff-test--wait-dual session 5.0)
              (should (equal (with-current-buffer abuf (buffer-string)) after-txt)))
            (should (= (length (omnidiff-dual--session-stops session)) init-stops))
            ;; Live edit mode: `C-c r' works, and stale edit without refresh is refused.
            (with-current-buffer abuf
              (omnidiff-dual-toggle-edit)
              (should (omnidiff-dual--session-editing-p session))
              (omnidiff-dual--goto-stop session (car (omnidiff-dual--session-stops session)) nil)
              (omnidiff-dual-revert-change))
            (omnidiff-test--wait-dual session 5.0)
            (should (string-search "let a = 1;" (with-current-buffer abuf (buffer-string))))
            (with-current-buffer abuf
              (goto-char (point-min))
              (insert "// unsaved stale edit\n")
              (let ((err (should-error (omnidiff-dual-revert-change) :type 'user-error)))
                (should (string-search "out of date" (error-message-string err))))))
          (omnidiff-dual-close session nil))
        ;; Read-only AFTER session refuses `r'/`u' and omits `r/u' from header hint.
        (let ((ro-session (omnidiff-dual-open
                           :before-source "let x = 1;\n"
                           :after-source "let x = 2;\n"
                           :display-path "ro.rs"
                           :editable-after nil)))
          (with-current-buffer (omnidiff-dual-session-after-buf ro-session)
            (should-not (string-search "r/u" (substring-no-properties (omnidiff-dual--header-line))))
            (should-error (omnidiff-dual-revert-change) :type 'user-error)
            (should-error (omnidiff-dual-undo) :type 'user-error))
          (omnidiff-dual-close ro-session nil))
        ;; Move stop refuses automatic revert without modifying AFTER buffer.
        (let* ((mb (expand-file-name "move-b.rs" dir))
               (ma (expand-file-name "move-a.rs" dir))
               (mb-txt (concat "fn first() {\n    let x = 1;\n    let y = 2;\n    x + y\n}\n\n"
                               "fn second() {\n    let p = 3;\n    let q = 4;\n    p * q\n}\n"))
               (ma-txt (concat "fn second() {\n    let p = 3;\n    let q = 4;\n    p * q\n}\n\n"
                               "fn first() {\n    let x = 1;\n    let y = 2;\n    x + y\n}\n")))
          (with-temp-file mb (insert mb-txt))
          (with-temp-file ma (insert ma-txt))
          (let* ((mabuf (find-file-noselect ma))
                 (move-session (omnidiff-dual-open
                                :before-path mb
                                :after-path ma
                                :after-buffer mabuf
                                :display-path "move.rs"
                                :editable-after t))
                 (move-stop (cl-find-if
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
              (should-not (buffer-modified-p)))))))))

(ert-deftest omnidiff-dual-test/emulation-keymap-precedence ()
  "Review keys in `omnidiff-dual-mode' override modal emulation maps cleanly."
  (let* ((modal-map (let ((m (make-sparse-keymap)))
                      (define-key m (kbd "n") #'forward-char)
                      (define-key m (kbd "p") #'backward-char)
                      (define-key m (kbd "j") #'next-line)
                      m))
         (saved-emul emulation-mode-map-alists))
    (unwind-protect
        (progn
          (setq emulation-mode-map-alists
                (cons (list (cons 'omnidiff-dual-test--modal-active modal-map))
                      emulation-mode-map-alists))
          (with-temp-buffer
            (setq-local omnidiff-dual-test--modal-active t)
            (should (eq (key-binding (kbd "n")) #'forward-char))
            (should (eq (key-binding (kbd "p")) #'backward-char))
            (should (eq (key-binding (kbd "j")) #'next-line))
            (omnidiff-dual-mode 1)
            (should (eq (key-binding (kbd "n")) #'omnidiff-dual-next-change))
            (should (eq (key-binding (kbd "p")) #'omnidiff-dual-prev-change))
            (should (eq (key-binding (kbd "j")) #'next-line))
            (omnidiff-dual-mode -1)
            (should (eq (key-binding (kbd "n")) #'forward-char))
            (should (eq (key-binding (kbd "p")) #'backward-char))))
      (setq emulation-mode-map-alists saved-emul))))

(ert-deftest omnidiff-dual-test/overlapping-stops-navigation ()
  "Stepping with `n'/`p' advances past overlapping edit and move stops cleanly."
  (omnidiff-test--with-buffer-cleanup
    (let* ((before-src (concat "line0\n"
                               "old(sharedA);\n"
                               "old2(sharedB);\n"
                               "tail_old\n"))
           (after-src (concat "line0\n"
                              "newCall(sharedA, extra);\n"
                              "sub(sharedB);\n"
                              "tail_new\n"))
           (mt-b1 (make-omnidiff-json-range :start-row 1 :start-col 8 :end-row 1 :end-col 15))
           (mt-a1 (make-omnidiff-json-range :start-row 1 :start-col 4 :end-row 1 :end-col 11))
           (mt-b2 (make-omnidiff-json-range :start-row 2 :start-col 4 :end-row 2 :end-col 11))
           (mt-a2 (make-omnidiff-json-range :start-row 2 :start-col 5 :end-row 2 :end-col 12))
           (json-diff
            (make-omnidiff-json-diff
             :before (make-omnidiff-json-side
                      :path "a.java"
                      :hunks (list (omnidiff-test--make-hunk 'delete 1 0 1 4)
                                   (omnidiff-test--make-hunk 'move 1 4 1 11 mt-b1)
                                   (omnidiff-test--make-hunk 'delete 2 0 2 5)
                                   (omnidiff-test--make-hunk 'move 2 5 2 12 mt-b2)
                                   (omnidiff-test--make-hunk 'update 3 0 3 8)))
             :after (make-omnidiff-json-side
                     :path "a.java"
                     :hunks (list (omnidiff-test--make-hunk 'insert 1 0 1 8)
                                  (omnidiff-test--make-hunk 'move 1 8 1 15 mt-a1)
                                  (omnidiff-test--make-hunk 'insert 1 15 1 25)
                                  (omnidiff-test--make-hunk 'insert 2 0 2 4)
                                  (omnidiff-test--make-hunk 'move 2 4 2 11 mt-a2)
                                  (omnidiff-test--make-hunk 'update 3 0 3 8)))))
           (omnidiff-dual-layout 'dual)
           (omnidiff-dual-fold-unchanged nil)
           (sess (omnidiff-dual-open :before-source before-src
                                     :after-source after-src
                                     :display-path "a.java"
                                     :json-diff json-diff))
           (stops (omnidiff-dual--session-stops sess)))
      (should (= (length stops) 4))
      (should (= (omnidiff-dual--session-stop-index sess) 1))
      ;; Forward stepping visits every stop in order without trapping on 2 or 3.
      (dolist (expected '(2 3 4 1))
        (with-current-buffer (omnidiff-dual--side-buf
                              sess (omnidiff-dual--session-active-side sess))
          (omnidiff-dual-next-change))
        (should (= (omnidiff-dual--session-stop-index sess) expected)))
      ;; Backward stepping visits every stop in reverse order.
      (dolist (expected '(4 3 2 1))
        (with-current-buffer (omnidiff-dual--side-buf
                              sess (omnidiff-dual--session-active-side sess))
          (omnidiff-dual-prev-change))
        (should (= (omnidiff-dual--session-stop-index sess) expected)))
      ;; Switching to counterpart pane at stop 2 preserves index 2 and advances to 3.
      (with-current-buffer (omnidiff-dual-session-after-buf sess)
        (omnidiff-dual-next-change))
      (should (= (omnidiff-dual--session-stop-index sess) 2))
      (with-current-buffer (omnidiff-dual-session-after-buf sess)
        (omnidiff-dual-switch-pane))
      (should (= (omnidiff-dual--session-stop-index sess) 2))
      (with-current-buffer (omnidiff-dual-session-before-buf sess)
        (omnidiff-dual-next-change))
      (should (= (omnidiff-dual--session-stop-index sess) 3)))))

;;; New P0/P1 regression tests for omnidiff-dual.el

(ert-deftest omnidiff-dual-test/tab-bar-same-tab-and-cross-tab-restore ()
  "`tab-bar-mode' restores same-tab windows, avoids clobbering other tabs, and cleans markers."
  (omnidiff-test--with-buffer-cleanup
    (let ((orig-tab-bar tab-bar-mode)
          (orig-width (frame-width (selected-frame)))
          (jdiff (make-omnidiff-json-diff
                  :before (make-omnidiff-json-side
                           :path "a.py"
                           :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5)))
                  :after (make-omnidiff-json-side
                          :path "a.py"
                          :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))))))
      (cl-flet ((any-winconf-marker-p ()
                  (cl-some (lambda (tab) (assq 'omnidiff-dual-winconf-id (cdr tab)))
                           (tab-bar-tabs (selected-frame)))))
        (unwind-protect
            (let ((omnidiff-dual-window-setup 'current-frame)
                  (omnidiff-dual-layout 'dual)
                  (orig-buf (generate-new-buffer " *omnidiff-tab-orig*"))
                  (tab2-buf (generate-new-buffer " *omnidiff-tab-other*")))
              (set-frame-width (selected-frame) 140)
              (tab-bar-mode 1)
              ;; Ensure single starting tab with known buffer.
              (while (> (length (tab-bar-tabs (selected-frame))) 1)
                (tab-bar-close-tab))
              (delete-other-windows)
              (set-window-buffer (selected-window) orig-buf)
              ;; 1. Same-tab open -> current buffer/tab name changes -> `q' restores winconf.
              (let ((s1 (omnidiff-dual-open :before-source "x = 1\n"
                                            :after-source "x = 2\n"
                                            :display-path "tab1.py"
                                            :json-diff jdiff)))
                (should (= (length (window-list)) 2))
                (should (any-winconf-marker-p))
                (with-current-buffer (omnidiff-dual-session-after-buf s1)
                  (omnidiff-dual-quit))
                (should (= (length (window-list)) 1))
                (should (eq (window-buffer (selected-window)) orig-buf))
                (should-not (any-winconf-marker-p)))
              ;; 2. Cross-tab close -> does NOT clobber Tab 2 and cleans markers from all tabs.
              (let ((s2 (omnidiff-dual-open :before-source "x = 1\n"
                                            :after-source "x = 2\n"
                                            :display-path "tab2.py"
                                            :json-diff jdiff)))
                (should (any-winconf-marker-p))
                (tab-bar-new-tab)
                (delete-other-windows)
                (set-window-buffer (selected-window) tab2-buf)
                (should (eq (window-buffer (selected-window)) tab2-buf))
                (omnidiff-dual-close s2 t)
                ;; Tab 2 was NOT overwritten by Tab 1's saved window configuration!
                (should (= (length (window-list)) 1))
                (should (eq (window-buffer (selected-window)) tab2-buf))
                ;; Markers removed from ALL tabs (including inactive Tab 1).
                (should-not (any-winconf-marker-p))))
          (while (> (length (tab-bar-tabs (selected-frame))) 1)
            (ignore-errors (tab-bar-close-tab)))
          (tab-bar-mode (if orig-tab-bar 1 -1))
          (set-frame-width (selected-frame) orig-width))))))

(ert-deftest omnidiff-dual-test/find-stop-at-overlapping-multiline-preference ()
  "`omnidiff-dual--find-stop-at' picks narrower nested multi-line stop over enclosing stop."
  (let* ((outer (make-omnidiff-change-stop
                 :index 1 :side 'after :operation 'move
                 :range (make-omnidiff-json-range
                         :start-row 10 :start-col 0 :end-row 11 :end-col 5)))
         (inner (make-omnidiff-change-stop
                 :index 2 :side 'after :operation 'update
                 :range (make-omnidiff-json-range
                         :start-row 10 :start-col 50 :end-row 11 :end-col 5)))
         (sess-fwd (make-omnidiff-dual--session
                    :stops (list outer inner)
                    :stop-index 0))
         (sess-rev (make-omnidiff-dual--session
                    :stops (list inner outer)
                    :stop-index 0)))
    ;; Inside both overlapping ranges: (10, 50)..(11, 5) is narrower than (10, 0)..(11, 5).
    (should (eq (omnidiff-dual--find-stop-at sess-fwd 'after 10 55) inner))
    (should (eq (omnidiff-dual--find-stop-at sess-fwd 'after 11 2) inner))
    (should (eq (omnidiff-dual--find-stop-at sess-rev 'after 10 55) inner))
    (should (eq (omnidiff-dual--find-stop-at sess-rev 'after 11 2) inner))
    ;; Outside `inner' but inside `outer' still resolves to `outer'.
    (should (eq (omnidiff-dual--find-stop-at sess-fwd 'after 10 10) outer))))

(ert-deftest omnidiff-dual-test/async-refresh-tick-race-guard ()
  "Stale async refresh callback discards outdated diff when buffer edited in flight."
  (omnidiff-test--with-buffer-cleanup
    (let* ((bb (generate-new-buffer " *omnidiff-race-b*"))
           (ab (generate-new-buffer " *omnidiff-race-a*"))
           (init-diff (make-omnidiff-json-diff
                       :before (make-omnidiff-json-side
                                :path "a.py"
                                :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5)))
                       :after (make-omnidiff-json-side
                               :path "a.py"
                               :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5)))))
           (stale-diff (make-omnidiff-json-diff
                        :before (make-omnidiff-json-side :path "a.py" :hunks nil)
                        :after (make-omnidiff-json-side :path "a.py" :hunks nil)))
           (fresh-diff (make-omnidiff-json-diff
                        :before (make-omnidiff-json-side
                                 :path "a.py"
                                 :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5)))
                        :after (make-omnidiff-json-side
                                :path "a.py"
                                :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 6)))))
           (captured-cbs nil))
      (with-current-buffer bb (insert "x = 1\n"))
      (with-current-buffer ab (insert "x = 2\n"))
      (let* ((omnidiff-dual-live-update nil)
             (sess (omnidiff-dual-open :before-buffer bb
                                       :after-buffer ab
                                       :json-diff init-diff
                                       :editable-after t))
             (orig-tick (omnidiff-dual--side-lines-tick sess 'after)))
        (cl-letf (((symbol-function 'omnidiff-engine-run-pair-async)
                   (lambda (_bf _af cb &rest _kw)
                     (push cb captured-cbs)
                     nil)))
          ;; 1. Start async refresh #1 while buffer is at `orig-tick'.
          (omnidiff-dual--refresh-async sess)
          (should (= (length captured-cbs) 1))
          (let ((cb1 (car captured-cbs)))
            ;; 2. Edit AFTER buffer while refresh #1 is in flight!
            (with-current-buffer ab
              (let ((inhibit-read-only t))
                (goto-char (point-min))
                (search-forward "2")
                (replace-match "99")))
            (let ((post-edit-tick (buffer-chars-modified-tick ab)))
              (should-not (= post-edit-tick orig-tick))
              ;; 3. Stale callback fires: MUST discard `stale-diff', keep `orig-tick',
              ;;    preserve `r' revert refusal, and automatically schedule retry #2.
              (funcall cb1 stale-diff)
              (should (eq (omnidiff-dual--session-json-diff sess) init-diff))
              (should (= (omnidiff-dual--side-lines-tick sess 'after) orig-tick))
              (should (= (length captured-cbs) 2))
              (with-current-buffer ab
                (let ((err (should-error (omnidiff-dual-revert-change) :type 'user-error)))
                  (should (string-search "out of date" (error-message-string err)))))
              ;; 4. Retry callback #2 completes cleanly for `post-edit-tick'.
              (let ((cb2 (car captured-cbs)))
                (funcall cb2 fresh-diff)
                (should (eq (omnidiff-dual--session-json-diff sess) fresh-diff))
                (should (= (omnidiff-dual--side-lines-tick sess 'after) post-edit-tick))))))))))

(ert-deftest omnidiff-dual-test/moved-line-inline-insert-revert ()
  "Reverting an inline insert on a moved line deletes only the inserted byte range."
  (omnidiff-test--with-buffer-cleanup
    (let* ((before-src (concat "fn moved_fn(a) {\n"
                               "    return a;\n"
                               "}\n"
                               "fn stay_1() {}\n"
                               "fn stay_2() {}\n"))
           (after-src (concat "fn stay_1() {}\n"
                              "fn stay_2() {}\n"
                              "fn moved_fn(a, extra) {\n"
                              "    return a;\n"
                              "}\n"))
           (mt-b (make-omnidiff-json-range :start-row 2 :start-col 0 :end-row 4 :end-col 1))
           (mt-a (make-omnidiff-json-range :start-row 0 :start-col 0 :end-row 2 :end-col 1))
           ;; Inline insert of ", extra" (bytes 13..20) inside moved row 2.
           (jdiff-ins
            (make-omnidiff-json-diff
             :before (make-omnidiff-json-side
                      :path "m.rs"
                      :hunks (list (omnidiff-test--make-hunk 'move 0 0 2 1 mt-b)))
             :after (make-omnidiff-json-side
                     :path "m.rs"
                     :hunks (list (omnidiff-test--make-hunk 'move 2 0 4 1 mt-a)
                                  (omnidiff-test--make-hunk 'insert 2 13 2 20)))))
           (abuf (generate-new-buffer " *omnidiff-mv-ins-after*")))
      (with-current-buffer abuf (insert after-src))
      (let* ((omnidiff-dual-live-update nil)
             (sess (omnidiff-dual-open :before-source before-src
                                       :after-buffer abuf
                                       :display-path "m.rs"
                                       :json-diff jdiff-ins
                                       :editable-after t))
             (ins-stop (cl-find-if
                        (lambda (st) (eq (omnidiff-change-stop-operation st) 'insert))
                        (omnidiff-dual--session-stops sess))))
        (should ins-stop)
        (omnidiff-dual--goto-stop sess ins-stop nil)
        (cl-letf (((symbol-function 'omnidiff-dual--refresh-async) (lambda (_s) nil)))
          (with-current-buffer abuf
            (let ((plan (omnidiff-dual-revert-change)))
              (should (eq (plist-get plan :kind) 'delete-ranges))
              ;; Only ", extra" was removed; the moved line itself is preserved intact!
              (should (equal (buffer-string)
                             (concat "fn stay_1() {}\n"
                                     "fn stay_2() {}\n"
                                     "fn moved_fn(a) {\n"
                                     "    return a;\n"
                                     "}\n"))))))
        (omnidiff-dual-close sess nil))
      ;; Partial `update' on an unaligned moved line refuses instead of deleting the line.
      (let* ((jdiff-upd
              (make-omnidiff-json-diff
               :before (make-omnidiff-json-side
                        :path "m.rs"
                        :hunks (list (omnidiff-test--make-hunk 'move 0 0 2 1 mt-b)))
               :after (make-omnidiff-json-side
                       :path "m.rs"
                       :hunks (list (omnidiff-test--make-hunk 'move 2 0 4 1 mt-a)
                                    (omnidiff-test--make-hunk 'update 2 13 2 20)))))
             (abuf2 (generate-new-buffer " *omnidiff-mv-upd-after*")))
        (with-current-buffer abuf2 (insert after-src))
        (let* ((sess2 (omnidiff-dual-open :before-source before-src
                                          :after-buffer abuf2
                                          :display-path "m.rs"
                                          :json-diff jdiff-upd
                                          :editable-after t))
               (upd-stop (cl-find-if
                          (lambda (st) (eq (omnidiff-change-stop-operation st) 'update))
                          (omnidiff-dual--session-stops sess2))))
          (should upd-stop)
          (omnidiff-dual--goto-stop sess2 upd-stop nil)
          (with-current-buffer abuf2
            (let ((err (should-error (omnidiff-dual-revert-change) :type 'user-error)))
              (should (string-search "unaligned/moved line" (error-message-string err))))
            (should (equal (buffer-string) after-src))))))))

(ert-deftest omnidiff-dual-test/utf8-bom-alignment-and-no-double-bom ()
  "UTF-8 BOM files align line 0 accurately and revert+save writes a single BOM."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((bp (expand-file-name "bom-before.py" dir))
             (ap (expand-file-name "bom-after.py" dir))
             (bom (unibyte-string #xef #xbb #xbf))
             (before-body "def alpha():\n    return 1\n")
             (after-body "def beta():\n    return 1\n"))
        (omnidiff-test--write-bin bp (concat bom (encode-coding-string before-body 'utf-8-unix)))
        (omnidiff-test--write-bin ap (concat bom (encode-coding-string after-body 'utf-8-unix)))
        (let* ((abuf (find-file-noselect ap))
               (sess (omnidiff-dual-open :before-path bp
                                         :after-path ap
                                         :after-buffer abuf
                                         :display-path "bom.py"
                                         :editable-after t))
               (bb (omnidiff-dual-session-before-buf sess)))
          ;; 1. Neither pane contains a literal U+FEFF character.
          (should-not (omnidiff-dual--buffer-starts-with-bom-p bb))
          (should-not (omnidiff-dual--buffer-starts-with-bom-p abuf))
          ;; 2. Line 0 token overlays are NOT shifted by 3 BOM bytes.
          (should (cl-some (lambda (ov)
                             (with-current-buffer bb
                               (equal (buffer-substring-no-properties
                                       (overlay-start ov) (overlay-end ov))
                                      "alpha")))
                           (omnidiff-test--token-overlays bb)))
          (should (cl-some (lambda (ov)
                             (with-current-buffer abuf
                               (equal (buffer-substring-no-properties
                                       (overlay-start ov) (overlay-end ov))
                                      "beta")))
                           (omnidiff-test--token-overlays abuf)))
          ;; 3. Revert line 0 and save -> exact single UTF-8 BOM (EF BB BF), never double BOM!
          (omnidiff-dual--goto-stop sess (car (omnidiff-dual--session-stops sess)) nil)
          (with-current-buffer abuf
            (omnidiff-dual-revert-change))
          (omnidiff-test--wait-dual sess 5.0)
          (with-current-buffer abuf
            (should-not (omnidiff-dual--buffer-starts-with-bom-p abuf))
            (should (equal (buffer-string) before-body))
            (let ((inhibit-message t))
              (save-buffer)))
          (omnidiff-test--wait-dual sess 5.0)
          (let ((disk-bytes (with-temp-buffer
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally ap)
                              (buffer-string))))
            (should (string-prefix-p bom disk-bytes))
            (should-not (string-prefix-p (concat bom bom) disk-bytes))
            (should (equal (substring disk-bytes 3)
                           (encode-coding-string before-body 'utf-8-unix)))))))))

(ert-deftest omnidiff-dual-test/sync-other-window-col0-stability ()
  "`omnidiff-dual--sync-other-window' does not jump by 1 line between col 0 and col 1."
  (omnidiff-test--with-buffer-cleanup
    (let* ((orig-width (frame-width (selected-frame)))
           (lines (mapconcat (lambda (i) (format "row_%02d_content = %d" i i))
                             (number-sequence 0 29) "\n"))
           (jdiff (make-omnidiff-json-diff
                   :before (make-omnidiff-json-side :path "a.py" :hunks nil)
                   :after (make-omnidiff-json-side :path "a.py" :hunks nil)))
           (omnidiff-dual-layout 'dual)
           (omnidiff-dual-sync-scroll t)
           (omnidiff-dual-fold-unchanged nil))
      (unwind-protect
          (progn
            (set-frame-width (selected-frame) 140)
            (let* ((sess (omnidiff-dual-open :before-source (concat lines "\n")
                                             :after-source (concat lines "\n")
                                             :display-path "sync.py"
                                             :json-diff jdiff))
                   (bb (omnidiff-dual-session-before-buf sess))
                   (awin (omnidiff-dual--side-win sess 'after))
                   (bwin (omnidiff-dual--side-win sess 'before)))
              (should (omnidiff-dual--dual-visible-p sess))
              (select-window awin)
              (set-window-start awin (point-min))
              ;; Row 0 col 0 vs Row 0 col 1 -> both screen line 0.
              (let ((b-pos0 (with-current-buffer bb (point-min))))
                (goto-char (point-min))
                (let ((sl-r0c0 (omnidiff-dual--sync-other-window sess 'before b-pos0 nil)))
                  (forward-char 1)
                  (let ((sl-r0c1 (omnidiff-dual--sync-other-window sess 'before b-pos0 nil)))
                    (should (= sl-r0c0 0))
                    (should (= sl-r0c1 0)))))
              ;; Row 5 col 0 vs Row 5 col 1 -> identical screen line and opposite window-start.
              (let ((b-pos5 (with-current-buffer bb
                              (goto-char (point-min))
                              (forward-line 5)
                              (point))))
                (set-window-start awin (point-min))
                (goto-char (point-min))
                (forward-line 5)
                (should (= (current-column) 0))
                (let* ((sl-col0 (omnidiff-dual--sync-other-window sess 'before b-pos5 nil))
                       (ws-col0 (window-start bwin)))
                  (forward-char 1)
                  (should (= (current-column) 1))
                  (let* ((sl-col1 (omnidiff-dual--sync-other-window sess 'before b-pos5 nil))
                         (ws-col1 (window-start bwin)))
                    (should (= sl-col0 5))
                    (should (= sl-col0 sl-col1))
                    (should (= ws-col0 ws-col1)))))))
        (set-frame-width (selected-frame) orig-width)))))

(ert-deftest omnidiff-dual-test/dirty-visited-after-buf-promotion ()
  "Opening Dual with `:editable-after t' promotes a dirty visited AFTER buffer."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((bp (expand-file-name "promo-before.py" dir))
             (ap (expand-file-name "promo-after.py" dir)))
        (with-temp-file bp (insert "x = 1\n"))
        (with-temp-file ap (insert "x = 1\n"))
        (let ((existing (find-file-noselect ap))
              (stale-empty-diff (make-omnidiff-json-diff
                                 :before (make-omnidiff-json-side :path bp :hunks nil)
                                 :after (make-omnidiff-json-side :path ap :hunks nil))))
          (with-current-buffer existing
            (goto-char (point-min))
            (search-forward "1")
            (replace-match "999")
            (should (buffer-modified-p)))
          ;; Omit `:after-buffer' and pass stale empty `:json-diff': must promote `existing'
          ;; and recompute diff against its live modified contents!
          (let ((sess (omnidiff-dual-open :before-path bp
                                          :after-path ap
                                          :editable-after t
                                          :json-diff stale-empty-diff)))
            (should (eq (omnidiff-dual-session-after-buf sess) existing))
            (should (omnidiff-dual--session-stops sess))
            (should (cl-some (lambda (ov)
                               (with-current-buffer existing
                                 (equal (buffer-substring-no-properties
                                         (overlay-start ov) (overlay-end ov))
                                        "999")))
                             (omnidiff-test--token-overlays existing)))))))))

(ert-deftest omnidiff-dual-test/reuse-session-buffer-swap-and-error-rollback ()
  "Swapping panes on `:reuse-session' keeps orig-state/remaps clean; errors roll back."
  (omnidiff-test--with-buffer-cleanup
    (let* ((jdiff (make-omnidiff-json-diff
                   :before (make-omnidiff-json-side
                            :path "a.py"
                            :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5)))
                   :after (make-omnidiff-json-side
                           :path "a.py"
                           :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5)))))
           (buf-a (generate-new-buffer " *omnidiff-swap-a*"))
           (buf-b (generate-new-buffer " *omnidiff-swap-b*")))
      (with-current-buffer buf-a
        (insert "x = 1\n")
        (setq buffer-read-only nil)
        (setq-local header-line-format "ORIG-HL-A"))
      (with-current-buffer buf-b
        (insert "x = 2\n")
        (setq buffer-read-only nil)
        (kill-local-variable 'header-line-format))
      ;; 1. Open (buf-a, buf-b), then reuse swapping to (buf-b, buf-a).
      (let ((sess (omnidiff-dual-open :before-buffer buf-a
                                      :after-buffer buf-b
                                      :json-diff jdiff)))
        (let ((reused (omnidiff-dual-open :before-buffer buf-b
                                          :after-buffer buf-a
                                          :json-diff jdiff
                                          :reuse-session sess)))
          (should (eq reused sess))
          (should (eq (omnidiff-dual-session-before-buf sess) buf-b))
          (should (eq (omnidiff-dual-session-after-buf sess) buf-a))
          ;; Exactly one `header-line' remap entry on each buffer (zero leaks).
          (dolist (b (list buf-a buf-b))
            (with-current-buffer b
              (should (= (cl-count 'header-line face-remapping-alist :key #'car) 1))))
          ;; True original states were preserved across the swap.
          (let ((orig-a (omnidiff-dual--pane-orig-state (omnidiff-dual--pane sess 'after)))
                (orig-b (omnidiff-dual--pane-orig-state (omnidiff-dual--pane sess 'before))))
            (should-not (plist-get orig-a :read-only))
            (should (plist-get orig-a :header-line-local-p))
            (should (equal (plist-get orig-a :header-line) "ORIG-HL-A"))
            (should-not (plist-get orig-b :read-only))
            (should-not (plist-get orig-b :header-line-local-p))))
        ;; 2. Pre-swap error during `:reuse-session' keeps existing session intact!
        (cl-letf (((symbol-function 'omnidiff-dual--resolve-input-diff)
                   (lambda (&rest _args) (error "Simulated pre-swap failure"))))
          (should-error
           (omnidiff-dual-open :before-source "z = 1\n"
                               :after-source "z = 2\n"
                               :reuse-session sess)))
        (should (memq sess omnidiff-dual--sessions))
        (should (eq (omnidiff-dual-session-before-buf sess) buf-b))
        (should (eq (omnidiff-dual-session-after-buf sess) buf-a))
        ;; Quit restores both user buffers cleanly.
        (omnidiff-dual-close sess nil)
        (with-current-buffer buf-a
          (should-not buffer-read-only)
          (should (local-variable-p 'header-line-format))
          (should (equal header-line-format "ORIG-HL-A"))
          (should-not (assq 'header-line face-remapping-alist)))
        (with-current-buffer buf-b
          (should-not buffer-read-only)
          (should-not (local-variable-p 'header-line-format))
          (should-not (assq 'header-line face-remapping-alist))))
      ;; 3. Mid-open error on brand new session rolls back owned buffers & user buffer state.
      (let ((ubuf (generate-new-buffer " *omnidiff-rollback-user*"))
            (sessions-before (copy-sequence omnidiff-dual--sessions))
            (bufs-before (buffer-list)))
        (with-current-buffer ubuf
          (insert "x = 2\n")
          (setq buffer-read-only nil)
          (setq-local header-line-format "ROLLBACK-HL"))
        (cl-letf (((symbol-function 'omnidiff-dual--apply-diff)
                   (lambda (&rest _args) (error "Simulated mid-open failure"))))
          (should-error
           (omnidiff-dual-open :before-source "x = 1\n"
                               :after-buffer ubuf
                               :display-path "rollback.py"
                               :json-diff jdiff)))
        (should (equal omnidiff-dual--sessions sessions-before))
        ;; No leaked ephemeral `*omnidiff BEFORE: rollback.py*' buffer.
        (should-not (cl-set-difference (buffer-list) bufs-before))
        (with-current-buffer ubuf
          (should-not buffer-read-only)
          (should-not omnidiff-dual--session)
          (should-not (bound-and-true-p omnidiff-dual-mode))
          (should (equal header-line-format "ROLLBACK-HL"))
          (should-not (assq 'header-line face-remapping-alist)))))))

(ert-deftest omnidiff-dual-test/narrowed-buffer-fold-creation ()
  "Opening Dual with folding on a narrowed buffer creates fold overlays cleanly."
  (omnidiff-test--with-buffer-cleanup
    (let* ((base-lines (cl-loop for i from 1 to 20 collect (format "line_%02d = %d" i i)))
           (after-lines (copy-sequence base-lines))
           (abuf (generate-new-buffer " *omnidiff-narrow-after*")))
      (setf (nth 0 after-lines) "line_01 = 999"
            (nth 19 after-lines) "line_20 = 999")
      (with-current-buffer abuf
        (insert (string-join after-lines "\n") "\n")
        (goto-char (point-min))
        (let ((beg (point))
              (end (progn (forward-line 4) (point))))
          (narrow-to-region beg end)))
      (let* ((jdiff (make-omnidiff-json-diff
                     :before (make-omnidiff-json-side
                              :path "n.py"
                              :hunks (list (omnidiff-test--make-hunk 'update 0 10 0 11)
                                           (omnidiff-test--make-hunk 'update 19 10 19 12)))
                     :after (make-omnidiff-json-side
                             :path "n.py"
                             :hunks (list (omnidiff-test--make-hunk 'update 0 10 0 13)
                                          (omnidiff-test--make-hunk 'update 19 10 19 13)))))
             (omnidiff-dual-fold-unchanged t)
             (omnidiff-dual-context-lines 2)
             (sess (omnidiff-dual-open :before-source (concat (string-join base-lines "\n") "\n")
                                       :after-buffer abuf
                                       :json-diff jdiff)))
        (should (omnidiff-dual-session-p sess))
        (should (= (length (omnidiff-dual-test--fold-overlays abuf)) 1))
        (should (= (length (omnidiff-dual-test--fold-overlays
                            (omnidiff-dual-session-before-buf sess)))
                   1))))))

(ert-deftest omnidiff-dual-test/syntax-toggle-large-visited-and-reuse-persistence ()
  "`toggle-syntax' works on >512KB visited buffers and `show-syntax = nil' persists."
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((big-tail (apply #'concat (make-list 8200 (concat ";; " (make-string 62 ?a) "\n"))))
             (big-before (concat "(defun alpha () 1)\n" big-tail))
             (big-after (concat "(defun alpha () 2)\n" big-tail))
             (big-file (expand-file-name "huge.el" dir))
             (small-file (expand-file-name "small.el" dir))
             (jdiff (make-omnidiff-json-diff
                     :before (make-omnidiff-json-side
                              :path "huge.el"
                              :hunks (list (omnidiff-test--make-hunk 'update 0 16 0 17)))
                     :after (make-omnidiff-json-side
                             :path "huge.el"
                             :hunks (list (omnidiff-test--make-hunk 'update 0 16 0 17))))))
        (with-temp-file big-file (insert big-after))
        (with-temp-file small-file (insert "(defun beta () 9)\n"))
        (let* ((big-buf (find-file-noselect big-file))
               (small-buf (find-file-noselect small-file)))
          (with-current-buffer big-buf
            (should (> (buffer-size) omnidiff-dual-syntax-max-bytes))
            (emacs-lisp-mode)
            (font-lock-ensure)
            (should (eq (get-text-property 2 'face) 'font-lock-keyword-face)))
          (with-current-buffer small-buf
            (emacs-lisp-mode)
            (font-lock-ensure)
            (should (eq (get-text-property 2 'face) 'font-lock-keyword-face)))
          (let* ((sess (omnidiff-dual-open :before-source big-before
                                           :after-path big-file
                                           :after-buffer big-buf
                                           :display-path "huge.el"
                                           :json-diff jdiff
                                           :editable-after t))
                 (bb (omnidiff-dual-session-before-buf sess)))
            ;; 1. Toggle syntax off -> visited >512KB AFTER unfontifies; toggle back on ->
            ;;    >512KB visited AFTER re-fontifies, while >512KB owned BEFORE skips.
            (with-current-buffer big-buf
              (should-not (omnidiff-dual-toggle-syntax))
              (should-not (get-text-property 2 'face))
              (should (omnidiff-dual-toggle-syntax))
              (should (eq (get-text-property 2 'face) 'font-lock-keyword-face)))
            (with-current-buffer bb
              (should-not (get-text-property 2 'face)))
            ;; 2. Turn syntax OFF, switch files via `:reuse-session' -> stays OFF on new panes.
            (with-current-buffer big-buf
              (should-not (omnidiff-dual-toggle-syntax)))
            (should-not (omnidiff-dual--session-show-syntax sess))
            (omnidiff-dual-open :before-source "(defun beta () 8)\n"
                                :after-path small-file
                                :after-buffer small-buf
                                :display-path "small.el"
                                :json-diff jdiff
                                :editable-after t
                                :reuse-session sess)
            (should-not (omnidiff-dual--session-show-syntax sess))
            (with-current-buffer (omnidiff-dual-session-before-buf sess)
              (should-not (get-text-property 2 'face)))
            (with-current-buffer small-buf
              (should-not (get-text-property 2 'face)))
            ;; Toggling syntax back ON fontifies both new small panes.
            (with-current-buffer small-buf
              (should (omnidiff-dual-toggle-syntax))
              (should (eq (get-text-property 2 'face) 'font-lock-keyword-face)))
            (with-current-buffer (omnidiff-dual-session-before-buf sess)
              (should (eq (get-text-property 2 'face) 'font-lock-keyword-face)))
            (omnidiff-dual-close sess nil)))))))

(ert-deftest omnidiff-dual-test/toggle-edit-then-save-sets-mutated-p ()
  "Leaving edit mode via `C-c C-c' keeps `after-save-hook' so `C-x C-s' sets `mutated-p'."
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((fa (expand-file-name "save_a.py" dir))
             (fb (expand-file-name "save_b.py" dir))
             (jdiff (make-omnidiff-json-diff
                     :before (make-omnidiff-json-side
                              :path fa
                              :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5)))
                     :after (make-omnidiff-json-side
                             :path fb
                             :hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))))))
        (with-temp-file fa (insert "x = 1\n"))
        (with-temp-file fb (insert "x = 2\n"))
        (let* ((abuf (find-file-noselect fb))
               (omnidiff-dual-live-update nil)
               (sess (omnidiff-dual-open :before-path fa
                                         :after-path fb
                                         :after-buffer abuf
                                         :json-diff jdiff
                                         :editable-after t)))
          (cl-letf (((symbol-function 'omnidiff-dual--refresh-async) (lambda (_s) nil)))
            (with-current-buffer abuf
              ;; Enter edit mode, exit edit mode (`C-c C-c').
              (should (omnidiff-dual-toggle-edit))
              (should-not (omnidiff-dual-toggle-edit))
              (should (memq #'omnidiff-dual--on-after-save after-save-hook))
              ;; Reset mutated-p, mark buffer modified, and save (`C-x C-s').
              (setf (omnidiff-dual--session-mutated-p sess) nil)
              (set-buffer-modified-p t)
              (let ((inhibit-message t))
                (save-buffer))
              (should (omnidiff-dual-session-mutated-p sess)))))))))

(ert-deftest omnidiff-dual-test/initial-side-line-single-sync-and-public-api ()
  "`:initial-side'/`:initial-line' do a single sync; close & snapshot APIs work cleanly."
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((lines (mapconcat (lambda (i) (format "v%d = %d" i i))
                               (number-sequence 1 10) "\n"))
             (b-src (concat lines "\n"))
             (a-src (string-replace "v2 = 2" "v2 = 20" (concat lines "\n")))
             (jdiff (make-omnidiff-json-diff
                     :before (make-omnidiff-json-side
                              :path "api.py"
                              :hunks (list (omnidiff-test--make-hunk 'update 1 5 1 6)))
                     :after (make-omnidiff-json-side
                             :path "api.py"
                             :hunks (list (omnidiff-test--make-hunk 'update 1 5 1 7)))))
             (orig-sync (symbol-function 'omnidiff-dual--sync-focus-and-counterpart))
             (sync-calls 0))
        ;; 1. `:initial-side' + `:initial-line' lands directly with exactly 1 sync call.
        (cl-letf (((symbol-function 'omnidiff-dual--sync-focus-and-counterpart)
                   (lambda (&rest args)
                     (setq sync-calls (1+ sync-calls))
                     (apply orig-sync args))))
          (let ((sess (omnidiff-dual-open :before-source b-src
                                          :after-source a-src
                                          :display-path "api.py"
                                          :json-diff jdiff
                                          :initial-side 'before
                                          :initial-line 7)))
            (should (= sync-calls 1))
            (should (eq (omnidiff-dual--session-active-side sess) 'before))
            (with-current-buffer (omnidiff-dual-session-before-buf sess)
              (should (= (line-number-at-pos) 7)))
            ;; 2. `omnidiff-dual-session-snapshot-paths' writes unsaved buffers into custom tmpdir.
            (let ((custom-tmp (expand-file-name "snap-custom" dir)))
              (make-directory custom-tmp t)
              (pcase-let ((`(,bf ,af ,temps)
                           (omnidiff-dual-session-snapshot-paths sess custom-tmp)))
                (should (= (length temps) 2))
                (should (string-prefix-p (file-name-as-directory (file-truename custom-tmp))
                                         (file-truename bf)))
                (should (string-prefix-p (file-name-as-directory (file-truename custom-tmp))
                                         (file-truename af)))
                (should (file-exists-p bf))
                (should (file-exists-p af))
                (should (equal (with-temp-buffer (insert-file-contents bf) (buffer-string))
                               b-src))
                (should (equal (with-temp-buffer (insert-file-contents af) (buffer-string))
                               a-src))))
            ;; 3. `omnidiff-dual-close' with no arg closes current buffer's active session.
            (let ((bb (omnidiff-dual-session-before-buf sess)))
              (with-current-buffer bb
                (omnidiff-dual-close nil nil))
              (should-not (memq sess omnidiff-dual--sessions))
              (should-not (buffer-live-p bb)))))
        ;; 4. `:initial-side 'after :initial-line 8' + disk-backed snapshot paths.
        (let ((fa (expand-file-name "disk_a.py" dir))
              (fb (expand-file-name "disk_b.py" dir)))
          (with-temp-file fa (insert b-src))
          (with-temp-file fb (insert a-src))
          (setq sync-calls 0)
          (cl-letf (((symbol-function 'omnidiff-dual--sync-focus-and-counterpart)
                     (lambda (&rest args)
                       (setq sync-calls (1+ sync-calls))
                       (apply orig-sync args))))
            (let ((sess2 (omnidiff-dual-open :before-path fa
                                             :after-path fb
                                             :display-path "disk.py"
                                             :json-diff jdiff
                                             :initial-side 'after
                                             :initial-line 8)))
              (should (= sync-calls 1))
              (should (eq (omnidiff-dual--session-active-side sess2) 'after))
              (with-current-buffer (omnidiff-dual-session-after-buf sess2)
                (should (= (line-number-at-pos) 8)))
              (pcase-let ((`(,bf ,af ,temps)
                           (omnidiff-dual-session-snapshot-paths sess2)))
                (should (equal bf fa))
                (should (equal af fb))
                (should-not temps))
              (omnidiff-dual-close sess2 nil)
              (should-not (memq sess2 omnidiff-dual--sessions)))))))))

(ert-deftest omnidiff-dual-test/zero-hunk-side-symmetric-folding ()
  "0-hunk sides fold symmetrically on insertion-only and deletion-only diffs."
  (omnidiff-test--with-buffer-cleanup
    (let* ((orig-width (frame-width (selected-frame)))
           (omnidiff-dual-layout 'dual)
           (omnidiff-dual-fold-unchanged t)
           (omnidiff-dual-context-lines 3)
           (base-lines (mapcar (lambda (i) (format "line_%02d = %d" i i))
                               (number-sequence 0 24)))
           (base-src (concat (mapconcat #'identity base-lines "\n") "\n"))
           (with-extra-lines (append (cl-subseq base-lines 0 12)
                                     (list "extra_line = 999")
                                     (cl-subseq base-lines 12)))
           (with-extra-src (concat (mapconcat #'identity with-extra-lines "\n") "\n")))
      (unwind-protect
          (progn
            (set-frame-width (selected-frame) 140)
            ;; 1. Insertion-only diff: BEFORE has 0 hunks, AFTER has 1 insert at row 12.
            (let* ((jdiff-ins
                    (make-omnidiff-json-diff
                     :before (make-omnidiff-json-side
                              :path "ins.py" :language "Python" :hunks nil)
                     :after (make-omnidiff-json-side
                             :path "ins.py" :language "Python"
                             :hunks (list (omnidiff-test--make-hunk 'insert 12 0 12 16)))
                     :summary 'modified))
                   (sess (omnidiff-dual-open :before-source base-src
                                             :after-source with-extra-src
                                             :display-path "ins.py"
                                             :json-diff jdiff-ins))
                   (bb (omnidiff-dual-session-before-buf sess))
                   (ab (omnidiff-dual-session-after-buf sess))
                   (b-folds (omnidiff-dual-test--fold-overlays bb))
                   (a-folds (omnidiff-dual-test--fold-overlays ab)))
              (should (= (length a-folds) 2))
              (should (= (length b-folds) 2))
              (with-current-buffer bb
                (dolist (ov b-folds)
                  (should (invisible-p (overlay-start ov))))
                (should-not (invisible-p (aref (omnidiff-dual--pane-line-starts
                                                (omnidiff-dual--pane sess 'before))
                                               12))))
              (with-current-buffer ab
                (dolist (ov a-folds)
                  (should (invisible-p (overlay-start ov))))
                (should-not (invisible-p (aref (omnidiff-dual--pane-line-starts
                                                (omnidiff-dual--pane sess 'after))
                                               12))))
              (omnidiff-dual-close sess nil))
            ;; 2. Deletion-only diff: BEFORE has 1 delete at row 12, AFTER has 0 hunks.
            (let* ((jdiff-del
                    (make-omnidiff-json-diff
                     :before (make-omnidiff-json-side
                              :path "del.py" :language "Python"
                              :hunks (list (omnidiff-test--make-hunk 'delete 12 0 12 16)))
                     :after (make-omnidiff-json-side
                             :path "del.py" :language "Python" :hunks nil)
                     :summary 'modified))
                   (sess (omnidiff-dual-open :before-source with-extra-src
                                             :after-source base-src
                                             :display-path "del.py"
                                             :json-diff jdiff-del))
                   (bb (omnidiff-dual-session-before-buf sess))
                   (ab (omnidiff-dual-session-after-buf sess))
                   (b-folds (omnidiff-dual-test--fold-overlays bb))
                   (a-folds (omnidiff-dual-test--fold-overlays ab)))
              (should (= (length b-folds) 2))
              (should (= (length a-folds) 2))
              (with-current-buffer bb
                (dolist (ov b-folds)
                  (should (invisible-p (overlay-start ov))))
                (should-not (invisible-p (aref (omnidiff-dual--pane-line-starts
                                                (omnidiff-dual--pane sess 'before))
                                               12))))
              (with-current-buffer ab
                (dolist (ov a-folds)
                  (should (invisible-p (overlay-start ov))))
                (should-not (invisible-p (aref (omnidiff-dual--pane-line-starts
                                                (omnidiff-dual--pane sess 'after))
                                               12))))
              (omnidiff-dual-close sess nil)))
        (set-frame-width (selected-frame) orig-width)))))

(ert-deftest omnidiff-dual-test/moved-line-before-partial-delete-refusal ()
  "Reverting an unpaired partial BEFORE delete on a moved line refuses cleanly."
  (omnidiff-test--with-buffer-cleanup
    (let* ((before-src (concat "fn moved_fn(a, extra) {\n"
                               "    return a;\n"
                               "}\n"
                               "fn stay_1() {}\n"
                               "fn stay_2() {}\n"))
           (after-src (concat "fn stay_1() {}\n"
                              "fn stay_2() {}\n"
                              "fn moved_fn(a) {\n"
                              "    return a;\n"
                              "}\n"))
           (mt-b (make-omnidiff-json-range :start-row 2 :start-col 0 :end-row 4 :end-col 1))
           (mt-a (make-omnidiff-json-range :start-row 0 :start-col 0 :end-row 2 :end-col 1))
           ;; Inline delete of ", extra" (bytes 13..20) inside BEFORE moved row 0.
           (jdiff-del
            (make-omnidiff-json-diff
             :before (make-omnidiff-json-side
                      :path "m.rs"
                      :hunks (list (omnidiff-test--make-hunk 'move 0 0 2 1 mt-b)
                                   (omnidiff-test--make-hunk 'delete 0 13 0 20)))
             :after (make-omnidiff-json-side
                     :path "m.rs"
                     :hunks (list (omnidiff-test--make-hunk 'move 2 0 4 1 mt-a)))))
           (abuf (generate-new-buffer " *omnidiff-mv-del-after*")))
      (with-current-buffer abuf (insert after-src))
      (let* ((omnidiff-dual-live-update nil)
             (sess (omnidiff-dual-open :before-source before-src
                                       :after-buffer abuf
                                       :display-path "m.rs"
                                       :json-diff jdiff-del
                                       :editable-after t))
             (bb (omnidiff-dual-session-before-buf sess))
             (del-stop (cl-find-if
                        (lambda (st) (eq (omnidiff-change-stop-operation st) 'delete))
                        (omnidiff-dual--session-stops sess))))
        (should del-stop)
        (should (eq (omnidiff-change-stop-side del-stop) 'before))
        (should-not (omnidiff-change-stop-counterpart-range del-stop))
        (omnidiff-dual--goto-stop sess del-stop nil)
        (with-current-buffer bb
          (let ((err (should-error (omnidiff-dual-revert-change) :type 'user-error)))
            (should (string-search "unaligned/moved line" (error-message-string err)))))
        ;; AFTER buffer must remain untouched (no duplicated moved line inserted).
        (with-current-buffer abuf
          (should (equal (buffer-string) after-src)))
        (should-not (omnidiff-dual-session-mutated-p sess))
        (omnidiff-dual-close sess nil)))))

(ert-deftest omnidiff-dual-test/header-line-minimap-cache-invalidation-goto-char ()
  "Programmatic `goto-char' without `post-command-hook' invalidates header minimap cache."
  (omnidiff-test--with-buffer-cleanup
    (let* ((orig-width (frame-width (selected-frame)))
           (omnidiff-dual-layout 'dual)
           (omnidiff-dual-fold-unchanged nil)
           (omnidiff-dual-show-minimap t)
           (b-lines (mapcar (lambda (i) (format "row_%02d = %d" i i))
                            (number-sequence 0 31)))
           (a-lines (cons "row_00 = 99" (cdr b-lines)))
           (b-src (concat (mapconcat #'identity b-lines "\n") "\n"))
           (a-src (concat (mapconcat #'identity a-lines "\n") "\n"))
           (jdiff (make-omnidiff-json-diff
                   :before (make-omnidiff-json-side
                            :path "mm.py" :language "Python"
                            :hunks (list (omnidiff-test--make-hunk 'update 0 9 0 10)))
                   :after (make-omnidiff-json-side
                           :path "mm.py" :language "Python"
                           :hunks (list (omnidiff-test--make-hunk 'update 0 9 0 11)))
                   :summary 'modified)))
      (unwind-protect
          (progn
            (set-frame-width (selected-frame) 140)
            (let* ((sess (omnidiff-dual-open :before-source b-src
                                             :after-source a-src
                                             :display-path "mm.py"
                                             :json-diff jdiff
                                             :initial-side 'before
                                             :initial-line 1))
                   (bb (omnidiff-dual-session-before-buf sess)))
              (with-current-buffer bb
                (goto-char (point-min))
                (let* ((hl0 (omnidiff-dual--header-line))
                       (b0 (plist-get (omnidiff-dual--session-minimap-cache sess)
                                      :last-cursor-bucket))
                       (hl0-cached (omnidiff-dual--header-line)))
                  (should (= b0 0))
                  ;; Unchanged point still hits the header cache cleanly.
                  (should (eq hl0 hl0-cached))
                  ;; Move point programmatically without triggering `post-command-hook'.
                  (goto-char (point-max))
                  (let* ((hl15 (omnidiff-dual--header-line))
                         (b15 (plist-get (omnidiff-dual--session-minimap-cache sess)
                                         :last-cursor-bucket))
                         (hl15-cached (omnidiff-dual--header-line))
                         (mm0-pos (text-property-any
                                   0 (length hl0) 'keymap omnidiff-dual--minimap-map hl0))
                         (mm15-pos (text-property-any
                                    0 (length hl15) 'keymap omnidiff-dual--minimap-map hl15)))
                    (should (= b15 15))
                    (should (eq hl15 hl15-cached))
                    (should-not (eq hl0 hl15))
                    (should-not (equal-including-properties hl0 hl15))
                    (should (integerp mm0-pos))
                    (should (integerp mm15-pos))
                    (if (and (display-graphic-p) (image-type-available-p 'svg))
                        (should-not (equal (get-text-property mm0-pos 'display hl0)
                                           (get-text-property mm15-pos 'display hl15)))
                      (should (plist-get (get-text-property mm0-pos 'face hl0) :underline))
                      (should-not (plist-get (get-text-property mm15-pos 'face hl15) :underline))
                      (should-not (plist-get (get-text-property (+ mm0-pos 15) 'face hl0)
                                             :underline))
                      (should (plist-get (get-text-property (+ mm15-pos 15) 'face hl15)
                                         :underline))))))
              (omnidiff-dual-close sess nil)))
        (set-frame-width (selected-frame) orig-width)))))

(provide 'omnidiff-dual-test)
;;; omnidiff-dual-test.el ends here
