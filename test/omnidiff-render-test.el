;;; omnidiff-render-test.el --- ERT tests for omnidiff-render -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; This file is part of omnidiff.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Focused ERT suite for `omnidiff-render.el': Overview layouts (`unified',
;; `side-by-side', `stacked'), highlight styles, folding, lazy JSON render
;; plans, TTY alignment, header hints, container cards, and stats.

;;; Code:

(require 'omnidiff-test-helper)

;;; Internal helpers for face inspection

(defun omnidiff-render-test--faces-at (pos)
  "Return list of face symbols present at POS in the current buffer."
  (let ((f (get-text-property pos 'face)))
    (cond ((null f) nil)
          ((symbolp f) (list f))
          ((listp f) (cl-remove-if-not #'symbolp f))
          (t nil))))

(defun omnidiff-render-test--collect-faces-in (beg end)
  "Return deduplicated list of face symbols in region BEG..END."
  (let ((pos beg)
        (seen nil))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'face nil end)))
        (dolist (f (omnidiff-render-test--faces-at pos))
          (cl-pushnew f seen))
        (setq pos next)))
    seen))

;;; Migrated Overview rendering, layout, folding, and navigation tests

(ert-deftest omnidiff-render-test/stacked-hunks-headers-and-folding ()
  "Stacked layout renders spacers, structured hunk headers, and clean folding."
  (omnidiff-test--with-buffer-cleanup
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
           (diff (omnidiff-test--make-diff f)))
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
              (h1-line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
          (should (string-search "▼ L8-14" h1-line))
          (should (string-search "+1" h1-line))
          (should (string-search "-1" h1-line))
          (should (string-search "7 unchanged lines" h1-line))
          (should (string-search "1/2" h1-line))
          (should (eql (get-text-property h1-pos 'omnidiff-line) 8))
          (save-excursion
            (forward-line -1)
            (should (string-empty-p
                     (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))
          ;; Fold from inside hunk body -> collapses body, flips triangle, lands on header.
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
          (let ((h2-line (buffer-substring-no-properties
                          (line-beginning-position) (line-end-position))))
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
          (should-not (overlay-get bov 'invisible)))))))

(ert-deftest omnidiff-render-test/fold-all-keeps-current-file ()
  "Folding all files from inside a file body lands on that same file's header."
  (omnidiff-test--with-buffer-cleanup
    (let* ((buf (generate-new-buffer " *omnidiff-fold-all-test*"))
           (make-f (lambda (name line-text)
                     (omnidiff-test--make-json-file
                      name "" (concat line-text "\n")
                      :status 'added
                      :after-hunks
                      (list (omnidiff-test--make-hunk
                             'insert 0 0 0 (string-bytes line-text))))))
           (diff (omnidiff-test--make-diff
                  (list (funcall make-f "a.el" "alpha-body")
                        (funcall make-f "b.el" "beta-body")))))
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
                                       (line-beginning-position) (line-end-position))))))))

(ert-deftest omnidiff-render-test/unified-and-sbs-layouts ()
  "Unified and side-by-side layouts render refine faces, headers, and CJK alignment."
  (should (eq omnidiff-default-layout 'unified))
  (should (eq (keymap-lookup omnidiff-mode-map "v") #'omnidiff-cycle-layout))
  (omnidiff-test--with-buffer-cleanup
    (let* ((pad (mapconcat (lambda (i) (format "// pad %d" i)) (number-sequence 1 11) "\n"))
           (b-src (concat "fn long_fn() {\n" pad "\n;; 中文上下文\n\nlet v = 15;\nend_fn();\n"))
           (a-src (concat "fn long_fn() {\n" pad "\n;; 中文上下文\n\nlet v = 1500;\n"
                          "let extra = 99; // cont-part\nend_fn();\n"))
           (f (omnidiff-test--make-json-file
               "src/demo.rs" b-src a-src
               :before-hunks (list (omnidiff-test--make-hunk 'update 14 8 14 10 nil 0))
               :after-hunks (list (omnidiff-test--make-hunk 'update 14 8 14 12 nil 0)
                                  (omnidiff-test--make-hunk 'insert 15 0 15 28 nil 0))))
           (diff (omnidiff-test--make-diff f))
           (buf (generate-new-buffer " *omnidiff-layouts-test*")))
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
        ;; Header line reflects file count, layout, and default minimal preset.
        (let ((omnidiff-default-preset 'minimal))
          (let ((hl (substring-no-properties (omnidiff-render-header-line))))
            (should (string-search "OMNIDIFF" hl))
            (should (string-search "1 file" hl))
            (should (string-search "unified · minimal" hl))))
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
            (should (= cjk-bar ascii-bar))))))))

(ert-deftest omnidiff-render-test/jump-to-source ()
  "RET jumps to the target source file and line in another window."
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((file (expand-file-name "demo.txt" dir))
            (buf (generate-new-buffer " *omnidiff-jump-test*")))
        (with-temp-file file
          (insert "line 1\nline 2\nline 3\n"))
        (with-current-buffer buf
          (omnidiff-mode)
          (setq omnidiff--repo-dir dir)
          (omnidiff-render-buffer
           (omnidiff-test--make-diff
            (omnidiff-test--make-json-file
             "demo.txt" "line 1\nline 2\n" "line 1\nline 2\nline 3\n"
             :after-hunks (list (omnidiff-test--make-hunk 'insert 2 0 2 6)))))
          (goto-char (point-min))
          (search-forward "line 3")
          (omnidiff-jump-to-source)
          (should (equal (file-truename (buffer-file-name)) (file-truename file)))
          (should (= (line-number-at-pos) 3)))))))

(ert-deftest omnidiff-render-test/empty-diff ()
  "Empty `omnidiff-diff' renders a clean placeholder message."
  (omnidiff-test--with-buffer-cleanup
    (let ((buf (generate-new-buffer " *omnidiff-empty-test*")))
      (with-current-buffer buf
        (omnidiff-mode)
        (omnidiff-render-buffer (omnidiff-test--make-diff nil))
        (should (string-search "No changes to display." (buffer-string)))))))

(ert-deftest omnidiff-render-test/header-newline-escaping ()
  "Newlines in filenames escape to one header line while keeping raw path."
  (omnidiff-test--with-buffer-cleanup
    (let ((buf (generate-new-buffer " *omnidiff-nl-title-test*"))
          (diff (omnidiff-test--make-diff
                 (omnidiff-test--make-json-file
                  "dir/a\nb.el" "" "x\n" :status 'added
                  :after-hunks (list (omnidiff-test--make-hunk 'insert 0 0 0 1))))))
      (with-current-buffer buf
        (omnidiff-mode)
        (omnidiff-render-buffer diff)
        (goto-char (point-min))
        (let ((hdr-line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
          (should (string-search "dir/a\\nb.el" hdr-line))
          (should (equal (get-text-property (point-min) 'omnidiff-file) "dir/a\nb.el"))
          (should (= (omnidiff-render-file-header-pos-at (+ (point-min) 8)) (point-min))))))))

(ert-deftest omnidiff-render-test/sbs-width-budget-and-narrow-fallback ()
  "SBS wraps losslessly within frame width and falls back to stacked when narrow."
  (omnidiff-test--with-buffer-cleanup
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
            ;; End-to-end check against real `omnidiff' CLI.
            (when-let* ((bin (executable-find omnidiff-executable)))
              (omnidiff-test--with-temp-dir dir
                (let* ((fa (expand-file-name "a.rs" dir))
                       (fb (expand-file-name "b.rs" dir))
                       (long-a "fn demo() { let x = \"0123456789abcdefghijklmnopqrstuvwxyz\"; }")
                       (long-b "fn demo() { let y = \"0123456789abcdefghijklmnopqrstuvwxyz\"; }"))
                  (with-temp-file fa (insert long-a "\n"))
                  (with-temp-file fb (insert long-b "\n"))
                  (let* ((jd (omnidiff-engine-run-pair fa fb 'default bin))
                         (diff (omnidiff-test--make-diff
                                (omnidiff-git-file-from-json "a.rs" fa fb "a.rs" jd))))
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
                            (when (string-match
                                   "\\`[0-9 ]+┃ [~ ] \\(.*?\\) │ [0-9 ]+┃ [~ ] \\(.*\\)\\'"
                                   row)
                              (push (string-trim-right (match-string 1 row) " +") left-parts)
                              (push (string-trim-right (match-string 2 row) " +") right-parts)))
                          (should (>= (length left-parts) 2))
                          (should (equal (apply #'concat (nreverse left-parts)) long-a))
                          (should (equal (apply #'concat (nreverse right-parts)) long-b)))))
                    ;; At 60 cols (code-w 18 < 20): auto-falls back to stacked without CLI.
                    (set-frame-width (selected-frame) 60)
                    (setq omnidiff--layout 'side-by-side)
                    (omnidiff-render-buffer diff)
                    (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                      (should (string-search " BEFORE \n" txt))
                      (should (string-search " AFTER  \n" txt))
                      (should-not (string-search "…" txt))
                      (should (string-search long-a txt))
                      (should (string-search long-b txt))))))))
        (set-frame-width (selected-frame) orig-width)))))

(ert-deftest omnidiff-render-test/file-header-pos-inside-folded-body ()
  "Enclosing file header lookup resolves folded body positions to their own file."
  (omnidiff-test--with-buffer-cleanup
    (let* ((buf (generate-new-buffer " *omnidiff-folded-pos-test*"))
           (make-f (lambda (name line-text)
                     (omnidiff-test--make-json-file
                      name "" (concat line-text "\n") :status 'added
                      :after-hunks
                      (list (omnidiff-test--make-hunk
                             'insert 0 0 0 (string-bytes line-text))))))
           (diff (omnidiff-test--make-diff
                  (list (funcall make-f "first.el" "first-body")
                        (funcall make-f "last.el" "last-body")))))
      (with-current-buffer buf
        (omnidiff-mode)
        (omnidiff-render-buffer diff)
        (goto-char (point-min))
        (let ((hdr1 (point))
              (body1 (progn (search-forward "first-body") (match-beginning 0)))
              (hdr2 (progn (omnidiff-next-file) (point)))
              (body2 (progn (search-forward "last-body") (match-beginning 0))))
          (goto-char hdr1)
          (omnidiff-toggle-fold-all)
          (should (invisible-p body1))
          (should (invisible-p body2))
          (should (= (omnidiff-render-file-header-pos-at body1) hdr1))
          (should (= (omnidiff-render-file-header-pos-at body2) hdr2)))))))

(ert-deftest omnidiff-render-test/moved-indicator-uses-named-face ()
  "Moved rails and move-refs use `omnidiff-moved-indicator' instead of literal hex."
  (omnidiff-test--with-buffer-cleanup
    (let* ((buf (generate-new-buffer " *omnidiff-moved-face-test*"))
           (b-src "(foo)\nline2\nline3\nline4\nline5\n")
           (a-src "line2\nline3\nline4\nline5\n(bar)\n")
           (mt-b (make-omnidiff-json-range :start-row 4 :start-col 0 :end-row 4 :end-col 5))
           (mt-a (make-omnidiff-json-range :start-row 0 :start-col 0 :end-row 0 :end-col 5))
           (diff (omnidiff-test--make-diff
                  (omnidiff-test--make-json-file
                   "moved.el" b-src a-src
                   :before-hunks (list (omnidiff-test--make-hunk 'move 0 0 0 5 mt-b)
                                       (omnidiff-test--make-hunk 'update 0 1 0 4))
                   :after-hunks (list (omnidiff-test--make-hunk 'move 4 0 4 5 mt-a)
                                      (omnidiff-test--make-hunk 'update 4 1 4 4))))))
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
        (should (eq (face-attribute 'omnidiff-moved-indicator :slant nil) 'unspecified))))))

(ert-deftest omnidiff-render-test/header-hints-cache-invalidation ()
  "Header hints cache reuses entries per context and invalidates on theme reapply."
  (let ((omnidiff-render--hints-cache nil)
        (rev0 (omnidiff-theme-revision)))
    (let ((def-first (omnidiff-render--header-hints 'default))
          (img-first (omnidiff-render--header-hints 'image)))
      ;; Repeated lookups return the exact cached entry.
      (should (eq (omnidiff-render--header-hints 'default) def-first))
      (should (eq (omnidiff-render--header-hints 'image) img-first))
      (should-not (equal (car def-first) (car img-first)))
      ;; Forced theme reapply bumps revision and invalidates cached entries.
      (omnidiff-apply-theme nil t)
      (should (> (omnidiff-theme-revision) rev0))
      (let ((def-second (omnidiff-render--header-hints 'default))
            (img-second (omnidiff-render--header-hints 'image)))
        (should-not (eq def-second def-first))
        (should-not (eq img-second img-first))
        (should (eq (omnidiff-render--header-hints 'default) def-second))))))

(ert-deftest omnidiff-render-test/cached-layout-switching-cjk-wrap-and-hooks ()
  "Cached diff cycles 3 layouts in memory, wraps CJK+spans, and keeps overlays/hooks."
  (omnidiff-test--with-buffer-cleanup
    (let* ((b-src "fn demo() {\nlet msg = \"旧中文内容\";\n}\n")
           (a-src (concat "fn demo() {\nlet msg = \"新中文跨行高亮内容测试\";\n"
                          "    let partial_refine_long_line_0123456789_continued_part;\n}\n"))
           (f (omnidiff-test--make-json-file
               "src/demo.rs" b-src a-src
               :before-hunks
               (list (omnidiff-test--make-hunk
                      'update 1 10 1 (string-bytes "let msg = \"旧中文内容\"")))
               :after-hunks
               (list (omnidiff-test--make-hunk
                      'update 1 10 1 (string-bytes "let msg = \"新中文跨行高亮内容测试\""))
                     (omnidiff-test--make-hunk
                      'insert 2 4 2
                      (string-bytes
                       "    let partial_refine_long_line_0123456789_continued_part;")))))
           (diff (omnidiff-test--make-diff f))
           (buf (generate-new-buffer " *omnidiff-p2-cache-test*"))
           (orig-width (frame-width (selected-frame))))
      (unwind-protect
          (with-current-buffer buf
            (omnidiff-mode)
            (set-frame-width (selected-frame) 80)
            (setq omnidiff--diff diff
                  omnidiff--layout 'unified
                  omnidiff--highlight-style 'traditional)
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
                (should-not (omnidiff-git-pending-request))
                (should (eq omnidiff--layout 'side-by-side))
                (should (eq (overlay-buffer ext-ov) buf))
                (should (= before-calls 2))
                (should (= (length after-calls) 2))
                (should (= (nth 2 (car after-calls)) prev-size))
                (should (equal (get-text-property (point) 'omnidiff-file) "src/demo.rs"))
                (should (eql (get-text-property (point) 'omnidiff-line) 2)))
              ;; In SBS, partial-refine wrapped line 3 preserves refine face on continuation.
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
              (should-not (omnidiff-git-pending-request))
              (should (eq omnidiff--layout 'stacked))
              (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-search " BEFORE \n" txt))
                (should (string-search " AFTER  \n" txt))
                (should (string-search
                         "let partial_refine_long_line_0123456789_continued_part;" txt)))
              ;; Cycle stacked -> unified in memory.
              (omnidiff-cycle-layout)
              (should-not (omnidiff-git-pending-request))
              (should (eq omnidiff--layout 'unified))))
        (set-frame-width (selected-frame) orig-width)))))

(ert-deftest omnidiff-render-test/dual-side-cursor-restore-across-layouts ()
  "Cursor side (BEFORE vs AFTER on same line number) persists across layout cycles."
  (omnidiff-test--with-buffer-cleanup
    (let* ((fa (omnidiff-test--make-json-file
                "a.rs"
                "keep_top();\nlet old_only = 1;\nremoved_line();\nkeep_bot();\n"
                "keep_top();\nlet new_only = 2;\nkeep_bot();\n"
                :before-hunks (list (omnidiff-test--make-hunk 'update 1 4 1 12)
                                    (omnidiff-test--make-hunk 'delete 2 0 2 15))
                :after-hunks (list (omnidiff-test--make-hunk 'update 1 4 1 12))))
           (diff (omnidiff-test--make-diff fa))
           (buf (generate-new-buffer " *omnidiff-side-restore*")))
      (with-current-buffer buf
        (omnidiff-mode)
        (setq omnidiff--diff diff
              omnidiff--layout 'unified)
        (omnidiff-render-buffer diff)
        ;; Put point on AFTER line 2 ("new_only"), cycle unified -> sbs -> stacked.
        (goto-char (point-min))
        (search-forward "new_only")
        (should (eq (get-text-property (point) 'omnidiff-side) 'after))
        (omnidiff-cycle-layout) ; -> side-by-side
        (should (eq (get-text-property (point) 'omnidiff-side) 'after))
        (should (eql (get-text-property (point) 'omnidiff-new-line) 2))
        (omnidiff-cycle-layout) ; -> stacked
        (should (eq (get-text-property (point) 'omnidiff-side) 'after))
        (should (string-search "new_only"
                               (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position))))
        ;; Put point on BEFORE line 2 ("old_only") in stacked -> stays on BEFORE.
        (goto-char (point-min))
        (search-forward "old_only")
        (should (eq (get-text-property (point) 'omnidiff-side) 'before))
        (omnidiff-cycle-layout) ; -> unified
        (should (eq (get-text-property (point) 'omnidiff-side) 'before))
        (should (string-search "old_only"
                               (buffer-substring-no-properties
                                (line-beginning-position) (line-end-position))))))))

(ert-deftest omnidiff-render-test/ret-on-deleted-line-and-folded-hunk-nav ()
  "RET on deleted line lands near working line; hunk nav skips folded files."
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
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
             (diff (omnidiff-test--make-diff (list fa fb)))
             (buf (generate-new-buffer " *omnidiff-ret-fold-nav*")))
        (with-temp-file (expand-file-name "a.rs" dir)
          (insert "keep_top();\nlet new_only = 2;\nkeep_bot();\n"))
        (with-current-buffer buf
          (omnidiff-mode)
          (setq omnidiff--diff diff
                omnidiff--repo-dir dir
                omnidiff--layout 'unified)
          (omnidiff-render-buffer diff)
          ;; RET on deleted-only line 3 jumps to nearest working-tree new line (2 or 3).
          (goto-char (point-min))
          (search-forward "removed_line")
          (omnidiff-jump-to-source)
          (should (memq (line-number-at-pos) '(2 3))))
        ;; Fold first file -> `n'/`p' skip invisible hunk and land on second file.
        (with-current-buffer buf
          (goto-char (point-min))
          (omnidiff-toggle-fold)
          (omnidiff-next-hunk)
          (should-not (invisible-p (point)))
          (should (equal (get-text-property (point) 'omnidiff-file) "b.rs"))
          (omnidiff-prev-hunk)
          (should-not (invisible-p (point)))
          (should (equal (get-text-property (point) 'omnidiff-file) "b.rs")))))))

(ert-deftest omnidiff-render-test/json-styles-layouts-and-trailing-comma ()
  "JSON Overview renders 3 styles x 3 layouts with syntax and safe trailing commas."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((demo-a (expand-file-name "demo_a.py" dir))
             (demo-b (expand-file-name "demo_b.py" dir))
             (buf (generate-new-buffer " *omnidiff-ov-styles*"))
             (orig-width (frame-width (selected-frame))))
        (unwind-protect
            (progn
              (set-frame-width (selected-frame) 140)
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
              (let* ((f-demo (omnidiff-git-file-from-json
                              "demo.py" demo-a demo-b "demo.py"
                              (omnidiff-engine-run-pair demo-a demo-b 'default)))
                     (diff (omnidiff-test--make-diff f-demo)))
                (with-current-buffer buf
                  (omnidiff-mode)
                  (setq omnidiff--diff diff
                        omnidiff--repo-dir dir)
                  (dolist (layout '(unified side-by-side stacked))
                    (dolist (style '(token subtle-line traditional))
                      (setq omnidiff--layout layout
                            omnidiff--highlight-style style)
                      (let ((omnidiff-overview-auto-fold-trivial nil))
                        (omnidiff-render-buffer diff))
                      (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                        (should (string-search "demo.py" txt))
                        (should (string-search "'gamma'" txt))
                        (when (memq layout '(side-by-side stacked))
                          (should (string-search " BEFORE " txt))
                          (should (string-search " AFTER " txt))))
                      (goto-char (point-min))
                      (let* ((demo-ov (get-text-property (point-min) 'omnidiff-body-ov))
                             (body-faces (omnidiff-render-test--collect-faces-in
                                          (overlay-start demo-ov) (overlay-end demo-ov))))
                        (pcase style
                          ('token
                           (should (cl-some (lambda (f)
                                              (string-prefix-p "font-lock-" (symbol-name f)))
                                            body-faces))
                           (should (memq 'omnidiff-token-update body-faces))
                           (should (memq 'omnidiff-token-insert body-faces))
                           (dolist (wash '(omnidiff-added omnidiff-removed
                                           omnidiff-changed omnidiff-moved))
                             (should-not (memq wash body-faces))))
                          ('subtle-line
                           (should (cl-some (lambda (f)
                                              (string-prefix-p "font-lock-" (symbol-name f)))
                                            body-faces))
                           (should (memq 'omnidiff-token-update body-faces))
                           (should (memq 'omnidiff-changed body-faces)))
                          ('traditional
                           (should (memq 'omnidiff-changed body-faces))
                           (should (memq 'omnidiff-changed-refine body-faces)))))
                      ;; In unified layout, single-sided trailing-comma edit does NOT paint
                      ;; unchanged left `'beta'` line red across any highlight style.
                      (when (eq layout 'unified)
                        (goto-char (point-min))
                        (let ((found-left-beta nil))
                          (while (and (not found-left-beta) (search-forward "'beta'" nil t))
                            (let ((bol (line-beginning-position)))
                              (when (eq (get-text-property bol 'omnidiff-side) 'before)
                                (setq found-left-beta t)
                                (let ((line-faces (omnidiff-render-test--collect-faces-in
                                                   bol (line-end-position))))
                                  (dolist (red '(omnidiff-removed omnidiff-removed-refine
                                                 omnidiff-token-delete omnidiff-lineno-removed))
                                    (should-not (memq red line-faces)))))))
                          (should found-left-beta))))))))
          (set-frame-width (selected-frame) orig-width))))))

(ert-deftest omnidiff-render-test/summary-badge-and-auto-fold-trivial ()
  "Summary badge face and auto-folding work on whitespace-only files."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((demo-a (expand-file-name "demo_a.py" dir))
             (demo-b (expand-file-name "demo_b.py" dir))
             (ws-a (expand-file-name "ws_a.py" dir))
             (ws-b (expand-file-name "ws_b.py" dir))
             (buf (generate-new-buffer " *omnidiff-ov-trivial*")))
        (with-temp-file demo-a (insert "x = 1\n"))
        (with-temp-file demo-b (insert "x = 2\n"))
        (with-temp-file ws-a (insert "val = 1 + 2\n"))
        (with-temp-file ws-b (insert "val = 1   +   2\n"))
        (let* ((f-demo (omnidiff-git-file-from-json
                        "demo.py" demo-a demo-b "demo.py"
                        (omnidiff-engine-run-pair demo-a demo-b 'default)))
               (f-ws (omnidiff-git-file-from-json
                      "ws.py" ws-a ws-b "ws.py"
                      (omnidiff-engine-run-pair ws-a ws-b 'default)))
               (diff (omnidiff-test--make-diff (list f-demo f-ws))))
          (should (eq (omnidiff-engine-file-summary f-ws) 'whitespace-only))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--diff diff
                  omnidiff--layout 'unified
                  omnidiff--highlight-style 'token)
            (let ((omnidiff-overview-auto-fold-trivial 'whitespace-and-comments))
              (omnidiff-render-buffer diff)
              (goto-char (point-min))
              (let ((demo-ov (get-text-property (point-min) 'omnidiff-body-ov)))
                (should-not (overlay-get demo-ov 'invisible)))
              (search-forward "whitespace only")
              (should (memq 'omnidiff-summary-badge
                            (omnidiff-render-test--faces-at (match-beginning 0))))
              (let ((ws-ov (get-text-property (line-beginning-position) 'omnidiff-body-ov)))
                (should (eq (overlay-get ws-ov 'invisible) 'omnidiff-fold))))
            (let ((omnidiff-overview-auto-fold-trivial nil))
              (omnidiff-render-buffer diff)
              (goto-char (point-min))
              (search-forward "ws.py")
              (let ((ws-ov (get-text-property (line-beginning-position) 'omnidiff-body-ov)))
                (should-not (overlay-get ws-ov 'invisible))))))))))

(ert-deftest omnidiff-render-test/container-card-and-member-props ()
  "Structured ZIP container card renders summary counts and member properties."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "python3")))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((z-src-1 (expand-file-name "z1.py" dir))
             (z-src-2 (expand-file-name "z2.py" dir))
             (z-keep (expand-file-name "keep.txt" dir))
             (z-gone (expand-file-name "gone.txt" dir))
             (z-add (expand-file-name "added.txt" dir))
             (zip-a (expand-file-name "bundle_a.zip" dir))
             (zip-b (expand-file-name "bundle_b.zip" dir))
             (buf (generate-new-buffer " *omnidiff-ov-container*")))
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
        (let* ((f-zip (omnidiff-git-file-from-json
                       "bundle.zip" zip-a zip-b "bundle.zip"
                       (omnidiff-engine-run-pair zip-a zip-b 'default)
                       nil nil nil nil zip-a zip-b))
               (diff (omnidiff-test--make-diff f-zip)))
          (should (omnidiff-json-container-p (omnidiff-engine-file-container f-zip)))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--diff diff
                  omnidiff--repo-dir dir)
            (omnidiff-render-buffer diff)
            (goto-char (point-min))
            (search-forward "bundle.zip")
            (let ((card-text (buffer-substring-no-properties
                              (line-beginning-position) (point-max))))
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
              (should (eq (omnidiff-json-container-member-status cmem) 'changed)))))))))

(ert-deftest omnidiff-render-test/fontify-lines-hook-hygiene ()
  "`omnidiff-render--fontify-lines' suppresses font-lock and major-mode hooks."
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
                    'font-lock-keyword-face))))))

(ert-deftest omnidiff-render-test/truncate-path-and-context-header-hints ()
  "Smart path truncation keeps filenames and header hints adapt to UI context."
  ;; Smart path truncation preserves filename & handles renames.
  (let ((short (omnidiff-render--truncate-path "src/very/deep/nested/component/widget.tsx" 24))
        (ren (omnidiff-render--truncate-path
              "src/deep/old_dir/old_name.py → src/deep/new_dir/new_name.py" 32)))
    (should (<= (string-width short) 24))
    (should (string-suffix-p "widget.tsx" short))
    (should (<= (string-width ren) 32))
    (should (string-search " → " ren))
    (should (string-suffix-p "new_name.py" ren)))
  ;; Overview context-aware header hints include `? help' and vary by context.
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
    (should-not (string-search "d dual" std-h))))

(ert-deftest omnidiff-render-test/tab-width-mouse-maps-and-sbs-reflow-folds ()
  "Tab expansion, mouse click keymaps, and SBS resize reflow preserve folds."
  (omnidiff-test--with-buffer-cleanup
    (let ((buf (generate-new-buffer " *omnidiff-ergo-ov*"))
          (orig-width (frame-width (selected-frame))))
      (unwind-protect
          (let* ((omnidiff-overview-tab-width 2)
                 (f-text (omnidiff-test--make-file
                          :path "src/alpha.py"
                          :before-source "\told()\n"
                          :after-source "\tnew()\n"
                          :before-hunks (list (omnidiff-test--make-hunk 'update 0 1 0 4))
                          :after-hunks (list (omnidiff-test--make-hunk 'update 0 1 0 4))))
                 (f-cont (omnidiff-test--make-file
                          :path "pkg.zip"
                          :container
                          (make-omnidiff-json-container
                           :family 'archives :unchanged 1
                           :members (list (make-omnidiff-json-container-member
                                           :key "inside.txt" :status 'changed)))))
                 (diff (omnidiff-test--make-diff (list f-text f-cont))))
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
              ;; Fold first file, resize window narrower, trigger reflow -> fold preserved.
              (goto-char (point-min))
              (omnidiff-toggle-fold)
              (should (overlay-get (get-text-property (point) 'omnidiff-body-ov) 'invisible))
              (set-frame-width (selected-frame) 90)
              (omnidiff--reflow-sbs-buffer buf)
              (should (= omnidiff--last-sbs-width (window-body-width (selected-window))))
              (goto-char (point-min))
              (should (overlay-get (get-text-property (point) 'omnidiff-body-ov) 'invisible))))
        (set-frame-width (selected-frame) orig-width)))))

(ert-deftest omnidiff-render-test/json-render-plan-cache-and-lazy-folding ()
  "Overview caches render plans across layout/style cycles and lazily unfolds."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((buf (generate-new-buffer " *omnidiff-phase1-lazy*"))
            (a1 (expand-file-name "code-before.py" dir))
            (b1 (expand-file-name "code-after.py" dir))
            (a2 (expand-file-name "ws-before.py" dir))
            (b2 (expand-file-name "ws-after.py" dir)))
        (with-temp-file a1 (insert "def greet(name):\n    return name\n"))
        (with-temp-file b1 (insert "def greet(user):\n    return user\n"))
        (with-temp-file a2 (insert "def noop():\n    return 1\n"))
        (with-temp-file b2 (insert "def noop():\n        return 1\n"))
        (let* ((jd1 (omnidiff-engine-run-pair a1 b1 'default))
               (jd2 (omnidiff-engine-run-pair a2 b2 'default))
               (f1 (omnidiff-git-file-from-json "code.py" a1 b1 "code.py" jd1))
               (f2 (omnidiff-git-file-from-json "ws.py" a2 b2 "ws.py" jd2))
               (diff (omnidiff-test--make-diff (list f1 f2)))
               (orig-build (symbol-function 'omnidiff-render--build-json-plan))
               (built-paths nil))
          (should (eq (omnidiff-engine-file-summary f2) 'whitespace-only))
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
                  ;; 3. Switch to side-by-side while folded, then expand on demand.
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
                    (should (= (length built-paths) 2))
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
                  (should (= (length built-paths) 2))))
              ;; 5. Changing context-lines invalidates the cached plan and rebuilds.
              (let ((omnidiff-overview-context-lines 1)
                    (omnidiff-overview-auto-fold-trivial nil))
                (omnidiff-render-buffer diff)
                (should (= (length built-paths) 4))))))))))

;;; New P0/P1 regression tests for omnidiff-render.el

(ert-deftest omnidiff-render-test/tty-right-align-and-header-fit ()
  "TTY right-align uses compact column margins and fits 80-col headers."
  ;; 1. Unit checks for TTY vs GUI margin scaling.
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) nil)))
    (should (= (omnidiff-render--right-margin-units 0) 0))
    (should (= (omnidiff-render--right-margin-units 8) 1))
    (should (= (omnidiff-render--right-margin-units 16) 2))
    (should (= (omnidiff-render--right-align-px "abc" 8) 4))
    (should (= (omnidiff-render--right-align-px "abc" 16) 5)))
  (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) t)))
    (should (= (omnidiff-render--right-margin-units 8) 8))
    (should (= (omnidiff-render--right-margin-units 16) 16)))
  ;; 2. 80-col TTY header with long path, summary badge, large residual, and ✓ fits.
  (omnidiff-test--with-buffer-cleanup
    (let* ((buf (generate-new-buffer " *omnidiff-tty-hdr*"))
           (orig-width (frame-width (selected-frame)))
           (long-path
            "src/very/deeply/nested/packages/frontend/components/SuperLongComponentName.tsx")
           (f (omnidiff-test--make-file
               :path long-path
               :status 'modified
               :added 12 :removed 3 :updated 5
               :summary 'whitespace-only
               :before-source "const a = 1;\n"
               :after-source "const a = 2;\n"
               :before-hunks (list (omnidiff-test--make-hunk 'update 0 10 0 11))
               :after-hunks (list (omnidiff-test--make-hunk 'update 0 10 0 11))))
           (diff (omnidiff-test--make-diff f)))
      (unwind-protect
          (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _frame) nil)))
            (with-current-buffer buf
              (omnidiff-mode)
              (set-window-buffer (selected-window) buf)
              (set-frame-width (selected-frame) 80)
              (setq omnidiff--reviewed-files (make-hash-table :test #'equal))
              (puthash long-path "sig" omnidiff--reviewed-files)
              (let ((omnidiff-overview-auto-fold-trivial nil))
                (omnidiff-render-buffer diff))
              (goto-char (point-min))
              (let* ((bol (line-beginning-position))
                     (eol (line-end-position))
                     (align-pos (next-single-property-change bol 'display nil eol))
                     (align-spec (and align-pos (< align-pos eol)
                                      (get-text-property align-pos 'display)))
                     (align-cols (car (nth 2 (plist-get (cdr align-spec) :align-to))))
                     (rev-ov (get-text-property bol 'omnidiff-review-ov))
                     (rev-w (string-width (or (and rev-ov (overlay-get rev-ov 'before-string))
                                              "")))
                     (left-w (+ rev-w
                                (string-width (buffer-substring-no-properties bol align-pos))))
                     (stats-str (buffer-substring-no-properties (1+ align-pos) eol))
                     (stats-w (string-width stats-str)))
                (should align-spec)
                (should (integerp align-cols))
                ;; Right margin on TTY is 2 cols (not 16 cols!).
                (should (= align-cols (+ stats-w 2)))
                ;; Left content (including ✓ overlay) + right-aligned stats fit in 80 cols.
                (should (<= (+ left-w align-cols) 80))
                ;; Filename suffix survives truncation.
                (should (string-search "SuperLongComponentName.tsx"
                                       (buffer-substring-no-properties bol eol))))))
        (set-frame-width (selected-frame) orig-width)))))

(ert-deftest omnidiff-render-test/initial-sbs-width-tracking ()
  "`omnidiff-render-buffer' initializes `omnidiff--last-sbs-width' for SBS."
  (omnidiff-test--with-buffer-cleanup
    (let* ((buf (generate-new-buffer " *omnidiff-sbs-init-w*"))
           (f (omnidiff-test--make-json-file
               "a.py" "x = 1\n" "x = 2\n"
               :before-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))
               :after-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))))
           (diff (omnidiff-test--make-diff f)))
      (with-current-buffer buf
        (omnidiff-mode)
        (set-window-buffer (selected-window) buf)
        ;; Initial render in side-by-side records live window body width.
        (setq omnidiff--layout 'side-by-side
              omnidiff--last-sbs-width nil)
        (omnidiff-render-buffer diff)
        (should (eql omnidiff--last-sbs-width (window-body-width (selected-window))))
        ;; Rendering in unified resets tracked SBS width to nil.
        (setq omnidiff--layout 'unified)
        (omnidiff-render-buffer diff)
        (should-not omnidiff--last-sbs-width)
        ;; Rendering in stacked also keeps tracked SBS width nil.
        (setq omnidiff--layout 'stacked)
        (omnidiff-render-buffer diff)
        (should-not omnidiff--last-sbs-width)))))

(ert-deftest omnidiff-render-test/inverted-json-plan-large-file ()
  "Inverted JSON plan materializes only visible rows and skips identical files."
  (let* ((total-lines 2100)
         (base-lines (cl-loop for i from 0 below total-lines
                              collect (format "def fn_%d(x): return x + %d" i i)))
         (after-lines (copy-sequence base-lines)))
    (setf (nth 100 after-lines) "def fn_100(x): return x + 9999"
          (nth 101 after-lines) "def fn_101(x): return x + 8888")
    (let* ((b-src (concat (string-join base-lines "\n") "\n"))
           (a-src (concat (string-join after-lines "\n") "\n"))
           (f-changed (omnidiff-test--make-file
                       :path "big.py"
                       :before-source b-src
                       :after-source a-src
                       :before-hunks (list (omnidiff-test--make-hunk 'update 100 26 100 29)
                                           (omnidiff-test--make-hunk 'update 101 26 101 29))
                       :after-hunks (list (omnidiff-test--make-hunk 'update 100 26 100 30)
                                          (omnidiff-test--make-hunk 'update 101 26 101 30))))
           (f-same (omnidiff-test--make-file
                    :path "identical.py"
                    :before-source b-src
                    :after-source b-src))
           (orig-make-cell (symbol-function 'omnidiff-render--make-json-cell))
           (orig-fontify (symbol-function 'omnidiff-render--fontify-lines))
           (cell-calls 0)
           (fontify-args nil))
      (cl-letf (((symbol-function 'omnidiff-render--make-json-cell)
                 (lambda (&rest args)
                   (setq cell-calls (1+ cell-calls))
                   (apply orig-make-cell args)))
                ((symbol-function 'omnidiff-render--fontify-lines)
                 (lambda (src hint &optional needed-rows)
                   (push (list hint (length needed-rows) needed-rows) fontify-args)
                   (funcall orig-fontify src hint needed-rows))))
        ;; 1. Changed 2,100-line file allocates cells & fontifies ONLY visible hunk rows.
        (let* ((omnidiff-overview-context-lines 3)
               (omnidiff-overview-syntax-highlight t)
               (plan (omnidiff-render--build-json-plan
                      f-changed (omnidiff-file-json-diff f-changed)))
               (hunks (omnidiff-json-render-plan-hunks plan)))
          (should (= (length hunks) 1))
          (should (= (omnidiff-json-render-plan-max-line plan) total-lines))
          ;; 2 changed rows + 3 context above + 3 context below = 8 rows x 2 sides = 16 cells.
          (should (= cell-calls 16))
          (should (= (length fontify-args) 2))
          (dolist (call fontify-args)
            (should (= (nth 1 call) 8)))
          ;; Visible cells retain native Python font-lock faces.
          (let* ((first-row (car (omnidiff-json-overview-hunk-rows (car hunks))))
                 (lcell (omnidiff-json-aligned-row-left first-row))
                 (rcell (omnidiff-json-aligned-row-right first-row)))
            (should (eq (get-text-property 0 'face (omnidiff-json-line-cell-text lcell))
                        'font-lock-keyword-face))
            (should (eq (get-text-property 0 'face (omnidiff-json-line-cell-text rcell))
                        'font-lock-keyword-face))))
        ;; 2. Identical 2,100-line file performs ZERO fontify calls and ZERO cell allocations.
        (setq cell-calls 0
              fontify-args nil)
        (let ((plan (omnidiff-render--build-json-plan
                     f-same (omnidiff-file-json-diff f-same))))
          (should-not (omnidiff-json-render-plan-hunks plan))
          (should (= (omnidiff-json-render-plan-max-line plan) total-lines))
          (should (= cell-calls 0))
          (should-not fontify-args))))))

(ert-deftest omnidiff-render-test/duplicate-path-file-obj-tagging ()
  "Two `omnidiff-file' objects sharing a path carry distinct `omnidiff-file-obj' tags."
  (omnidiff-test--with-buffer-cleanup
    (let* ((buf (generate-new-buffer " *omnidiff-dup-path*"))
           (f1 (omnidiff-test--make-json-file
                "dup.py" "x = 1\n" "x = 2\n"
                :before-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))
                :after-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))))
           (f2 (omnidiff-test--make-json-file
                "dup.py" "x = 2\n" "x = 3\n"
                :before-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))
                :after-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))))
           (diff (omnidiff-test--make-diff (list f1 f2))))
      (should-not (eq f1 f2))
      (with-current-buffer buf
        (omnidiff-mode)
        (setq omnidiff--diff diff
              omnidiff--layout 'unified)
        ;; 1. Eager render: headers and bodies carry distinct `eq' `omnidiff-file-obj'.
        (let ((omnidiff-overview-auto-fold-trivial nil)
              (omnidiff-overview-auto-fold-threshold nil))
          (omnidiff-render-buffer diff)
          (goto-char (point-min))
          (let ((h1 (point))
                (b1-ov (get-text-property (point) 'omnidiff-body-ov)))
            (should (eq (get-text-property h1 'omnidiff-file-obj) f1))
            (should (eq (get-text-property (overlay-start b1-ov) 'omnidiff-file-obj) f1))
            (omnidiff-next-file)
            (let ((h2 (point))
                  (b2-ov (get-text-property (point) 'omnidiff-body-ov)))
              (should (> h2 h1))
              (should (eq (get-text-property h2 'omnidiff-file-obj) f2))
              (should (eq (get-text-property (overlay-start b2-ov) 'omnidiff-file-obj) f2)))))
        ;; 2. Lazy-folded render: materializing each body tags it with its own `eq' file obj.
        (let ((omnidiff-overview-auto-fold-trivial nil)
              (omnidiff-overview-auto-fold-threshold 2))
          (omnidiff-render-buffer diff)
          (goto-char (point-min))
          (let* ((h1 (point))
                 (ov1 (get-text-property h1 'omnidiff-body-ov)))
            (should (eq (overlay-get ov1 'omnidiff-lazy-file) f1))
            (omnidiff-next-file)
            (let* ((h2 (point))
                   (ov2 (get-text-property h2 'omnidiff-body-ov)))
              (should (eq (overlay-get ov2 'omnidiff-lazy-file) f2))
              ;; Materialize card 2 then card 1 and check both header and body tags.
              (omnidiff-toggle-fold)
              (should-not (overlay-get ov2 'omnidiff-lazy-file))
              (should (eq (get-text-property (overlay-start ov2) 'omnidiff-file-obj) f2))
              (goto-char h1)
              (omnidiff-toggle-fold)
              (should-not (overlay-get ov1 'omnidiff-lazy-file))
              (should (eq (get-text-property (overlay-start ov1) 'omnidiff-file-obj) f1))
              ;; Verify body 1 still belongs exclusively to f1 and body 2 to f2.
              (save-excursion
                (goto-char (overlay-start ov1))
                (search-forward "x = 1" (overlay-end ov1))
                (should (eq (get-text-property (match-beginning 0) 'omnidiff-file-obj) f1)))
              (save-excursion
                (goto-char (overlay-start ov2))
                (search-forward "x = 3" (overlay-end ov2))
                (should (eq (get-text-property (match-beginning 0) 'omnidiff-file-obj) f2))))))))))

(ert-deftest omnidiff-render-test/unified-file-and-hunk-stats ()
  "Stats count mixed rows, avoid move double-counting, and populate lazy headers."
  ;; 1. Mixed update (left) + insert (right) on the same aligned row counts BOTH.
  (let* ((f-mixed (omnidiff-test--make-file
                   :path "mixed.py"
                   :before-source "a = 1\n"
                   :after-source "a = 2\n"
                   :before-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))
                   :after-hunks (list (omnidiff-test--make-hunk 'insert 0 4 0 5))))
         (plan (omnidiff-render--build-json-plan f-mixed (omnidiff-file-json-diff f-mixed)))
         (hunk (car (omnidiff-json-render-plan-hunks plan)))
         (fstats (omnidiff-render-compute-json-file-stats (omnidiff-file-json-diff f-mixed) 1 1)))
    (should (= (omnidiff-json-overview-hunk-added hunk) 1))
    (should (= (omnidiff-json-overview-hunk-updated hunk) 1))
    (should (= (omnidiff-json-render-plan-added plan) 1))
    (should (= (omnidiff-json-render-plan-updated plan) 1))
    (should (= (plist-get fstats :added) 1))
    (should (= (plist-get fstats :updated) 1)))
  ;; 2. Cross-hunk 2-line move is NOT double-counted to 4 in plan or file stats.
  (let* ((mid (mapconcat (lambda (i) (format "mid_%d = %d" i i)) (number-sequence 1 16) "\n"))
         (b-src (concat "mv_a = 1\nmv_b = 2\n" mid "\n"))
         (a-src (concat mid "\nmv_a = 1\nmv_b = 2\n"))
         (mt-b (make-omnidiff-json-range :start-row 16 :start-col 0 :end-row 17 :end-col 8))
         (mt-a (make-omnidiff-json-range :start-row 0 :start-col 0 :end-row 1 :end-col 8))
         (f-move (omnidiff-test--make-file
                  :path "move.py"
                  :before-source b-src
                  :after-source a-src
                  :before-hunks (list (omnidiff-test--make-hunk 'move 0 0 1 8 mt-b))
                  :after-hunks (list (omnidiff-test--make-hunk 'move 16 0 17 8 mt-a))))
         (omnidiff-overview-context-lines 2)
         (plan (omnidiff-render--build-json-plan f-move (omnidiff-file-json-diff f-move)))
         (hunks (omnidiff-json-render-plan-hunks plan))
         (fstats (omnidiff-render-compute-json-file-stats
                  (omnidiff-file-json-diff f-move) 18 18)))
    (should (= (length hunks) 2))
    (should (= (omnidiff-json-overview-hunk-moved (nth 0 hunks)) 2))
    (should (= (omnidiff-json-overview-hunk-moved (nth 1 hunks)) 2))
    (should (= (omnidiff-json-render-plan-moved plan) 2))
    (should (= (plist-get fstats :moved) 2)))
  ;; 3. Lazy-folded file with unpopulated struct stats shows accurate header stats without plan.
  (omnidiff-test--with-buffer-cleanup
    (let* ((buf (generate-new-buffer " *omnidiff-lazy-stats*"))
           (f-lazy (omnidiff-test--make-file
                    :path "lazy.py"
                    :before-source "a = 1\nb = 2\nc = 3\n"
                    :after-source "a = 10\nc = 3\nd = 4\ne = 5\n"
                    :before-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5)
                                        (omnidiff-test--make-hunk 'delete 1 0 1 5))
                    :after-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 6)
                                       (omnidiff-test--make-hunk 'insert 2 0 3 5))))
           (diff (omnidiff-test--make-diff f-lazy))
           (plan-built 0))
      ;; Confirm file struct stats start at 0.
      (should (= (omnidiff-file-added f-lazy) 0))
      (should (= (omnidiff-file-removed f-lazy) 0))
      (should (= (omnidiff-file-updated f-lazy) 0))
      (cl-letf (((symbol-function 'omnidiff-render--build-json-plan)
                 (lambda (&rest _args)
                   (setq plan-built (1+ plan-built))
                   (error "Should not build plan for lazy-folded file"))))
        (with-current-buffer buf
          (omnidiff-mode)
          (let ((omnidiff-overview-auto-fold-threshold 1))
            (omnidiff-render-buffer diff))
          (should (= plan-built 0))
          (goto-char (point-min))
          (let ((hdr (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position))))
            (should (string-search "+2" hdr))
            (should (string-search "-1" hdr))
            (should (string-search "~1" hdr))
            (should (eq (overlay-get (get-text-property (point-min) 'omnidiff-body-ov)
                                     'omnidiff-lazy-file)
                        f-lazy))))))))

(ert-deftest omnidiff-render-test/static-op-style-and-batch-caches ()
  "Static `omnidiff-op-style' entries are `eq' and batch caches are shared."
  ;; 1. All 10 (op, moved-p) combinations return static `eq' `omnidiff-op-style' structs.
  (let ((seen nil))
    (dolist (op '(insert delete update move context))
      (dolist (moved-p '(nil t))
        (let ((s1 (omnidiff-render--op-style op moved-p))
              (s2 (omnidiff-render--op-style op moved-p)))
          (should (omnidiff-op-style-p s1))
          (should (eq s1 s2))
          (should-not (memq s1 seen))
          (push s1 seen))))
    (should (= (length seen) 10)))
  ;; 2. `omnidiff-render-call-with-batch-caches' binds caches and reuses them across nesting.
  (should-not omnidiff-render--sbs-metrics-cache)
  (should-not omnidiff-render--pixel-width-cache)
  (omnidiff-render-call-with-batch-caches
   (lambda ()
     (let ((outer-sbs omnidiff-render--sbs-metrics-cache)
           (outer-px omnidiff-render--pixel-width-cache))
       (should (hash-table-p outer-sbs))
       (should (hash-table-p outer-px))
       (let ((m1 (omnidiff-render--sbs-metrics 4))
             (w1 (omnidiff-render--string-pixel-width "sample-header")))
         (should (= (hash-table-count outer-sbs) 1))
         (should (= (hash-table-count outer-px) 1))
         (omnidiff-render-call-with-batch-caches
          (lambda ()
            (should (eq omnidiff-render--sbs-metrics-cache outer-sbs))
            (should (eq omnidiff-render--pixel-width-cache outer-px))
            (should (eq (omnidiff-render--sbs-metrics 4) m1))
            (should (= (omnidiff-render--string-pixel-width "sample-header") w1))))))))
  (should-not omnidiff-render--sbs-metrics-cache)
  (should-not omnidiff-render--pixel-width-cache))

(ert-deftest omnidiff-render-test/file-count-excludes-empty-target-placeholders ()
  "`omnidiff--file-count' and card indices exclude empty-target commit placeholders."
  (omnidiff-test--with-buffer-cleanup
    (let* ((buf (generate-new-buffer " *omnidiff-placeholder-count*"))
           (f-real (omnidiff-test--make-json-file
                    "real.py" "x = 1\n" "x = 2\n"
                    :before-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))
                    :after-hunks (list (omnidiff-test--make-hunk 'update 0 4 0 5))))
           (f-empty (make-omnidiff-file
                     :path ""
                     :target-path ""
                     :status 'modified
                     :preamble "commit deadbeef\nAuthor: Alice\n\n    empty commit\n"
                     :note "No file changes in commit"))
           (diff (omnidiff-test--make-diff (list f-real f-empty) "commit 1234567\n")))
      (with-current-buffer buf
        (omnidiff-mode)
        (setq omnidiff--diff diff
              omnidiff--layout 'unified)
        (omnidiff-render-buffer diff)
        (should (= omnidiff--file-count 1))
        (let ((hdr (substring-no-properties (omnidiff-render-header-line)))
              (txt (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-search "1 file" hdr))
          (should-not (string-search "2 files" hdr))
          (should (string-search "[1/1]" txt))
          (should-not (string-search "[2/1]" txt))
          (should-not (string-search "[2/2]" txt))
          (should (string-search "empty commit" txt))
          (should (string-search "No file changes in commit" txt)))
        ;; Placeholder-only diff still renders its preamble/note and reports 0 files.
        (let ((only-empty (omnidiff-test--make-diff (list f-empty))))
          (setq omnidiff--diff only-empty)
          (omnidiff-render-buffer only-empty)
          (should (= omnidiff--file-count 0))
          (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-search "empty commit" txt))
            (should (string-search "No file changes in commit" txt))
            (should-not (string-search "No changes to display." txt))))))))

(provide 'omnidiff-render-test)
;;; omnidiff-render-test.el ends here
