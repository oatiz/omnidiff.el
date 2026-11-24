;;; omnidiff-image-test.el --- ERT tests for omnidiff-image -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; This file is part of omnidiff.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Focused ERT test suite for `omnidiff-image.el': TTY/GUI comparison modes,
;; SVG preview and compositing, Git blob & LFS handling, external viewers,
;; live ImageMagick RGBA spotlight workers, cancellation, and boundary/perf
;; invariants.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'omnidiff-test-helper)

;;; Local fixture helpers

(defun omnidiff-image-test--make-standard-repo-diff (repo)
  "Populate REPO with PNG and SVG fixtures and return an `omnidiff-diff'."
  (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-red-2x2)
  (omnidiff-test--write-bin (expand-file-name "shape.png" repo) omnidiff-test--png-red-2x2)
  (omnidiff-test--write-bin (expand-file-name "gone.png" repo) omnidiff-test--png-red-2x2)
  (with-temp-file (expand-file-name "icon.svg" repo)
    (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\"><rect width=\"24\" height=\"24\" fill=\"red\"/></svg>\n"))
  (omnidiff-test--git repo "add" ".")
  (omnidiff-test--git repo "commit" "-q" "-m" "v1")
  (let ((oid-red (omnidiff-test--git repo "rev-parse" "HEAD:mod.png"))
        (oid-svg-old (omnidiff-test--git repo "rev-parse" "HEAD:icon.svg")))
    (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-blue-2x2)
    (omnidiff-test--write-bin (expand-file-name "shape.png" repo) omnidiff-test--png-wide-4x2)
    (delete-file (expand-file-name "gone.png" repo))
    (with-temp-file (expand-file-name "icon.svg" repo)
      (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\" viewBox=\"0 0 24 24\"><rect width=\"24\" height=\"24\" fill=\"blue\"/></svg>\n"))
    (omnidiff-test--git repo "add" "-A")
    (omnidiff-test--git repo "commit" "-q" "-m" "v2")
    (let ((oid-blue (omnidiff-test--git repo "rev-parse" "HEAD:mod.png"))
          (oid-wide (omnidiff-test--git repo "rev-parse" "HEAD:shape.png"))
          (oid-svg-new (omnidiff-test--git repo "rev-parse" "HEAD:icon.svg")))
      (omnidiff-test--make-diff
       (list
        (omnidiff-test--make-file
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
        (omnidiff-test--make-file
         :path "shape.png" :target-path "shape.png" :status 'modified
         :old-oid oid-red :new-oid oid-wide
         :picture (make-omnidiff-picture
                   :before (make-omnidiff-picture-info
                            :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                   :after (make-omnidiff-picture-info
                           :format "PNG" :width 4 :height 2 :color "RGB8" :bytes 73)
                   :comparison 'resized))
        (omnidiff-test--make-file
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
         :after-hunks (list (omnidiff-test--make-hunk 'update 0 6 0 10))))))))

(defun omnidiff-image-test--wait-workers (&optional timeout-sec)
  "Wait until `omnidiff-image--workers' in current buffer drains to nil."
  (should (omnidiff-test--wait-until
           (lambda () (null omnidiff-image--workers))
           (or timeout-sec 8.0))))

;;; Migrated tests: TTY, GUI modes, diff styles, limits, cleanup & SVG compositing

(ert-deftest omnidiff-image-test/tty-summary-bar-regions-and-ret ()
  "TTY summary renders metadata, bar, toggleable regions, and RET blobs."
  (skip-unless (executable-find "git"))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (let ((diff (omnidiff-image-test--make-standard-repo-diff repo))
            (buf (generate-new-buffer " *omnidiff-img-tty*")))
        (with-current-buffer buf
          (omnidiff-mode)
          (setq omnidiff--repo-dir repo
                omnidiff--diff diff
                omnidiff--layout 'unified)
          (omnidiff-render-buffer diff)
          (let ((tty (buffer-substring-no-properties (point-min) (point-max))))
            (should (string-search "PNG 2×2" tty))
            (should (string-search "100.0% Δ" tty))
            ;; Flattened raster cards omit redundant 1/1 "Image diff" hunk header.
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
          (should (string-search "Changed regions (showing 1 / 1):"
                                 (buffer-substring-no-properties (point-min) (point-max))))
          (should (string-prefix-p "▏" (omnidiff-image--format-tty-bar 0.1 20)))
          (should (equal (omnidiff-theme-color-hex "blue") "#0000ff"))
          ;; RET on deleted image opens read-only before-blob buffer.
          (goto-char (point-min))
          (search-forward "gone.png")
          (save-current-buffer
            (omnidiff-jump-to-source)
            (let ((opened (current-buffer)))
              (should (string-search "gone.png (before)" (buffer-name opened)))
              (should buffer-read-only)
              (kill-buffer opened)))
          ;; RET on historical `git show' modified image opens exact commit blob, not worktree edits.
          (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-wide-4x2)
          (let ((omnidiff--command '("git" "--no-pager" "show" "--ext-diff" "HEAD")))
            (goto-char (point-min))
            (save-current-buffer
              (omnidiff-jump-to-source)
              (let ((opened (current-buffer)))
                (should (string-search "mod.png (after)" (buffer-name opened)))
                (should (= (buffer-size opened) (length omnidiff-test--png-blue-2x2)))
                (kill-buffer opened)))))))))

(ert-deftest omnidiff-image-test/gui-modes-swipe-fold-loupe-zoom-backdrop-blink-onion ()
  "GUI modes support swipe, fold, loupe, zoom, backdrop, blink, and onion."
  (skip-unless (and (executable-find "git")
                    (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (let ((diff (omnidiff-image-test--make-standard-repo-diff repo))
            (buf (generate-new-buffer " *omnidiff-img-gui*")))
        (with-current-buffer buf
          (omnidiff-mode)
          (setq omnidiff--repo-dir repo
                omnidiff--diff diff
                omnidiff--layout 'unified)
          (omnidiff-test--with-gui-image
            (omnidiff-render-buffer diff)
            ;; Unified defaults to swipe (native slice specs), degrades resized to 2-up,
            ;; and inserts SVG preview above XML.
            (let ((txt (buffer-substring-no-properties (point-min) (point-max)))
                  (displays (omnidiff-test--collect-display-props)))
              (should (string-search "3:swipe 50%" txt))
              (should (string-search "SVG preview" txt))
              (should (string-search "fill=\"blue\"" txt))
              (should (cl-some (lambda (d)
                                 (and (consp d) (consp (car d))
                                      (eq (caar d) 'slice)))
                               displays)))
            ;; `n' lands on mod.png card -> image commands work directly.
            (goto-char (point-min))
            (omnidiff-next-hunk)
            (should (omnidiff-image-card-ov-at))
            ;; Fold flattened raster card: background rerender stays folded; user `l' unfolds.
            (omnidiff-toggle-fold)
            (let ((fov (get-text-property (point) 'omnidiff-body-ov))
                  (cov (omnidiff-image-card-ov-at)))
              (should (eq (overlay-get fov 'invisible) 'omnidiff-fold))
              (omnidiff-image--rerender-card cov nil)
              (should (eq (overlay-get fov 'invisible) 'omnidiff-fold))
              (omnidiff-image-adjust-increase)
              (should-not (overlay-get fov 'invisible))
              (should (string-search "3:swipe 55%"
                                     (buffer-substring-no-properties (point-min) (point-max)))))
            ;; Fine step, reset swipe to 50%, regions loupe + multi-file header tracking.
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
            ;; Expanding card #1 must not corrupt file header resolution on card #2.
            (save-excursion
              (goto-char (point-min))
              (omnidiff-next-file)
              (let ((hdr2 (line-beginning-position)))
                (forward-line 1)
                (should (= (omnidiff-render-file-header-pos-at (point)) hdr2))))
            (omnidiff-image-toggle-regions)
            ;; Zoom + backdrop cycle + non-destructive blink peek (`b' then `DEL').
            (let ((mod-file (car (omnidiff-diff-files diff))))
              (omnidiff-image-zoom-in)
              (should (omnidiff-image--state-zoom (omnidiff-image--state-for mod-file)))
              (omnidiff-image-zoom-reset)
              (should-not (omnidiff-image--state-zoom (omnidiff-image--state-for mod-file)))
              (let ((omnidiff-image-backdrop 'auto))
                (omnidiff-image-cycle-backdrop)
                (should (eq (omnidiff-image--state-backdrop (omnidiff-image--state-for mod-file))
                            'checker)))
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
              ;; Resized image refuses swipe & `='; single-sided image refuses `b'.
              (omnidiff-next-file)
              (should-error (omnidiff-image-mode-swipe) :type 'user-error)
              (should-error (omnidiff-image-adjust-reset) :type 'user-error)
              (omnidiff-next-file) ; gone.png (deleted)
              (should-error (omnidiff-image-toggle-side) :type 'user-error)
              ;; Plain `v' preserves pinned per-card mode; `C-u v' resets overrides.
              (goto-char (point-min))
              (omnidiff-cycle-layout) ; -> side-by-side
              (should (eq omnidiff--layout 'side-by-side))
              (should (eq (omnidiff-image--state-mode (omnidiff-image--state-for mod-file))
                          'onion))
              (omnidiff-cycle-layout t) ; C-u v -> stacked + reset pinned overrides
              (should (eq omnidiff--layout 'stacked))
              (should-not (omnidiff-image--state-mode (omnidiff-image--state-for mod-file))))))))))

(ert-deftest omnidiff-image-test/gui-diff-styles-cycling ()
  "Diff mode cycles cleanly across spotlight, mask, and SVG delta styles."
  (skip-unless (and (executable-find "git")
                    (image-type-available-p 'png)
                    (image-type-available-p 'svg)
                    (omnidiff-image--magick-convert-command)
                    (omnidiff-image--magick-command)))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (let ((diff (omnidiff-image-test--make-standard-repo-diff repo))
            (buf (generate-new-buffer " *omnidiff-img-styles*")))
        (with-current-buffer buf
          (omnidiff-mode)
          (setq omnidiff--repo-dir repo
                omnidiff--diff diff
                omnidiff--layout 'unified)
          (omnidiff-test--with-gui-image
            (let ((omnidiff-image-diff-style 'spotlight-sbs))
              (omnidiff-render-buffer diff)
              (goto-char (point-min))
              (omnidiff-next-hunk)
              (omnidiff-image-mode-diff)
              (should (string-search "4:diff"
                                     (buffer-substring-no-properties (point-min) (point-max))))
              (omnidiff-image-test--wait-workers)
              (let ((sbs-txt (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-search "spotlight 2-up" sbs-txt))
                (should (string-search "BEFORE" sbs-txt))
                (should (string-search "AFTER" sbs-txt)))
              ;; Cycle -> single spotlight.
              (omnidiff-image-cycle-diff-style)
              (omnidiff-image-test--wait-workers)
              (should (string-search "spotlight (s)"
                                     (buffer-substring-no-properties (point-min) (point-max))))
              ;; Cycle -> classic magick compare mask.
              (omnidiff-image-cycle-diff-style)
              (omnidiff-image-test--wait-workers)
              (should (string-search "mask (magick)"
                                     (buffer-substring-no-properties (point-min) (point-max))))
              ;; Cycle -> built-in SVG delta.
              (omnidiff-image-cycle-diff-style)
              (should (string-search "delta ×4.0 (RGB+α)"
                                     (buffer-substring-no-properties (point-min) (point-max))))
              ;; Cycle wraps back to spotlight 2-up.
              (omnidiff-image-cycle-diff-style)
              (omnidiff-image-test--wait-workers)
              (should (string-search "spotlight 2-up"
                                     (buffer-substring-no-properties (point-min) (point-max)))))))))))

(ert-deftest omnidiff-image-test/gui-narrow-auto-load-max-bytes-and-cleanup ()
  "Verify narrow fallback, deferral limits, `C-u L', and cleanup."
  (skip-unless (and (executable-find "git")
                    (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (let ((diff (omnidiff-image-test--make-standard-repo-diff repo))
            (buf (generate-new-buffer " *omnidiff-img-limits*")))
        (with-current-buffer buf
          (omnidiff-mode)
          (setq omnidiff--repo-dir repo
                omnidiff--diff diff)
          ;; 1. Narrow viewport degrades 2-up toolbar honestly to `2:stack · narrow window'.
          (let ((omnidiff-test--gui-viewport-px '(280 . 400)))
            (omnidiff-test--with-gui-image
              (setq omnidiff--layout 'side-by-side)
              (omnidiff-render-buffer diff)
              (let ((narrow (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-search "2:stack" narrow))
                (should (string-search "narrow window" narrow)))))
          ;; 2. Auto-load limit defers extra cards; `C-u L' loads all deferred cards.
          (omnidiff-test--with-gui-image
            (omnidiff-image-cleanup)
            (let ((omnidiff-image-auto-load-limit 1))
              (setq omnidiff--layout 'unified)
              (omnidiff-render-buffer diff)
              (should (string-search "Preview deferred (> 1 auto-loaded images)"
                                     (buffer-substring-no-properties (point-min) (point-max))))
              (goto-char (point-min))
              (omnidiff-image-force-load t)
              (should (omnidiff-image--state-force-load
                       (omnidiff-image--state-for (nth 1 (omnidiff-diff-files diff))))))
            ;; 3. Cleanup with PRESERVE-STATES-P clears caches & timers but keeps states;
            ;;    full cleanup clears states too.
            (setq omnidiff-image--resize-timer (run-with-idle-timer 60 nil #'ignore))
            (let ((timer omnidiff-image--resize-timer))
              (should (> (hash-table-count omnidiff-image--bytes-cache) 0))
              (should (> (hash-table-count omnidiff-image--states) 0))
              (omnidiff-image-cleanup t)
              (should-not omnidiff-image--resize-timer)
              (should-not (memq timer timer-idle-list))
              (should (= (hash-table-count omnidiff-image--bytes-cache) 0))
              (should (= (hash-table-count omnidiff-image--b64-cache) 0))
              (should (= (hash-table-count omnidiff-image--img-cache) 0))
              (should (= (hash-table-count omnidiff-image--slot-descs) 0))
              (should (= (hash-table-count omnidiff-image--magick-cache) 0))
              (should (> (hash-table-count omnidiff-image--states) 0))
              (omnidiff-image-cleanup nil)
              (should (= (hash-table-count omnidiff-image--states) 0)))
            ;; 4. Max-bytes guard defers oversized image until `L'.
            (let ((omnidiff-image-max-bytes 10))
              (omnidiff-render-buffer diff)
              (should (string-search
                       (format "exceeds %s auto-load limit" (omnidiff-engine-format-bytes 10))
                       (buffer-substring-no-properties (point-min) (point-max)))))))))))

(ert-deftest omnidiff-image-test/svg-compositing-auto-aim-and-compact-identical ()
  "Verify SVG compositing, region auto-aim, and compact identical card."
  (skip-unless (and (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
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
        ;; At 100% onion, BEFORE must not be embedded so erased alpha never leaks.
        (should-not (string-search (base64-encode-string omnidiff-test--png-red-2x2 t)
                                   svg-onion-100))
        ;; Region centered at x=80 of 100px width auto-aims swipe to 0.8.
        (should (= (omnidiff-image--default-swipe aim-pic 100) 0.8)))
      ;; Compact identical card shows IDENTICAL + `i:compare' and expands to swipe on `i'.
      (let* ((f-png (expand-file-name "same.png" dir))
             (buf (generate-new-buffer " *omnidiff-img-ident*")))
        (omnidiff-test--write-bin f-png omnidiff-test--png-red-2x2)
        (let ((ident-diff
               (omnidiff-test--make-diff
                (omnidiff-test--make-file
                 :path "same.png" :target-path "same.png" :status 'modified
                 :before-disk-path f-png :after-disk-path f-png
                 :picture (make-omnidiff-picture
                           :before (make-omnidiff-picture-info
                                    :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                           :after (make-omnidiff-picture-info
                                   :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                           :comparison 'identical :changed-pct 0.0)))))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff ident-diff
                  omnidiff--layout 'unified)
            (omnidiff-test--with-gui-image
              (omnidiff-render-buffer ident-diff)
              (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-search "IDENTICAL" txt))
                (should (string-search "i:compare" txt)))
              (goto-char (point-min))
              (omnidiff-image-cycle-mode)
              (should (string-search "3:swipe"
                                     (buffer-substring-no-properties (point-min) (point-max)))))))))))

;;; Migrated tests: Git image parity, ergonomics, omnidiff-files PNG, binary blob, keymaps

(ert-deftest omnidiff-image-test/git-parallel-and-serial-parity ()
  "Parallel and serial Git pipelines produce identical picture/SVG diffs."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (cl-flet ((run-show (max-par)
                  (let ((omnidiff-max-parallel max-par)
                        (buf (generate-new-buffer (format " *omnidiff-img-par-%d*" max-par))))
                    (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                                  "show HEAD" repo 'default 'unified)
                    (omnidiff-test--wait-request buf)
                    (with-current-buffer buf
                      (list (buffer-substring-no-properties (point-min) (point-max))
                            (mapcar (lambda (f)
                                      (let ((pic (omnidiff-engine-file-picture f)))
                                        (list (omnidiff-file-path f)
                                              (omnidiff-file-target-path f)
                                              (omnidiff-file-status f)
                                              (omnidiff-file-old-oid f)
                                              (omnidiff-file-new-oid f)
                                              (and pic (omnidiff-picture-comparison pic)))))
                                    (omnidiff-diff-files omnidiff--diff)))))))
        (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin (expand-file-name "del.png" repo) omnidiff-test--png-red-2x2)
        (with-temp-file (expand-file-name "vec.svg" repo)
          (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\"><rect width=\"10\" height=\"10\" fill=\"red\"/></svg>\n"))
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "seed images")
        (omnidiff-test--write-bin (expand-file-name "mod.png" repo) omnidiff-test--png-blue-2x2)
        (delete-file (expand-file-name "del.png" repo))
        (omnidiff-test--write-bin (expand-file-name "add.png" repo) omnidiff-test--png-wide-4x2)
        (with-temp-file (expand-file-name "vec.svg" repo)
          (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\"><rect width=\"10\" height=\"10\" fill=\"blue\"/></svg>\n"))
        (omnidiff-test--git repo "add" "-A")
        (omnidiff-test--git repo "commit" "-q" "-m" "update images")
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
            (should-not (nth 5 vec))))))))

(ert-deftest omnidiff-image-test/swipe-endpoints-partial-decode-and-region-stepping ()
  "Verify partial-decode notice, zero-Vernier swipe math, and region step."
  (skip-unless (and (executable-find "git")
                    (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (omnidiff-test--write-bin (expand-file-name "a.png" repo) omnidiff-test--png-red-2x2)
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "v1")
      (let ((oid-red (omnidiff-test--git repo "rev-parse" "HEAD:a.png")))
        (omnidiff-test--write-bin (expand-file-name "a.png" repo) omnidiff-test--png-blue-2x2)
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "v2")
        (let* ((oid-blue (omnidiff-test--git repo "rev-parse" "HEAD:a.png"))
               (diff
                (omnidiff-test--make-diff
                 (list
                  (omnidiff-test--make-file
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
                  (omnidiff-test--make-file
                   :path "partial.png" :target-path "partial.png" :status 'modified
                   :old-oid (make-string 40 ?f) :new-oid oid-blue
                   :picture (make-omnidiff-picture
                             :before (make-omnidiff-picture-info
                                      :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                             :after (make-omnidiff-picture-info
                                     :format "PNG" :width 2 :height 2 :color "RGB8" :bytes 73)
                             :comparison 'unknown :raw-comparison "backend note")))))
               (buf (generate-new-buffer " *omnidiff-img-ergo*")))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir repo
                  omnidiff--diff diff
                  omnidiff--layout 'unified)
            (omnidiff-test--with-gui-image
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
                (omnidiff-image--rerender-card (omnidiff-image-card-ov-at) t)
                (should (string-search "3:swipe 0%"
                                       (buffer-substring-no-properties (point-min) (point-max))))
                (should-not (cl-some (lambda (d) (and (consp d) (consp (car d)) (eq (caar d) 'slice)))
                                     (omnidiff-test--collect-display-props)))
                (setf (omnidiff-image--state-swipe st) 1.0)
                (omnidiff-image--rerender-card (omnidiff-image-card-ov-at) t)
                (should (string-search "3:swipe 100%"
                                       (buffer-substring-no-properties (point-min) (point-max))))
                (should-not (cl-some (lambda (d) (and (consp d) (consp (car d)) (eq (caar d) 'slice)))
                                     (omnidiff-test--collect-display-props))))
              ;; 4. Stepping regions `>' / `<' moves active region 1/2 -> 2/2 -> 1/2.
              (omnidiff-image-toggle-regions)
              (should (string-search "Region 1/2"
                                     (buffer-substring-no-properties (point-min) (point-max))))
              (omnidiff-image-next-region)
              (should (string-search "Region 2/2"
                                     (buffer-substring-no-properties (point-min) (point-max))))
              (omnidiff-image-prev-region)
              (should (string-search "Region 1/2"
                                     (buffer-substring-no-properties (point-min) (point-max)))))))))))

(ert-deftest omnidiff-image-test/diff-delta-gain-and-reset ()
  "SVG `delta' diff style steps gain interactively and resets cleanly with `='."
  (skip-unless (and (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((fa (expand-file-name "before.png" dir))
             (fb (expand-file-name "after.png" dir))
             (buf (generate-new-buffer " *omnidiff-img-delta*")))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        (let* ((pic (make-omnidiff-picture
                     :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :comparison 'pixels :changed-pct 100.0))
               (file (omnidiff-test--make-file
                      :path "a.png" :target-path "a.png" :status 'modified
                      :before-disk-path fa :after-disk-path fb :picture pic))
               (diff (omnidiff-test--make-diff file)))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'unified)
            (omnidiff-test--with-gui-image
              (let ((omnidiff-image-diff-style 'delta))
                (omnidiff-render-buffer diff)
                (goto-char (point-min))
                (omnidiff-image-mode-diff)
                (should (string-search "delta ×4.0"
                                       (buffer-substring-no-properties (point-min) (point-max))))
                (omnidiff-image-adjust-increase)
                (should (string-search "delta ×8.0"
                                       (buffer-substring-no-properties (point-min) (point-max))))
                (omnidiff-image-adjust-reset)
                (should (string-search "delta ×4.0"
                                       (buffer-substring-no-properties (point-min) (point-max))))))))))))

(ert-deftest omnidiff-image-test/refresh-preserves-image-card-state ()
  "Refreshing (`g') preserves per-card image mode and opacity across re-run."
  (skip-unless (and (executable-find omnidiff-executable)
                    (executable-find "git")
                    (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (omnidiff-test--write-bin (expand-file-name "a.png" repo) omnidiff-test--png-red-2x2)
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "v1")
      (omnidiff-test--write-bin (expand-file-name "a.png" repo) omnidiff-test--png-blue-2x2)
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "v2")
      (let ((buf (generate-new-buffer " *omnidiff-img-refresh*")))
        (omnidiff-test--with-gui-image
          (omnidiff-run buf '("git" "--no-pager" "show" "--ext-diff" "HEAD")
                        "show HEAD" repo 'default 'unified)
          (omnidiff-test--wait-request buf)
          (with-current-buffer buf
            (goto-char (point-min))
            (omnidiff-next-hunk)
            (omnidiff-image-mode-onion)
            (omnidiff-image-adjust-increase) ; -> 60%
            (omnidiff-refresh))
          (omnidiff-test--wait-request buf)
          (with-current-buffer buf
            (let ((st (omnidiff-image--state-for (car (omnidiff-diff-files omnidiff--diff)))))
              (should (eq (omnidiff-image--state-mode st) 'onion))
              (should (= (round (* 100.0 (omnidiff-image--state-opacity st))) 60)))))))))

(ert-deftest omnidiff-image-test/omnidiff-files-png-pair ()
  "`omnidiff-files' on a PNG pair loads both sides without Git OIDs."
  (skip-unless (and (executable-find omnidiff-executable)
                    (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((png-a (expand-file-name "before.png" dir))
            (png-b (expand-file-name "after.png" dir)))
        (omnidiff-test--write-bin png-a omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin png-b omnidiff-test--png-blue-2x2)
        (omnidiff-test--with-gui-image
          (let ((img-buf (omnidiff-files png-a png-b)))
            (should (buffer-live-p img-buf))
            (with-current-buffer img-buf
              (should (derived-mode-p 'omnidiff-mode))
              (let* ((f (car (omnidiff-diff-files omnidiff--diff)))
                     (pic (omnidiff-engine-file-picture f)))
                (should (omnidiff-picture-p pic))
                (should-not (omnidiff-file-old-oid f))
                (should-not (omnidiff-file-new-oid f))
                (should (equal (omnidiff-image--fetch-side-bytes f pic 'before dir t)
                               omnidiff-test--png-red-2x2))
                (should (equal (omnidiff-image--fetch-side-bytes f pic 'after dir t)
                               omnidiff-test--png-blue-2x2))
                (let ((txt (buffer-substring-no-properties (point-min) (point-max))))
                  (should (string-search "3:swipe" txt))
                  (should-not (string-search "BEFORE unavailable" txt)))))))))))

(ert-deftest omnidiff-image-test/read-git-blob-preserves-binary-under-dos-coding ()
  "`omnidiff-image--read-git-blob' preserves raw CRLF PNG bytes under DOS."
  (skip-unless (executable-find "git"))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--write-bin (expand-file-name "dot.png" repo) omnidiff-test--png-red-2x2)
    (omnidiff-test--git repo "add" "dot.png")
    (omnidiff-test--git repo "commit" "-q" "-m" "add png")
    (let* ((oid (omnidiff-test--git repo "rev-parse" "HEAD:dot.png"))
           (default-process-coding-system '(utf-8-dos . utf-8-dos))
           (raw (omnidiff-image--read-git-blob oid "dot.png" repo nil)))
      (should (stringp raw))
      (should-not (multibyte-string-p raw))
      (should (string-prefix-p omnidiff-image--png-magic raw))
      (should (= (aref raw 4) #x0d))
      (should (= (aref raw 5) #x0a))
      (should (equal raw omnidiff-test--png-red-2x2)))))

(ert-deftest omnidiff-image-test/keymaps-non-hijack-and-del-scroll-fallback ()
  "Image header map stays safe; 2-up avoids hijack; DEL scrolls outside peek."
  ;; Header/card keymap bindings (pure, no GUI required).
  (should-not (lookup-key omnidiff-image-header-map (kbd "<left>")))
  (should-not (lookup-key omnidiff-image-header-map (kbd "<right>")))
  (should-not (lookup-key omnidiff-image-header-map (kbd "h")))
  (should-not (lookup-key omnidiff-image-header-map (kbd "l")))
  (should (eq (lookup-key omnidiff-image-card-map (kbd "<left>"))
              #'omnidiff-image-adjust-decrease))
  (should (eq (lookup-key omnidiff-image-card-map (kbd "h"))
              #'omnidiff-image-adjust-decrease))
  ;; Interactive 2-up non-hijack and DEL fallback.
  (skip-unless (and (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (buf (generate-new-buffer " *omnidiff-ergo-img*")))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        (let* ((pic (make-omnidiff-picture
                     :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :comparison 'pixels :changed-pct 100.0 :changed-pixels 4 :total-pixels 4))
               (file (omnidiff-image-make-file-from-paths fa fb pic "icon.png"))
               (diff (omnidiff-test--make-diff file)))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--diff diff
                  omnidiff--layout 'side-by-side)
            (let ((omnidiff-test--gui-viewport-px '(1000 . 600)))
              (omnidiff-test--with-gui-image
                (omnidiff-render-buffer diff)
                (goto-char (point-min))
                (let ((hmap (get-text-property (point) 'keymap)))
                  (should (eq (lookup-key hmap [mouse-1]) #'omnidiff-render--mouse-toggle-fold))
                  (should-not (lookup-key hmap (kbd "<left>"))))
                ;; On 2-up mode, adjust-increase does not hijack into swipe.
                (omnidiff-image-adjust-increase)
                (let ((st (omnidiff-image--state-for file)))
                  (should-not (eq (omnidiff-image--state-mode st) 'swipe))
                  (should-not (omnidiff-image--state-mode-pinned st)))
                ;; DEL outside blink peek delegates to `scroll-down-command'.
                (let ((scrolled nil))
                  (cl-letf (((symbol-function 'scroll-down-command)
                             (lambda (&optional _arg) (interactive) (setq scrolled t))))
                    (omnidiff-image-return-from-peek)
                    (should scrolled)))))))))))

;;; New P0/P1 regression tests

(ert-deftest omnidiff-image-test/external-viewer-argv-and-open-external ()
  "External viewer resolves nil/string/list and spawns `xdg-open' per path."
  ;; 1. `omnidiff-image--external-viewer-argv': nil auto-detects instead of shorting via `listp'.
  (let ((omnidiff-image-external-viewer nil))
    (let ((system-type 'darwin))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name &rest _) (when (equal name "open") "/usr/bin/open"))))
        (should (equal (omnidiff-image--external-viewer-argv) '("/usr/bin/open")))))
    (let ((system-type 'gnu/linux))
      (cl-letf (((symbol-function 'executable-find)
                 (lambda (name &rest _) (when (equal name "xdg-open") "/usr/bin/xdg-open"))))
        (should (equal (omnidiff-image--external-viewer-argv) '("/usr/bin/xdg-open")))))
    (let ((system-type 'gnu/linux))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
        (should-not (omnidiff-image--external-viewer-argv)))))
  ;; Explicit string & list overrides, plus empty string falling through to auto-detect.
  (let ((omnidiff-image-external-viewer "feh"))
    (should (equal (omnidiff-image--external-viewer-argv) '("feh"))))
  (let ((omnidiff-image-external-viewer '("imv" "-r")))
    (should (equal (omnidiff-image--external-viewer-argv) '("imv" "-r"))))
  (let ((omnidiff-image-external-viewer "")
        (system-type 'darwin))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (name &rest _) (when (equal name "open") "/usr/bin/open"))))
      (should (equal (omnidiff-image--external-viewer-argv) '("/usr/bin/open")))))
  ;; 2. `omnidiff-image-open-external': `xdg-open' spawns one process per exported file;
  ;;    non-xdg-open viewers receive both paths in a single `start-process' call.
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (buf (generate-new-buffer " *omnidiff-ext-viewer*"))
            (orig-ext-tmpdirs omnidiff-image--ext-tmpdirs)
            (created-tmpdirs nil))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        (unwind-protect
            (let* ((pic (make-omnidiff-picture
                         :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                         :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                         :comparison 'pixels))
                   (file (omnidiff-image-make-file-from-paths fa fb pic "logo.png"))
                   (diff (omnidiff-test--make-diff file)))
              (with-current-buffer buf
                (omnidiff-mode)
                (setq omnidiff--repo-dir dir
                      omnidiff--diff diff)
                (omnidiff-render-buffer diff)
                (goto-char (point-min))
                (let ((calls nil)
                      (omnidiff-image-external-viewer '("/usr/bin/xdg-open")))
                  (cl-letf (((symbol-function 'start-process)
                             (lambda (_name _buf prog &rest args)
                               (push (cons prog args) calls)
                               nil)))
                    (omnidiff-image-open-external))
                  (setq calls (nreverse calls))
                  (should (= (length calls) 2))
                  (should (equal (caar calls) "/usr/bin/xdg-open"))
                  (should (= (length (cdar calls)) 1))
                  (should (= (length (cdr (cadr calls))) 1))
                  (let ((p1 (cadar calls))
                        (p2 (cadr (cadr calls))))
                    (should (string-suffix-p "logo-before.png" p1))
                    (should (string-suffix-p "logo-after.png" p2))
                    (should (equal (omnidiff-image--read-file-bytes p1 t)
                                   omnidiff-test--png-red-2x2))
                    (should (equal (omnidiff-image--read-file-bytes p2 t)
                                   omnidiff-test--png-blue-2x2))))
                ;; Non-xdg-open viewer receives both paths in one call.
                (let ((calls nil)
                      (omnidiff-image-external-viewer '("preview-tool" "--compare")))
                  (cl-letf (((symbol-function 'start-process)
                             (lambda (_name _buf prog &rest args)
                               (push (cons prog args) calls)
                               nil)))
                    (omnidiff-image-open-external))
                  (should (= (length calls) 1))
                  (should (equal (nth 0 (car calls)) "preview-tool"))
                  (should (equal (nth 1 (car calls)) "--compare"))
                  (should (= (length (car calls)) 4)))))
          (dolist (d omnidiff-image--ext-tmpdirs)
            (unless (memq d orig-ext-tmpdirs)
              (push d created-tmpdirs)))
          (setq omnidiff-image--ext-tmpdirs orig-ext-tmpdirs)
          (dolist (d created-tmpdirs)
            (when (file-directory-p d)
              (delete-directory d t))))))))

(ert-deftest omnidiff-image-test/live-magick-rgba-spotlight ()
  "Live `magick' RGBA spotlight avoids false positives and catches edits."
  (skip-unless (and (executable-find "magick")
                    (omnidiff-image--magick-convert-command)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((buf (generate-new-buffer " *omnidiff-rgba-spot*"))
            (orig-spawn (symbol-function 'omnidiff-image--spawn-magick-worker)))
        (cl-flet*
            ((pixel-hex (png-path x y)
               (with-temp-buffer
                 (unless (zerop (call-process "magick" nil t nil png-path
                                              "-format" (format "%%[hex:u.p{%d,%d}]" x y)
                                              "info:"))
                   (error "magick identify failed on %s" png-path))
                 (string-trim (buffer-string))))
             (run-spotlight-and-mask (tag b-bytes a-bytes)
               "Run spotlight worker and evaluate its exact `mpr:mask' pipeline."
               (let ((captured-builder nil)
                     (key (list 'spotlight tag))
                     (mask-png (expand-file-name (format "%s-mask.png" tag) dir))
                     (spot-png (expand-file-name (format "%s-spot.png" tag) dir))
                     (tmp-b (expand-file-name (format "%s-b.png" tag) dir))
                     (tmp-a (expand-file-name (format "%s-a.png" tag) dir)))
                 (cl-letf (((symbol-function 'omnidiff-image--spawn-magick-worker)
                            (lambda (&rest args)
                              (setq captured-builder (nth 11 args))
                              (apply orig-spawn args))))
                   (omnidiff-image--start-magick-spotlight
                    (list tag) key b-bytes 'png a-bytes 'png 'spotlight t 0 nil))
                 (omnidiff-image-test--wait-workers 10.0)
                 (let ((out-bytes (gethash key omnidiff-image--magick-cache)))
                   (should (stringp out-bytes))
                   (should (string-prefix-p omnidiff-image--png-magic out-bytes))
                   (omnidiff-test--write-bin spot-png out-bytes))
                 ;; Evaluate the exact production argv prefix up to `-write mpr:mask'.
                 (should (functionp captured-builder))
                 (omnidiff-test--write-bin tmp-b b-bytes)
                 (omnidiff-test--write-bin tmp-a a-bytes)
                 (let* ((full-cmd (funcall captured-builder dir tmp-b tmp-a))
                        (mask-pos (cl-position "mpr:mask" full-cmd :test #'equal))
                        (mask-cmd (append (cl-subseq full-cmd 0 (1- mask-pos))
                                          (list mask-png))))
                   (should (integerp mask-pos))
                   (should (zerop (apply #'call-process (car mask-cmd) nil nil nil (cdr mask-cmd)))))
                 (list :mask-tl (pixel-hex mask-png 0 0)
                       :mask-br (pixel-hex mask-png 3 3)
                       :spot-tl (pixel-hex spot-png 0 0)
                       :spot-br (pixel-hex spot-png 3 3))))
             (run-spotlight-sbs (tag b-bytes a-bytes dark-p)
               "Run `spotlight-sbs' worker and return `(BEFORE-PNG . AFTER-PNG)' paths."
               (let ((key (list 'spotlight-sbs tag dark-p))
                     (out-b (expand-file-name (format "%s-%s-b.png" tag dark-p) dir))
                     (out-a (expand-file-name (format "%s-%s-a.png" tag dark-p) dir)))
                 (omnidiff-image--start-magick-spotlight
                  (list tag dark-p) key b-bytes 'png a-bytes 'png 'spotlight-sbs dark-p 0 nil)
                 (omnidiff-image-test--wait-workers 10.0)
                 (let ((pair (gethash key omnidiff-image--magick-cache)))
                   (should (consp pair))
                   (should (string-prefix-p omnidiff-image--png-magic (car pair)))
                   (should (string-prefix-p omnidiff-image--png-magic (cdr pair)))
                   (omnidiff-test--write-bin out-b (car pair))
                   (omnidiff-test--write-bin out-a (cdr pair))
                   (cons out-b out-a))))
             (make-png (name &rest magick-args)
               (let ((path (expand-file-name name dir)))
                 (should (zerop (apply #'call-process "magick" nil nil nil
                                       (append magick-args (list (concat "PNG32:" path))))))
                 (omnidiff-image--read-file-bytes path t))))
          (with-current-buffer buf
            (omnidiff-mode)
            (omnidiff-image--ensure-caches)
            ;; 1. Identical semi-transparent RGBA(128,64,32,128) -> 0 changed pixels,
            ;;    desaturated neutral RGB and exact preserved alpha byte 0x80.
            (let ((ident (run-spotlight-and-mask
                          "ident"
                          omnidiff-test--png-rgba-base-4x4
                          omnidiff-test--png-rgba-base-4x4)))
              (should (equal (plist-get ident :mask-tl) "000000"))
              (should (equal (plist-get ident :mask-br) "000000"))
              (let ((tl (plist-get ident :spot-tl)))
                (should (= (length tl) 8))
                (should (equal (substring tl 0 2) (substring tl 2 4)))
                (should (equal (substring tl 2 4) (substring tl 4 6)))
                (should (equal (substring tl 6 8) "80"))))
            ;; 2. Alpha-only change (128 -> 200=0xC8) -> #FFFFFF mask, chromatic RGB, alpha 0xC8.
            (let ((alpha (run-spotlight-and-mask
                          "alpha"
                          omnidiff-test--png-rgba-base-4x4
                          omnidiff-test--png-rgba-alpha-4x4)))
              (should (equal (plist-get alpha :mask-tl) "FFFFFF"))
              (should (equal (plist-get alpha :mask-br) "FFFFFF"))
              (let ((tl (plist-get alpha :spot-tl)))
                (should (= (length tl) 8))
                (should-not (equal (substring tl 0 2) (substring tl 4 6)))
                (should (equal (substring tl 6 8) "C8"))))
            ;; 3. RGB-only change (alpha=128) -> #FFFFFF mask, chromatic RGB, alpha 0x80.
            (let ((rgb (run-spotlight-and-mask
                        "rgb"
                        omnidiff-test--png-rgba-base-4x4
                        omnidiff-test--png-rgba-rgb-4x4)))
              (should (equal (plist-get rgb :mask-tl) "FFFFFF"))
              (should (equal (plist-get rgb :mask-br) "FFFFFF"))
              (let ((tl (plist-get rgb :spot-tl)))
                (should (= (length tl) 8))
                (should-not (equal (substring tl 0 2) (substring tl 4 6)))
                (should (equal (substring tl 6 8) "80"))))
            ;; 4. Partial alpha change -> top-left #FFFFFF (0xC8), bottom-right #000000 (0x80).
            (let ((part (run-spotlight-and-mask
                         "part"
                         omnidiff-test--png-rgba-base-4x4
                         omnidiff-test--png-rgba-partial-alpha-4x4)))
              (should (equal (plist-get part :mask-tl) "FFFFFF"))
              (should (equal (plist-get part :mask-br) "000000"))
              (should (equal (substring (plist-get part :spot-tl) 6 8) "C8"))
              (should (equal (substring (plist-get part :spot-br) 6 8) "80")))
            ;; 5. `spotlight-sbs' on changed semi-transparent (128,64,32,128 -> 128,64,32,204):
            ;;    BEFORE keeps alpha 0x80, AFTER keeps alpha 0xCC, both chromatic tinted.
            (let* ((a-204 (make-png "semi-204.png" "-size" "4x4" "xc:rgba(128,64,32,0.8)"))
                   (sbs (run-spotlight-sbs "semi-sbs"
                                           omnidiff-test--png-rgba-base-4x4 a-204 t))
                   (b-px (pixel-hex (car sbs) 0 0))
                   (a-px (pixel-hex (cdr sbs) 0 0)))
              (should (= (length b-px) 8))
              (should (= (length a-px) 8))
              (should (equal (substring b-px 6 8) "80"))
              (should (equal (substring a-px 6 8) "CC"))
              (should-not (equal (substring b-px 0 2) (substring b-px 4 6)))
              (should-not (equal (substring a-px 0 2) (substring a-px 4 6))))
            ;; 6. `spotlight-sbs' transparent (0,0,0,0) -> opaque red (255,0,0,255) center 2x2:
            ;;    BEFORE changed pixel stays alpha 00 (NOT opaque #000000FF!), and 1px contour
            ;;    rim over transparent pixels keeps partial 35%/40% alpha (0x59 / 0x66, NOT 0xFF).
            (let* ((trans-b (make-png "trans-b.png" "-size" "6x6" "xc:rgba(0,0,0,0)"))
                   (red-a (make-png "red-a.png" "-size" "6x6" "xc:rgba(0,0,0,0)"
                                    "-fill" "rgba(255,0,0,1)"
                                    "-draw" "rectangle 2,2 3,3"))
                   (dark-sbs (run-spotlight-sbs "trans-red" trans-b red-a t))
                   (light-sbs (run-spotlight-sbs "trans-red" trans-b red-a nil)))
              ;; Dark theme (35% contour -> 0x59):
              (let ((b-unchg (pixel-hex (car dark-sbs) 0 0))
                    (b-chg (pixel-hex (car dark-sbs) 2 2))
                    (b-edge (pixel-hex (car dark-sbs) 1 2))
                    (a-chg (pixel-hex (cdr dark-sbs) 2 2))
                    (a-edge (pixel-hex (cdr dark-sbs) 1 2)))
                (should (equal (substring b-unchg 6 8) "00"))
                (should (equal (substring b-chg 6 8) "00"))
                (should (equal (substring a-chg 6 8) "FF"))
                (should-not (equal (substring a-chg 0 2) (substring a-chg 4 6)))
                (should (equal (substring b-edge 6 8) "59"))
                (should (equal (substring a-edge 6 8) "59")))
              ;; Light theme (40% contour -> 0x66):
              (let ((b-chg (pixel-hex (car light-sbs) 2 2))
                    (b-edge (pixel-hex (car light-sbs) 1 2)))
                (should (equal (substring b-chg 6 8) "00"))
                (should (equal (substring b-edge 6 8) "66"))))))))))

(ert-deftest omnidiff-image-test/live-worker-cancellation-cleanup ()
  "Cancelling a live worker skips rerenders, clears cache, and drops tmpdir."
  (skip-unless (and (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (buf (generate-new-buffer " *omnidiff-cancel-worker*")))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        (let* ((pic (make-omnidiff-picture
                     :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :comparison 'pixels))
               (file (omnidiff-image-make-file-from-paths fa fb pic "cancel.png"))
               (diff (omnidiff-test--make-diff file)))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'unified)
            (omnidiff-test--with-gui-image
              (omnidiff-render-buffer diff)
              (goto-char (point-min))
              (let ((card-ov (omnidiff-image-card-ov-at))
                    (rerenders 0)
                    (orig-rerender (symbol-function 'omnidiff-image--rerender-card)))
                (cl-letf (((symbol-function 'omnidiff-image--magick-command)
                           (lambda () '("sh" "-c" "sleep 10" "_")))
                          ((symbol-function 'omnidiff-image--rerender-card)
                           (lambda (&rest args)
                             (setq rerenders (1+ rerenders))
                             (apply orig-rerender args))))
                  ;; Phase 1: cancel via `omnidiff-image--cancel-workers' directly (cache not cleared)
                  ;; to verify the sentinel writes 0 `failed' entries and 0 rerenders.
                  (let ((diff-key (list 'magick 'b 'a t "#ff0000" 0)))
                    (omnidiff-image--start-magick-diff
                     (omnidiff-image--card-key file) diff-key
                     omnidiff-test--png-red-2x2 'png
                     omnidiff-test--png-blue-2x2 'png "#ff0000" 0 card-ov)
                    (should (= (length omnidiff-image--workers) 1))
                    (let ((proc (caar omnidiff-image--workers))
                          (wdir (cdar omnidiff-image--workers)))
                      (should (process-live-p proc))
                      (should (file-directory-p wdir))
                      (omnidiff-image--cancel-workers)
                      (should (omnidiff-test--wait-until (lambda () (not (process-live-p proc))) 3.0))
                      (accept-process-output nil 0.05)
                      (should (= rerenders 0))
                      (should-not omnidiff-image--workers)
                      (should-not (file-directory-p wdir))
                      (let ((failed-count 0))
                        (maphash (lambda (_k v) (when (eq v 'failed) (setq failed-count (1+ failed-count))))
                                 omnidiff-image--magick-cache)
                        (should (= failed-count 0)))))
                  ;; Phase 2: full cancellation via `omnidiff-image-cleanup'.
                  (let ((diff-key (list 'magick 'b2 'a2 t "#ff0000" 0)))
                    (omnidiff-image--start-magick-diff
                     (omnidiff-image--card-key file) diff-key
                     omnidiff-test--png-red-2x2 'png
                     omnidiff-test--png-blue-2x2 'png "#ff0000" 0 card-ov)
                    (should (= (length omnidiff-image--workers) 1))
                    (let ((proc (caar omnidiff-image--workers))
                          (wdir (cdar omnidiff-image--workers)))
                      (should (process-live-p proc))
                      (should (file-directory-p wdir))
                      (omnidiff-image-cleanup)
                      (should (omnidiff-test--wait-until (lambda () (not (process-live-p proc))) 3.0))
                      (accept-process-output nil 0.05)
                      (should (= rerenders 0))
                      (should-not omnidiff-image--workers)
                      (should (= (hash-table-count omnidiff-image--magick-cache) 0))
                      (should-not (file-directory-p wdir)))))))))))))

(ert-deftest omnidiff-image-test/git-lfs-pointer-and-stderr-isolation ()
  "Detect LF/CRLF Git LFS pointers safely and isolate Git stderr noise."
  (let* ((lfs-lf (concat "version https://git-lfs.github.com/spec/v1\n"
                         "oid sha256:" (make-string 64 ?a) "\n"
                         "size 12345\n"))
         (lfs-crlf (concat "version https://git-lfs.github.com/spec/v1\r\n"
                           "oid sha256:" (make-string 64 ?b) "\r\n"
                           "size 67890\r\n")))
    ;; 1. LF and CRLF pointer detection + declared size parsing.
    (should (omnidiff-image-lfs-pointer-p lfs-lf))
    (should (omnidiff-image-lfs-pointer-p lfs-crlf))
    (should (= (omnidiff-image--lfs-pointer-declared-size lfs-lf) 12345))
    (should (= (omnidiff-image--lfs-pointer-declared-size lfs-crlf) 67890))
    (should-not (omnidiff-image-lfs-pointer-p omnidiff-test--png-red-2x2))
    (should-not (omnidiff-image-lfs-pointer-p (concat lfs-lf (make-string 600 ?x))))
    ;; 2. Unsmudged LFS bytes detect as `lfs-pointer' even when path ends in `.png'.
    (should (equal (omnidiff-image--detect-type lfs-lf "photo.png") '(lfs-pointer . nil)))
    (should (equal (omnidiff-image--detect-type lfs-crlf "photo.png") '(lfs-pointer . nil)))
    ;; 3. Rendering an unsmudged LFS card shows a clear LFS fallback notice.
    (omnidiff-test--with-temp-dir dir
      (omnidiff-test--with-buffer-cleanup
        (let* ((f-lfs-a (expand-file-name "before.png" dir))
               (f-lfs-b (expand-file-name "after.png" dir))
               (empty-bin (expand-file-name "empty-bin" dir))
               (buf (generate-new-buffer " *omnidiff-lfs-ui*")))
          (make-directory empty-bin t)
          (omnidiff-test--write-bin f-lfs-a lfs-lf)
          (omnidiff-test--write-bin f-lfs-b lfs-crlf)
          (let* ((pic (make-omnidiff-picture
                       :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 12345)
                       :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 67890)
                       :comparison 'unknown))
                 (file-both (omnidiff-image-make-file-from-paths f-lfs-a f-lfs-b pic "lfs.png"))
                 (diff (omnidiff-test--make-diff file-both)))
            (with-current-buffer buf
              (omnidiff-mode)
              (setq omnidiff--repo-dir dir
                    omnidiff--diff diff)
              (let ((exec-path (list empty-bin)))
                (omnidiff-test--with-gui-image
                  (omnidiff-render-buffer diff)
                  (should (string-search
                           "Git LFS image object is not materialized locally"
                           (buffer-substring-no-properties (point-min) (point-max)))))))))))
    ;; 4. Stderr isolation: noisy Git stderr never contaminates PNG bytes from
    ;;    `--read-git-blob' or `--smudge-disk-lfs-if-needed'.
    (omnidiff-test--with-temp-dir dir
      (let* ((fake-bin (expand-file-name "bin" dir))
             (fake-git (expand-file-name "git" fake-bin))
             (png-path (expand-file-name "sample.png" dir)))
        (make-directory fake-bin t)
        (omnidiff-test--write-bin png-path omnidiff-test--png-red-2x2)
        (with-temp-file fake-git
          (insert (format "#!/bin/sh\n\
echo 'warning: git stderr noise' >&2\n\
if [ \"$1\" = 'cat-file' ] && [ \"$2\" = '-s' ]; then\n\
  printf '%%d\\n' %d\n\
  exit 0\n\
fi\n\
cat %s\n"
                          (length omnidiff-test--png-red-2x2)
                          (shell-quote-argument png-path))))
        (set-file-modes fake-git #o755)
        (let ((exec-path (cons fake-bin exec-path)))
          (let ((blob (omnidiff-image--read-git-blob
                       (make-string 40 ?1) "sample.png" dir nil))
                (smudged (omnidiff-image--smudge-disk-lfs-if-needed lfs-crlf dir nil)))
            (should (equal blob omnidiff-test--png-red-2x2))
            (should (equal smudged omnidiff-test--png-red-2x2))))))))

(ert-deftest omnidiff-image-test/git-lfs-half-smudged-warning ()
  "Single-sided LFS fallback warns explicitly when BEFORE is unsmudged."
  (skip-unless (image-type-available-p 'png))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let* ((lfs-lf (concat "version https://git-lfs.github.com/spec/v1\n"
                             "oid sha256:" (make-string 64 ?a) "\n"
                             "size 12345\n"))
             (f-lfs (expand-file-name "before.png" dir))
             (f-real (expand-file-name "after.png" dir))
             (empty-bin (expand-file-name "empty-bin" dir))
             (buf (generate-new-buffer " *omnidiff-lfs-half*")))
        (make-directory empty-bin t)
        (omnidiff-test--write-bin f-lfs lfs-lf)
        (omnidiff-test--write-bin f-real omnidiff-test--png-blue-2x2)
        (let* ((pic (make-omnidiff-picture
                     :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 12345)
                     :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :comparison 'unknown))
               (file (omnidiff-image-make-file-from-paths f-lfs f-real pic "half.png"))
               (diff (omnidiff-test--make-diff file)))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff)
            (let ((exec-path (list empty-bin)))
              (omnidiff-test--with-gui-image
                (omnidiff-render-buffer diff)
                (should (string-search
                         "BEFORE is an unsmudged Git LFS pointer — showing AFTER only"
                         (buffer-substring-no-properties (point-min) (point-max))))))))))))

(ert-deftest omnidiff-image-test/parse-svg-dimensions-stroke-width ()
  "SVG dimension parser ignores `stroke-width' and resolves viewBox/attrs."
  (should (equal (omnidiff-image--parse-svg-dimensions
                  "<svg viewBox=\"0 0 24 24\" stroke-width=\"2\" height=\"24\"></svg>")
                 '(24.0 24.0 viewbox)))
  (should (equal (omnidiff-image--parse-svg-dimensions
                  "<svg xmlns=\"http://www.w3.org/2000/svg\" stroke-width=\"2\" width=\"48px\" height=\"32px\" viewBox=\"0 0 24 24\"></svg>")
                 '(48.0 32.0 intrinsic)))
  (should (equal (omnidiff-image--parse-svg-dimensions
                  "<svg\n  stroke-width=\"3\"\n  viewBox=\"0, 0, 120, 60\"></svg>")
                 '(120.0 60.0 viewbox))))

(ert-deftest omnidiff-image-test/tty-arrow-fallthrough ()
  "Non-GUI `<left>' and `<right>' fall through to character movement."
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (buf (generate-new-buffer " *omnidiff-tty-guards*")))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        (let* ((pic (make-omnidiff-picture
                     :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :comparison 'pixels))
               (file (omnidiff-image-make-file-from-paths fa fb pic "dual.png"))
               (diff (omnidiff-test--make-diff file)))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'unified)
            (omnidiff-render-buffer diff)
            (goto-char (point-min))
            (omnidiff-next-hunk)
            (let ((start (point)))
              (let ((last-command-event 'right))
                (omnidiff-image-adjust-increase))
              (should (= (point) (1+ start)))
              (let ((last-command-event 'left))
                (omnidiff-image-adjust-decrease))
              (should (= (point) start)))))))))

(ert-deftest omnidiff-image-test/gui-deferred-oversized-and-single-sided-guards ()
  "Single-sided arrows fall through; deferred/oversized require `L' lazily."
  (skip-unless (and (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (buf (generate-new-buffer " *omnidiff-gui-guards*")))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        (let* ((pic-dual (make-omnidiff-picture
                          :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                          :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                          :comparison 'pixels))
               (pic-add (make-omnidiff-picture
                         :before nil
                         :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                         :comparison 'one-sided))
               (f-dual (omnidiff-image-make-file-from-paths fa fb pic-dual "dual.png"))
               (f-add (omnidiff-image-make-file-from-paths nil fb pic-add "added.png"))
               (diff (omnidiff-test--make-diff (list f-dual f-add))))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'unified)
            (omnidiff-test--with-gui-image
              ;; 1. GUI single-sided `<right>'/`<left>' falls through; `h'/`l' errors.
              (omnidiff-render-buffer diff)
              (goto-char (point-min))
              (omnidiff-next-file)
              (omnidiff-next-hunk)
              (let ((p0 (point)))
                (let ((last-command-event 'right))
                  (omnidiff-image-adjust-increase))
                (should (= (point) (1+ p0)))
                (let ((last-command-event 'left))
                  (omnidiff-image-adjust-decrease))
                (should (= (point) p0)))
              (let ((last-command-event ?l))
                (should-error (omnidiff-image-adjust-increase) :type 'user-error))
              ;; 2. Deferred card signals `press L' without fetching bytes.
              (omnidiff-image-cleanup)
              (let ((omnidiff-image-auto-load-limit 1))
                (omnidiff-render-buffer diff)
                (goto-char (point-min))
                (omnidiff-next-file)
                (let ((fetched nil)
                      (orig-fetch (symbol-function 'omnidiff-image--fetch-side-bytes)))
                  (cl-letf (((symbol-function 'omnidiff-image--fetch-side-bytes)
                             (lambda (&rest args)
                               (setq fetched t)
                               (apply orig-fetch args))))
                    (let ((err (should-error (omnidiff-image-zoom-in) :type 'user-error)))
                      (should (string-search "press L" (error-message-string err))))
                    (let ((err (should-error (omnidiff-image-cycle-backdrop) :type 'user-error)))
                      (should (string-search "press L" (error-message-string err))))
                    (should-not fetched))))
              ;; 3. Oversized card signals `press L' without loading full bytes.
              (omnidiff-image-cleanup)
              (let ((omnidiff-image-max-bytes 10))
                (omnidiff-render-buffer diff)
                (goto-char (point-min))
                (let ((err (should-error (omnidiff-image-zoom-in) :type 'user-error)))
                  (should (string-search "press L" (error-message-string err))))))))))))

(ert-deftest omnidiff-image-test/resized-scaled-unified-region-boxes ()
  "`resized-scaled' maps regions, loupe, and swipe in `(min bw aw)' x `(min bh ah)'."
  (skip-unless (and (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (buf (generate-new-buffer " *omnidiff-scaled-reg*")))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        ;; BEFORE is 100x50, AFTER is 200x100 (same 2:1 aspect ratio -> `resized-scaled').
        ;; `omnidiff' downscales to min(bw,aw) x min(bh,ah) = 100x50, so a box at
        ;; (x=50, y=25, w=10, h=5) sits at center (50%..60%, 50%..60%) of both sides.
        (let* ((pic (make-omnidiff-picture
                     :before (make-omnidiff-picture-info :format "PNG" :width 100 :height 50 :bytes 73)
                     :after (make-omnidiff-picture-info :format "PNG" :width 200 :height 100 :bytes 73)
                     :comparison 'resized-scaled :changed-pct 1.0 :region-count 1
                     :regions (list (make-omnidiff-picture-region
                                     :x 50 :y 25 :width 10 :height 5))))
               (file (omnidiff-image-make-file-from-paths fa fb pic "scaled.png"))
               (diff (omnidiff-test--make-diff file))
               (rect-re "<rect x=\"\\([0-9.]+\\)\" y=\"\\([0-9.]+\\)\" width=\"\\([0-9.]+\\)\" height=\"\\([0-9.]+\\)\" fill=\"none\""))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'side-by-side)
            (omnidiff-test--with-gui-image
              (omnidiff-render-buffer diff)
              (goto-char (point-min))
              (omnidiff-image-toggle-regions)
              (let* ((card-key (omnidiff-image--card-key file))
                     (b-svg (plist-get (cdr (cdr (gethash (cons card-key 'view-before) omnidiff-image--slot-descs))) :data))
                     (a-svg (plist-get (cdr (cdr (gethash (cons card-key 'view-after) omnidiff-image--slot-descs))) :data))
                     (lb-svg (plist-get (cdr (cdr (gethash (cons card-key 'loupe-before) omnidiff-image--slot-descs))) :data))
                     (la-svg (plist-get (cdr (cdr (gethash (cons card-key 'loupe-after) omnidiff-image--slot-descs))) :data)))
                (should (stringp b-svg))
                (should (stringp a-svg))
                (should (stringp lb-svg))
                (should (stringp la-svg))
                ;; BEFORE (disp-w=100, disp-h=50): x=50/100 maps to x≈49..50, y≈21.5.
                (should (string-match rect-re b-svg))
                (let ((bx (string-to-number (match-string 1 b-svg)))
                      (by (string-to-number (match-string 2 b-svg))))
                  (should (<= 45.0 bx 55.0))
                  (should (<= 18.0 by 28.0)))
                ;; AFTER (disp-w=200, disp-h=100): x=50/100 maps to x≈100, y≈49 (NOT 50, 25!).
                (should (string-match rect-re a-svg))
                (let ((ax (string-to-number (match-string 1 a-svg)))
                      (ay (string-to-number (match-string 2 a-svg))))
                  (should (<= 95.0 ax 105.0))
                  (should (<= 44.0 ay 54.0)))
                ;; Loupe crops embed both sides onto the 100x50 min grid, not 200x100.
                (should (string-search "width=\"100\" height=\"50\"" lb-svg))
                (should (string-search "width=\"100\" height=\"50\"" la-svg))
                ;; Swipe mode: display canvas stays 200x100, auto-aim aims at 55% (55/100),
                ;; and region boxes sit at x≈100, y≈49 on the 200x100 canvas.
                (omnidiff-image-mode-swipe)
                (should (string-search "3:swipe 55%" (buffer-string)))
                (let ((sb-svg (plist-get (cdr (cdr (gethash (cons card-key 'view-before) omnidiff-image--slot-descs))) :data)))
                  (should (string-search "width=\"200\" height=\"100\"" sb-svg))
                  (should (string-match rect-re sb-svg))
                  (should (<= 95.0 (string-to-number (match-string 1 sb-svg)) 105.0))
                  (should (<= 44.0 (string-to-number (match-string 2 sb-svg)) 54.0)))
                ;; Onion and delta diff modes also map regions onto the 200x100 canvas via 100x50 grid.
                (omnidiff-image-mode-onion)
                (let ((onion-svg (plist-get (cdr (cdr (gethash (cons card-key 'onion) omnidiff-image--slot-descs))) :data)))
                  (should (string-search "width=\"200\" height=\"100\"" onion-svg))
                  (should (string-match rect-re onion-svg))
                  (should (<= 95.0 (string-to-number (match-string 1 onion-svg)) 105.0))
                  (should (<= 44.0 (string-to-number (match-string 2 onion-svg)) 54.0)))
                (omnidiff-image-mode-diff)
                (let ((diff-svg (plist-get (cdr (cdr (gethash (cons card-key 'svg-diff) omnidiff-image--slot-descs))) :data)))
                  (should (string-search "width=\"200\" height=\"100\"" diff-svg))
                  (should (string-match rect-re diff-svg))
                  (should (<= 95.0 (string-to-number (match-string 1 diff-svg)) 105.0))
                  (should (<= 44.0 (string-to-number (match-string 2 diff-svg)) 54.0)))))))))))

(ert-deftest omnidiff-image-test/resized-scaled-live-omnidiff-integration ()
  "Live `omnidiff-engine-run-pair' `scaled.regions' align on rendered `resized-scaled' cards."
  (skip-unless (and (executable-find "omnidiff")
                    (executable-find "magick")
                    (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (buf (generate-new-buffer " *omnidiff-scaled-live*")))
        ;; 100x50 white BEFORE vs 200x100 white AFTER with a 20x10 red block at (100,50)..(119,59).
        (should (zerop (call-process "magick" nil nil nil
                                     "-size" "100x50" "xc:white" (concat "PNG24:" fa))))
        (should (zerop (call-process "magick" nil nil nil
                                     "-size" "200x100" "xc:white"
                                     "-fill" "red" "-draw" "rectangle 100,50 119,59"
                                     (concat "PNG24:" fb))))
        (let* ((jd (omnidiff-engine-run-pair fa fb))
               (pic (omnidiff-json-diff-picture jd))
               (regions (and pic (omnidiff-picture-regions pic)))
               (r (car regions))
               (file (omnidiff-image-make-file-from-paths fa fb pic "scaled-live.png"))
               (diff (omnidiff-test--make-diff file))
               (rect-re "<rect x=\"\\([0-9.]+\\)\" y=\"\\([0-9.]+\\)\" width=\"\\([0-9.]+\\)\" height=\"\\([0-9.]+\\)\" fill=\"none\""))
          (should pic)
          (should (eq (omnidiff-picture-comparison pic) 'resized-scaled))
          (should (= (omnidiff-picture-info-width (omnidiff-picture-before pic)) 100))
          (should (= (omnidiff-picture-info-height (omnidiff-picture-before pic)) 50))
          (should (= (omnidiff-picture-info-width (omnidiff-picture-after pic)) 200))
          (should (= (omnidiff-picture-info-height (omnidiff-picture-after pic)) 100))
          (should r)
          (should (= (omnidiff-picture-region-x r) 50))
          (should (= (omnidiff-picture-region-y r) 25))
          (should (= (omnidiff-picture-region-width r) 10))
          (should (= (omnidiff-picture-region-height r) 5))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'side-by-side)
            (omnidiff-test--with-gui-image
              (omnidiff-render-buffer diff)
              (goto-char (point-min))
              (omnidiff-image-toggle-regions)
              (let* ((card-key (omnidiff-image--card-key file))
                     (b-svg (plist-get (cdr (cdr (gethash (cons card-key 'view-before) omnidiff-image--slot-descs))) :data))
                     (a-svg (plist-get (cdr (cdr (gethash (cons card-key 'view-after) omnidiff-image--slot-descs))) :data))
                     (lb-svg (plist-get (cdr (cdr (gethash (cons card-key 'loupe-before) omnidiff-image--slot-descs))) :data)))
                (should (string-match rect-re b-svg))
                (should (<= 45.0 (string-to-number (match-string 1 b-svg)) 55.0))
                (should (<= 18.0 (string-to-number (match-string 2 b-svg)) 28.0))
                (should (string-match rect-re a-svg))
                (should (<= 95.0 (string-to-number (match-string 1 a-svg)) 105.0))
                (should (<= 44.0 (string-to-number (match-string 2 a-svg)) 54.0))
                (should (string-search "width=\"100\" height=\"50\"" lb-svg)))
              (omnidiff-image-mode-swipe)
              (should (string-search "3:swipe 55%" (buffer-string))))))))))

(ert-deftest omnidiff-image-test/zoomed-side-by-side-header-alignment ()
  "Zoomed `1:2-up' aligns the `AFTER' header with zoomed `BEFORE' width."
  (skip-unless (and (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (buf (generate-new-buffer " *omnidiff-zoom-sbs*")))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        (let* ((pic (make-omnidiff-picture
                     :before (make-omnidiff-picture-info :format "PNG" :width 100 :height 100 :bytes 73)
                     :after (make-omnidiff-picture-info :format "PNG" :width 100 :height 100 :bytes 73)
                     :comparison 'pixels))
               (file (omnidiff-image-make-file-from-paths fa fb pic "zoom.png"))
               (diff (omnidiff-test--make-diff file)))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'side-by-side)
            (let ((omnidiff-test--gui-viewport-px '(1000 . 600)))
              (omnidiff-test--with-gui-image
                (omnidiff-render-buffer diff)
                ;; Zoom 100px image to 8x = 800px (> pane-w ~484px).
                (setf (omnidiff-image--state-zoom (omnidiff-image--state-for file)) 8.0)
                (goto-char (point-min))
                (omnidiff-image--rerender-card (omnidiff-image-card-ov-at) t)
                (let* ((align-cols
                        (delq nil
                              (mapcar (lambda (d)
                                        (when (and (consp d) (eq (car d) 'space)
                                                   (consp (plist-get (cdr d) :align-to))
                                                   (integerp (car (plist-get (cdr d) :align-to))))
                                          (car (plist-get (cdr d) :align-to))))
                                      (omnidiff-test--collect-display-props)))))
                  ;; Both header row and image row must share the exact same align-to >= 800px.
                  (should (= (length align-cols) 2))
                  (should (= (nth 0 align-cols) (nth 1 align-cols)))
                  (should (>= (nth 0 align-cols) 800)))))))))))

(ert-deftest omnidiff-image-test/folded-svg-unfolds-file-and-hunk-overlays ()
  "User action on folded `.svg' unfolds both `file-ov' and `hunk-ov'."
  (skip-unless (and (executable-find "git")
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (with-temp-file (expand-file-name "icon.svg" repo)
        (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\"><rect width=\"24\" height=\"24\" fill=\"red\"/></svg>\n"))
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "v1")
      (let ((oid-old (omnidiff-test--git repo "rev-parse" "HEAD:icon.svg")))
        (with-temp-file (expand-file-name "icon.svg" repo)
          (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"24\" height=\"24\"><rect width=\"24\" height=\"24\" fill=\"blue\"/></svg>\n"))
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "v2")
        (let* ((oid-new (omnidiff-test--git repo "rev-parse" "HEAD:icon.svg"))
               (file (omnidiff-test--make-json-file
                      "icon.svg" "fill=\"red\"\n" "fill=\"blue\"\n"
                      :old-oid oid-old :new-oid oid-new
                      :before-hunks (list (omnidiff-test--make-hunk 'update 0 6 0 9))
                      :after-hunks (list (omnidiff-test--make-hunk 'update 0 6 0 10))))
               (diff (omnidiff-test--make-diff file))
               (buf (generate-new-buffer " *omnidiff-svg-fold*")))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir repo
                  omnidiff--diff diff
                  omnidiff--layout 'unified)
            (omnidiff-test--with-gui-image
              (omnidiff-render-buffer diff)
              ;; Locate file-ov, inner SVG preview hunk-ov, and card-ov.
              (goto-char (point-min))
              (let* ((file-ov (get-text-property (point) 'omnidiff-body-ov))
                     (hunk-ov (progn
                                (search-forward "SVG preview")
                                (get-text-property (line-beginning-position) 'omnidiff-hunk-ov)))
                     (card-ov (progn
                                (forward-line 1)
                                (omnidiff-image-card-ov-at))))
                (should file-ov)
                (should hunk-ov)
                (should card-ov)
                ;; Fold inner hunk-ov first, then outer file-ov.
                (omnidiff--set-overlay-folded hunk-ov t 'omnidiff-hunk-fold 'omnidiff-hunk-accent)
                (omnidiff--set-overlay-folded file-ov t 'omnidiff-fold 'omnidiff-file-accent)
                (should (eq (overlay-get hunk-ov 'invisible) 'omnidiff-hunk-fold))
                (should (eq (overlay-get file-ov 'invisible) 'omnidiff-fold))
                ;; Background rerender leaves BOTH folded.
                (omnidiff-image--rerender-card card-ov nil)
                (should (eq (overlay-get hunk-ov 'invisible) 'omnidiff-hunk-fold))
                (should (eq (overlay-get file-ov 'invisible) 'omnidiff-fold))
                ;; User action unfolds BOTH `file-ov' and `hunk-ov' and clears fold markers.
                (omnidiff-image--rerender-card card-ov t)
                (should-not (overlay-get file-ov 'invisible))
                (should-not (overlay-get hunk-ov 'invisible))
                (should-not (overlay-get (overlay-get file-ov 'omnidiff-fold-ov) 'display))
                (should-not (overlay-get (overlay-get hunk-ov 'omnidiff-fold-ov) 'display))))))))))

(ert-deftest omnidiff-image-test/cycle-backdrop-honors-custom-default ()
  "`omnidiff-image-cycle-backdrop' honors custom `omnidiff-image-backdrop'."
  (skip-unless (and (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (buf (generate-new-buffer " *omnidiff-bd-custom*")))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        (let* ((pic (make-omnidiff-picture
                     :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :comparison 'pixels))
               (file (omnidiff-image-make-file-from-paths fa fb pic "bd.png"))
               (diff (omnidiff-test--make-diff file)))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'unified)
            (omnidiff-test--with-gui-image
              (let ((omnidiff-image-backdrop 'light))
                (omnidiff-render-buffer diff)
                (goto-char (point-min))
                (let ((st (omnidiff-image--state-for file)))
                  (should-not (omnidiff-image--state-backdrop st))
                  ;; Cycling from custom `light' must go directly to `dark', not `checker'.
                  (omnidiff-image-cycle-backdrop)
                  (should (eq (omnidiff-image--state-backdrop st) 'dark))
                  (omnidiff-image-cycle-backdrop)
                  (should (eq (omnidiff-image--state-backdrop st) 'none))
                  (omnidiff-image-cycle-backdrop)
                  (should (eq (omnidiff-image--state-backdrop st) 'auto))
                  (omnidiff-image-cycle-backdrop)
                  (should (eq (omnidiff-image--state-backdrop st) 'checker)))))))))))

(ert-deftest omnidiff-image-test/multi-commit-same-path-isolation-and-bounded-slots ()
  "Same-path multi-commit cards isolate state and bound descriptor slots."
  (skip-unless (and (executable-find "git")
                    (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-git-repo repo
    (omnidiff-test--with-buffer-cleanup
      (omnidiff-test--write-bin (expand-file-name "logo.png" repo) omnidiff-test--png-red-2x2)
      (omnidiff-test--git repo "add" ".")
      (omnidiff-test--git repo "commit" "-q" "-m" "c1")
      (let ((oid1 (omnidiff-test--git repo "rev-parse" "HEAD:logo.png")))
        (omnidiff-test--write-bin (expand-file-name "logo.png" repo) omnidiff-test--png-blue-2x2)
        (omnidiff-test--git repo "add" ".")
        (omnidiff-test--git repo "commit" "-q" "-m" "c2")
        (let ((oid2 (omnidiff-test--git repo "rev-parse" "HEAD:logo.png")))
          (omnidiff-test--write-bin (expand-file-name "logo.png" repo) omnidiff-test--png-rgba-alpha-4x4)
          (omnidiff-test--git repo "add" ".")
          (omnidiff-test--git repo "commit" "-q" "-m" "c3")
          (let* ((oid3 (omnidiff-test--git repo "rev-parse" "HEAD:logo.png"))
                 (f1 (omnidiff-test--make-file
                      :path "logo.png" :target-path "logo.png" :status 'modified
                      :old-oid oid1 :new-oid oid2
                      :picture (make-omnidiff-picture
                                :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                                :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                                :comparison 'pixels)))
                 (f2 (omnidiff-test--make-file
                      :path "logo.png" :target-path "logo.png" :status 'modified
                      :preamble "commit c3\n"
                      :old-oid oid2 :new-oid oid3
                      :picture (make-omnidiff-picture
                                :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                                :after (make-omnidiff-picture-info :format "PNG" :width 4 :height 4 :bytes 75)
                                :comparison 'resized-scaled)))
                 (diff (omnidiff-test--make-diff (list f1 f2)))
                 (buf (generate-new-buffer " *omnidiff-same-path-img*")))
            (should-not (equal (omnidiff-image--card-key f1)
                               (omnidiff-image--card-key f2)))
            (with-current-buffer buf
              (omnidiff-mode)
              (setq omnidiff--repo-dir repo
                    omnidiff--diff diff
                    omnidiff--layout 'unified)
              (omnidiff-test--with-gui-image
                (omnidiff-render-buffer diff)
                ;; Mutate card 1 -> card 2 stays completely isolated.
                (goto-char (point-min))
                (omnidiff-image-mode-onion)
                (omnidiff-image-zoom-in)
                (should (eq (omnidiff-image--state-mode (omnidiff-image--state-for f1)) 'onion))
                (should (omnidiff-image--state-zoom (omnidiff-image--state-for f1)))
                (should-not (omnidiff-image--state-mode (omnidiff-image--state-for f2)))
                (should-not (omnidiff-image--state-zoom (omnidiff-image--state-for f2)))
                ;; Repeated opacity adjustments replace the single onion slot in place without leaking.
                (let* ((k1 (omnidiff-image--card-key f1))
                       (slot-count-before (hash-table-count omnidiff-image--slot-descs))
                       (prev-img (cdr (gethash (cons k1 'onion) omnidiff-image--slot-descs)))
                       (replacements 0))
                  (should prev-img)
                  (dotimes (_ 4)
                    (omnidiff-image-adjust-increase)
                    (let ((cur-img (cdr (gethash (cons k1 'onion) omnidiff-image--slot-descs))))
                      (should cur-img)
                      (unless (eq cur-img prev-img)
                        (setq replacements (1+ replacements)
                              prev-img cur-img))))
                  (should (= (hash-table-count omnidiff-image--slot-descs) slot-count-before))
                  (should (= replacements 4)))))))))))

(ert-deftest omnidiff-image-test/magick-cache-bounded-eviction ()
  "Bounded `--magick-cache' evicts oldest beyond 2 entries per card."
  (with-temp-buffer
    (omnidiff-mode)
    (omnidiff-image--ensure-caches)
    (let ((card-a '("a.png" "1" "2"))
          (card-b '("b.png" "3" "4")))
      (omnidiff-image--put-magick-cache card-a 'k1 "v1")
      (omnidiff-image--put-magick-cache card-a 'k2 "v2")
      (omnidiff-image--put-magick-cache card-b 'kb "vb")
      (should (equal (gethash 'k1 omnidiff-image--magick-cache) "v1"))
      (should (equal (gethash 'k2 omnidiff-image--magick-cache) "v2"))
      (should (equal (gethash 'kb omnidiff-image--magick-cache) "vb"))
      ;; 3rd entry for card-a evicts oldest (`k1'), keeping `k2', `k3', and `kb'.
      (omnidiff-image--put-magick-cache card-a 'k3 "v3")
      (should-not (gethash 'k1 omnidiff-image--magick-cache))
      (should (equal (gethash 'k2 omnidiff-image--magick-cache) "v2"))
      (should (equal (gethash 'k3 omnidiff-image--magick-cache) "v3"))
      (should (equal (gethash 'kb omnidiff-image--magick-cache) "vb"))
      ;; Re-putting `k2' refreshes its MRU position so `k4' evicts `k3' instead of `k2'.
      (omnidiff-image--put-magick-cache card-a 'k2 "v2-refreshed")
      (omnidiff-image--put-magick-cache card-a 'k4 "v4")
      (should-not (gethash 'k3 omnidiff-image--magick-cache))
      (should (equal (gethash 'k2 omnidiff-image--magick-cache) "v2-refreshed"))
      (should (equal (gethash 'k4 omnidiff-image--magick-cache) "v4"))
      (should (equal (gethash 'kb omnidiff-image--magick-cache) "vb")))))

(ert-deftest omnidiff-image-test/cross-window-swipe-frame-x ()
  "`omnidiff-image--posn-frame-x' handles split windows, fringes, and frames."
  (let* ((frame (selected-frame))
         (orig-w (frame-width frame))
         (w1 (selected-window))
         (w2 nil))
    (unwind-protect
        (progn
          (set-frame-width frame 120)
          (setq w2 (split-window-right))
          (let* ((w1-in (window-inside-pixel-edges w1))
                 (w2-in (window-inside-pixel-edges w2))
                 (w2-out (window-pixel-edges w2))
                 (p-w1 (list w1 1 '(12 . 4)))
                 (p-w2 (list w2 1 '(15 . 4)))
                 (p-rf (list w1 'right-fringe '(3 . 4)))
                 (p-lf (list w2 'left-fringe '(2 . 4)))
                 (p-fr (list frame nil '(77 . 4))))
            (should (> (car w2-in) (car w1-in)))
            ;; Text-area posn on w1 vs w2 (same frame) adds each window's inside left edge.
            (should (= (omnidiff-image--posn-frame-x p-w1 w1) (+ (car w1-in) 12)))
            (should (= (omnidiff-image--posn-frame-x p-w2 w1) (+ (car w2-in) 15)))
            ;; Cross-window delta tracks real frame-pixel displacement across the split.
            (should (= (- (omnidiff-image--posn-frame-x p-w2 w1)
                          (omnidiff-image--posn-frame-x p-w1 w1))
                       (- (+ (car w2-in) 15) (+ (car w1-in) 12))))
            ;; Right fringe uses inside-right edge; left fringe uses outer-left edge.
            (should (= (omnidiff-image--posn-frame-x p-rf w1) (+ (nth 2 w1-in) 3)))
            (should (= (omnidiff-image--posn-frame-x p-lf w1) (+ (car w2-out) 2)))
            ;; Frame-level posn returns raw X; nil posn returns nil.
            (should (= (omnidiff-image--posn-frame-x p-fr w1) 77))
            (should-not (omnidiff-image--posn-frame-x nil w1))))
      (when (window-live-p w2) (delete-window w2))
      (set-frame-width frame orig-w))))

(ert-deftest omnidiff-image-test/background-buffer-theme-change-reflow ()
  "`omnidiff-image-on-theme-change' reflows off-screen non-deferred cards."
  (skip-unless (and (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (bg-buf (generate-new-buffer " *omnidiff-bg-theme*"))
            (other-buf (generate-new-buffer " *omnidiff-fg-other*")))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        (let* ((pic (make-omnidiff-picture
                     :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :comparison 'pixels))
               (f1 (omnidiff-image-make-file-from-paths fa fb pic "c1.png"))
               (f2 (omnidiff-image-make-file-from-paths fa fb pic "c2.png"))
               (diff (omnidiff-test--make-diff (list f1 f2))))
          (set-window-buffer (selected-window) other-buf)
          (with-current-buffer bg-buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'unified)
            (omnidiff-test--with-gui-image
              (let ((omnidiff-image-auto-load-limit 1))
                (omnidiff-render-buffer diff))
              ;; Confirm bg-buf is genuinely off-screen (not displayed in any window).
              (should-not (get-buffer-window-list bg-buf nil t))
              (let* ((cards (cl-remove-if-not
                             (lambda (ov) (overlay-get ov 'omnidiff-image-ov))
                             (overlays-in (point-min) (point-max))))
                     (old-rev (omnidiff-theme-revision))
                     (new-rev (+ old-rev 7))
                     (rerendered nil)
                     (orig-rerender (symbol-function 'omnidiff-image--rerender-card)))
                (should (= (length cards) 2))
                (cl-letf (((symbol-function 'omnidiff-theme-revision) (lambda () new-rev))
                          ((symbol-function 'omnidiff-image--rerender-card)
                           (lambda (ov &optional user-p)
                             (push (omnidiff-file-target-path
                                    (overlay-get ov 'omnidiff-image-file))
                                   rerendered)
                             (funcall orig-rerender ov user-p))))
                  (omnidiff-image-on-theme-change))
                ;; Only non-deferred c1.png was reflowed; deferred c2.png was skipped.
                (should (equal rerendered '("c1.png")))
                (let ((c1-ov (cl-find "c1.png" cards
                                      :key (lambda (ov)
                                             (omnidiff-file-target-path
                                              (overlay-get ov 'omnidiff-image-file)))
                                      :test #'equal)))
                  (should (eql (overlay-get c1-ov 'omnidiff-image-theme-rev) new-rev)))))))))))

(ert-deftest omnidiff-image-test/svg-delta-desc-cache-hit ()
  "`omnidiff-image--build-svg-delta-desc' hits cache without rebuilding SVG."
  (skip-unless (image-type-available-p 'svg))
  (with-temp-buffer
    (omnidiff-mode)
    (omnidiff-image--ensure-caches)
    (let ((builds 0)
          (orig-build (symbol-function 'omnidiff-image--build-svg-diff)))
      (cl-letf (((symbol-function 'omnidiff-image--build-svg-diff)
                 (lambda (&rest args)
                   (setq builds (1+ builds))
                   (apply orig-build args))))
        (let* ((d1 (omnidiff-image--build-svg-delta-desc
                    'card1 'b omnidiff-test--png-red-2x2 "image/png"
                    'a omnidiff-test--png-blue-2x2 "image/png"
                    64 64 4.0 nil 0 2 2 "#ff0000"))
               (d2 (omnidiff-image--build-svg-delta-desc
                    'card1 'b omnidiff-test--png-red-2x2 "image/png"
                    'a omnidiff-test--png-blue-2x2 "image/png"
                    64 64 4.0 nil 0 2 2 "#ff0000"))
               (d3 (omnidiff-image--build-svg-delta-desc
                    'card1 'b omnidiff-test--png-red-2x2 "image/png"
                    'a omnidiff-test--png-blue-2x2 "image/png"
                    64 64 8.0 nil 0 2 2 "#ff0000"))
               (d4 (omnidiff-image--build-svg-delta-desc
                    'card1 'b omnidiff-test--png-red-2x2 "image/png"
                    'a omnidiff-test--png-blue-2x2 "image/png"
                    64 64 8.0 nil 0 2 2 "#ff0000")))
          (should d1)
          (should (eq d1 d2))
          (should d3)
          (should-not (eq d1 d3))
          (should (eq d3 d4))
          (should (= builds 2)))))))

(ert-deftest omnidiff-image-test/cached-slot-desc-preserves-shared-equal-descriptor ()
  "Zooming Card 1 does not `image-flush' descriptors `equal' to Card 2's."
  (skip-unless (and (image-type-available-p 'png)
                    (image-type-available-p 'svg)))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((fa (expand-file-name "before.png" dir))
            (fb (expand-file-name "after.png" dir))
            (buf (generate-new-buffer " *omnidiff-shared-slot*")))
        (omnidiff-test--write-bin fa omnidiff-test--png-red-2x2)
        (omnidiff-test--write-bin fb omnidiff-test--png-blue-2x2)
        (let* ((pic (make-omnidiff-picture
                     :before (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :after (make-omnidiff-picture-info :format "PNG" :width 2 :height 2 :bytes 73)
                     :comparison 'pixels))
               (f1 (omnidiff-image-make-file-from-paths fa fb pic "card1.png"))
               (f2 (omnidiff-image-make-file-from-paths fa fb pic "card2.png"))
               (diff (omnidiff-test--make-diff (list f1 f2))))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'side-by-side)
            (let ((omnidiff-image-backdrop 'checker))
              (omnidiff-test--with-gui-image
                (omnidiff-render-buffer diff)
                (let* ((k1 (omnidiff-image--card-key f1))
                       (k2 (omnidiff-image--card-key f2))
                       (c1-b (cdr (gethash (cons k1 'view-before) omnidiff-image--slot-descs)))
                       (c2-b (cdr (gethash (cons k2 'view-before) omnidiff-image--slot-descs)))
                       (c1-a (cdr (gethash (cons k1 'view-after) omnidiff-image--slot-descs)))
                       (c2-a (cdr (gethash (cons k2 'view-after) omnidiff-image--slot-descs)))
                       (flushed nil)
                       (orig-flush (when (fboundp 'image-flush)
                                     (symbol-function 'image-flush))))
                  (should c1-b)
                  (should c2-b)
                  (should (equal c1-b c2-b))
                  (should-not (eq c1-b c2-b))
                  (should (equal c1-a c2-a))
                  (dlet ((native-comp-enable-subr-trampolines nil))
                    (cl-letf (((symbol-function 'image-flush)
                               (lambda (spec &rest args)
                                 (push spec flushed)
                                 (when orig-flush (apply orig-flush spec args)))))
                      ;; Zooming Card 1 replaces its descriptors, but Card 2 still holds `equal' ones.
                      (goto-char (point-min))
                      (omnidiff-image-zoom-in)
                      (let ((c1-b-zoom1 (cdr (gethash (cons k1 'view-before) omnidiff-image--slot-descs)))
                            (c1-a-zoom1 (cdr (gethash (cons k1 'view-after) omnidiff-image--slot-descs))))
                        (should c1-b-zoom1)
                        (should-not (equal c1-b-zoom1 c2-b))
                        (should-not (cl-member c2-b flushed :test #'equal))
                        (should-not (cl-member c2-a flushed :test #'equal))
                        ;; Zooming Card 1 a second time replaces now-unique descriptors -> MUST flush!
                        (setq flushed nil)
                        (omnidiff-image-zoom-in)
                        (should (cl-member c1-b-zoom1 flushed :test #'equal))
                        (should (cl-member c1-a-zoom1 flushed :test #'equal)))
                      ;; Also verify `--img-cache' references protect replaced slot descriptors.
                      (setq flushed nil)
                      (let ((shared-img (copy-tree c2-b)))
                        (puthash 'shared-ref shared-img omnidiff-image--img-cache)
                        (omnidiff-image--cached-slot-desc 'card-x 'slot-x 'k-a (lambda () (copy-tree shared-img)))
                        (omnidiff-image--cached-slot-desc 'card-x 'slot-x 'k-b (lambda () '(image :type svg :data "other")))
                        (should-not (cl-member shared-img flushed :test #'equal))))))))))))))

(ert-deftest omnidiff-image-test/single-sided-svg-zoom-out ()
  "Single-sided 200x200 SVG `omnidiff-image-zoom-out' zooms to 0.75, not 8.0."
  (skip-unless (image-type-available-p 'svg))
  (omnidiff-test--with-temp-dir dir
    (omnidiff-test--with-buffer-cleanup
      (let ((svg-path (expand-file-name "badge.svg" dir))
            (buf (generate-new-buffer " *omnidiff-svg-zoom-out*")))
        (with-temp-file svg-path
          (insert "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"200\" height=\"200\" viewBox=\"0 0 200 200\"><rect width=\"200\" height=\"200\" fill=\"green\"/></svg>\n"))
        (let* ((added-file (omnidiff-image-make-file-from-paths nil svg-path nil "added.svg"))
               (deleted-file (omnidiff-image-make-file-from-paths svg-path nil nil "deleted.svg"))
               (diff (omnidiff-test--make-diff (list added-file deleted-file))))
          (with-current-buffer buf
            (omnidiff-mode)
            (setq omnidiff--repo-dir dir
                  omnidiff--diff diff
                  omnidiff--layout 'unified)
            ;; In a 400x200 viewport, a 200x200 SVG fits naturally at 1.0 (100%).
            (let ((omnidiff-test--gui-viewport-px '(400 . 200)))
              (omnidiff-test--with-gui-image
                (omnidiff-render-buffer diff)
                (let ((cards (sort (cl-remove-if-not
                                    (lambda (ov) (overlay-get ov 'omnidiff-image-ov))
                                    (overlays-in (point-min) (point-max)))
                                   (lambda (a b) (< (overlay-start a) (overlay-start b))))))
                  (should (= (length cards) 2))
                  ;; Added SVG (`after-only'): dims resolved to 200x200, zoom-out goes 1.0 -> 0.75.
                  (let* ((ov-add (nth 0 cards))
                         (caps-add (omnidiff-image--card-capabilities ov-add)))
                    (should (eq (plist-get caps-add :status) 'after-only))
                    (should (= (plist-get caps-add :aw) 200.0))
                    (should (= (plist-get caps-add :ah) 200.0))
                    (should-not (plist-get caps-add :modes))
                    (goto-char (overlay-start ov-add))
                    (omnidiff-image-zoom-out)
                    (should (= (omnidiff-image--state-zoom (omnidiff-image--state-for added-file))
                               0.75)))
                  ;; Deleted SVG (`before-only'): dims resolved to 200x200, dual-only fields nil, zoom-out -> 0.75.
                  (let* ((ov-del (nth 1 cards))
                         (caps-del (omnidiff-image--card-capabilities ov-del)))
                    (should (eq (plist-get caps-del :status) 'before-only))
                    (should (= (plist-get caps-del :bw) 200.0))
                    (should (= (plist-get caps-del :bh) 200.0))
                    (should-not (plist-get caps-del :compat))
                    (should-not (plist-get caps-del :magick))
                    (should-not (plist-get caps-del :modes))
                    (goto-char (overlay-start ov-del))
                    (omnidiff-image-zoom-out)
                    (should (= (omnidiff-image--state-zoom (omnidiff-image--state-for deleted-file))
                               0.75))))))))))))

(provide 'omnidiff-image-test)
;;; omnidiff-image-test.el ends here
