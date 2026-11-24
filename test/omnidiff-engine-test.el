;;; omnidiff-engine-test.el --- ERT tests for omnidiff-engine -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; This file is part of omnidiff.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Unit, integration, and regression tests for `omnidiff-engine': JSON diff,
;; picture, and recursive container parsing; UTF-8 byte/char mapping and EOF
;; spans; hunk/counterpart/alignment/change-stop algorithms; path, byte-size,
;; and Git helpers; and sync/async CLI execution.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'omnidiff-engine)
(require 'omnidiff-test-helper)

;;; Shared path, range, formatting, file-accessor, and Git primitives

(ert-deftest omnidiff-engine-test/expand-repo-path-tilde-safety ()
  "`omnidiff-engine-expand-repo-path' never expands leading `~' as HOME."
  (omnidiff-test--with-temp-dir repo-dir
    (let ((root-prefix (file-name-as-directory (expand-file-name repo-dir))))
      (should (string-prefix-p
               root-prefix
               (omnidiff-engine-expand-repo-path "~/local.el" repo-dir)))
      (should (string-prefix-p
               root-prefix
               (omnidiff-engine-expand-repo-path "~root/file.el" repo-dir)))
      (should (equal (omnidiff-engine-expand-repo-path "src/a.el" repo-dir)
                     (expand-file-name "src/a.el" repo-dir)))
      (should (equal (omnidiff-engine-expand-repo-path "/tmp/abs.el" repo-dir)
                     "/tmp/abs.el")))))

(ert-deftest omnidiff-engine-test/sanitize-filename-hint-and-temp-containment ()
  "Filename sanitization and `write-temp-for-buffer' contain tricky hints."
  (omnidiff-test--with-temp-dir dir
    (let ((src-buf (generate-new-buffer " *omnidiff-san-buf*"))
          (tmp-sub (expand-file-name "san-tmp" dir))
          (extra-dirs nil))
      (unwind-protect
          (progn
            (with-current-buffer src-buf
              (insert "payload = 1\n"))
            (dolist (hint '("~" "dir/~" "~root" "a\\b" ".." "foo\x00bar.py"))
              (let* ((clean (omnidiff-engine-sanitize-filename-hint hint))
                     (dest (omnidiff-engine-write-temp-for-buffer
                            src-buf tmp-sub hint)))
                (should-not (string-empty-p clean))
                (should-not (string-prefix-p "~" clean))
                (should-not (member clean '("." "..")))
                (should-not (string-search "/" clean))
                (should-not (string-search "\\" clean))
                (should-not (string-search "\x00" clean))
                (should (file-exists-p dest))
                (should (file-in-directory-p dest tmp-sub))
                (should-not (string-prefix-p "~" (file-name-nondirectory dest)))
                (should (equal (omnidiff-engine-read-file-decoded dest)
                               "payload = 1\n"))))
            ;; Uniquify `<N>' suffix stripping and custom fallback.
            (should (equal (omnidiff-engine-sanitize-filename-hint "main.py<2>")
                           "main.py"))
            (should (equal (omnidiff-engine-sanitize-filename-hint ".." "fallback")
                           "fallback"))
            (should (equal (omnidiff-engine-sanitize-filename-hint nil "fallback")
                           "fallback"))
            ;; `tmpdir = nil' still contains `~root' without HOME expansion.
            (let ((nil-dest (omnidiff-engine-write-temp-for-buffer
                             src-buf nil "~root")))
              (push (directory-file-name (file-name-directory nil-dest))
                    extra-dirs)
              (should (file-exists-p nil-dest))
              (should (file-in-directory-p nil-dest temporary-file-directory))
              (should-not (string-prefix-p
                           "~" (file-name-nondirectory nil-dest)))))
        (when (buffer-live-p src-buf)
          (kill-buffer src-buf))
        (dolist (d extra-dirs)
          (when (file-directory-p d)
            (ignore-errors (delete-directory d t))))))))

(ert-deftest omnidiff-engine-test/range-last-row-and-contains-p ()
  "`range-last-row' guards negative/empty ranges and `range-contains-p' is right-open."
  ;; Empty, inverted, nil, and negative start-row ranges return nil.
  (should-not (omnidiff-engine-range-last-row nil))
  (should-not (omnidiff-engine-range-last-row
               (make-omnidiff-json-range :start-row 2 :start-col 5
                                         :end-row 2 :end-col 5)))
  (should-not (omnidiff-engine-range-last-row
               (make-omnidiff-json-range :start-row 5 :start-col 0
                                         :end-row 3 :end-col 0)))
  (should-not (omnidiff-engine-range-last-row
               (make-omnidiff-json-range :start-row -1 :start-col 0
                                         :end-row 2 :end-col 0)))
  (should-not (omnidiff-engine-range-last-row
               (make-omnidiff-json-range :start-row -2 :start-col 0
                                         :end-row 4 :end-col 3)
               10))
  ;; Non-empty single-line, multi-line (ec=0 vs ec>0), and num-rows clamping.
  (let ((single (make-omnidiff-json-range :start-row 2 :start-col 3
                                          :end-row 2 :end-col 8))
        (multi-eof (make-omnidiff-json-range :start-row 2 :start-col 0
                                             :end-row 5 :end-col 0))
        (multi-col (make-omnidiff-json-range :start-row 2 :start-col 4
                                             :end-row 5 :end-col 3)))
    (should (= (omnidiff-engine-range-last-row single) 2))
    (should (= (omnidiff-engine-range-last-row multi-eof) 4))
    (should (= (omnidiff-engine-range-last-row multi-col) 5))
    (should (= (omnidiff-engine-range-last-row multi-eof 4) 3))
    (should (= (omnidiff-engine-range-last-row multi-col 4) 3))
    (should-not (omnidiff-engine-range-last-row multi-col 2))
    (should-not (omnidiff-engine-range-last-row multi-col 0))
    ;; `omnidiff-engine-range-contains-p' right-open semantics.
    (should-not (omnidiff-engine-range-contains-p nil 0 0))
    (should-not (omnidiff-engine-range-contains-p single 2 2))
    (should (omnidiff-engine-range-contains-p single 2 3))
    (should (omnidiff-engine-range-contains-p single 2 7))
    (should-not (omnidiff-engine-range-contains-p single 2 8))
    (should-not (omnidiff-engine-range-contains-p multi-col 2 3))
    (should (omnidiff-engine-range-contains-p multi-col 2 4))
    (should (omnidiff-engine-range-contains-p multi-col 3 99))
    (should (omnidiff-engine-range-contains-p multi-col 5 2))
    (should-not (omnidiff-engine-range-contains-p multi-col 5 3))))

(ert-deftest omnidiff-engine-test/path-format-bytes-and-git-helpers ()
  "Promoted path, byte-formatting, worktree-diff, and file accessors behave cleanly."
  ;; `omnidiff-engine-truncate-path'
  (should (equal (omnidiff-engine-truncate-path "a/b.el" 20) "a/b.el"))
  (should (equal (omnidiff-engine-truncate-path "a/b.el" 0) "a/b.el"))
  (let ((trunc (omnidiff-engine-truncate-path "alpha/beta/gamma/delta.el" 18)))
    (should (string-prefix-p "…/" trunc))
    (should (string-suffix-p "gamma/delta.el" trunc))
    (should (<= (string-width trunc) 18)))
  (let ((narrow (omnidiff-engine-truncate-path "verylongfilename.el" 10)))
    (should (<= (string-width narrow) 10))
    (should (string-suffix-p "…" narrow)))
  (let ((tiny (omnidiff-engine-truncate-path "a/b/c.el" 4)))
    (should (<= (string-width tiny) 4)))
  ;; `omnidiff-engine-pick-path-hint'
  (should (equal (omnidiff-engine-pick-path-hint nil "" "/dev/null" 42 "src/a.el" "b.el")
                 "src/a.el"))
  (should-not (omnidiff-engine-pick-path-hint nil "" "/dev/null"))
  ;; `omnidiff-engine-format-bytes' (IEC binary units + negative deltas)
  (should (equal (omnidiff-engine-format-bytes 0) "0 B"))
  (should (equal (omnidiff-engine-format-bytes 1023) "1023 B"))
  (should (equal (omnidiff-engine-format-bytes 1024) "1.0 KiB"))
  (should (equal (omnidiff-engine-format-bytes 1536) "1.5 KiB"))
  (should (equal (omnidiff-engine-format-bytes 1048576) "1.0 MiB"))
  (should (equal (omnidiff-engine-format-bytes 1073741824) "1.0 GiB"))
  (should (equal (omnidiff-engine-format-bytes -42) "-42 B"))
  (should (equal (omnidiff-engine-format-bytes -2048) "-2.0 KiB"))
  (should (equal (omnidiff-engine-format-bytes -1048576) "-1.0 MiB"))
  (should (equal (omnidiff-engine-format-bytes -1073741824) "-1.0 GiB"))
  ;; `omnidiff-engine-unstaged-worktree-diff-p'
  (should (omnidiff-engine-unstaged-worktree-diff-p
           '("git" "--no-pager" "diff" "--no-ext-diff")))
  (should (omnidiff-engine-unstaged-worktree-diff-p
           '("git" "--no-pager" "diff" "-M" "--" "HEAD" "a..b")))
  (should-not (omnidiff-engine-unstaged-worktree-diff-p
               '("git" "--no-pager" "diff" "--no-ext-diff" "--cached")))
  (should-not (omnidiff-engine-unstaged-worktree-diff-p
               '("git" "--no-pager" "diff" "--no-ext-diff" "--staged")))
  (should-not (omnidiff-engine-unstaged-worktree-diff-p
               '("git" "--no-pager" "diff" "--no-ext-diff" "HEAD~1..HEAD")))
  (should-not (omnidiff-engine-unstaged-worktree-diff-p
               '("git" "--no-pager" "diff" "--no-ext-diff" "HEAD")))
  (should-not (omnidiff-engine-unstaged-worktree-diff-p
               '("git" "--no-pager" "show" "HEAD")))
  (should-not (omnidiff-engine-unstaged-worktree-diff-p nil))
  (should-not (omnidiff-engine-unstaged-worktree-diff-p
               '("git" "--no-pager" "diff" "--no-ext-diff" . "improper")))
  ;; `omnidiff-engine-file-*' accessors (nil-safe and populated)
  (dolist (f (list nil (make-omnidiff-file :path "empty.el")))
    (should-not (omnidiff-engine-file-picture f))
    (should-not (omnidiff-engine-file-summary f))
    (should-not (omnidiff-engine-file-binary-p f))
    (should-not (omnidiff-engine-file-container f))
    (should-not (omnidiff-engine-file-large-residual f)))
  (let* ((pic (make-omnidiff-picture :comparison 'pixels :changed-pixels 5))
         (cont (make-omnidiff-json-container :family 'archives :unchanged 3))
         (file (omnidiff-test--make-file
                :path "bundle.zip"
                :picture pic
                :summary 'whitespace-only
                :binary-p t
                :container cont
                :large-residual t)))
    (should (eq (omnidiff-engine-file-picture file) pic))
    (should (eq (omnidiff-engine-file-summary file) 'whitespace-only))
    (should (omnidiff-engine-file-binary-p file))
    (should (eq (omnidiff-engine-file-container file) cont))
    (should (omnidiff-engine-file-large-residual file))))

;;; JSON diff, picture, and recursive container parsing

(ert-deftest omnidiff-engine-test/json-unsorted-hunks-summary-and-encoding ()
  "JSON parser sorts hunks and decodes summary, encoding, and large-residual."
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
    (should (equal (omnidiff-engine-summary-label 'no-changes) "no changes"))
    (should (equal (omnidiff-engine-summary-label 'new-file) "new file"))
    (should (equal (omnidiff-engine-summary-label 'deleted-file) "deleted file"))
    (should-not (omnidiff-engine-summary-label nil))
    (should (equal (omnidiff-json-side-encoding b-side) "UTF-16LE"))
    (should (= (length hunks) 3))
    (should (equal (mapcar (lambda (h)
                             (let ((r (omnidiff-json-hunk-range h)))
                               (list (omnidiff-json-hunk-operation h)
                                     (omnidiff-json-range-start-row r)
                                     (omnidiff-json-range-start-col r))))
                           hunks)
                   '((move 2 1) (update 2 8) (delete 5 0))))
    (should (= (omnidiff-json-range-start-row
                (omnidiff-json-hunk-move-target (nth 0 hunks)))
               9))
    (should (= (omnidiff-json-hunk-reference-line (nth 1 hunks)) 1))))

(defun omnidiff-engine-test--wrap-picture-json (cmp-json)
  "Parse an `omnidiff-json-diff' wrapping picture comparison CMP-JSON."
  (omnidiff-engine-parse-json
   (format
    (concat "{\"before\":{\"path\":\"a.png\",\"hunks\":[]},"
            "\"after\":{\"path\":\"b.png\",\"hunks\":[]},"
            "\"binary\":true,"
            "\"content\":{\"kind\":\"picture\","
            "\"before\":{\"format\":\"PNG\",\"width\":10,\"height\":10,\"color\":\"RGB8\",\"bytes\":100},"
            "\"after\":{\"format\":\"PNG\",\"width\":20,\"height\":20,\"color\":\"RGBA8\",\"bytes\":200},"
            "\"comparison\":%s}}")
    cmp-json)))

(ert-deftest omnidiff-engine-test/json-picture-variants ()
  "Picture JSON parsing handles pixels, identical, resized, scaled, and frames."
  (let ((p-pix (omnidiff-json-diff-picture
                (omnidiff-engine-test--wrap-picture-json
                 "{\"kind\":\"pixels\",\"changed_pixels\":25,\"total_pixels\":100,\"regions\":[{\"x\":1,\"y\":2,\"width\":5,\"height\":5}]}"))))
    (should (omnidiff-picture-p p-pix))
    (should (eq (omnidiff-picture-comparison p-pix) 'pixels))
    (should (= (omnidiff-picture-changed-pixels p-pix) 25))
    (should (= (omnidiff-picture-total-pixels p-pix) 100))
    (should (< (abs (- (omnidiff-picture-changed-pct p-pix) 25.0)) 0.001))
    (should (= (omnidiff-picture-region-count p-pix) 1))
    (should (= (omnidiff-picture-region-y (car (omnidiff-picture-regions p-pix))) 2)))
  (let ((p-ident (omnidiff-json-diff-picture
                  (omnidiff-engine-test--wrap-picture-json
                   "{\"kind\":\"pixels\",\"changed_pixels\":0,\"total_pixels\":100,\"regions\":[]}"))))
    (should (eq (omnidiff-picture-comparison p-ident) 'identical))
    (should (= (omnidiff-picture-changed-pct p-ident) 0.0))
    (should (= (omnidiff-picture-changed-pixels p-ident) 0)))
  (let ((p-scaled (omnidiff-json-diff-picture
                   (omnidiff-engine-test--wrap-picture-json
                    "{\"kind\":\"resized\",\"scaled\":{\"changed_pixels\":10,\"total_pixels\":200,\"regions\":[{\"x\":0,\"y\":0,\"width\":2,\"height\":5}]}}"))))
    (should (eq (omnidiff-picture-comparison p-scaled) 'resized-scaled))
    (should (< (abs (- (omnidiff-picture-changed-pct p-scaled) 5.0)) 0.001))
    (should (= (omnidiff-picture-region-count p-scaled) 1)))
  (let ((p-resized (omnidiff-json-diff-picture
                    (omnidiff-engine-test--wrap-picture-json "{\"kind\":\"resized\"}"))))
    (should (eq (omnidiff-picture-comparison p-resized) 'resized)))
  (let ((p-one-sided (omnidiff-json-diff-picture
                      (omnidiff-engine-test--wrap-picture-json "{\"kind\":\"one_sided\"}"))))
    (should (eq (omnidiff-picture-comparison p-one-sided) 'one-sided)))
  (let ((p-frames (omnidiff-json-diff-picture
                   (omnidiff-engine-test--wrap-picture-json
                    "{\"kind\":\"frames\",\"total_pixels\":400,\"retimed\":true,\"faint\":true,\"steps\":[{\"kind\":\"changed\"},{\"kind\":\"inserted\",\"frames\":2},{\"kind\":\"deleted\",\"frames\":1}]}"))))
    (should (eq (omnidiff-picture-comparison p-frames) 'unknown))
    (should (= (omnidiff-picture-total-pixels p-frames) 400))
    (should (equal (omnidiff-picture-raw-comparison p-frames)
                   "animation: 1 changed, 2 added, 1 removed frames (retimed) (faint)"))))

(ert-deftest omnidiff-engine-test/parse-picture-nonzero-changed-pixels-empty-regions ()
  "Non-zero `changed_pixels' with empty `regions' stays non-identical."
  ;; `pixels' with changed_pixels > 0 and regions: [] must stay `pixels'.
  (let ((p-pix (omnidiff-json-diff-picture
                (omnidiff-engine-test--wrap-picture-json
                 "{\"kind\":\"pixels\",\"changed_pixels\":12,\"total_pixels\":100,\"regions\":[]}"))))
    (should (eq (omnidiff-picture-comparison p-pix) 'pixels))
    (should-not (eq (omnidiff-picture-comparison p-pix) 'identical))
    (should (= (omnidiff-picture-changed-pixels p-pix) 12))
    (should (= (omnidiff-picture-total-pixels p-pix) 100))
    (should (< (abs (- (omnidiff-picture-changed-pct p-pix) 12.0)) 0.001))
    (should (= (omnidiff-picture-region-count p-pix) 0))
    (should-not (omnidiff-picture-regions p-pix)))
  ;; `resized.scaled' with changed_pixels > 0 and regions: [] preserves counts/pct.
  (let ((p-scaled (omnidiff-json-diff-picture
                   (omnidiff-engine-test--wrap-picture-json
                    "{\"kind\":\"resized\",\"scaled\":{\"changed_pixels\":8,\"total_pixels\":200,\"regions\":[]}}"))))
    (should (eq (omnidiff-picture-comparison p-scaled) 'resized-scaled))
    (should-not (eq (omnidiff-picture-comparison p-scaled) 'identical))
    (should (= (omnidiff-picture-changed-pixels p-scaled) 8))
    (should (= (omnidiff-picture-total-pixels p-scaled) 200))
    (should (< (abs (- (omnidiff-picture-changed-pct p-scaled) 4.0)) 0.001))
    (should (= (omnidiff-picture-region-count p-scaled) 0))
    (should-not (omnidiff-picture-regions p-scaled)))
  ;; Contrast: `pixels' with changed_pixels: 0 and regions: [] IS `identical'.
  (let ((p-ident (omnidiff-json-diff-picture
                  (omnidiff-engine-test--wrap-picture-json
                   "{\"kind\":\"pixels\",\"changed_pixels\":0,\"total_pixels\":100,\"regions\":[]}"))))
    (should (eq (omnidiff-picture-comparison p-ident) 'identical))
    (should (= (omnidiff-picture-changed-pixels p-ident) 0))
    (should (= (omnidiff-picture-changed-pct p-ident) 0.0))))

(ert-deftest omnidiff-engine-test/json-recursive-container ()
  "Recursive container JSON parses text, binary, picture, and nested containers."
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
    (should (equal (omnidiff-json-container-side-format
                    (omnidiff-json-container-before cont))
                   "ZIP"))
    (should (= (length members) 4))
    (let ((m-txt (nth 0 members))
          (m-bin (nth 1 members))
          (m-pic (nth 2 members))
          (m-doc (nth 3 members)))
      (should (eq (omnidiff-json-container-member-detail-kind m-txt) 'text))
      (should (eq (plist-get (omnidiff-json-container-member-detail m-txt) :whitespace)
                  'trailing))
      (should (plist-get (omnidiff-json-container-member-detail m-txt) :words-kept))
      (should (eq (omnidiff-json-container-member-detail-kind m-bin) 'binary))
      (should (= (plist-get (omnidiff-json-container-member-detail m-bin) :after-bytes)
                 32))
      (should (eq (omnidiff-json-container-member-detail-kind m-pic) 'picture))
      (should (omnidiff-picture-p (omnidiff-json-container-member-detail m-pic)))
      (should (eq (omnidiff-json-container-member-detail-kind m-doc) 'content))
      (let ((nested (omnidiff-json-container-member-detail m-doc)))
        (should (omnidiff-json-container-p nested))
        (should (eq (omnidiff-json-container-family nested) 'documents))
        (should (equal (omnidiff-json-container-member-key
                        (car (omnidiff-json-container-members nested)))
                       "page 2"))))))

;;; UTF-8 byte offsets, paint-row length, and EOF/trailing-ws spans

(ert-deftest omnidiff-engine-test/utf8-byte-to-char-and-paint-row-len ()
  "UTF-8 byte-to-char and `paint-row-byte-len' handle ASCII, CJK, and emoji."
  (should (= (omnidiff-engine-line-byte-to-char "abcdef" -2) 0))
  (should (= (omnidiff-engine-line-byte-to-char "abcdef" 3) 3))
  (should (= (omnidiff-engine-line-byte-to-char "abcdef" 99) 6))
  (should (= (omnidiff-engine-line-byte-to-char "" 5) 0))
  (let ((cjk "a中b文"))
    (should (= (omnidiff-engine-line-byte-to-char cjk 1) 1))
    (should (= (omnidiff-engine-line-byte-to-char cjk 2) 1)) ; mid-char floors
    (should (= (omnidiff-engine-line-byte-to-char cjk 3) 1))
    (should (= (omnidiff-engine-line-byte-to-char cjk 4) 2))
    (should (= (omnidiff-engine-line-byte-to-char cjk 5) 3))
    (should (= (omnidiff-engine-line-byte-to-char cjk 8) 4)))
  (let ((emoji (concat "x" (string #x1f600) "y"))) ; x(1) + emoji(4) + y(1) = 6 bytes
    (should (= (omnidiff-engine-line-byte-to-char emoji 1) 1))
    (should (= (omnidiff-engine-line-byte-to-char emoji 2) 1))
    (should (= (omnidiff-engine-line-byte-to-char emoji 4) 1))
    (should (= (omnidiff-engine-line-byte-to-char emoji 5) 2))
    (should (= (omnidiff-engine-line-byte-to-char emoji 6) 3)))
  (should (= (omnidiff-engine-paint-row-byte-len "abc   \r") 3))
  (should (= (omnidiff-engine-paint-row-byte-len
              (concat "中" (string #x1f600) " \u00a0\u3000\r"))
             7))
  (should (= (omnidiff-engine-paint-row-byte-len "  \t\u00a0") 0)))

(ert-deftest omnidiff-engine-test/eof-and-trailing-ws-spans-and-buffer-regions ()
  "EOF-normalized and trailing-whitespace ranges produce exact line and buffer spans."
  (let* ((emoji (string #x1f600))
         (lines (vector "a中  " (concat "b" emoji "  ") "end"))
         (eof-rng (make-omnidiff-json-range :start-row 0 :start-col 0
                                            :end-row 3 :end-col 0))
         (ws-rng (make-omnidiff-json-range :start-row 0 :start-col 4
                                           :end-row 0 :end-col 6)))
    (should (equal (omnidiff-engine-range-line-spans eof-rng lines)
                   '((0 0 2) (1 0 2) (2 0 3))))
    (should (equal (omnidiff-engine-range-line-spans ws-rng lines)
                   '((0 2 4))))
    (with-temp-buffer
      (insert (aref lines 0) "\n" (aref lines 1) "\n" (aref lines 2) "\n")
      (let* ((starts (omnidiff-engine-buffer-line-offsets (current-buffer)))
             (eof-regs (omnidiff-engine-range-to-buffer-regions
                        eof-rng starts (current-buffer)))
             (ws-regs (omnidiff-engine-range-to-buffer-regions
                       ws-rng starts (current-buffer))))
        (should (equal (mapcar (lambda (r)
                                 (buffer-substring-no-properties (car r) (cdr r)))
                               eof-regs)
                       (list "a中" (concat "b" emoji) "end")))
        (should (equal (mapcar (lambda (r)
                                 (buffer-substring-no-properties (car r) (cdr r)))
                               ws-regs)
                       '("  ")))))))

;;; Hunk lookup, counterpart lookup, alignment anchors, and change stops

(ert-deftest omnidiff-engine-test/hunk-at-nested-multiline-prefix-max-fallback-and-cache ()
  "`hunk-at' picks narrower nested multi-line hunks, uses prefix-max, and invalidates cache."
  ;; 1. Narrower nested multi-line hunk (10, 50)..(11, 5) beats (10, 0)..(11, 5).
  (let* ((outer (omnidiff-test--make-hunk 'update 10 0 11 5))
         (inner (omnidiff-test--make-hunk 'insert 10 50 11 5))
         (side (make-omnidiff-json-side :path "nested.el" :hunks (list outer inner))))
    (should-not (omnidiff-engine-hunk-at nil 0 0 t))
    (should (eq (omnidiff-engine-hunk-at side 10 60) inner))
    (should (eq (omnidiff-engine-hunk-at side 11 2) inner))
    (should (eq (omnidiff-engine-hunk-at side 10 10) outer))
    ;; Raw list input also respects signed column-delta tie-breaking.
    (should (eq (omnidiff-engine-hunk-at (list outer inner) 11 2) inner)))
  ;; 2. Prefix-max index finds an earlier multi-line hunk spanning `row' past shorter hunks.
  (let* ((long-hunk (omnidiff-test--make-hunk 'move 0 0 50 10))
         (short-hunks (cl-loop for r from 2 to 20
                               collect (omnidiff-test--make-hunk 'update r 0 r 4)))
         (later-hunk (omnidiff-test--make-hunk 'insert 60 0 60 5))
         (side (make-omnidiff-json-side
                :path "span.el"
                :hunks (append (list long-hunk) short-hunks (list later-hunk)))))
    (should (eq (omnidiff-engine-hunk-at side 30 5) long-hunk))
    (should (eq (omnidiff-engine-hunk-at side 10 2) (nth 8 short-hunks)))
    (should-not (omnidiff-engine-hunk-at side 55 0))
    (should (eq (omnidiff-engine-hunk-at side 60 2) later-hunk)))
  ;; 3. `fallback-on-row' picks nearest hunk on row when exact containment misses.
  (let* ((h-left (omnidiff-test--make-hunk 'update 5 10 5 20))
         (h-right (omnidiff-test--make-hunk 'insert 5 30 5 40))
         (h-multi (omnidiff-test--make-hunk 'delete 3 0 6 5))
         (side (make-omnidiff-json-side
                :path "fb.el" :hunks (list h-multi h-left h-right))))
    ;; On row 5, `h-multi' contains (5, 0) exactly (5 < 6).
    (should (eq (omnidiff-engine-hunk-at side 5 0 nil) h-multi))
    (should (eq (omnidiff-engine-hunk-at side 5 0 t) h-multi))
    ;; Test pure fallback on row 6 beyond `h-multi's end-col=5.
    (should-not (omnidiff-engine-hunk-at side 6 10 nil))
    (should (eq (omnidiff-engine-hunk-at side 6 10 t) h-multi))
    ;; Pure single-row fallback picks nearest by start-col distance.
    (let ((row5-only (make-omnidiff-json-side
                      :path "r5.el" :hunks (list h-left h-right))))
      (should-not (omnidiff-engine-hunk-at row5-only 5 0 nil))
      (should (eq (omnidiff-engine-hunk-at row5-only 5 0 t) h-left))
      (should (eq (omnidiff-engine-hunk-at row5-only 5 28 t) h-right))
      (should (eq (omnidiff-engine-hunk-at row5-only 5 15 t) h-left))))
  ;; 4. Mutating `hunks' invalidates the cached prefix-max index automatically.
  (let* ((h1 (omnidiff-test--make-hunk 'update 1 0 1 5))
         (h2 (omnidiff-test--make-hunk 'delete 4 0 4 8))
         (side (make-omnidiff-json-side :path "cache.el" :hunks (list h1))))
    (should (eq (omnidiff-engine-hunk-at side 1 2) h1))
    (should (eq (omnidiff-json-side-indexed-hunks side)
                (omnidiff-json-side-hunks side)))
    (let ((new-hunks (list h2)))
      (setf (omnidiff-json-side-hunks side) new-hunks)
      (should-not (omnidiff-engine-hunk-at side 1 2))
      (should (eq (omnidiff-engine-hunk-at side 4 3) h2))
      (should (eq (omnidiff-json-side-indexed-hunks side) new-hunks)))))

(ert-deftest omnidiff-engine-test/counterpart-at-overlapping-multiline-and-early-exit ()
  "`counterpart-at' accurately pairs overlapping multi-line hunks and terminates early."
  (let* ((n 200)
         (b-upd (omnidiff-test--make-hunk 'update 5 0 7 10))
         (b-mov-target (make-omnidiff-json-range :start-row 20 :start-col 0
                                                 :end-row 20 :end-col 8))
         (b-mov (omnidiff-test--make-hunk 'move 12 0 12 8 b-mov-target))
         (a-ins-broad (omnidiff-test--make-hunk 'insert 2 0 9 10))
         (a-upd-match (omnidiff-test--make-hunk 'update 4 0 8 10))
         (a-mov-ignore (omnidiff-test--make-hunk 'move 5 0 6 5))
         (a-trailing (cl-loop for r from 30 below 190
                              collect (omnidiff-test--make-hunk 'update r 0 r 6)))
         (jdiff (make-omnidiff-json-diff
                 :before (make-omnidiff-json-side
                          :path "a.el" :hunks (list b-upd b-mov))
                 :after (make-omnidiff-json-side
                         :path "b.el"
                         :hunks (append (list a-ins-broad a-upd-match a-mov-ignore)
                                        a-trailing))))
         (aln (let ((id (vconcat (number-sequence 0 (1- n))))
                    (anc (cl-loop for i from 0 below n collect (cons i i))))
                (make-omnidiff-line-alignment
                 :before-to-after id
                 :after-to-before id
                 :anchors anc
                 :anchor-vec (vconcat anc))))
         (prio-calls 0)
         (orig-prio (symbol-function 'omnidiff-engine--op-counterpart-priority)))
    ;; 1. Overlapping multi-line hunks: prefers `update' over broader `insert' and ignores `move',
    ;;    while stopping early before the 160 trailing hunks on rows 30..189.
    (cl-letf (((symbol-function 'omnidiff-engine--op-counterpart-priority)
               (lambda (src dst)
                 (setq prio-calls (1+ prio-calls))
                 (funcall orig-prio src dst))))
      (let ((cp (omnidiff-engine-counterpart-at jdiff aln 'before 6 2)))
        (should (eq (car cp) 'after))
        (should (equal (cdr cp) (omnidiff-json-hunk-range a-upd-match)))
        (should (<= prio-calls 3))))
    ;; 2. Move hunk resolves directly to its `move-target'.
    (let ((cp-mov (omnidiff-engine-counterpart-at jdiff aln 'before 12 2)))
      (should (eq (car cp-mov) 'after))
      (should (equal (cdr cp-mov) b-mov-target)))
    ;; 3. Unchanged row maps directly to a single-row counterpart range.
    (let ((cp-same (omnidiff-engine-counterpart-at jdiff aln 'before 0 0)))
      (should (eq (car cp-same) 'after))
      (should (= (omnidiff-json-range-start-row (cdr cp-same)) 0))
      (should (= (omnidiff-json-range-end-row (cdr cp-same)) 1)))))

(ert-deftest omnidiff-engine-test/alignment-direct-and-surrounding-anchors ()
  "Alignment anchor helpers handle empty defaults, single rows, and ranges."
  ;; Empty default alignment.
  (let ((empty (make-omnidiff-line-alignment)))
    (should-not (omnidiff-engine-alignment-direct-anchor empty 'before 0))
    (should-not (omnidiff-engine-alignment-direct-anchor empty 'after 0))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors empty 'before 0)
                   '(nil . nil)))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors empty 'after 0 5)
                   '(nil . nil))))
  ;; Populated alignment with strictly increasing anchors.
  (let* ((anchors '((0 . 0) (3 . 4) (7 . 9) (12 . 12)))
         (aln (make-omnidiff-line-alignment
               :before-to-after (make-vector 13 nil)
               :after-to-before (make-vector 13 nil)
               :anchors anchors
               :anchor-vec (vconcat anchors))))
    ;; Direct anchor lookups on both sides.
    (should (= (omnidiff-engine-alignment-direct-anchor aln 'before 0) 0))
    (should (= (omnidiff-engine-alignment-direct-anchor aln 'before 3) 4))
    (should (= (omnidiff-engine-alignment-direct-anchor aln 'before 7) 9))
    (should (= (omnidiff-engine-alignment-direct-anchor aln 'after 4) 3))
    (should (= (omnidiff-engine-alignment-direct-anchor aln 'after 9) 7))
    (should-not (omnidiff-engine-alignment-direct-anchor aln 'before 2))
    (should-not (omnidiff-engine-alignment-direct-anchor aln 'after 5))
    (should-not (omnidiff-engine-alignment-direct-anchor aln 'before 99))
    ;; Single-row strictly surrounding anchors.
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'before -1)
                   '(nil . (0 . 0))))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'before 0)
                   '(nil . (3 . 4))))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'before 2)
                   '((0 . 0) . (3 . 4))))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'before 3)
                   '((0 . 0) . (7 . 9))))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'after 4)
                   '((0 . 0) . (7 . 9))))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'before 12)
                   '((7 . 9) . nil)))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'before 15)
                   '((12 . 12) . nil)))
    ;; Range queries [r0, r1].
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'before 3 7)
                   '((0 . 0) . (12 . 12))))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'before 4 6)
                   '((3 . 4) . (7 . 9))))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'after 4 9)
                   '((0 . 0) . (12 . 12))))
    (should (equal (omnidiff-engine-alignment-surrounding-anchors aln 'before 0 12)
                   '(nil . nil)))))

(ert-deftest omnidiff-engine-test/zero-width-gap-alignment-stops-and-counterpart ()
  "Zero-width gap delete before an edited line stays two separate stops."
  (let* ((b-lines ["def run(x):" "    obsolete()" "    return compute(x)"])
         (a-lines ["def run(x):" "    return compute(x, debug=True)"])
         (jdiff (make-omnidiff-json-diff
                 :before (make-omnidiff-json-side
                          :path "a.py"
                          :hunks (list (omnidiff-test--make-hunk 'delete 1 0 1 14 nil 0)))
                 :after (make-omnidiff-json-side
                         :path "b.py"
                         :hunks (list (omnidiff-test--make-hunk 'insert 1 20 1 32 nil 0)))))
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
              ;; Counterpart ranges must be empty (never cross-highlight `, debug=True').
              (should (eq (car cp-del) 'after))
              (should-not (omnidiff-engine-range-last-row (cdr cp-del)))
              (should (eq (car cp-ins) 'before))
              (should-not (omnidiff-engine-range-last-row (cdr cp-ins)))))))
    (funcall verify-gap jdiff)
    (when (executable-find omnidiff-executable)
      (omnidiff-test--with-temp-dir dir
        (let ((fa (expand-file-name "gap_a.py" dir))
              (fb (expand-file-name "gap_b.py" dir)))
          (with-temp-file fa (insert (string-join (append b-lines nil) "\n") "\n"))
          (with-temp-file fb (insert (string-join (append a-lines nil) "\n") "\n"))
          (funcall verify-gap (omnidiff-engine-run-pair fa fb 'minimal)))))))

(ert-deftest omnidiff-engine-test/build-change-stops-order-and-non-quadratic ()
  "`build-change-stops' orders stops by reading key and scales non-quadratically."
  ;; 1. Verify `stop-sort-key' and `stop-key-less-p' ordering rules directly.
  (let* ((st-del (make-omnidiff-change-stop
                  :side 'before :operation 'delete
                  :range (make-omnidiff-json-range :start-row 2 :start-col 0
                                                   :end-row 2 :end-col 5)))
         (st-upd (make-omnidiff-change-stop
                  :side 'after :operation 'update
                  :range (make-omnidiff-json-range :start-row 2 :start-col 4
                                                   :end-row 2 :end-col 9)
                  :counterpart-side 'before
                  :counterpart-range (make-omnidiff-json-range
                                      :start-row 3 :start-col 4
                                      :end-row 3 :end-col 9)))
         (st-ins (make-omnidiff-change-stop
                  :side 'after :operation 'insert
                  :range (make-omnidiff-json-range :start-row 5 :start-col 1
                                                   :end-row 5 :end-col 6)))
         (k-del (omnidiff-engine-stop-sort-key st-del))
         (k-upd (omnidiff-engine-stop-sort-key st-upd))
         (k-ins (omnidiff-engine-stop-sort-key st-ins)))
    (should (equal k-del '(2 2 0)))
    (should (equal k-upd '(2 3 4)))
    (should (equal k-ins '(5 5 1)))
    (should (omnidiff-engine-stop-key-less-p k-del k-upd))
    (should (omnidiff-engine-stop-key-less-p k-upd k-ins))
    (should-not (omnidiff-engine-stop-key-less-p k-ins k-del))
    (should-not (omnidiff-engine-stop-key-less-p k-upd k-upd))
    ;; With alignment, pure deletes/inserts map their missing side across anchors.
    (let ((shift-aln (make-omnidiff-line-alignment
                      :before-to-after [5 6 7 8 9 10]
                      :after-to-before [0 0 0 0 0 0 1 2 3 4 5]
                      :anchors '((0 . 5) (5 . 10))
                      :anchor-vec [(0 . 5) (5 . 10)])))
      (should (equal (omnidiff-engine-stop-sort-key st-del shift-aln) '(7 2 0)))
      (should (equal (omnidiff-engine-stop-sort-key st-ins shift-aln) '(5 0 1)))))
  ;; 2. 500 paired edits + 200 reversed non-contiguous moves = 700 stops in O(N log N).
  (let* ((n-edits 500)
         (n-moves 200)
         (move-base (* 2 n-edits))
         (total-rows (* 2 (+ n-edits n-moves)))
         (b-lines (make-vector total-rows ""))
         (a-lines (make-vector total-rows ""))
         (b-hunks nil)
         (a-hunks nil))
    (dotimes (i n-edits)
      (let ((edit-row (* 2 i))
            (anc-row (1+ (* 2 i))))
        (aset b-lines edit-row (format "old_val_%d = 1" i))
        (aset a-lines edit-row (format "new_val_%d = 2" i))
        (aset b-lines anc-row (format "anchor_edit_%d" i))
        (aset a-lines anc-row (format "anchor_edit_%d" i))
        (push (omnidiff-test--make-hunk 'update edit-row 0 edit-row 10) b-hunks)
        (push (omnidiff-test--make-hunk 'update edit-row 0 edit-row 10) a-hunks)))
    (dotimes (m n-moves)
      (let* ((b-row (+ move-base (* 2 m)))
             (a-row (+ move-base (* 2 (- (1- n-moves) m))))
             (anc-row (1+ b-row)))
        (aset b-lines b-row (format "moved_block_%d()" m))
        (aset a-lines a-row (format "moved_block_%d()" m))
        (aset b-lines anc-row (format "anchor_move_%d" m))
        (aset a-lines anc-row (format "anchor_move_%d" m))
        (push (omnidiff-test--make-hunk
               'move b-row 0 b-row 14
               (make-omnidiff-json-range :start-row a-row :start-col 0
                                         :end-row a-row :end-col 14))
              b-hunks)
        (push (omnidiff-test--make-hunk
               'move a-row 0 a-row 14
               (make-omnidiff-json-range :start-row b-row :start-col 0
                                         :end-row b-row :end-col 14))
              a-hunks)))
    (let* ((jdiff (make-omnidiff-json-diff
                   :before (make-omnidiff-json-side
                            :path "perf_a.py"
                            :hunks (sort b-hunks #'omnidiff-engine--hunk-less-p))
                   :after (make-omnidiff-json-side
                           :path "perf_b.py"
                           :hunks (sort a-hunks #'omnidiff-engine--hunk-less-p))))
           (aln (omnidiff-engine-compute-alignment b-lines a-lines jdiff))
           (t0 (float-time))
           (stops (omnidiff-engine-build-change-stops jdiff aln))
           (elapsed (- (float-time) t0))
           (update-count 0)
           (move-count 0)
           (prev-key nil)
           (expected-idx 1))
      (should (= (length stops) (+ n-edits n-moves)))
      (should (< elapsed 2.0))
      (dolist (st stops)
        (should (= (omnidiff-change-stop-index st) expected-idx))
        (setq expected-idx (1+ expected-idx))
        (pcase (omnidiff-change-stop-operation st)
          ('update (setq update-count (1+ update-count)))
          ('move (setq move-count (1+ move-count))))
        (let ((key (omnidiff-engine-stop-sort-key st aln)))
          (when prev-key
            (should (omnidiff-engine-stop-key-less-p prev-key key)))
          (setq prev-key key)))
      (should (= update-count n-edits))
      (should (= move-count n-moves)))))

;;; Sync and async CLI execution

(ert-deftest omnidiff-engine-test/real-sync-and-async-run-pair ()
  "Sync and async `omnidiff' runs handle Python CJK, moves, `/dev/null', and errors."
  (skip-unless (executable-find omnidiff-executable))
  (omnidiff-test--with-temp-dir dir
    (let ((fa (expand-file-name "before.py" dir))
          (fb (expand-file-name "after.py" dir)))
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
        (should (equal (omnidiff-json-side-language (omnidiff-json-diff-after sync-diff))
                       "Python"))
        (should (cl-some (lambda (h)
                           (and (eq (omnidiff-json-hunk-operation h) 'move)
                                (omnidiff-json-range-p (omnidiff-json-hunk-move-target h))))
                         bh))
        (should (cl-some (lambda (h) (eq (omnidiff-json-hunk-operation h) 'update))
                         ah))
        ;; `/dev/null' new-file and deleted-file runs.
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
          (omnidiff-engine-run-pair-async
           fa fb
           (lambda (d) (setq async-res d))
           :preset 'default
           :error-callback (lambda (e) (setq async-err e)))
          (should (omnidiff-test--wait-until (lambda () (or async-res async-err))))
          (should-not async-err)
          (should (omnidiff-json-diff-p async-res))
          (should (= (length (omnidiff-json-side-hunks (omnidiff-json-diff-before async-res)))
                     (length bh)))
          (should (= (length (omnidiff-json-side-hunks (omnidiff-json-diff-after async-res)))
                     (length ah))))
        ;; Async run error callback fires on nonexistent input file.
        (let ((ok-called nil)
              (err-msg nil)
              (missing (expand-file-name "does-not-exist-xyz.py" dir)))
          (omnidiff-engine-run-pair-async
           missing fb
           (lambda (_d) (setq ok-called t))
           :error-callback (lambda (msg) (setq err-msg msg)))
          (should (omnidiff-test--wait-until (lambda () (or ok-called err-msg))))
          (should-not ok-called)
          (should (stringp err-msg))
          (should-not (string-empty-p err-msg)))))))

(ert-deftest omnidiff-engine-test/async-stderr-clean-error-callback ()
  "Async stderr error callback reports stderr without sentinel noise."
  (omnidiff-test--with-temp-dir dir
    (let ((fake-bin (expand-file-name "fake-omnidiff-err.sh" dir))
          (fa (expand-file-name "err_a.txt" dir))
          (fb (expand-file-name "err_b.txt" dir))
          (ok-called nil)
          (err-msg nil))
      (with-temp-file fake-bin
        (insert "#!/bin/sh\necho 'synthetic stderr boom' >&2\nexit 2\n"))
      (set-file-modes fake-bin #o755)
      (with-temp-file fa (insert "a\n"))
      (with-temp-file fb (insert "b\n"))
      (omnidiff-engine-run-pair-async
       fa fb
       (lambda (_d) (setq ok-called t))
       :executable fake-bin
       :error-callback (lambda (msg) (setq err-msg msg)))
      (should (omnidiff-test--wait-until (lambda () (or ok-called err-msg))))
      (should-not ok-called)
      (should (stringp err-msg))
      (should (string-search "synthetic stderr boom" err-msg))
      (should-not (string-search "Process omnidiff-json-stderr finished" err-msg))
      (should-not (string-search "Process omnidiff-json-stderr" err-msg)))))

(provide 'omnidiff-engine-test)
;;; omnidiff-engine-test.el ends here
