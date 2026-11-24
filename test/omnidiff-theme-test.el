;;; omnidiff-theme-test.el --- ERT tests for omnidiff-theme -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; This file is part of omnidiff.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Unit and regression tests for `omnidiff-theme': color math and Rec. 709
;; luminance, face definitions, scheme cycling, palette resolution across
;; Modus and generic themes, `face-defface-spec' override preservation,
;; memoization, daemon/low-color frame deferral, and semantic color caching.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'omnidiff-theme)
(require 'omnidiff-test-helper)

;;; Test constants & isolation helpers

(defconst omnidiff-theme-test--token-faces
  '(omnidiff-token-insert
    omnidiff-token-delete
    omnidiff-token-update
    omnidiff-token-move
    omnidiff-token-cross-highlight
    omnidiff-token-active)
  "Managed semantic token highlight faces.")

(defconst omnidiff-theme-test--token-bg-keys
  '((omnidiff-token-insert . :insert-bg)
    (omnidiff-token-delete . :delete-bg)
    (omnidiff-token-update . :update-bg)
    (omnidiff-token-move . :move-bg)
    (omnidiff-token-cross-highlight . :cross-bg)
    (omnidiff-token-active . :active-bg))
  "Mapping from semantic token faces to `omnidiff-theme-semantic-colors' keys.")

(defconst omnidiff-theme-test--semantic-keys
  '(:insert-bg :delete-bg :update-bg :move-bg :cross-bg :active-bg
    :overlay-fg :bg-main :bg-chrome :bg-card :border :fg-main :fg-dim
    :accent :gutter-insert :gutter-delete :gutter-update :gutter-move)
  "All keys exported by `omnidiff-theme-semantic-colors'.")

(defconst omnidiff-theme-test--dark-display
  '((class color) (min-colors 88) (background dark))
  "Display predicate for dark >=88-color face spec clauses.")

(defconst omnidiff-theme-test--light-display
  '((class color) (min-colors 88) (background light))
  "Display predicate for light >=88-color face spec clauses.")

(defconst omnidiff-theme-test--rich-display
  '((class color) (min-colors 88))
  "Display predicate for single-polarity >=88-color face spec clauses.")

(defun omnidiff-theme-test--clause-attrs (spec display)
  "Return attribute plist for DISPLAY clause in face SPEC, or nil."
  (cdr (assoc display spec)))

(defmacro omnidiff-theme-test--with-clean-state (&rest body)
  "Run BODY saving and restoring theme globals, frame background, and hooks."
  (declare (indent 0) (debug (body)))
  (let ((saved-bg (make-symbol "saved-bg"))
        (saved-scheme (make-symbol "saved-scheme"))
        (saved-fg (make-symbol "saved-fg"))
        (saved-custom (make-symbol "saved-custom"))
        (saved-height (make-symbol "saved-height"))
        (saved-frame-hooks (make-symbol "saved-frame-hooks"))
        (saved-after-hooks (make-symbol "saved-after-hooks")))
    `(let ((,saved-bg (frame-parameter nil 'background-mode))
           (,saved-scheme omnidiff-color-scheme)
           (,saved-fg omnidiff-dual-overlay-foreground)
           (,saved-custom omnidiff-custom-palette)
           (,saved-height omnidiff-ui-font-height)
           (,saved-frame-hooks (copy-sequence after-make-frame-functions))
           (,saved-after-hooks (copy-sequence omnidiff-theme-after-apply-hook)))
       (unwind-protect
           (progn ,@body)
         (set-frame-parameter nil 'background-mode ,saved-bg)
         (setq omnidiff-color-scheme ,saved-scheme
               omnidiff-dual-overlay-foreground ,saved-fg
               omnidiff-custom-palette ,saved-custom
               omnidiff-ui-font-height ,saved-height
               after-make-frame-functions ,saved-frame-hooks
               omnidiff-theme-after-apply-hook ,saved-after-hooks)
         (omnidiff-apply-theme nil t)))))

;;; Color math, Rec. 709 luma, and face definitions

(ert-deftest omnidiff-theme-test/hex-parsing-blending-and-dark-p ()
  "Hex parser, color converter, blender, and dark predicate handle edge cases."
  (should (equal (omnidiff-theme--parse-hex "#102030") '(16 32 48)))
  (should (equal (omnidiff-theme--parse-hex "#aAbBcC") '(170 187 204)))
  (should-not (omnidiff-theme--parse-hex "invalid"))
  (should-not (omnidiff-theme--parse-hex "#123"))
  (should (equal (omnidiff-theme-color-hex "#AABBCC") "#aabbcc"))
  (should (equal (omnidiff-theme-color-hex "blue") "#0000ff"))
  (should-not (omnidiff-theme-color-hex "unspecified"))
  (should-not (omnidiff-theme-color-hex "unspecified-fg"))
  (should-not (omnidiff-theme-color-hex ""))
  (should-not (omnidiff-theme-color-hex nil))
  (should (equal (omnidiff-theme--blend-hex "#000000" "#ffffff" 0.5) "#808080"))
  (should (equal (omnidiff-theme--blend-hex "unspecified" "#ffffff" 0.5)
                 "unspecified"))
  (should (omnidiff-theme-dark-color-p "#0d0e1c"))
  (should-not (omnidiff-theme-dark-color-p "#ffffff")))

(ert-deftest omnidiff-theme-test/rec709-luma-and-dual-chrome-faces ()
  "Rec. 709 luminance, dual header faces, and underline styling are defined."
  ;; Rec. 709 relative luma: pure green (~182) is light, pure blue (~18) is dark.
  (should-not (omnidiff-theme-dark-color-p "#00ff00"))
  (should (omnidiff-theme-dark-color-p "#0000ff"))
  (should-not (omnidiff-theme-dark-color-p "white"))
  (should (omnidiff-theme-dark-color-p "black"))
  ;; Dual header chrome faces (note `omnidiff-dual-header-file-inactive' was removed).
  (dolist (f '(omnidiff-dual-header-before
               omnidiff-dual-header-after
               omnidiff-dual-header-meta
               omnidiff-dual-header-file
               omnidiff-dual-header-before-inactive
               omnidiff-dual-header-after-inactive
               omnidiff-dual-header-badge))
    (should (facep f)))
  (let* ((omnidiff-dual-overlay-foreground 'preserve-syntax)
         (attrs (omnidiff-theme--face-color-attrs
                 (omnidiff-theme--resolve-palette
                  (omnidiff-theme--generic-base 'dark) 'omni-semantic)))
         (badge-attrs (cdr (assq 'omnidiff-header-badge attrs)))
         (cross-attrs (cdr (assq 'omnidiff-token-cross-highlight attrs))))
    (should (plist-get badge-attrs :underline))
    (should (plist-get cross-attrs :underline))
    (should-not (plist-get cross-attrs :foreground))))

(ert-deftest omnidiff-theme-test/standalone-diff-faces-defined ()
  "Standalone `emacs -Q' load defines inherited `diff-mode' fallback faces."
  (dolist (face '(diff-added diff-removed diff-changed
                  diff-refine-added diff-refine-removed diff-refine-changed))
    (should (facep face))))

(ert-deftest omnidiff-theme-test/exported-apis-and-no-dead-faces ()
  "Wave 1 exported theme helpers work and deleted legacy faces stay absent."
  (should (natnump (omnidiff-theme-revision)))
  (should (= (omnidiff-theme-op-priority 'update) 3))
  (should (= (omnidiff-theme-op-priority 'insert) 2))
  (should (= (omnidiff-theme-op-priority 'delete) 2))
  (should (= (omnidiff-theme-op-priority 'move) 1))
  (should (= (omnidiff-theme-op-priority 'unknown) 0))
  (should (= (omnidiff-theme-op-priority nil) 0))
  (should (eq (omnidiff-theme-token-face 'insert) 'omnidiff-token-insert))
  (should (eq (omnidiff-theme-token-face 'delete) 'omnidiff-token-delete))
  (should (eq (omnidiff-theme-token-face 'update) 'omnidiff-token-update))
  (should (eq (omnidiff-theme-token-face 'move) 'omnidiff-token-move))
  (should (eq (omnidiff-theme-token-face 'unknown) 'omnidiff-token-update))
  ;; Legacy dead faces must remain removed.
  (should-not (facep 'omnidiff-scope))
  (should-not (facep 'omnidiff-moved-header))
  (should-not (facep 'omnidiff-dual-header-file-inactive)))

(ert-deftest omnidiff-theme-test/cycle-color-scheme ()
  "Cycling color scheme rotates exhaustively across all three palettes."
  (omnidiff-theme-test--with-clean-state
    (setq omnidiff-color-scheme 'omni-semantic)
    (omnidiff-apply-theme nil t)
    ;; Interactive wrapper command cycles through all three presets.
    (omnidiff-cycle-color-scheme)
    (should (eq omnidiff-color-scheme 'calm-modus))
    (omnidiff-cycle-color-scheme)
    (should (eq omnidiff-color-scheme 'neutral-paper))
    (omnidiff-cycle-color-scheme)
    (should (eq omnidiff-color-scheme 'omni-semantic))
    ;; Theme-layer API returns the newly selected scheme and bumps revision.
    (let ((rev (omnidiff-theme-revision)))
      (should (eq (omnidiff-theme-cycle-color-scheme) 'calm-modus))
      (should (> (omnidiff-theme-revision) rev)))
    (should (eq (omnidiff-theme-cycle-color-scheme) 'neutral-paper))
    (should (eq (omnidiff-theme-cycle-color-scheme) 'omni-semantic))
    ;; Unknown scheme resets cleanly to `omni-semantic'.
    (setq omnidiff-color-scheme 'nonexistent-scheme)
    (should (eq (omnidiff-theme-cycle-color-scheme) 'omni-semantic))))

;;; Palette resolution across schemes & themes

(ert-deftest omnidiff-theme-test/light-neutral-paper-vs-calm-modus ()
  "Light theme `neutral-paper' produces distinct palette from `calm-modus'."
  (let* ((light-base
          (make-omnidiff-theme--base
           :dark-p nil
           :bg-main "#ffffff" :bg-dim "#f2f2f2" :bg-inactive "#e6e6e6"
           :bg-active "#d7d7d7" :bg-card "#f0f0f0" :bg-chrome "#f5f5f5"
           :border "#cccccc" :fg-main "#000000" :fg-dim "#595959"
           :accent "#0031a9" :blue-cooler "#0000b0"
           :accent-green "#005000" :accent-red "#8f1313" :accent-yellow "#553d00"
           :nuances
           (list :bg-added-faint "#e0f6e0" :bg-added-refine "#bae0ba"
                 :fg-added "#005000" :bg-removed-faint "#ffe8e8"
                 :bg-removed-refine "#f5c2c2" :fg-removed "#8f1313"
                 :bg-changed-faint "#fff3d0" :bg-changed-refine "#ebd080"
                 :fg-changed "#553d00" :green-faint "#2a5045"
                 :red-faint "#7f0000" :yellow-faint "#624416"
                 :indigo "#4a3ab0" :bg-green-nuanced "#ecf7ed"
                 :bg-sage "#c1e7c1" :bg-ochre "#f0d890"
                 :fg-ochre "#624416" :bg-lavender "#ece8ff"
                 :raw-moved "#ece8ff")))
         (p-calm (omnidiff-theme--resolve-palette light-base 'calm-modus))
         (p-paper (omnidiff-theme--resolve-palette light-base 'neutral-paper)))
    (should-not (equal (plist-get p-calm :add-fg) (plist-get p-paper :add-fg)))
    (should-not (equal (plist-get p-calm :add-bg) (plist-get p-paper :add-bg)))
    (should (eq (plist-get p-paper :ref-weight) 'normal))
    (should (eq (plist-get p-calm :ref-weight) 'semi-bold))))

(ert-deftest omnidiff-theme-test/unknown-scheme-falls-back-to-omni-semantic ()
  "Unknown `omnidiff-color-scheme' values fall back gracefully to `omni-semantic'."
  (omnidiff-theme-test--with-clean-state
    (cl-letf (((symbol-function 'omnidiff-theme--active-modus-theme) (lambda () nil)))
      (let* ((base (omnidiff-theme--generic-base 'dark))
             (expected (omnidiff-theme--resolve-palette base 'omni-semantic))
             (fallback (omnidiff-theme--resolve-palette base 'totally-unknown-scheme)))
        (should (equal fallback expected)))
      (setq omnidiff-color-scheme 'omni-semantic
            omnidiff-custom-palette nil)
      (omnidiff-apply-theme nil t)
      (let ((omni-colors (omnidiff-theme-semantic-colors)))
        (setq omnidiff-color-scheme 'bogus-scheme)
        (should (omnidiff-apply-theme nil t))
        (should (equal (omnidiff-theme-semantic-colors) omni-colors))))))

(ert-deftest omnidiff-theme-test/calm-modus-distinct-from-omni-semantic ()
  "`calm-modus' token backgrounds are visibly distinct from `omni-semantic'."
  (dolist (mode '(dark light))
    (let* ((base (omnidiff-theme--generic-base mode))
           (omni (omnidiff-theme--resolve-palette base 'omni-semantic))
           (calm (omnidiff-theme--resolve-palette base 'calm-modus)))
      (dolist (k '(:insert-bg :delete-bg :update-bg :move-bg))
        (let ((c-omni (plist-get omni k))
              (c-calm (plist-get calm k)))
          (should (omnidiff-theme--parse-hex c-omni))
          (should (omnidiff-theme--parse-hex c-calm))
          (should-not (equal c-omni c-calm))
          (should (> (omnidiff-theme--color-distance c-omni c-calm) 10)))))))

(ert-deftest omnidiff-theme-test/neutral-paper-dark-adapts-dynamically ()
  "`neutral-paper' dark mode adapts dynamically across distinct dark themes."
  (omnidiff-theme-test--with-clean-state
    (cl-letf (((symbol-function 'omnidiff-theme--active-modus-theme) (lambda () nil))
              ((symbol-function 'display-color-cells) (lambda (&optional _disp) 16777216)))
      (set-frame-parameter nil 'background-mode 'dark)
      (setq omnidiff-color-scheme 'neutral-paper
            omnidiff-custom-palette nil)
      (cl-flet ((resolve-for (face-map)
                  (cl-letf (((symbol-function 'omnidiff-theme--face-hex)
                             (lambda (face attr)
                               (cdr (assoc (cons face attr) face-map)))))
                    (omnidiff-theme--resolve-palette
                     (omnidiff-theme--generic-base 'dark)
                     'neutral-paper))))
        (let* ((gruvbox
                (resolve-for
                 '(((default . :background) . "#282828")
                   ((default . :foreground) . "#ebdbb2")
                   ((shadow . :foreground) . "#928374")
                   ((success . :foreground) . "#b8bb26")
                   ((error . :foreground) . "#fb4934")
                   ((warning . :foreground) . "#fabd2f")
                   ((link . :foreground) . "#83a598"))))
               (solarized
                (resolve-for
                 '(((default . :background) . "#002b36")
                   ((default . :foreground) . "#839496")
                   ((shadow . :foreground) . "#586e75")
                   ((success . :foreground) . "#859900")
                   ((error . :foreground) . "#dc322f")
                   ((warning . :foreground) . "#b58900")
                   ((link . :foreground) . "#268bd2")))))
          (should (plist-get gruvbox :dark-p))
          (should (plist-get solarized :dark-p))
          (dolist (k '(:bg-main :insert-bg :delete-bg :update-bg :move-bg :add-bg))
            (should-not (equal (plist-get gruvbox k)
                               (plist-get solarized k)))))))))

(ert-deftest omnidiff-theme-test/light-modus-bg-blue-nuanced-fallback ()
  "Light Modus missing `bg-blue-nuanced' falls back to light `#e8ebfa'."
  (cl-flet ((modus-base-with (bg-main fg-main)
              (cl-letf (((symbol-function 'modus-themes-get-current-theme)
                         (lambda () 'modus-operandi))
                        ((symbol-function 'modus-themes-get-color-value)
                         (lambda (name &optional _overrides _theme)
                           (pcase name
                             ('bg-main bg-main)
                             ('fg-main fg-main)
                             (_ nil)))))
                (omnidiff-theme--modus-base))))
    (let* ((light-base (modus-base-with "#ffffff" "#000000"))
           (light-nuances (omnidiff-theme--base-nuances light-base))
           (dark-base (modus-base-with "#000000" "#ffffff"))
           (dark-nuances (omnidiff-theme--base-nuances dark-base)))
      (should-not (omnidiff-theme--base-dark-p light-base))
      (should (equal (plist-get light-nuances :raw-moved) "#e8ebfa"))
      (should (equal (plist-get light-nuances :bg-lavender) "#e8ebfa"))
      (should-not (omnidiff-theme-dark-color-p (plist-get light-nuances :raw-moved)))
      (should (omnidiff-theme--base-dark-p dark-base))
      (should (equal (plist-get dark-nuances :raw-moved) "#12154a"))
      (should (omnidiff-theme-dark-color-p (plist-get dark-nuances :raw-moved))))))

;;; Display-guarded face specs, foreground modes, and custom overrides

(ert-deftest omnidiff-theme-test/no-theme-dual-branch-specs-and-semantic-colors ()
  "No-theme Emacs emits dark/light >=88-color clauses plus non-hex fallback."
  (omnidiff-theme-test--with-clean-state
    (cl-letf (((symbol-function 'omnidiff-theme--active-modus-theme) (lambda () nil)))
      (let ((custom-enabled-themes nil)
            dark-colors light-colors)
        (setq omnidiff-color-scheme 'omni-semantic
              omnidiff-custom-palette nil
              omnidiff-dual-overlay-foreground 'preserve-syntax)
        (dolist (mode '(dark light))
          (set-frame-parameter nil 'background-mode mode)
          (let ((prev-rev (omnidiff-theme-revision)))
            (should (omnidiff-apply-theme nil t))
            (should (> (omnidiff-theme-revision) prev-rev)))
          (let ((colors (omnidiff-theme-semantic-colors)))
            (if (eq mode 'dark)
                (setq dark-colors colors)
              (setq light-colors colors))
            (dolist (k omnidiff-theme-test--semantic-keys)
              (let ((hex (plist-get colors k)))
                (should (stringp hex))
                (should (omnidiff-theme--parse-hex hex))))
            (should (eq (omnidiff-theme-dark-color-p (plist-get colors :bg-main))
                        (eq mode 'dark)))))
        (should-not (equal (plist-get dark-colors :bg-main)
                           (plist-get light-colors :bg-main)))
        (should-not (equal (plist-get dark-colors :insert-bg)
                           (plist-get light-colors :insert-bg)))
        (dolist (f omnidiff-theme-test--token-faces)
          (let* ((spec (get f 'face-defface-spec))
                 (dark-attrs (omnidiff-theme-test--clause-attrs
                              spec omnidiff-theme-test--dark-display))
                 (light-attrs (omnidiff-theme-test--clause-attrs
                               spec omnidiff-theme-test--light-display))
                 (fb-attrs (omnidiff-theme-test--clause-attrs spec t))
                 (bg-key (cdr (assq f omnidiff-theme-test--token-bg-keys))))
            (should (= (length spec) 3))
            (should dark-attrs)
            (should light-attrs)
            (should fb-attrs)
            (should (equal (plist-get dark-attrs :background)
                           (plist-get dark-colors bg-key)))
            (should (equal (plist-get light-attrs :background)
                           (plist-get light-colors bg-key)))
            (should-not (equal (plist-get dark-attrs :background)
                               (plist-get light-attrs :background)))
            ;; 8-color/mono `(t ...)' fallback must avoid hex colors.
            (should-not (omnidiff-theme--parse-hex (plist-get fb-attrs :background)))
            (should-not (omnidiff-theme--parse-hex (plist-get fb-attrs :foreground)))
            ;; Token faces never extend and carry no foreground under `preserve-syntax'.
            (should-not (eq (face-attribute f :extend nil) t))
            (dolist (clause spec)
              (let ((attrs (cdr clause)))
                (should-not (eq (plist-get attrs :extend) t))
                (should-not (plist-get attrs :foreground))))))))))

(ert-deftest omnidiff-theme-test/dual-overlay-foreground-modes ()
  "`omnidiff-dual-overlay-foreground' toggles `uniform' vs `preserve-syntax'."
  (omnidiff-theme-test--with-clean-state
    (cl-letf (((symbol-function 'omnidiff-theme--active-modus-theme) (lambda () nil)))
      (let ((custom-enabled-themes nil)
            dark-fg light-fg)
        (setq omnidiff-color-scheme 'omni-semantic
              omnidiff-custom-palette nil)
        (set-frame-parameter nil 'background-mode 'dark)
        (omnidiff-apply-theme nil t)
        (setq dark-fg (plist-get (omnidiff-theme-semantic-colors) :overlay-fg))
        (set-frame-parameter nil 'background-mode 'light)
        (omnidiff-apply-theme nil t)
        (setq light-fg (plist-get (omnidiff-theme-semantic-colors) :overlay-fg))
        (should (omnidiff-theme--parse-hex dark-fg))
        (should (omnidiff-theme--parse-hex light-fg))
        ;; `uniform' adds `:foreground' to dark and light token clauses.
        (let ((omnidiff-dual-overlay-foreground 'uniform))
          (should (omnidiff-apply-theme))
          (dolist (f omnidiff-theme-test--token-faces)
            (let* ((spec (get f 'face-defface-spec))
                   (dark-attrs (omnidiff-theme-test--clause-attrs
                                spec omnidiff-theme-test--dark-display))
                   (light-attrs (omnidiff-theme-test--clause-attrs
                                 spec omnidiff-theme-test--light-display)))
              (should (equal (plist-get dark-attrs :foreground) dark-fg))
              (should (equal (plist-get light-attrs :foreground) light-fg))
              (dolist (clause spec)
                (should-not (eq (plist-get (cdr clause) :extend) t))))))
        ;; Switching back to `preserve-syntax' drops `:foreground' from all clauses.
        (let ((omnidiff-dual-overlay-foreground 'preserve-syntax))
          (should (omnidiff-apply-theme))
          (dolist (f omnidiff-theme-test--token-faces)
            (dolist (clause (get f 'face-defface-spec))
              (should-not (plist-get (cdr clause) :foreground))
              (should-not (eq (plist-get (cdr clause) :extend) t)))))))))

(ert-deftest omnidiff-theme-test/custom-palette-overrides-and-invalid-fallback ()
  "`omnidiff-custom-palette' applies valid overrides and ignores invalid values."
  (omnidiff-theme-test--with-clean-state
    (cl-letf (((symbol-function 'omnidiff-theme--active-modus-theme) (lambda () nil)))
      (let ((custom-enabled-themes nil))
        (set-frame-parameter nil 'background-mode 'dark)
        (setq omnidiff-color-scheme 'omni-semantic
              omnidiff-custom-palette nil
              omnidiff-dual-overlay-foreground 'preserve-syntax)
        (omnidiff-apply-theme nil t)
        (let ((baseline (omnidiff-theme-semantic-colors)))
          ;; Valid hex and named color overrides apply to dark and light clauses.
          (let ((omnidiff-custom-palette '((insert-bg . "#112233")
                                           (delete-bg . "blue")
                                           (accent . "#abcdef"))))
            (should (omnidiff-apply-theme))
            (let ((ov-colors (omnidiff-theme-semantic-colors)))
              (should (equal (plist-get ov-colors :insert-bg) "#112233"))
              (should (equal (plist-get ov-colors :delete-bg) "#0000ff"))
              (should (equal (plist-get ov-colors :accent) "#abcdef"))
              (let ((ins-spec (get 'omnidiff-token-insert 'face-defface-spec))
                    (del-spec (get 'omnidiff-token-delete 'face-defface-spec))
                    (act-spec (get 'omnidiff-token-active 'face-defface-spec)))
                (should (equal (plist-get (omnidiff-theme-test--clause-attrs
                                           ins-spec omnidiff-theme-test--dark-display)
                                          :background)
                               "#112233"))
                (should (equal (plist-get (omnidiff-theme-test--clause-attrs
                                           ins-spec omnidiff-theme-test--light-display)
                                          :background)
                               "#112233"))
                (should (equal (plist-get (omnidiff-theme-test--clause-attrs
                                           del-spec omnidiff-theme-test--dark-display)
                                          :background)
                               "#0000ff"))
                (should (equal (plist-get
                                (plist-get (omnidiff-theme-test--clause-attrs
                                            act-spec omnidiff-theme-test--dark-display)
                                           :box)
                                :color)
                               "#abcdef")))))
          ;; Invalid override entries fall back cleanly to baseline values.
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
              (should (equal (plist-get
                              (omnidiff-theme-test--clause-attrs
                               (get 'omnidiff-token-insert 'face-defface-spec)
                               omnidiff-theme-test--dark-display)
                              :background)
                             (plist-get baseline :insert-bg))))))))))

;;; Memoization, override preservation, daemon deferral, and revision caching

(ert-deftest omnidiff-theme-test/apply-theme-memoization-and-force ()
  "`omnidiff-apply-theme' skips unchanged state unless FORCE is non-nil."
  (omnidiff-theme-test--with-clean-state
    (let ((hook-calls 0))
      (add-hook 'omnidiff-theme-after-apply-hook
                (lambda () (setq hook-calls (1+ hook-calls))))
      ;; Initial forced apply establishes memoization key.
      (should (omnidiff-apply-theme nil t))
      (let ((rev-after-first (omnidiff-theme-revision)))
        (should (= hook-calls 1))
        ;; Unchanged re-apply is a no-op.
        (should-not (omnidiff-apply-theme))
        (should (= (omnidiff-theme-revision) rev-after-first))
        (should (= hook-calls 1))
        ;; Disabled theme argument is ignored even with FORCE.
        (should-not (omnidiff-apply-theme 'omnidiff-nonexistent-theme t))
        (should (= (omnidiff-theme-revision) rev-after-first))
        (should (= hook-calls 1))
        ;; Explicit FORCE re-applies and runs hooks.
        (should (omnidiff-apply-theme nil t))
        (should (= (omnidiff-theme-revision) (1+ rev-after-first)))
        (should (= hook-calls 2))))))

(ert-deftest omnidiff-theme-test/preserves-user-and-third-party-theme-overrides ()
  "`face-defface-spec' updates uncustomized faces without clobbering overrides."
  (omnidiff-theme-test--with-clean-state
    (let ((dummy-theme 'omnidiff-theme-test--third-party)
          (orig-user-settings (copy-sequence (get 'user 'theme-settings)))
          (orig-user-face-prop (copy-sequence (get 'omnidiff-token-insert 'theme-face))))
      (unwind-protect
          (cl-letf (((symbol-function 'omnidiff-theme--active-modus-theme) (lambda () nil))
                    ((symbol-function 'display-color-cells) (lambda (&optional _disp) 16777216)))
            ;; Third-party theme customizes `omnidiff-token-delete'.
            (custom-declare-theme dummy-theme nil)
            (let ((custom--inhibit-theme-enable t))
              (custom-theme-set-faces
               dummy-theme
               '(omnidiff-token-delete ((t :background "#440022")))))
            (enable-theme dummy-theme)
            ;; User customizes `omnidiff-token-insert' via `'user' theme (`custom-set-faces').
            (custom-theme-set-faces
             'user
             '(omnidiff-token-insert ((t :background "#123456"))))
            ;; Apply omnidiff theme with custom UI height on uncustomized `omnidiff-ui'.
            (setq omnidiff-ui-font-height 0.9)
            (should (omnidiff-apply-theme nil t))
            ;; 1. Third-party theme's `theme-settings' is NEVER polluted by omnidiff.
            (let ((settings (get dummy-theme 'theme-settings)))
              (should (= (length settings) 1))
              (should (eq (nth 1 (car settings)) 'omnidiff-token-delete)))
            ;; 2. User override (`custom-set-faces') is strictly preserved.
            (should (equal (face-attribute 'omnidiff-token-insert :background nil)
                           "#123456"))
            (should (assq 'user (get 'omnidiff-token-insert 'theme-face)))
            ;; 3. Third-party theme override is strictly preserved.
            (should (equal (face-attribute 'omnidiff-token-delete :background nil)
                           "#440022"))
            (should (assq dummy-theme (get 'omnidiff-token-delete 'theme-face)))
            ;; 4. Uncustomized faces receive omnidiff's `face-defface-spec' cleanly.
            (should-not (get 'omnidiff-token-update 'theme-face))
            (should (eql (face-attribute 'omnidiff-ui :height nil) 0.9))
            (let* ((spec (get 'omnidiff-token-update 'face-defface-spec))
                   (rich-attrs (omnidiff-theme-test--clause-attrs
                                spec omnidiff-theme-test--rich-display))
                   (fb-attrs (omnidiff-theme-test--clause-attrs spec t)))
              (should (= (length spec) 2))
              (should (omnidiff-theme--parse-hex (plist-get rich-attrs :background)))
              (should fb-attrs)
              (should-not (omnidiff-theme--parse-hex (plist-get fb-attrs :background)))
              (dolist (clause spec)
                (should-not (eq (plist-get (cdr clause) :extend) t))))
            ;; 5. Disabling third-party theme restores `omnidiff-token-delete' to
            ;;    defface spec while keeping the `'user' override intact.
            (disable-theme dummy-theme)
            (should-not (assq dummy-theme (get 'omnidiff-token-delete 'theme-face)))
            (should (equal (face-attribute 'omnidiff-token-insert :background nil)
                           "#123456")))
        (when (memq dummy-theme custom-enabled-themes)
          (disable-theme dummy-theme))
        (setq custom-known-themes (delq dummy-theme custom-known-themes))
        (setplist dummy-theme nil)
        (put 'user 'theme-settings orig-user-settings)
        (put 'omnidiff-token-insert 'theme-face orig-user-face-prop)
        (custom-theme-recalc-face 'omnidiff-token-insert)))))

(ert-deftest omnidiff-theme-test/daemon-low-color-emits-dual-branches-and-defers ()
  "Low-color/daemon startup emits dark+light specs and installs one-shot frame hook."
  (omnidiff-theme-test--with-clean-state
    (let ((dummy-theme 'omnidiff-theme-test--daemon-theme)
          (cells 8))
      (unwind-protect
          (cl-letf (((symbol-function 'omnidiff-theme--active-modus-theme) (lambda () nil))
                    ((symbol-function 'display-color-cells) (lambda (&optional _disp) cells)))
            (custom-declare-theme dummy-theme nil)
            (let ((custom-enabled-themes (list dummy-theme)))
              ;; Low-color startup (< 88 colors) emits both dark and light branches
              ;; and registers the one-shot `after-make-frame-functions' hook.
              (should (omnidiff-apply-theme nil t))
              (should (memq #'omnidiff-theme--on-first-rich-frame
                            after-make-frame-functions))
              (let ((spec (get 'omnidiff-token-insert 'face-defface-spec)))
                (should (= (length spec) 3))
                (should (omnidiff-theme-test--clause-attrs
                         spec omnidiff-theme-test--dark-display))
                (should (omnidiff-theme-test--clause-attrs
                         spec omnidiff-theme-test--light-display))
                (should (omnidiff-theme-test--clause-attrs spec t)))
              ;; Another low-color frame must NOT trigger reapply or remove the hook.
              (let ((rev-before (omnidiff-theme-revision)))
                (setq cells 16)
                (omnidiff-theme--on-first-rich-frame (selected-frame))
                (should (= (omnidiff-theme-revision) rev-before))
                (should (memq #'omnidiff-theme--on-first-rich-frame
                              after-make-frame-functions)))
              ;; First >=88-color frame re-applies theme, collapses to 2 clauses,
              ;; and removes the one-shot hook.
              (let ((rev-before (omnidiff-theme-revision)))
                (setq cells 16777216)
                (omnidiff-theme--on-first-rich-frame (selected-frame))
                (should (> (omnidiff-theme-revision) rev-before))
                (should-not (memq #'omnidiff-theme--on-first-rich-frame
                                  after-make-frame-functions))
                (let ((spec (get 'omnidiff-token-insert 'face-defface-spec)))
                  (should (= (length spec) 2))
                  (should (omnidiff-theme-test--clause-attrs
                           spec omnidiff-theme-test--rich-display))
                  (should (omnidiff-theme-test--clause-attrs spec t))))))
        (setq custom-known-themes (delq dummy-theme custom-known-themes))
        (setplist dummy-theme nil)))))

(ert-deftest omnidiff-theme-test/semantic-colors-revision-cache-and-fresh-copy ()
  "`omnidiff-theme-semantic-colors' caches by revision and returns fresh copies."
  (omnidiff-theme-test--with-clean-state
    (let ((resolve-calls 0)
          (orig-resolve (symbol-function 'omnidiff-theme--resolve-palette)))
      (omnidiff-apply-theme nil t)
      (cl-letf (((symbol-function 'omnidiff-theme--resolve-palette)
                 (lambda (base scheme)
                   (setq resolve-calls (1+ resolve-calls))
                   (funcall orig-resolve base scheme))))
        (let ((c1 (omnidiff-theme-semantic-colors))
              (c2 (omnidiff-theme-semantic-colors)))
          ;; First call populates cache; second call hits cache.
          (should (= resolve-calls 1))
          (should (equal c1 c2))
          (should-not (eq c1 c2))
          ;; Caller mutation must not corrupt cached plist.
          (let ((orig-ins (plist-get c1 :insert-bg)))
            (plist-put c1 :insert-bg "#000000")
            (let ((c3 (omnidiff-theme-semantic-colors)))
              (should (= resolve-calls 1))
              (should (equal (plist-get c3 :insert-bg) orig-ins)))))
        ;; Bumping theme revision invalidates cache so next call recomputes once.
        (should (omnidiff-apply-theme nil t))
        (setq resolve-calls 0)
        (let ((c4 (omnidiff-theme-semantic-colors))
              (c5 (omnidiff-theme-semantic-colors)))
          (should (plist-get c4 :insert-bg))
          (should (equal c4 c5))
          (should-not (eq c4 c5))
          (should (= resolve-calls 1)))))))

(provide 'omnidiff-theme-test)
;;; omnidiff-theme-test.el ends here
