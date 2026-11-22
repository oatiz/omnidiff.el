;;; omnidiff-theme.el --- Faces and theme integration for omnidiff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/omnidiff.el
;; Keywords: faces, vc, diff

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

;; Customization group, semantic faces, color blending, and Modus / generic
;; theme synchronization for `omnidiff-mode'.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)

(declare-function modus-themes-get-color-value "modus-themes" (color &optional overrides theme))
(declare-function modus-themes-get-current-theme "modus-themes" ())

;;; Customization

(defgroup omnidiff nil
  "Content-aware Git diffs rendered with `omnidiff'."
  :group 'diff
  :prefix "omnidiff-")

(defcustom omnidiff-color-scheme 'omni-semantic
  "Color palette style for `omnidiff-mode'.
- `omni-semantic': OmniDiff perceptual token tints with neutral grey moves.
- `calm-modus': Low-glare Modus-derived tints (sage, plum, ochre, indigo).
- `neutral-paper': Neutral gray code text with muted diff washes."
  :type '(choice (const :tag "Omni Semantic (token tints, grey moves)" omni-semantic)
                 (const :tag "Calm Modus (low-glare tints)" calm-modus)
                 (const :tag "Neutral Paper (muted gray text)" neutral-paper))
  :group 'omnidiff
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'omnidiff-theme)
           (omnidiff-apply-theme))))

(defcustom omnidiff-custom-palette nil
  "Alist of semantic color overrides applied over the resolved theme palette.
Each entry has the form `(KEY . HEX)' where KEY is one of `insert-bg',
`delete-bg', `update-bg', `move-bg', `cross-highlight-bg', `active-bg',
`overlay-fg', `gutter-insert', `gutter-delete', `gutter-update',
`gutter-move', `accent', or `border', and HEX is a color string (e.g.
\"#347059\") or nil to keep the theme-derived default."
  :type '(alist :key-type (choice (const :tag "Insert token background" insert-bg)
                                  (const :tag "Delete token background" delete-bg)
                                  (const :tag "Update token background" update-bg)
                                  (const :tag "Move token background" move-bg)
                                  (const :tag "Cross-highlight background" cross-highlight-bg)
                                  (const :tag "Active change background" active-bg)
                                  (const :tag "Overlay foreground (uniform mode)" overlay-fg)
                                  (const :tag "Insert gutter foreground" gutter-insert)
                                  (const :tag "Delete gutter foreground" gutter-delete)
                                  (const :tag "Update gutter foreground" gutter-update)
                                  (const :tag "Move gutter foreground" gutter-move)
                                  (const :tag "Primary accent" accent)
                                  (const :tag "Border color" border))
                :value-type (choice (const :tag "Use theme default" nil)
                                    (string :tag "Hex color (#rrggbb)")))
  :group 'omnidiff
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'omnidiff-theme)
           (omnidiff-apply-theme))))

(defcustom omnidiff-dual-overlay-foreground 'preserve-syntax
  "Foreground styling strategy inside semantic token diff highlights.
- `preserve-syntax': Keep native `font-lock' syntax foreground colors.
- `uniform': Apply a uniform high-contrast foreground color over tokens."
  :type '(choice (const :tag "Preserve syntax foreground" preserve-syntax)
                 (const :tag "Uniform high-contrast foreground" uniform))
  :group 'omnidiff
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'omnidiff-theme)
           (omnidiff-apply-theme))))

(defcustom omnidiff-ui-font-height nil
  "Optional height applied to the base `omnidiff-ui' chrome face.
When nil, chrome inherits the default font height.  Otherwise specify a
floating-point relative scale (e.g. 0.9) or an integer in 1/10pt units."
  :type '(choice (const :tag "Inherit default" nil)
                 (float :tag "Relative scale")
                 (integer :tag "Absolute 1/10pt"))
  :group 'omnidiff
  :set (lambda (sym val)
         (set-default sym val)
         (when (featurep 'omnidiff-theme)
           (omnidiff-apply-theme))))

;;; Faces

(defface omnidiff-ui
  '((t nil))
  "Base chrome face inherited by `omnidiff-mode' headers, gutters, and badges."
  :group 'omnidiff)

(defface omnidiff-header-line
  '((t :inherit omnidiff-ui))
  "Header line face for `omnidiff-mode'."
  :group 'omnidiff)

(defface omnidiff-header-badge
  '((t :inherit omnidiff-ui :weight bold))
  "Accent badge face in the `omnidiff-mode' header line."
  :group 'omnidiff)

(defface omnidiff-header-file
  '((t :inherit omnidiff-ui :weight semi-bold))
  "Current visible file name in the fixed header line."
  :group 'omnidiff)

(defface omnidiff-file-header
  '((t :inherit omnidiff-ui :weight bold :extend t))
  "File card header face in `omnidiff-mode'."
  :group 'omnidiff)

(defface omnidiff-file-accent
  '((t :inherit omnidiff-file-header))
  "Leading accent bar on file cards."
  :group 'omnidiff)

(defface omnidiff-stat-added
  '((t :inherit omnidiff-file-header))
  "Added count in file card header."
  :group 'omnidiff)

(defface omnidiff-stat-removed
  '((t :inherit omnidiff-file-header))
  "Removed count in file card header."
  :group 'omnidiff)

(defface omnidiff-stat-changed
  '((t :inherit omnidiff-file-header))
  "Updated count in file card header."
  :group 'omnidiff)

(defface omnidiff-stat-moved
  '((t :inherit omnidiff-file-header))
  "Moved count in file card header."
  :group 'omnidiff)

(defface omnidiff-stat-index
  '((t :inherit omnidiff-file-header :weight normal))
  "File index counter in file card header."
  :group 'omnidiff)

(defface omnidiff-file-status-added
  '((t :inherit omnidiff-file-header))
  "Status badge for newly added files."
  :group 'omnidiff)

(defface omnidiff-file-status-deleted
  '((t :inherit omnidiff-file-header))
  "Status badge for deleted files."
  :group 'omnidiff)

(defface omnidiff-file-status-renamed
  '((t :inherit omnidiff-file-header))
  "Status badge for renamed files."
  :group 'omnidiff)

(defface omnidiff-hunk-header
  '((t :inherit omnidiff-ui :weight semi-bold :extend t))
  "Hunk header strip face in `omnidiff-mode'."
  :group 'omnidiff)

(defface omnidiff-hunk-accent
  '((t :inherit omnidiff-hunk-header :weight bold))
  "Disclosure indicator face on `omnidiff-mode' hunk headers."
  :group 'omnidiff)

(defface omnidiff-hunk-meta
  '((t :inherit omnidiff-hunk-header :weight normal))
  "Secondary metadata (unchanged count, hunk index) on hunk headers."
  :group 'omnidiff)

(defface omnidiff-hunk-stat-added
  '((t :inherit omnidiff-hunk-header))
  "Added line count face on hunk headers."
  :group 'omnidiff)

(defface omnidiff-hunk-stat-removed
  '((t :inherit omnidiff-hunk-header))
  "Removed line count face on hunk headers."
  :group 'omnidiff)

(defface omnidiff-hunk-stat-changed
  '((t :inherit omnidiff-hunk-header))
  "Changed line count face on hunk headers."
  :group 'omnidiff)

(defface omnidiff-hunk-stat-moved
  '((t :inherit omnidiff-hunk-header))
  "Moved line count face on hunk headers."
  :group 'omnidiff)

(defface omnidiff-side-bar
  '((t :inherit omnidiff-ui :extend t))
  "Full-width subtle strip behind BEFORE/AFTER labels."
  :group 'omnidiff)

(defface omnidiff-side-before
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#f5e3e7" :foreground "#8a2b3f" :weight semi-bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#3a2129" :foreground "#e09eb0" :weight semi-bold)
    (t :inherit (omnidiff-ui diff-removed) :weight semi-bold))
  "BEFORE sub-header pill face."
  :group 'omnidiff)

(defface omnidiff-side-after
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#e1f0e6" :foreground "#23623d" :weight semi-bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#1f3529" :foreground "#96ccab" :weight semi-bold)
    (t :inherit (omnidiff-ui diff-added) :weight semi-bold))
  "AFTER sub-header pill face."
  :group 'omnidiff)

(defface omnidiff-gap
  '((t :inherit omnidiff-ui :slant italic :extend t))
  "Collapsed unchanged-lines band face."
  :group 'omnidiff)

(defface omnidiff-scope
  '((t :slant italic :extend t))
  "Enclosing scope (`@') anchor line face."
  :group 'omnidiff)

(defface omnidiff-lineno
  '((t :inherit (omnidiff-ui shadow)))
  "Line number gutter face for context lines."
  :group 'omnidiff)

(defface omnidiff-lineno-added
  '((t :inherit omnidiff-lineno))
  "Line number gutter face for added lines."
  :group 'omnidiff)

(defface omnidiff-lineno-removed
  '((t :inherit omnidiff-lineno))
  "Line number gutter face for removed lines."
  :group 'omnidiff)

(defface omnidiff-lineno-changed
  '((t :inherit omnidiff-lineno))
  "Line number gutter face for updated lines."
  :group 'omnidiff)

(defface omnidiff-lineno-moved
  '((t :inherit omnidiff-lineno))
  "Line number gutter face for moved lines."
  :group 'omnidiff)

(defface omnidiff-moved-indicator
  '((((class color grayscale) (min-colors 88) (background light))
     :foreground "medium purple")
    (((class color grayscale) (min-colors 88) (background dark))
     :foreground "light steel blue")
    (t :foreground "grey50"))
  "Foreground-only face for moved rail markers and `→ N' move references."
  :group 'omnidiff)

(defface omnidiff-added
  '((t :inherit diff-added :extend t))
  "Full-line face for inserted lines."
  :group 'omnidiff)

(defface omnidiff-added-refine
  '((t :inherit diff-refine-added :weight medium))
  "Inline token face for inserted spans."
  :group 'omnidiff)

(defface omnidiff-removed
  '((t :inherit diff-removed :extend t))
  "Full-line face for deleted lines."
  :group 'omnidiff)

(defface omnidiff-removed-refine
  '((t :inherit diff-refine-removed :weight medium))
  "Inline token face for deleted spans."
  :group 'omnidiff)

(defface omnidiff-changed
  '((t :inherit diff-changed :extend t))
  "Full-line face for syntax-updated lines."
  :group 'omnidiff)

(defface omnidiff-changed-refine
  '((t :inherit diff-refine-changed :weight bold))
  "Inline token face for exact syntax-updated spans."
  :group 'omnidiff)

(defface omnidiff-moved
  '((t :extend t))
  "Full-line face for moved blocks."
  :group 'omnidiff)

(defface omnidiff-moved-header
  '((t :inherit omnidiff-ui :slant italic :extend t))
  "Banner face for `Moved to/from line N' annotations."
  :group 'omnidiff)

(defface omnidiff-commit-hash
  '((t :weight bold))
  "Commit hash face in `git show' preambles."
  :group 'omnidiff)

(defface omnidiff-commit-meta
  '((t :inherit shadow))
  "Author/Date metadata face in `git show' preambles."
  :group 'omnidiff)

(defface omnidiff-image-bar
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#f2f3f6" :foreground "#595e6a" :extend t)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#1f222a" :foreground "#9aa0ae" :extend t)
    (t :inherit (omnidiff-ui header-line) :extend t))
  "Subtle control strip behind image diff mode pills."
  :group 'omnidiff)

(defface omnidiff-image-mode-active
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#dde4f2" :foreground "#2454a8" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#2e3647" :foreground "#82b4ff" :weight bold)
    (t :inherit (omnidiff-ui highlight) :weight bold))
  "Selected visual diff mode pill on an image card toolbar."
  :group 'omnidiff)

(defface omnidiff-image-mode-inactive
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#f2f3f6" :foreground "#595e6a")
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#1f222a" :foreground "#9aa0ae")
    (t :inherit (omnidiff-ui shadow)))
  "Unselected visual diff mode pill on an image card toolbar."
  :group 'omnidiff)

(defface omnidiff-image-meta
  '((t :inherit (omnidiff-ui shadow)))
  "Image dimensions, format, and byte-size summary face."
  :group 'omnidiff)

(defface omnidiff-image-stat-delta
  '((t :inherit omnidiff-stat-changed))
  "Changed-pixel percentage badge on image file card headers."
  :group 'omnidiff)

(defface omnidiff-image-divider
  '((t :inherit omnidiff-file-accent :weight bold))
  "Swipe curtain vertical divider face."
  :group 'omnidiff)

(defface omnidiff-image-frame
  '((((class color) (min-colors 88) (background light))
     :foreground "#c8cbd4")
    (((class color) (min-colors 88) (background dark))
     :foreground "#3d4252")
    (t :inherit shadow))
  "Subtle border color framing inline image previews and 1:1 region crops."
  :group 'omnidiff)

(defface omnidiff-image-diff-highlight
  '((((class color) (min-colors 88) (background light))
     :foreground "#c82d63")
    (((class color) (min-colors 88) (background dark))
     :foreground "#e65c8e")
    (t :foreground "magenta"))
  "Accent color for pixel-diff highlights and changed-region boxes."
  :group 'omnidiff)

;;; Semantic token faces (no `:extend t' -- wrap only changed token spans)

(defface omnidiff-token-insert
  '((((class color) (min-colors 88) (background light))
     :background "#c3decb")
    (((class color) (min-colors 88) (background dark))
     :background "#336c57")
    (t :inherit diff-refine-added))
  "Inline semantic token highlight for inserted spans."
  :group 'omnidiff)

(defface omnidiff-token-delete
  '((((class color) (min-colors 88) (background light))
     :background "#f3c6c9")
    (((class color) (min-colors 88) (background dark))
     :background "#7a3f4d")
    (t :inherit diff-refine-removed))
  "Inline semantic token highlight for deleted spans."
  :group 'omnidiff)

(defface omnidiff-token-update
  '((((class color) (min-colors 88) (background light))
     :background "#e5d7bd")
    (((class color) (min-colors 88) (background dark))
     :background "#72624e")
    (t :inherit diff-refine-changed))
  "Inline semantic token highlight for updated spans."
  :group 'omnidiff)

(defface omnidiff-token-move
  '((((class color) (min-colors 88) (background light))
     :background "#d9dcde")
    (((class color) (min-colors 88) (background dark))
     :background "#525560")
    (t :underline t))
  "Inline semantic token highlight for moved spans (neutral grey)."
  :group 'omnidiff)

(defface omnidiff-token-cross-highlight
  '((((class color) (min-colors 88) (background light))
     :background "#70a8ea")
    (((class color) (min-colors 88) (background dark))
     :background "#4878a6")
    (t :inherit highlight))
  "Vivid counterpart highlight linking active changes and move targets."
  :group 'omnidiff)

(defface omnidiff-token-active
  '((((class color) (min-colors 88) (background light))
     :background "#d6e6fb" :box (:line-width -1 :color "#0969da"))
    (((class color) (min-colors 88) (background dark))
     :background "#354a66" :box (:line-width -1 :color "#61afef"))
    (t :box (:line-width -1)))
  "Focused change accent for the current navigation stop."
  :group 'omnidiff)

;;; Dual viewer chrome & gutter faces

(defface omnidiff-dual-header-before
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#f5e3e7" :foreground "#cf222e"
     :weight semi-bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#3a2129" :foreground "#f06875"
     :weight semi-bold)
    (t :inherit omnidiff-side-before))
  "Header face for the BEFORE pane in the dual semantic viewer."
  :group 'omnidiff)

(defface omnidiff-dual-header-after
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#e1f0e6" :foreground "#1a7f37"
     :weight semi-bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#1f3529" :foreground "#50d890"
     :weight semi-bold)
    (t :inherit omnidiff-side-after))
  "Header face for the AFTER pane in the dual semantic viewer."
  :group 'omnidiff)

(defface omnidiff-dual-header-meta
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#f4f4f4" :foreground "#595e6a")
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#262636" :foreground "#9aa0ae")
    (t :inherit (omnidiff-ui shadow)))
  "Secondary metadata face in dual viewer pane headers."
  :group 'omnidiff)

(defface omnidiff-dual-header-before-inactive
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#f4f4f4" :foreground "#8a2b3f")
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#262636" :foreground "#c49aa6")
    (t :inherit omnidiff-dual-header-meta))
  "Inactive BEFORE pill face when the opposite dual pane has focus."
  :group 'omnidiff)

(defface omnidiff-dual-header-after-inactive
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#f4f4f4" :foreground "#23623d")
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#262636" :foreground "#91b8a2")
    (t :inherit omnidiff-dual-header-meta))
  "Inactive AFTER pill face when the opposite dual pane has focus."
  :group 'omnidiff)

(defface omnidiff-dual-header-file
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#f4f4f4" :foreground "#1f2328" :weight semi-bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#262636" :foreground "#e2e4ec" :weight semi-bold)
    (t :inherit omnidiff-ui :weight semi-bold))
  "Focused filename label face in dual viewer pane headers."
  :group 'omnidiff)

(defface omnidiff-dual-header-file-inactive
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#f4f4f4" :foreground "#595e6a")
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#262636" :foreground "#9aa0ae")
    (t :inherit (omnidiff-ui shadow)))
  "Unfocused filename label face in dual viewer pane headers."
  :group 'omnidiff)

(defface omnidiff-dual-header-badge
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#eef1f6" :foreground "#0969da" :weight semi-bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#323241" :foreground "#61afef" :weight semi-bold)
    (t :inherit omnidiff-summary-badge))
  "Inline badge face inside dual viewer pane headers with continuous underline."
  :group 'omnidiff)

(defface omnidiff-dual-fold-banner
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#f4f4f4" :foreground "#595e6a"
     :slant italic :overline "#d2d3d4" :underline "#d2d3d4" :extend t)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#262636" :foreground "#898b97"
     :slant italic :overline "#3b3c4a" :underline "#3b3c4a" :extend t)
    (t :inherit omnidiff-gap :extend t))
  "Full-width fold banner separating unchanged regions in the dual viewer."
  :group 'omnidiff)

(defface omnidiff-dual-gutter-insert
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :foreground "#1a7f37" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :foreground "#50d890" :weight bold)
    (t :inherit (omnidiff-ui success) :weight bold))
  "Gutter marker face for inserted lines in the dual viewer."
  :group 'omnidiff)

(defface omnidiff-dual-gutter-delete
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :foreground "#cf222e" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :foreground "#f06875" :weight bold)
    (t :inherit (omnidiff-ui error) :weight bold))
  "Gutter marker face for deleted lines in the dual viewer."
  :group 'omnidiff)

(defface omnidiff-dual-gutter-update
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :foreground "#9a6700" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :foreground "#e5c07b" :weight bold)
    (t :inherit (omnidiff-ui warning) :weight bold))
  "Gutter marker face for updated lines in the dual viewer."
  :group 'omnidiff)

(defface omnidiff-dual-gutter-move
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :foreground "#6e7781" :weight bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :foreground "#9aa0a6" :weight bold)
    (t :inherit (omnidiff-ui shadow) :weight bold))
  "Gutter marker face for moved lines in the dual viewer."
  :group 'omnidiff)

;;; Semantic summary & container faces

(defface omnidiff-summary-badge
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :background "#eef1f6" :foreground "#0969da"
     :weight semi-bold :box (:line-width -1 :color "#d2d3d4"))
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :background "#323241" :foreground "#61afef"
     :weight semi-bold :box (:line-width -1 :color "#4d4e5b"))
    (t :inherit omnidiff-header-badge))
  "Badge face for semantic change summaries."
  :group 'omnidiff)

(defface omnidiff-container-member-changed
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :foreground "#9a6700" :weight semi-bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :foreground "#e5c07b" :weight semi-bold)
    (t :inherit omnidiff-stat-changed))
  "Face for changed members in semantic container summaries."
  :group 'omnidiff)

(defface omnidiff-container-member-added
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :foreground "#1a7f37" :weight semi-bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :foreground "#50d890" :weight semi-bold)
    (t :inherit omnidiff-stat-added))
  "Face for added members in semantic container summaries."
  :group 'omnidiff)

(defface omnidiff-container-member-removed
  '((((class color) (min-colors 88) (background light))
     :inherit omnidiff-ui :foreground "#cf222e" :weight semi-bold)
    (((class color) (min-colors 88) (background dark))
     :inherit omnidiff-ui :foreground "#f06875" :weight semi-bold)
    (t :inherit omnidiff-stat-removed))
  "Face for removed members in semantic container summaries."
  :group 'omnidiff)

;;; Color utilities

(defun omnidiff-theme--parse-hex (hex)
  "Parse \"#rrggbb\" HEX into a `(R G B)' integer list, or nil."
  (when (and (stringp hex)
             (string-match
              "\\`#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\'"
              hex))
    (list (string-to-number (match-string 1 hex) 16)
          (string-to-number (match-string 2 hex) 16)
          (string-to-number (match-string 3 hex) 16))))

(defun omnidiff-theme-color-hex (color)
  "Convert Emacs color name or hex string COLOR to \"#rrggbb\", or nil."
  (when (and (stringp color)
             (not (string-empty-p color))
             (not (string= color "unspecified"))
             (not (string-prefix-p "unspecified-" color)))
    (if (omnidiff-theme--parse-hex color)
        (downcase color)
      (when-let* ((rgb (ignore-errors (color-name-to-rgb color))))
        (apply #'format "#%02x%02x%02x"
               (mapcar (lambda (c)
                         (round (* 255.0 (max 0.0 (min 1.0 c)))))
                       rgb))))))

(defun omnidiff-theme--blend-hex (c1 c2 ratio)
  "Blend \"#rrggbb\" C1 toward C2 by RATIO (0.0..1.0) without TTY quantization."
  (let ((rgb1 (omnidiff-theme--parse-hex c1))
        (rgb2 (omnidiff-theme--parse-hex c2)))
    (if (and rgb1 rgb2)
        (apply #'format "#%02x%02x%02x"
               (cl-mapcar (lambda (a b)
                            (round (+ (* (- 1.0 ratio) a) (* ratio b))))
                          rgb1 rgb2))
      c1)))

(defun omnidiff-theme--dark-color-p (hex)
  "Return non-nil when HEX is a dark color (Rec. 709 luma below 128)."
  (pcase (omnidiff-theme--parse-hex hex)
    (`(,r ,g ,b) (< (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)) 128.0))
    (_ (eq (frame-parameter nil 'background-mode) 'dark))))

;;; Palette resolution

(defun omnidiff-theme--color-distance (c1 c2)
  "Return Manhattan RGB distance between \"#rrggbb\" C1 and C2, or nil."
  (let ((rgb1 (omnidiff-theme--parse-hex c1))
        (rgb2 (omnidiff-theme--parse-hex c2)))
    (when (and rgb1 rgb2)
      (+ (abs (- (nth 0 rgb1) (nth 0 rgb2)))
         (abs (- (nth 1 rgb1) (nth 1 rgb2)))
         (abs (- (nth 2 rgb1) (nth 2 rgb2)))))))

(defun omnidiff-theme--color-chroma (hex)
  "Return RGB channel spread (max - min) for \"#rrggbb\" HEX, or 0."
  (pcase (omnidiff-theme--parse-hex hex)
    (`(,r ,g ,b) (- (max r g b) (min r g b)))
    (_ 0)))

(defun omnidiff-theme--face-hex (face attr)
  "Return ATTR (`:foreground' or `:background') of FACE as \"#rrggbb\", or nil."
  (when (facep face)
    (omnidiff-theme-color-hex
     (pcase attr
       (:foreground (face-foreground face nil t))
       (:background (face-background face nil t))
       (_ (face-attribute face attr nil t))))))

(defun omnidiff-theme--custom-color (key fallback)
  "Return hex override for KEY in `omnidiff-custom-palette', or FALLBACK."
  (or (when-let* ((cell (assq key omnidiff-custom-palette))
                  (val (cdr cell)))
        (omnidiff-theme-color-hex val))
      fallback))

(cl-defstruct (omnidiff-theme--base (:copier nil))
  "Raw theme colors resolved from Modus or generic Emacs faces."
  bg-main bg-dim bg-inactive bg-active bg-card bg-chrome border
  accent dark-p fg-main fg-dim
  bg-added-faint bg-added-refine fg-added
  bg-removed-faint bg-removed-refine fg-removed
  bg-changed-faint bg-changed-refine fg-changed
  blue-cooler green-faint red-faint yellow-faint indigo
  bg-green-nuanced bg-sage bg-ochre fg-ochre bg-lavender raw-moved
  accent-green accent-red accent-yellow accent-cyan accent-grey)

(cl-defstruct (omnidiff-theme--palette (:copier nil))
  "Resolved semantic surface and foreground colors for `omnidiff-mode'."
  bg-main bg-active bg-card bg-chrome bg-hunk border hunk-border
  accent hunk-accent fg-main fg-dim blue-cooler
  add-bg add-fg add-ref-bg add-gut-fg
  rem-bg rem-fg rem-ref-bg rem-gut-fg
  chg-bg chg-fg chg-ref-bg chg-gut-fg
  mov-bg mov-fg mov-gut-fg mov-hdr-fg
  ref-fg ref-weight
  side-before-bg side-after-bg
  stat-add-fg stat-rem-fg stat-chg-fg stat-mov-fg
  token-insert-bg token-delete-bg token-update-bg token-move-bg
  token-cross-bg token-active-bg token-active-border token-overlay-fg
  dual-gutter-insert dual-gutter-delete dual-gutter-update dual-gutter-move)

(defun omnidiff-theme--modus-base ()
  "Resolve `omnidiff-theme--base' from the active Modus theme."
  (cl-flet ((c (name &optional fallback)
              (or (omnidiff-theme-color-hex (modus-themes-get-color-value name t))
                  fallback)))
    (let* ((frame-dark-p (eq (frame-parameter nil 'background-mode) 'dark))
           (bg-main (c 'bg-main (if frame-dark-p "#1e1e2e" "#ffffff")))
           (dark-p (omnidiff-theme--dark-color-p bg-main))
           (fg-main (c 'fg-main (if dark-p "#e2e4ec" "#1f2328")))
           (bg-dim (c 'bg-dim (omnidiff-theme--blend-hex bg-main fg-main 0.06)))
           (bg-inactive (c 'bg-inactive (omnidiff-theme--blend-hex bg-main fg-main 0.08)))
           (bg-tab-bar (c 'bg-tab-bar bg-dim))
           (bg-tab-current (c 'bg-tab-current bg-inactive))
           (bg-added-faint (c 'bg-added-faint (if dark-p "#0c2b1f" "#e0f6e0")))
           (bg-added-refine (c 'bg-added-refine (if dark-p "#1a4834" "#bae0ba")))
           (fg-added (c 'fg-added (if dark-p "#50d890" "#1a7f37")))
           (bg-removed-faint (c 'bg-removed-faint (if dark-p "#2f171d" "#ffe8e8")))
           (bg-removed-refine (c 'bg-removed-refine (if dark-p "#592832" "#f5c2c2")))
           (fg-removed (c 'fg-removed (if dark-p "#f06875" "#cf222e")))
           (bg-changed-faint (c 'bg-changed-faint (if dark-p "#2b2414" "#fff3d0")))
           (bg-changed-refine (c 'bg-changed-refine (if dark-p "#4f4222" "#ebd080")))
           (fg-changed (c 'fg-changed (if dark-p "#e5c07b" "#9a6700")))
           (yellow-faint (c 'yellow-faint fg-changed))
           (blue-cooler (c 'blue-cooler (if dark-p "#61afef" "#0969da")))
           (bg-blue-nuanced (c 'bg-blue-nuanced "#12154a")))
      (make-omnidiff-theme--base
       :bg-main bg-main
       :bg-dim bg-dim
       :bg-inactive bg-inactive
       :bg-active (c 'bg-active (omnidiff-theme--blend-hex bg-main fg-main 0.14))
       :bg-card (if (equal bg-tab-current bg-main) bg-inactive bg-tab-current)
       :bg-chrome (if (equal bg-tab-current bg-main) bg-dim bg-tab-bar)
       :border (c 'border (omnidiff-theme--blend-hex bg-main fg-main 0.22))
       :accent (c 'blue-warmer blue-cooler)
       :dark-p dark-p
       :fg-main fg-main
       :fg-dim (c 'fg-dim (omnidiff-theme--blend-hex fg-main bg-main 0.45))
       :bg-added-faint bg-added-faint
       :bg-added-refine bg-added-refine
       :fg-added fg-added
       :bg-removed-faint bg-removed-faint
       :bg-removed-refine bg-removed-refine
       :fg-removed fg-removed
       :bg-changed-faint bg-changed-faint
       :bg-changed-refine bg-changed-refine
       :fg-changed fg-changed
       :blue-cooler blue-cooler
       :green-faint (c 'green-faint fg-added)
       :red-faint (c 'red-faint fg-removed)
       :yellow-faint yellow-faint
       :indigo (c 'indigo (c 'magenta-cooler (if dark-p "#b4a7f0" "#5c4bb8")))
       :bg-green-nuanced (c 'bg-green-nuanced bg-added-faint)
       :bg-sage (c 'bg-sage bg-added-refine)
       :bg-ochre (c 'bg-ochre bg-changed-refine)
       :fg-ochre (c 'fg-ochre yellow-faint)
       :bg-lavender (c 'bg-lavender bg-blue-nuanced)
       :raw-moved bg-blue-nuanced
       :accent-green (c 'green (c 'green-cooler fg-added))
       :accent-red (c 'red (c 'red-warmer fg-removed))
       :accent-yellow (c 'yellow (c 'yellow-warmer fg-changed))
       :accent-cyan (c 'cyan blue-cooler)
       :accent-grey (if dark-p "#9aa0a6" "#6e7781")))))

(defun omnidiff-theme--generic-base (&optional background-mode)
  "Resolve `omnidiff-theme--base' dynamically from active Emacs faces.
When BACKGROUND-MODE is `dark' or `light', resolve a base matching that
polarity.  On displays with fewer than 88 colors (including 8/16-color TTYs
and headless daemons), skips probing `color-name-to-rgb' on quantized live
faces and uses clean canonical dark/light hex fallbacks instead."
  (let* ((frame-mode (or (frame-parameter nil 'background-mode) 'light))
         (req-mode (or background-mode frame-mode))
         (req-dark-p (eq req-mode 'dark))
         (rich-color-p (>= (or (display-color-cells) 0) 88))
         (raw-bg (when (and rich-color-p
                            (or (null background-mode)
                                (eq background-mode frame-mode)))
                   (omnidiff-theme--face-hex 'default :background)))
         (probe-p (and rich-color-p
                       (or (null background-mode)
                           (and (eq background-mode frame-mode)
                                (or (null raw-bg)
                                    (eq (omnidiff-theme--dark-color-p raw-bg)
                                        req-dark-p))))))
         (bg-main (or (and probe-p raw-bg)
                      (if req-dark-p "#1e1e2e" "#ffffff")))
         (dark-p (if background-mode
                     req-dark-p
                   (omnidiff-theme--dark-color-p bg-main)))
         (fg-main (or (and probe-p (omnidiff-theme--face-hex 'default :foreground))
                      (if dark-p "#e2e4ec" "#1f2328")))
         (shadow-fg (when probe-p (omnidiff-theme--face-hex 'shadow :foreground)))
         (fg-dim (if (and shadow-fg
                          (>= (or (omnidiff-theme--color-distance shadow-fg bg-main) 0) 40)
                          (>= (or (omnidiff-theme--color-distance shadow-fg fg-main) 0) 25))
                     shadow-fg
                   (omnidiff-theme--blend-hex fg-main bg-main 0.45))))
    (cl-labels
        ((pick-surface (faces fallback)
           (or (when probe-p
                 (cl-loop for face in faces
                          for hex = (omnidiff-theme--face-hex face :background)
                          for dist = (and hex (omnidiff-theme--color-distance hex bg-main))
                          when (and hex
                                    (eq (omnidiff-theme--dark-color-p hex) dark-p)
                                    (>= dist 12)
                                    (<= dist 110))
                          return hex))
               fallback))
         (pick-accent (faces fallback &optional avoid)
           (or (when probe-p
                 (cl-loop for face in faces
                          for hex = (omnidiff-theme--face-hex face :foreground)
                          when (and hex
                                    (>= (omnidiff-theme--color-chroma hex) 30)
                                    (>= (or (omnidiff-theme--color-distance hex bg-main) 0) 60)
                                    (>= (or (omnidiff-theme--color-distance hex fg-main) 0) 35)
                                    (cl-every
                                     (lambda (other)
                                       (>= (or (omnidiff-theme--color-distance hex other) 0) 70))
                                     avoid))
                          return hex))
               fallback)))
      (let* ((bg-chrome (pick-surface
                         '(fringe)
                         (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.04 0.03))))
             (bg-dim (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.06 0.05)))
             (bg-inactive (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.08 0.08)))
             (bg-card (pick-surface
                       '(header-line mode-line-inactive mode-line)
                       (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.10 0.07))))
             (bg-active (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.15 0.13)))
             (border (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.24 0.20)))
             (accent (pick-accent '(link font-lock-keyword-face highlight)
                                  (if dark-p "#61afef" "#0969da")))
             (accent-green (pick-accent '(success diff-added)
                                        (if dark-p "#50d890" "#1a7f37")))
             (accent-red (pick-accent '(error diff-removed)
                                      (if dark-p "#f06875" "#cf222e")
                                      (list accent-green)))
             (accent-yellow (pick-accent '(warning diff-changed)
                                         (if dark-p "#e5c07b" "#9a6700")
                                         (list accent-green accent-red)))
             (accent-cyan (pick-accent '(font-lock-constant-face link)
                                       (if dark-p "#61afef" "#0969da")
                                       (list accent-green accent-red accent-yellow)))
             (accent-grey (if dark-p "#9aa0a6" "#6e7781"))
             (bg-added-faint (omnidiff-theme--blend-hex accent-green bg-main
                                                        (if dark-p 0.80 0.88)))
             (bg-added-refine (omnidiff-theme--blend-hex accent-green bg-main
                                                         (if dark-p 0.62 0.76)))
             (bg-removed-faint (omnidiff-theme--blend-hex accent-red bg-main
                                                          (if dark-p 0.80 0.88)))
             (bg-removed-refine (omnidiff-theme--blend-hex accent-red bg-main
                                                           (if dark-p 0.62 0.76)))
             (bg-changed-faint (omnidiff-theme--blend-hex accent-yellow bg-main
                                                          (if dark-p 0.80 0.88)))
             (bg-changed-refine (omnidiff-theme--blend-hex accent-yellow bg-main
                                                           (if dark-p 0.62 0.76)))
             (green-faint (omnidiff-theme--blend-hex accent-green fg-main
                                                     (if dark-p 0.25 0.10)))
             (red-faint (omnidiff-theme--blend-hex accent-red fg-main
                                                   (if dark-p 0.25 0.10)))
             (yellow-faint (omnidiff-theme--blend-hex accent-yellow fg-main
                                                      (if dark-p 0.25 0.10)))
             (indigo (if dark-p "#b4a7f0" "#5c4bb8"))
             (raw-moved (omnidiff-theme--blend-hex accent-grey bg-main
                                                   (if dark-p 0.82 0.90))))
        (make-omnidiff-theme--base
         :bg-main bg-main
         :bg-dim bg-dim
         :bg-inactive bg-inactive
         :bg-active bg-active
         :bg-card bg-card
         :bg-chrome bg-chrome
         :border border
         :accent accent
         :dark-p dark-p
         :fg-main fg-main
         :fg-dim fg-dim
         :bg-added-faint bg-added-faint
         :bg-added-refine bg-added-refine
         :fg-added accent-green
         :bg-removed-faint bg-removed-faint
         :bg-removed-refine bg-removed-refine
         :fg-removed accent-red
         :bg-changed-faint bg-changed-faint
         :bg-changed-refine bg-changed-refine
         :fg-changed accent-yellow
         :blue-cooler accent-cyan
         :green-faint green-faint
         :red-faint red-faint
         :yellow-faint yellow-faint
         :indigo indigo
         :bg-green-nuanced (omnidiff-theme--blend-hex accent-green bg-main
                                                      (if dark-p 0.78 0.90))
         :bg-sage bg-added-refine
         :bg-ochre bg-changed-refine
         :fg-ochre yellow-faint
         :bg-lavender (omnidiff-theme--blend-hex indigo bg-main (if dark-p 0.76 0.90))
         :raw-moved raw-moved
         :accent-green accent-green
         :accent-red accent-red
         :accent-yellow accent-yellow
         :accent-cyan accent-cyan
         :accent-grey accent-grey)))))

(defun omnidiff-theme--resolve-palette (base scheme)
  "Compute an `omnidiff-theme--palette' from BASE colors and color SCHEME."
  (let* ((bg-main (omnidiff-theme--base-bg-main base))
         (bg-dim (omnidiff-theme--base-bg-dim base))
         (bg-inactive (omnidiff-theme--base-bg-inactive base))
         (bg-active (omnidiff-theme--base-bg-active base))
         (border (omnidiff-theme--custom-color 'border (omnidiff-theme--base-border base)))
         (accent (omnidiff-theme--custom-color 'accent (omnidiff-theme--base-accent base)))
         (dark-p (omnidiff-theme--base-dark-p base))
         (fg-main (omnidiff-theme--base-fg-main base))
         (fg-dim (omnidiff-theme--base-fg-dim base))
         (bg-added-faint (omnidiff-theme--base-bg-added-faint base))
         (bg-added-refine (omnidiff-theme--base-bg-added-refine base))
         (fg-added (omnidiff-theme--base-fg-added base))
         (bg-removed-faint (omnidiff-theme--base-bg-removed-faint base))
         (bg-removed-refine (omnidiff-theme--base-bg-removed-refine base))
         (fg-removed (omnidiff-theme--base-fg-removed base))
         (bg-changed-faint (omnidiff-theme--base-bg-changed-faint base))
         (bg-changed-refine (omnidiff-theme--base-bg-changed-refine base))
         (fg-changed (omnidiff-theme--base-fg-changed base))
         (green-faint (omnidiff-theme--base-green-faint base))
         (red-faint (omnidiff-theme--base-red-faint base))
         (yellow-faint (omnidiff-theme--base-yellow-faint base))
         (indigo (omnidiff-theme--base-indigo base))
         (bg-green-nuanced (omnidiff-theme--base-bg-green-nuanced base))
         (bg-sage (omnidiff-theme--base-bg-sage base))
         (bg-ochre (omnidiff-theme--base-bg-ochre base))
         (fg-ochre (omnidiff-theme--base-fg-ochre base))
         (bg-lavender (omnidiff-theme--base-bg-lavender base))
         (raw-moved (omnidiff-theme--base-raw-moved base))
         (accent-green (or (omnidiff-theme--base-accent-green base)
                           fg-added
                           (if dark-p "#50d890" "#1a7f37")))
         (accent-red (or (omnidiff-theme--base-accent-red base)
                         fg-removed
                         (if dark-p "#f06875" "#cf222e")))
         (accent-yellow (or (omnidiff-theme--base-accent-yellow base)
                            fg-changed
                            (if dark-p "#e5c07b" "#9a6700")))
         (accent-cyan (or (omnidiff-theme--base-accent-cyan base)
                          (omnidiff-theme--base-blue-cooler base)
                          accent))
         (accent-grey (or (omnidiff-theme--base-accent-grey base)
                          (if dark-p "#9aa0a6" "#6e7781")))
         (fg-soft (omnidiff-theme--blend-hex fg-main fg-dim 0.16))
         (omni-p (eq scheme 'omni-semantic))
         (neutral-p (cl-ecase scheme
                      (omni-semantic nil)
                      (calm-modus nil)
                      (neutral-paper t)))
         (paper-p (and dark-p neutral-p))
         (light-paper-p (and (not dark-p) neutral-p))
         ;; Perceptual token blending: ~56-60% toward bg in dark, ~74% in light,
         ;; neutral grey for moves, vivid ~38-44% cyan/blue for cross-highlight.
         (band-ratio (cond (omni-p (if dark-p 0.56 0.74))
                           (neutral-p (if dark-p 0.64 0.80))
                           (t (if dark-p 0.58 0.75))))
         (move-ratio (cond (omni-p (if dark-p 0.60 0.76))
                           (neutral-p (if dark-p 0.66 0.82))
                           (t (if dark-p 0.62 0.78))))
         ;; Keep cross-highlight legible under `preserve-syntax' while an underline
         ;; supplies crisp counterpart prominence without swallowing syntax hues.
         (cross-ratio (cond (omni-p (if dark-p 0.56 0.68))
                            (neutral-p (if dark-p 0.60 0.74))
                            (t (if dark-p 0.58 0.70))))
         (token-insert-bg
          (omnidiff-theme--custom-color
           'insert-bg (omnidiff-theme--blend-hex accent-green bg-main band-ratio)))
         (token-delete-bg
          (omnidiff-theme--custom-color
           'delete-bg (omnidiff-theme--blend-hex accent-red bg-main band-ratio)))
         (token-update-bg
          (omnidiff-theme--custom-color
           'update-bg (omnidiff-theme--blend-hex accent-yellow bg-main band-ratio)))
         (token-move-bg
          (omnidiff-theme--custom-color
           'move-bg (omnidiff-theme--blend-hex accent-grey bg-main move-ratio)))
         (token-cross-bg
          (omnidiff-theme--custom-color
           'cross-highlight-bg
           (omnidiff-theme--blend-hex accent-cyan bg-main cross-ratio)))
         (token-active-bg
          (omnidiff-theme--custom-color
           'active-bg (omnidiff-theme--blend-hex accent bg-main (if dark-p 0.68 0.82))))
         (token-overlay-fg
          (omnidiff-theme--custom-color 'overlay-fg fg-main))
         (dual-gutter-insert
          (omnidiff-theme--custom-color
           'gutter-insert (if dark-p
                              (omnidiff-theme--blend-hex accent-green fg-main 0.15)
                            accent-green)))
         (dual-gutter-delete
          (omnidiff-theme--custom-color
           'gutter-delete (if dark-p
                              (omnidiff-theme--blend-hex accent-red fg-main 0.15)
                            accent-red)))
         (dual-gutter-update
          (omnidiff-theme--custom-color
           'gutter-update (if dark-p
                              (omnidiff-theme--blend-hex accent-yellow fg-main 0.12)
                            accent-yellow)))
         (dual-gutter-move
          (omnidiff-theme--custom-color 'gutter-move accent-grey)))
    (make-omnidiff-theme--palette
     :bg-main bg-main
     :bg-active bg-active
     :bg-card (omnidiff-theme--base-bg-card base)
     :bg-chrome (omnidiff-theme--base-bg-chrome base)
     :bg-hunk (if dark-p bg-inactive bg-dim)
     :border border
     :hunk-border (if dark-p (omnidiff-theme--blend-hex bg-active border 0.50) border)
     :accent accent
     :hunk-accent (if dark-p (omnidiff-theme--blend-hex accent fg-dim 0.25) accent)
     :fg-main fg-main
     :fg-dim fg-dim
     :blue-cooler (omnidiff-theme--base-blue-cooler base)
     :add-bg (cond (omni-p (omnidiff-theme--blend-hex accent-green bg-main
                                                      (if dark-p 0.88 0.93)))
                   (paper-p "#14221f")
                   (dark-p (omnidiff-theme--blend-hex bg-green-nuanced bg-main 0.38))
                   (light-paper-p (omnidiff-theme--blend-hex bg-added-faint bg-main 0.35))
                   (t bg-added-faint))
     :add-fg (cond (omni-p fg-main)
                   (paper-p "#c9cbd2")
                   (dark-p (omnidiff-theme--blend-hex fg-soft green-faint 0.45))
                   (light-paper-p fg-soft)
                   (t fg-added))
     :add-ref-bg (cond (omni-p token-insert-bg)
                       (paper-p "#284a3e")
                       (dark-p bg-sage)
                       (t bg-added-refine))
     :add-gut-fg (omnidiff-theme--custom-color
                  'gutter-insert
                  (cond (omni-p dual-gutter-insert)
                        (paper-p "#91b8a2")
                        (dark-p (omnidiff-theme--blend-hex green-faint fg-dim 0.30))
                        (t fg-added)))
     :rem-bg (cond (omni-p (omnidiff-theme--blend-hex accent-red bg-main
                                                      (if dark-p 0.88 0.93)))
                   (paper-p "#291b23")
                   (dark-p (omnidiff-theme--blend-hex bg-removed-faint bg-main 0.40))
                   (light-paper-p (omnidiff-theme--blend-hex bg-removed-faint bg-main 0.35))
                   (t bg-removed-faint))
     :rem-fg (cond (omni-p fg-main)
                   (paper-p "#c9cbd2")
                   (dark-p (omnidiff-theme--blend-hex fg-soft fg-removed 0.45))
                   (light-paper-p fg-soft)
                   (t fg-removed))
     :rem-ref-bg (cond (omni-p token-delete-bg)
                       (paper-p "#57323e")
                       (dark-p (omnidiff-theme--blend-hex bg-removed-refine bg-dim 0.42))
                       (t bg-removed-refine))
     :rem-gut-fg (omnidiff-theme--custom-color
                  'gutter-delete
                  (cond (omni-p dual-gutter-delete)
                        (paper-p "#c49aa6")
                        (dark-p (omnidiff-theme--blend-hex red-faint fg-dim 0.30))
                        (t fg-removed)))
     :chg-bg (cond (omni-p (omnidiff-theme--blend-hex accent-yellow bg-main
                                                      (if dark-p 0.88 0.93)))
                   (paper-p "#28231b")
                   (dark-p (omnidiff-theme--blend-hex bg-changed-faint bg-main 0.35))
                   (light-paper-p (omnidiff-theme--blend-hex bg-changed-faint bg-main 0.35))
                   (t bg-changed-faint))
     :chg-fg (cond (omni-p fg-main)
                   (paper-p "#c9cbd2")
                   (dark-p (omnidiff-theme--blend-hex fg-soft fg-ochre 0.55))
                   (light-paper-p fg-soft)
                   (t fg-changed))
     :chg-ref-bg (cond (omni-p token-update-bg)
                       (paper-p "#50452f")
                       (dark-p bg-ochre)
                       (t bg-changed-refine))
     :chg-gut-fg (omnidiff-theme--custom-color
                  'gutter-update
                  (cond (omni-p dual-gutter-update)
                        (paper-p "#c2ad84")
                        (dark-p (omnidiff-theme--blend-hex yellow-faint fg-dim 0.25))
                        (t fg-changed)))
     :mov-bg (cond (omni-p (omnidiff-theme--blend-hex accent-grey bg-main
                                                      (if dark-p 0.86 0.92)))
                   (paper-p "#101233")
                   (dark-p (omnidiff-theme--blend-hex bg-lavender bg-main 0.72))
                   (light-paper-p (omnidiff-theme--blend-hex raw-moved bg-main 0.35))
                   (t raw-moved))
     :mov-fg (cond (omni-p fg-soft)
                   (paper-p "#c9cbd2")
                   (dark-p (omnidiff-theme--blend-hex fg-main fg-dim 0.22))
                   (light-paper-p fg-soft)
                   (t fg-main))
     :mov-gut-fg (omnidiff-theme--custom-color
                  'gutter-move
                  (cond (omni-p dual-gutter-move)
                        (paper-p "#a69ac5")
                        (dark-p (omnidiff-theme--blend-hex indigo fg-dim 0.20))
                        (t indigo)))
     :mov-hdr-fg (cond (omni-p dual-gutter-move) (paper-p "#a69ac5") (t indigo))
     :ref-fg (cond (omni-p token-overlay-fg)
                   (paper-p "#dedee3")
                   (dark-p fg-soft)
                   (t fg-main))
     :ref-weight (if (or neutral-p omni-p) 'normal 'semi-bold)
     :side-before-bg (if dark-p
                         (omnidiff-theme--blend-hex bg-removed-faint bg-dim 0.35)
                       bg-removed-faint)
     :side-after-bg (if dark-p
                        (omnidiff-theme--blend-hex bg-green-nuanced bg-dim 0.35)
                      bg-added-faint)
     :stat-add-fg (if dark-p green-faint fg-added)
     :stat-rem-fg (if dark-p red-faint fg-removed)
     :stat-chg-fg (if dark-p yellow-faint fg-changed)
     :stat-mov-fg (cond (omni-p dual-gutter-move) (paper-p "#a69ac5") (t indigo))
     :token-insert-bg token-insert-bg
     :token-delete-bg token-delete-bg
     :token-update-bg token-update-bg
     :token-move-bg token-move-bg
     :token-cross-bg token-cross-bg
     :token-active-bg token-active-bg
     :token-active-border accent
     :token-overlay-fg token-overlay-fg
     :dual-gutter-insert dual-gutter-insert
     :dual-gutter-delete dual-gutter-delete
     :dual-gutter-update dual-gutter-update
     :dual-gutter-move dual-gutter-move)))

(defconst omnidiff-theme--fallback-specs
  '((omnidiff-header-line . (:inherit (omnidiff-ui header-line)))
    (omnidiff-header-badge . (:inherit (omnidiff-ui highlight) :weight bold))
    (omnidiff-header-file . (:inherit (omnidiff-ui header-line) :weight semi-bold))
    (omnidiff-file-header . (:inherit (omnidiff-ui header-line) :weight bold :extend t))
    (omnidiff-file-accent . (:inherit omnidiff-file-header))
    (omnidiff-stat-added . (:inherit (omnidiff-file-header success) :weight semi-bold))
    (omnidiff-stat-removed . (:inherit (omnidiff-file-header error) :weight semi-bold))
    (omnidiff-stat-changed . (:inherit (omnidiff-file-header warning) :weight semi-bold))
    (omnidiff-stat-moved . (:inherit (omnidiff-file-header shadow) :weight semi-bold))
    (omnidiff-stat-index . (:inherit (omnidiff-file-header shadow) :weight normal))
    (omnidiff-file-status-added . (:inherit (omnidiff-file-header diff-added)))
    (omnidiff-file-status-deleted . (:inherit (omnidiff-file-header diff-removed)))
    (omnidiff-file-status-renamed . (:inherit (omnidiff-file-header warning)))
    (omnidiff-hunk-header . (:inherit (omnidiff-ui header-line) :weight semi-bold :extend t))
    (omnidiff-hunk-accent . (:inherit omnidiff-hunk-header :weight bold))
    (omnidiff-hunk-meta . (:inherit (omnidiff-hunk-header shadow) :weight normal))
    (omnidiff-hunk-stat-added . (:inherit (omnidiff-hunk-header success) :weight semi-bold))
    (omnidiff-hunk-stat-removed . (:inherit (omnidiff-hunk-header error) :weight semi-bold))
    (omnidiff-hunk-stat-changed . (:inherit (omnidiff-hunk-header warning) :weight semi-bold))
    (omnidiff-hunk-stat-moved . (:inherit (omnidiff-hunk-header shadow) :weight semi-bold))
    (omnidiff-side-bar . (:inherit (omnidiff-ui shadow) :extend t))
    (omnidiff-side-before . (:inherit (omnidiff-ui diff-removed) :weight semi-bold))
    (omnidiff-side-after . (:inherit (omnidiff-ui diff-added) :weight semi-bold))
    (omnidiff-gap . (:inherit (omnidiff-ui shadow) :slant italic :extend t))
    (omnidiff-scope . (:inherit shadow :slant italic :extend t))
    (omnidiff-lineno . (:inherit (omnidiff-ui shadow)))
    (omnidiff-lineno-added . (:inherit (omnidiff-lineno success)))
    (omnidiff-lineno-removed . (:inherit (omnidiff-lineno error)))
    (omnidiff-lineno-changed . (:inherit (omnidiff-lineno warning)))
    (omnidiff-lineno-moved . (:inherit omnidiff-lineno))
    (omnidiff-moved-indicator . (:foreground "grey50"))
    (omnidiff-added . (:inherit diff-added :extend t))
    (omnidiff-added-refine . (:inherit diff-refine-added :weight medium))
    (omnidiff-removed . (:inherit diff-removed :extend t))
    (omnidiff-removed-refine . (:inherit diff-refine-removed :weight medium))
    (omnidiff-changed . (:inherit diff-changed :extend t))
    (omnidiff-changed-refine . (:inherit diff-refine-changed :weight bold))
    (omnidiff-moved . (:extend t))
    (omnidiff-moved-header . (:inherit (omnidiff-ui shadow) :slant italic :extend t))
    (omnidiff-commit-hash . (:weight bold))
    (omnidiff-commit-meta . (:inherit shadow))
    (omnidiff-image-bar . (:inherit (omnidiff-ui header-line) :extend t))
    (omnidiff-image-mode-active . (:inherit (omnidiff-ui highlight) :weight bold))
    (omnidiff-image-mode-inactive . (:inherit (omnidiff-ui shadow)))
    (omnidiff-image-meta . (:inherit (omnidiff-ui shadow)))
    (omnidiff-image-stat-delta . (:inherit omnidiff-stat-changed))
    (omnidiff-image-divider . (:inherit omnidiff-file-accent :weight bold))
    (omnidiff-image-frame . (:inherit shadow))
    (omnidiff-image-diff-highlight . (:foreground "magenta"))
    ;; Semantic token faces (no `:extend t')
    (omnidiff-token-insert . (:inherit diff-refine-added))
    (omnidiff-token-delete . (:inherit diff-refine-removed))
    (omnidiff-token-update . (:inherit diff-refine-changed))
    (omnidiff-token-move . (:underline t))
    (omnidiff-token-cross-highlight . (:inherit highlight))
    (omnidiff-token-active . (:box (:line-width -1)))
    ;; Dual viewer chrome & gutter faces
    (omnidiff-dual-header-before . (:inherit omnidiff-side-before))
    (omnidiff-dual-header-after . (:inherit omnidiff-side-after))
    (omnidiff-dual-header-meta . (:inherit (omnidiff-ui shadow)))
    (omnidiff-dual-header-before-inactive . (:inherit (omnidiff-ui shadow)))
    (omnidiff-dual-header-after-inactive . (:inherit (omnidiff-ui shadow)))
    (omnidiff-dual-header-file . (:inherit omnidiff-ui :weight semi-bold))
    (omnidiff-dual-header-file-inactive . (:inherit (omnidiff-ui shadow)))
    (omnidiff-dual-header-badge . (:inherit omnidiff-summary-badge))
    (omnidiff-dual-fold-banner . (:inherit omnidiff-gap :extend t))
    (omnidiff-dual-gutter-insert . (:inherit (omnidiff-ui success) :weight bold))
    (omnidiff-dual-gutter-delete . (:inherit (omnidiff-ui error) :weight bold))
    (omnidiff-dual-gutter-update . (:inherit (omnidiff-ui warning) :weight bold))
    (omnidiff-dual-gutter-move . (:inherit (omnidiff-ui shadow) :weight bold))
    ;; Semantic summary & container faces
    (omnidiff-summary-badge . (:inherit omnidiff-header-badge))
    (omnidiff-container-member-changed . (:inherit (omnidiff-ui warning) :weight semi-bold))
    (omnidiff-container-member-added . (:inherit (omnidiff-ui success) :weight semi-bold))
    (omnidiff-container-member-removed . (:inherit (omnidiff-ui error) :weight semi-bold)))
  "8-color/16-color TTY and monochrome fallback face attributes.")

(defun omnidiff-theme--face-color-attrs (p)
  "Build `(FACE . ATTRS)' alist for `((class color) (min-colors 88))' from P."
  (let* ((bg-main (omnidiff-theme--palette-bg-main p))
         (bg-active (omnidiff-theme--palette-bg-active p))
         (bg-card (omnidiff-theme--palette-bg-card p))
         (bg-chrome (omnidiff-theme--palette-bg-chrome p))
         (bg-hunk (omnidiff-theme--palette-bg-hunk p))
         (border (omnidiff-theme--palette-border p))
         (hunk-border (omnidiff-theme--palette-hunk-border p))
         (accent (omnidiff-theme--palette-accent p))
         (hunk-accent (omnidiff-theme--palette-hunk-accent p))
         (fg-main (omnidiff-theme--palette-fg-main p))
         (fg-dim (omnidiff-theme--palette-fg-dim p))
         (blue-cooler (omnidiff-theme--palette-blue-cooler p))
         (add-bg (omnidiff-theme--palette-add-bg p))
         (add-fg (omnidiff-theme--palette-add-fg p))
         (add-ref-bg (omnidiff-theme--palette-add-ref-bg p))
         (add-gut-fg (omnidiff-theme--palette-add-gut-fg p))
         (rem-bg (omnidiff-theme--palette-rem-bg p))
         (rem-fg (omnidiff-theme--palette-rem-fg p))
         (rem-ref-bg (omnidiff-theme--palette-rem-ref-bg p))
         (rem-gut-fg (omnidiff-theme--palette-rem-gut-fg p))
         (chg-bg (omnidiff-theme--palette-chg-bg p))
         (chg-fg (omnidiff-theme--palette-chg-fg p))
         (chg-ref-bg (omnidiff-theme--palette-chg-ref-bg p))
         (chg-gut-fg (omnidiff-theme--palette-chg-gut-fg p))
         (mov-bg (omnidiff-theme--palette-mov-bg p))
         (mov-fg (omnidiff-theme--palette-mov-fg p))
         (mov-gut-fg (omnidiff-theme--palette-mov-gut-fg p))
         (mov-hdr-fg (omnidiff-theme--palette-mov-hdr-fg p))
         (ref-fg (omnidiff-theme--palette-ref-fg p))
         (ref-weight (omnidiff-theme--palette-ref-weight p))
         (side-before-bg (omnidiff-theme--palette-side-before-bg p))
         (side-after-bg (omnidiff-theme--palette-side-after-bg p))
         (stat-add-fg (omnidiff-theme--palette-stat-add-fg p))
         (stat-rem-fg (omnidiff-theme--palette-stat-rem-fg p))
         (stat-chg-fg (omnidiff-theme--palette-stat-chg-fg p))
         (stat-mov-fg (omnidiff-theme--palette-stat-mov-fg p))
         (token-insert-bg (omnidiff-theme--palette-token-insert-bg p))
         (token-delete-bg (omnidiff-theme--palette-token-delete-bg p))
         (token-update-bg (omnidiff-theme--palette-token-update-bg p))
         (token-move-bg (omnidiff-theme--palette-token-move-bg p))
         (token-cross-bg (omnidiff-theme--palette-token-cross-bg p))
         (token-active-bg (omnidiff-theme--palette-token-active-bg p))
         (token-active-border (omnidiff-theme--palette-token-active-border p))
         (token-overlay-fg (omnidiff-theme--palette-token-overlay-fg p))
         (dual-gutter-insert (omnidiff-theme--palette-dual-gutter-insert p))
         (dual-gutter-delete (omnidiff-theme--palette-dual-gutter-delete p))
         (dual-gutter-update (omnidiff-theme--palette-dual-gutter-update p))
         (dual-gutter-move (omnidiff-theme--palette-dual-gutter-move p))
         (uniform-fg (when (eq omnidiff-dual-overlay-foreground 'uniform)
                       (list :foreground token-overlay-fg)))
         (fold-rule (omnidiff-theme--blend-hex border bg-main 0.35)))
    `((omnidiff-header-line
       :inherit omnidiff-ui :background ,bg-main :foreground ,fg-dim
       :underline (:color ,border :position t))
      (omnidiff-header-badge
       :inherit omnidiff-ui :background ,bg-card :foreground ,accent :weight bold
       :underline (:color ,border :position t))
      (omnidiff-header-file
       :inherit omnidiff-ui :background ,bg-main :foreground ,fg-main
       :weight semi-bold :underline (:color ,border :position t))
      (omnidiff-file-header
       :inherit omnidiff-ui :background ,bg-card :foreground ,fg-main
       :weight bold :overline ,bg-active :extend t)
      (omnidiff-file-accent
       :inherit omnidiff-file-header :foreground ,accent)
      (omnidiff-stat-added
       :inherit omnidiff-file-header :foreground ,stat-add-fg :weight semi-bold)
      (omnidiff-stat-removed
       :inherit omnidiff-file-header :foreground ,stat-rem-fg :weight semi-bold)
      (omnidiff-stat-changed
       :inherit omnidiff-file-header :foreground ,stat-chg-fg :weight semi-bold)
      (omnidiff-stat-moved
       :inherit omnidiff-file-header :foreground ,stat-mov-fg :weight semi-bold)
      (omnidiff-stat-index
       :inherit omnidiff-file-header :foreground ,fg-dim :weight normal)
      (omnidiff-file-status-added
       :inherit omnidiff-file-header :background ,side-after-bg :foreground ,add-gut-fg
       :box (:line-width -1 :color ,border))
      (omnidiff-file-status-deleted
       :inherit omnidiff-file-header :background ,side-before-bg :foreground ,rem-gut-fg
       :box (:line-width -1 :color ,border))
      (omnidiff-file-status-renamed
       :inherit omnidiff-file-header
       :background ,(omnidiff-theme--blend-hex chg-ref-bg bg-card 0.55)
       :foreground ,chg-gut-fg
       :box (:line-width -1 :color ,border))
      (omnidiff-hunk-header
       :inherit omnidiff-ui :background ,bg-hunk :foreground ,fg-main
       :weight semi-bold :overline ,hunk-border :extend t)
      (omnidiff-hunk-accent
       :inherit omnidiff-hunk-header :foreground ,hunk-accent :weight bold)
      (omnidiff-hunk-meta
       :inherit omnidiff-hunk-header :foreground ,fg-dim :weight normal)
      (omnidiff-hunk-stat-added
       :inherit omnidiff-hunk-header :foreground ,stat-add-fg :weight semi-bold)
      (omnidiff-hunk-stat-removed
       :inherit omnidiff-hunk-header :foreground ,stat-rem-fg :weight semi-bold)
      (omnidiff-hunk-stat-changed
       :inherit omnidiff-hunk-header :foreground ,stat-chg-fg :weight semi-bold)
      (omnidiff-hunk-stat-moved
       :inherit omnidiff-hunk-header :foreground ,stat-mov-fg :weight semi-bold)
      (omnidiff-side-bar
       :inherit omnidiff-ui :background ,bg-main :foreground ,fg-dim :extend t)
      (omnidiff-side-before
       :inherit omnidiff-ui :background ,side-before-bg :foreground ,rem-gut-fg
       :weight semi-bold)
      (omnidiff-side-after
       :inherit omnidiff-ui :background ,side-after-bg :foreground ,add-gut-fg
       :weight semi-bold)
      (omnidiff-gap
       :inherit omnidiff-ui :background ,bg-chrome :foreground ,fg-dim
       :slant italic :extend t)
      (omnidiff-scope
       :background ,bg-chrome :foreground ,fg-dim :extend t)
      (omnidiff-lineno
       :inherit omnidiff-ui :background ,bg-chrome :foreground ,fg-dim)
      (omnidiff-lineno-added
       :inherit omnidiff-ui :background ,add-bg :foreground ,add-gut-fg)
      (omnidiff-lineno-removed
       :inherit omnidiff-ui :background ,rem-bg :foreground ,rem-gut-fg)
      (omnidiff-lineno-changed
       :inherit omnidiff-ui :background ,chg-bg :foreground ,chg-gut-fg)
      (omnidiff-lineno-moved
       :inherit omnidiff-ui :background ,mov-bg :foreground ,mov-gut-fg)
      (omnidiff-moved-indicator
       :foreground ,mov-gut-fg)
      (omnidiff-added
       :background ,add-bg :foreground ,add-fg :extend t)
      (omnidiff-added-refine
       :background ,add-ref-bg :foreground ,ref-fg :weight ,ref-weight)
      (omnidiff-removed
       :background ,rem-bg :foreground ,rem-fg :extend t)
      (omnidiff-removed-refine
       :background ,rem-ref-bg :foreground ,ref-fg :weight ,ref-weight)
      (omnidiff-changed
       :background ,chg-bg :foreground ,chg-fg :extend t)
      (omnidiff-changed-refine
       :background ,chg-ref-bg :foreground ,ref-fg :weight ,ref-weight)
      (omnidiff-moved
       :background ,mov-bg :foreground ,mov-fg :extend t)
      (omnidiff-moved-header
       :inherit omnidiff-ui :background ,mov-bg :foreground ,mov-hdr-fg
       :slant italic :extend t)
      (omnidiff-commit-hash
       :foreground ,blue-cooler :weight bold)
      (omnidiff-commit-meta
       :foreground ,fg-dim)
      (omnidiff-image-bar
       :inherit omnidiff-ui :background ,bg-chrome :foreground ,fg-dim :extend t)
      (omnidiff-image-mode-active
       :inherit omnidiff-ui :background ,bg-card :foreground ,accent :weight bold)
      (omnidiff-image-mode-inactive
       :inherit omnidiff-ui :background ,bg-chrome :foreground ,fg-dim)
      (omnidiff-image-meta
       :inherit omnidiff-ui :foreground ,fg-dim)
      (omnidiff-image-stat-delta
       :inherit omnidiff-file-header :foreground ,stat-chg-fg :weight semi-bold)
      (omnidiff-image-divider
       :foreground ,accent :weight bold)
      (omnidiff-image-frame
       :foreground ,(omnidiff-theme--blend-hex border bg-main 0.30))
      (omnidiff-image-diff-highlight
       :foreground ,(if (omnidiff-theme--dark-color-p bg-main)
                        "#e65c8e" "#c82d63"))
      ;; Semantic token faces (no `:extend t')
      (omnidiff-token-insert
       :background ,token-insert-bg ,@uniform-fg)
      (omnidiff-token-delete
       :background ,token-delete-bg ,@uniform-fg)
      (omnidiff-token-update
       :background ,token-update-bg ,@uniform-fg)
      (omnidiff-token-move
       :background ,token-move-bg ,@uniform-fg)
      (omnidiff-token-cross-highlight
       :background ,token-cross-bg
       :underline (:color ,blue-cooler :style line)
       ,@uniform-fg)
      (omnidiff-token-active
       :background ,token-active-bg
       :box (:line-width -1 :color ,token-active-border)
       ,@uniform-fg)
      ;; Dual viewer chrome & gutter faces
      (omnidiff-dual-header-before
       :inherit omnidiff-ui :background ,side-before-bg
       :foreground ,dual-gutter-delete :weight semi-bold
       :underline (:color ,border :position t))
      (omnidiff-dual-header-after
       :inherit omnidiff-ui :background ,side-after-bg
       :foreground ,dual-gutter-insert :weight semi-bold
       :underline (:color ,border :position t))
      (omnidiff-dual-header-meta
       :inherit omnidiff-ui :background ,bg-chrome :foreground ,fg-dim
       :underline (:color ,border :position t))
      (omnidiff-dual-header-before-inactive
       :inherit omnidiff-ui :background ,bg-chrome
       :foreground ,rem-gut-fg :weight normal
       :underline (:color ,border :position t))
      (omnidiff-dual-header-after-inactive
       :inherit omnidiff-ui :background ,bg-chrome
       :foreground ,add-gut-fg :weight normal
       :underline (:color ,border :position t))
      (omnidiff-dual-header-file
       :inherit omnidiff-ui :background ,bg-chrome
       :foreground ,fg-main :weight semi-bold
       :underline (:color ,border :position t))
      (omnidiff-dual-header-file-inactive
       :inherit omnidiff-ui :background ,bg-chrome
       :foreground ,fg-dim :weight normal
       :underline (:color ,border :position t))
      (omnidiff-dual-header-badge
       :inherit omnidiff-ui :background ,bg-card
       :foreground ,accent :weight semi-bold
       :underline (:color ,border :position t))
      (omnidiff-dual-fold-banner
       :inherit omnidiff-ui :background ,bg-chrome :foreground ,fg-dim
       :slant italic :overline ,fold-rule
       :underline (:color ,fold-rule :position t) :extend t)
      (omnidiff-dual-gutter-insert
       :inherit omnidiff-ui :foreground ,dual-gutter-insert :weight bold)
      (omnidiff-dual-gutter-delete
       :inherit omnidiff-ui :foreground ,dual-gutter-delete :weight bold)
      (omnidiff-dual-gutter-update
       :inherit omnidiff-ui :foreground ,dual-gutter-update :weight bold)
      (omnidiff-dual-gutter-move
       :inherit omnidiff-ui :foreground ,dual-gutter-move :weight bold)
      ;; Semantic summary & container faces
      (omnidiff-summary-badge
       :inherit omnidiff-ui :background ,bg-card :foreground ,accent
       :weight semi-bold :box (:line-width -1 :color ,border))
      (omnidiff-container-member-changed
       :inherit omnidiff-ui :foreground ,dual-gutter-update :weight semi-bold)
      (omnidiff-container-member-added
       :inherit omnidiff-ui :foreground ,dual-gutter-insert :weight semi-bold)
      (omnidiff-container-member-removed
       :inherit omnidiff-ui :foreground ,dual-gutter-delete :weight semi-bold))))

(defun omnidiff-theme--face-specs (p &optional light-palette)
  "Build face specification list from `omnidiff-theme--palette' P.
Wraps hex-color specs under `((class color) (min-colors 88))' and includes
a clean `(t ...)' fallback clause for 8-color/16-color TTY and monochrome
displays.  When LIGHT-PALETTE is non-nil, P provides `(background dark)'
specs and LIGHT-PALETTE provides `(background light)' specs."
  (let* ((ui-spec `(omnidiff-ui
                    ((t ,@(when omnidiff-ui-font-height
                            (list :height omnidiff-ui-font-height))))))
         (primary-attrs (omnidiff-theme--face-color-attrs p))
         (light-attrs (when light-palette
                        (omnidiff-theme--face-color-attrs light-palette))))
    (cons
     ui-spec
     (if light-palette
         (cl-mapcar
          (lambda (dark-entry light-entry)
            (let* ((face (car dark-entry))
                   (fallback (cdr (assq face omnidiff-theme--fallback-specs))))
              (list face
                    (list (cons '((class color) (min-colors 88) (background dark))
                                (cdr dark-entry))
                          (cons '((class color) (min-colors 88) (background light))
                                (cdr light-entry))
                          (cons t fallback)))))
          primary-attrs
          light-attrs)
       (mapcar
        (lambda (entry)
          (let* ((face (car entry))
                 (fallback (cdr (assq face omnidiff-theme--fallback-specs))))
            (list face
                  (list (cons '((class color) (min-colors 88))
                              (cdr entry))
                        (cons t fallback)))))
        primary-attrs)))))

(defun omnidiff-theme--active-modus-theme ()
  "Return the active topmost Modus theme symbol, or nil."
  (when (and (fboundp 'modus-themes-get-current-theme)
             (fboundp 'modus-themes-get-color-value))
    (when-let* ((current (modus-themes-get-current-theme)))
      (when (or (null custom-enabled-themes)
                (eq (car custom-enabled-themes) current))
        current))))

(defun omnidiff-theme-semantic-colors ()
  "Return a plist of theme-synchronized \"#rrggbb\" semantic colors.
Keys include `:insert-bg', `:delete-bg', `:update-bg', `:move-bg',
`:cross-bg', `:active-bg', `:overlay-fg', `:bg-main', `:bg-chrome',
`:bg-card', `:border', `:fg-main', `:fg-dim', `:accent',
`:gutter-insert', `:gutter-delete', `:gutter-update', and `:gutter-move'."
  (let* ((base (if (omnidiff-theme--active-modus-theme)
                   (omnidiff-theme--modus-base)
                 (omnidiff-theme--generic-base)))
         (p (omnidiff-theme--resolve-palette base omnidiff-color-scheme)))
    (list :insert-bg (omnidiff-theme--palette-token-insert-bg p)
          :delete-bg (omnidiff-theme--palette-token-delete-bg p)
          :update-bg (omnidiff-theme--palette-token-update-bg p)
          :move-bg (omnidiff-theme--palette-token-move-bg p)
          :cross-bg (omnidiff-theme--palette-token-cross-bg p)
          :active-bg (omnidiff-theme--palette-token-active-bg p)
          :overlay-fg (omnidiff-theme--palette-token-overlay-fg p)
          :bg-main (omnidiff-theme--palette-bg-main p)
          :bg-chrome (omnidiff-theme--palette-bg-chrome p)
          :bg-card (omnidiff-theme--palette-bg-card p)
          :border (omnidiff-theme--palette-border p)
          :fg-main (omnidiff-theme--palette-fg-main p)
          :fg-dim (omnidiff-theme--palette-fg-dim p)
          :accent (omnidiff-theme--palette-accent p)
          :gutter-insert (omnidiff-theme--palette-dual-gutter-insert p)
          :gutter-delete (omnidiff-theme--palette-dual-gutter-delete p)
          :gutter-update (omnidiff-theme--palette-dual-gutter-update p)
          :gutter-move (omnidiff-theme--palette-dual-gutter-move p))))

(defvar omnidiff-theme--revision 0
  "Monotonic revision counter incremented whenever `omnidiff-apply-theme' runs.")

(defvar omnidiff-theme-after-apply-hook nil
  "Normal hook run after `omnidiff-apply-theme' updates palette faces.")

(defun omnidiff-theme--on-disable-theme (_theme)
  "Reapply `omnidiff' faces after disabling a custom theme."
  (omnidiff-apply-theme))

;;;###autoload
(defun omnidiff-apply-theme (&optional theme)
  "Apply active theme palette faces for `omnidiff-mode'.
When THEME is non-nil and not enabled in `custom-enabled-themes', do nothing.
Uses Modus palette values when a Modus theme is active, and derives colors
from the active Emacs faces on all other themes.  Return non-nil when applied."
  (when (or (null theme) (memq theme custom-enabled-themes))
    (setq omnidiff-theme--revision (1+ omnidiff-theme--revision))
    (face-spec-set 'omnidiff-ui
                   `((t ,@(when omnidiff-ui-font-height
                            (list :height omnidiff-ui-font-height))))
                   'face-defface-spec)
    (let* ((modus-current (omnidiff-theme--active-modus-theme))
           (use-modus (and modus-current
                           (or (null theme) (eq theme modus-current))))
           (target-theme (if use-modus
                             modus-current
                           (or theme (car custom-enabled-themes))))
           (specs
            (if target-theme
                (let* ((base (if use-modus
                                 (omnidiff-theme--modus-base)
                               (omnidiff-theme--generic-base)))
                       (palette (omnidiff-theme--resolve-palette
                                 base omnidiff-color-scheme)))
                  (omnidiff-theme--face-specs palette))
              (let* ((dark-palette (omnidiff-theme--resolve-palette
                                    (omnidiff-theme--generic-base 'dark)
                                    omnidiff-color-scheme))
                     (light-palette (omnidiff-theme--resolve-palette
                                     (omnidiff-theme--generic-base 'light)
                                     omnidiff-color-scheme)))
                (omnidiff-theme--face-specs dark-palette light-palette)))))
      (if target-theme
          (progn
            (apply #'custom-theme-set-faces target-theme specs)
            (dolist (entry specs)
              (custom-theme-recalc-face (car entry))))
        (dolist (entry specs)
          (face-spec-set (car entry) (cadr entry) 'face-defface-spec)))
      (run-hooks 'omnidiff-theme-after-apply-hook)
      t)))

(add-hook 'enable-theme-functions #'omnidiff-apply-theme)
(add-hook 'disable-theme-functions #'omnidiff-theme--on-disable-theme)
(add-hook 'modus-themes-after-load-theme-hook #'omnidiff-apply-theme)
(omnidiff-apply-theme)

(provide 'omnidiff-theme)
;;; omnidiff-theme.el ends here
