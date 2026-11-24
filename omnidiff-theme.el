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

(declare-function modus-themes-get-color-value "ext:modus-themes" (color &optional overrides theme))
(declare-function modus-themes-get-current-theme "ext:modus-themes" ())

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
;;
;; Each `defface' carries the single-source-of-truth `(t ...)' fallback spec
;; for 8/16-color TTY and monochrome displays.  `omnidiff-apply-theme' prepends
;; `((class color) (min-colors 88) ...)' clauses while preserving each face's
;; `(t ...)' fallback clause.

(defface omnidiff-ui
  '((t nil))
  "Base chrome face inherited by `omnidiff-mode' headers, gutters, and badges."
  :group 'omnidiff)

(defface omnidiff-header-line
  '((t :inherit (omnidiff-ui header-line)))
  "Header line face for `omnidiff-mode'."
  :group 'omnidiff)

(defface omnidiff-header-badge
  '((t :inherit (omnidiff-ui highlight) :weight bold))
  "Accent badge face in the `omnidiff-mode' header line."
  :group 'omnidiff)

(defface omnidiff-header-file
  '((t :inherit (omnidiff-ui header-line) :weight semi-bold))
  "Current visible file name in the fixed header line."
  :group 'omnidiff)

(defface omnidiff-file-header
  '((t :inherit (omnidiff-ui header-line) :weight bold :extend t))
  "File card header face in `omnidiff-mode'."
  :group 'omnidiff)

(defface omnidiff-file-accent
  '((t :inherit omnidiff-file-header))
  "Leading accent bar on file cards."
  :group 'omnidiff)

(defface omnidiff-stat-added
  '((t :inherit (omnidiff-file-header success) :weight semi-bold))
  "Added count in file card header."
  :group 'omnidiff)

(defface omnidiff-stat-removed
  '((t :inherit (omnidiff-file-header error) :weight semi-bold))
  "Removed count in file card header."
  :group 'omnidiff)

(defface omnidiff-stat-changed
  '((t :inherit (omnidiff-file-header warning) :weight semi-bold))
  "Updated count in file card header."
  :group 'omnidiff)

(defface omnidiff-stat-moved
  '((t :inherit (omnidiff-file-header shadow) :weight semi-bold))
  "Moved count in file card header."
  :group 'omnidiff)

(defface omnidiff-stat-index
  '((t :inherit (omnidiff-file-header shadow) :weight normal))
  "File index counter in file card header."
  :group 'omnidiff)

(defface omnidiff-file-status-added
  '((t :inherit (omnidiff-file-header diff-added)))
  "Status badge for newly added files."
  :group 'omnidiff)

(defface omnidiff-file-status-deleted
  '((t :inherit (omnidiff-file-header diff-removed)))
  "Status badge for deleted files."
  :group 'omnidiff)

(defface omnidiff-file-status-renamed
  '((t :inherit (omnidiff-file-header warning)))
  "Status badge for renamed files."
  :group 'omnidiff)

(defface omnidiff-hunk-header
  '((t :inherit (omnidiff-ui header-line) :weight semi-bold :extend t))
  "Hunk header strip face in `omnidiff-mode'."
  :group 'omnidiff)

(defface omnidiff-hunk-accent
  '((t :inherit omnidiff-hunk-header :weight bold))
  "Disclosure indicator face on `omnidiff-mode' hunk headers."
  :group 'omnidiff)

(defface omnidiff-hunk-meta
  '((t :inherit (omnidiff-hunk-header shadow) :weight normal))
  "Secondary metadata (unchanged count, hunk index) on hunk headers."
  :group 'omnidiff)

(defface omnidiff-hunk-stat-added
  '((t :inherit (omnidiff-hunk-header success) :weight semi-bold))
  "Added line count face on hunk headers."
  :group 'omnidiff)

(defface omnidiff-hunk-stat-removed
  '((t :inherit (omnidiff-hunk-header error) :weight semi-bold))
  "Removed line count face on hunk headers."
  :group 'omnidiff)

(defface omnidiff-hunk-stat-changed
  '((t :inherit (omnidiff-hunk-header warning) :weight semi-bold))
  "Changed line count face on hunk headers."
  :group 'omnidiff)

(defface omnidiff-hunk-stat-moved
  '((t :inherit (omnidiff-hunk-header shadow) :weight semi-bold))
  "Moved line count face on hunk headers."
  :group 'omnidiff)

(defface omnidiff-side-bar
  '((t :inherit (omnidiff-ui shadow) :extend t))
  "Full-width subtle strip behind BEFORE/AFTER labels."
  :group 'omnidiff)

(defface omnidiff-side-before
  '((t :inherit (omnidiff-ui diff-removed) :weight semi-bold))
  "BEFORE sub-header pill face."
  :group 'omnidiff)

(defface omnidiff-side-after
  '((t :inherit (omnidiff-ui diff-added) :weight semi-bold))
  "AFTER sub-header pill face."
  :group 'omnidiff)

(defface omnidiff-gap
  '((t :inherit (omnidiff-ui shadow) :slant italic :extend t))
  "Collapsed unchanged-lines band face."
  :group 'omnidiff)

(defface omnidiff-lineno
  '((t :inherit (omnidiff-ui shadow)))
  "Line number gutter face for context lines."
  :group 'omnidiff)

(defface omnidiff-lineno-added
  '((t :inherit (omnidiff-lineno success)))
  "Line number gutter face for added lines."
  :group 'omnidiff)

(defface omnidiff-lineno-removed
  '((t :inherit (omnidiff-lineno error)))
  "Line number gutter face for removed lines."
  :group 'omnidiff)

(defface omnidiff-lineno-changed
  '((t :inherit (omnidiff-lineno warning)))
  "Line number gutter face for updated lines."
  :group 'omnidiff)

(defface omnidiff-lineno-moved
  '((t :inherit omnidiff-lineno))
  "Line number gutter face for moved lines."
  :group 'omnidiff)

(defface omnidiff-moved-indicator
  '((t :foreground "grey50"))
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

(defface omnidiff-commit-hash
  '((t :weight bold))
  "Commit hash face in `git show' preambles."
  :group 'omnidiff)

(defface omnidiff-commit-meta
  '((t :inherit shadow))
  "Author/Date metadata face in `git show' preambles."
  :group 'omnidiff)

(defface omnidiff-image-bar
  '((t :inherit (omnidiff-ui header-line) :extend t))
  "Subtle control strip behind image diff mode pills."
  :group 'omnidiff)

(defface omnidiff-image-mode-active
  '((t :inherit (omnidiff-ui highlight) :weight bold))
  "Selected visual diff mode pill on an image card toolbar."
  :group 'omnidiff)

(defface omnidiff-image-mode-inactive
  '((t :inherit (omnidiff-ui shadow)))
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
  '((t :inherit shadow))
  "Subtle border color framing inline image previews and 1:1 region crops."
  :group 'omnidiff)

(defface omnidiff-image-diff-highlight
  '((t :foreground "magenta"))
  "Accent color for pixel-diff highlights and changed-region boxes."
  :group 'omnidiff)

;;; Semantic token faces (no `:extend t' -- wrap only changed token spans)

(defface omnidiff-token-insert
  '((t :inherit diff-refine-added))
  "Inline semantic token highlight for inserted spans."
  :group 'omnidiff)

(defface omnidiff-token-delete
  '((t :inherit diff-refine-removed))
  "Inline semantic token highlight for deleted spans."
  :group 'omnidiff)

(defface omnidiff-token-update
  '((t :inherit diff-refine-changed))
  "Inline semantic token highlight for updated spans."
  :group 'omnidiff)

(defface omnidiff-token-move
  '((t :underline t))
  "Inline semantic token highlight for moved spans."
  :group 'omnidiff)

(defface omnidiff-token-cross-highlight
  '((t :inherit highlight))
  "Vivid counterpart highlight linking active changes and move targets."
  :group 'omnidiff)

(defface omnidiff-token-active
  '((t :box (:line-width -1)))
  "Focused change accent for the current navigation stop."
  :group 'omnidiff)

;;; Dual viewer chrome & gutter faces

(defface omnidiff-dual-header-before
  '((t :inherit omnidiff-side-before))
  "Header face for the BEFORE pane in the dual semantic viewer."
  :group 'omnidiff)

(defface omnidiff-dual-header-after
  '((t :inherit omnidiff-side-after))
  "Header face for the AFTER pane in the dual semantic viewer."
  :group 'omnidiff)

(defface omnidiff-dual-header-meta
  '((t :inherit (omnidiff-ui shadow)))
  "Secondary metadata face in dual viewer pane headers."
  :group 'omnidiff)

(defface omnidiff-dual-header-before-inactive
  '((t :inherit (omnidiff-ui shadow)))
  "Inactive BEFORE pill face when the opposite dual pane has focus."
  :group 'omnidiff)

(defface omnidiff-dual-header-after-inactive
  '((t :inherit (omnidiff-ui shadow)))
  "Inactive AFTER pill face when the opposite dual pane has focus."
  :group 'omnidiff)

(defface omnidiff-dual-header-file
  '((t :inherit omnidiff-ui :weight semi-bold))
  "Focused filename label face in dual viewer pane headers."
  :group 'omnidiff)

(defface omnidiff-dual-header-badge
  '((t :inherit omnidiff-summary-badge))
  "Inline badge face inside dual viewer pane headers with continuous underline."
  :group 'omnidiff)

(defface omnidiff-dual-fold-banner
  '((t :inherit omnidiff-gap :extend t))
  "Full-width fold banner separating unchanged regions in the dual viewer."
  :group 'omnidiff)

(defface omnidiff-dual-gutter-insert
  '((t :inherit (omnidiff-ui success) :weight bold))
  "Gutter marker face for inserted lines in the dual viewer."
  :group 'omnidiff)

(defface omnidiff-dual-gutter-delete
  '((t :inherit (omnidiff-ui error) :weight bold))
  "Gutter marker face for deleted lines in the dual viewer."
  :group 'omnidiff)

(defface omnidiff-dual-gutter-update
  '((t :inherit (omnidiff-ui warning) :weight bold))
  "Gutter marker face for updated lines in the dual viewer."
  :group 'omnidiff)

(defface omnidiff-dual-gutter-move
  '((t :inherit (omnidiff-ui shadow) :weight bold))
  "Gutter marker face for moved lines in the dual viewer."
  :group 'omnidiff)

;;; Semantic summary & container faces

(defface omnidiff-summary-badge
  '((t :inherit omnidiff-header-badge))
  "Badge face for semantic change summaries."
  :group 'omnidiff)

(defface omnidiff-container-member-changed
  '((t :inherit (omnidiff-ui warning) :weight semi-bold))
  "Face for changed members in semantic container summaries."
  :group 'omnidiff)

(defface omnidiff-container-member-added
  '((t :inherit (omnidiff-ui success) :weight semi-bold))
  "Face for added members in semantic container summaries."
  :group 'omnidiff)

(defface omnidiff-container-member-removed
  '((t :inherit (omnidiff-ui error) :weight semi-bold))
  "Face for removed members in semantic container summaries."
  :group 'omnidiff)

;;; State & public helpers

(defvar omnidiff-theme--revision 0
  "Monotonic revision counter incremented whenever `omnidiff-apply-theme' runs.")

(defvar omnidiff-theme--last-applied-key nil
  "Memoization key for the most recent `omnidiff-apply-theme' application.")

(defvar omnidiff-theme--semantic-colors-cache nil
  "Cached `(KEY . PLIST)' pair for `omnidiff-theme-semantic-colors'.")

(defvar omnidiff-theme-after-apply-hook nil
  "Normal hook run after `omnidiff-apply-theme' updates palette faces.")

(defconst omnidiff-theme--semantic-color-keys
  '(:insert-bg :delete-bg :update-bg :move-bg :cross-bg :active-bg
    :overlay-fg :bg-main :bg-chrome :bg-card :border :fg-main :fg-dim
    :accent :gutter-insert :gutter-delete :gutter-update :gutter-move)
  "Keys exported by `omnidiff-theme-semantic-colors'.")

(defun omnidiff-theme-revision ()
  "Return the current monotonic theme revision counter."
  omnidiff-theme--revision)

(defun omnidiff-theme-op-priority (op)
  "Return precedence rank for diff operation OP (higher wins)."
  (pcase op
    ('update 3)
    ('insert 2)
    ('delete 2)
    ('move 1)
    (_ 0)))

(defun omnidiff-theme-token-face (op)
  "Return semantic token face symbol for diff operation OP."
  (pcase op
    ('insert 'omnidiff-token-insert)
    ('delete 'omnidiff-token-delete)
    ('update 'omnidiff-token-update)
    ('move 'omnidiff-token-move)
    (_ 'omnidiff-token-update)))

(defun omnidiff-theme-cycle-color-scheme ()
  "Cycle `omnidiff-color-scheme' across presets and re-apply faces.
Return the newly selected scheme symbol."
  (let ((next (pcase omnidiff-color-scheme
                ('omni-semantic 'calm-modus)
                ('calm-modus 'neutral-paper)
                (_ 'omni-semantic))))
    (setq omnidiff-color-scheme next)
    (omnidiff-apply-theme)
    next))

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

(defun omnidiff-theme-dark-color-p (color)
  "Return non-nil when COLOR is dark (Rec. 709 relative luma below 128)."
  (let ((rgb (or (omnidiff-theme--parse-hex color)
                 (when-let* ((hex (omnidiff-theme-color-hex color)))
                   (omnidiff-theme--parse-hex hex)))))
    (pcase rgb
      (`(,r ,g ,b) (< (+ (* 0.2126 r) (* 0.7152 g) (* 0.0722 b)) 128.0))
      (_ (eq (frame-parameter nil 'background-mode) 'dark)))))

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

;;; Palette resolution

(cl-defstruct (omnidiff-theme--base (:copier nil))
  "Core theme colors resolved from Modus or generic Emacs faces.
NUANCES is an optional plist of theme-specific soft tints (used by Modus)."
  dark-p bg-main bg-dim bg-inactive bg-active bg-card bg-chrome border
  fg-main fg-dim accent blue-cooler
  accent-green accent-red accent-yellow
  nuances)

(defun omnidiff-theme--modus-base ()
  "Resolve `omnidiff-theme--base' from the active Modus theme."
  (cl-flet ((c (name &optional fallback)
              (or (omnidiff-theme-color-hex (modus-themes-get-color-value name t))
                  fallback)))
    (let* ((frame-dark-p (eq (frame-parameter nil 'background-mode) 'dark))
           (bg-main (c 'bg-main (if frame-dark-p "#1e1e2e" "#ffffff")))
           (dark-p (omnidiff-theme-dark-color-p bg-main))
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
           (bg-blue-nuanced (c 'bg-blue-nuanced (if dark-p "#12154a" "#e8ebfa")))
           (bg-magenta-nuanced (c 'bg-magenta-nuanced)))
      (make-omnidiff-theme--base
       :dark-p dark-p
       :bg-main bg-main
       :bg-dim bg-dim
       :bg-inactive bg-inactive
       :bg-active (c 'bg-active (omnidiff-theme--blend-hex bg-main fg-main 0.14))
       :bg-card (if (equal bg-tab-current bg-main) bg-inactive bg-tab-current)
       :bg-chrome (if (equal bg-tab-current bg-main) bg-dim bg-tab-bar)
       :border (c 'border (omnidiff-theme--blend-hex bg-main fg-main 0.22))
       :fg-main fg-main
       :fg-dim (c 'fg-dim (omnidiff-theme--blend-hex fg-main bg-main 0.45))
       :accent (c 'blue-warmer blue-cooler)
       :blue-cooler blue-cooler
       :accent-green (c 'green (c 'green-cooler fg-added))
       :accent-red (c 'red (c 'red-warmer fg-removed))
       :accent-yellow (c 'yellow (c 'yellow-warmer fg-changed))
       :nuances
       (list :bg-added-faint bg-added-faint
             :bg-added-refine bg-added-refine
             :fg-added fg-added
             :bg-removed-faint bg-removed-faint
             :bg-removed-refine bg-removed-refine
             :fg-removed fg-removed
             :bg-changed-faint bg-changed-faint
             :bg-changed-refine bg-changed-refine
             :fg-changed fg-changed
             :green-faint (c 'green-faint fg-added)
             :red-faint (c 'red-faint fg-removed)
             :yellow-faint yellow-faint
             :indigo (c 'indigo (c 'magenta-cooler (if dark-p "#b4a7f0" "#5c4bb8")))
             :bg-green-nuanced (c 'bg-green-nuanced bg-added-faint)
             :bg-sage (c 'bg-sage bg-added-refine)
             :bg-plum (if bg-magenta-nuanced
                          (omnidiff-theme--blend-hex bg-removed-refine bg-magenta-nuanced 0.45)
                        (omnidiff-theme--blend-hex bg-removed-refine bg-dim 0.42))
             :bg-ochre (c 'bg-ochre bg-changed-refine)
             :fg-ochre (c 'fg-ochre yellow-faint)
             :bg-lavender (c 'bg-lavender bg-blue-nuanced)
             :raw-moved bg-blue-nuanced
             :accent-cyan (c 'cyan blue-cooler))))))

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
                                    (eq (omnidiff-theme-dark-color-p raw-bg)
                                        req-dark-p))))))
         (bg-main (or (and probe-p raw-bg)
                      (if req-dark-p "#1e1e2e" "#ffffff")))
         (dark-p (if background-mode
                     req-dark-p
                   (omnidiff-theme-dark-color-p bg-main)))
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
                                    (eq (omnidiff-theme-dark-color-p hex) dark-p)
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
      (let* ((accent (pick-accent '(link font-lock-keyword-face highlight)
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
                                       (list accent-green accent-red accent-yellow))))
        (make-omnidiff-theme--base
         :dark-p dark-p
         :bg-main bg-main
         :bg-dim (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.06 0.05))
         :bg-inactive (omnidiff-theme--blend-hex bg-main fg-main 0.08)
         :bg-active (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.15 0.13))
         :bg-card (pick-surface
                   '(header-line mode-line-inactive mode-line)
                   (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.10 0.07)))
         :bg-chrome (pick-surface
                     '(fringe)
                     (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.04 0.03)))
         :border (omnidiff-theme--blend-hex bg-main fg-main (if dark-p 0.24 0.20))
         :fg-main fg-main
         :fg-dim fg-dim
         :accent accent
         :blue-cooler accent-cyan
         :accent-green accent-green
         :accent-red accent-red
         :accent-yellow accent-yellow)))))

(defun omnidiff-theme--resolve-palette (base scheme)
  "Compute a resolved semantic color palette plist from BASE and SCHEME.
Unrecognized SCHEME values gracefully fall back to `omni-semantic'."
  (pcase-let (((cl-struct omnidiff-theme--base
                          dark-p bg-main bg-dim bg-inactive bg-active
                          bg-card bg-chrome border fg-main fg-dim
                          accent blue-cooler accent-green accent-red
                          accent-yellow nuances)
               base))
    (cl-flet ((blend (c1 c2 r) (omnidiff-theme--blend-hex c1 c2 r))
              (custom (k fb) (omnidiff-theme--custom-color k fb))
              (nuance (k fb) (or (plist-get nuances k) fb)))
      (let* ((scheme (if (memq scheme '(omni-semantic calm-modus neutral-paper))
                         scheme
                       'omni-semantic))
             (omni-p (eq scheme 'omni-semantic))
             (calm-p (eq scheme 'calm-modus))
             (neutral-p (eq scheme 'neutral-paper))
             (border (custom 'border border))
             (accent (custom 'accent accent))
             (accent-cyan (nuance :accent-cyan blue-cooler))
             (accent-grey (if dark-p "#9aa0a6" "#6e7781"))
             (indigo (nuance :indigo (if dark-p "#b4a7f0" "#5c4bb8")))
             ;; Core diff & soft tint surfaces (Modus nuances when available,
             ;; otherwise dynamically blended from core theme slots).
             (bg-added-faint (nuance :bg-added-faint
                                     (blend accent-green bg-main (if dark-p 0.80 0.88))))
             (bg-added-refine (nuance :bg-added-refine
                                      (blend accent-green bg-main (if dark-p 0.62 0.76))))
             (fg-added (nuance :fg-added accent-green))
             (bg-removed-faint (nuance :bg-removed-faint
                                       (blend accent-red bg-main (if dark-p 0.80 0.88))))
             (bg-removed-refine (nuance :bg-removed-refine
                                        (blend accent-red bg-main (if dark-p 0.62 0.76))))
             (fg-removed (nuance :fg-removed accent-red))
             (bg-changed-faint (nuance :bg-changed-faint
                                       (blend accent-yellow bg-main (if dark-p 0.80 0.88))))
             (bg-changed-refine (nuance :bg-changed-refine
                                        (blend accent-yellow bg-main (if dark-p 0.62 0.76))))
             (fg-changed (nuance :fg-changed accent-yellow))
             (green-faint (nuance :green-faint (blend accent-green fg-main (if dark-p 0.25 0.10))))
             (red-faint (nuance :red-faint (blend accent-red fg-main (if dark-p 0.25 0.10))))
             (yellow-faint (nuance :yellow-faint (blend accent-yellow fg-main (if dark-p 0.25 0.10))))
             (bg-green-nuanced (nuance :bg-green-nuanced
                                       (blend accent-green bg-main (if dark-p 0.78 0.90))))
             ;; Soft `calm-modus' tints: sage, muted plum, warm ochre, lavender.
             (bg-sage (nuance :bg-sage
                              (blend (blend accent-green fg-dim 0.30) bg-main
                                     (if dark-p 0.68 0.80))))
             (bg-plum (nuance :bg-plum
                              (blend (blend accent-red indigo 0.30) bg-main
                                     (if dark-p 0.64 0.78))))
             (bg-ochre (nuance :bg-ochre
                               (blend (blend accent-yellow fg-dim 0.25) bg-main
                                      (if dark-p 0.68 0.80))))
             (fg-ochre (nuance :fg-ochre yellow-faint))
             (bg-lavender (nuance :bg-lavender
                                  (blend indigo bg-main (if dark-p 0.64 0.82))))
             (raw-moved (nuance :raw-moved (blend accent-grey bg-main (if dark-p 0.82 0.90))))
             (fg-soft (blend fg-main fg-dim 0.16))
             ;; Dynamic `neutral-paper' muted washes derived from bg-main/fg-main
             ;; so dark paper harmonizes with warm (Gruvbox/Solarized) and cool themes.
             (paper-green (blend accent-green fg-dim (if dark-p 0.32 0.22)))
             (paper-red (blend accent-red fg-dim (if dark-p 0.32 0.22)))
             (paper-yellow (blend accent-yellow fg-dim (if dark-p 0.32 0.22)))
             (paper-grey (blend fg-dim bg-main (if dark-p 0.20 0.15)))
             (paper-fg (blend fg-main fg-dim 0.22))
             ;; Semantic token backgrounds per scheme.
             (insert-bg
              (custom 'insert-bg
                      (cond (calm-p bg-sage)
                            (neutral-p (blend paper-green bg-main (if dark-p 0.68 0.82)))
                            (t (blend accent-green bg-main (if dark-p 0.56 0.74))))))
             (delete-bg
              (custom 'delete-bg
                      (cond (calm-p bg-plum)
                            (neutral-p (blend paper-red bg-main (if dark-p 0.68 0.82)))
                            (t (blend accent-red bg-main (if dark-p 0.56 0.74))))))
             (update-bg
              (custom 'update-bg
                      (cond (calm-p bg-ochre)
                            (neutral-p (blend paper-yellow bg-main (if dark-p 0.68 0.82)))
                            (t (blend accent-yellow bg-main (if dark-p 0.56 0.74))))))
             (move-bg
              (custom 'move-bg
                      (cond (calm-p bg-lavender)
                            (neutral-p (blend paper-grey bg-main (if dark-p 0.68 0.82)))
                            (t (blend accent-grey bg-main (if dark-p 0.60 0.76))))))
             (cross-bg
              (custom 'cross-highlight-bg
                      (cond (calm-p (blend (blend accent-cyan indigo 0.35) bg-main
                                           (if dark-p 0.64 0.76)))
                            (neutral-p (blend (blend accent-cyan fg-dim 0.25) bg-main
                                              (if dark-p 0.64 0.76)))
                            (t (blend accent-cyan bg-main (if dark-p 0.56 0.68))))))
             (active-bg
              (custom 'active-bg
                      (blend accent bg-main
                             (if omni-p (if dark-p 0.68 0.82) (if dark-p 0.74 0.86)))))
             (overlay-fg (custom 'overlay-fg fg-main))
             ;; Gutter foregrounds (custom overrides applied once per key).
             (gutter-insert
              (custom 'gutter-insert
                      (cond (neutral-p paper-green)
                            (dark-p (blend accent-green fg-main 0.15))
                            (t accent-green))))
             (gutter-delete
              (custom 'gutter-delete
                      (cond (neutral-p paper-red)
                            (dark-p (blend accent-red fg-main 0.15))
                            (t accent-red))))
             (gutter-update
              (custom 'gutter-update
                      (cond (neutral-p paper-yellow)
                            (dark-p (blend accent-yellow fg-main 0.12))
                            (t accent-yellow))))
             (gutter-move
              (custom 'gutter-move
                      (cond (calm-p (if dark-p (blend indigo fg-dim 0.20) indigo))
                            (neutral-p paper-grey)
                            (t accent-grey))))
             (add-gut-fg
              (if calm-p
                  (custom 'gutter-insert
                          (if dark-p (blend green-faint fg-dim 0.30) fg-added))
                gutter-insert))
             (rem-gut-fg
              (if calm-p
                  (custom 'gutter-delete
                          (if dark-p (blend red-faint fg-dim 0.30) fg-removed))
                gutter-delete))
             (chg-gut-fg
              (if calm-p
                  (custom 'gutter-update
                          (if dark-p (blend yellow-faint fg-dim 0.25) fg-changed))
                gutter-update)))
        (list :dark-p dark-p
              :bg-main bg-main
              :bg-active bg-active
              :bg-card bg-card
              :bg-chrome bg-chrome
              :bg-hunk (if dark-p bg-inactive bg-dim)
              :border border
              :hunk-border (if dark-p (blend bg-active border 0.50) border)
              :accent accent
              :hunk-accent (if dark-p (blend accent fg-dim 0.25) accent)
              :fg-main fg-main
              :fg-dim fg-dim
              :blue-cooler blue-cooler
              :add-bg (cond (omni-p (blend accent-green bg-main (if dark-p 0.88 0.93)))
                            (neutral-p (blend paper-green bg-main (if dark-p 0.88 0.93)))
                            (dark-p (blend bg-green-nuanced bg-main 0.38))
                            (t bg-added-faint))
              :add-fg (cond (omni-p fg-main)
                            (neutral-p paper-fg)
                            (dark-p (blend fg-soft green-faint 0.45))
                            (t fg-added))
              :add-ref-bg (if calm-p (if dark-p bg-sage bg-added-refine) insert-bg)
              :add-gut-fg add-gut-fg
              :rem-bg (cond (omni-p (blend accent-red bg-main (if dark-p 0.88 0.93)))
                            (neutral-p (blend paper-red bg-main (if dark-p 0.88 0.93)))
                            (dark-p (blend bg-removed-faint bg-main 0.40))
                            (t bg-removed-faint))
              :rem-fg (cond (omni-p fg-main)
                            (neutral-p paper-fg)
                            (dark-p (blend fg-soft fg-removed 0.45))
                            (t fg-removed))
              :rem-ref-bg (if calm-p (if dark-p bg-plum bg-removed-refine) delete-bg)
              :rem-gut-fg rem-gut-fg
              :chg-bg (cond (omni-p (blend accent-yellow bg-main (if dark-p 0.88 0.93)))
                            (neutral-p (blend paper-yellow bg-main (if dark-p 0.88 0.93)))
                            (dark-p (blend bg-changed-faint bg-main 0.35))
                            (t bg-changed-faint))
              :chg-fg (cond (omni-p fg-main)
                            (neutral-p paper-fg)
                            (dark-p (blend fg-soft fg-ochre 0.55))
                            (t fg-changed))
              :chg-ref-bg (if calm-p (if dark-p bg-ochre bg-changed-refine) update-bg)
              :chg-gut-fg chg-gut-fg
              :mov-bg (cond (omni-p (blend accent-grey bg-main (if dark-p 0.86 0.92)))
                            (neutral-p (blend paper-grey bg-main (if dark-p 0.85 0.90)))
                            (dark-p (blend bg-lavender bg-main 0.72))
                            (t raw-moved))
              :mov-fg (cond (omni-p fg-soft)
                            (neutral-p paper-fg)
                            (dark-p (blend fg-main fg-dim 0.22))
                            (t fg-main))
              :mov-gut-fg gutter-move
              :ref-fg (cond (omni-p overlay-fg)
                            (neutral-p (blend fg-main fg-dim 0.08))
                            (dark-p fg-soft)
                            (t fg-main))
              :ref-weight (if calm-p 'semi-bold 'normal)
              :side-before-bg (if dark-p (blend bg-removed-faint bg-dim 0.35) bg-removed-faint)
              :side-after-bg (if dark-p (blend bg-green-nuanced bg-dim 0.35) bg-added-faint)
              :stat-add-fg (cond (neutral-p paper-green) (dark-p green-faint) (t fg-added))
              :stat-rem-fg (cond (neutral-p paper-red) (dark-p red-faint) (t fg-removed))
              :stat-chg-fg (cond (neutral-p paper-yellow) (dark-p yellow-faint) (t fg-changed))
              :stat-mov-fg (if calm-p indigo gutter-move)
              :insert-bg insert-bg
              :delete-bg delete-bg
              :update-bg update-bg
              :move-bg move-bg
              :cross-bg cross-bg
              :active-bg active-bg
              :overlay-fg overlay-fg
              :gutter-insert gutter-insert
              :gutter-delete gutter-delete
              :gutter-update gutter-update
              :gutter-move gutter-move)))))

(defun omnidiff-theme--face-color-attrs (p)
  "Build `(FACE . ATTRS)' alist for `((class color) (min-colors 88))' from P."
  (cl-destructuring-bind
      (&key dark-p bg-main bg-active bg-card bg-chrome bg-hunk
            border hunk-border accent hunk-accent fg-main fg-dim blue-cooler
            add-bg add-fg add-ref-bg add-gut-fg
            rem-bg rem-fg rem-ref-bg rem-gut-fg
            chg-bg chg-fg chg-ref-bg chg-gut-fg
            mov-bg mov-fg mov-gut-fg
            ref-fg ref-weight side-before-bg side-after-bg
            stat-add-fg stat-rem-fg stat-chg-fg stat-mov-fg
            insert-bg delete-bg update-bg move-bg cross-bg active-bg overlay-fg
            gutter-insert gutter-delete gutter-update gutter-move
            &allow-other-keys)
      p
    (let ((uniform-fg (when (eq omnidiff-dual-overlay-foreground 'uniform)
                        (list :foreground overlay-fg)))
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
        (omnidiff-image-divider
         :foreground ,accent :weight bold)
        (omnidiff-image-frame
         :foreground ,(omnidiff-theme--blend-hex border bg-main 0.30))
        (omnidiff-image-diff-highlight
         :foreground ,(if dark-p "#e65c8e" "#c82d63"))
        ;; Semantic token faces (no `:extend t')
        (omnidiff-token-insert
         :background ,insert-bg ,@uniform-fg)
        (omnidiff-token-delete
         :background ,delete-bg ,@uniform-fg)
        (omnidiff-token-update
         :background ,update-bg ,@uniform-fg)
        (omnidiff-token-move
         :background ,move-bg ,@uniform-fg)
        (omnidiff-token-cross-highlight
         :background ,cross-bg
         :underline (:color ,blue-cooler :style line)
         ,@uniform-fg)
        (omnidiff-token-active
         :background ,active-bg
         :box (:line-width -1 :color ,accent)
         ,@uniform-fg)
        ;; Dual viewer chrome & gutter faces
        (omnidiff-dual-header-before
         :inherit omnidiff-ui :background ,side-before-bg
         :foreground ,gutter-delete :weight semi-bold
         :underline (:color ,border :position t))
        (omnidiff-dual-header-after
         :inherit omnidiff-ui :background ,side-after-bg
         :foreground ,gutter-insert :weight semi-bold
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
        (omnidiff-dual-header-badge
         :inherit omnidiff-ui :background ,bg-card
         :foreground ,accent :weight semi-bold
         :underline (:color ,border :position t))
        (omnidiff-dual-fold-banner
         :inherit omnidiff-ui :background ,bg-chrome :foreground ,fg-dim
         :slant italic :overline ,fold-rule
         :underline (:color ,fold-rule :position t) :extend t)
        (omnidiff-dual-gutter-insert
         :inherit omnidiff-ui :foreground ,gutter-insert :weight bold)
        (omnidiff-dual-gutter-delete
         :inherit omnidiff-ui :foreground ,gutter-delete :weight bold)
        (omnidiff-dual-gutter-update
         :inherit omnidiff-ui :foreground ,gutter-update :weight bold)
        (omnidiff-dual-gutter-move
         :inherit omnidiff-ui :foreground ,gutter-move :weight bold)
        ;; Semantic summary & container faces
        (omnidiff-summary-badge
         :inherit omnidiff-ui :background ,bg-card :foreground ,accent
         :weight semi-bold :box (:line-width -1 :color ,border))
        (omnidiff-container-member-changed
         :inherit omnidiff-ui :foreground ,gutter-update :weight semi-bold)
        (omnidiff-container-member-added
         :inherit omnidiff-ui :foreground ,gutter-insert :weight semi-bold)
        (omnidiff-container-member-removed
         :inherit omnidiff-ui :foreground ,gutter-delete :weight semi-bold)))))

(defun omnidiff-theme--fallback-attrs (face)
  "Return the `(t ...)' fallback attribute plist from FACE's defface spec."
  (cdr (assq t (get face 'face-defface-spec))))

(defun omnidiff-theme--face-specs (p &optional light-palette)
  "Build face specification list from palette plist P.
Wraps hex-color specs under `((class color) (min-colors 88))' and preserves
each face's `(t ...)' fallback clause for 8-color/16-color TTY and monochrome
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
                   (fallback (omnidiff-theme--fallback-attrs face)))
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
                 (fallback (omnidiff-theme--fallback-attrs face)))
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
`:gutter-insert', `:gutter-delete', `:gutter-update', and `:gutter-move'.
Results are cached per theme revision and returned as a fresh copy."
  (let* ((modus (omnidiff-theme--active-modus-theme))
         (key (list omnidiff-theme--revision
                    modus
                    omnidiff-color-scheme
                    omnidiff-custom-palette
                    (frame-parameter nil 'background-mode)
                    (>= (or (display-color-cells) 0) 88))))
    (unless (and omnidiff-theme--semantic-colors-cache
                 (equal (car omnidiff-theme--semantic-colors-cache) key))
      (let* ((base (if modus
                       (omnidiff-theme--modus-base)
                     (omnidiff-theme--generic-base)))
             (palette (omnidiff-theme--resolve-palette base omnidiff-color-scheme))
             (result (cl-loop for k in omnidiff-theme--semantic-color-keys
                              nconc (list k (plist-get palette k)))))
        (setq omnidiff-theme--semantic-colors-cache
              (cons (copy-tree key) result))))
    (copy-sequence (cdr omnidiff-theme--semantic-colors-cache))))

(defun omnidiff-theme--on-disable-theme (_theme)
  "Reapply `omnidiff' faces after disabling a custom theme."
  (omnidiff-apply-theme))

(defun omnidiff-theme--on-first-rich-frame (frame)
  "Reapply `omnidiff' faces on the first >=88-color FRAME after daemon startup."
  (when (and (frame-live-p frame)
             (>= (or (display-color-cells frame) 0) 88))
    (remove-hook 'after-make-frame-functions
                 #'omnidiff-theme--on-first-rich-frame)
    (with-selected-frame frame
      (omnidiff-apply-theme nil t))))

;;;###autoload
(defun omnidiff-apply-theme (&optional theme force)
  "Apply active theme palette faces for `omnidiff-mode'.
When THEME is non-nil and not enabled in `custom-enabled-themes', do nothing.
Uses Modus palette values when a Modus theme is active, and derives colors
from the active Emacs faces on all other themes.  Updates only each managed
face's `face-defface-spec' so user (`custom-set-faces') and third-party
theme overrides are preserved.  Skips redundant re-application when the
effective theme state is unchanged unless FORCE is non-nil.  Return non-nil
when applied."
  (when (or (null theme) (memq theme custom-enabled-themes))
    (let* ((modus-current (omnidiff-theme--active-modus-theme))
           (use-modus (and modus-current
                           (or (null theme) (eq theme modus-current))))
           (cells (or (display-color-cells) 0))
           (rich-color-p (>= cells 88))
           (key (list (copy-sequence custom-enabled-themes)
                      modus-current
                      omnidiff-color-scheme
                      (copy-tree omnidiff-custom-palette)
                      omnidiff-dual-overlay-foreground
                      omnidiff-ui-font-height
                      (frame-parameter nil 'background-mode)
                      cells
                      (when rich-color-p
                        (list (omnidiff-theme--face-hex 'default :background)
                              (omnidiff-theme--face-hex 'default :foreground))))))
      (when (or force (not (equal key omnidiff-theme--last-applied-key)))
        (setq omnidiff-theme--last-applied-key key
              omnidiff-theme--revision (1+ omnidiff-theme--revision)
              omnidiff-theme--semantic-colors-cache nil)
        (if rich-color-p
            (remove-hook 'after-make-frame-functions
                         #'omnidiff-theme--on-first-rich-frame)
          (add-hook 'after-make-frame-functions
                    #'omnidiff-theme--on-first-rich-frame))
        (let* ((dual-branch-p (and (not use-modus)
                                   (or (not rich-color-p)
                                       (null custom-enabled-themes))))
               (specs
                (if dual-branch-p
                    (omnidiff-theme--face-specs
                     (omnidiff-theme--resolve-palette
                      (omnidiff-theme--generic-base 'dark) omnidiff-color-scheme)
                     (omnidiff-theme--resolve-palette
                      (omnidiff-theme--generic-base 'light) omnidiff-color-scheme))
                  (let* ((base (if use-modus
                                   (omnidiff-theme--modus-base)
                                 (omnidiff-theme--generic-base)))
                         (palette (omnidiff-theme--resolve-palette
                                   base omnidiff-color-scheme)))
                    (omnidiff-theme--face-specs palette)))))
          (dolist (entry specs)
            (face-spec-set (car entry) (cadr entry) 'face-defface-spec)))
        (run-hooks 'omnidiff-theme-after-apply-hook)
        t))))

(add-hook 'enable-theme-functions #'omnidiff-apply-theme)
(add-hook 'disable-theme-functions #'omnidiff-theme--on-disable-theme)
(add-hook 'modus-themes-after-load-theme-hook #'omnidiff-apply-theme)
(omnidiff-apply-theme)

(provide 'omnidiff-theme)
;;; omnidiff-theme.el ends here
