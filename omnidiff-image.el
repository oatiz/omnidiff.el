;;; omnidiff-image.el --- Visual image diffing for omnidiff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 oatiz

;; Author: oatiz <iamoatiz@gmail.com>
;; URL: https://github.com/oatiz/omnidiff.el
;; Keywords: vc, diff, tools, multimedia

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

;; Visual inline image comparison for `omnidiff-mode' across raster pictures
;; parsed from `omnidiff --mode json' and optional visual previews for `.svg'
;; files alongside their XML syntax diffs.
;;
;; Supported comparison modes:
;;  - `side-by-side': Two-up BEFORE | AFTER panes (honest narrow fallback).
;;  - `stacked': Vertical BEFORE then AFTER panes.
;;  - `swipe': Zero-redecode native `(slice ...)' split curtain with `h'/`l',
;;    arrow keys, mouse click/drag, and changed-region auto-aim.
;;  - `diff': High-contrast pixel mask via async ImageMagick `compare' (with
;;    adjustable `-fuzz' tolerance) and alpha-aware neutral-gray `librsvg'
;;    difference preview (with adjustable gain).
;;  - `onion': Alpha-correct adjustable opacity overlay via built-in `librsvg'.
;;  - `blink': Uniform-canvas in-place BEFORE <-> AFTER flip (`b') with
;;    non-destructive peek return (`DEL').
;;  - Changed-region visual overlay boxes and 1:1 / magnified local crop loupe.
;;  - TTY fallback: Sub-character delta bar, metadata summary, and toggleable
;;    changed-region list.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'subr-x)
(require 'omnidiff-engine)
(require 'omnidiff-theme)
(require 'transient)

(defvar omnidiff--repo-dir)
(defvar omnidiff--layout)
(defvar omnidiff--command)
(defvar omnidiff-default-layout)
(declare-function omnidiff-toggle-fold "omnidiff" ())
(declare-function omnidiff-cycle-layout "omnidiff" (&optional reset-pinned-p))
(declare-function omnidiff-open-in-tui "omnidiff" ())

;;; Customization

(defcustom omnidiff-image-enabled t
  "When non-nil, render inline visual image diffs in graphical frames."
  :type 'boolean
  :group 'omnidiff)

(defcustom omnidiff-image-unified-mode 'swipe
  "Visual comparison mode used for modified images in `unified' layout.
Automatically falls back to `side-by-side' when image aspect ratios differ."
  :type '(choice (const :tag "Swipe split curtain" swipe)
                 (const :tag "Side-by-side (2-up)" side-by-side)
                 (const :tag "Pixel diff highlight" diff)
                 (const :tag "Onion skin overlay" onion)
                 (const :tag "Blink (A/B toggle)" blink)
                 (const :tag "Stacked (vertical)" stacked))
  :group 'omnidiff)

(defcustom omnidiff-image-diff-backend 'auto
  "Backend used for `diff' pixel-highlight mode.
- `auto': Use ImageMagick (`magick' or `compare') asynchronously when
  available, displaying the built-in SVG color-delta preview immediately
  while the pixel mask computes, and falling back to `svg' when ImageMagick
  is absent.
- `magick': Prefer ImageMagick (`magick' / `compare'), falling back to `svg'.
- `svg': Pure Emacs built-in `librsvg' RGB+alpha difference preview."
  :type '(choice (const :tag "Auto (ImageMagick mask with SVG delta preview)" auto)
                 (const :tag "ImageMagick pixel mask" magick)
                 (const :tag "Built-in SVG delta preview" svg))
  :group 'omnidiff)

(defcustom omnidiff-image-diff-style 'spotlight-sbs
  "Visual style used for `4:diff' pixel comparison mode.
- `spotlight-sbs': Side-by-side BEFORE | AFTER spotlight (desaturated
  unchanged context, readable full-color changed regions with subtle
  warm-amber tint and crisp 1px outer contour; stacks on narrow windows).
- `spotlight': Single AFTER spotlight with desaturated unchanged context
  and illuminated changed regions.
- `mask': Classic high-contrast ImageMagick `compare' pixel mask.
- `delta': Pure Emacs built-in `librsvg' RGB+alpha difference preview."
  :type '(choice (const :tag "Side-by-side spotlight" spotlight-sbs)
                 (const :tag "Single spotlight" spotlight)
                 (const :tag "Classic pixel mask (magick compare)" mask)
                 (const :tag "Built-in SVG RGB+alpha delta" delta))
  :group 'omnidiff)

(defcustom omnidiff-image-diff-gain 4.0
  "Default amplification factor applied to built-in SVG difference output.
Higher values make subtle tonal changes easier to spot against black."
  :type 'number
  :group 'omnidiff)

(defcustom omnidiff-image-diff-fuzz 0
  "Default percentage fuzz tolerance passed to ImageMagick `compare'.
Non-zero values filter out subtle JPEG/recompression noise."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-image-backdrop 'auto
  "Default backdrop behind inline image previews.
- `auto': Subtle theme-adapted checkerboard for alpha/SVG images; clean
  framed canvas for opaque images.
- `checker': Always show subtle theme-adapted checkerboard.
- `light': Solid light backdrop.
- `dark': Solid dark backdrop.
- `none': No backdrop (raw frame background)."
  :type '(choice (const :tag "Auto (checkerboard for alpha/SVG)" auto)
                 (const :tag "Checkerboard" checker)
                 (const :tag "Light" light)
                 (const :tag "Dark" dark)
                 (const :tag "None" none))
  :group 'omnidiff)

(defcustom omnidiff-image-preview-svg t
  "When non-nil, render a collapsible visual preview above `.svg' XML diffs."
  :type 'boolean
  :group 'omnidiff)

(defcustom omnidiff-image-max-bytes (* 16 1024 1024)
  "Maximum compressed byte size per image side loaded automatically.
Larger images show a metadata placeholder until loaded explicitly with `L'."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-image-auto-load-limit 12
  "Maximum number of image cards auto-loaded inline per diff buffer.
Additional image cards render a compact summary until loaded with `L'."
  :type 'natnum
  :group 'omnidiff)

(defcustom omnidiff-image-max-height 0.65
  "Maximum rendered image height in pixels (integer) or window ratio (float)."
  :type '(choice (float :tag "Fraction of window height")
                 (integer :tag "Pixels"))
  :group 'omnidiff)

(defcustom omnidiff-image-swipe-step 0.05
  "Coarse increment for adjusting the swipe split ratio with `h'/`l'."
  :type 'float
  :group 'omnidiff)

(defcustom omnidiff-image-onion-step 0.1
  "Increment for adjusting onion-skin opacity with `h'/`l'.
Divides 1.0 evenly so stepping away from 50% always returns cleanly to 50%."
  :type 'float
  :group 'omnidiff)

(defcustom omnidiff-image-external-viewer nil
  "Optional command string or argv list used by `omnidiff-image-open-external'.
When nil, uses `open' on macOS, `xdg-open' on Linux/BSD, or `start' on Windows."
  :type '(choice (const :tag "System default" nil)
                 (string :tag "Executable name or path")
                 (repeat :tag "Command argv" string))
  :group 'omnidiff)

;;; Internal state & constants

(cl-defstruct (omnidiff-image--state (:copier nil))
  "Per-file interactive display state for an image diff card."
  (mode nil) ; nil (follow layout) | side-by-side | stacked | swipe | diff | onion | blink
  (mode-pinned nil :type boolean)
  (prev-mode nil)
  (swipe nil) ; nil (auto-aim from primary region or 0.5) | float 0.0..1.0
  (opacity 0.5 :type float)
  (diff-gain nil) ; nil (use `omnidiff-image-diff-gain') | float
  (diff-fuzz nil) ; nil (use `omnidiff-image-diff-fuzz') | integer
  (diff-style nil) ; nil (use `omnidiff-image-diff-style') | spotlight-sbs | spotlight | mask | delta
  (zoom nil) ; nil (fit) | float scale factor
  (backdrop nil) ; nil (use `omnidiff-image-backdrop') | auto | checker | light | dark | none
  (blink-side 'after :type symbol)
  (show-regions nil :type boolean)
  (tty-hide-regions nil :type boolean)
  (region-idx 0 :type integer)
  (expand-identical nil :type boolean)
  (deferred nil :type boolean)
  (force-load nil :type boolean))

(defconst omnidiff-image--svg-embeddable-mimes
  '("image/png" "image/jpeg" "image/gif" "image/webp" "image/svg+xml")
  "MIME types embeddable inside `librsvg' `<image href=data:...>' elements.")

(defconst omnidiff-image--png-magic
  (unibyte-string #x89 ?P ?N ?G #x0d #x0a #x1a #x0a)
  "8-byte PNG signature.")

(defconst omnidiff-image--zoom-steps
  '(0.25 0.5 0.75 1.0 1.5 2.0 3.0 4.0 6.0 8.0)
  "Ordered magnification factors stepped by `+' and `-'.")

(defconst omnidiff-image--gain-steps
  '(1.0 2.0 4.0 8.0 16.0)
  "Ordered amplification steps for SVG difference preview.")

(defconst omnidiff-image--fuzz-steps
  '(0 1 2 5 10 20)
  "Ordered percentage fuzz tolerances for ImageMagick `compare'.")

(defconst omnidiff-image--diff-styles
  '(spotlight-sbs spotlight mask delta)
  "Ordered visual styles cycled by `omnidiff-image-cycle-diff-style'.")

(defconst omnidiff-image--tty-blocks
  " ▏▎▍▌▋▊▉█"
  "Sub-character horizontal block glyphs for TTY delta bars.")

(defvar omnidiff-image--ext-tmpdirs nil
  "Temporary directories created for external image viewers, removed on exit.")

(defvar-local omnidiff-image--states nil
  "Hash table mapping file path key to `omnidiff-image--state'.")

(defvar-local omnidiff-image--bytes-cache nil
  "Hash table caching raw unibyte image data by source key.")

(defvar-local omnidiff-image--b64-cache nil
  "Hash table caching base64 strings by source key.")

(defvar-local omnidiff-image--img-cache nil
  "Hash table caching Emacs image descriptors for O(1) slice/blink reuse.")

(defvar-local omnidiff-image--onion-descs nil
  "Hash table mapping file key to the latest onion descriptor for bounded reuse.")

(defvar-local omnidiff-image--magick-cache nil
  "Hash table caching async ImageMagick diff PNG bytes (or `pending'/`failed').")

(defvar-local omnidiff-image--descriptors nil
  "Image descriptors created for the current buffer, flushed on cleanup.")

(defvar-local omnidiff-image--workers nil
  "List of `(PROC . TMPDIR)' for active async ImageMagick diff workers.")

(defvar-local omnidiff-image--render-auto-count 0
  "Counter of auto-loaded image cards during the current buffer render.")

(defvar-local omnidiff-image--last-viewport-w nil
  "Last rendered viewport pixel width for debounced resize reflow.")

(defvar-local omnidiff-image--resize-timer nil
  "Active debounce timer for window-resize image card reflow.")

(defvar-keymap omnidiff-image-header-map
  :doc "Keymap for image file/hunk headers (keeps horizontal motion free)."
  "i" #'omnidiff-image-cycle-mode
  "1" #'omnidiff-image-mode-side-by-side
  "2" #'omnidiff-image-mode-stacked
  "3" #'omnidiff-image-mode-swipe
  "4" #'omnidiff-image-mode-diff
  "5" #'omnidiff-image-mode-onion
  "6" #'omnidiff-image-mode-blink
  "b" #'omnidiff-image-toggle-side
  "DEL" #'omnidiff-image-return-from-peek
  "<backspace>" #'omnidiff-image-return-from-peek
  "=" #'omnidiff-image-adjust-reset
  "+" #'omnidiff-image-zoom-in
  "-" #'omnidiff-image-zoom-out
  "0" #'omnidiff-image-zoom-reset
  "t" #'omnidiff-image-cycle-backdrop
  "s" #'omnidiff-image-cycle-diff-style
  "r" #'omnidiff-image-toggle-regions
  ">" #'omnidiff-image-next-region
  "<" #'omnidiff-image-prev-region
  "o" #'omnidiff-image-open-side
  "x" #'omnidiff-image-open-external
  "L" #'omnidiff-image-force-load)

(defvar-keymap omnidiff-image-card-map
  :parent omnidiff-image-header-map
  :doc "Local keymap active on image diff card bodies in `omnidiff-mode'."
  "h" #'omnidiff-image-adjust-decrease
  "l" #'omnidiff-image-adjust-increase
  "<left>" #'omnidiff-image-adjust-decrease
  "<right>" #'omnidiff-image-adjust-increase
  "S-<left>" #'omnidiff-image-adjust-fine-decrease
  "S-<right>" #'omnidiff-image-adjust-fine-increase
  "," #'omnidiff-image-adjust-decrease
  "." #'omnidiff-image-adjust-increase)

(defun omnidiff-image--ensure-caches ()
  "Initialize buffer-local image state and caches if needed."
  (unless omnidiff-image--states
    (setq omnidiff-image--states (make-hash-table :test #'equal)))
  (unless omnidiff-image--bytes-cache
    (setq omnidiff-image--bytes-cache (make-hash-table :test #'equal)))
  (unless omnidiff-image--b64-cache
    (setq omnidiff-image--b64-cache (make-hash-table :test #'equal)))
  (unless omnidiff-image--img-cache
    (setq omnidiff-image--img-cache (make-hash-table :test #'equal)))
  (unless omnidiff-image--onion-descs
    (setq omnidiff-image--onion-descs (make-hash-table :test #'equal)))
  (unless omnidiff-image--magick-cache
    (setq omnidiff-image--magick-cache (make-hash-table :test #'equal))))

(defun omnidiff-image--state-for (file)
  "Return or create the `omnidiff-image--state' for `omnidiff-file' FILE."
  (omnidiff-image--ensure-caches)
  (let ((key (omnidiff-file-target-path file)))
    (or (gethash key omnidiff-image--states)
        (puthash key (make-omnidiff-image--state) omnidiff-image--states))))

(defun omnidiff-image--cancel-workers ()
  "Terminate any in-flight async ImageMagick workers and remove their temp dirs."
  (let ((workers omnidiff-image--workers))
    (setq omnidiff-image--workers nil)
    (dolist (entry workers)
      (let ((proc (car entry))
            (dir (cdr entry)))
        (when (and proc (process-live-p proc))
          (delete-process proc))
        (when (and dir (file-directory-p dir))
          (ignore-errors (delete-directory dir t)))))))

(defun omnidiff-image--cancel-resize-timer ()
  "Cancel any pending resize reflow timer owned by the current buffer."
  (when (timerp omnidiff-image--resize-timer)
    (cancel-timer omnidiff-image--resize-timer))
  (setq omnidiff-image--resize-timer nil))

(defun omnidiff-image--flush-descriptors ()
  "Flush cached Emacs image descriptors registered by the current buffer."
  (let ((descs omnidiff-image--descriptors))
    (setq omnidiff-image--descriptors nil)
    (when (and descs (fboundp 'image-flush))
      (dolist (img descs)
        (ignore-errors (image-flush img)))))
  (when omnidiff-image--onion-descs
    (maphash (lambda (_k img)
               (when (and img (fboundp 'image-flush))
                 (ignore-errors (image-flush img))))
             omnidiff-image--onion-descs)
    (clrhash omnidiff-image--onion-descs))
  (when omnidiff-image--img-cache
    (clrhash omnidiff-image--img-cache)))

(defun omnidiff-image-cleanup (&optional keep-bytes-p keep-states-p)
  "Release image workers, timers, descriptors, and caches for current buffer.
When KEEP-BYTES-P is non-nil, keep decoded blob bytes and per-file states.
When KEEP-STATES-P is non-nil, keep per-file interactive states while
flushing bytes, descriptors, and ImageMagick caches (used on refresh)."
  (omnidiff-image--cancel-workers)
  (omnidiff-image--cancel-resize-timer)
  (omnidiff-image--flush-descriptors)
  (unless keep-bytes-p
    (when omnidiff-image--bytes-cache (clrhash omnidiff-image--bytes-cache))
    (when omnidiff-image--b64-cache (clrhash omnidiff-image--b64-cache))
    (when omnidiff-image--magick-cache (clrhash omnidiff-image--magick-cache))
    (unless keep-states-p
      (when omnidiff-image--states (clrhash omnidiff-image--states)))))

(defun omnidiff-image-prune-states (files)
  "Drop saved states for paths absent from `omnidiff-file' list FILES."
  (when omnidiff-image--states
    (let ((live (make-hash-table :test #'equal)))
      (dolist (f files)
        (puthash (omnidiff-file-target-path f) t live))
      (let ((stale nil))
        (maphash (lambda (k _v)
                   (unless (gethash k live)
                     (push k stale)))
                 omnidiff-image--states)
        (dolist (k stale)
          (remhash k omnidiff-image--states))))))

(defun omnidiff-image-reset-layout-overrides (&optional force-p)
  "Clear per-card mode overrides so a global layout change applies.
By default preserves cards whose mode was explicitly pinned by the user;
when FORCE-P is non-nil, resets every card."
  (when omnidiff-image--states
    (maphash (lambda (_k st)
               (when (or force-p (not (omnidiff-image--state-mode-pinned st)))
                 (setf (omnidiff-image--state-mode st) nil
                       (omnidiff-image--state-mode-pinned st) nil
                       (omnidiff-image--state-prev-mode st) nil)))
             omnidiff-image--states)))

(defun omnidiff-image-begin-render ()
  "Reset per-render counters before rendering a buffer."
  (omnidiff-image--ensure-caches)
  (setq omnidiff-image--render-auto-count 0
        omnidiff-image--last-viewport-w (car (omnidiff-image--viewport-px))))

(defun omnidiff-image-seed-blobs (blob-table)
  "Pre-populate `omnidiff-image--bytes-cache' from OID hash table BLOB-TABLE."
  (when (hash-table-p blob-table)
    (omnidiff-image--ensure-caches)
    (maphash (lambda (oid bytes)
               (when (and (stringp oid) (stringp bytes) (> (length bytes) 0))
                 (puthash (cons 'oid oid) bytes omnidiff-image--bytes-cache)))
             blob-table)))

(defun omnidiff-image--cleanup-ext-tmpdirs ()
  "Remove temporary directories created for external image viewers on exit."
  (let ((dirs omnidiff-image--ext-tmpdirs))
    (setq omnidiff-image--ext-tmpdirs nil)
    (dolist (d dirs)
      (when (and d (file-directory-p d))
        (ignore-errors (delete-directory d t))))))

(add-hook 'kill-emacs-hook #'omnidiff-image--cleanup-ext-tmpdirs)

;;; Picture detection, metadata & formatting

(defun omnidiff-image--svg-path-p (path)
  "Return non-nil when PATH has an `.svg' extension (case-insensitive)."
  (and (stringp path)
       (let ((case-fold-search t))
         (string-match-p "\\.svg\\'" path))))

(defun omnidiff-image-path-looks-visual-p (path)
  "Return non-nil when PATH has a common raster or SVG image extension."
  (and (stringp path)
       (let ((case-fold-search t))
         (string-match-p
          "\\.\\(?:png\\|jpe?g\\|gif\\|webp\\|tiff?\\|bmp\\|ico\\|heic\\|avif\\|svg\\)\\'"
          path))))

(defun omnidiff-image-file-picture (file)
  "Return `omnidiff-picture' for FILE, or synthetic SVG preview, or nil."
  (or (omnidiff-file-picture file)
      (when (and omnidiff-image-preview-svg
                 (or (omnidiff-image--svg-path-p (omnidiff-file-target-path file))
                     (omnidiff-image--svg-path-p (omnidiff-file-path file))))
        (let ((st (omnidiff-file-status file)))
          (make-omnidiff-picture
           :before (unless (eq st 'added)
                     (make-omnidiff-picture-info :format "SVG"))
           :after (unless (eq st 'deleted)
                    (make-omnidiff-picture-info :format "SVG"))
           :comparison (if (memq st '(added deleted)) 'one-sided 'svg)
           :svg-p t)))))

(defun omnidiff-image-format-bytes (n)
  "Format byte count N as a concise human-readable string."
  (let ((abs-n (abs n))
        (sign (if (< n 0) "-" "")))
    (cond
     ((>= abs-n (* 1024 1024))
      (format "%s%.1f MB" sign (/ (float abs-n) (* 1024.0 1024.0))))
     ((>= abs-n 1024)
      (format "%s%.1f KB" sign (/ (float abs-n) 1024.0)))
     (t
      (format "%s%d B" sign abs-n)))))

(defun omnidiff-image--format-int-grouped (n)
  "Format non-negative integer N with thousands separators."
  (let* ((s (number-to-string n))
         (len (length s))
         (parts nil)
         (pos len))
    (while (> pos 3)
      (push (substring s (- pos 3) pos) parts)
      (setq pos (- pos 3)))
    (push (substring s 0 pos) parts)
    (string-join parts ",")))

(defun omnidiff-image--format-size-delta (before-bytes after-bytes)
  "Format byte delta between BEFORE-BYTES and AFTER-BYTES."
  (let ((delta (- after-bytes before-bytes)))
    (cond
     ((= delta 0) "±0 B")
     ((> before-bytes 0)
      (format "%s%s, %+.1f%%"
              (if (> delta 0) "+" "")
              (omnidiff-image-format-bytes delta)
              (* 100.0 (/ (float delta) (float before-bytes)))))
     (t
      (format "%s%s" (if (> delta 0) "+" "") (omnidiff-image-format-bytes delta))))))

(defun omnidiff-image-header-stats (file)
  "Return list of propertized stat fragments for FILE's header, or nil.
Returns nil for SVG files that already display text line-diff counters."
  (when-let* ((pic (omnidiff-file-picture file)))
    (unless (omnidiff-picture-svg-p pic)
      (cl-flet ((fmt-side (info)
                  (let ((fmt (omnidiff-picture-info-format info))
                        (w (omnidiff-picture-info-width info))
                        (h (omnidiff-picture-info-height info)))
                    (cond
                     ((and (> w 0) (> h 0))
                      (if (string-empty-p fmt)
                          (format "%d×%d" w h)
                        (format "%s %d×%d" fmt w h)))
                     ((not (string-empty-p fmt)) fmt)
                     (t nil)))))
        (let* ((b (omnidiff-picture-before pic))
               (a (omnidiff-picture-after pic))
               (comp (omnidiff-picture-comparison pic))
               (pct (omnidiff-picture-changed-pct pic))
               (fmt-dim
                (cond
                 ((and b a)
                  (let* ((bf (omnidiff-picture-info-format b))
                         (af (omnidiff-picture-info-format a))
                         (bw (omnidiff-picture-info-width b))
                         (bh (omnidiff-picture-info-height b))
                         (aw (omnidiff-picture-info-width a))
                         (ah (omnidiff-picture-info-height a))
                         (b-dim (and (> bw 0) (> bh 0)))
                         (a-dim (and (> aw 0) (> ah 0)))
                         (same-fmt (string= bf af))
                         (same-dim (and b-dim a-dim (= bw aw) (= bh ah))))
                    (cond
                     ((and b-dim a-dim same-fmt same-dim)
                      (format "%s %d×%d" af aw ah))
                     ((and b-dim a-dim same-dim)
                      (format "%s→%s %d×%d" bf af aw ah))
                     ((and b-dim a-dim same-fmt)
                      (format "%s %d×%d→%d×%d" af bw bh aw ah))
                     ((and b-dim a-dim)
                      (format "%s %d×%d→%s %d×%d" bf bw bh af aw ah))
                     ((and (not b-dim) (not a-dim))
                      (cond
                       ((and same-fmt (not (string-empty-p af))) af)
                       ((and (not (string-empty-p bf)) (not (string-empty-p af)))
                        (format "%s→%s" bf af))
                       ((not (string-empty-p af)) af)
                       ((not (string-empty-p bf)) bf)
                       (t nil)))
                     (t
                      (let ((bs (fmt-side b))
                            (as (fmt-side a)))
                        (if (and bs as) (format "%s→%s" bs as) (or as bs)))))))
                 (a (fmt-side a))
                 (b (fmt-side b))))
               (delta-badge
                (cond
                 ((and (numberp pct) (> pct 0.0))
                  (propertize (format "%.1f%% Δ" pct) 'face 'omnidiff-image-stat-delta))
                 ((eq comp 'identical)
                  (propertize "identical" 'face 'omnidiff-stat-index))
                 ((eq comp 'resized)
                  (propertize "resized" 'face 'omnidiff-image-stat-delta))
                 ((eq comp 'resized-scaled)
                  (propertize "scaled" 'face 'omnidiff-image-stat-delta))
                 ((eq comp 'unknown)
                  (let* ((raw (omnidiff-picture-raw-comparison pic))
                         (raw-ok (and (stringp raw)
                                      (not (string-empty-p raw))
                                      (not (string= raw "unknown"))))
                         (shown (when raw-ok
                                  (truncate-string-to-width raw 72 nil nil "…"))))
                    (cond
                     ((and shown (string-prefix-p "animation:" shown))
                      (propertize shown 'face 'omnidiff-image-stat-delta))
                     (shown
                      (propertize (format "unverified · %s" shown)
                                  'face 'omnidiff-stat-index))
                     (t
                      (propertize "unverified" 'face 'omnidiff-stat-index)))))))
               (size-badge
                (cond
                 ;; Neutral color for modified byte deltas: bigger isn't "good" and smaller isn't "deleted".
                 ((and b a)
                  (let ((b-sz (omnidiff-picture-info-bytes b))
                        (a-sz (omnidiff-picture-info-bytes a)))
                    (when (or (> b-sz 0) (> a-sz 0))
                      (let ((delta (- a-sz b-sz)))
                        (propertize (if (= delta 0)
                                        (omnidiff-image-format-bytes a-sz)
                                      (format "%s%s"
                                              (if (> delta 0) "+" "")
                                              (omnidiff-image-format-bytes delta)))
                                    'face 'omnidiff-stat-index)))))
                 ((and a (> (omnidiff-picture-info-bytes a) 0))
                  (propertize (omnidiff-image-format-bytes (omnidiff-picture-info-bytes a))
                              'face 'omnidiff-stat-added))
                 ((and b (> (omnidiff-picture-info-bytes b) 0))
                  (propertize (omnidiff-image-format-bytes (omnidiff-picture-info-bytes b))
                              'face 'omnidiff-stat-removed)))))
          (delq nil
                (list (when fmt-dim (propertize fmt-dim 'face 'omnidiff-stat-index))
                      delta-badge
                      size-badge)))))))

(defun omnidiff-image-hunk-summary (pic)
  "Build concise hunk header title and metadata text for `omnidiff-picture' PIC.
Return `(RANGE-LABEL . META-TEXT)' for the hunk header strip."
  (if (omnidiff-picture-svg-p pic)
      (cons "SVG preview" nil)
    (cl-flet ((fmt-one (info)
                (let* ((fmt (omnidiff-picture-info-format info))
                       (w (omnidiff-picture-info-width info))
                       (h (omnidiff-picture-info-height info))
                       (col (omnidiff-picture-info-color info))
                       (sz (omnidiff-picture-info-bytes info))
                       (head (string-join
                              (delq nil
                                    (list (unless (string-empty-p fmt) fmt)
                                          (when (and (> w 0) (> h 0))
                                            (format "%d×%d" w h))
                                          (when (and (stringp col)
                                                     (not (string-empty-p col)))
                                            col)))
                              " ")))
                  (string-join
                   (delq nil
                         (list (unless (string-empty-p head) head)
                               (when (> sz 0)
                                 (omnidiff-image-format-bytes sz))))
                   " · "))))
      (let* ((b (omnidiff-picture-before pic))
             (a (omnidiff-picture-after pic))
             (comp (omnidiff-picture-comparison pic))
             (pct (omnidiff-picture-changed-pct pic))
             (regions (omnidiff-picture-region-count pic))
             (label (cond
                     ((and b a) "Image diff")
                     (a "New image")
                     (t "Deleted image")))
             (parts
              (delq nil
                    (list
                     (cond
                      ((and b a)
                       (when (or (> (omnidiff-picture-info-bytes b) 0)
                                 (> (omnidiff-picture-info-bytes a) 0))
                         (format "%s → %s (%s)"
                                 (omnidiff-image-format-bytes (omnidiff-picture-info-bytes b))
                                 (omnidiff-image-format-bytes (omnidiff-picture-info-bytes a))
                                 (omnidiff-image--format-size-delta
                                  (omnidiff-picture-info-bytes b)
                                  (omnidiff-picture-info-bytes a)))))
                      (a (let ((s (fmt-one a)))
                           (unless (string-empty-p s) s)))
                      (b (let ((s (fmt-one b)))
                           (unless (string-empty-p s) s))))
                     (pcase comp
                       ('pixels
                        (format "%.2f%% pixels changed (%s %s)"
                                (or pct 0.0)
                                (omnidiff-image--format-int-grouped regions)
                                (if (= regions 1) "region" "regions")))
                       ('identical "no pixel changed")
                       ('resized "resized (different aspect ratio)")
                       ('resized-scaled
                        (format "resized (same aspect) · %.2f%% changed" (or pct 0.0)))
                       ('unknown
                        (let ((raw (omnidiff-picture-raw-comparison pic)))
                          (if (and (stringp raw)
                                   (not (string-empty-p raw))
                                   (not (string= raw "unknown")))
                              raw
                            "pixel comparison unavailable")))
                       (_ nil))))))
        (cons label (when parts (string-join parts " · ")))))))

;;; Image type detection, alpha & SVG dimension parsing

(defun omnidiff-image--detect-type (bytes &optional path)
  "Detect `(IMAGE-TYPE . MIME-STRING)' from unibyte BYTES and optional PATH."
  (when (and (stringp bytes) (> (length bytes) 0))
    (cond
     ((string-prefix-p omnidiff-image--png-magic bytes)
      '(png . "image/png"))
     ((and (>= (length bytes) 3)
           (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8) (= (aref bytes 2) #xff))
      '(jpeg . "image/jpeg"))
     ((or (string-prefix-p "GIF87a" bytes) (string-prefix-p "GIF89a" bytes))
      '(gif . "image/gif"))
     ((and (>= (length bytes) 12)
           (string-prefix-p "RIFF" bytes)
           (string= (substring bytes 8 12) "WEBP"))
      '(webp . "image/webp"))
     ((or (string-prefix-p (unibyte-string ?I ?I #x2a #x00) bytes)
          (string-prefix-p (unibyte-string ?M ?M #x00 #x2a) bytes))
      '(tiff . "image/tiff"))
     ((and (>= (length bytes) 12)
           (string= (substring bytes 4 8) "ftyp")
           (member (substring bytes 8 12) '("heic" "heix" "mif1" "msf1" "hevc" "hevs")))
      '(heic . "image/heic"))
     ((string-prefix-p "BM" bytes)
      '(bmp . "image/bmp"))
     ((and (>= (length bytes) 4)
           (string-prefix-p (unibyte-string #x00 #x00 #x01 #x00) bytes))
      '(ico . "image/x-icon"))
     ((let* ((prefix (substring bytes 0 (min 2048 (length bytes))))
             (case-fold-search t))
        (or (string-match-p "<svg\\b" prefix)
            (and path (omnidiff-image--svg-path-p path))))
      '(svg . "image/svg+xml"))
     (path
      (let ((ext (downcase (or (file-name-extension path) ""))))
        (pcase ext
          ("png" '(png . "image/png"))
          ((or "jpg" "jpeg") '(jpeg . "image/jpeg"))
          ("gif" '(gif . "image/gif"))
          ("webp" '(webp . "image/webp"))
          ((or "tif" "tiff") '(tiff . "image/tiff"))
          ("svg" '(svg . "image/svg+xml"))
          ("heic" '(heic . "image/heic"))
          ("bmp" '(bmp . "image/bmp"))
          ("ico" '(ico . "image/x-icon"))
          (_ nil)))))))

(defun omnidiff-image--png-has-alpha-p (bytes)
  "Return non-nil when PNG BYTES declare an alpha channel or `tRNS' chunk."
  (and (stringp bytes)
       (>= (length bytes) 26)
       (string-prefix-p omnidiff-image--png-magic bytes)
       (or (memq (aref bytes 25) '(4 6)) ; Grayscale+Alpha or RGBA in IHDR
           (let ((head (substring bytes 0 (min 4096 (length bytes)))))
             (not (null (string-search "tRNS" head)))))))

(defun omnidiff-image--has-alpha-p (pic b-bytes b-type a-bytes a-type)
  "Return non-nil when either image side may contain transparency."
  (or (omnidiff-picture-svg-p pic)
      (memq b-type '(svg gif ico webp))
      (memq a-type '(svg gif ico webp))
      (omnidiff-image--png-has-alpha-p b-bytes)
      (omnidiff-image--png-has-alpha-p a-bytes)
      (let ((case-fold-search t))
        (cl-some (lambda (info)
                   (when-let* ((col (and info (omnidiff-picture-info-color info))))
                     (string-match-p "rgba\\|\\`la[0-9]*\\'\\|alpha\\|graya" col)))
                 (list (omnidiff-picture-before pic)
                       (omnidiff-picture-after pic))))))

(defun omnidiff-image--parse-svg-dimensions (bytes)
  "Extract `(WIDTH HEIGHT KIND)' from SVG BYTES, or nil if unspecified.
KIND is `intrinsic' (explicit width/height) or `viewbox'."
  (when (stringp bytes)
    (let* ((head (substring bytes 0 (min 16384 (length bytes))))
           (case-fold-search t))
      (when (string-match "<svg\\b[^>]*>" head)
        (let ((tag (match-string 0 head))
              (vw nil) (vh nil)
              (iw nil) (ih nil))
          (when (string-match "\\bwidth=[\"']\\([0-9.]+\\)\\(?:px\\)?[\"']" tag)
            (setq iw (string-to-number (match-string 1 tag))))
          (when (string-match "\\bheight=[\"']\\([0-9.]+\\)\\(?:px\\)?[\"']" tag)
            (setq ih (string-to-number (match-string 1 tag))))
          (when (string-match
                 "viewBox=[\"'][ \t\n\r]*[-0-9.]+[ \t\n\r,]+[-0-9.]+[ \t\n\r,]+\\([0-9.]+\\)[ \t\n\r,]+\\([0-9.]+\\)"
                 tag)
            (setq vw (string-to-number (match-string 1 tag))
                  vh (string-to-number (match-string 2 tag))))
          (cond
           ((and iw ih (> iw 0) (> ih 0))
            (list (float iw) (float ih) 'intrinsic))
           ((and vw vh (> vw 0) (> vh 0))
            (list (float vw) (float vh) 'viewbox))
           (t nil)))))))

(defun omnidiff-image--probe-bytes-dims (bytes type)
  "Probe `(WIDTH . HEIGHT)' from header BYTES of image TYPE, or nil."
  (when (and (stringp bytes) (> (length bytes) 0))
    (let ((len (length bytes)))
      (cl-flet ((u16be (i) (logior (ash (aref bytes i) 8) (aref bytes (1+ i))))
                (u16le (i) (logior (aref bytes i) (ash (aref bytes (1+ i)) 8)))
                (u32be (i) (logior (ash (aref bytes i) 24)
                                   (ash (aref bytes (+ i 1)) 16)
                                   (ash (aref bytes (+ i 2)) 8)
                                   (aref bytes (+ i 3))))
                (u32le (i) (logior (aref bytes i)
                                   (ash (aref bytes (+ i 1)) 8)
                                   (ash (aref bytes (+ i 2)) 16)
                                   (ash (aref bytes (+ i 3)) 24))))
        (pcase type
          ('png
           (when (and (>= len 24)
                      (string-prefix-p omnidiff-image--png-magic bytes)
                      (string= (substring bytes 12 16) "IHDR"))
             (let ((w (u32be 16))
                   (h (u32be 20)))
               (when (and (> w 0) (> h 0))
                 (cons w h)))))
          ('gif
           (when (and (>= len 10)
                      (or (string-prefix-p "GIF87a" bytes)
                          (string-prefix-p "GIF89a" bytes)))
             (let ((w (u16le 6))
                   (h (u16le 8)))
               (when (and (> w 0) (> h 0))
                 (cons w h)))))
          ('bmp
           (when (and (>= len 22) (string-prefix-p "BM" bytes))
             (let ((dib (u32le 14)))
               (cond
                ((and (>= dib 40) (>= len 26))
                 (let* ((rw (u32le 18))
                        (rh (u32le 22))
                        (w (abs (if (>= rw #x80000000) (- rw #x100000000) rw)))
                        (h (abs (if (>= rh #x80000000) (- rh #x100000000) rh))))
                   (when (and (> w 0) (> h 0))
                     (cons w h))))
                ((= dib 12)
                 (let ((w (u16le 18))
                       (h (u16le 20)))
                   (when (and (> w 0) (> h 0))
                     (cons w h))))))))
          ('webp
           (when (and (>= len 25)
                      (string-prefix-p "RIFF" bytes)
                      (string= (substring bytes 8 12) "WEBP"))
             (let ((chunk (substring bytes 12 16)))
               (cond
                ((and (string= chunk "VP8X") (>= len 30))
                 (let ((w (1+ (logior (aref bytes 24)
                                      (ash (aref bytes 25) 8)
                                      (ash (aref bytes 26) 16))))
                       (h (1+ (logior (aref bytes 27)
                                      (ash (aref bytes 28) 8)
                                      (ash (aref bytes 29) 16)))))
                   (when (and (> w 0) (> h 0))
                     (cons w h))))
                ((and (string= chunk "VP8 ") (>= len 30)
                      (= (aref bytes 23) #x9d)
                      (= (aref bytes 24) #x01)
                      (= (aref bytes 25) #x2a))
                 (let ((w (logand (u16le 26) #x3fff))
                       (h (logand (u16le 28) #x3fff)))
                   (when (and (> w 0) (> h 0))
                     (cons w h))))
                ((and (string= chunk "VP8L") (= (aref bytes 20) #x2f))
                 (let* ((bits (u32le 21))
                        (w (1+ (logand bits #x3fff)))
                        (h (1+ (logand (ash bits -14) #x3fff))))
                   (when (and (> w 0) (> h 0))
                     (cons w h))))))))
          ('jpeg
           (when (and (>= len 4) (= (aref bytes 0) #xff) (= (aref bytes 1) #xd8))
             (let ((pos 2)
                   (found nil))
               (while (and (< pos len) (not found))
                 (while (and (< pos len) (/= (aref bytes pos) #xff))
                   (setq pos (1+ pos)))
                 (while (and (< pos len) (= (aref bytes pos) #xff))
                   (setq pos (1+ pos)))
                 (when (< pos len)
                   (let ((m (aref bytes pos)))
                     (setq pos (1+ pos))
                     (cond
                      ((or (= m #xd9) (= m #xda))
                       (setq pos len))
                      ((or (= m #x01) (and (>= m #xd0) (<= m #xd8)))
                       nil)
                      ((<= (+ pos 2) len)
                       (let ((seg-len (u16be pos)))
                         (if (and (memq m '(#xc0 #xc1 #xc2 #xc3 #xc5 #xc6 #xc7
                                                 #xc9 #xca #xcb #xcd #xce #xcf))
                                  (<= (+ pos 7) len))
                             (let ((h (u16be (+ pos 3)))
                                   (w (u16be (+ pos 5))))
                               (when (and (> w 0) (> h 0))
                                 (setq found (cons w h)))
                               (setq pos len))
                           (setq pos (if (>= seg-len 2) (+ pos seg-len) len)))))
                      (t (setq pos len))))))
               found)))
          ('svg
           (when-let* ((d (omnidiff-image--parse-svg-dimensions bytes)))
             (cons (round (nth 0 d)) (round (nth 1 d)))))
          (_ nil))))))

(defun omnidiff-image--resolve-dims (info bytes type)
  "Resolve `(WIDTH HEIGHT KIND)' for one image side from INFO or BYTES.
KIND is `pixels', `intrinsic', `viewbox', or `unknown'."
  (cond
   ((and info (> (omnidiff-picture-info-width info) 0)
         (> (omnidiff-picture-info-height info) 0))
    (list (omnidiff-picture-info-width info)
          (omnidiff-picture-info-height info)
          'pixels))
   ((eq type 'svg)
    (or (omnidiff-image--parse-svg-dimensions bytes)
        '(400 300 unknown)))
   ((when-let* ((dims (and bytes type (omnidiff-image--probe-bytes-dims bytes type))))
      (list (car dims) (cdr dims) 'pixels)))
   (t '(400 300 unknown))))

(defun omnidiff-image--format-dim-label (w h kind disp-w)
  "Format dimension and scale percentage label for W×H (KIND) drawn at DISP-W."
  (let* ((base (pcase kind
                 ('unknown "size unknown")
                 ('viewbox (format "viewBox %d×%d" (round w) (round h)))
                 (_ (format "%d×%d" (round w) (round h)))))
         (pct (when (and (not (eq kind 'unknown)) (> w 0) disp-w (> disp-w 0))
                (round (* 100.0 (/ (float disp-w) (float w)))))))
    (if (and pct (>= (abs (- pct 100)) 2))
        (format "%s (%d%%)" base pct)
      base)))

;;; Blob loading & batch prefetch

(defun omnidiff-image--expand-path (rel dir)
  "Expand repo-relative REL under DIR without treating leading `~' as HOME."
  (if (string-prefix-p "~" rel)
      (expand-file-name (concat "./" rel) dir)
    (expand-file-name rel dir)))

(defun omnidiff-image--valid-disk-path (path)
  "Return PATH when it is a non-empty existing regular file (not `/dev/null')."
  (when (and (stringp path)
             (not (string-empty-p path))
             (not (string= path "/dev/null"))
             (file-regular-p path))
    path))

(defun omnidiff-image--temp-path-p (path)
  "Return non-nil when PATH resides under `temporary-file-directory'."
  (when (and (stringp path) (not (string-empty-p path)))
    (let ((norm (file-truename (expand-file-name path)))
          (tmp (file-name-as-directory
                (file-truename (expand-file-name temporary-file-directory))))
          (sys-tmp (when (file-directory-p "/tmp")
                     (file-name-as-directory (file-truename "/tmp")))))
      (or (string-prefix-p tmp norm)
          (and sys-tmp (string-prefix-p sys-tmp norm))))))

(defun omnidiff-image--lfs-pointer-p (raw)
  "Return non-nil when unibyte string RAW looks like a Git LFS pointer."
  (and (stringp raw)
       (< (length raw) 512)
       (or (string-prefix-p "version https://git-lfs.github.com/spec/v1\n" raw)
           (string-prefix-p "version https://git-lfs.github.com/spec/v1\r\n" raw))))

(defun omnidiff-image--lfs-pointer-declared-size (raw)
  "Return declared integer byte size from Git LFS pointer RAW, or nil."
  (when (and (stringp raw)
             (let ((case-fold-search nil))
               (string-match "^size \\([0-9]+\\)\r?$" raw)))
    (string-to-number (match-string 1 raw))))

(defun omnidiff-image--disk-file-lfs-pointer-p (path)
  "Return non-nil when regular disk file PATH starts with a Git LFS pointer."
  (when (omnidiff-image--valid-disk-path path)
    (let* ((file-name-handler-alist nil)
           (prefix (ignore-errors
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally path nil 0 512)
                       (buffer-string)))))
      (omnidiff-image--lfs-pointer-p prefix))))

(defun omnidiff-image--smudge-disk-lfs-if-needed (raw dir force-p)
  "Smudge RAW if it is a Git LFS pointer read from disk under DIR."
  (if (omnidiff-image--lfs-pointer-p raw)
      (let ((declared (omnidiff-image--lfs-pointer-declared-size raw)))
        (cond
         ((and (not force-p) declared (> omnidiff-image-max-bytes 0)
               (> declared omnidiff-image-max-bytes))
          'too-large)
         ((and dir (file-directory-p dir) (executable-find "git"))
          (let ((default-directory dir)
                (coding-system-for-read 'no-conversion)
                (coding-system-for-write 'no-conversion))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (if (zerop (call-process-region raw nil "git" nil '(t nil) nil
                                              "lfs" "smudge"))
                  (let ((smudged (buffer-string)))
                    (if (and (not force-p) (> omnidiff-image-max-bytes 0)
                             (> (length smudged) omnidiff-image-max-bytes))
                        'too-large
                      smudged))
                raw))))
         (t raw)))
    raw))

(defun omnidiff-image--read-file-bytes (path force-p &optional lfs-dir)
  "Read regular file PATH as a unibyte string, `too-large', or `missing'.
Respects `omnidiff-image-max-bytes' (unless FORCE-P) and smudges Git LFS
pointers using LFS-DIR or PATH's parent directory when available."
  (if (not (omnidiff-image--valid-disk-path path))
      'missing
    (let* ((abs (expand-file-name path))
           (attrs (file-attributes (file-truename abs)))
           (sz (and attrs (file-attribute-size attrs))))
      (if (and (not force-p) sz (> omnidiff-image-max-bytes 0)
               (> sz omnidiff-image-max-bytes))
          'too-large
        (let* ((file-name-handler-alist nil)
               (raw (with-temp-buffer
                      (set-buffer-multibyte nil)
                      (insert-file-contents-literally abs)
                      (buffer-string))))
          (omnidiff-image--smudge-disk-lfs-if-needed
           raw (or lfs-dir (file-name-directory abs)) force-p))))))

(defun omnidiff-image--git-blob-size (oid repo-dir)
  "Return integer byte size of Git blob OID in REPO-DIR, or nil if unavailable."
  (let ((default-directory repo-dir))
    (with-temp-buffer
      (when (zerop (call-process "git" nil t nil "cat-file" "-s" oid))
        (let ((n (string-to-number (string-trim (buffer-string)))))
          (when (>= n 0) n))))))

(defun omnidiff-image--smudge-lfs-if-needed (raw oid path repo-dir force-p)
  "Smudge RAW if it is a Git LFS pointer for OID/PATH in REPO-DIR."
  (if (and (omnidiff-image--lfs-pointer-p raw) path)
      (let ((declared (omnidiff-image--lfs-pointer-declared-size raw)))
        (if (and (not force-p) declared (> omnidiff-image-max-bytes 0)
                 (> declared omnidiff-image-max-bytes))
            'too-large
          (let ((default-directory repo-dir)
                (coding-system-for-read 'no-conversion))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (if (zerop (call-process "git" nil t nil
                                       "cat-file" "--filters"
                                       (concat "--path=" path) oid))
                  (let ((smudged (buffer-string)))
                    (if (and (not force-p) (> omnidiff-image-max-bytes 0)
                             (> (length smudged) omnidiff-image-max-bytes))
                        'too-large
                      smudged))
                raw)))))
    raw))

(defun omnidiff-image--read-git-blob (oid path repo-dir force-p &optional known-bytes)
  "Read Git blob OID in REPO-DIR as a unibyte string, `too-large', or `missing'.
Automatically smudges Git LFS pointers via `git cat-file --filters' when needed."
  (let ((sz (or (and (integerp known-bytes) (> known-bytes 0) known-bytes)
                (omnidiff-image--git-blob-size oid repo-dir))))
    (if (and (not force-p) sz (> omnidiff-image-max-bytes 0)
             (> sz omnidiff-image-max-bytes))
        'too-large
      (let ((default-directory repo-dir)
            (coding-system-for-read 'no-conversion))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (if (not (zerop (call-process "git" nil t nil "cat-file" "blob" oid)))
              'missing
            (omnidiff-image--smudge-lfs-if-needed
             (buffer-string) oid path repo-dir force-p)))))))

(defun omnidiff-image-prefetch-blobs (files repo-dir)
  "Batch-prefetch uncached Git image blobs for FILES in REPO-DIR.
Uses one `git cat-file --batch' invocation instead of per-side spawns for
the first `omnidiff-image-auto-load-limit' GUI image cards."
  (when (and omnidiff-image-enabled repo-dir
             (omnidiff-image--display-graphic-p)
             (file-directory-p repo-dir))
    (omnidiff-image--ensure-caches)
    (let ((oids nil)
          (oid-paths (make-hash-table :test #'equal))
          (count 0)
          (limit (if (> omnidiff-image-auto-load-limit 0)
                     omnidiff-image-auto-load-limit
                   most-positive-fixnum)))
      (dolist (f files)
        (when-let* ((pic (omnidiff-image-file-picture f)))
          (when (and (omnidiff-image-should-render-preview-p pic)
                     (or (< count limit)
                         (omnidiff-image--state-force-load
                          (omnidiff-image--state-for f))))
            (setq count (1+ count))
            (dolist (pair (list (list (omnidiff-file-old-oid f)
                                      (omnidiff-file-path f)
                                      (omnidiff-picture-before pic))
                                (list (omnidiff-file-new-oid f)
                                      (omnidiff-file-target-path f)
                                      (omnidiff-picture-after pic))))
              (pcase-let ((`(,oid ,rel ,info) pair))
                (when (and oid
                           (not (gethash (cons 'oid oid) omnidiff-image--bytes-cache))
                           (not (gethash oid oid-paths))
                           (or (<= omnidiff-image-max-bytes 0)
                               (null info)
                               (<= (omnidiff-picture-info-bytes info)
                                   omnidiff-image-max-bytes)))
                  (puthash oid rel oid-paths)
                  (push oid oids)))))))
      (when oids
        (setq oids (nreverse oids))
        (let ((default-directory repo-dir)
              (coding-system-for-read 'no-conversion)
              (coding-system-for-write 'no-conversion)
              (cache omnidiff-image--bytes-cache))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (let* ((in-str (concat (string-join oids "\n") "\n"))
                   (rc (call-process-region
                        in-str nil "git" nil t nil "cat-file" "--batch")))
              (when (zerop rc)
                (goto-char (point-min))
                (dolist (oid oids)
                  (when (< (point) (point-max))
                    (let* ((eol (line-end-position))
                           (hdr (split-string
                                 (buffer-substring-no-properties (point) eol) " ")))
                      (forward-line 1)
                      (unless (equal (nth 1 hdr) "missing")
                        (let* ((sz (string-to-number (or (nth 2 hdr) "0")))
                               (start (point))
                               (end (min (point-max) (+ start sz))))
                          (if (and (> omnidiff-image-max-bytes 0)
                                   (> sz omnidiff-image-max-bytes))
                              (puthash (cons 'oid oid) 'too-large cache)
                            (let* ((raw (buffer-substring-no-properties start end))
                                   (data (omnidiff-image--smudge-lfs-if-needed
                                          raw oid (gethash oid oid-paths)
                                          repo-dir nil)))
                              (puthash (cons 'oid oid) data cache)))
                          (goto-char (min (point-max) (1+ end))))))))))))))))

(defun omnidiff-image--side-source-key (file side repo-dir)
  "Return stable cache key for SIDE (`before' or `after') of FILE in REPO-DIR."
  (let* ((oid (if (eq side 'before)
                  (omnidiff-file-old-oid file)
                (omnidiff-file-new-oid file)))
         (disk-path (omnidiff-image--valid-disk-path
                     (if (eq side 'before)
                         (omnidiff-file-before-disk-path file)
                       (omnidiff-file-after-disk-path file))))
         (rel (if (eq side 'before)
                  (omnidiff-file-path file)
                (omnidiff-file-target-path file))))
    (cond
     (oid (cons 'oid oid))
     (disk-path
      (let* ((abs (expand-file-name disk-path))
             (attrs (file-attributes (file-truename abs))))
        (list 'file abs (and attrs (file-attribute-modification-time attrs))
              (and attrs (file-attribute-size attrs)))))
     ((and (eq side 'after) repo-dir rel (not (string-empty-p rel)))
      (let* ((abs (omnidiff-image--expand-path rel repo-dir))
             (attrs (file-attributes abs)))
        (list 'file abs (and attrs (file-attribute-modification-time attrs))
              (and attrs (file-attribute-size attrs)))))
     (t nil))))

(defun omnidiff-image--fetch-side-bytes (file pic side repo-dir force-p)
  "Load unibyte image bytes for SIDE (`before' or `after') of FILE.
Returns a unibyte string, `too-large', `missing', or nil when side is absent."
  (let ((st (omnidiff-file-status file)))
    (unless (or (and (eq side 'before) (eq st 'added))
                (and (eq side 'after) (eq st 'deleted)))
      (omnidiff-image--ensure-caches)
      (let* ((oid (if (eq side 'before)
                      (omnidiff-file-old-oid file)
                    (omnidiff-file-new-oid file)))
             (disk-path (omnidiff-image--valid-disk-path
                         (if (eq side 'before)
                             (omnidiff-file-before-disk-path file)
                           (omnidiff-file-after-disk-path file))))
             (rel (if (eq side 'before)
                      (omnidiff-file-path file)
                    (omnidiff-file-target-path file)))
             (abs (when (and repo-dir rel (not (string-empty-p rel)))
                    (omnidiff-image--expand-path rel repo-dir)))
             (info (when pic
                     (if (eq side 'before)
                         (omnidiff-picture-before pic)
                       (omnidiff-picture-after pic))))
             (known-sz (and info (omnidiff-picture-info-bytes info)))
             (key (omnidiff-image--side-source-key file side repo-dir))
             (cached (and key (gethash key omnidiff-image--bytes-cache))))
        (cond
         ((and (stringp cached) (> (length cached) 0))
          cached)
         ((and (eq cached 'too-large) (not force-p))
          'too-large)
         (t
          (let ((data
                 (cond
                  ((and oid repo-dir)
                   (let ((blob (omnidiff-image--read-git-blob
                                oid rel repo-dir force-p known-sz)))
                     ;; Unpersisted working-tree hashes (e.g. `git add -N') fall back to disk.
                     (if (and (eq blob 'missing) (eq side 'after) (or disk-path abs))
                         (omnidiff-image--read-file-bytes (or disk-path abs) force-p repo-dir)
                       blob)))
                  ((and (null oid) disk-path)
                   (omnidiff-image--read-file-bytes disk-path force-p repo-dir))
                  ((and (eq side 'after) abs)
                   (omnidiff-image--read-file-bytes abs force-p repo-dir))
                  (t 'missing))))
            (when key
              (puthash key data omnidiff-image--bytes-cache))
            data)))))))

(defun omnidiff-image--base64 (cache-key bytes)
  "Return cached single-line base64 encoding of unibyte BYTES under CACHE-KEY."
  (omnidiff-image--ensure-caches)
  (or (and cache-key (gethash cache-key omnidiff-image--b64-cache))
      (let ((enc (base64-encode-string bytes t)))
        (when cache-key
          (puthash cache-key enc omnidiff-image--b64-cache))
        enc)))

;;; Geometry, theme colors, backdrops & descriptors

(defun omnidiff-image--display-graphic-p ()
  "Return non-nil when the current buffer's target frame supports GUI images."
  (let* ((win (or (get-buffer-window (current-buffer) t)
                  (selected-window)))
         (frame (if win (window-frame win) (selected-frame))))
    (display-graphic-p frame)))

(defun omnidiff-image--viewport-px ()
  "Return `(AVAIL-WIDTH-PX . MAX-HEIGHT-PX)' for the narrowest visible window."
  (let* ((wins (or (get-buffer-window-list (current-buffer) nil t)
                   (list (selected-window))))
         (gui-p (omnidiff-image--display-graphic-p))
         (win-w (cl-reduce #'min
                           (mapcar (lambda (w)
                                     (if (and gui-p w)
                                         (window-body-width w t)
                                       (* (or (and w (window-body-width w)) 120) 8)))
                                   wins)))
         (win-h (cl-reduce #'min
                           (mapcar (lambda (w)
                                     (if (and gui-p w)
                                         (window-body-height w t)
                                       (* (or (and w (window-body-height w)) 40) 16)))
                                   wins)))
         (avail-w (max 80 (- win-w 40)))
         (max-h (max 120
                     (if (floatp omnidiff-image-max-height)
                         (round (* win-h (max 0.2 (min 0.95 omnidiff-image-max-height))))
                       omnidiff-image-max-height))))
    (cons avail-w max-h)))

(defun omnidiff-image--avail-cols ()
  "Return approximate character column width of the narrowest visible window."
  (let* ((wins (or (get-buffer-window-list (current-buffer) nil t)
                   (list (selected-window)))))
    (cl-reduce #'min (mapcar (lambda (w) (or (and w (window-body-width w)) 100)) wins))))

(defun omnidiff-image--fit-size (src-w src-h max-w max-h &optional allow-upscale-p zoom)
  "Compute rendered `(DISP-W . DISP-H)' for SRC-W × SRC-H within MAX-W × MAX-H.
When ZOOM is a positive number, render at `SRC * ZOOM' (clamped to 4096 px)."
  (let* ((sw (float (if (and src-w (> src-w 0)) src-w 400)))
         (sh (float (if (and src-h (> src-h 0)) src-h 300))))
    (if (and (numberp zoom) (> zoom 0.0))
        (let* ((max-dim 4096.0)
               (clamped-scale (min (float zoom)
                                   (/ max-dim (max 1.0 sw))
                                   (/ max-dim (max 1.0 sh)))))
          (cons (max 1 (round (* sw clamped-scale)))
                (max 1 (round (* sh clamped-scale)))))
      (let* ((cap-w (if (and allow-upscale-p (< sw 160.0) (< sh 160.0))
                        (min max-w 256)
                      max-w))
             (cap-h (if (and allow-upscale-p (< sw 160.0) (< sh 160.0))
                        (min max-h 256)
                      max-h))
             (scale-w (/ (float (max 1 cap-w)) sw))
             (scale-h (/ (float (max 1 cap-h)) sh))
             (scale (min scale-w scale-h))
             (eff-scale (if allow-upscale-p scale (min 1.0 scale))))
        (cons (max 1 (round (* sw eff-scale)))
              (max 1 (round (* sh eff-scale))))))))

(defun omnidiff-image--compatible-overlay-p (pic bw bh aw ah)
  "Return non-nil when BEFORE and AFTER dimensions align for swipe/onion/diff."
  (cond
   ((eq (omnidiff-picture-comparison pic) 'resized) nil)
   ((eq (omnidiff-picture-comparison pic) 'resized-scaled) t)
   ((and (> bw 0) (> bh 0) (> aw 0) (> ah 0))
    (let ((ar-b (/ (float bw) (float bh)))
          (ar-a (/ (float aw) (float ah))))
      (<= (/ (abs (- ar-b ar-a)) (max ar-b ar-a)) 0.02)))
   ((omnidiff-picture-svg-p pic) t)
   (t nil)))

(defun omnidiff-image--dark-background-p ()
  "Return non-nil when the current frame uses a dark background."
  (let ((bg (omnidiff-theme-color-hex (face-background 'default nil t))))
    (if bg
        (omnidiff-theme--dark-color-p bg)
      (eq (frame-parameter nil 'background-mode) 'dark))))

(defun omnidiff-image--accent-hex ()
  "Resolve CSS hex color matching `omnidiff-file-accent'."
  (or (omnidiff-theme-color-hex (face-foreground 'omnidiff-file-accent nil t))
      (if (omnidiff-image--dark-background-p) "#61afef" "#2454a8")))

(defun omnidiff-image--frame-hex ()
  "Resolve subtle CSS hex border color from `omnidiff-image-frame'."
  (or (omnidiff-theme-color-hex (face-foreground 'omnidiff-image-frame nil t))
      (if (omnidiff-image--dark-background-p) "#3d4252" "#c8cbd4")))

(defun omnidiff-image--diff-highlight-hex ()
  "Resolve theme-harmonized CSS hex color for pixel-diff highlights."
  (or (omnidiff-theme-color-hex (face-foreground 'omnidiff-image-diff-highlight nil t))
      (if (omnidiff-image--dark-background-p) "#e65c8e" "#c82d63")))

(defun omnidiff-image--bg-hex ()
  "Resolve CSS hex color for the default buffer background."
  (or (omnidiff-theme-color-hex (face-background 'default nil t))
      (if (omnidiff-image--dark-background-p) "#181a20" "#ffffff")))

(defun omnidiff-image--effective-backdrop (state has-alpha-p)
  "Resolve active backdrop symbol (`checker' | `light' | `dark' | `none')."
  (let ((pref (or (omnidiff-image--state-backdrop state) omnidiff-image-backdrop)))
    (if (eq pref 'auto)
        (if has-alpha-p 'checker 'none)
      pref)))

(defun omnidiff-image--backdrop-svg-defs (backdrop dark-p)
  "Return `(DEFS-XML . FILL-VALUE)' for BACKDROP on DARK-P frame."
  (pcase backdrop
    ('checker
     (let ((c1 (if dark-p "#1e2028" "#eff0f4"))
           (c2 (if dark-p "#272a34" "#ffffff")))
       (cons (format
              (concat
               "<pattern id=\"chk\" width=\"16\" height=\"16\" patternUnits=\"userSpaceOnUse\">"
               "<rect width=\"16\" height=\"16\" fill=\"%s\"/>"
               "<rect width=\"8\" height=\"8\" fill=\"%s\"/>"
               "<rect x=\"8\" y=\"8\" width=\"8\" height=\"8\" fill=\"%s\"/>"
               "</pattern>")
              c1 c2 c2)
             "url(#chk)")))
    ('light (cons "" "#f6f7fa"))
    ('dark (cons "" "#15171e"))
    (_ (cons "" (omnidiff-image--bg-hex)))))

(defun omnidiff-image--frame-face ()
  "Return subtle 1px frame face spec matching the active theme."
  (list :box (list :line-width '(1 . 1) :color (omnidiff-image--frame-hex))))

(defun omnidiff-image--register-descriptor (img)
  "Track IMG in `omnidiff-image--descriptors' and return IMG."
  (when img
    (push img omnidiff-image--descriptors))
  img)

(defun omnidiff-image--svg-embeddable-p (mime)
  "Return non-nil when Emacs supports SVG and MIME embeds in `librsvg'."
  (and (image-type-available-p 'svg)
       (member mime omnidiff-image--svg-embeddable-mimes)))

(defun omnidiff-image--region-boxes-svg (regions active-idx src-w src-h disp-w disp-h)
  "Build SVG `<rect>' overlays for REGIONS mapped from SRC to DISP coordinates."
  (when (and regions (> src-w 0) (> src-h 0))
    (let* ((sx (/ (float disp-w) (float src-w)))
           (sy (/ (float disp-h) (float src-h)))
           (hi (omnidiff-image--diff-highlight-hex))
           (idx 0)
           (parts nil))
      (dolist (r regions)
        (let* ((raw-x (* (omnidiff-picture-region-x r) sx))
               (raw-y (* (omnidiff-picture-region-y r) sy))
               (raw-w (* (omnidiff-picture-region-width r) sx))
               (raw-h (* (omnidiff-picture-region-height r) sy))
               (min-sz 12.0)
               (w (max min-sz raw-w))
               (h (max min-sz raw-h))
               (x (max 1.0 (min (- disp-w w 1.0) (- raw-x (/ (- w raw-w) 2.0)))))
               (y (max 1.0 (min (- disp-h h 1.0) (- raw-y (/ (- h raw-h) 2.0)))))
               (selected-p (= idx active-idx)))
          (push (if selected-p
                    (format
                     (concat
                      "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" "
                      "fill=\"none\" stroke=\"rgba(0,0,0,0.55)\" stroke-width=\"3.5\"/>"
                      "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" "
                      "fill=\"none\" stroke=\"%s\" stroke-width=\"1.8\"/>")
                     x y w h x y w h hi)
                  (format
                   (concat
                    "<rect x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\" "
                    "fill=\"none\" stroke=\"%s\" stroke-width=\"1.2\" "
                    "stroke-dasharray=\"4 2\" opacity=\"0.78\"/>")
                   x y w h hi))
                parts))
        (setq idx (1+ idx)))
      (apply #'concat (nreverse parts)))))

(defun omnidiff-image--make-native (cache-key bytes type max-w max-h
                                              &optional exact-size-p ascent)
  "Create or reuse a cached native Emacs image descriptor for BYTES of TYPE."
  (when (and bytes type (image-type-available-p type))
    (omnidiff-image--ensure-caches)
    (let* ((asc (or ascent 'center))
           (key (list 'native cache-key type max-w max-h exact-size-p asc)))
      (or (gethash key omnidiff-image--img-cache)
          (when-let* ((img (if exact-size-p
                               (create-image bytes type t
                                             :width max-w :height max-h
                                             :scale 1 :ascent asc)
                             (create-image bytes type t
                                           :max-width max-w :max-height max-h
                                           :scale 1 :ascent asc))))
            (omnidiff-image--register-descriptor img)
            (puthash key img omnidiff-image--img-cache))))))

(defun omnidiff-image--make-view-image (cache-key bytes type mime
                                                  src-w src-h disp-w disp-h
                                                  &optional exact-size-p ascent
                                                  backdrop regions active-ridx)
  "Create a cached image descriptor with optional backdrop, regions, or pixelation.
Falls back cleanly to native `create-image' when no SVG composition is needed."
  (let* ((pixelated-p (and (not (eq type 'svg))
                           (> src-w 0) (> disp-w src-w)))
         (need-svg (and (omnidiff-image--svg-embeddable-p mime)
                        (or (and backdrop (not (eq backdrop 'none)))
                            regions
                            pixelated-p))))
    (if (not need-svg)
        (omnidiff-image--make-native cache-key bytes type disp-w disp-h
                                     exact-size-p ascent)
      (omnidiff-image--ensure-caches)
      (let* ((dark-p (omnidiff-image--dark-background-p))
             (asc (or ascent 'center))
             (hi (when regions (omnidiff-image--diff-highlight-hex)))
             (key (list 'svg-wrap cache-key mime disp-w disp-h asc
                        backdrop dark-p pixelated-p
                        (and regions (length regions)) active-ridx hi)))
        (or (gethash key omnidiff-image--img-cache)
            (pcase-let* ((`(,defs-xml . ,fill-val)
                          (omnidiff-image--backdrop-svg-defs backdrop dark-p))
                         (b64 (omnidiff-image--base64 cache-key bytes))
                         (bg-rect (if (and backdrop (not (eq backdrop 'none)))
                                      (format "<rect width=\"%d\" height=\"%d\" fill=\"%s\"/>"
                                              disp-w disp-h fill-val)
                                    ""))
                         (render-attr (if pixelated-p
                                          " style=\"image-rendering: pixelated\""
                                        ""))
                         (boxes (or (omnidiff-image--region-boxes-svg
                                     regions (or active-ridx 0)
                                     src-w src-h disp-w disp-h)
                                    ""))
                         (svg (format
                               (concat
                                "<svg xmlns=\"http://www.w3.org/2000/svg\" "
                                "width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">"
                                "%s%s"
                                "<image href=\"data:%s;base64,%s\" "
                                "width=\"%d\" height=\"%d\" preserveAspectRatio=\"none\"%s/>"
                                "%s</svg>")
                               disp-w disp-h disp-w disp-h
                               (if (string-empty-p defs-xml) "" (concat "<defs>" defs-xml "</defs>"))
                               bg-rect
                               mime b64 disp-w disp-h render-attr
                               boxes))
                         (img (create-image svg 'svg t :scale 1 :ascent asc)))
              (when img
                (omnidiff-image--register-descriptor img)
                (puthash key img omnidiff-image--img-cache))))))))

(defun omnidiff-image--make-swipe-handle (height-px width-px)
  "Return a dual-contrast vertical divider SVG image of WIDTH-PX × HEIGHT-PX."
  (when (image-type-available-p 'svg)
    (omnidiff-image--ensure-caches)
    (let* ((color (omnidiff-image--accent-hex))
           (w (max 1 width-px))
           (key (list 'swipe-handle color w height-px)))
      (or (gethash key omnidiff-image--img-cache)
          (let* ((mid-y (/ height-px 2))
                 (svg (if (>= w 3)
                          (format
                           (concat
                            "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\">"
                            "<rect x=\"0\" y=\"0\" width=\"1\" height=\"%d\" fill=\"rgba(0,0,0,0.55)\"/>"
                            "<rect x=\"1\" y=\"0\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
                            "<rect x=\"%d\" y=\"0\" width=\"1\" height=\"%d\" fill=\"rgba(0,0,0,0.55)\"/>"
                            "<rect x=\"0\" y=\"%d\" width=\"%d\" height=\"18\" rx=\"1.5\" fill=\"%s\"/>"
                            "</svg>")
                           w height-px
                           height-px
                           (- w 2) height-px "#ffffff"
                           (1- w) height-px
                           (max 0 (- mid-y 9)) w color)
                        (format
                         "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"1\" height=\"%d\"><rect width=\"1\" height=\"%d\" fill=\"%s\"/></svg>"
                         height-px height-px color)))
                 (img (create-image svg 'svg t :scale 1 :ascent 'center)))
            (when img
              (omnidiff-image--register-descriptor img)
              (puthash key img omnidiff-image--img-cache)))))))

(defun omnidiff-image--make-pane-divider (height-px)
  "Return a full-height 1px vertical pane divider SVG for 2-up view."
  (when (image-type-available-p 'svg)
    (omnidiff-image--ensure-caches)
    (let* ((color (omnidiff-image--frame-hex))
           (h (max 1 height-px))
           (key (list 'pane-divider color h)))
      (or (gethash key omnidiff-image--img-cache)
          (let* ((svg (format
                       "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"13\" height=\"%d\"><rect x=\"6\" y=\"0\" width=\"1\" height=\"%d\" fill=\"%s\"/></svg>"
                       h h color))
                 (img (create-image svg 'svg t :scale 1 :ascent 0)))
            (when img
              (omnidiff-image--register-descriptor img)
              (puthash key img omnidiff-image--img-cache)))))))

;;; SVG compositing (Alpha-correct Onion Skin & Neutral-gray Difference Blend)

(defun omnidiff-image--build-svg-onion (b-key b-bytes b-mime a-key a-bytes a-mime
                                              disp-w disp-h opacity backdrop dark-p
                                              &optional regions active-ridx src-w src-h)
  "Build an alpha-correct SVG onion-skin composite over an opaque BACKDROP.
Both BEFORE and AFTER are composited onto the same opaque backdrop before
cross-fading so erased transparent pixels vanish cleanly at 100% AFTER."
  (pcase-let* ((`(,defs-xml . ,fill-val)
                (omnidiff-image--backdrop-svg-defs
                 (if (eq backdrop 'none) 'none backdrop) dark-p))
               (b64-b (when (< opacity 0.999) (omnidiff-image--base64 b-key b-bytes)))
               (b64-a (when (> opacity 0.001) (omnidiff-image--base64 a-key a-bytes)))
               (boxes (or (omnidiff-image--region-boxes-svg
                           regions (or active-ridx 0)
                           (or src-w disp-w) (or src-h disp-h) disp-w disp-h)
                          ""))
               (defs (if (string-empty-p defs-xml) "" (concat "<defs>" defs-xml "</defs>")))
               (bg-rect (format "<rect width=\"%d\" height=\"%d\" fill=\"%s\"/>"
                                disp-w disp-h fill-val))
               (body
                (cond
                 ((>= opacity 0.999)
                  (format "%s<image href=\"data:%s;base64,%s\" width=\"%d\" height=\"%d\" preserveAspectRatio=\"none\"/>"
                          bg-rect a-mime b64-a disp-w disp-h))
                 ((<= opacity 0.001)
                  (format "%s<image href=\"data:%s;base64,%s\" width=\"%d\" height=\"%d\" preserveAspectRatio=\"none\"/>"
                          bg-rect b-mime b64-b disp-w disp-h))
                 (t
                  (format
                   (concat
                    "<g>%s<image href=\"data:%s;base64,%s\" width=\"%d\" height=\"%d\" preserveAspectRatio=\"none\"/></g>"
                    "<g opacity=\"%.3f\">%s<image href=\"data:%s;base64,%s\" width=\"%d\" height=\"%d\" preserveAspectRatio=\"none\"/></g>")
                   bg-rect b-mime b64-b disp-w disp-h
                   opacity bg-rect a-mime b64-a disp-w disp-h)))))
    (format
     "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\">%s%s%s</svg>"
     disp-w disp-h disp-w disp-h defs body boxes)))

(defun omnidiff-image--build-svg-diff (b-key b-bytes b-mime a-key a-bytes a-mime
                                             disp-w disp-h gain
                                             &optional regions active-ridx src-w src-h)
  "Build an alpha-aware SVG difference preview in display coordinates.
Both sides are pre-composited onto neutral mid-gray `#808080' before
`mix-blend-mode: difference' so transparent-to-black and transparent-to-white
changes produce bright signals instead of zeroing out."
  (let* ((b64-b (omnidiff-image--base64 b-key b-bytes))
         (b64-a (omnidiff-image--base64 a-key a-bytes))
         (g (max 1.0 (float (or gain 1.0))))
         (use-filter (> g 1.01))
         (defs (if use-filter
                   (format
                    (concat
                     "<defs><filter id=\"amp\" x=\"0\" y=\"0\" width=\"100%%\" height=\"100%%\" "
                     "color-interpolation-filters=\"sRGB\">"
                     "<feColorMatrix type=\"matrix\" "
                     "values=\"%.2f 0 0 0 0  0 %.2f 0 0 0  0 0 %.2f 0 0  0 0 0 1 0\"/>"
                     "</filter></defs>")
                    g g g)
                 ""))
         (group-open (if use-filter
                         "<g filter=\"url(#amp)\" style=\"isolation: isolate\">"
                       "<g style=\"isolation: isolate\">"))
         (boxes (or (omnidiff-image--region-boxes-svg
                     regions (or active-ridx 0)
                     (or src-w disp-w) (or src-h disp-h) disp-w disp-h)
                    "")))
    (format
     (concat
      "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" "
      "viewBox=\"0 0 %d %d\">%s"
      "<rect width=\"%d\" height=\"%d\" fill=\"#000000\"/>%s"
      "<g style=\"isolation: isolate\">"
      "<rect width=\"%d\" height=\"%d\" fill=\"#808080\"/>"
      "<image href=\"data:%s;base64,%s\" width=\"%d\" height=\"%d\" preserveAspectRatio=\"none\"/>"
      "</g>"
      "<g style=\"isolation: isolate; mix-blend-mode: difference\">"
      "<rect width=\"%d\" height=\"%d\" fill=\"#808080\"/>"
      "<image href=\"data:%s;base64,%s\" width=\"%d\" height=\"%d\" preserveAspectRatio=\"none\"/>"
      "</g></g>%s</svg>")
     disp-w disp-h disp-w disp-h defs
     disp-w disp-h group-open
     disp-w disp-h b-mime b64-b disp-w disp-h
     disp-w disp-h a-mime b64-a disp-w disp-h
     boxes)))

;;; Optional asynchronous ImageMagick `compare' backend

(defun omnidiff-image--magick-command ()
  "Return base argv list for ImageMagick `compare', or nil if unavailable."
  (cond
   ((executable-find "magick")
    (list (executable-find "magick") "compare"))
   ((executable-find "compare")
    (list (executable-find "compare")))
   (t nil)))

(defun omnidiff-image--magick-convert-command ()
  "Return base argv list for ImageMagick `magick'/`convert', or nil."
  (cond
   ((executable-find "magick")
    (list (executable-find "magick")))
   ((executable-find "convert")
    (list (executable-find "convert")))
   (t nil)))

(defun omnidiff-image--magick-eligible-p (pic b-type a-type bw bh aw ah)
  "Return non-nil when ImageMagick should be used for pixel diff modes."
  (and (memq omnidiff-image-diff-backend '(auto magick))
       (not (omnidiff-picture-svg-p pic))
       (not (eq b-type 'svg))
       (not (eq a-type 'svg))
       (> bw 0) (> bh 0) (= bw aw) (= bh ah)
       (or (omnidiff-image--magick-command)
           (omnidiff-image--magick-convert-command))))

(defun omnidiff-image--available-diff-styles (magick-p)
  "Return ordered list of diff styles supported for current capabilities."
  (let ((has-convert (and magick-p (omnidiff-image--magick-convert-command) t))
        (has-compare (and magick-p (omnidiff-image--magick-command) t)))
    (delq nil
          (list (when has-convert 'spotlight-sbs)
                (when has-convert 'spotlight)
                (when has-compare 'mask)
                'delta))))

(defun omnidiff-image--effective-diff-style (state magick-p)
  "Resolve active `4:diff' visual style for STATE given MAGICK-P."
  (let ((avail (omnidiff-image--available-diff-styles magick-p))
        (pref (or (and state (omnidiff-image--state-diff-style state))
                  omnidiff-image-diff-style)))
    (cond
     ((eq omnidiff-image-diff-backend 'svg) 'delta)
     ((memq pref avail) pref)
     (t (or (car avail) 'delta)))))

(defun omnidiff-image--type-extension (type)
  "Return filename extension for Emacs image symbol TYPE."
  (pcase type
    ('png ".png")
    ('jpeg ".jpg")
    ('gif ".gif")
    ('webp ".webp")
    ('tiff ".tiff")
    ('bmp ".bmp")
    ('ico ".ico")
    ('heic ".heic")
    ('svg ".svg")
    (_ ".img")))

(defun omnidiff-image--write-binary-temp (path bytes)
  "Write unibyte BYTES to PATH without fsync or handlers."
  (let ((file-name-handler-alist nil)
        (write-region-inhibit-fsync t)
        (coding-system-for-write 'no-conversion))
    (write-region bytes nil path nil 'silent)
    path))

(defun omnidiff-image--start-magick-diff (diff-key b-bytes b-type a-bytes a-type
                                                   highlight fuzz card-ov)
  "Launch async ImageMagick `compare' for DIFF-KEY and refresh CARD-OV when done."
  (when-let* ((base-cmd (omnidiff-image--magick-command)))
    (let* ((buf (current-buffer))
           (tmpdir (make-temp-file "omnidiff-magick-" t))
           (f-before (expand-file-name (concat "before" (omnidiff-image--type-extension b-type)) tmpdir))
           (f-after (expand-file-name (concat "after" (omnidiff-image--type-extension a-type)) tmpdir))
           (f-out (expand-file-name "diff.png" tmpdir))
           (lowlight (if (omnidiff-image--dark-background-p)
                         "rgba(20,22,30,0.65)"
                       "rgba(246,247,250,0.72)"))
           (fuzz-args (when (and (integerp fuzz) (> fuzz 0))
                        (list "-fuzz" (format "%d%%" fuzz))))
           (cmd (append base-cmd
                        fuzz-args
                        (list "-highlight-color" highlight
                              "-lowlight-color" lowlight
                              f-before f-after f-out)))
           (entry nil)
           (proc nil))
      (puthash diff-key 'pending omnidiff-image--magick-cache)
      (condition-case nil
          (progn
            (omnidiff-image--write-binary-temp f-before b-bytes)
            (omnidiff-image--write-binary-temp f-after a-bytes)
            (setq proc
                  (make-process
                   :name "omnidiff-magick"
                   :buffer nil
                   :command cmd
                   :connection-type 'pipe
                   :noquery t
                   :filter #'ignore
                   :sentinel
                   (lambda (p _ev)
                     (when (memq (process-status p) '(exit signal))
                       (unwind-protect
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (setq omnidiff-image--workers
                                     (delq entry omnidiff-image--workers))
                               (let ((result 'failed))
                                 (when (and (eq (process-status p) 'exit)
                                            (memq (process-exit-status p) '(0 1))
                                            (file-exists-p f-out))
                                   (let ((bytes (omnidiff-image--read-file-bytes f-out t)))
                                     (when (and (stringp bytes)
                                                (string-prefix-p omnidiff-image--png-magic bytes))
                                       (setq result bytes))))
                                 (when omnidiff-image--magick-cache
                                   (puthash diff-key result omnidiff-image--magick-cache))
                                 (when (and card-ov (overlay-buffer card-ov))
                                   (let* ((f (overlay-get card-ov 'omnidiff-image-file))
                                          (st (and f (omnidiff-image--state-for f))))
                                     (when (and st (eq (omnidiff-image--effective-mode st t) 'diff))
                                       ;; Background completion: never unfold a user-folded hunk.
                                       (omnidiff-image--rerender-card card-ov nil)))))))
                         (when (file-directory-p tmpdir)
                           (ignore-errors (delete-directory tmpdir t))))))))
            (setq entry (cons proc tmpdir))
            (push entry omnidiff-image--workers))
        (error
         (puthash diff-key 'failed omnidiff-image--magick-cache)
         (when (file-directory-p tmpdir)
           (ignore-errors (delete-directory tmpdir t))))))))

(defun omnidiff-image--spotlight-side-args (reg ctx-mul ctx-add tint tint-pct contour contour-alpha)
  "Build ImageMagick argv fragment producing one spotlight side from register REG.
Unchanged pixels are desaturated and attenuated by CTX-MUL/CTX-ADD; changed
pixels retain full readability with TINT at TINT-PCT plus a 1px CONTOUR
outer rim at CONTOUR-ALPHA."
  (list reg "-channel" "RGB" "-colorspace" "Gray" "-colorspace" "sRGB"
        "-evaluate" "multiply" ctx-mul "-evaluate" "add" ctx-add
        "+channel" "-type" "TrueColorAlpha"
        "(" reg "-channel" "RGB" "-fill" tint "-colorize" tint-pct "+channel" ")"
        "mpr:mask" "-compose" "Over" "-composite"
        "(" reg "-alpha" "opaque" "-fill" contour "-colorize" "100%"
        "-channel" "A" "-evaluate" "set" contour-alpha "+channel" ")"
        "mpr:edge" "-compose" "Over" "-composite"))

(defun omnidiff-image--start-magick-spotlight (diff-key b-bytes b-type a-bytes a-type
                                                        style dark-p fuzz card-ov)
  "Launch async ImageMagick spotlight diff for DIFF-KEY and refresh CARD-OV.
When STYLE is `spotlight-sbs', caches `(BEFORE-PNG . AFTER-PNG)'; when
`spotlight', caches `AFTER-PNG'."
  (when-let* ((base-cmd (omnidiff-image--magick-convert-command)))
    (let* ((buf (current-buffer))
           (sbs-p (eq style 'spotlight-sbs))
           (tmpdir (make-temp-file "omnidiff-spot-" t))
           (f-before (expand-file-name
                      (concat "before" (omnidiff-image--type-extension b-type)) tmpdir))
           (f-after (expand-file-name
                     (concat "after" (omnidiff-image--type-extension a-type)) tmpdir))
           (f-out-b (expand-file-name "spot-before.png" tmpdir))
           (f-out-a (expand-file-name "spot-after.png" tmpdir))
           (thr (if (and (integerp fuzz) (> fuzz 0)) (format "%d%%" fuzz) "0.2%"))
           (ctx-mul (if dark-p "0.68" "0.62"))
           (ctx-add (if dark-p "3%" "34%"))
           (tint (if dark-p "#f0a848" "#d97706"))
           (tint-pct (if sbs-p "18%" (if dark-p "24%" "22%")))
           (contour (if dark-p "#f5be6b" "#b45309"))
           (contour-alpha (if dark-p "35%" "40%"))
           (cmd (append
                 base-cmd
                 (list f-before "-alpha" "set" "-colorspace" "sRGB" "-write" "mpr:b" "+delete"
                       f-after "-alpha" "set" "-colorspace" "sRGB" "-write" "mpr:a" "+delete"
                       "mpr:b" "mpr:a" "-compose" "Difference" "-composite"
                       "-channel" "RGBA" "-separate" "+channel"
                       "-evaluate-sequence" "max" "-threshold" thr
                       "-write" "mpr:mask" "+delete"
                       "mpr:mask" "(" "+clone" "-morphology" "Dilate" "Octagon:1" ")"
                       "+swap" "-compose" "MinusSrc" "-composite"
                       "-write" "mpr:edge" "+delete")
                 (when sbs-p
                   (append
                    (omnidiff-image--spotlight-side-args
                     "mpr:b" ctx-mul ctx-add tint tint-pct contour contour-alpha)
                    (list "-write" (concat "PNG32:" f-out-b) "+delete")))
                 (omnidiff-image--spotlight-side-args
                  "mpr:a" ctx-mul ctx-add tint tint-pct contour contour-alpha)
                 (list (concat "PNG32:" f-out-a))))
           (entry nil)
           (proc nil))
      (puthash diff-key 'pending omnidiff-image--magick-cache)
      (condition-case nil
          (progn
            (omnidiff-image--write-binary-temp f-before b-bytes)
            (omnidiff-image--write-binary-temp f-after a-bytes)
            (setq proc
                  (make-process
                   :name "omnidiff-magick-spot"
                   :buffer nil
                   :command cmd
                   :connection-type 'pipe
                   :noquery t
                   :filter #'ignore
                   :sentinel
                   (lambda (p _ev)
                     (when (memq (process-status p) '(exit signal))
                       (unwind-protect
                           (when (buffer-live-p buf)
                             (with-current-buffer buf
                               (setq omnidiff-image--workers
                                     (delq entry omnidiff-image--workers))
                               (let ((result 'failed))
                                 (when (and (eq (process-status p) 'exit)
                                            (zerop (process-exit-status p))
                                            (file-exists-p f-out-a)
                                            (or (not sbs-p) (file-exists-p f-out-b)))
                                   (let ((ab (omnidiff-image--read-file-bytes f-out-a t))
                                         (bb (when sbs-p
                                               (omnidiff-image--read-file-bytes f-out-b t))))
                                     (when (and (stringp ab)
                                                (string-prefix-p omnidiff-image--png-magic ab)
                                                (or (not sbs-p)
                                                    (and (stringp bb)
                                                         (string-prefix-p
                                                          omnidiff-image--png-magic bb))))
                                       (setq result (if sbs-p (cons bb ab) ab)))))
                                 (when omnidiff-image--magick-cache
                                   (puthash diff-key result omnidiff-image--magick-cache))
                                 (when (and card-ov (overlay-buffer card-ov))
                                   (let* ((f (overlay-get card-ov 'omnidiff-image-file))
                                          (st (and f (omnidiff-image--state-for f))))
                                     (when (and st (eq (omnidiff-image--effective-mode st t) 'diff))
                                       (omnidiff-image--rerender-card card-ov nil)))))))
                         (when (file-directory-p tmpdir)
                           (ignore-errors (delete-directory tmpdir t))))))))
            (setq entry (cons proc tmpdir))
            (push entry omnidiff-image--workers))
        (error
         (puthash diff-key 'failed omnidiff-image--magick-cache)
         (when (file-directory-p tmpdir)
           (ignore-errors (delete-directory tmpdir t))))))))

;;; Mode resolution, compact toolbar & changed-region 1:1 crop loupe

(defun omnidiff-image--effective-mode (state compatible-overlay-p)
  "Resolve active visual diff mode given STATE and COMPATIBLE-OVERLAY-P."
  (let* ((layout (or (and (boundp 'omnidiff--layout) omnidiff--layout)
                     (and (boundp 'omnidiff-default-layout) omnidiff-default-layout)
                     'unified))
         (desired (or (omnidiff-image--state-mode state)
                      (pcase layout
                        ('side-by-side 'side-by-side)
                        ('stacked 'stacked)
                        (_ omnidiff-image-unified-mode)))))
    (if (and (memq desired '(swipe diff onion))
             (not compatible-overlay-p))
        'side-by-side
      desired)))

(defun omnidiff-image--available-modes (compatible-overlay-p svg-embed-p magick-p)
  "Return ordered list of usable visual modes for a dual-sided image pair."
  (delq nil
        (list 'side-by-side
              'stacked
              (when compatible-overlay-p 'swipe)
              (when (and compatible-overlay-p (or svg-embed-p magick-p)) 'diff)
              (when (and compatible-overlay-p svg-embed-p) 'onion)
              'blink)))

(defun omnidiff-image--default-swipe (pic img-w)
  "Compute smart initial swipe split ratio for PIC of pixel width IMG-W.
Aims at the horizontal center of the primary non-full-width changed region
when available, otherwise returns 0.5."
  (let ((regions (and pic (omnidiff-picture-regions pic)))
        (best nil))
    (when (and regions (> img-w 0))
      (dolist (r regions)
        (let ((rw (omnidiff-picture-region-width r)))
          (when (and (> rw 0) (< rw (* 0.85 img-w)) (null best))
            (setq best r)))))
    (if best
        (let* ((cx (+ (omnidiff-picture-region-x best)
                      (/ (float (omnidiff-picture-region-width best)) 2.0)))
               (frac (/ cx (float img-w))))
          (max 0.15 (min 0.85 (/ (round (* frac 20.0)) 20.0))))
      0.5)))

(defun omnidiff-image--effective-swipe (state pic img-w)
  "Return active swipe split fraction for STATE, auto-aiming when unset."
  (max 0.0 (min 1.0 (or (omnidiff-image--state-swipe state)
                        (omnidiff-image--default-swipe pic img-w)))))

(defun omnidiff-image--mode-label (mode active-p state &optional pic img-w)
  "Return concise toolbar chip text for MODE.
Only the ACTIVE-P chip carries live parameter readouts so inactive chips
stay compact and readable inside 80-column splits."
  (pcase mode
    ('side-by-side "1:2-up")
    ('stacked "2:stack")
    ('swipe (if active-p
                (format "3:swipe %d%%"
                        (round (* 100.0 (omnidiff-image--effective-swipe state pic (or img-w 0)))))
              "3:swipe"))
    ('diff "4:diff")
    ('onion (if active-p
                (format "5:onion %d%%" (round (* 100.0 (omnidiff-image--state-opacity state))))
              "5:onion"))
    ('blink (if active-p
                (format "6:blink (%s)"
                        (if (eq (omnidiff-image--state-blink-side state) 'before)
                            "old" "new"))
              "6:blink"))
    (_ (symbol-name mode))))

(defun omnidiff-image--make-mode-chip (mode active-p state &optional pic img-w)
  "Return a clickable propertized toolbar pill for MODE."
  (let ((label (concat " " (omnidiff-image--mode-label mode active-p state pic img-w) " "))
        (map (make-sparse-keymap)))
    (set-keymap-parent map omnidiff-image-card-map)
    (define-key map [mouse-1]
      (lambda (ev)
        (interactive "e")
        (mouse-set-point ev)
        (omnidiff-image-set-mode mode)))
    (propertize label
                'face (if active-p
                          'omnidiff-image-mode-active
                        'omnidiff-image-mode-inactive)
                'mouse-face 'highlight
                'help-echo (format "Switch image diff view to %s" mode)
                'keymap map)))

(defun omnidiff-image--insert-toolbar (active-mode available state
                                                   &optional status-fragments pic img-w)
  "Insert compact single-line image toolbar for ACTIVE-MODE with STATUS-FRAGMENTS."
  (let ((narrow-p (< (omnidiff-image--avail-cols) 66)))
    (insert (propertize "  " 'face 'omnidiff-image-bar))
    (if narrow-p
        (insert (omnidiff-image--make-mode-chip active-mode t state pic img-w)
                (propertize " (i)" 'face 'omnidiff-image-bar))
      (let ((first t))
        (dolist (m available)
          (unless first
            (insert (propertize " " 'face 'omnidiff-image-bar)))
          (setq first nil)
          (insert (omnidiff-image--make-mode-chip m (eq m active-mode) state pic img-w)))))
    (let ((frags (delq nil status-fragments)))
      (when frags
        (insert (propertize "  ·  " 'face 'omnidiff-image-bar))
        (let ((first t))
          (dolist (f frags)
            (unless first
              (insert (propertize " · " 'face 'omnidiff-image-bar)))
            (setq first nil)
            (insert f)))))
    (insert (propertize "\n" 'face 'omnidiff-image-bar))))

(defun omnidiff-image--toolbar-extras (state backdrop &optional pic)
  "Return compact trailing status fragments for zoom, backdrop, and regions."
  (let ((z (omnidiff-image--state-zoom state))
        (bd-pref (omnidiff-image--state-backdrop state))
        (rcount (if pic (omnidiff-picture-region-count pic) 0)))
    (delq nil
          (list
           (when z
             (propertize (format "%d%%" (round (* 100.0 z))) 'face 'omnidiff-image-bar))
           (when (and bd-pref (not (eq bd-pref 'auto)))
             (propertize (format "[%s]" (pcase backdrop
                                          ('checker "chk")
                                          ('light "lt")
                                          ('dark "dk")
                                          (_ "raw")))
                         'face 'omnidiff-image-bar))
           (when (> rcount 0)
             (propertize (format "r:%s" (omnidiff-image--format-int-grouped rcount))
                         'face (if (omnidiff-image--state-show-regions state)
                                   'omnidiff-image-mode-active
                                 'omnidiff-image-bar)))))))

(defun omnidiff-image--insert-spacer ()
  "Insert subtle vertical breathing room (~6px) above/below an image canvas."
  (insert (propertize "\n" 'line-height 0.35 'cursor-intangible t)))

(defun omnidiff-image--make-region-crop-svg (cache-key bytes mime src-w src-h
                                                       region backdrop dark-p
                                                       crop-x crop-y crop-w crop-h
                                                       tile-w tile-h)
  "Build cached SVG loupe tile cropping CROP-X..H into TILE-W × TILE-H."
  (when (omnidiff-image--svg-embeddable-p mime)
    (omnidiff-image--ensure-caches)
    (let* ((hi (omnidiff-image--diff-highlight-hex))
           (rx (omnidiff-picture-region-x region))
           (ry (omnidiff-picture-region-y region))
           (rw (omnidiff-picture-region-width region))
           (rh (omnidiff-picture-region-height region))
           (key (list 'rcrop cache-key mime crop-x crop-y crop-w crop-h
                      tile-w tile-h rx ry rw rh backdrop dark-p hi)))
      (or (gethash key omnidiff-image--img-cache)
          (pcase-let* ((`(,defs-xml . ,fill-val)
                        (omnidiff-image--backdrop-svg-defs backdrop dark-p))
                       (b64 (omnidiff-image--base64 cache-key bytes))
                       (defs (if (string-empty-p defs-xml) "" (concat "<defs>" defs-xml "</defs>")))
                       (bg-rect (format "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" fill=\"%s\"/>"
                                        crop-x crop-y crop-w crop-h fill-val))
                       (stroke-w (max 0.5 (/ (float crop-w) (float (max 1 tile-w)))))
                       (svg (format
                             (concat
                              "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"%d\" height=\"%d\" "
                              "viewBox=\"%d %d %d %d\">%s%s"
                              "<image href=\"data:%s;base64,%s\" width=\"%d\" height=\"%d\" "
                              "preserveAspectRatio=\"none\" style=\"image-rendering: pixelated\"/>"
                              "<rect x=\"%d\" y=\"%d\" width=\"%d\" height=\"%d\" "
                              "fill=\"none\" stroke=\"%s\" stroke-width=\"%.2f\" stroke-dasharray=\"%.2f %.2f\"/>"
                              "</svg>")
                             tile-w tile-h crop-x crop-y crop-w crop-h
                             defs bg-rect
                             mime b64 src-w src-h
                             rx ry rw rh hi stroke-w (* 3.0 stroke-w) (* 2.0 stroke-w)))
                       (img (create-image svg 'svg t :scale 1 :ascent 0)))
            (when img
              (omnidiff-image--register-descriptor img)
              (puthash key img omnidiff-image--img-cache)))))))

(defun omnidiff-image--insert-region-loupe (pic state b-key b-bytes b-mime
                                                a-key a-bytes a-mime
                                                src-w src-h backdrop avail-w)
  "Insert 1:1 / pixelated BEFORE | AFTER local crop tiles for active region."
  (let* ((regions (omnidiff-picture-regions pic))
         (listed (length regions))
         (total (omnidiff-picture-region-count pic))
         (dark-p (omnidiff-image--dark-background-p)))
    (when (and regions (> src-w 0) (> src-h 0)
               (omnidiff-image--svg-embeddable-p b-mime)
               (omnidiff-image--svg-embeddable-p a-mime))
      (let* ((idx (mod (max 0 (omnidiff-image--state-region-idx state)) listed))
             (r (nth idx regions))
             (rx (omnidiff-picture-region-x r))
             (ry (omnidiff-picture-region-y r))
             (rw (max 1 (omnidiff-picture-region-width r)))
             (rh (max 1 (omnidiff-picture-region-height r)))
             (pad (max 12 (min 36 (/ (max rw rh) 2))))
             (cw (min src-w (max 48 (+ rw (* 2 pad)))))
             (ch (min src-h (max 48 (+ rh (* 2 pad)))))
             (cx (max 0 (min (- src-w cw) (- (+ rx (/ rw 2)) (/ cw 2)))))
             (cy (max 0 (min (- src-h ch) (- (+ ry (/ rh 2)) (/ ch 2)))))
             (max-tile-w (min 220 (max 96 (/ (- avail-w 40) 2))))
             (max-tile-h 180)
             ;; Magnify tiny regions cleanly by integer factor (up to 4x), else fit/1:1.
             (zoom (cond
                    ((and (<= cw 48) (<= ch 48) (<= (* cw 4) max-tile-w) (<= (* ch 4) max-tile-h)) 4.0)
                    ((and (<= cw 96) (<= ch 96) (<= (* cw 2) max-tile-w) (<= (* ch 2) max-tile-h)) 2.0)
                    (t (min 1.0 (/ (float max-tile-w) cw) (/ (float max-tile-h) ch)))))
             (tile-w (max 1 (round (* cw zoom))))
             (tile-h (max 1 (round (* ch zoom))))
             (b-tile (omnidiff-image--make-region-crop-svg
                      b-key b-bytes b-mime src-w src-h r backdrop dark-p
                      cx cy cw ch tile-w tile-h))
             (a-tile (omnidiff-image--make-region-crop-svg
                      a-key a-bytes a-mime src-w src-h r backdrop dark-p
                      cx cy cw ch tile-w tile-h))
             (frame-face (omnidiff-image--frame-face)))
        (when (and b-tile a-tile)
          (insert (propertize
                   (format "  Region %d/%d%s · %d×%d at (%d, %d) · %d%% loupe%s\n"
                           (1+ idx) listed
                           (if (> total listed)
                               (format " of %s" (omnidiff-image--format-int-grouped total))
                             "")
                           rw rh rx ry (round (* 100.0 zoom))
                           (if (> listed 1) " · </> step" ""))
                   'face 'omnidiff-image-meta))
          (insert (propertize "  " 'cursor-intangible t)
                  (propertize " " 'display b-tile 'face frame-face
                              'rear-nonsticky '(display face) 'cursor-intangible t)
                  (propertize "  " 'cursor-intangible t)
                  (propertize " " 'display a-tile 'face frame-face
                              'rear-nonsticky '(display face) 'cursor-intangible t)
                  "\n"))))))

(defun omnidiff-image--insert-regions (pic indent &optional active-idx clickable-p)
  "Insert structured changed-regions list for `omnidiff-picture' PIC with INDENT."
  (let ((regions (omnidiff-picture-regions pic))
        (more (omnidiff-picture-more-regions pic))
        (total (omnidiff-picture-region-count pic)))
    (when (or regions (> total 0))
      (insert (propertize
               (format "%sChanged regions (showing %s / %s):\n"
                       indent
                       (omnidiff-image--format-int-grouped (length regions))
                       (omnidiff-image--format-int-grouped total))
               'face 'omnidiff-image-meta))
      (let ((idx 0))
        (dolist (r regions)
          (let* ((sel-p (and active-idx (= idx active-idx)))
                 (line (format "%s  %s%d×%d at (%d, %d)\n"
                               indent
                               (if sel-p "▸ " "  ")
                               (omnidiff-picture-region-width r)
                               (omnidiff-picture-region-height r)
                               (omnidiff-picture-region-x r)
                               (omnidiff-picture-region-y r)))
                 (target-idx idx))
            (if clickable-p
                (let ((map (make-sparse-keymap)))
                  (set-keymap-parent map omnidiff-image-card-map)
                  (define-key map [mouse-1]
                    (lambda (ev)
                      (interactive "e")
                      (mouse-set-point ev)
                      (omnidiff-image-select-region target-idx)))
                  (insert (propertize line
                                      'face (if sel-p 'omnidiff-image-mode-active
                                              'omnidiff-image-meta)
                                      'mouse-face 'highlight
                                      'keymap map)))
              (insert (propertize line 'face 'omnidiff-image-meta))))
          (setq idx (1+ idx))))
      (when (> more 0)
        (insert (propertize
                 (format "%s  … %s more regions (not returned by omnidiff)\n"
                         indent (omnidiff-image--format-int-grouped more))
                 'face 'omnidiff-image-meta))))))

;;; TTY summary (sub-character bar & toggleable regions)

(defun omnidiff-image--format-tty-bar (pct width)
  "Render a WIDTH-cell progress bar for PCT (0..100) with 1/8-block precision."
  (let* ((p (max 0.0 (min 100.0 (float (or pct 0.0)))))
         (eighths (round (* width 8.0 (/ p 100.0))))
         (eff (if (and (> p 0.0) (= eighths 0)) 1 eighths))
         (full (/ eff 8))
         (rem (% eff 8))
         (has-part (and (> rem 0) (< full width)))
         (empty (max 0 (- width full (if has-part 1 0)))))
    (concat (make-string full ?█)
            (if has-part (char-to-string (aref omnidiff-image--tty-blocks rem)) "")
            (make-string empty ?░))))

(defun omnidiff-image--insert-tty-summary (pic &optional reason state clickable-load-p)
  "Insert clean text-mode metadata card for `omnidiff-picture' PIC.
Optional REASON explains why inline visual display was skipped."
  (let ((b (omnidiff-picture-before pic))
        (a (omnidiff-picture-after pic))
        (comp (omnidiff-picture-comparison pic))
        (pct (omnidiff-picture-changed-pct pic))
        (chg (omnidiff-picture-changed-pixels pic))
        (tot (omnidiff-picture-total-pixels pic)))
    (when b
      (insert (propertize "  " 'face 'default)
              (propertize " BEFORE " 'face 'omnidiff-side-before)
              (propertize (concat "  "
                                  (string-join
                                   (delq nil
                                         (list (omnidiff-picture-info-format b)
                                               (when (and (> (omnidiff-picture-info-width b) 0)
                                                          (> (omnidiff-picture-info-height b) 0))
                                                 (format "%d×%d"
                                                         (omnidiff-picture-info-width b)
                                                         (omnidiff-picture-info-height b)))
                                               (omnidiff-picture-info-color b)
                                               (when (> (omnidiff-picture-info-bytes b) 0)
                                                 (concat "· " (omnidiff-image-format-bytes
                                                               (omnidiff-picture-info-bytes b))))))
                                   " ")
                                  "\n")
                          'face 'omnidiff-image-meta)))
    (when a
      (insert (propertize "  " 'face 'default)
              (propertize " AFTER  " 'face 'omnidiff-side-after)
              (propertize (concat "  "
                                  (string-join
                                   (delq nil
                                         (list (omnidiff-picture-info-format a)
                                               (when (and (> (omnidiff-picture-info-width a) 0)
                                                          (> (omnidiff-picture-info-height a) 0))
                                                 (format "%d×%d"
                                                         (omnidiff-picture-info-width a)
                                                         (omnidiff-picture-info-height a)))
                                               (omnidiff-picture-info-color a)
                                               (when (> (omnidiff-picture-info-bytes a) 0)
                                                 (concat "· " (omnidiff-image-format-bytes
                                                               (omnidiff-picture-info-bytes a))))))
                                   " ")
                                  "\n")
                          'face 'omnidiff-image-meta)))
    (pcase comp
      ('pixels
       (let ((bar (omnidiff-image--format-tty-bar pct 20)))
         (insert (propertize (format "  Delta   [%s] %.2f%% (%s / %s px)\n"
                                     bar (or pct 0.0)
                                     (omnidiff-image--format-int-grouped (or chg 0))
                                     (omnidiff-image--format-int-grouped (or tot 0)))
                             'face 'omnidiff-image-meta))))
      ('identical
       (insert (propertize "  Delta   No pixels changed (metadata or compression only)\n"
                           'face 'omnidiff-image-meta)))
      ('resized
       (insert (propertize "  Delta   Resized to a different aspect ratio (not compared pixel-by-pixel)\n"
                           'face 'omnidiff-image-meta)))
      ('resized-scaled
       (insert (propertize (format "  Delta   Resized (same aspect ratio); %.2f%% pixels changed\n"
                                   (or pct 0.0))
                           'face 'omnidiff-image-meta)))
      ('unknown
       (let* ((raw (omnidiff-picture-raw-comparison pic))
              (raw-ok (and (stringp raw)
                           (not (string-empty-p raw))
                           (not (string= raw "unknown")))))
         (insert (propertize
                  (cond
                   ((and raw-ok (string-prefix-p "animation:" raw))
                    (format "  Delta   %s\n" raw))
                   (raw-ok
                    (format "  Delta   Pixel comparison unavailable (%s)\n" raw))
                   (t
                    "  Delta   Pixel comparison unavailable\n"))
                  'face 'omnidiff-image-meta)))))
    (unless (and state (omnidiff-image--state-tty-hide-regions state))
      (omnidiff-image--insert-regions pic "  "))
    (when reason
      (if clickable-load-p
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map omnidiff-image-card-map)
            (define-key map [mouse-1]
              (lambda (ev)
                (interactive "e")
                (mouse-set-point ev)
                (omnidiff-image-force-load)))
            (insert (propertize (format "  %s\n" reason)
                                'face 'omnidiff-gap
                                'mouse-face 'highlight
                                'help-echo "Click or press L to load preview"
                                'keymap map)))
        (insert (propertize (format "  %s\n" reason) 'face 'omnidiff-gap))))))

;;; Visual mode renderers

(defun omnidiff-image--insert-side-by-side (b-key b-bytes b-type b-mime
                                                  a-key a-bytes a-type a-mime
                                                  bw bh b-kind aw ah a-kind
                                                  avail-w max-h state svg-p
                                                  backdrop regions active-ridx)
  "Insert two-up BEFORE | AFTER view with full-height divider and top alignment."
  (let* ((gui-px (and (omnidiff-image--display-graphic-p)
                      (fboundp 'string-pixel-width)))
         (indent-px (if gui-px (string-pixel-width "  ") 16))
         (div-px 16)
         (pane-w (/ (- avail-w div-px 16) 2))
         (zoom (omnidiff-image--state-zoom state))
         ;; Share scale factor for moderate size differences so resolution changes stay visible.
         (shared-scale
          (when (and (null zoom) (not svg-p)
                     (> bw 0) (> bh 0) (> aw 0) (> ah 0)
                     (<= (/ (float (max bw aw)) (float (min bw aw))) 4.0)
                     (<= (/ (float (max bh ah)) (float (min bh ah))) 4.0))
            (min 1.0
                 (/ (float (max 1 pane-w)) (float (max bw aw)))
                 (/ (float (max 1 max-h)) (float (max bh ah))))))
         (b-fit (if shared-scale
                    (cons (max 1 (round (* bw shared-scale)))
                          (max 1 (round (* bh shared-scale))))
                  (omnidiff-image--fit-size bw bh pane-w max-h svg-p zoom)))
         (a-fit (if shared-scale
                    (cons (max 1 (round (* aw shared-scale)))
                          (max 1 (round (* ah shared-scale))))
                  (omnidiff-image--fit-size aw ah pane-w max-h svg-p zoom)))
         (b-img (omnidiff-image--make-view-image
                 b-key b-bytes b-type b-mime bw bh (car b-fit) (cdr b-fit)
                 (or svg-p (and zoom t)) 0 backdrop regions active-ridx))
         (a-img (omnidiff-image--make-view-image
                 a-key a-bytes a-type a-mime aw ah (car a-fit) (cdr a-fit)
                 (or svg-p (and zoom t)) 0 backdrop regions active-ridx))
         (b-dim-lbl (omnidiff-image--format-dim-label bw bh b-kind (car b-fit)))
         (a-dim-lbl (omnidiff-image--format-dim-label aw ah a-kind (car a-fit)))
         (b-hdr-str (concat (propertize " BEFORE " 'face 'omnidiff-side-before)
                            (propertize (concat " " b-dim-lbl) 'face 'omnidiff-side-bar)))
         (b-hdr-px (if gui-px (string-pixel-width b-hdr-str) (* (length b-hdr-str) 8)))
         ;; Tighten gap for small/portrait images to avoid 600px saccades.
         (left-col-w (min pane-w (max (car b-fit) b-hdr-px 120)))
         (center-px (+ indent-px left-col-w 12))
         (div-h (max (cdr b-fit) (cdr a-fit)))
         (div-img (omnidiff-image--make-pane-divider div-h))
         (frame-face (omnidiff-image--frame-face)))
    (insert (propertize "  " 'face 'omnidiff-side-bar)
            b-hdr-str
            (propertize " " 'face 'omnidiff-side-bar
                        'display `(space :align-to (,center-px)))
            (propertize " │ " 'face 'omnidiff-side-bar)
            (propertize " AFTER  " 'face 'omnidiff-side-after)
            (propertize (concat " " a-dim-lbl) 'face 'omnidiff-side-bar)
            (propertize "\n" 'face 'omnidiff-side-bar))
    (omnidiff-image--insert-spacer)
    (insert (propertize "  " 'cursor-intangible t))
    (if b-img
        (insert (propertize " " 'display b-img 'face frame-face
                            'rear-nonsticky '(display face) 'cursor-intangible t))
      (insert (propertize "[unable to render before]" 'face 'omnidiff-gap)))
    (insert (propertize " " 'display `(space :align-to (,center-px)) 'cursor-intangible t)
            (if div-img
                (propertize " " 'display div-img 'rear-nonsticky '(display)
                            'cursor-intangible t)
              (propertize " │ " 'face 'omnidiff-lineno 'cursor-intangible t)))
    (if a-img
        (insert (propertize " " 'display a-img 'face frame-face
                            'rear-nonsticky '(display face) 'cursor-intangible t))
      (insert (propertize "[unable to render after]" 'face 'omnidiff-gap)))
    (insert "\n")
    (omnidiff-image--insert-spacer)))

(defun omnidiff-image--insert-stacked (b-key b-bytes b-type b-mime
                                             a-key a-bytes a-type a-mime
                                             bw bh b-kind aw ah a-kind
                                             avail-w max-h state svg-p
                                             backdrop regions active-ridx)
  "Insert stacked BEFORE-then-AFTER image view."
  (let* ((zoom (omnidiff-image--state-zoom state))
         (pane-h (max 220 (round (* 0.65 max-h))))
         (b-fit (omnidiff-image--fit-size bw bh avail-w pane-h svg-p zoom))
         (a-fit (omnidiff-image--fit-size aw ah avail-w pane-h svg-p zoom))
         (b-img (omnidiff-image--make-view-image
                 b-key b-bytes b-type b-mime bw bh (car b-fit) (cdr b-fit)
                 (or svg-p (and zoom t)) 0 backdrop regions active-ridx))
         (a-img (omnidiff-image--make-view-image
                 a-key a-bytes a-type a-mime aw ah (car a-fit) (cdr a-fit)
                 (or svg-p (and zoom t)) 0 backdrop regions active-ridx))
         (frame-face (omnidiff-image--frame-face)))
    (insert (propertize "  " 'face 'omnidiff-side-bar)
            (propertize " BEFORE " 'face 'omnidiff-side-before)
            (propertize (concat " " (omnidiff-image--format-dim-label bw bh b-kind (car b-fit)))
                        'face 'omnidiff-side-bar)
            (propertize "\n" 'face 'omnidiff-side-bar))
    (omnidiff-image--insert-spacer)
    (insert (propertize "  " 'cursor-intangible t)
            (if b-img
                (propertize " " 'display b-img 'face frame-face
                            'rear-nonsticky '(display face) 'cursor-intangible t)
              (propertize "[unable to render before]" 'face 'omnidiff-gap))
            "\n")
    (omnidiff-image--insert-spacer)
    (insert (propertize "  " 'face 'omnidiff-side-bar)
            (propertize " AFTER  " 'face 'omnidiff-side-after)
            (propertize (concat " " (omnidiff-image--format-dim-label aw ah a-kind (car a-fit)))
                        'face 'omnidiff-side-bar)
            (propertize "\n" 'face 'omnidiff-side-bar))
    (omnidiff-image--insert-spacer)
    (insert (propertize "  " 'cursor-intangible t)
            (if a-img
                (propertize " " 'display a-img 'face frame-face
                            'rear-nonsticky '(display face) 'cursor-intangible t)
              (propertize "[unable to render after]" 'face 'omnidiff-gap))
            "\n")
    (omnidiff-image--insert-spacer)))

(defun omnidiff-image--insert-blink (b-key b-bytes b-type b-mime
                                           a-key a-bytes a-type a-mime
                                           bw bh b-kind aw ah a-kind
                                           avail-w max-h state svg-p compat
                                           backdrop regions active-ridx)
  "Insert uniform-canvas in-place BEFORE/AFTER toggle image view."
  (let* ((before-p (eq (omnidiff-image--state-blink-side state) 'before))
         (key (if before-p b-key a-key))
         (bytes (if before-p b-bytes a-bytes))
         (type (if before-p b-type a-type))
         (mime (if before-p b-mime a-mime))
         (w (if before-p bw aw))
         (h (if before-p bh ah))
         (kind (if before-p b-kind a-kind))
         (zoom (omnidiff-image--state-zoom state))
         ;; Register both sides onto one shared canvas when aspect-compatible so flipping never jumps.
         (ref-w (if compat (max bw aw) w))
         (ref-h (if compat (max bh ah) h))
         (fit (omnidiff-image--fit-size ref-w ref-h avail-w max-h svg-p zoom))
         (img (omnidiff-image--make-view-image
               key bytes type mime w h (car fit) (cdr fit)
               (or compat svg-p (and zoom t)) 'center backdrop regions active-ridx))
         (frame-face (omnidiff-image--frame-face))
         (click-map (let ((m (make-sparse-keymap)))
                      (set-keymap-parent m omnidiff-image-card-map)
                      (define-key m [mouse-1]
                        (lambda (ev)
                          (interactive "e")
                          (let ((ov (omnidiff-image--card-ov-at (posn-point (event-start ev)))))
                            (when ov
                              (goto-char (overlay-start ov))
                              (omnidiff-image-toggle-side)))))
                      m)))
    (ignore kind)
    (omnidiff-image--insert-spacer)
    (insert (propertize "  " 'cursor-intangible t)
            (if img
                (propertize " " 'display img 'face frame-face
                            'keymap click-map 'mouse-face nil
                            'help-echo "Click or press b to flip BEFORE/AFTER"
                            'rear-nonsticky '(display face keymap)
                            'cursor-intangible t)
              (propertize "[unable to render image]" 'face 'omnidiff-gap))
            "\n")
    (omnidiff-image--insert-spacer)))

(defun omnidiff-image--insert-swipe (pic b-key b-bytes b-type b-mime
                                         a-key a-bytes a-type a-mime
                                         bw bh aw ah avail-w max-h state svg-p
                                         backdrop regions active-ridx)
  "Insert zero-Vernier-offset native `(slice ...)' split-curtain comparison."
  (let* ((src-w (max bw aw))
         (src-h (max bh ah))
         (split (omnidiff-image--effective-swipe state pic src-w))
         (zoom (omnidiff-image--state-zoom state))
         (fit (omnidiff-image--fit-size src-w src-h (- avail-w 6) max-h svg-p zoom))
         (disp-w (car fit))
         (disp-h (cdr fit))
         (b-img (omnidiff-image--make-view-image
                 b-key b-bytes b-type b-mime bw bh disp-w disp-h
                 t 'center backdrop regions active-ridx))
         (a-img (omnidiff-image--make-view-image
                 a-key a-bytes a-type a-mime aw ah disp-w disp-h
                 t 'center backdrop regions active-ridx))
         (handle-w (if (< disp-w 48) 1 3))
         (handle (omnidiff-image--make-swipe-handle disp-h handle-w))
         (frame-face (omnidiff-image--frame-face))
         (drag-map (let ((m (make-sparse-keymap)))
                     (set-keymap-parent m omnidiff-image-card-map)
                     (define-key m [down-mouse-1] #'omnidiff-image-swipe-mouse-drag)
                     m)))
    (omnidiff-image--insert-spacer)
    (insert (propertize "  " 'cursor-intangible t))
    (let ((img-start (point)))
      (cond
       ((not (and b-img a-img))
        (insert (propertize "[unable to render swipe view]" 'face 'omnidiff-gap)))
       ;; Full endpoints [0.0, 1.0]: skip zero-width slices and dead zones.
       ((<= split 0.001)
        (insert (propertize " " 'display a-img 'face frame-face
                            'keymap drag-map 'help-echo "Click or drag to move swipe curtain"
                            'rear-nonsticky '(display face keymap) 'cursor-intangible t)))
       ((>= split 0.999)
        (insert (propertize " " 'display b-img 'face frame-face
                            'keymap drag-map 'help-echo "Click or drag to move swipe curtain"
                            'rear-nonsticky '(display face keymap) 'cursor-intangible t)))
       (t
        ;; Subtract half handle width from each slice so horizontal lines have 0px offset.
        (let* ((half (/ (/ (float handle-w) 2.0) (float (max 1 disp-w))))
               (left-w (max 0.0 (- split half)))
               (right-x (min 1.0 (+ split half)))
               (right-w (max 0.0 (- 1.0 right-x))))
          (when (> left-w 0.0005)
            (insert (propertize " " 'display `((slice 0.0 0.0 ,left-w 1.0) ,b-img)
                                'face frame-face 'keymap drag-map
                                'help-echo "Click or drag to move swipe curtain"
                                'rear-nonsticky '(display face keymap)
                                'cursor-intangible t)))
          (insert (if handle
                      (propertize "┃" 'display handle 'face frame-face 'keymap drag-map
                                  'help-echo "Drag to move swipe curtain"
                                  'rear-nonsticky '(display face keymap)
                                  'cursor-intangible t)
                    (propertize "┃" 'face 'omnidiff-image-divider 'keymap drag-map
                                'rear-nonsticky '(face keymap) 'cursor-intangible t)))
          (when (> right-w 0.0005)
            (insert (propertize " " 'display `((slice ,right-x 0.0 ,right-w 1.0) ,a-img)
                                'face frame-face 'keymap drag-map
                                'help-echo "Click or drag to move swipe curtain"
                                'rear-nonsticky '(display face keymap)
                                'cursor-intangible t))))))
      (put-text-property img-start (point) 'omnidiff-swipe-geometry
                         (cons img-start disp-w)))
    (insert "\n")
    (omnidiff-image--insert-spacer)))

(defun omnidiff-image--insert-onion (file b-key b-bytes b-mime a-key a-bytes a-mime
                                          bw bh aw ah avail-w max-h state svg-p
                                          backdrop regions active-ridx)
  "Insert alpha-correct `librsvg' onion-skin overlay with bounded descriptor cache."
  (let* ((opacity (max 0.0 (min 1.0 (omnidiff-image--state-opacity state))))
         (src-w (max 1 (round (max bw aw))))
         (src-h (max 1 (round (max bh ah))))
         (zoom (omnidiff-image--state-zoom state))
         (fit (omnidiff-image--fit-size src-w src-h avail-w max-h svg-p zoom))
         (disp-w (car fit))
         (disp-h (cdr fit))
         (dark-p (omnidiff-image--dark-background-p))
         (fkey (omnidiff-file-target-path file))
         (svg (omnidiff-image--build-svg-onion
               b-key b-bytes b-mime a-key a-bytes a-mime
               disp-w disp-h opacity backdrop dark-p
               regions active-ridx src-w src-h))
         (prev (gethash fkey omnidiff-image--onion-descs))
         (img (create-image svg 'svg t :scale 1 :ascent 'center))
         (frame-face (omnidiff-image--frame-face)))
    (when (and prev (not (eq prev img)) (fboundp 'image-flush))
      (ignore-errors (image-flush prev)))
    (when img
      (puthash fkey img omnidiff-image--onion-descs))
    (omnidiff-image--insert-spacer)
    (insert (propertize "  " 'cursor-intangible t)
            (if img
                (propertize " " 'display img 'face frame-face
                            'rear-nonsticky '(display face) 'cursor-intangible t)
              (propertize "[unable to render onion-skin view]" 'face 'omnidiff-gap))
            "\n")
    (omnidiff-image--insert-spacer)))

(defun omnidiff-image--make-diff-style-chip (legend can-cycle-p)
  "Return toolbar fragment for diff LEGEND.
When CAN-CYCLE-P is non-nil, clicking it cycles diff styles."
  (if (not can-cycle-p)
      (propertize legend 'face 'omnidiff-image-bar)
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map omnidiff-image-card-map)
      (define-key map [mouse-1]
        (lambda (ev)
          (interactive "e")
          (mouse-set-point ev)
          (omnidiff-image-cycle-diff-style)))
      (propertize legend
                  'face 'omnidiff-image-bar
                  'mouse-face 'highlight
                  'help-echo "Click or press `s' to cycle diff style (spotlight 2-up / spotlight / mask / delta)"
                  'keymap map))))

(defun omnidiff-image--build-svg-delta-desc (b-key b-bytes b-mime a-key a-bytes a-mime
                                                   disp-w disp-h gain regions active-ridx
                                                   src-w src-h highlight)
  "Return cached built-in SVG RGB+alpha difference descriptor, or nil."
  (when (and (omnidiff-image--svg-embeddable-p b-mime)
             (omnidiff-image--svg-embeddable-p a-mime))
    (let* ((svg (omnidiff-image--build-svg-diff
                 b-key b-bytes b-mime a-key a-bytes a-mime
                 disp-w disp-h gain regions active-ridx src-w src-h))
           (svg-key (list 'svg-diff b-key a-key disp-w disp-h gain
                          (and regions (length regions)) active-ridx
                          (when regions highlight))))
      (or (gethash svg-key omnidiff-image--img-cache)
          (when-let* ((d (create-image svg 'svg t :scale 1 :ascent 'center)))
            (omnidiff-image--register-descriptor d)
            (puthash svg-key d omnidiff-image--img-cache))))))

(defun omnidiff-image--diff-render-plan (pic b-key b-bytes b-type b-mime
                                             a-key a-bytes a-type a-mime
                                             bw bh aw ah avail-w max-h
                                             state card-ov svg-p backdrop
                                             regions active-ridx)
  "Compute render plan plist for `4:diff' mode.
Returns a plist with `:kind' (`spotlight-sbs' or `single'), `:legend',
`:can-cycle', and either `:b-bytes'/`:a-bytes'/`:diff-key' or `:img'."
  (let* ((src-w (max 1 (round (max bw aw))))
         (src-h (max 1 (round (max bh ah))))
         (zoom (omnidiff-image--state-zoom state))
         (fit (omnidiff-image--fit-size src-w src-h avail-w max-h svg-p zoom))
         (disp-w (car fit))
         (disp-h (cdr fit))
         (dark-p (omnidiff-image--dark-background-p))
         (highlight (omnidiff-image--diff-highlight-hex))
         (fuzz (or (omnidiff-image--state-diff-fuzz state) omnidiff-image-diff-fuzz))
         (gain (or (omnidiff-image--state-diff-gain state) omnidiff-image-diff-gain))
         (magick-ok (omnidiff-image--magick-eligible-p pic b-type a-type bw bh aw ah))
         (avail-styles (omnidiff-image--available-diff-styles magick-ok))
         (can-cycle (> (length avail-styles) 1))
         (style (omnidiff-image--effective-diff-style state magick-ok))
         (fuzz-tag (if (> fuzz 0) (format " · fuzz %d%%" fuzz) ""))
         (scaled-tag (if (eq (omnidiff-picture-comparison pic) 'resized-scaled) " · scaled" ""))
         (style-hint (if can-cycle " (s)" "")))
    (pcase style
      ('spotlight-sbs
       (let* ((diff-key (list 'spotlight-sbs b-key a-key dark-p fuzz))
              (cached (gethash diff-key omnidiff-image--magick-cache)))
         (if (and (consp cached)
                  (stringp (car cached)) (> (length (car cached)) 0)
                  (stringp (cdr cached)) (> (length (cdr cached)) 0))
             (list :kind 'spotlight-sbs
                   :diff-key diff-key
                   :b-bytes (car cached)
                   :a-bytes (cdr cached)
                   :can-cycle can-cycle
                   :legend (format "spotlight 2-up%s%s" fuzz-tag style-hint))
           (when (and (null cached) card-ov)
             (omnidiff-image--start-magick-spotlight
              diff-key b-bytes b-type a-bytes a-type style dark-p fuzz card-ov)
             (setq cached (gethash diff-key omnidiff-image--magick-cache)))
           (let ((img (omnidiff-image--build-svg-delta-desc
                       b-key b-bytes b-mime a-key a-bytes a-mime
                       disp-w disp-h gain regions active-ridx src-w src-h highlight)))
             (list :kind 'single
                   :img img
                   :can-cycle can-cycle
                   :legend (if (eq cached 'pending)
                               "spotlight 2-up · computing…"
                             (format "delta ×%.1f (RGB+α%s)%s" gain scaled-tag style-hint)))))))
      ('spotlight
       (let* ((diff-key (list 'spotlight b-key a-key dark-p fuzz))
              (cached (gethash diff-key omnidiff-image--magick-cache)))
         (if (and (stringp cached) (> (length cached) 0))
             (list :kind 'single
                   :img (omnidiff-image--make-view-image
                         diff-key cached 'png "image/png" src-w src-h disp-w disp-h
                         (and zoom t) 'center backdrop regions active-ridx)
                   :can-cycle can-cycle
                   :legend (format "spotlight%s%s" fuzz-tag style-hint))
           (when (and (null cached) card-ov)
             (omnidiff-image--start-magick-spotlight
              diff-key b-bytes b-type a-bytes a-type style dark-p fuzz card-ov)
             (setq cached (gethash diff-key omnidiff-image--magick-cache)))
           (let ((img (omnidiff-image--build-svg-delta-desc
                       b-key b-bytes b-mime a-key a-bytes a-mime
                       disp-w disp-h gain regions active-ridx src-w src-h highlight)))
             (list :kind 'single
                   :img img
                   :can-cycle can-cycle
                   :legend (if (eq cached 'pending)
                               "spotlight · computing…"
                             (format "delta ×%.1f (RGB+α%s)%s" gain scaled-tag style-hint)))))))
      ('mask
       (let* ((diff-key (list 'magick b-key a-key dark-p highlight fuzz))
              (cached (gethash diff-key omnidiff-image--magick-cache)))
         (if (and (stringp cached) (> (length cached) 0))
             (list :kind 'single
                   :img (omnidiff-image--make-view-image
                         diff-key cached 'png "image/png" src-w src-h disp-w disp-h
                         (and zoom t) 'center 'none regions active-ridx)
                   :can-cycle can-cycle
                   :legend (format "mask (magick%s)%s" fuzz-tag style-hint))
           (when (and (null cached) card-ov)
             (omnidiff-image--start-magick-diff
              diff-key b-bytes b-type a-bytes a-type highlight fuzz card-ov)
             (setq cached (gethash diff-key omnidiff-image--magick-cache)))
           (let ((img (omnidiff-image--build-svg-delta-desc
                       b-key b-bytes b-mime a-key a-bytes a-mime
                       disp-w disp-h gain regions active-ridx src-w src-h highlight)))
             (list :kind 'single
                   :img img
                   :can-cycle can-cycle
                   :legend (if (eq cached 'pending)
                               (format "delta ×%.1f · loading pixel mask…" gain)
                             (format "delta ×%.1f (RGB+α%s)%s" gain scaled-tag style-hint)))))))
      (_
       (let ((img (omnidiff-image--build-svg-delta-desc
                   b-key b-bytes b-mime a-key a-bytes a-mime
                   disp-w disp-h gain regions active-ridx src-w src-h highlight)))
         (list :kind 'single
               :img img
               :can-cycle can-cycle
               :legend (if img
                           (format "delta ×%.1f (RGB+α%s)%s" gain scaled-tag style-hint)
                         "unavailable")))))))

(defun omnidiff-image--insert-single-side (side key bytes type mime info
                                                avail-w max-h state svg-p
                                                backdrop &optional warning)
  "Insert single-side preview for added, deleted, or partially-readable image."
  (pcase-let* ((`(,w ,h ,kind) (omnidiff-image--resolve-dims info bytes type))
               (zoom (omnidiff-image--state-zoom state))
               (`(,disp-w . ,disp-h) (omnidiff-image--fit-size w h avail-w max-h svg-p zoom))
               (img (omnidiff-image--make-view-image
                     key bytes type mime w h disp-w disp-h
                     (or svg-p (and zoom t)) 'center backdrop nil nil))
               (frame-face (omnidiff-image--frame-face)))
    (insert (propertize "  " 'face 'omnidiff-image-bar)
            (if (eq side 'before)
                (propertize " BEFORE " 'face 'omnidiff-side-before)
              (propertize " AFTER  " 'face 'omnidiff-side-after))
            (propertize (concat " " (omnidiff-image--format-dim-label w h kind disp-w))
                        'face 'omnidiff-image-bar)
            (if warning
                (propertize (concat "  ·  " warning) 'face 'omnidiff-image-stat-delta)
              "")
            (propertize "\n" 'face 'omnidiff-image-bar))
    (omnidiff-image--insert-spacer)
    (insert (propertize "  " 'cursor-intangible t)
            (if img
                (propertize " " 'display img 'face frame-face
                            'rear-nonsticky '(display face) 'cursor-intangible t)
              (propertize "[unable to render image]" 'face 'omnidiff-gap))
            "\n")
    (omnidiff-image--insert-spacer)))

(defun omnidiff-image--insert-identical-compact (pic a-key a-bytes a-type a-mime
                                                     aw ah a-kind avail-w max-h
                                                     state svg-p backdrop)
  "Insert sleek single-image view for pixel-identical files with `i' expand button."
  (let* ((b-info (omnidiff-picture-before pic))
         (a-info (omnidiff-picture-after pic))
         (zoom (omnidiff-image--state-zoom state))
         (fit (omnidiff-image--fit-size aw ah avail-w max-h svg-p zoom))
         (img (omnidiff-image--make-view-image
               a-key a-bytes a-type a-mime aw ah (car fit) (cdr fit)
               (or svg-p (and zoom t)) 'center backdrop nil nil))
         (size-note (when (and b-info a-info)
                      (format "%s → %s (%s)"
                              (omnidiff-image-format-bytes (omnidiff-picture-info-bytes b-info))
                              (omnidiff-image-format-bytes (omnidiff-picture-info-bytes a-info))
                              (omnidiff-image--format-size-delta
                               (omnidiff-picture-info-bytes b-info)
                               (omnidiff-picture-info-bytes a-info)))))
         (expand-map (let ((m (make-sparse-keymap)))
                       (set-keymap-parent m omnidiff-image-card-map)
                       (define-key m [mouse-1]
                         (lambda (ev)
                           (interactive "e")
                           (mouse-set-point ev)
                           (omnidiff-image-cycle-mode)))
                       m))
         (frame-face (omnidiff-image--frame-face)))
    (insert (propertize "  " 'face 'omnidiff-image-bar)
            (propertize " IDENTICAL " 'face 'omnidiff-image-mode-active)
            (propertize (concat "  no pixel changed"
                                (if size-note (concat " · " size-note) "")
                                " · " (omnidiff-image--format-dim-label aw ah a-kind (car fit))
                                "  ·  ")
                        'face 'omnidiff-image-bar)
            (propertize " i:compare " 'face 'omnidiff-image-mode-inactive
                        'mouse-face 'highlight 'keymap expand-map
                        'help-echo "Expand full comparison modes")
            (propertize "\n" 'face 'omnidiff-image-bar))
    (omnidiff-image--insert-spacer)
    (insert (propertize "  " 'cursor-intangible t)
            (if img
                (propertize " " 'display img 'face frame-face
                            'rear-nonsticky '(display face) 'cursor-intangible t)
              (propertize "[unable to render image]" 'face 'omnidiff-gap))
            "\n")
    (omnidiff-image--insert-spacer)))

(defun omnidiff-image--insert-card-content (file pic state card-ov &optional initial-render-p)
  "Insert interactive image diff content for FILE and PIC into current buffer."
  (let* ((repo-dir (or (and (boundp 'omnidiff--repo-dir) omnidiff--repo-dir)
                       default-directory))
         (svg-p (omnidiff-picture-svg-p pic))
         (gui-p (and omnidiff-image-enabled (omnidiff-image--display-graphic-p)))
         (force-p (omnidiff-image--state-force-load state))
         (over-limit (if initial-render-p
                         (and gui-p (not force-p)
                              (> omnidiff-image-auto-load-limit 0)
                              (>= omnidiff-image--render-auto-count
                                  omnidiff-image-auto-load-limit))
                       (and gui-p (not force-p)
                            (omnidiff-image--state-deferred state)))))
    (cond
     ((not gui-p)
      (omnidiff-image--insert-tty-summary pic nil state nil))
     (over-limit
      (setf (omnidiff-image--state-deferred state) t)
      (omnidiff-image--insert-tty-summary
       pic
       (format "Preview deferred (> %d auto-loaded images) — click or press L to load (C-u L all)"
               omnidiff-image-auto-load-limit)
       state t))
     (t
      (setf (omnidiff-image--state-deferred state) nil)
      (let* ((b-bytes (omnidiff-image--fetch-side-bytes file pic 'before repo-dir force-p))
             (a-bytes (omnidiff-image--fetch-side-bytes file pic 'after repo-dir force-p))
             (b-key (omnidiff-image--side-source-key file 'before repo-dir))
             (a-key (omnidiff-image--side-source-key file 'after repo-dir))
             (too-large (or (eq b-bytes 'too-large) (eq a-bytes 'too-large)))
             (b-ok (and (stringp b-bytes) (> (length b-bytes) 0)))
             (a-ok (and (stringp a-bytes) (> (length a-bytes) 0)))
             (b-tm (when b-ok (omnidiff-image--detect-type b-bytes (omnidiff-file-path file))))
             (a-tm (when a-ok (omnidiff-image--detect-type a-bytes (omnidiff-file-target-path file))))
             (b-type (car b-tm))
             (b-mime (cdr b-tm))
             (a-type (car a-tm))
             (a-mime (cdr a-tm))
             (b-avail (and b-type (image-type-available-p b-type)))
             (a-avail (and a-type (image-type-available-p a-type)))
             (has-alpha (omnidiff-image--has-alpha-p pic b-bytes b-type a-bytes a-type))
             (backdrop (omnidiff-image--effective-backdrop state has-alpha))
             (dual-expected (and (not (memq (omnidiff-file-status file) '(added deleted)))
                                 (or (omnidiff-picture-before pic)
                                     (omnidiff-file-old-oid file)
                                     (omnidiff-file-before-disk-path file))
                                 (or (omnidiff-picture-after pic)
                                     (omnidiff-file-new-oid file)
                                     (omnidiff-file-after-disk-path file)))))
        ;; Only count toward auto-load budget when at least one side actually renders.
        (when (and initial-render-p (not force-p) (not too-large) (or b-avail a-avail))
          (setq omnidiff-image--render-auto-count (1+ omnidiff-image--render-auto-count)))
        (pcase-let ((`(,avail-w . ,max-h) (omnidiff-image--viewport-px)))
          (cond
           (too-large
            (omnidiff-image--insert-tty-summary
             pic
             (format "Image exceeds %s auto-load limit — click or press L to load"
                     (omnidiff-image-format-bytes omnidiff-image-max-bytes))
             state t))
           ((and b-avail a-avail)
            (pcase-let* ((`(,bw ,bh ,b-kind) (omnidiff-image--resolve-dims
                                              (omnidiff-picture-before pic) b-bytes b-type))
                         (`(,aw ,ah ,a-kind) (omnidiff-image--resolve-dims
                                              (omnidiff-picture-after pic) a-bytes a-type))
                         (compat (omnidiff-image--compatible-overlay-p pic bw bh aw ah))
                         (svg-embed-p (and (omnidiff-image--svg-embeddable-p b-mime)
                                           (omnidiff-image--svg-embeddable-p a-mime)))
                         (magick-p (and (omnidiff-image--magick-eligible-p
                                         pic b-type a-type bw bh aw ah)
                                        t))
                         (avail-modes (omnidiff-image--available-modes compat svg-embed-p magick-p))
                         (compact-ident (and (eq (omnidiff-picture-comparison pic) 'identical)
                                             (not (omnidiff-image--state-expand-identical state))
                                             (null (omnidiff-image--state-mode state))))
                         (show-r (omnidiff-image--state-show-regions state))
                         (regions (when (and show-r compat
                                             (not (eq (omnidiff-picture-comparison pic) 'resized)))
                                    (omnidiff-picture-regions pic)))
                         (active-ridx (if regions
                                          (mod (max 0 (omnidiff-image--state-region-idx state))
                                               (length regions))
                                        0)))
              (if compact-ident
                  (omnidiff-image--insert-identical-compact
                   pic a-key a-bytes a-type a-mime aw ah a-kind avail-w max-h state svg-p backdrop)
                (let* ((req-mode (omnidiff-image--effective-mode state compat))
                       (pane-w (/ (- avail-w 32) 2))
                       (narrow-fallback (and (eq req-mode 'side-by-side) (< pane-w 160)))
                       (mode (if narrow-fallback 'stacked req-mode))
                       (extras (omnidiff-image--toolbar-extras state backdrop pic)))
                  (unless (memq mode avail-modes)
                    (setq mode (car avail-modes)))
                  (pcase mode
                    ('side-by-side
                     (omnidiff-image--insert-toolbar mode avail-modes state extras pic (max bw aw))
                     (omnidiff-image--insert-side-by-side
                      b-key b-bytes b-type b-mime a-key a-bytes a-type a-mime
                      bw bh b-kind aw ah a-kind avail-w max-h state svg-p
                      backdrop regions active-ridx))
                    ('stacked
                     (omnidiff-image--insert-toolbar
                      mode avail-modes state
                      (append (when narrow-fallback
                                (list (propertize "narrow window" 'face 'omnidiff-image-bar)))
                              extras)
                      pic (max bw aw))
                     (omnidiff-image--insert-stacked
                      b-key b-bytes b-type b-mime a-key a-bytes a-type a-mime
                      bw bh b-kind aw ah a-kind avail-w max-h state svg-p
                      backdrop regions active-ridx))
                    ('blink
                     (let* ((before-p (eq (omnidiff-image--state-blink-side state) 'before))
                            (w (if before-p bw aw))
                            (h (if before-p bh ah))
                            (k (if before-p b-kind a-kind))
                            (ref-w (if compat (max bw aw) w))
                            (ref-h (if compat (max bh ah) h))
                            (fit (omnidiff-image--fit-size
                                  ref-w ref-h avail-w max-h svg-p
                                  (omnidiff-image--state-zoom state)))
                            (side-pill (if before-p
                                           (propertize " BEFORE " 'face 'omnidiff-side-before)
                                         (propertize " AFTER  " 'face 'omnidiff-side-after)))
                            (dim-str (propertize
                                      (omnidiff-image--format-dim-label w h k (car fit))
                                      'face 'omnidiff-image-bar))
                            (back-hint (when (omnidiff-image--state-prev-mode state)
                                         (propertize "DEL back" 'face 'omnidiff-image-bar))))
                       (omnidiff-image--insert-toolbar
                        mode avail-modes state
                        (append (list side-pill dim-str back-hint) extras)
                        pic (max bw aw))
                       (omnidiff-image--insert-blink
                        b-key b-bytes b-type b-mime a-key a-bytes a-type a-mime
                        bw bh b-kind aw ah a-kind avail-w max-h state svg-p compat
                        backdrop regions active-ridx)))
                    ('swipe
                     (let* ((src-w (max bw aw))
                            (fit (omnidiff-image--fit-size
                                  src-w (max bh ah) (- avail-w 6) max-h svg-p
                                  (omnidiff-image--state-zoom state)))
                            (dim-str (omnidiff-image--format-dim-label
                                      src-w (max bh ah) a-kind (car fit))))
                       (omnidiff-image--insert-toolbar
                        mode avail-modes state
                        (append (list (concat (propertize " BEFORE " 'face 'omnidiff-side-before)
                                              (propertize " ◀┃▶ " 'face 'omnidiff-image-bar)
                                              (propertize " AFTER  " 'face 'omnidiff-side-after))
                                      (propertize dim-str 'face 'omnidiff-image-bar))
                                extras)
                        pic src-w)
                       (omnidiff-image--insert-swipe
                        pic b-key b-bytes b-type b-mime a-key a-bytes a-type a-mime
                        bw bh aw ah avail-w max-h state svg-p
                        backdrop regions active-ridx)))
                    ('onion
                     (omnidiff-image--insert-toolbar
                      mode avail-modes state
                      (append (list (propertize "BEFORE ◀ AFTER ▶" 'face 'omnidiff-image-bar))
                              extras)
                      pic (max bw aw))
                     (omnidiff-image--insert-onion
                      file b-key b-bytes b-mime a-key a-bytes a-mime
                      bw bh aw ah avail-w max-h state svg-p
                      backdrop regions active-ridx))
                    ('diff
                     (let* ((plan (omnidiff-image--diff-render-plan
                                   pic b-key b-bytes b-type b-mime
                                   a-key a-bytes a-type a-mime
                                   bw bh aw ah avail-w max-h
                                   state card-ov svg-p backdrop regions active-ridx))
                            (chip (omnidiff-image--make-diff-style-chip
                                   (plist-get plan :legend)
                                   (plist-get plan :can-cycle))))
                       (if (eq (plist-get plan :kind) 'spotlight-sbs)
                           (let* ((diff-key (plist-get plan :diff-key))
                                  (sb-bytes (plist-get plan :b-bytes))
                                  (sa-bytes (plist-get plan :a-bytes))
                                  (sbs-narrow (< pane-w 160)))
                             (omnidiff-image--insert-toolbar
                              mode avail-modes state
                              (append (list chip)
                                      (when sbs-narrow
                                        (list (propertize "narrow window" 'face 'omnidiff-image-bar)))
                                      extras)
                              pic (max bw aw))
                             (if sbs-narrow
                                 (omnidiff-image--insert-stacked
                                  (cons 'spot-b diff-key) sb-bytes 'png "image/png"
                                  (cons 'spot-a diff-key) sa-bytes 'png "image/png"
                                  bw bh b-kind aw ah a-kind avail-w max-h state nil
                                  backdrop regions active-ridx)
                               (omnidiff-image--insert-side-by-side
                                (cons 'spot-b diff-key) sb-bytes 'png "image/png"
                                (cons 'spot-a diff-key) sa-bytes 'png "image/png"
                                bw bh b-kind aw ah a-kind avail-w max-h state nil
                                backdrop regions active-ridx)))
                         (let ((img (plist-get plan :img)))
                           (omnidiff-image--insert-toolbar
                            mode avail-modes state
                            (append (list chip) extras)
                            pic (max bw aw))
                           (omnidiff-image--insert-spacer)
                           (insert (propertize "  " 'cursor-intangible t)
                                   (if img
                                       (propertize " " 'display img
                                                   'face (omnidiff-image--frame-face)
                                                   'rear-nonsticky '(display face)
                                                   'cursor-intangible t)
                                     (propertize "[pixel diff unavailable for this format]"
                                                 'face 'omnidiff-gap))
                                   "\n")
                           (omnidiff-image--insert-spacer))))))))
              (when show-r
                (when (and regions (= bw aw) (= bh ah))
                  (omnidiff-image--insert-region-loupe
                   pic state b-key b-bytes b-mime a-key a-bytes a-mime
                   bw bh backdrop avail-w))
                (omnidiff-image--insert-regions pic "  " active-ridx t))))
           (a-avail
            (omnidiff-image--insert-single-side
             'after a-key a-bytes a-type a-mime (omnidiff-picture-after pic)
             avail-w max-h state svg-p backdrop
             (when dual-expected "BEFORE unavailable — showing AFTER only")))
           (b-avail
            (omnidiff-image--insert-single-side
             'before b-key b-bytes b-type b-mime (omnidiff-picture-before pic)
             avail-w max-h state svg-p backdrop
             (when dual-expected "AFTER unavailable — showing BEFORE only")))
           (t
            (omnidiff-image--insert-tty-summary
             pic
             (cond
              ((or b-ok a-ok)
               "Emacs build does not support rendering this image format (press x for external viewer)")
              (t "Image blob unavailable"))
             state nil)))))))))

;;; In-place card re-rendering, resize reflow & theme sync

(defun omnidiff-image--card-ov-at (&optional pos)
  "Return the `omnidiff-image-ov' overlay enclosing POS (default point), or nil."
  (let ((p (or pos (point))))
    (or (get-text-property p 'omnidiff-image-ov)
        (cl-find-if (lambda (ov) (overlay-get ov 'omnidiff-image-ov))
                    (overlays-at p))
        (and (> p (point-min))
             (cl-find-if (lambda (ov) (overlay-get ov 'omnidiff-image-ov))
                         (overlays-at (1- p)))))))

(defun omnidiff-image--apply-card-properties (start end file card-ov)
  "Attach navigation, keymap, and optional hunk-root properties across START..END."
  (add-text-properties
   start end
   (list 'omnidiff-file (omnidiff-file-target-path file)
         'omnidiff-image-ov card-ov))
  (when (and (< start end) (overlay-get card-ov 'omnidiff-image-hunk-root-p))
    (save-excursion
      (goto-char start)
      (put-text-property start (min end (1+ (line-end-position)))
                         'omnidiff-hunk t)))
  ;; Preserve clickable chip / drag / crop keymaps while attaching card keymap elsewhere.
  (let ((pos start))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'keymap nil end)))
        (unless (get-text-property pos 'keymap)
          (put-text-property pos next 'keymap omnidiff-image-card-map))
        (setq pos next)))))

(defun omnidiff-image--rerender-card (card-ov &optional user-action-p)
  "Re-render the image card owned by CARD-OV in place without losing folds.
When USER-ACTION-P is non-nil, unfolds a collapsed enclosing hunk body."
  (when (and card-ov (overlay-buffer card-ov))
    (with-current-buffer (overlay-buffer card-ov)
      (let* ((file (overlay-get card-ov 'omnidiff-image-file))
             (pic (overlay-get card-ov 'omnidiff-image-picture))
             (state (omnidiff-image--state-for file))
             (old-start (overlay-start card-ov))
             (old-end (overlay-end card-ov))
             (orig-point (point))
             (rel-off (if (and (>= orig-point old-start) (<= orig-point old-end))
                          (- orig-point old-start)
                        nil))
             (win (get-buffer-window (current-buffer) t))
             (orig-wstart (when win (window-start win)))
             (hunk-ov (cl-find-if (lambda (ov)
                                    (and (overlay-get ov 'omnidiff-hunk-ov)
                                         (<= (overlay-start ov) old-start)
                                         (>= (overlay-end ov) old-end)))
                                  (overlays-in old-start (min (point-max) (1+ old-end)))))
             (file-ov (cl-find-if (lambda (ov)
                                    (and (overlay-get ov 'omnidiff-file-body)
                                         (<= (overlay-start ov) old-start)
                                         (>= (overlay-end ov) old-end)))
                                  (overlays-in old-start (min (point-max) (1+ old-end)))))
             (inhibit-read-only t)
             (old-len (- old-end old-start)))
        (run-hook-with-args 'before-change-functions old-start old-end)
        (let ((inhibit-modification-hooks t)
              (new-end nil))
          ;; Insert replacement at OLD-END before deleting OLD-START..OLD-END so
          ;; enclosing `evaporate' overlays (`hunk-ov', `file-ov') never vanish.
          (save-excursion
            (goto-char old-end)
            (omnidiff-image--insert-card-content file pic state card-ov nil)
            (setq new-end (point))
            (omnidiff-image--apply-card-properties old-end new-end file card-ov)
            (move-overlay card-ov old-end new-end)
            (when hunk-ov
              (move-overlay hunk-ov (min (overlay-start hunk-ov) old-start)
                            (max (overlay-end hunk-ov) new-end)))
            (when file-ov
              (move-overlay file-ov (min (overlay-start file-ov) old-start)
                            (max (overlay-end file-ov) new-end)))
            ;; Unfold a collapsed body ONLY on explicit user commands.
            (when (and user-action-p
                       (or (and hunk-ov (overlay-get hunk-ov 'invisible)
                                (not (and file-ov (overlay-get file-ov 'invisible))))
                           (and (null hunk-ov) file-ov (overlay-get file-ov 'invisible))))
              (let ((target-ov (or hunk-ov file-ov)))
                (overlay-put target-ov 'invisible nil)
                (when-let* ((fold-ov (overlay-get target-ov 'omnidiff-fold-ov)))
                  (overlay-put fold-ov 'display nil))))
            (delete-region old-start old-end)))
        (run-hook-with-args 'after-change-functions
                            (overlay-start card-ov) (overlay-end card-ov) old-len)
        (set-buffer-modified-p nil)
        (when rel-off
          (let ((target (min (max (overlay-start card-ov)
                                  (+ (overlay-start card-ov) rel-off))
                             (max (overlay-start card-ov) (1- (overlay-end card-ov))))))
            ;; Never leave point stuck on a cursor-intangible image span.
            (goto-char (if (get-text-property target 'cursor-intangible)
                           (overlay-start card-ov)
                         target))))
        (when (and win orig-wstart)
          (set-window-start win orig-wstart t))))))

(defun omnidiff-image--reflow-visible-cards (buf)
  "Re-render visible image cards in BUF after a window resize or theme change."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq omnidiff-image--resize-timer nil)
      (let* ((wins (get-buffer-window-list buf nil t))
             (cur-w (car (omnidiff-image--viewport-px))))
        (when (and wins (omnidiff-image--display-graphic-p))
          (setq omnidiff-image--last-viewport-w cur-w)
          (let ((cards nil))
            (dolist (w wins)
              (let ((beg (max (point-min) (- (window-start w) 200)))
                    (end (min (point-max) (+ (window-end w t) 200))))
                (dolist (ov (overlays-in beg end))
                  (when (and (overlay-get ov 'omnidiff-image-ov)
                             (not (memq ov cards)))
                    (push ov cards)))))
            (dolist (ov cards)
              (when (overlay-buffer ov)
                (omnidiff-image--rerender-card ov nil)))))))))

(defun omnidiff-image-on-window-resize (&optional _frame-or-win)
  "Debounced `window-size-change-functions' handler reflowing visible image cards."
  (when (and (derived-mode-p 'omnidiff-mode)
             omnidiff-image-enabled
             (omnidiff-image--display-graphic-p))
    (let ((cur-w (car (omnidiff-image--viewport-px))))
      (when (or (null omnidiff-image--last-viewport-w)
                (> (abs (- cur-w omnidiff-image--last-viewport-w)) 24))
        (omnidiff-image--cancel-resize-timer)
        (setq omnidiff-image--resize-timer
              (run-with-idle-timer 0.15 nil #'omnidiff-image--reflow-visible-cards
                                   (current-buffer)))))))

(defun omnidiff-image-on-theme-change ()
  "Invalidate theme-sensitive image caches and refresh visible image cards."
  (dolist (buf (buffer-list))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and (derived-mode-p 'omnidiff-mode) omnidiff-image--img-cache)
          (omnidiff-image--flush-descriptors)
          (omnidiff-image--reflow-visible-cards buf))))))

(add-hook 'omnidiff-theme-after-apply-hook #'omnidiff-image-on-theme-change)

;;; Interactive card commands

(defun omnidiff-image--require-card ()
  "Return active image card overlay at point or signal `user-error'."
  (or (omnidiff-image--card-ov-at)
      (user-error "Point is not on an image diff card")))

(defun omnidiff-image--card-capabilities (card-ov)
  "Return plist describing dual-sided availability, modes, and active diff backend."
  (let* ((file (overlay-get card-ov 'omnidiff-image-file))
         (pic (overlay-get card-ov 'omnidiff-image-picture))
         (state (omnidiff-image--state-for file))
         (repo-dir (or (and (boundp 'omnidiff--repo-dir) omnidiff--repo-dir)
                       default-directory))
         (force-p (omnidiff-image--state-force-load state))
         (b-bytes (omnidiff-image--fetch-side-bytes file pic 'before repo-dir force-p))
         (a-bytes (omnidiff-image--fetch-side-bytes file pic 'after repo-dir force-p))
         (b-ok (and (stringp b-bytes) (> (length b-bytes) 0)))
         (a-ok (and (stringp a-bytes) (> (length a-bytes) 0))))
    (if (and b-ok a-ok)
        (let* ((b-tm (omnidiff-image--detect-type b-bytes (omnidiff-file-path file)))
               (a-tm (omnidiff-image--detect-type a-bytes (omnidiff-file-target-path file)))
               (b-type (car b-tm)) (b-mime (cdr b-tm))
               (a-type (car a-tm)) (a-mime (cdr a-tm)))
          (pcase-let* ((`(,bw ,bh ,_) (omnidiff-image--resolve-dims
                                       (omnidiff-picture-before pic) b-bytes b-type))
                       (`(,aw ,ah ,_) (omnidiff-image--resolve-dims
                                       (omnidiff-picture-after pic) a-bytes a-type))
                       (compat (omnidiff-image--compatible-overlay-p pic bw bh aw ah))
                       (svg-embed-p (and (omnidiff-image--svg-embeddable-p b-mime)
                                         (omnidiff-image--svg-embeddable-p a-mime)))
                       (magick-p (and (omnidiff-image--magick-eligible-p
                                       pic b-type a-type bw bh aw ah)
                                      t)))
            (list :dual t :compat compat :magick magick-p
                  :bw bw :bh bh :aw aw :ah ah
                  :modes (omnidiff-image--available-modes compat svg-embed-p magick-p))))
      (list :dual nil :compat nil :magick nil :modes nil))))

(defun omnidiff-image-set-mode (mode)
  "Switch the image diff card at point to MODE."
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (state (omnidiff-image--state-for file))
         (caps (omnidiff-image--card-capabilities card-ov))
         (avail (plist-get caps :modes)))
    (unless (plist-get caps :dual)
      (user-error "Comparison modes require both BEFORE and AFTER images"))
    (unless (memq mode avail)
      (user-error "Image diff mode `%s' is unavailable for this image pair" mode))
    (setf (omnidiff-image--state-mode state) mode
          (omnidiff-image--state-mode-pinned state) t
          (omnidiff-image--state-prev-mode state) nil
          (omnidiff-image--state-expand-identical state) t)
    (omnidiff-image--rerender-card card-ov t)
    (message "Omnidiff image mode: %s" mode)))

(defun omnidiff-image-cycle-mode ()
  "Cycle visual comparison mode on the image diff card at point."
  (interactive)
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (pic (overlay-get card-ov 'omnidiff-image-picture))
         (state (omnidiff-image--state-for file))
         (caps (omnidiff-image--card-capabilities card-ov))
         (avail (plist-get caps :modes)))
    (unless (plist-get caps :dual)
      (user-error "Comparison modes require both BEFORE and AFTER images"))
    (if (and (eq (omnidiff-picture-comparison pic) 'identical)
             (not (omnidiff-image--state-expand-identical state))
             (null (omnidiff-image--state-mode state)))
        (progn
          (setf (omnidiff-image--state-expand-identical state) t)
          (omnidiff-image--rerender-card card-ov t)
          (message "Omnidiff image mode: %s"
                   (omnidiff-image--effective-mode state (plist-get caps :compat))))
      (let* ((cur (omnidiff-image--effective-mode state (plist-get caps :compat)))
             (tail (cdr (memq cur avail)))
             (next (or (car tail) (car avail))))
        (setf (omnidiff-image--state-mode state) next
              (omnidiff-image--state-mode-pinned state) t
              (omnidiff-image--state-prev-mode state) nil
              (omnidiff-image--state-expand-identical state) t)
        (omnidiff-image--rerender-card card-ov t)
        (message "Omnidiff image mode: %s" next)))))

(defun omnidiff-image-mode-side-by-side ()
  "Switch the current image card to side-by-side (2-up) mode."
  (interactive)
  (omnidiff-image-set-mode 'side-by-side))

(defun omnidiff-image-mode-stacked ()
  "Switch the current image card to stacked (vertical) mode."
  (interactive)
  (omnidiff-image-set-mode 'stacked))

(defun omnidiff-image-mode-swipe ()
  "Switch the current image card to swipe split-curtain mode."
  (interactive)
  (omnidiff-image-set-mode 'swipe))

(defun omnidiff-image-mode-diff ()
  "Switch the current image card to pixel-difference highlight mode."
  (interactive)
  (omnidiff-image-set-mode 'diff))

(defun omnidiff-image-mode-onion ()
  "Switch the current image card to onion-skin opacity overlay mode."
  (interactive)
  (omnidiff-image-set-mode 'onion))

(defun omnidiff-image-mode-blink ()
  "Switch the current image card to blink (A/B toggle) mode."
  (interactive)
  (omnidiff-image-set-mode 'blink))

(defun omnidiff-image-cycle-diff-style ()
  "Cycle `4:diff' visual style (spotlight 2-up / spotlight / mask / delta)."
  (interactive)
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (state (omnidiff-image--state-for file))
         (caps (omnidiff-image--card-capabilities card-ov))
         (avail-modes (plist-get caps :modes))
         (avail-styles (omnidiff-image--available-diff-styles (plist-get caps :magick))))
    (unless (plist-get caps :dual)
      (user-error "Pixel diff styles require both BEFORE and AFTER images"))
    (unless (memq 'diff avail-modes)
      (user-error "Pixel diff mode is unavailable for this image pair"))
    (let* ((in-diff (eq (omnidiff-image--effective-mode state (plist-get caps :compat)) 'diff))
           (cur (omnidiff-image--effective-diff-style state (plist-get caps :magick)))
           (pos (cl-position cur avail-styles))
           (next (if (and in-diff pos (> (length avail-styles) 1))
                     (nth (mod (1+ pos) (length avail-styles)) avail-styles)
                   cur)))
      (setf (omnidiff-image--state-mode state) 'diff
            (omnidiff-image--state-mode-pinned state) t
            (omnidiff-image--state-prev-mode state) nil
            (omnidiff-image--state-expand-identical state) t
            (omnidiff-image--state-diff-style state) next)
      (omnidiff-image--rerender-card card-ov t)
      (message "Omnidiff diff style: %s"
               (pcase next
                 ('spotlight-sbs "spotlight 2-up (side-by-side)")
                 ('spotlight "spotlight (single)")
                 ('mask "pixel mask (magick compare)")
                 ('delta "SVG RGB+alpha delta")
                 (_ (symbol-name next)))))))

(defun omnidiff-image-toggle-side ()
  "Flip between BEFORE and AFTER in place on the current image card.
When invoked from another mode, remembers it so `DEL' returns cleanly."
  (interactive)
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (state (omnidiff-image--state-for file))
         (caps (omnidiff-image--card-capabilities card-ov)))
    (unless (plist-get caps :dual)
      (user-error "Cannot flip sides on a single-sided image"))
    (let ((cur (omnidiff-image--effective-mode state (plist-get caps :compat))))
      (unless (eq cur 'blink)
        (setf (omnidiff-image--state-prev-mode state) cur))
      (setf (omnidiff-image--state-mode state) 'blink
            (omnidiff-image--state-expand-identical state) t
            (omnidiff-image--state-blink-side state)
            (if (eq (omnidiff-image--state-blink-side state) 'before)
                'after 'before)))
    (omnidiff-image--rerender-card card-ov t)
    (message "Omnidiff image: %s%s"
             (upcase (symbol-name (omnidiff-image--state-blink-side state)))
             (if (omnidiff-image--state-prev-mode state)
                 (format " (DEL to return to %s)" (omnidiff-image--state-prev-mode state))
               ""))))

(defun omnidiff-image-return-from-peek ()
  "Return from temporary `b' blink peek, or scroll down like `special-mode'."
  (interactive)
  (let* ((card-ov (omnidiff-image--card-ov-at))
         (file (and card-ov (overlay-get card-ov 'omnidiff-image-file)))
         (state (and file (omnidiff-image--state-for file)))
         (prev (and state (omnidiff-image--state-prev-mode state))))
    (if prev
        (progn
          (setf (omnidiff-image--state-mode state) prev
                (omnidiff-image--state-prev-mode state) nil)
          (omnidiff-image--rerender-card card-ov t)
          (message "Omnidiff image mode: %s" prev))
      (call-interactively #'scroll-down-command))))

(defun omnidiff-image--step-in-list (current steps delta-sign)
  "Move from CURRENT one step in DELTA-SIGN (-1 or +1) within ascending STEPS."
  (if (> delta-sign 0)
      (or (cl-find-if (lambda (v) (> v current)) steps)
          (car (last steps)))
    (or (cl-find-if (lambda (v) (< v current)) (reverse steps))
        (car steps))))

(defun omnidiff-image--adjust (delta-sign &optional fine-p)
  "Adjust active mode parameter (swipe, onion, or diff gain/fuzz) by DELTA-SIGN.
When FINE-P is non-nil, uses finer step increments."
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (pic (overlay-get card-ov 'omnidiff-image-picture))
         (state (omnidiff-image--state-for file))
         (caps (omnidiff-image--card-capabilities card-ov))
         (avail (plist-get caps :modes))
         (cur (omnidiff-image--effective-mode state (plist-get caps :compat))))
    (unless (plist-get caps :dual)
      (user-error "Adjustment requires both BEFORE and AFTER images"))
    (pcase cur
      ('onion
       (let* ((step (if fine-p (/ omnidiff-image-onion-step 2.0) omnidiff-image-onion-step))
              (raw (+ (omnidiff-image--state-opacity state) (* delta-sign step)))
              (val (max 0.0 (min 1.0 (/ (round (* raw 100.0)) 100.0)))))
         (setf (omnidiff-image--state-opacity state) val)
         (omnidiff-image--rerender-card card-ov t)
         (message "Omnidiff onion opacity: %d%%" (round (* 100.0 val)))))
      ('swipe
       (let* ((src-w (max (or (plist-get caps :bw) 0) (or (plist-get caps :aw) 0)))
              (step (if fine-p 0.01 omnidiff-image-swipe-step))
              (cur-s (omnidiff-image--effective-swipe state pic src-w))
              (raw (+ cur-s (* delta-sign step)))
              (val (max 0.0 (min 1.0 (/ (round (* raw 100.0)) 100.0)))))
         (setf (omnidiff-image--state-swipe state) val)
         (omnidiff-image--rerender-card card-ov t)
         (message "Omnidiff swipe split: %d%%" (round (* 100.0 val)))))
      ('diff
       (if (not (eq (omnidiff-image--effective-diff-style state (plist-get caps :magick)) 'delta))
           (let* ((cur-f (or (omnidiff-image--state-diff-fuzz state) omnidiff-image-diff-fuzz))
                  (next-f (omnidiff-image--step-in-list cur-f omnidiff-image--fuzz-steps delta-sign)))
             (setf (omnidiff-image--state-diff-fuzz state) next-f)
             (omnidiff-image--rerender-card card-ov t)
             (message "Omnidiff diff fuzz: %d%%" next-f))
         (let* ((cur-g (or (omnidiff-image--state-diff-gain state) omnidiff-image-diff-gain))
                (next-g (omnidiff-image--step-in-list cur-g omnidiff-image--gain-steps delta-sign)))
           (setf (omnidiff-image--state-diff-gain state) next-g)
           (omnidiff-image--rerender-card card-ov t)
           (message "Omnidiff SVG delta gain: ×%.1f" next-g))))
      (_
       (ignore avail)
       (if (memq (event-basic-type last-command-event) '(left right))
           (if (< delta-sign 0) (backward-char 1) (forward-char 1))
         (message "Mode `%s' has no adjustable parameter (3:swipe, 5:onion, 4:diff)"
                  cur))))))

(defun omnidiff-image-adjust-decrease (&optional arg)
  "Decrease swipe split, onion opacity, or diff gain/fuzz.
With prefix ARG, use fine increments."
  (interactive "P")
  (omnidiff-image--adjust -1 arg))

(defun omnidiff-image-adjust-increase (&optional arg)
  "Increase swipe split, onion opacity, or diff gain/fuzz.
With prefix ARG, use fine increments."
  (interactive "P")
  (omnidiff-image--adjust 1 arg))

(defun omnidiff-image-adjust-fine-decrease ()
  "Finely decrease swipe split or onion opacity."
  (interactive)
  (omnidiff-image--adjust -1 t))

(defun omnidiff-image-adjust-fine-increase ()
  "Finely increase swipe split or onion opacity."
  (interactive)
  (omnidiff-image--adjust 1 t))

(defun omnidiff-image-adjust-reset ()
  "Reset active parameter for swipe (50%), onion (50%), or diff (default)."
  (interactive)
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (state (omnidiff-image--state-for file))
         (caps (omnidiff-image--card-capabilities card-ov))
         (cur (omnidiff-image--effective-mode state (plist-get caps :compat))))
    (unless (plist-get caps :dual)
      (user-error "No adjustable parameter on a single-sided image"))
    (pcase cur
      ('onion
       (setf (omnidiff-image--state-opacity state) 0.5)
       (omnidiff-image--rerender-card card-ov t)
       (message "Omnidiff onion opacity: 50%%"))
      ('swipe
       (setf (omnidiff-image--state-swipe state) 0.5)
       (omnidiff-image--rerender-card card-ov t)
       (message "Omnidiff swipe split: 50%%"))
      ('diff
       (setf (omnidiff-image--state-diff-gain state) nil
             (omnidiff-image--state-diff-fuzz state) nil)
       (omnidiff-image--rerender-card card-ov t)
       (message "Omnidiff diff parameters reset to default"))
      (_
       (user-error "No adjustable parameter in `%s' mode" cur)))))

(defun omnidiff-image-swipe-mouse-drag (event)
  "Click or drag on a swipe image to position the BEFORE/AFTER curtain."
  (interactive "e")
  (let* ((start (event-start event))
         (win (posn-window start))
         (pt (posn-point start))
         (buf (and win (window-buffer win))))
    (when (and buf (integerp pt))
      (with-current-buffer buf
        (when-let* ((card-ov (omnidiff-image--card-ov-at pt))
                    (geom (get-text-property pt 'omnidiff-swipe-geometry))
                    (img-start (car geom))
                    (disp-w (cdr geom)))
          (when (> disp-w 0)
            (let* ((file (overlay-get card-ov 'omnidiff-image-file))
                   (state (omnidiff-image--state-for file))
                   (origin-posn (posn-at-point img-start win))
                   (left-x (and origin-posn (car (posn-x-y origin-posn)))))
              (when (numberp left-x)
                (cl-flet ((apply-x (mx)
                            (let* ((frac (max 0.0 (min 1.0 (/ (float (- mx left-x))
                                                              (float disp-w)))))
                                   (quant (/ (round (* frac 100.0)) 100.0)))
                              (unless (eql quant (omnidiff-image--state-swipe state))
                                (setf (omnidiff-image--state-swipe state) quant)
                                (omnidiff-image--rerender-card card-ov t)))))
                  (goto-char (overlay-start card-ov))
                  (when-let* ((mx (car (posn-x-y start))))
                    (apply-x mx))
                  (track-mouse
                    (let ((ev (read-event)))
                      (while (mouse-movement-p ev)
                        (when-let* ((mx (car (posn-x-y (event-start ev)))))
                          (apply-x mx))
                        (setq ev (read-event))))))))))))))

;;; Zoom & backdrop controls

(defun omnidiff-image--current-effective-zoom (card-ov)
  "Return current display/source zoom ratio for CARD-OV."
  (let* ((file (overlay-get card-ov 'omnidiff-image-file))
         (pic (overlay-get card-ov 'omnidiff-image-picture))
         (state (omnidiff-image--state-for file)))
    (or (omnidiff-image--state-zoom state)
        (let* ((caps (omnidiff-image--card-capabilities card-ov))
               (b-info (omnidiff-picture-before pic))
               (a-info (omnidiff-picture-after pic))
               (w (max (or (plist-get caps :bw) 0)
                       (or (plist-get caps :aw) 0)
                       (if a-info (omnidiff-picture-info-width a-info) 0)
                       (if b-info (omnidiff-picture-info-width b-info) 0)
                       1))
               (h (max (or (plist-get caps :bh) 0)
                       (or (plist-get caps :ah) 0)
                       (if a-info (omnidiff-picture-info-height a-info) 0)
                       (if b-info (omnidiff-picture-info-height b-info) 0)
                       1))
               (vp (omnidiff-image--viewport-px))
               (fit (omnidiff-image--fit-size
                     w h (car vp) (cdr vp) (omnidiff-picture-svg-p pic) nil)))
          (/ (float (car fit)) (float w))))))

(defun omnidiff-image--step-zoom (delta-sign)
  "Step magnification by DELTA-SIGN (-1 out, +1 in) on the current image card."
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (state (omnidiff-image--state-for file))
         (cur (omnidiff-image--current-effective-zoom card-ov))
         (next (if (> delta-sign 0)
                   (or (cl-find-if (lambda (z) (> z (+ cur 0.04)))
                                   omnidiff-image--zoom-steps)
                       (car (last omnidiff-image--zoom-steps)))
                 (or (cl-find-if (lambda (z) (< z (- cur 0.04)))
                                 (reverse omnidiff-image--zoom-steps))
                     (car omnidiff-image--zoom-steps)))))
    (setf (omnidiff-image--state-zoom state) next)
    (omnidiff-image--rerender-card card-ov t)
    (message "Omnidiff image zoom: %d%% (0 to fit)" (round (* 100.0 next)))))

(defun omnidiff-image-zoom-in ()
  "Zoom in on the image diff card at point."
  (interactive)
  (omnidiff-image--step-zoom 1))

(defun omnidiff-image-zoom-out ()
  "Zoom out on the image diff card at point."
  (interactive)
  (omnidiff-image--step-zoom -1))

(defun omnidiff-image-zoom-reset ()
  "Reset zoom on the image diff card at point to fit-to-window."
  (interactive)
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (state (omnidiff-image--state-for file)))
    (setf (omnidiff-image--state-zoom state) nil)
    (omnidiff-image--rerender-card card-ov t)
    (message "Omnidiff image zoom: fit")))

(defun omnidiff-image-cycle-backdrop ()
  "Cycle backdrop (`auto' -> `checker' -> `light' -> `dark' -> `none')."
  (interactive)
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (state (omnidiff-image--state-for file))
         (cur (or (omnidiff-image--state-backdrop state) 'auto))
         (next (pcase cur
                 ('auto 'checker)
                 ('checker 'light)
                 ('light 'dark)
                 ('dark 'none)
                 (_ 'auto))))
    (setf (omnidiff-image--state-backdrop state) next)
    (omnidiff-image--rerender-card card-ov t)
    (message "Omnidiff image backdrop: %s" next)))

;;; Changed-region navigation

(defun omnidiff-image-toggle-regions ()
  "Toggle changed pixel region boxes, 1:1 crop loupe, and coordinates."
  (interactive)
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (pic (overlay-get card-ov 'omnidiff-image-picture))
         (state (omnidiff-image--state-for file)))
    (unless (or (omnidiff-picture-regions pic) (> (omnidiff-picture-region-count pic) 0))
      (user-error "No changed pixel regions recorded for this image"))
    (if (and omnidiff-image-enabled (omnidiff-image--display-graphic-p))
        (setf (omnidiff-image--state-show-regions state)
              (not (omnidiff-image--state-show-regions state))
              (omnidiff-image--state-expand-identical state) t)
      (setf (omnidiff-image--state-tty-hide-regions state)
            (not (omnidiff-image--state-tty-hide-regions state))))
    (omnidiff-image--rerender-card card-ov t)))

(defun omnidiff-image-select-region (idx)
  "Highlight changed region at zero-based IDX and show its 1:1 crop loupe."
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (pic (overlay-get card-ov 'omnidiff-image-picture))
         (regions (omnidiff-picture-regions pic))
         (state (omnidiff-image--state-for file)))
    (unless regions
      (user-error "No listed changed regions for this image"))
    (let ((clamped (mod idx (length regions))))
      (setf (omnidiff-image--state-show-regions state) t
            (omnidiff-image--state-region-idx state) clamped)
      (omnidiff-image--rerender-card card-ov t)
      (let ((r (nth clamped regions)))
        (message "Region %d/%d: %d×%d at (%d, %d)"
                 (1+ clamped) (length regions)
                 (omnidiff-picture-region-width r)
                 (omnidiff-picture-region-height r)
                 (omnidiff-picture-region-x r)
                 (omnidiff-picture-region-y r))))))

(defun omnidiff-image-next-region ()
  "Select the next listed changed pixel region on the current image card."
  (interactive)
  (let* ((card-ov (omnidiff-image--require-card))
         (state (omnidiff-image--state-for (overlay-get card-ov 'omnidiff-image-file))))
    (omnidiff-image-select-region (1+ (omnidiff-image--state-region-idx state)))))

(defun omnidiff-image-prev-region ()
  "Select the previous listed changed pixel region on the current image card."
  (interactive)
  (let* ((card-ov (omnidiff-image--require-card))
         (state (omnidiff-image--state-for (overlay-get card-ov 'omnidiff-image-file))))
    (omnidiff-image-select-region (1- (omnidiff-image--state-region-idx state)))))

;;; Loading, exact version viewing & external viewer

(defun omnidiff-image-force-load (&optional all-p)
  "Force-load an oversized or deferred image preview at point.
With prefix argument ALL-P, load all deferred image cards in the buffer."
  (interactive "P")
  (if all-p
      (let ((count 0))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlay-get ov 'omnidiff-image-ov)
            (let* ((f (overlay-get ov 'omnidiff-image-file))
                   (st (omnidiff-image--state-for f)))
              (when (omnidiff-image--state-deferred st)
                (setf (omnidiff-image--state-force-load st) t
                      (omnidiff-image--state-deferred st) nil)
                (omnidiff-image--rerender-card ov t)
                (setq count (1+ count))))))
        (message "Loaded %d deferred image %s" count (if (= count 1) "card" "cards")))
    (let* ((card-ov (omnidiff-image--require-card))
           (file (overlay-get card-ov 'omnidiff-image-file))
           (state (omnidiff-image--state-for file)))
      (setf (omnidiff-image--state-force-load state) t
            (omnidiff-image--state-deferred state) nil)
      (omnidiff-image--rerender-card card-ov t))))

(defun omnidiff-image--unstaged-worktree-diff-p ()
  "Return non-nil when the current buffer displays an unstaged working-tree diff."
  (let ((cmd (and (boundp 'omnidiff--command) omnidiff--command)))
    (and (consp cmd)
         (proper-list-p cmd)
         (equal (nth 0 cmd) "git")
         (equal (nth 2 cmd) "diff")
         (let ((tail (nthcdr 4 cmd))
               (non-worktree nil))
           (while (and tail (not non-worktree))
             (let ((arg (pop tail)))
               (cond
                ((not (stringp arg)) (setq non-worktree t))
                ((string= arg "--") (setq tail nil))
                ((or (member arg '("--cached" "--staged"))
                     (string-search ".." arg)
                     (not (string-prefix-p "-" arg)))
                 (setq non-worktree t)))))
           (not non-worktree)))))

(defun omnidiff-image--open-disk-path-other-window (path side &optional file repo-dir)
  "Open existing regular disk file PATH for SIDE in another window.
For temporary paths or Git LFS pointer files, opens the copied/smudged
bytes via `omnidiff-image-open-blob-other-window' when FILE is supplied so
ephemeral temp files are never visited directly and LFS images open decoded.
Otherwise opens `before' read-only and real `after' files directly."
  (when-let* ((valid (omnidiff-image--valid-disk-path path)))
    (cond
     ((or (omnidiff-image--temp-path-p valid)
          (omnidiff-image--disk-file-lfs-pointer-p valid))
      (when file
        (omnidiff-image-open-blob-other-window file side repo-dir)))
     ((eq side 'before)
      (find-file-read-only-other-window valid)
      t)
     (t
      (find-file-other-window valid)
      t))))

(defun omnidiff-image-open-blob-other-window (file side repo-dir)
  "Open SIDE (`before' or `after') image blob of FILE in a read-only buffer."
  (when-let* ((pic (omnidiff-image-file-picture file)))
    (let ((bytes (omnidiff-image--fetch-side-bytes file pic side repo-dir t)))
      (when (and (stringp bytes) (> (length bytes) 0))
        (let* ((oid (if (eq side 'before)
                        (omnidiff-file-old-oid file)
                      (omnidiff-file-new-oid file)))
               (rev-tag (if oid
                            (substring oid 0 (min 7 (length oid)))
                          (if (eq side 'before) "disk" "worktree")))
               (name (format "*omnidiff image: %s (%s) [%s]*"
                             (omnidiff-file-target-path file) side rev-tag))
               (buf (get-buffer-create name)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (set-buffer-multibyte nil)
              (insert bytes)
              (set-buffer-modified-p nil))
            (ignore-errors (image-mode))
            (setq buffer-read-only t))
          (pop-to-buffer buf)
          t)))))

(defun omnidiff-image--pick-side (file state &optional prompt-p)
  "Resolve which side (`before' or `after') to open for FILE and STATE."
  (let ((st (omnidiff-file-status file)))
    (cond
     ((eq st 'deleted) 'before)
     ((eq st 'added) 'after)
     ((eq (omnidiff-image--state-mode state) 'blink)
      (omnidiff-image--state-blink-side state))
     (prompt-p
      (intern (completing-read "Open image version: " '("after" "before")
                               nil t nil nil "after")))
     (t 'after))))

(defun omnidiff-image-open-side ()
  "Open the exact compared image version (`before' or `after') in `image-mode'."
  (interactive)
  (let* ((card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (state (omnidiff-image--state-for file))
         (repo-dir (or (and (boundp 'omnidiff--repo-dir) omnidiff--repo-dir)
                       default-directory))
         (side (omnidiff-image--pick-side file state t))
         (oid (if (eq side 'before)
                  (omnidiff-file-old-oid file)
                (omnidiff-file-new-oid file)))
         (disk-path (if (eq side 'before)
                        (omnidiff-file-before-disk-path file)
                      (omnidiff-file-after-disk-path file))))
    (unless (or (and (null oid)
                     (omnidiff-image--open-disk-path-other-window disk-path side file repo-dir))
                (omnidiff-image-open-blob-other-window file side repo-dir))
      (user-error "Unable to load %s image blob for %s"
                  side (omnidiff-file-target-path file)))))

(defun omnidiff-image-jump-at-point ()
  "Handle `RET' on an image diff card, opening the exact compared version.
Returns non-nil if handled."
  (when-let* ((card-ov (omnidiff-image--card-ov-at)))
    (let* ((file (overlay-get card-ov 'omnidiff-image-file))
           (pic (overlay-get card-ov 'omnidiff-image-picture))
           (state (omnidiff-image--state-for file))
           (repo-dir (or (and (boundp 'omnidiff--repo-dir) omnidiff--repo-dir)
                         default-directory)))
      ;; Let `.svg' XML hunks fall through to normal source jump; handle raster & SVG preview cards.
      (when (and pic (or (not (omnidiff-picture-svg-p pic))
                         (eq card-ov (get-text-property (point) 'omnidiff-image-ov))))
        (let* ((side (omnidiff-image--pick-side file state nil))
               (oid (if (eq side 'before)
                        (omnidiff-file-old-oid file)
                      (omnidiff-file-new-oid file)))
               (disk-path (if (eq side 'before)
                              (omnidiff-file-before-disk-path file)
                            (omnidiff-file-after-disk-path file)))
               (rel (if (eq side 'before)
                        (omnidiff-file-path file)
                      (omnidiff-file-target-path file)))
               (full (when (and repo-dir rel (not (string-empty-p rel)))
                       (omnidiff-image--expand-path rel repo-dir))))
          (cond
           ;; Direct disk-path pair (non-Git or extracted member) when OID is nil.
           ((and (null oid)
                 (omnidiff-image--valid-disk-path disk-path))
            (omnidiff-image--open-disk-path-other-window disk-path side file repo-dir))
           ;; Unstaged working-tree AFTER that exists on disk -> visit file directly
           ;; (or open decoded blob if temporary or a Git LFS pointer).
           ((and (eq side 'after)
                 (or (null oid)
                     (omnidiff-image--unstaged-worktree-diff-p))
                 full
                 (omnidiff-image--valid-disk-path full))
            (omnidiff-image--open-disk-path-other-window full side file repo-dir))
           ;; Otherwise open the exact Git blob being compared (never a mismatched worktree copy).
           ((omnidiff-image-open-blob-other-window file side repo-dir)
            t)
           ((and full
                 (omnidiff-image--valid-disk-path full)
                 (not (omnidiff-image--temp-path-p full))
                 (not (omnidiff-image--disk-file-lfs-pointer-p full)))
            (find-file-other-window full)
            t)
           (t nil)))))))

(defun omnidiff-image--external-viewer-argv ()
  "Return base argv list for opening an external image viewer, or nil."
  (cond
   ((listp omnidiff-image-external-viewer)
    omnidiff-image-external-viewer)
   ((stringp omnidiff-image-external-viewer)
    (list omnidiff-image-external-viewer))
   ((and (eq system-type 'darwin) (executable-find "open"))
    (list (executable-find "open")))
   ((executable-find "xdg-open")
    (list (executable-find "xdg-open")))
   (t nil)))

(defun omnidiff-image-open-external ()
  "Export the active image version(s) to temp files and open in system viewer."
  (interactive)
  (let* ((viewer (or (omnidiff-image--external-viewer-argv)
                     (user-error "No external image viewer found; set `omnidiff-image-external-viewer'")))
         (card-ov (omnidiff-image--require-card))
         (file (overlay-get card-ov 'omnidiff-image-file))
         (pic (overlay-get card-ov 'omnidiff-image-picture))
         (state (omnidiff-image--state-for file))
         (repo-dir (or (and (boundp 'omnidiff--repo-dir) omnidiff--repo-dir)
                       default-directory))
         (sides (cond
                 ((eq (omnidiff-file-status file) 'deleted) '(before))
                 ((eq (omnidiff-file-status file) 'added) '(after))
                 ((eq (omnidiff-image--state-mode state) 'blink)
                  (list (omnidiff-image--state-blink-side state)))
                 (t '(before after))))
         (tmpdir (make-temp-file "omnidiff-ext-" t))
         (paths nil))
    (push tmpdir omnidiff-image--ext-tmpdirs)
    (dolist (side sides)
      (let ((bytes (omnidiff-image--fetch-side-bytes file pic side repo-dir t)))
        (when (and (stringp bytes) (> (length bytes) 0))
          (let* ((rel (if (eq side 'before)
                          (omnidiff-file-path file)
                        (omnidiff-file-target-path file)))
                 (base (file-name-sans-extension (file-name-nondirectory rel)))
                 (ext (or (let ((tm (omnidiff-image--detect-type bytes rel)))
                            (when tm (omnidiff-image--type-extension (car tm))))
                          (file-name-extension rel t)
                          ".img"))
                 (dest (expand-file-name (format "%s-%s%s" base side ext) tmpdir)))
            (omnidiff-image--write-binary-temp dest bytes)
            (push dest paths)))))
    (setq paths (nreverse paths))
    (unless paths
      (user-error "No image data available to open externally"))
    (apply #'start-process "omnidiff-ext-viewer" nil (append viewer paths))
    (message "Opened %s externally" (string-join (mapcar #'file-name-nondirectory paths) ", "))))

;;; Public entry points for `omnidiff-render' & `omnidiff'

(defun omnidiff-image-insert-body (file pic &optional hunk-root-p)
  "Insert interactive image diff body for FILE and PIC at point.
When HUNK-ROOT-P is non-nil, mark the first line as an `omnidiff-hunk' target
so `n'/`p' lands directly on the card without a redundant hunk header.
Returns the `omnidiff-image-ov' overlay tracking the card body."
  (omnidiff-image--ensure-caches)
  (let* ((state (omnidiff-image--state-for file))
         (start (point))
         (card-ov (make-overlay start start)))
    (overlay-put card-ov 'omnidiff-image-ov t)
    (overlay-put card-ov 'omnidiff-ov t)
    (overlay-put card-ov 'omnidiff-image-file file)
    (overlay-put card-ov 'omnidiff-image-picture pic)
    (overlay-put card-ov 'omnidiff-image-hunk-root-p (and hunk-root-p t))
    (omnidiff-image--insert-card-content file pic state card-ov t)
    (let ((end (point)))
      (move-overlay card-ov start end)
      (omnidiff-image--apply-card-properties start end file card-ov))
    card-ov))

(defun omnidiff-image-should-render-preview-p (pic)
  "Return non-nil when PIC should render a card in the current buffer.
Raster pictures always render (GUI visual or TTY summary); SVG previews
render alongside XML hunks only in graphical frames."
  (if (omnidiff-picture-svg-p pic)
      (and omnidiff-image-enabled
           omnidiff-image-preview-svg
           (omnidiff-image--display-graphic-p)
           (image-type-available-p 'svg))
    t))

(defun omnidiff-image--probe-picture-info (path)
  "Probe `omnidiff-picture-info' for existing regular file PATH, or nil."
  (when-let* ((valid (omnidiff-image--valid-disk-path path)))
    (let* ((abs (expand-file-name valid))
           (attrs (file-attributes (file-truename abs)))
           (sz (or (and attrs (file-attribute-size attrs)) 0))
           (head (let ((file-name-handler-alist nil))
                   (ignore-errors
                     (with-temp-buffer
                       (set-buffer-multibyte nil)
                       (insert-file-contents-literally abs nil 0 65536)
                       (buffer-string)))))
           (tm (when head (omnidiff-image--detect-type head abs)))
           (type (car tm))
           (dims (or (when (and head type)
                       (omnidiff-image--probe-bytes-dims head type))
                     (when (and type
                                (omnidiff-image--display-graphic-p)
                                (image-type-available-p type)
                                (or (<= omnidiff-image-max-bytes 0)
                                    (<= sz omnidiff-image-max-bytes)))
                       (ignore-errors
                         (let ((d (image-size (create-image abs type) t)))
                           (when (and (consp d) (> (car d) 0) (> (cdr d) 0))
                             (cons (round (car d)) (round (cdr d)))))))))
           (fmt (pcase type
                  ('png "PNG")
                  ('jpeg "JPEG")
                  ('gif "GIF")
                  ('webp "WebP")
                  ('tiff "TIFF")
                  ('bmp "BMP")
                  ('ico "ICO")
                  ('heic "HEIC")
                  ('svg "SVG")
                  (_ (let ((ext (file-name-extension abs)))
                       (if (and ext (not (string-empty-p ext)))
                           (upcase ext)
                         "IMG")))))
           (color (when (and (eq type 'png) (stringp head) (>= (length head) 26)
                             (string-prefix-p omnidiff-image--png-magic head))
                    (let ((depth (aref head 24))
                          (ctype (aref head 25)))
                      (pcase ctype
                        (0 (format "Gray%d" depth))
                        (2 (format "RGB%d" depth))
                        (3 "Indexed")
                        (4 (format "GrayA%d" depth))
                        (6 (format "RGBA%d" depth))
                        (_ nil))))))
      (make-omnidiff-picture-info
       :format fmt
       :width (or (car dims) 0)
       :height (or (cdr dims) 0)
       :color color
       :bytes sz))))

(defun omnidiff-image-make-file-from-paths (before-path after-path
                                                        &optional picture display-path)
  "Construct an `omnidiff-file' for a standalone image pair on disk.
BEFORE-PATH and AFTER-PATH are filesystem paths (or nil / `\"/dev/null\"' for
added/deleted images).  When PICTURE is provided (e.g. an `omnidiff-picture'
parsed from JSON), it is attached directly; otherwise a minimal
`omnidiff-picture' is constructed by probing image headers when possible.
Optional DISPLAY-PATH overrides `:path' and `:target-path'."
  (let* ((before-present (and (stringp before-path)
                              (not (string-empty-p before-path))
                              (not (string= before-path "/dev/null"))))
         (after-present (and (stringp after-path)
                             (not (string-empty-p after-path))
                             (not (string= after-path "/dev/null"))))
         (before-disk (when before-present (expand-file-name before-path)))
         (after-disk (when after-present (expand-file-name after-path)))
         (status (cond
                  ((and (not before-present) after-present) 'added)
                  ((and before-present (not after-present)) 'deleted)
                  (t 'modified)))
         (disp (when (and (stringp display-path) (not (string-empty-p display-path)))
                 display-path))
         (path (or disp before-disk after-disk ""))
         (target (or disp after-disk before-disk ""))
         (pic (or picture
                  (let* ((b-info (when before-disk
                                   (omnidiff-image--probe-picture-info before-disk)))
                         (a-info (when after-disk
                                   (omnidiff-image--probe-picture-info after-disk)))
                         (svg-p (or (and b-info (string= (omnidiff-picture-info-format b-info) "SVG"))
                                    (and a-info (string= (omnidiff-picture-info-format a-info) "SVG"))
                                    (and (null b-info) (null a-info)
                                         (or (omnidiff-image--svg-path-p before-disk)
                                             (omnidiff-image--svg-path-p after-disk)
                                             (omnidiff-image--svg-path-p disp)))))
                         (comp (cond
                                ((memq status '(added deleted)) 'one-sided)
                                (svg-p 'svg)
                                (t 'unknown))))
                    (make-omnidiff-picture
                     :before b-info
                     :after a-info
                     :comparison comp
                     :svg-p svg-p)))))
    (make-omnidiff-file
     :path path
     :target-path target
     :status status
     :before-disk-path before-disk
     :after-disk-path after-disk
     :picture pic)))

(defun omnidiff-image--menu-state-summary ()
  "Return concise live status header for `omnidiff-image-menu'."
  (if-let* ((ov (omnidiff-image--card-ov-at))
            (file (overlay-get ov 'omnidiff-image-file))
            (state (omnidiff-image--state-for file)))
      (let* ((mode (or (omnidiff-image--state-mode state) 'auto))
             (z (omnidiff-image--state-zoom state))
             (bd (or (omnidiff-image--state-backdrop state) omnidiff-image-backdrop)))
        (format "OmniDiff Image — %s [%s · zoom:%s · backdrop:%s]"
                (omnidiff-file-target-path file)
                mode
                (if z (format "%d%%" (round (* 100.0 z))) "fit")
                bd))
    "OmniDiff Image"))

(transient-define-prefix omnidiff-image-menu ()
  "Interactive command dispatch menu for OmniDiff image diff cards."
  [:description omnidiff-image--menu-state-summary
   ["Comparison Mode"
    ("1" "2-up side-by-side" omnidiff-image-mode-side-by-side)
    ("2" "Stacked vertical" omnidiff-image-mode-stacked)
    ("3" "Swipe curtain" omnidiff-image-mode-swipe)
    ("4" "Pixel diff" omnidiff-image-mode-diff)
    ("5" "Onion skin" omnidiff-image-mode-onion)
    ("6" "Blink A/B" omnidiff-image-mode-blink)
    ("i" "Cycle mode" omnidiff-image-cycle-mode)
    ("b" "Flip BEFORE / AFTER" omnidiff-image-toggle-side)]
   ["Inspect & Tune"
    ("h" "Decrease swipe/onion/diff" omnidiff-image-adjust-decrease)
    ("l" "Increase swipe/onion/diff" omnidiff-image-adjust-increase)
    ("=" "Reset parameter" omnidiff-image-adjust-reset)
    ("+" "Zoom in" omnidiff-image-zoom-in)
    ("-" "Zoom out" omnidiff-image-zoom-out)
    ("0" "Zoom fit" omnidiff-image-zoom-reset)
    ("t" "Cycle backdrop" omnidiff-image-cycle-backdrop)
    ("s" "Cycle diff style (spotlight/mask/delta)" omnidiff-image-cycle-diff-style)
    ("r" "Toggle changed regions & loupe" omnidiff-image-toggle-regions)
    (">" "Next changed region" omnidiff-image-next-region)
    ("<" "Previous changed region" omnidiff-image-prev-region)]
   ["Open & Buffer"
    ("o" "Open exact revision in Emacs" omnidiff-image-open-side)
    ("x" "Open in external viewer" omnidiff-image-open-external)
    ("L" "Force-load deferred card" omnidiff-image-force-load)
    ("TAB" "Fold / unfold card" omnidiff-toggle-fold)
    ("v" "Cycle Overview layout" omnidiff-cycle-layout)
    ("T" "Open in native TUI" omnidiff-open-in-tui)
    ("q" "Quit viewer" quit-window)]])

(provide 'omnidiff-image)
;;; omnidiff-image.el ends here
