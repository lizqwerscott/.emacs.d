;;; modus-vivendi-catppuccin-mocha-theme.el --- combine catppuccin-mocha with modus vivendi themes -*- lexical-binding:t -*-
;;; Commentary:
;;; Code:

(unless (and (fboundp 'require-theme)
             load-file-name
             (equal (file-name-directory load-file-name)
                    (expand-file-name "themes/" data-directory))
             (require-theme 'modus-themes t))
  (require 'modus-themes))

(defcustom modus-vivendi-catppuccin-mocha-palette-user nil
  "Like the `modus-vivendi--palette' for user-defined entries.
This is meant to extend the palette with custom named colors and/or
semantic palette mappings.  Those may then be used in combination with
palette overrides (also see `modus-themes-common-palette-overrides' and
`modus-vivendi--palette-overrides')."
  :group 'modus-themes
  :package-version '(modus-themes . "4.5.0")
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Option to extend the palette for use with overrides"))

(defcustom modus-vivendi-catppuccin-mocha-palette-overrides
  '((accent-0 "#89b4fa")
    (accent-1 "#89dceb")
    (bg-active bg-main)
    (bg-added "#364144")
    (bg-added-refine "#4A5457")
    (bg-changed "#3e4b6c")
    (bg-changed-refine "#515D7B")
    (bg-completion "#45475a")
    (bg-completion-match-0 "#1e1e2e")
    (bg-completion-match-1 "#1e1e2e")
    (bg-completion-match-2 "#1e1e2e")
    (bg-completion-match-3 "#1e1e2e")
    (bg-hl-line "#2a2b3d")
    (bg-hover-secondary "#585b70")
    (bg-line-number-active unspecified)
    (bg-line-number-inactive "#1e1e2e")
    (bg-main "#1e1e2e")
    (bg-mark-delete "#443245")
    (bg-mark-select "#3e4b6c")
    (bg-mode-line-active "#181825")
    (bg-mode-line-inactive "#181825")
    (bg-prominent-err "#443245")
    (bg-prompt unspecified)
    (bg-prose-block-contents "#313244")
    (bg-prose-block-delimiter bg-prose-block-contents)
    (bg-region "#585b70")
    (bg-removed "#443245")
    (bg-removed-refine "#574658")
    (bg-tab-bar      "#1e1e2e")
    (bg-tab-current  bg-main)
    (bg-tab-other    "#1e1e2e")
    (border-mode-line-active nil)
    (border-mode-line-inactive nil)
    (builtin "#89b4fa")
    (comment "#9399b2")
    (constant  "#f38ba8")
    (cursor  "#f5e0dc")
    (date-weekday "#89b4fa")
    (date-weekend "#fab387")
    (docstring "#a6adc8")
    (err     "#f38ba8")
    (fg-active fg-main)
    (fg-completion "#cdd6f4")
    (fg-completion-match-0 "#89b4fa")
    (fg-completion-match-1 "#f38ba8")
    (fg-completion-match-2 "#a6e3a1")
    (fg-completion-match-3 "#fab387")
    (fg-heading-0 "#f38ba8")
    (fg-heading-1 "#fab387")
    (fg-heading-2 "#f9e2af")
    (fg-heading-3 "#a6e3a1")
    (fg-heading-4 "#74c7ec")
    (fg-line-number-active "#b4befe")
    (fg-line-number-inactive "#7f849c")
    (fg-link  "#89b4fa")
    (fg-main "#cdd6f4")
    (fg-mark-delete "#f38ba8")
    (fg-mark-select "#89b4fa")
    (fg-mode-line-active "#bac2de")
    (fg-mode-line-inactive "#585b70")
    (fg-prominent-err "#f38ba8")
    (fg-prompt "#cba6f7")
    (fg-prose-block-delimiter "#9399b2")
    (fg-prose-verbatim "#a6e3a1")
    (fg-region "#cdd6f4")
    (fnname    "#89b4fa")
    (fringe "#1e1e2e")
    (identifier "#cba6f7")
    (info    "#94e2d5")
    (keyword   "#cba6f7")
    (keyword "#cba6f7")
    (name "#89b4fa")
    (number "#fab387")
    (property "#89b4fa")
    (string "#a6e3a1")
    (type      "#f9e2af")
    (variable  "#fab387")
    (warning "#f9e2af"))
  "Overrides for `modus-vivendi-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Modus themes,
refer to `modus-themes-common-palette-overrides'.

Theme-specific overrides take precedence over shared overrides.
The idea of common overrides is to change semantic color
mappings, such as to make the cursor red.  Wherea theme-specific
overrides can also be used to change the value of a named color,
such as what hexadecimal RGB value the red-warmer symbol
represents."
  :group 'modus-themes
  :package-version '(modus-themes . "4.0.0")
  :version "30.1"
  :type '(repeat (list symbol (choice symbol string)))
  :link '(info-link "(modus-themes) Palette overrides"))

(defconst modus-vivendi-cappuccin-mocha-custom-faces
  '(
    `(magit-section-highlight ((,c :background ,bg-alt)))
    `(magit-diff-file-heading-highlight ((,c :inherit magit-diff-file-heading :background ,bg-alt)))))

(defconst modus-vivendi-catppuccin-mocha-custom-faces
  '(`(change-log-acknowledgment ((,c :foreground "#b4befe")))
    `(change-log-date ((,c :foreground "#a6e3a1")))
    `(change-log-name ((,c :foreground "#fab387")))
    `(diff-context ((,c :foreground "#89b4fa")))
    `(diff-file-header ((,c :foreground "#f5c2e7")))
    `(diff-header ((,c :foreground "#89b4fa")))
    `(diff-hunk-header ((,c :foreground "#fab387")))
    `(gnus-button ((,c :foreground "#8aadf4")))
    `(gnus-group-mail-3 ((,c :foreground "#8aadf4")))
    `(gnus-group-mail-3-empty ((,c :foreground "#8aadf4")))
    `(gnus-header-content ((,c :foreground "#7dc4e4")));;
    `(gnus-header-from ((,c :foreground "#cba6f7")))
    `(gnus-header-name ((,c :foreground "#a6e3a1")))
    `(gnus-header-subject ((,c :foreground "#8aadf4")))
    `(log-view-message ((,c :foreground "#b4befe")))
    `(match ((,c :background "#3e5768" :foreground "#cdd6f5")))
    `(modus-themes-search-current ((,c :background "#f38ba8" :foreground "#11111b" )))
    `(modus-themes-search-lazy ((,c :background "#3e5768" :foreground "#cdd6f5")))
    `(newsticker-extra-face ((,c :foreground "#9399b2" :height 0.8 :slant italic)))
    `(newsticker-feed-face ((,c :foreground "#f38ba8" :height 1.2 :weight bold)))
    `(newsticker-treeview-face ((,c :foreground "#cdd6f4")))
    `(newsticker-treeview-selection-face ((,c :background "#3e5768" :foreground "#cdd6f5")))
    `(tab-bar ((,c :background "#1e1e2e" :foreground "#bac2de")))
    `(tab-bar-tab ((,c :background "#1e1e2e" :underline t)))
    `(tab-bar-tab-group-current ((,c :background "#1e1e2e" :foreground "#bac2de" :underline t)))
    `(tab-bar-tab-group-inactive ((,c :background "#1e1e2e" :foreground "#9399b2")))
    `(tab-bar-tab-inactive ((,c :background "#1e1e2e" :foreground "#a6adc8")))
    `(vc-dir-file ((,c :foreground "#89b4fa")))
    `(vc-dir-header-value ((,c :foreground "#b4befe")))))

(modus-themes-theme
 'modus-vivendi-catppuccin-mocha
 'modus-themes
 "Elegant, highly legible theme with a black background.
Conforms with the highest legibility standard for color contrast
between background and foreground in any given piece of text,
which corresponds to a minimum contrast in relative luminance of
7:1 (WCAG AAA standard)."
 'dark
 'modus-themes-vivendi-palette
 'modus-vivendi-catppuccin-mocha-palette-user
 'modus-vivendi-catppuccin-mocha-palette-overrides
 'modus-vivendi-catppuccin-mocha-custom-faces)

;;; modus-vivendi-catppuccin-mocha-theme.el ends here
