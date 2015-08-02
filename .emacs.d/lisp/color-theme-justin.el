(defun color-theme-justin ()
  "Color theme by Justin Van Winkle, created 2011-10-09."
  (interactive)
  (color-theme-install
   '(color-theme-justin
     ((background-color . "black")
      (background-mode . dark)
      (border-color . "gray4")
      (cursor-color . "aquamarine")
      (foreground-color . "#AAA")
      (mouse-color . "khaki"))
     ((buffer-face-mode-face . variable-pitch)
      (compilation-message-face . underline)
      (diary-face . diary)
      (list-matching-lines-buffer-name-face . font-lock-type-face)
      (list-matching-lines-face . bold)
      (rst-block-face . font-lock-keyword-face)
      (rst-comment-face . font-lock-comment-face)
      (rst-definition-face . font-lock-function-name-face)
      (rst-directive-face . font-lock-builtin-face)
      (rst-emphasis1-face quote italic)
      (rst-emphasis2-face quote bold)
      (rst-external-face . font-lock-type-face)
      (rst-level-face-base-color . "grey")
      (rst-literal-face . font-lock-string-face)
      (rst-reference-face . font-lock-variable-name-face)
      (tags-tag-face . default)
      (view-highlight-face . highlight)
      (widget-mouse-face . highlight))
     (Buffer-menu-buffer-face
      ((t
	(:foreground "cadet blue"))))
     (ac-candidate-face
      ((t
	(:foreground "lightgray"))))
     (ac-selection-face
      ((t
	(:foreground "red"))))
     (bold
      ((t
	(:bold t :weight bold))))
     (bold-italic
      ((t
	(:italic t :bold t :slant italic :weight bold))))
     (bookmark-menu-heading
      ((t
	(:bold t :weight bold :foreground "SeaGreen3"))))
     (border
      ((t
	(:background "gray4"))))
     (buffer-menu-buffer
      ((t
	(:bold t :weight bold))))
     (button
      ((t
	(:underline t))))
     (calendar-today
      ((t
	(:foreground "khaki" :underline t))))
     (change-log-acknowledgement-face
      ((t
	(:background "cadet blue" :foreground "gray4"))))
     (change-log-conditionals-face
      ((t
	(:foreground "indian red"))))
     (change-log-date-face
      ((t
	(:foreground "lemon chiffon"))))
     (change-log-email-face
      ((t
	(:foreground "indian red"))))
     (change-log-file-face
      ((t
	(:foreground "cadet blue"))))
     (change-log-function-face
      ((t
	(:foreground "indian red"))))
     (change-log-list-face
      ((t
	(:foreground "LightSteelBlue4"))))
     (change-log-name-face
      ((t
	(:foreground "khaki"))))
     (comint-highlight-input
      ((t
	(:bold t :weight bold))))
     (comint-highlight-prompt
      ((t
	(:foreground "pale violet red"))))
     (compilation-column-number
      ((t
	(:bold t :weight bold :foreground "SeaGreen3"))))
     (compilation-error
      ((t
	(:foreground "dark orchid"))))
     (compilation-error-face
      ((t
	(:foreground "dark orchid"))))
     (compilation-info
      ((t
	(:foreground "dark orchid"))))
     (compilation-info-face
      ((t
	(:foreground "dark orchid"))))
     (compilation-line-number
      ((t
	(:foreground "IndianRed1"))))
     (compilation-warning
      ((t
	(:background "indian red" :foreground "white"))))
     (compilation-warning-face
      ((t
	(:background "indian red" :foreground "white"))))
     (completions-annotations
      ((t
	(:italic t :slant italic))))
     (completions-common-part
      ((t
	(nil))))
     (completions-first-difference
      ((t
	(nil))))
     (css-property
      ((t
	(:foreground "light sea green"))))
     (css-selector
      ((t
	(:foreground "indian red"))))
     (cursor
      ((t
	(:background "aquamarine" :foreground "gray4"))))
     (custom-button
      ((t
	(:background "lightgrey" :foreground "black" :box
		     (:line-width 2 :style released-button)))))
     (custom-button-face
      ((t
	(:background "lightgrey" :foreground "black" :box
		     (:line-width 2 :style released-button)))))
     (custom-button-mouse
      ((t
	(:background "grey90" :foreground "black" :box
		     (:line-width 2 :style released-button)))))
     (custom-button-pressed
      ((t
	(:background "lightgrey" :foreground "black" :box
		     (:line-width 2 :style pressed-button)))))
     (custom-button-pressed-face
      ((t
	(:background "lightgrey" :foreground "black" :box
		     (:line-width 2 :style pressed-button)))))
     (custom-button-pressed-unraised
      ((t
	(:underline t :foreground "violet"))))
     (custom-button-unraised
      ((t
	(:underline t))))
     (custom-changed
      ((t
	(:background "blue1" :foreground "white"))))
     (custom-comment
      ((t
	(:background "dim gray"))))
     (custom-comment-tag
      ((t
	(:foreground "gray80"))))
     (custom-documentation
      ((t
	(nil))))
     (custom-face-tag
      ((t
	(:bold t :weight bold :foreground "light blue"))))
     (custom-face-tag-face
      ((t
	(:bold t :weight bold :foreground "light blue"))))
     (custom-group-tag
      ((t
	(:bold t :family "Sans Serif" :foreground "light blue" :weight bold :height 1.2))))
     (custom-group-tag-1
      ((t
	(:bold t :family "Sans Serif" :foreground "pink" :weight bold :height 1.2))))
     (custom-invalid
      ((t
	(:background "red1" :foreground "yellow1"))))
     (custom-link
      ((t
	(:underline t :foreground "cyan1"))))
     (custom-modified
      ((t
	(:background "blue1" :foreground "white"))))
     (custom-rogue
      ((t
	(:background "black" :foreground "pink"))))
     (custom-saved
      ((t
	(:underline t))))
     (custom-set
      ((t
	(:background "white" :foreground "blue1"))))
     (custom-state
      ((t
	(:foreground "lime green"))))
     (custom-state-face
      ((t
	(:foreground "lime green"))))
     (custom-themed
      ((t
	(:background "blue1" :foreground "white"))))
     (custom-variable-button
      ((t
	(:bold t :underline t :weight bold))))
     (custom-variable-tag
      ((t
	(:bold t :foreground "light blue" :weight bold))))
     (custom-variable-tag-face
      ((t
	(:bold t :foreground "light blue" :weight bold))))
     (custom-visibility
      ((t
	(:underline t :foreground "cyan1"))))
     (diary
      ((t
	(:foreground "indian red"))))
     (diary-anniversary
      ((t
	(:foreground "steel blue"))))
     (diary-button
      ((t
	(:bold t :background "light gray" :foreground "gray4" :underline t :weight bold))))
     (diary-time
      ((t
	(:foreground "IndianRed1"))))
     (diff-hunk-header-face
      ((t
	(:background "gray30" :foreground "white"))))
     (dired-directory
      ((t
	(:foreground "khaki"))))
     (dired-face-boring
      ((t
	(:foreground "gray30"))))
     (dired-face-directory
      ((t
	(:foreground "khaki"))))
     (dired-face-executable
      ((t
	(:foreground "medium sea green"))))
     (dired-face-flagged
      ((t
	(:background "indian red" :foreground "white"))))
     (dired-face-header
      ((t
	(:foreground "medium sea green"))))
     (dired-face-marked
      ((t
	(:background "cadet blue" :foreground "gray4"))))
     (dired-face-permissions
      ((t
	(:foreground "gray30"))))
     (dired-face-setuid
      ((t
	(:bold t :foreground "medium sea green" :weight bold))))
     (dired-face-socket
      ((t
	(:foreground "gray30"))))
     (dired-face-symlink
      ((t
	(:foreground "cadet blue"))))
     (dired-flagged
      ((t
	(:background "indian red" :foreground "white"))))
     (dired-header
      ((t
	(:foreground "medium sea green"))))
     (dired-ignored
      ((t
	(:foreground "gray30"))))
     (dired-mark
      ((t
	(:foreground "khaki"))))
     (dired-marked
      ((t
	(:background "cadet blue" :foreground "gray4"))))
     (dired-perm-write
      ((t
	(:foreground "midnight blue"))))
     (dired-symlink
      ((t
	(:foreground "cadet blue"))))
     (dired-warning
      ((t
	(:background "indian red" :foreground "white"))))
     (ediff-current-diff-face-A
      ((t
	(:background "cadet blue" :foreground "gray4"))))
     (ediff-current-diff-face-Ancestor
      ((t
	(:background "IndianRed1" :foreground "light gray"))))
     (ediff-current-diff-face-B
      ((t
	(:background "lemon chiffon" :foreground "gray30"))))
     (ediff-current-diff-face-C
      ((t
	(:background "medium sea green" :foreground "light gray"))))
     (ediff-even-diff-face-A
      ((t
	(:bold t :background "cadet blue" :foreground "gray4" :weight bold))))
     (ediff-even-diff-face-Ancestor
      ((t
	(:bold t :background "IndianRed1" :foreground "light gray" :weight bold))))
     (ediff-even-diff-face-B
      ((t
	(:bold t :background "lemon chiffon" :foreground "gray30" :weight bold))))
     (ediff-even-diff-face-C
      ((t
	(:bold t :background "medium sea green" :foreground "light gray" :weight bold))))
     (ediff-fine-diff-face-A
      ((t
	(:background "steel blue" :foreground "gray4"))))
     (ediff-fine-diff-face-Ancestor
      ((t
	(:background "orange red" :foreground "gray4"))))
     (ediff-fine-diff-face-B
      ((t
	(:background "LightSteelBlue4" :foreground "white"))))
     (ediff-fine-diff-face-C
      ((t
	(:background "DarkGreen" :foreground "gray4"))))
     (ediff-odd-diff-face-A
      ((t
	(:bold t :background "steel blue" :foreground "gray4" :weight bold))))
     (ediff-odd-diff-face-Ancestor
      ((t
	(:bold t :background "orange red" :foreground "gray4" :weight bold))))
     (ediff-odd-diff-face-B
      ((t
	(:bold t :background "LightSteelBlue4" :foreground "white" :weight bold))))
     (ediff-odd-diff-face-C
      ((t
	(:bold t :background "DarkGreen" :foreground "gray4" :weight bold))))
     (escape-glyph
      ((t
	(:foreground "gray30"))))
     (eshell-ls-archive
      ((t
	(:foreground "indian red"))))
     (eshell-ls-archive-face
      ((t
	(:foreground "indian red"))))
     (eshell-ls-backup
      ((t
	(:foreground "LightSteelBlue4"))))
     (eshell-ls-backup-face
      ((t
	(:foreground "LightSteelBlue4"))))
     (eshell-ls-clutter
      ((t
	(:background "indian red" :foreground "white"))))
     (eshell-ls-clutter-face
      ((t
	(:background "indian red" :foreground "white"))))
     (eshell-ls-directory
      ((t
	(:foreground "khaki"))))
     (eshell-ls-directory-face
      ((t
	(:foreground "khaki"))))
     (eshell-ls-executable
      ((t
	(:foreground "medium sea green"))))
     (eshell-ls-executable-face
      ((t
	(:foreground "medium sea green"))))
     (eshell-ls-missing
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (eshell-ls-missing-face
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (eshell-ls-product
      ((t
	(:foreground "gray30"))))
     (eshell-ls-product-face
      ((t
	(:foreground "gray30"))))
     (eshell-ls-readonly
      ((t
	(:foreground "gray30"))))
     (eshell-ls-readonly-face
      ((t
	(:foreground "gray30"))))
     (eshell-ls-special
      ((t
	(:foreground "pale violet red"))))
     (eshell-ls-special-face
      ((t
	(:foreground "pale violet red"))))
     (eshell-ls-symlink
      ((t
	(:foreground "cadet blue"))))
     (eshell-ls-symlink-face
      ((t
	(:foreground "cadet blue"))))
     (eshell-ls-unreadable
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (eshell-ls-unreadable-face
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (eshell-prompt
      ((t
	(:foreground "pale violet red"))))
     (eshell-prompt-face
      ((t
	(:foreground "pale violet red"))))
     (ffap
      ((t
	(:underline t :foreground "light gray" :background "DarkGreen"))))
     (file-name-shadow
      ((t
	(:foreground "gray30"))))
     (fixed-pitch
      ((t
	(:family "Monospace"))))
     (flymake-errline
      ((t
	(:background "red4"))))
     (flymake-warnline
      ((t
	(:background "midnight blue"))))
     (font-lock-builtin-face
      ((t
	(:foreground "sandy brown"))))
     (font-lock-color-constant-face
      ((t
	(:foreground "khaki"))))
     (font-lock-comment-delimiter-face
      ((t
	(:foreground "midnight blue"))))
     (font-lock-comment-face
      ((t
	(:foreground "gray40"))))
     (font-lock-constant-face
      ((t
	(:foreground "pale violet red"))))
     (font-lock-doc-face
      ((t
	(:foreground "khaki"))))
     (font-lock-doc-string-face
      ((t
	(:foreground "medium purple"))))
     (font-lock-function-name-face
      ((t
	(:foreground "firebrick"))))
     (font-lock-keyword-face
      ((t
	(:foreground "steel blue"))))
     (font-lock-negation-char-face
      ((t
	(nil))))
     (font-lock-preprocessor-face
      ((t
	(:foreground "khaki"))))
     (font-lock-reference-face
      ((t
	(:foreground "khaki"))))
     (font-lock-regexp-grouping-backslash
      ((t
	(:bold t :weight bold))))
     (font-lock-regexp-grouping-construct
      ((t
	(:bold t :weight bold))))
     (font-lock-string-face
      ((t
	(:foreground "slate grey"))))
     (font-lock-type-face
      ((t
	(:bold t :foreground "SeaGreen3" :weight bold))))
     (font-lock-variable-name-face
      ((t
	(:foreground "IndianRed1"))))
     (font-lock-warning-face
      ((t
	(:background "indian red" :foreground "white"))))
     (fringe
      ((t
	(:background "gray6" :foreground "gray21"))))
     (header-line
      ((t
	(:background "gray8" :foreground "white" :box
		     (:line-width 2 :color "gray8" :style nil)))))
     (help-argument-name
      ((t
	(:foreground "IndianRed1"))))
     (highlight
      ((t
	(:background "DarkGreen" :foreground "light gray" :underline t))))
     (highline-face
      ((t
	(:background "gray30" :foreground "white"))))
     (highline-vertical-face
      ((t
	(:background "gray30" :foreground "white"))))
     (holiday
      ((t
	(:background "chocolate4" :foreground "pale violet red"))))
     (html-helper-bold-face
      ((t
	(nil))))
     (html-helper-italic-face
      ((t
	(nil))))
     (html-helper-link-face
      ((t
	(nil))))
     (html-helper-significant-tag-face
      ((t
	(:foreground "cadet blue"))))
     (html-helper-strikethrough-face
      ((t
	(nil))))
     (html-helper-underline-face
      ((t
	(nil))))
     (hyper-apropos-apropos-heading
      ((t
	(:foreground "sandy brown"))))
     (hyper-apropos-apropos-warning
      ((t
	(:background "indian red" :foreground "white"))))
     (hyper-apropos-documentation
      ((t
	(:foreground "white"))))
     (hyper-apropos-hyperlink
      ((t
	(:underline t))))
     (hyper-apropos-major-heading
      ((t
	(:foreground "lemon chiffon"))))
     (hyper-apropos-section-heading
      ((t
	(:foreground "khaki"))))
     (icicle-Completions-instruction-1
      ((t
	(:foreground "#AC4AAC4A0000"))))
     (icicle-Completions-instruction-2
      ((t
	(:foreground "#0000D53CD53C"))))
     (icicle-candidate-part
      ((t
	(:background "#451700143197"))))
     (icicle-common-match-highlight-Completions
      ((t
	(:foreground "#2017A71F2017"))))
     (icicle-complete-input
      ((t
	(:foreground "#B19E6A64B19E"))))
     (icicle-completion
      ((t
	(:foreground "#0000D53CD53C"))))
     (icicle-current-candidate-highlight
      ((t
	(:background "#69D40A460000"))))
     (icicle-extra-candidate
      ((t
	(:background "#4517305D0000"))))
     (icicle-historical-candidate
      ((t
	(:foreground "#DBD599DF0000"))))
     (icicle-input-completion-fail
      ((t
	(:background "#22225F5F2222"))))
     (icicle-input-completion-fail-lax
      ((t
	(:background "#00005E3B5A8D"))))
     (icicle-match-highlight-Completions
      ((t
	(:foreground "#1F1FA21CA21C"))))
     (icicle-match-highlight-minibuffer
      ((t
	(:underline t))))
     (icicle-mode-line-help
      ((t
	(:foreground "#AC4AAC4A0000"))))
     (icicle-multi-command-completion
      ((t
	(:background "#8B3500007533" :foreground "#0000D53CD53C"))))
     (icicle-mustmatch-completion
      ((t
	(nil))))
     (icicle-proxy-candidate
      ((t
	(:background "#316B22970000"))))
     (icicle-saved-candidate
      ((t
	(:background "gray20"))))
     (icicle-search-context-level-1
      ((t
	(:background "#4AE3524C4A11"))))
     (icicle-search-context-level-2
      ((t
	(:background "#524C4A114E2E"))))
     (icicle-search-context-level-3
      ((t
	(:background "#524C4FD34A11"))))
     (icicle-search-context-level-4
      ((t
	(:background "#4C894A11524C"))))
     (icicle-search-context-level-5
      ((t
	(:background "#0843529E0000"))))
     (icicle-search-context-level-6
      ((t
	(:background "#529E0000294F"))))
     (icicle-search-context-level-7
      ((t
	(:background "#529E39D30000"))))
     (icicle-search-context-level-8
      ((t
	(:background "#18C80000529E"))))
     (icicle-search-current-input
      ((t
	(:background "#7F0D00007F0D" :foreground "White"))))
     (icicle-search-main-regexp-current
      ((t
	(:background "#00004AA652F1"))))
     (icicle-search-main-regexp-others
      ((t
	(:background "#348608690000"))))
     (icicle-special-candidate
      ((t
	(:background "#176900004E0A"))))
     (icicle-whitespace-highlight
      ((t
	(:background "#000093F402A2"))))
     (info-header-node
      ((t
	(:italic t :bold t :weight bold :slant italic :foreground "white"))))
     (info-header-xref
      ((t
	(:foreground "cyan1" :underline t))))
     (info-menu-header
      ((t
	(:bold t :family "Sans Serif" :weight bold))))
     (info-menu-star
      ((t
	(:foreground "red1"))))
     (info-node
      ((t
	(:italic t :bold t :foreground "white" :slant italic :weight bold))))
     (info-title-1
      ((t
	(:bold t :weight bold :family "Sans Serif" :height 1.728))))
     (info-title-2
      ((t
	(:bold t :family "Sans Serif" :weight bold :height 1.44))))
     (info-title-3
      ((t
	(:bold t :weight bold :family "Sans Serif" :height 1.2))))
     (info-title-4
      ((t
	(:bold t :family "Sans Serif" :weight bold))))
     (info-xref
      ((t
	(:underline t :foreground "cyan1"))))
     (info-xref-visited
      ((t
	(:foreground "violet" :underline t))))
     (isearch
      ((t
	(:background "steel blue" :foreground "lemon chiffon"))))
     (isearch-fail
      ((t
	(:background "indian red" :foreground "lemon chiffon"))))
     (isearch-secondary
      ((t
	(:background "indian red" :foreground "lemon chiffon"))))
     (iswitchb-current-match
      ((t
	(:foreground "cadet blue"))))
     (iswitchb-invalid-regexp
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (iswitchb-single-match
      ((t
	(:foreground "medium sea green"))))
     (iswitchb-virtual-matches
      ((t
	(:foreground "sandy brown"))))
     (italic
      ((t
	(:italic t :slant italic))))
     (js2-builtin-face
      ((t
	(:foreground "sandy brown"))))
     (js2-comment-face
      ((t
	(:foreground "dark orchid"))))
     (js2-constant-face
      ((t
	(:foreground "pale violet red"))))
     (js2-error-face
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (js2-external-variable-face
      ((t
	(:foreground "indian red"))))
     (js2-function-name-face
      ((t
	(:foreground "cadet blue"))))
     (js2-function-param-face
      ((t
	(:foreground "IndianRed1"))))
     (js2-instance-member-face
      ((t
	(:foreground "IndianRed1"))))
     (js2-jsdoc-html-tag-delimiter-face
      ((t
	(:foreground "gray30"))))
     (js2-jsdoc-html-tag-name-face
      ((t
	(:foreground "cadet blue"))))
     (js2-jsdoc-tag-face
      ((t
	(:foreground "medium orchid"))))
     (js2-jsdoc-type-face
      ((t
	(:foreground "medium orchid"))))
     (js2-jsdoc-value-face
      ((t
	(:foreground "medium orchid"))))
     (js2-keyword-face
      ((t
	(:foreground "steel blue"))))
     (js2-private-function-call-face
      ((t
	(:foreground "cadet blue"))))
     (js2-private-member-face
      ((t
	(:foreground "IndianRed1"))))
     (js2-regexp-face
      ((t
	(:foreground "khaki"))))
     (js2-string-face
      ((t
	(:foreground "lemon chiffon"))))
     (js2-type-face
      ((t
	(:foreground "medium sea green"))))
     (js2-variable-name-face
      ((t
	(:foreground "IndianRed1"))))
     (js2-warning-face
      ((t
	(:background "indian red" :foreground "white"))))
     (lazy-highlight
      ((t
	(:background "DarkGreen" :foreground "lemon chiffon"))))
     (link
      ((t
	(:foreground "cyan1" :underline t))))
     (link-visited
      ((t
	(:underline t :foreground "violet"))))
     (makefile-shell-face
      ((t
	(nil))))
     (makefile-space-face
      ((t
	(:background "IndianRed1" :foreground "light gray"))))
     (makefile-targets-face
      ((t
	(nil))))
     (match
      ((t
	(:background "DodgerBlue3" :foreground "light gray"))))
     (menu
      ((t
	(:background "light gray" :foreground "gray4"))))
     (message-cited-text
      ((t
	(:foreground "indian red"))))
     (message-cited-text-face
      ((t
	(:foreground "indian red"))))
     (message-header-cc
      ((t
	(:foreground "medium sea green"))))
     (message-header-cc-face
      ((t
	(:foreground "medium sea green"))))
     (message-header-name
      ((t
	(:foreground "medium sea green"))))
     (message-header-name-face
      ((t
	(:foreground "medium sea green"))))
     (message-header-newsgroups
      ((t
	(:foreground "medium sea green"))))
     (message-header-newsgroups-face
      ((t
	(:foreground "medium sea green"))))
     (message-header-other
      ((t
	(:foreground "cadet blue"))))
     (message-header-other-face
      ((t
	(:foreground "cadet blue"))))
     (message-header-subject
      ((t
	(:foreground "lemon chiffon" :height 1.2))))
     (message-header-subject-face
      ((t
	(:foreground "lemon chiffon" :height 1.2))))
     (message-header-to
      ((t
	(:foreground "cadet blue"))))
     (message-header-to-face
      ((t
	(:foreground "cadet blue"))))
     (message-header-xheader
      ((t
	(:foreground "cadet blue"))))
     (message-header-xheader-face
      ((t
	(:foreground "cadet blue"))))
     (message-mml
      ((t
	(:foreground "medium sea green"))))
     (message-mml-face
      ((t
	(:foreground "medium sea green"))))
     (message-separator
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (message-separator-face
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (minibuffer-prompt
      ((t
	(:foreground "medium sea green"))))
     (mm-uu-extract
      ((t
	(:background "gray8" :foreground "white"))))
     (mode-line
      ((t
	(:background "gray10" :foreground "light gray" :inverse-video nil :box
		     (:line-width 2 :style released-button)))))
     (mode-line-buffer-id
      ((t
	(:bold t :background "gray8" :foreground "gray61" :inverse-video nil :box
	       (:line-width 2 :style released-button)
	       :weight bold))))
     (mode-line-emphasis
      ((t
	(:bold t :weight bold))))
     (mode-line-highlight
      ((t
	(:background "gray20" :foreground "black" :inverse-video nil :box
		     (:line-width 2 :style released-button)))))
     (mode-line-inactive
      ((t
	(:background "gray5" :foreground "gray30" :inverse-video nil :box
		     (:line-width 2 :style released-button)))))
     (modeline-mousable
      ((t
	(:background "gray8" :foreground "gray61" :inverse-video nil :box
		     (:line-width 2 :style released-button)))))
     (modeline-mousable-minor-mode
      ((t
	(:background "gray10" :foreground "gray30" :inverse-video nil :box
		     (:line-width 2 :style released-button)))))
     (mouse
      ((t
	(:background "khaki" :foreground "khaki"))))
     (multi-region-face
      ((t
	(:background "LightSteelBlue4" :foreground "white"))))
     (next-error
      ((t
	(:foreground "white" :background "dark slate blue"))))
     (nobreak-space
      ((t
	(:foreground "gray30" :underline t))))
     (ns-working-text-face
      ((t
	(:underline t))))
     (nxml-attribute-colon
      ((t
	(:foreground "sandy brown"))))
     (nxml-attribute-local-name
      ((t
	(:foreground "sandy brown"))))
     (nxml-attribute-prefix
      ((t
	(:foreground "sandy brown"))))
     (nxml-attribute-value
      ((t
	(:foreground "lemon chiffon"))))
     (nxml-attribute-value-delimiter
      ((t
	(:foreground "lemon chiffon"))))
     (nxml-cdata-section-CDATA
      ((t
	(:foreground "cadet blue"))))
     (nxml-cdata-section-content
      ((t
	(nil))))
     (nxml-cdata-section-delimiter
      ((t
	(:foreground "gray30"))))
     (nxml-char-ref-delimiter
      ((t
	(:foreground "IndianRed1"))))
     (nxml-char-ref-number
      ((t
	(:foreground "IndianRed1"))))
     (nxml-comment-content
      ((t
	(:foreground "dark orchid"))))
     (nxml-comment-delimiter
      ((t
	(:foreground "medium orchid"))))
     (nxml-element-colon
      ((t
	(:foreground "steel blue"))))
     (nxml-element-local-name
      ((t
	(:foreground "cadet blue"))))
     (nxml-element-prefix
      ((t
	(:foreground "steel blue"))))
     (nxml-entity-ref-delimiter
      ((t
	(:foreground "IndianRed1"))))
     (nxml-entity-ref-name
      ((t
	(:foreground "IndianRed1"))))
     (nxml-markup-declaration-delimiter
      ((t
	(:foreground "gray30"))))
     (nxml-namespace-attribute-colon
      ((t
	(:foreground "sandy brown"))))
     (nxml-namespace-attribute-prefix
      ((t
	(:foreground "sandy brown"))))
     (nxml-namespace-attribute-xmlns
      ((t
	(:foreground "sandy brown"))))
     (nxml-processing-instruction-content
      ((t
	(nil))))
     (nxml-processing-instruction-delimiter
      ((t
	(:foreground "gray30"))))
     (nxml-processing-instruction-target
      ((t
	(:foreground "steel blue"))))
     (nxml-prolog-keyword
      ((t
	(:foreground "cadet blue"))))
     (nxml-prolog-literal-content
      ((t
	(:foreground "lemon chiffon"))))
     (nxml-prolog-literal-delimiter
      ((t
	(:foreground "lemon chiffon"))))
     (nxml-tag-delimiter
      ((t
	(:foreground "gray30"))))
     (nxml-tag-slash
      ((t
	(:foreground "gray30"))))
     (nxml-text
      ((t
	(nil))))
     (paren-face
      ((t
	(:foreground "gray30"))))
     (paren-face-match
      ((t
	(:background "cadet blue" :foreground "gray4"))))
     (paren-face-mismatch
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (paren-face-no-match
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (primary-selection
      ((t
	(:background "dark slate blue" :foreground "white"))))
     (query-replace
      ((t
	(:foreground "lemon chiffon" :background "steel blue"))))
     (region
      ((t
	(:background "dark slate blue" :foreground "white"))))
     (rng-error
      ((t
	(:underline "red"))))
     (rst-level-1-face
      ((t
	(:background "black"))))
     (rst-level-2-face
      ((t
	(:background "black"))))
     (rst-level-3-face
      ((t
	(:background "black"))))
     (rst-level-4-face
      ((t
	(:background "black"))))
     (rst-level-5-face
      ((t
	(:background "black"))))
     (rst-level-6-face
      ((t
	(:background "black"))))
     (scroll-bar
      ((t
	(:background "light gray" :foreground "gray4"))))
     (secondary-selection
      ((t
	(:background "LightSteelBlue4" :foreground "white"))))
     (sgml-namespace-face
      ((t
	(:foreground "steel blue"))))
     (sh-heredoc-face
      ((t
	(:foreground "khaki"))))
     (sh-quoted-exec
      ((t
	(:foreground "khaki"))))
     (shadow
      ((t
	(:foreground "gray30"))))
     (show-paren-match
      ((t
	(:background "cadet blue" :foreground "gray4"))))
     (show-paren-mismatch
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (show-tabs-space
      ((t
	(:background "IndianRed1" :foreground "light gray"))))
     (show-tabs-space-face
      ((t
	(:background "IndianRed1" :foreground "light gray"))))
     (show-tabs-tab
      ((t
	(:background "IndianRed1" :foreground "light gray"))))
     (show-tabs-tab-face
      ((t
	(:background "IndianRed1" :foreground "light gray"))))
     (show-ws-hard-space
      ((t
	(:background "PaleGreen"))))
     (show-ws-tab
      ((t
	(:background "LemonChiffon"))))
     (show-ws-trailing-whitespace
      ((t
	(:background "Gold"))))
     (sldb-catch-tag-face
      ((t
	(:foreground "khaki"))))
     (sldb-condition-face
      ((t
	(:foreground "light sea green"))))
     (sldb-detailed-frame-line-face
      ((t
	(nil))))
     (sldb-frame-label-face
      ((t
	(:foreground "dark orchid"))))
     (sldb-frame-line-face
      ((t
	(nil))))
     (sldb-local-name-face
      ((t
	(:foreground "IndianRed1"))))
     (sldb-local-value-face
      ((t
	(:foreground "lemon chiffon"))))
     (sldb-reference-face
      ((t
	(:background "cyan"))))
     (sldb-restart-face
      ((t
	(:foreground "pale violet red"))))
     (sldb-restart-number-face
      ((t
	(:foreground "dark orchid"))))
     (sldb-restart-type-face
      ((t
	(:foreground "medium orchid"))))
     (sldb-section-face
      ((t
	(:foreground "sandy brown"))))
     (sldb-topline-face
      ((t
	(:foreground "IndianRed1"))))
     (slime-error-face
      ((t
	(:bold t :background "indian red" :foreground "white" :weight bold))))
     (slime-highlight-face
      ((t
	(:background "indian red" :foreground "white"))))
     (slime-inspector-action-face
      ((t
	(:background "cyan"))))
     (slime-inspector-label-face
      ((t
	(:foreground "indian red"))))
     (slime-inspector-topline-face
      ((t
	(:background "gray30" :foreground "white"))))
     (slime-inspector-type-face
      ((t
	(:foreground "light sea green"))))
     (slime-inspector-value-face
      ((t
	(:foreground "light pink"))))
     (slime-note-face
      ((t
	(:background "indian red" :foreground "white"))))
     (slime-reader-conditional-face
      ((t
	(:foreground "gray30"))))
     (slime-repl-input-face
      ((t
	(:foreground "pale violet red"))))
     (slime-repl-output-face
      ((t
	(:foreground "dark orchid"))))
     (slime-repl-prompt-face
      ((t
	(:foreground "pale violet red"))))
     (slime-repl-result-face
      ((t
	(:foreground "medium orchid"))))
     (slime-style-warning-face
      ((t
	(:background "indian red" :foreground "white"))))
     (slime-warning-face
      ((t
	(:background "indian red" :foreground "white"))))
     (speedbar-button-face
      ((t
	(:foreground "medium sea green"))))
     (speedbar-directory-face
      ((t
	(:foreground "khaki"))))
     (speedbar-file-face
      ((t
	(:foreground "cadet blue"))))
     (speedbar-highlight-face
      ((t
	(:background "DarkGreen" :foreground "light gray" :underline t))))
     (speedbar-selected-face
      ((t
	(:background "cadet blue" :foreground "gray4"))))
     (speedbar-tag-face
      ((t
	(:foreground "khaki"))))
     (tex-math
      ((t
	(:foreground "sandy brown"))))
     (tex-math-face
      ((t
	(:foreground "sandy brown"))))
     (tex-verbatim-face
      ((t
	(nil))))
     (tool-bar
      ((t
	(:background "light gray" :foreground "gray4" :inverse-video nil :underline t))))
     (tooltip
      ((t
	(:family "Sans Serif" :background "lightyellow" :foreground "black"))))
     (trailing-whitespace
      ((t
	(:background "IndianRed1" :foreground "light gray"))))
     (underline
      ((t
	(:underline t))))
     (variable-pitch
      ((t
	(:family "Sans Serif"))))
     (vertical-border
      ((t
	(nil))))
     (whitespace-highlight-face
      ((t
	(:background "IndianRed1" :foreground "light gray"))))
     (widget-button
      ((t
	(:bold t :underline t :weight bold))))
     (widget-button-pressed
      ((t
	(:bold t :foreground "red1" :underline t :weight bold))))
     (widget-documentation
      ((t
	(:foreground "medium sea green"))))
     (widget-field
      ((t
	(:background "gray30" :foreground "white"))))
     (widget-inactive
      ((t
	(:foreground "gray30"))))
     (widget-single-line-field
      ((t
	(:background "gray30" :foreground "white"))))
     (woman-addition-face
      ((t
	(:foreground "medium sea green"))))
     (woman-bold-face
      ((t
	(:bold t :weight bold))))
     (woman-italic-face
      ((t
	(:italic t :slant italic))))
     (woman-unknwon-face
      ((t
	(:foreground "cadet blue"))))
     (zmacs-region
      ((t
	(:background "dark slate blue" :foreground "white")))))))

(provide 'color-theme-justin)
