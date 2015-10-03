(deftheme justin
  "Created 2015-03-17.")

(custom-theme-set-faces
 'justin
 '(default ((t (:foreground "#AAA" :background "black"))))
 '(cursor ((t (:foreground "gray4" :background "aquamarine"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "gray30"))))
 '(minibuffer-prompt ((t (:foreground "medium sea green"))))
 '(highlight ((t (:underline (:color foreground-color :style line) :foreground "light gray" :background "DarkGreen"))))
 '(region ((t (:foreground "white" :background "dark slate blue"))))
 '(shadow ((t (:foreground "gray30"))))
 '(secondary-selection ((t (:foreground "white" :background "LightSteelBlue4"))))
 '(trailing-whitespace ((t (:foreground "light gray" :background "IndianRed1"))))
 '(font-lock-builtin-face ((t (:foreground "sandy brown"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "midnight blue"))))
 '(font-lock-comment-face ((t (:foreground "gray40"))))
 '(font-lock-constant-face ((t (:foreground "pale violet red"))))
 '(font-lock-doc-face ((t (:foreground "khaki"))))
 '(font-lock-function-name-face ((t (:foreground "firebrick"))))
 '(font-lock-keyword-face ((t (:foreground "steel blue"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-preprocessor-face ((t (:foreground "khaki"))))
 '(font-lock-regexp-grouping-backslash ((t (:weight bold))))
 '(font-lock-regexp-grouping-construct ((t (:weight bold))))
 '(font-lock-string-face ((t (:foreground "slate grey"))))
 '(font-lock-type-face ((t (:weight bold :foreground "SeaGreen3"))))
 '(font-lock-variable-name-face ((t (:foreground "IndianRed1"))))
 '(font-lock-warning-face ((t (:foreground "white" :background "indian red"))))
 '(button ((t (:underline (:color foreground-color :style line)))))
 '(link ((t (:underline (:color foreground-color :style line) :foreground "cyan1"))))
 '(link-visited ((t (:foreground "violet" :underline (:color foreground-color :style line)))))
 '(fringe ((t (:foreground "gray21" :background "gray6"))))
 '(header-line ((t (:box (:line-width 2 :color "gray8" :style nil) :foreground "white" :background "gray8"))))
 '(tooltip ((t (:foreground "black" :background "lightyellow" :family "Sans Serif"))))
 '(mode-line ((t (:box (:line-width 2 :color nil :style released-button) :inverse-video nil :foreground "light gray" :background "gray10"))))
 '(mode-line-buffer-id ((t (:weight bold :box (:line-width 2 :color nil :style released-button) :inverse-video nil :foreground "gray61" :background "gray8"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color nil :style released-button) :inverse-video nil :foreground "black" :background "gray20"))))
 '(mode-line-inactive ((t (:box (:line-width 2 :color nil :style released-button) :inverse-video nil :foreground "gray30" :background "gray5"))))
 '(isearch ((t (:foreground "lemon chiffon" :background "steel blue"))))
 '(isearch-fail ((t (:foreground "lemon chiffon" :background "indian red"))))
 '(lazy-highlight ((t (:foreground "lemon chiffon" :background "DarkGreen"))))
 '(match ((t (:foreground "light gray" :background "DodgerBlue3"))))
 '(next-error ((t (:background "dark slate blue" :foreground "white"))))
 '(query-replace ((t (:background "steel blue" :foreground "lemon chiffon")))))

(provide-theme 'justin)