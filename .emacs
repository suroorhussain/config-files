;(set-default-font "Dejavu Sans Mono-10")

;(set-default-font "-bitstream-bitstream vera sans mono-medium-r-normal--10-100-72-72-m-100-iso8859-9")
;(set-default-font "-apple-andale mono-medium-r-normal--12-120-72-72-m-120-mac-roman")
;(set-default-font "-apple-monaco-medium-r-normal--12-120-72-72-m-120-mac-roman")
;(set-default-font "-apple-dejavu sans mono-medium-r-normal--14-140-72-72-m-140-mac-cyrillic")
(set-default-font "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-1")
(setq default-frame-alist
                '((font . "-misc-fixed-medium-r-normal--13-120-75-75-c-80-iso8859-1")))

(add-to-list 'load-path "~/.emacs.d")

; Setup menu's etc.
(show-paren-mode t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t)

(setq require-final-newline t)
(setq ring-bell-function 'ignore)
(setq mac-pass-command-to-system nil)

; Setup browser
;(when (executable-find "twb-browser")
;  (eval-after-load "browse-url"
;    '(defun browse-url-default-browser (url &rest unused)
;       (cond ((getenv "DISPLAY")
;              (call-process "twb-browser" nil 0 nil url))
;             ;;; This is BROKEN because Screen does test -t 0 before realizing
;             ;;; that it doesn't need a controlling tty to make a new window.
;             ;; ((getenv "STY")
;             ;;  (call-process "screen" nil 0 nil "twb-browser" url)
;             ;;  (call-process "screen" nil 0 nil "-X" "screen" "twb-browser" "http://google.com"))
;             (nil
;              (error "cannot start fancy-pants browser"))))))


; color-theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-billw)


; SLIME
(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime)
(slime-setup '(slime-fancy))



; javascript-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


; nxhtml-mode 
;(load "~/.emacs.d/nxhtml/autostart.el")
; nXML-mode

;(load "~/.emacs.d/nxml-mode-20041004/rng-auto.el")
;(setq auto-mode-alist
;      (cons '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|html\\)\\'" . nxml-mode);
;	    auto-mode-alist))

(server-start)


