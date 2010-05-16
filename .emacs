(require 'cl)
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/predictive")

(defmacro try-this (&rest body)
  `(unwind-protect
       (let (retval (gensym))
	 (condition-case ex
	     (setq retval (progn ,@body))
	   ('error
	    (message (format "Caught exception: [%s]" ex))
	    (setq retval (cons 'exception (list ex)))))
	 retval)))

(defmacro try-independently (&rest body)
  (let (retval (gensym))
    (dolist (x body retval) ()
	    (push `(try-this ,x) retval))
    (setq retval (reverse retval))
    (push 'progn retval)))

(defun condense-whitespace ()
  "Kill the whitespace between two non-whitespace characters"
  (interactive "*")
  (save-excursion
    (save-restriction
      (save-match-data
	(progn
	  (re-search-backward "[^ \t\r\n]" nil t)
	  (re-search-forward "[ \t\r\n]+" nil t)
	  (replace-match " " nil nil))))))


(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(global-set-key (kbd "M-c") 'whitespace-cleanup)
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "C-;") 'dabbrev-expand)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(try-this
 (require 'color-theme)
 (color-theme-initialize)
 (require 'color-theme-justin)
 (color-theme-justin))

; Set font
(try-this
 (set-frame-font "Menlo-12.5"))

(try-this
 (column-number-mode 1))

(try-this
 (require 'ido)
 (ido-mode t)
 (setq ido-enable-flex-matching t))
;(try-this
; (setq default-frame-alist '((font . "Monaco-9"))))

;(try-this
 ;(require 'sql)
 ;(defalias 'sql-get-login 'ignore))


; Setup menu's etc.
(try-independently
 (show-paren-mode t)
 (scroll-bar-mode -1)
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (tooltip-mode -1)
 (fringe-mode 'minimal)
 (setq inhibit-startup-message t)
 (setq require-final-newline t)
 (setq ring-bell-function 'ignore)
 (setq mac-pass-command-to-system nil)
 (setq mac-option-key-is-meta nil)
 (setq mac-command-key-is-meta t)
 (setq mac-command-modifier 'meta)
 (setq mac-option-modifier nil))


(try-this
 (autoload 'yaml-mode "yaml-mode" "yaml Mode." t)
 (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(try-this
 (autoload 'python-mode "python-mode" "Python Mode." t)
 (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

;code checking via flymake
;set code checker here from "epylint", "pyflakes"
(try-this
 (when (load "flymake" t)
   (defun flymake-pylint-init ()
     (list "ssh" (list
		  "captcrunch" "/pluto/pycloud/apps/emacs/bin/lintrunner.sh"
		  buffer-file-name)))
   (defun flymake-display-warning (warning)
     "Display a warning to the user, using lwarn"
     (message warning))

   (add-to-list 'flymake-allowed-file-name-masks
		'("\\.py\\'" flymake-pylint-init))))

(try-this
 (add-hook 'python-mode-hook '(lambda ()
				(if (not (null buffer-file-name)) (flymake-mode)))))


;; (try-independently
;;  (set-background-color "#040A04")
;;  (set-foreground-color "#EEEEEE")
;;  (set-cursor-color "#656565")
;;  (set-face-background 'flymake-errline "#500000")
;;  (set-face-foreground 'flymake-errline "#FFFFFF")
;;  (set-face-background 'flymake-warnline "#000050")
;;  (set-face-foreground 'flymake-warnline "#FFFFFF")
;;  (set-face-foreground 'font-lock-comment-face "#555540")
;;  (set-face-italic-p 'font-lock-comment-face nil)
;;  (set-face-foreground 'font-lock-doc-face "#393939")
;;  (set-face-italic-p 'font-lock-doc-face t)
;;  (set-face-foreground 'font-lock-constant-face "#e5786d")
;;  (set-face-foreground 'font-lock-string-face "#908080")
;;  (set-face-italic-p 'font-lock-string-face nil)
;;  (set-face-foreground 'font-lock-variable-name-face "#7a0402")
;;  (set-face-foreground 'font-lock-function-name-face "#992020")
;;  (set-face-foreground 'font-lock-type-face "#cae682")
;;  (set-face-foreground 'font-lock-builtin-face "#3030c0")
;;  (set-face-foreground 'font-lock-keyword-face "#8080ff")
;;  (set-face-foreground 'font-lock-preprocessor-face "#e5786d")
;;  (set-face-foreground 'font-lock-negation-char-face "#990000")
;;  (set-face-foreground 'link "#8ac6f2")
;;  (set-face-bold-p 'link t)
;;  (set-face-underline-p 'link t)
;;  (set-face-foreground 'show-paren-match "#000000")
;;  (set-face-background 'show-paren-match "#40FFC0")
;;  (set-face-bold-p 'show-paren-match t)
;;  (set-face-foreground 'region "#f6f3e8")
;;  (set-face-background 'region "#444444")
;;  (set-face-foreground 'lazy-highlight "black")
;;  (set-face-background 'lazy-highlight "yellow"))

(setq org-publish-blog-index ())

;(try-this
; (require 'auto-complete)
; (global-auto-complete-mode t))

(try-this
 (autoload 'css-mode "css-mode" nil t)
 (setq auto-mode-alist
       (append '(("\\.css$" . css-mode))
	       auto-mode-alist)))

; SLIME
(try-this
 (setq inferior-lisp-program
       (car (remove-if-not 'file-exists-p
			   '("/usr/local/bin/sbcl" "/usr/bin/sbcl"))))
 (setq common-lisp-hyperspec-root "~/.hyperspec")
 (add-to-list 'load-path "~/.slime")
 (autoload 'slime "slime"
   "Start an inferior^_superior Lisp and connect to its Swank server."
   t)
 (autoload 'slime-mode "slime"
   "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)."
   t)
 (eval-after-load "slime"
   '(slime-setup '(slime-fancy slime-asdf
			       slime-sbcl-exts
			       slime-compiler-notes-tree))))

;icicles
;(try-this
; (add-to-list 'load-path "~/.icicles")
; (require 'icicles)
; (icy-mode))

; javascript-mode
(try-this
 (autoload 'js2-mode "js2" nil t)
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

 (add-to-list 'auto-mode-alist '("\\.\\(html\\|rng\\|xhtml\\)$" . html-mode)))

(try-this
 (server-start))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 '(org-export-html-style-include-default nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(defun recompile-everything-under-the-sun ()
  (interactive)
  (dolist (path load-path)
    (byte-recompile-directory path 0)))
