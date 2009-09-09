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

(add-to-list 'load-path "~/.emacs.d")

; Set font
(try-this
 (setq default-frame-alist '((font . "Monaco-7.5"))))

(try-independently
 (set-background-color "#080808")
 (set-foreground-color "#e6e3d8")
 (set-cursor-color "#656565")
 (set-face-foreground 'font-lock-comment-face "#99968b")
 (set-face-italic-p 'font-lock-comment-face t)
 (set-face-foreground 'font-lock-doc-face "#99968b")
 (set-face-italic-p 'font-lock-doc-face t)
 (set-face-foreground 'font-lock-constant-face "#e5786d")
 (set-face-foreground 'font-lock-string-face "#95e454")
 (set-face-italic-p 'font-lock-string-face t)
 (set-face-foreground 'font-lock-variable-name-face "#cae682")
 (set-face-foreground 'font-lock-function-name-face "#cae682")
 (set-face-foreground 'font-lock-type-face "#cae682")
 (set-face-foreground 'font-lock-builtin-face "#8ac6f2")
 (set-face-foreground 'font-lock-keyword-face "#8ac6f2")
 (set-face-foreground 'font-lock-preprocessor-face "#e5786d")
 (set-face-foreground 'font-lock-negation-char-face "#e7f6da")
 (set-face-foreground 'link "#8ac6f2")
 (set-face-bold-p 'link t)
 (set-face-underline-p 'link t)
 (set-face-foreground 'show-paren-match "#f6f3e8")
 (set-face-background 'show-paren-match "#857b6f")
 (set-face-bold-p 'show-paren-match t)
 (set-face-foreground 'region "#f6f3e8")
 (set-face-background 'region "#444444")
 (set-face-foreground 'lazy-highlight "black")
 (set-face-background 'lazy-highlight "yellow"))

; Setup menu's etc.
(try-independently
 (show-paren-mode t)
 (scroll-bar-mode -1)
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (setq inhibit-startup-message t)
 (setq require-final-newline t)
 (setq ring-bell-function 'ignore)
 (setq mac-pass-command-to-system nil))

(try-this
 (autoload 'python-mode "python-mode" "Python Mode." t)
 (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

;; code checking via flymake
;; set code checker here from "epylint", "pyflakes"
(setq pycodechecker "pyflakes")
(try-this
 (when (load "flymake" t)
   (defun flymake-pycodecheck-init ()
     (let* ((temp-file (flymake-init-create-temp-buffer-copy
			'flymake-create-temp-inplace))
	    (local-file (file-relative-name
			 temp-file
			 (file-name-directory buffer-file-name))))
       (list "epylint" (list local-file))))
   (add-to-list 'flymake-allowed-file-name-masks
		'("\\.py\\'" flymake-pycodecheck-init))))


(add-hook 'find-file-hook 'flymake-find-file-hook)

 
;pymacs, ropemacs
(try-this
 (require 'pymacs)
 (autoload 'pymacs-apply "pymacs")
 (autoload 'pymacs-call "pymacs")
 (autoload 'pymacs-eval "pymacs" nil t)
 (autoload 'pymacs-exec "pymacs" nil t)
 (autoload 'pymacs-load "pymacs" nil t)

 (pymacs-load "ropemacs" "rope-")
 (setq ropemacs-enable-autoimport t))

; autocomplete-mode
(try-this
 (require 'auto-complete)
 (global-auto-complete-mode t))

; css-mode
(try-this
 (autoload 'css-mode "css-mode" "Mode for editing CSS files" t)
 (setq auto-mode-alist
       (append '(("\\.css$" . css-mode))
	       auto-mode-alist)))

; SLIME
(try-this
 (setq inferior-lisp-program "/usr/bin/sbcl")
 ;(add-to-list 'load-path "~/.emacs.d/slime")
 (require 'slime)
 (slime-setup '(slime-fancy slime-asdf))
 (define-key global-map (kbd "<f12>") 'slime-selector)

 (require 'paredit)

 (define-key slime-mode-map (kbd "(") 'paredit-open-parenthesis)
 (define-key slime-mode-map (kbd ")") 'paredit-close-parenthesis)
 
 (define-key slime-mode-map (kbd "\"") 'paredit-doublequote)
 (define-key slime-mode-map (kbd "\\") 'paredit-backslash)
 
 (define-key slime-mode-map (kbd "RET") 'paredit-newline)
 (define-key slime-mode-map (kbd "<return>") 'paredit-newline)
 (define-key slime-mode-map (kbd "C-j") 'newline)
 
;;;; nb: this assumes dvorak key layout
 (define-key slime-mode-map (kbd "C-h") 'backward-sexp)
 (define-key slime-mode-map (kbd "C-t") 'transpose-sexps)
 (define-key slime-mode-map (kbd "C-M-t") 'transpose-chars)
 (define-key slime-mode-map (kbd "C-n") 'forward-sexp)
 (define-key slime-mode-map (kbd "C-k") 'kill-sexp)
 (define-key slime-mode-map (kbd "C-M-k") 'paredit-kill)
 (define-key slime-mode-map (kbd "C-'") 'paredit-splice-sexp)
 (define-key slime-mode-map (kbd "C-M-l") 'paredit-recentre-on-sexp)
 (define-key slime-mode-map (kbd "C-,") 'paredit-backward-slurp-sexp)
 (define-key slime-mode-map (kbd "C-.") 'paredit-forward-slurp-sexp)
 (define-key slime-mode-map (kbd "C-<") 'paredit-backward-barf-sexp)
 (define-key slime-mode-map (kbd "C->") 'paredit-forward-barf-sexp)
 (define-key slime-mode-map (kbd "C-/") 'backward-up-list)
 (define-key slime-mode-map (kbd "C-=") 'down-list)
 (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
 (define-key slime-mode-map (kbd "C-c TAB") 'slime-complete-form)
;;;; this may seem strange, but i often use the C-<whatever> motion
;;;; commands in sequence to reformat code and having to take a finger off of control
;;;; to add a return is a pain
 (define-key slime-mode-map (kbd "C-<return>") 'paredit-newline)
;;;; i hate having to take my key off of ctrl for this and i don't use complete-form anyway...
 (define-key slime-mode-map (kbd "C-c C-i") 'slime-inspect)
 (define-key global-map (kbd "<f12>") 'slime-selector))

; javascript-mode
(try-this
 (autoload 'js2-mode "js2" nil t)
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

 (add-to-list 'auto-mode-alist '("\\.\\(html\\|rng\\|xhtml\\)$" . html-mode)))

(try-this
 (require 'nav))

;; Bind hippie-expand
(try-this
 (global-set-key [(meta f1)] (make-hippie-expand-function
			      '(try-expand-dabbrev-visible
				try-expand-dabbrev
				try-expand-dabbrev-all-buffers) t)))
(try-this
 (server-start))


;; Use this for remote so I can specify command line arguments
(defun remote-term (new-buffer-name cmd &rest switches)
  (setq term-ansi-buffer-name (concat "*" new-buffer-name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name 
	(apply 'term-ansi-make-term term-ansi-buffer-name cmd nil switches))
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer term-ansi-buffer-name))



