(require 'cl)

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
 (set-frame-font "-unknown-Monaco-normal-normal-normal-*-16-160-*-*-*-0-iso10646-1"))

;(try-this
; (setq default-frame-alist '((font . "Monaco-9"))))

(try-independently
 (set-background-color "#060606")
 (set-foreground-color "#c0c0c0")
 (set-cursor-color "#656565")
 (set-face-foreground 'font-lock-comment-face "#303030")
 (set-face-italic-p 'font-lock-comment-face t)
 (set-face-foreground 'font-lock-doc-face "#090909")
 (set-face-italic-p 'font-lock-doc-face t)
 (set-face-foreground 'font-lock-constant-face "#e5786d")
 (set-face-foreground 'font-lock-string-face "#908080")
 (set-face-italic-p 'font-lock-string-face t)
 (set-face-foreground 'font-lock-variable-name-face "#7a0402")
 (set-face-foreground 'font-lock-function-name-face "#992020")
 (set-face-foreground 'font-lock-type-face "#cae682")
 (set-face-foreground 'font-lock-builtin-face "#3030c0")
 (set-face-foreground 'font-lock-keyword-face "#8080ff")
 (set-face-foreground 'font-lock-preprocessor-face "#e5786d")
 (set-face-foreground 'font-lock-negation-char-face "#dd0000")
 (set-face-foreground 'link "#8ac6f2")
 (set-face-bold-p 'link t)
 (set-face-underline-p 'link t)
 (set-face-foreground 'show-paren-match "#006900")
 (set-face-background 'show-paren-match "#000000")
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
 (autoload 'yaml-mode "yaml-mode" "yaml Mode." t)
 (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(try-this
 (autoload 'python-mode "python-mode" "Python Mode." t)
 (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
 (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

code checking via flymake
set code checker here from "epylint", "pyflakes"
(setq pycodechecker "pyflakes")
(try-this
 (eval-after-load "python-mode"
   (when (load "flymake" t)
     (defun flymake-pycodecheck-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
			  'flymake-create-temp-inplace))
	      (local-file (file-relative-name
			   temp-file
			   (file-name-directory buffer-file-name))))
	 (list "epylint" (list local-file))))
     (add-to-list 'flymake-allowed-file-name-masks
		  '("\\.py\\'" flymake-pycodecheck-init)))))

(try-this
 (require 'htmlize)
 (require 'org)
 (require 'org-publish)
 (require 'org-blog)
 (setq org-hide-leading-stars t)
 (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
 (setq org-publish-project-alist
       (list
	'("blog" 
	  :author "Justin Van Winkle"
	  :email "justin.vanwinkle@gmail.com"
	  :base-directory "/home/jvanwink/blog/"
	  :base-extension "org"
	  :publishing-directory "~/.blog_deploy"
	  :publishing-function org-publish-org-to-html
	  :auto-index t
	  :blog-base-url "http://pythonorific.org/blog/"
	  :blog-title "Pythonorific"
	  :blog-description "Blogtastic"
	  :blog-export-rss t
	  ;:index-function org-publish-blog-index
	  :index-filename "index.org"
	  :index-title "Pythonorific"
	  :index-posts 2
	  :style "<style>pre.src {color:#e6e3d8; background:#080808;}</style><link rel=stylesheet href=\"files/style.css\" type=\"text/css\">"
	  :preamble "<div id=\"header\"><h1>Pythonorific</h1></div>"
	  :postamble "")
	'("images" :base-directory "~/blog/images/"
 	             :base-extension "jpg\\|gif\\|png"
 		     :publishing-directory "~/.blog_deploy/images/"
 		     :publishing-function org-publish-attachment)

	'("other"  :base-directory "~/blog/files/"
 	   	     :base-extension "css"
 		     :publishing-directory "~/.blog_deploy/files/"
 		     :publishing-function org-publish-attachment)
	'("website" :components ("orgfiles" "images" "other")))))

(setq org-publish-blog-index ())

(try-this
 (require 'auto-complete)
 (global-auto-complete-mode t))

(try-this
 (autoload 'css-mode "css-mode" nil t)
 (setq auto-mode-alist
       (append '(("\\.css$" . css-mode))
	       auto-mode-alist)))

; SLIME
(try-this
 (setq inferior-lisp-program
       (car (remove-if-not 'file-exists-p '("/usr/local/bin/sbcl" "/usr/bin/sbcl"))))
 (setq common-lisp-hyperspec-root "~/.hyperspec")
 (add-to-list 'load-path "~/.slime")
 (autoload 'slime "slime"
   "Start an inferior^_superior Lisp and connect to its Swank server."
   t)
 (autoload 'slime-mode "slime"
   "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)."
   t)
 (eval-after-load "slime"
   '(slime-setup '(slime-fancy slime-asdf slime-sbcl-exts slime-compiler-notes-tree))))

;icicles
(try-this
 (add-to-list 'load-path "~/.icicles")
 (require 'icicles)
 (icy-mode))

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







