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
 (set-frame-font "DejaVu Sans Mono-8"))

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
 (require 'sql)
 (defun sql-add-newline-first (output)
   "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
   (concat "\n" output))
 (defun sqli-add-hooks ()
   "Add hooks to `sql-interactive-mode-hook'."
   (add-hook 'comint-preoutput-filter-functions
	     'sql-add-newline-first))
 (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks))

(try-this
 (autoload 'yaml-mode "yaml-mode" "yaml Mode." t)
 (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;(try-this
; (autoload 'python-mode "python-mode" "Python Mode." t)
; (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
; (add-to-list 'interpreter-mode-alist '("python" . python-mode)))

;code checking via flymake
(try-this
 (when (load "flymake" t)
   (defun flymake-pylint-init ()
     (list "/pluto/pycloud/apps/emacs/bin/lintrunner.py"
	   (list buffer-file-name)))

   (add-to-list 'flymake-allowed-file-name-masks
        '("^[^\*]+\\.py$" flymake-pylint-init))))


;set code checker here from "epylint", "pyflakes"
;(try-this
; (when (load "flymake" t)
;   (defun flymake-pylint-init ()
;     (list "ssh" (list;
;		  "captcrunch" "/pluto/pycloud/apps/emacs/bin/lintrunner.sh"
;		  buffer-file-name)))
;   (defun flymake-display-warning (warning)
;     "Display a warning to the user, using lwarn"
;     (message warning))

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

;(try-this
; (require 'auto-complete)
; (global-auto-complete-mode t))

(try-this
 (autoload 'css-mode "css-mode" nil t)
 (setq auto-mode-alist
       (append '(("\\.css$" . css-mode))
           auto-mode-alist)))

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
