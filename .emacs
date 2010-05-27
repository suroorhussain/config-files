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

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
;(global-set-key (kbd "M-c") 'whitespace-cleanup)
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "<M-f12>") 'revert-buffer)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key [mouse-16] 'revert-buffer)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(try-this
 (require 'show-wspace)
 (add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

(try-this
 (require 'color-theme)
 (color-theme-initialize)
 (require 'color-theme-justin)
 (color-theme-justin))

; Set font
(try-this
 (set-frame-font "Menlo-12"))

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
 (setq mac-option-modifier nil)
 (setq-default indent-tabs-mode nil))


(try-this
 (autoload 'yaml-mode "yaml-mode" "yaml Mode." t)
 (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

(try-this
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (list "ssh" (list;
		  "captcrunch" "/pluto/pycloud/apps/emacs/bin/lintrunner.sh"
		  buffer-file-name)))
  (defun flymake-display-warning (warning)
    "Display a warning to the user, using lwarn"
    (message warning))

   (add-to-list 'flymake-allowed-file-name-masks
        '("^[^\*]+\\.py$" flymake-pylint-init))))

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

(defun recompile-everything-under-the-sun ()
  (interactive)
  (dolist (path load-path)
    (byte-recompile-directory path 0)))
