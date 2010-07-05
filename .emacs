(require 'cl)
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme")

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
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "C-;") 'dabbrev-expand)
(global-set-key (kbd "<M-f12>") 'revert-buffer)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key [mouse-16] 'revert-buffer)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)

(try-this
 (require 'show-wspace)
 (add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs))

(try-this
 (require 'color-theme)
 (color-theme-initialize)
 (require 'color-theme-justin)
 (color-theme-justin))

; Set font
(try-this
 (set-frame-font "DejaVu Sans Mono-12")
 (set-frame-font "Menlo-12"))

(try-this
 (column-number-mode 1))

(try-this
 (require 'ido)
 (ido-mode t)
 (setq ido-enable-flex-matching t))

; Setup menu's etc.
(try-independently
 (show-paren-mode t)
 (scroll-bar-mode -1)
 (tool-bar-mode -1)
 (menu-bar-mode -1)
 (tooltip-mode -1)
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

;code checking via flymake
(try-this
 (when (load "flymake" t)
   (defun flymake-pylint-init ()
     (list "/pluto/pycloud/apps/emacs/bin/lintrunner.py"
	   (list buffer-file-name)))

   (add-to-list 'flymake-allowed-file-name-masks
        '("^[^\*]+\\.py$" flymake-pylint-init))))

;(try-this
;(when (load "flymake" t)
;  (defun flymake-pylint-init ()
;    (list "ssh" (list
;		  "captcrunch" "/pluto/pycloud/apps/emacs/bin/lintrunner.sh"
;		  buffer-file-name)))
;  (defun flymake-display-warning (warning)
;    "Display a warning to the user, using lwarn"
;    (message warning))

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

(defun recompile-everything-under-the-sun ()
  (interactive)
  (dolist (path load-path)
    (byte-recompile-directory path 0)))
