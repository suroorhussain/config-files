(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/color-theme")
(add-to-list 'load-path "~/.emacs.d/icicles")

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

; Setup menu's etc.
(try-independently
 (show-paren-mode t)
 (setq inhibit-startup-message t)
 (setq require-final-newline t)
 (setq ring-bell-function 'ignore)
 (setq mac-pass-command-to-system nil)
 (setq mac-option-key-is-meta nil)
 (setq mac-command-key-is-meta t)
 (setq mac-command-modifier 'meta)
 (setq mac-option-modifier nil)
 (setq-default indent-tabs-mode nil)
 (global-auto-revert-mode 1)
 (column-number-mode 1))

(defun window-mode-init ()
  "Set things up for a gui window."
  (try-independently
   (scroll-bar-mode -1)
   (tool-bar-mode -1)
   (menu-bar-mode -1)
   (tooltip-mode -1))

  (try-this
   (set-fringe-mode 2))

  (try-this
   (require 'midnight)
   (midnight-delay-set 'midnight-delay "4:30am"))

  (try-this
   (require 'show-wspace)
   (add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs))

  (try-this
   (require 'color-theme-justin)
   (color-theme-justin))
  (try-this
   (server-start)))

(defun text-mode-init ()
  "Set up for quick loading on a terminal window."
  (color-theme-dark-green))

(try-this
  (require 'color-theme)
  (color-theme-initialize)
  (if window-system
      (window-mode-init)
    (text-mode-init)))

;; icicles
(try-this
 (require 'icicles)
 (icy-mode))

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
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "s-N") 'flymake-goto-next-error)
(global-set-key [mouse-16] 'revert-buffer)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Set font
;(try-this
; (set-frame-font "DejaVu Sans Mono-9"))
; (set-frame-font "Monaco-9"))

(try-this
 (add-to-list
  'auto-mode-alist
  '("\.\(xml\|svg\|wsdl\|xslt\|wsdd\|xsl\|rng\|xhtml\)\'" . nxml-mode) nil)
 (add-hook 'nxml-mode-hook '(lambda ()
                              (setq nxml-slash-auto-complete-flag t))))

(try-this
 (eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand)))

(try-this
 (autoload 'yaml-mode "yaml-mode" "yaml Mode." t)
 (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode)))

;code checking via flymake
(try-this
 (when (load "flymake" t)
   (defun flymake-pylint-init ()
     (list "~/bin/lintrunner.sh"
           (list buffer-file-name)))

   (add-to-list 'flymake-allowed-file-name-masks
        '("^[^\*]+\\.py$" flymake-pylint-init)))
 (add-hook 'python-mode-hook '(lambda ()
                                (if (not (null buffer-file-name)) (flymake-mode)))))

(try-this
 (autoload 'css-mode "css-mode" nil t)
 (setq auto-mode-alist
       (append '(("\\.css$" . css-mode))
           auto-mode-alist)))

; javascript-mode
(try-this
 (autoload 'js2-mode "js2" nil t)
 (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
 (add-to-list 'auto-mode-alist '("\\.\\(html\\|rng\\|xhtml\\)$" . html-mode)))

(defun recompile-everything-under-the-sun ()
  (interactive)
  (dolist (path load-path)
    (byte-recompile-directory path 0)))
