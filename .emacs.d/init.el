(add-to-list 'load-path "~/.emacs.d/libs")


(require 'package)
(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'auto-compile)
(auto-compile-on-load-mode 1)

(dolist (dir (directory-files "~/.emacs.d/conf.d" t ".*\.el$"))
  (load dir))

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$"
          ""
          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defmacro try-this (&rest body)
  `(unwind-protect
       (let (retval (gensym))
         (condition-case ex
             (setq retval (progn ,@body))
           ('error
            (message (format "Caught exception: [%s]" ex))
            (setq retval (cons 'exception (list ex)))))
         retval)))

(defun auto-load-mode (mode extensions &optional mode-fn)
  ; If not already a list, wrap it in one.
  (setq extensions (if (listp extensions) extensions (list extensions))
        extension-fn (if mode-fn mode-fn (symbol-name mode))
        regex (concat "\\(" (mapconcat 'identity extensions "\\|") "\\)\\'"))
  (autoload mode extension-fn nil t)
  (add-to-list 'auto-mode-alist (cons regex mode)))

(defun mapply (func args)
  (dolist (someargs args)
    (apply func someargs)))

; Setup menu's etc.
(setq inhibit-startup-message t
      require-final-newline t
      ring-bell-function 'ignore
      mac-pass-command-to-system nil
      mac-option-key-is-meta nil
      mac-option-modifier 'hyper
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      ac-auto-start nil
      backup-by-copying-when-mismatch t
      make-backup-files nil
      icicle-image-files-in-Completions nil)

(setq-default indent-tabs-mode nil)
(setq c-default-style "python"
      c-basic-offset 4)
(global-auto-revert-mode 1)
(column-number-mode 1)
(show-paren-mode t)
(menu-bar-mode -1)

(defun window-mode-init ()
  "Set things up for a gui window."
  (global-unset-key "\C-z")
  (set-exec-path-from-shell-PATH)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 2)
  (auto-insert-mode 1)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'reverse)
  (when (eq system-type 'darwin)
    (set-face-attribute 'default
                        nil
                        :font "DejaVu Sans Mono"
                        :height 120))
  (require 'midnight)
  (midnight-delay-set 'midnight-delay "4:30am")

  (require 'show-wspace)
  (require 'color-theme-justin)
  (color-theme-justin))

(defun text-mode-init ()
  "Set up for quick loading on a terminal window."
  (color-theme-dark-green))

(require 'color-theme)
(color-theme-initialize)
(if window-system
    (window-mode-init)
  (text-mode-init))

(autoload 'find-file-in-project "find-file-in-project" "Find file in project." t)

;; icicles
(require 'icicles)
(icy-mode 1)

(add-to-list 'completion-ignored-extensions "pyc")

(mapply 'global-set-key
        `((,(kbd "RET") newline-and-indent)
          (,(kbd "M-RET") toggle-frame-fullscreen)
          (,(kbd "C-\\") condense-whitespace)
          (,(kbd "M-c") kill-ring-save)
          (,(kbd "C-=") flymake-goto-next-error)
          (,(kbd "C-o") find-file-in-project)))

(require 'python)
;; Auto mode loading
(mapply 'auto-load-mode
        '((js2-mode ("\\.js" "\\.json") "js2")
          (html-mode "\\.html")
          (markdown-mode "\\.md")
          (sass-mode "\\.sass")
          (css-mode "\\.css")
          (coffee-mode "\\.coffee")
          (jinja2-mode "\\.jinja" "jinja2")
          (yaml-mode ("\\.yml" "\\.yaml"))
          ;(python-mode "\\.py" "python")
          (nxml-mode
           ("\\.xml" "\\.wsdl" "\\.svg" "\\.xslt"
            "\\.wsdd" "\\.xsl" "\\.rng" "\\.xhtml"))
          (cython-mode ("\\.pyx" "\\.pxd"))
          (ruby-mode "Rakefile")
          (peg-mode ("\\.peg"))
          (go-mode "\\.go")))

(mapply 'add-hook
        '((coffee-mode-hook
           (lambda () (set (make-local-variable 'tab-width) 2)))
          (go-mode-hook
           (lambda () (setq tab-width 4)))
          (nxml-mode-hook
           (lambda () (setq nxml-slash-auto-complete-flag t)))
          (before-save-hook delete-trailing-whitespace)
          (python-mode-hook show-ws-highlight-tabs)
          (python-mode-hook
           (lambda () (if (not (null buffer-file-name)) (flymake-mode))))
          ;(before-save-hook
	  ; gofmt-before-save)))
	  ))

(define-skeleton new-py-file
  "Python skeleton"
  ""
  "#!/usr/bin/env python\n"
  "# -*- coding: utf-8 -*-\n"
  "from __future__ import unicode_literals\n"
  "\n")

;(define-auto-insert "\\.py" 'new-py-file)

(autoload 'gofmt "go-mode")

;code checking via flymake
(try-this
 (when (load "flymake" t)
   (defun flymake-pylint-init ()
     (list "~/bin/lintrunner.sh"
           (list buffer-file-name)))
   (add-to-list 'flymake-allowed-file-name-masks
                '("^[^\*]+\\.py$" flymake-pylint-init))))

(defun recompile-everything-under-the-sun ()
  (interactive)
  (dolist (path load-path)
    (byte-recompile-directory path 0)))

;(require 'slime)
(require 'eldoc)
(dolist (hook (list
               'ielm-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'message-mode-hook
               'Info-mode-hook
               'erc-mode-hook
               'org-mode-hook
               ))
  (add-hook hook 'turn-on-eldoc-mode))
(setq eldoc-idle-delay 0)
