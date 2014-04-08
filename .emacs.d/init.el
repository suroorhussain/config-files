(setq gc-cons-threshold 20000000)

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'load-path "~/.emacs.d/lisp")

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell
	 (replace-regexp-in-string
	  "[ \t\n]*$"
	  ""
	  (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(defun auto-load-mode (mode extensions &optional mode-fn)
					; If not already a list, wrap it in one.
  (setq extensions (if (listp extensions) extensions (list extensions))
	extension-fn (if mode-fn mode-fn (symbol-name mode))
	regex (concat "\\(" (mapconcat 'identity extensions "\\|") "\\)\\'"))
  (autoload mode extension-fn nil t)
  (add-to-list 'auto-mode-alist (cons regex mode)))

(auto-load-mode 'markdown-mode "\\.md")
(auto-load-mode 'sass-mode "\\.sass")
(auto-load-mode 'css-mode "\\.css")
(auto-load-mode 'coffee-mode "\\.coffee")

(auto-load-mode 'yaml-mode '("\\.yml" "\\.yaml"))
(auto-load-mode 'nxml-mode '("\\.xml" "\\.wsdl" "\\.svg" "\\.xslt" "\\.wsdd" "\\.xsl" "\\.rng" "\\.xhtml"))
(auto-load-mode 'ruby-mode "Rakefile")
(auto-load-mode 'go-mode "\\.go")

(autoload 'find-file-in-repository "find-file-in-repository" "Find file in repo." t)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(setq ac-auto-start t
      backup-by-copying-when-mismatch t
      icicle-image-files-in-Completions nil
      inhibit-startup-message t
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-key-is-meta nil
      mac-option-modifier 'hyper
      mac-pass-command-to-system nil
      make-backup-files nil
      require-final-newline t
      ring-bell-function 'ignore)

(auto-insert-mode 1)
(column-number-mode 1)
(global-auto-revert-mode 1)
(menu-bar-mode -1)
(show-paren-mode t)

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

(defun find-first-non-ascii-char ()
  "Find the first non-ascii character from point onwards."
  (interactive)
  (let (point)
    (save-excursion
      (setq point
	    (catch 'non-ascii
	      (while (not (eobp))
		(or (eq (char-charset (following-char))
			'ascii)
		    (throw 'non-ascii (point)))
		(forward-char 1)))))
    (if point
	(goto-char point)
      (message "No non-ascii characters."))))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "C-o") 'find-file-in-repository)

(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")


(define-skeleton new-py-file
  "Python skeleton"
  ""
  "#!/usr/bin/env python\n"
  "# -*- coding: utf-8 -*-\n"
  "from __future__ import unicode_literals\n"
  "\n")

(auto-load-mode 'cython-mode '("\\.pyx" "\\.pxd"))

(add-hook 'python-mode-hook
	  (lambda ()
	    (if (not (null buffer-file-name))
		(flymake-mode))))

(add-hook 'python-mode-hook 'show-ws-highlight-tabs)
(add-to-list 'completion-ignored-extensions "pyc")

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (list "~/bin/lintrunner.sh"
	  (list buffer-file-name)))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("^[^\*]+\\.py$" flymake-pylint-init)))
					;(setq inferior-lisp-program "/opt/sbcl/bin/sbcl")
					;(require 'slime-autoloads)
					;(slime-setup '(slime-fancy))
(setq-default indent-tabs-mode nil)
(setq c-default-style "python"
      c-basic-offset 4)

(require 'uniquify)
(defvar uniquify-buffer-name-style 'reverse)

(when window-system
  (when (eq system-type 'darwin)
    (set-face-attribute 'default
			nil
			:font "DejaVu Sans Mono"
			:height 120))
  (require 'flycheck)
  (global-flycheck-mode t)
  (global-set-key (kbd "M-n") 'flycheck-next-error)
  (global-set-key (kbd "M-p") 'flycheck-previous-error)

  (require 'color-theme)
  (require 'color-theme-justin)
  (color-theme-justin)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (global-unset-key "\C-z")
  (set-exec-path-from-shell-PATH)
  (scroll-bar-mode -1)
  (set-fringe-mode 2))
