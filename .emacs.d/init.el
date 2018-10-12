;;; -*- lexical-binding: t; -*-

(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'load-path "~/.emacs.d/lisp")

(defmacro call-if-defined (fun arg)
  `(if (functionp ',fun) (,fun ,arg)))

;; (defun set-exec-path-from-shell-PATH ()
;;   (let ((path-from-shell
;; 	 (replace-regexp-in-string
;; 	  "[ \t\n]*$"
;; 	  ""
;; 	  (shell-command-to-string
;;            "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq exec-path (split-string path-from-shell path-separator))))

(defun auto-load-mode (mode extensions &optional mode-fn)
  "If not already a list, wrap it in one."
  (setq extensions (if (listp extensions) extensions (list extensions))
	extension-fn (if mode-fn mode-fn (symbol-name mode))
	regex (concat "\\(" (mapconcat 'identity extensions "\\|") "\\)\\'"))
  (autoload mode extension-fn nil t)
  (add-to-list 'auto-mode-alist (cons regex mode)))

(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 80))

(defun set-all-80 ()
  "Set all windows in this frame to 80 columns"
  (interactive)
  (dolist (window (window-list))
    (adjust-window-trailing-edge window (- 80 (window-width window)) t)))

(auto-load-mode 'markdown-mode "\\.md")
(auto-load-mode 'sass-mode "\\.sass")
(auto-load-mode 'css-mode "\\.css")
(auto-load-mode 'coffee-mode "\\.coffee")
(auto-load-mode 'jinja2-mode "\\.jinja")
(auto-load-mode 'yaml-mode '("\\.yml" "\\.yaml"))
(auto-load-mode 'nxml-mode '("\\.xml" "\\.wsdl" "\\.svg"
                             "\\.xslt" "\\.wsdd" "\\.xsl"
                             "\\.rng" "\\.xhtml"))
(auto-load-mode 'ruby-mode "Rakefile")
(auto-load-mode 'go-mode "\\.go")

(autoload 'find-file-in-repository
  "find-file-in-repository"
  "Find file in repo." t)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-auto-start t)
 '(auto-insert-mode 1)
 '(backup-by-copying-when-mismatch t)
 '(c-basic-offset 4)
 '(c-default-style "stroustrup")
 '(column-number-mode 1)
 '(global-auto-revert-mode 1)
 '(icicle-image-files-in-Completions nil)
 '(inhibit-startup-screen t)
 '(mac-command-key-is-meta t)
 '(mac-command-modifier (quote meta))
 '(mac-option-key-is-meta nil)
 '(mac-option-modifier (quote hyper))
 '(mac-pass-command-to-system nil)
 '(make-backup-files nil)
 '(matlab-fill-code nil)
 '(menu-bar-mode -1)
 '(package-selected-packages
   (quote
    (clang-format lsp-mode rustic flycheck-rust use-package flycheck-clang-analyzer intero xterm-color js2-mode matlab-mode latex-pretty-symbols latex-preview-pane icicles yaml-mode virtualenvwrapper swiper jedi haskell-mode flycheck-color-mode-line elpy caml)))
 '(require-final-newline t)
 '(ring-bell-function (quote ignore))
 '(show-paren-mode t))

(defun condense-whitespace ()
  "Kill the whitespace between two non-whitespace characters."
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
(global-set-key (kbd "C-x ~") 'set-80-columns)

(require 'tramp)
(setq tramp-default-method "ssh")
(require 'tramp-sh nil t)
(setf tramp-ssh-controlmaster-options
      (concat "-o SendEnv TRAMP=yes " tramp-ssh-controlmaster-options))

(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")

(require 'virtualenvwrapper)
(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
(setq venv-location "~/.venv/")

(auto-load-mode 'cython-mode '("\\.pyx" "\\.pxd"))
(auto-load-mode 'gcode-mode '("\\.gcd" "\\.gcode"))

(add-to-list 'completion-ignored-extensions "pyc")

(setq-default indent-tabs-mode nil)

(when (eq system-type 'darwin)
  (set-face-attribute 'default
                      nil
                      :font "DejaVu Sans Mono"
                      :height 120))

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(global-set-key (kbd "M-n") 'flycheck-next-error)
(global-set-key (kbd "M-p") 'flycheck-previous-error)
(setq flycheck-python-pycompile-executable "/usr/bin/python3")
(setq flycheck-python-flake8-executable "/usr/local/bin/flake8")

(setq comint-output-filter-functions
      (remove 'ansi-color-process-output comint-output-filter-functions))

(add-hook 'shell-mode-hook
          (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

;; Also set TERM accordingly (xterm-256color)
;; You can also use it with eshell (and thus get color output from system ls):

(require 'eshell)

(add-hook 'eshell-before-prompt-hook
          (lambda ()
            (setq xterm-color-preserve-properties t)))
(use-package flycheck-clang-analyzer
  :ensure t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(require 'clang-format)
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)

(add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
(setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))

(load-theme 'justin t)
(call-if-defined tool-bar-mode -1)
(call-if-defined menu-bar-mode -1)
(call-if-defined tooltip-mode -1)
(call-if-defined scroll-bar-mode -1)
(call-if-defined set-fringe-mode 2)
(global-unset-key "\C-z")
;; (set-exec-path-from-shell-PATH)         ;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
