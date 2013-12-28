(setq gc-cons-threshold 20000000)
(require 'package)
(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'color-theme-justin)
(color-theme-justin)

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
(scroll-bar-mode -1)
(set-fringe-mode 2)
(show-paren-mode t)
(tool-bar-mode -1)
(tooltip-mode -1)

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


(dolist (fn (directory-files "~/.emacs.d/conf.d" t ".*\.el$"))
  (load (file-name-sans-extension fn)))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq-default indent-tabs-mode nil)
(setq c-default-style "python"
      c-basic-offset 4)

(autoload 'find-file-in-repository "find-file-in-repository" "Find file in repo." t)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "C-=") 'flymake-goto-next-error)
(global-set-key (kbd "C-o") 'find-file-in-repository)

(auto-load-mode 'html-mode "\\.html")
(auto-load-mode 'markdown-mode "\\.md")
(auto-load-mode 'sass-mode "\\.sass")
(auto-load-mode 'css-mode "\\.css")
(auto-load-mode 'coffee-mode "\\.coffee")
(auto-load-mode 'jinja2-mode "\\.jinja" "jinja2")
(auto-load-mode 'yaml-mode '("\\.yml" "\\.yaml"))
(auto-load-mode 'nxml-mode '("\\.xml" "\\.wsdl" "\\.svg" "\\.xslt" "\\.wsdd" "\\.xsl" "\\.rng" "\\.xhtml"))
(auto-load-mode 'ruby-mode "Rakefile")
(auto-load-mode 'go-mode "\\.go")

(require 'uniquify)
(defvar uniquify-buffer-name-style 'reverse)
(require 'midnight)
(midnight-delay-set 'midnight-delay "4:30am")

(require 'icicles)
(icy-mode 1)

(defun compile-uncompiled ()
  (interactive)
  (require 'em-glob)
  (dolist (fn (eshell-extended-glob "~/.emacs.d/**/*.el"))
    (unless (file-exists-p (concat fn "c"))
      (unless (string-match-p "load" fn)
        (print (concat "automatically compiling: " fn))
        (toggle-auto-compile fn 'start)))))

(compile-uncompiled)
