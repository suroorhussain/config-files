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

(setq start-time (float-time))

(dolist (fn (directory-files "~/.emacs.d/conf.d" t ".*\.el$"))
  (load (file-name-sans-extension fn))
  (print (- (float-time) start-time))
  (setq start-time (float-time)))

(require 'color-theme-justin)
(color-theme-justin)

; Setup menu's etc.
(setq inhibit-startup-message t
      require-final-newline t
      ring-bell-function 'ignore
      mac-pass-command-to-system nil
      mac-option-key-is-meta nil
      mac-option-modifier 'hyper
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      ac-auto-start t
      backup-by-copying-when-mismatch t
      make-backup-files nil
      icicle-image-files-in-Completions nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)
(setq c-default-style "python"
      c-basic-offset 4)
(global-auto-revert-mode 1)
(column-number-mode 1)
(show-paren-mode t)


(autoload 'find-file-in-project "find-file-in-project" "Find file in project." t)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-\\") 'condense-whitespace)
(global-set-key (kbd "M-c") 'kill-ring-save)
(global-set-key (kbd "C-=") 'flymake-goto-next-error)
(global-set-key (kbd "C-o") 'find-file-in-project)

(auto-load-mode 'html-mode "\\.html")
(auto-load-mode 'markdown-mode "\\.md")
(auto-load-mode 'sass-mode "\\.sass")
(auto-load-mode 'css-mode "\\.css")
(auto-load-mode 'coffee-mode "\\.coffee")
(auto-load-mode 'jinja2-mode "\\.jinja" "jinja2")
(auto-load-mode 'yaml-mode '("\\.yml" "\\.yaml"))
(auto-load-mode 'nxml-mode '("\\.xml" "\\.wsdl" "\\.svg" "\\.xslt" "\\.wsdd" "\\.xsl" "\\.rng" "\\.xhtml"))
(auto-load-mode 'ruby-mode "Rakefile")
(auto-load-mode 'peg-mode '("\\.peg"))
(auto-load-mode 'go-mode "\\.go")

(require 'icicles)
(icy-mode 1)
