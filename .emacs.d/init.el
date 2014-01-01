(setq gc-cons-threshold 20000000)

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

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  ;(require 'auto-compile)
  ;(auto-compile-on-load-mode 1)
  ;(auto-compile-on-save-mode 1)

  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)

  (add-to-list 'load-path "~/.emacs.d/lisp")
  (dolist (fn (directory-files "~/.emacs.d/conf.d" t ".*\.el$"))
    (load (file-name-sans-extension fn)))
  (icy-mode 1)
  )


(defun compile-uncompiled (glob)
  (interactive "GPath glob: \n")
  (require 'em-glob)
  (dolist (fn (eshell-extended-glob glob))
    (unless (file-exists-p (concat fn "c"))
      (print (concat "automatically compiling: " fn))
      (toggle-auto-compile fn 'start))))
