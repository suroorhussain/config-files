(setq gc-cons-threshold 20000000)

(defvar extension-fn)
(defvar regex)

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

(print "after-init-hook defined")
(defun my-after-init-hook ()
  (print "running after-init-hook")
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'load-path "~/.emacs.d/lisp")

  (print "loading conf.d")
  (dolist (fn (directory-files "~/.emacs.d/conf.d" t ".*\.el$"))
    (print fn)
    (load (file-name-sans-extension fn)))
  (icy-mode 1))
(add-hook 'after-init-hook 'my-after-init-hook)

;(defun compile-uncompiled (glob)
;  (interactive "GPath glob: \n")
;  (require 'em-glob)
;  (dolist (fn (eshell-extended-glob glob))
;    (unless (file-exists-p (concat fn "c"))
;      (print (concat "automatically compiling: " fn))
;      (toggle-auto-compile fn 'start))))
