

(defun glob-compile (glob)
  (interactive "GPath glob: \n")
  (require 'em-glob)
  (dolist (fn (eshell-extended-glob glob))
    (print (concat "automatically compiling: " fn))
    (byte-compile-file fn)))

(glob-compile "~/.emacs.d/**/*.el")
