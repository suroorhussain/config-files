

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
