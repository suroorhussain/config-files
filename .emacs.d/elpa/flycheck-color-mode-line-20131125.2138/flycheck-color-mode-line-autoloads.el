;;; flycheck-color-mode-line-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "flycheck-color-mode-line" "../../../../.emacs.d/elpa/flycheck-color-mode-line-20131125.2138/flycheck-color-mode-line.el"
;;;;;;  "2c7271e8eb9f5133efbd1c7ed718aafb")
;;; Generated autoloads from ../../../../.emacs.d/elpa/flycheck-color-mode-line-20131125.2138/flycheck-color-mode-line.el

(autoload 'flycheck-color-mode-line-mode "flycheck-color-mode-line" "\
Minor mode to color the mode line with the Flycheck status.

When called interactively, toggle
`flycheck-color-mode-line-mode'.  With prefix ARG, enable
`flycheck-color-mode-line-mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `flycheck-color-mode-line-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `flycheck-color-mode-line-mode'.
Otherwise behave as if called interactively.

\(fn &optional ARG)" t nil)

(custom-add-frequent-value 'flycheck-mode-hook 'flycheck-color-mode-line-mode)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/flycheck-color-mode-line-20131125.2138/flycheck-color-mode-line-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/flycheck-color-mode-line-20131125.2138/flycheck-color-mode-line.el")
;;;;;;  (21315 59822 423755 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; flycheck-color-mode-line-autoloads.el ends here
