
;; (add-to-list 'load-path "~/.emacs.d")
;; (require 'cl)
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
;; (require 'auto-complete-clang-async)

;; ;; Select candidates with C-n/C-p only when completion menu is displayed:
;; (setq ac-use-menu-map t)
;; (define-key ac-menu-map "C-n" 'ac-next)
;; (define-key ac-menu-map "C-p" 'ac-previous)

;; (require 'xcscope)
;; (setq cscope-index-recursively t)

;; (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-filename ac-source-yasnippet))
;; (setq ac-candidate-limit 100) ;; do not stall with too many results
;; (setq ac-auto-start 0)
;; (setq ac-auto-show-menu t)
;; (setq ac-quick-help-delay 0)
;; (setq ac-use-fuzzy 1.5)
;; (setq ac-show-menu-immediately-on-auto-complete t)
;; (setq ac-expand-on-auto-complete nil)
;; (setq ac-quick-help-height 20)
;; (setq ac-menu-height 20)
;; (ac-set-trigger-key "TAB")
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)

;; (defun ac-cc-mode-setup ()
;;   (setq ac-clang-complete-executable "/opt/bin/clang-complete")

;;   (setq clang-completion-suppress-error 't)
;;   (setq ac-clang-cflags
;;         (mapcar (lambda (item)(concat "-I" item))
;;                 (split-string
;;                  "
;;  /usr/include/c++/4.8
;;  /usr/include/x86_64-linux-gnu/c++/4.8
;;  /usr/include/c++/4.8/backward
;;  /usr/lib/gcc/x86_64-linux-gnu/4.8/include
;;  /usr/local/include
;;  /usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed
;;  /usr/include/x86_64-linux-gnu
;;  /usr/include
;; " )))

;;   (setq ac-clang-cflags (append '("-std=c++11") ac-clang-cflags))
;;   (setq ac-sources '(ac-source-clang-async))
;;   (ac-clang-launch-completion-process))

;; (defun my-ac-config ()
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))

;; (my-ac-config)

;; (print ac-clang-cflags)
