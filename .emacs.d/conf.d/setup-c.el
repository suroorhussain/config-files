;(require 'google-c-style)
;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;; (require 'autopair)
;; (autopair-global-mode) ;; enable autopair in all buffers
;; '(autopair-blink t)
;; '(autopair-autowrap t)
;; prevent the { (opening brace) character from being autopaired in C++ comments:
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (push ?{
;;                     (getf autopair-dont-pair :comment))))
;; ;; Have '<' and '>' pair in c++-mode buffers, but only in code:
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (push '(?< . ?>)
;;                     (getf autopair-extra-pairs :code))))
;; ;; disable pair creation when there already is a non-whitespace character after the cursor
;; (defun autopair-dont-if-point-non-whitespace (action pair pos-before)
;;   (if (or (eq 'opening action) (eq 'insert-quote action))
;;       (let ((delete? (save-excursion
;;                        ;;move forward past the paired element
;;                        (goto-char (+ (point) 1))
;;                        (let* ((eol? (eq (point) (line-end-position)))
;;                               (next-whitespace (save-excursion (search-forward " " (point-max) t) (point)))
;;                               (next-char-is-whitespace? (eq next-whitespace (+ (point) 1)))
;;                               (delete? (not (or eol? next-char-is-whitespace?))))
;;                          delete?))))
;;         (if delete? (delete-char 1) 't))
;;     't))
;; (add-hook 'c++-mode-hook
;;           #'(lambda ()
;;               (setq autopair-handle-action-fns
;;                     (list #'autopair-default-handle-action
;;                           #'autopair-dont-if-point-non-whitespace))))
