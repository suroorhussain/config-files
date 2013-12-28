(auto-load-mode 'js2-mode '("\\.js" "\\.json") "js2")

(defadvice js2-reparse (before json)
  (setq js2-buffer-file-name buffer-file-name))
(ad-activate 'js2-reparse)


(add-hook
 'c-mode-hook
 (function
  (lambda nil
    (if (string-match "postgresql" buffer-file-name)
        (progn
          (c-set-style "bsd")
          (setq c-basic-offset 4)
          (setq tab-width 4)
          (c-set-offset 'case-label '+)
          (setq fill-column 79)
          (setq indent-tabs-mode t))))))

(defadvice js2-parse-statement (around json)
  (if (and (= tt js2-LC)
           js2-buffer-file-name
           (string-equal (substring js2-buffer-file-name -5) ".json")
           (eq (+ (save-excursion
                    (goto-char (point-min))
                    (back-to-indentation)
                    (while (eolp)
                      (next-line)
                      (back-to-indentation))
                    (point)) 1) js2-ts-cursor))
      (setq ad-return-value (js2-parse-assign-expr))
    ad-do-it))
(ad-activate 'js2-parse-statement)
