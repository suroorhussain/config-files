;;; find-file-in-repository-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "find-file-in-repository" "../../../../.emacs.d/elpa/find-file-in-repository-20130221.511/find-file-in-repository.el"
;;;;;;  "421bf4e9668c0092ad63cc829c1126aa")
;;; Generated autoloads from ../../../../.emacs.d/elpa/find-file-in-repository-20130221.511/find-file-in-repository.el

(autoload 'find-file-in-repository "find-file-in-repository" "\
find-file-in-repository will autocomplete all files in the
   current git, mercurial or other type of repository, using
   ido-find-file when available. When the current file is not
   located inside of any repository, falls back on a regular
   find-file operation.

\(fn)" t nil)

(defalias 'ffir 'find-file-in-repository)

(put 'ffir-repository-types 'safe-local-variable 'listp)

(put 'ffir-avoid-HOME-repository 'safe-local-variable 'booleanp)

;;;***

;;;### (autoloads nil nil ("../../../../.emacs.d/elpa/find-file-in-repository-20130221.511/find-file-in-repository-autoloads.el"
;;;;;;  "../../../../.emacs.d/elpa/find-file-in-repository-20130221.511/find-file-in-repository.el")
;;;;;;  (21315 61071 314558 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; find-file-in-repository-autoloads.el ends here
