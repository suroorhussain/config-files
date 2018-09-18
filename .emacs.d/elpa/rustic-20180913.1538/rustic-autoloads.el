;;; rustic-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "rustic" "rustic.el" (0 0 0 0))
;;; Generated autoloads from rustic.el

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))

(autoload 'rustic-mode "rustic" "\
Major mode for Rust code.

\\{rustic-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic" '("rustic-")))

;;;***

;;;### (autoloads nil "rustic-babel" "rustic-babel.el" (0 0 0 0))
;;; Generated autoloads from rustic-babel.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-babel" '("org-babel-execute:rustic" "rustic-babel-")))

;;;***

;;;### (autoloads nil "rustic-cargo" "rustic-cargo.el" (0 0 0 0))
;;; Generated autoloads from rustic-cargo.el

(autoload 'rustic-cargo-clippy "rustic-cargo" "\
Run `cargo clippy'.

\(fn)" t nil)

(autoload 'rustic-cargo-test "rustic-cargo" "\
Run 'cargo test'.

\(fn)" t nil)

(autoload 'rustic-cargo-build "rustic-cargo" "\


\(fn)" t nil)

(autoload 'rustic-cargo-run "rustic-cargo" "\


\(fn)" t nil)

(autoload 'rustic-cargo-clean "rustic-cargo" "\


\(fn)" t nil)

(autoload 'rustic-cargo-check "rustic-cargo" "\


\(fn)" t nil)

(autoload 'rustic-cargo-bench "rustic-cargo" "\


\(fn)" t nil)

(autoload 'rustic-cargo-new "rustic-cargo" "\


\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-cargo" '("rustic-")))

;;;***

;;;### (autoloads nil "rustic-compile" "rustic-compile.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from rustic-compile.el

(autoload 'rustic-compile "rustic-compile" "\
Compile rust project.
If called without arguments use `rustic-compile-command'.

Otherwise use provided argument ARG and store it in
`compilation-arguments'.

\(fn &optional ARG)" t nil)

(autoload 'rustic-recompile "rustic-compile" "\
Re-compile the program using `compilation-arguments'.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-compile" '("rust")))

;;;***

;;;### (autoloads nil "rustic-flycheck" "rustic-flycheck.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from rustic-flycheck.el

(autoload 'rustic-flycheck-setup "rustic-flycheck" "\
Setup Rust in Flycheck.
If the current file is part of a Cargo project, configure
Flycheck according to the Cargo project layout.

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-flycheck" '("rustic-flycheck-")))

;;;***

;;;### (autoloads nil "rustic-interaction" "rustic-interaction.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from rustic-interaction.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-interaction" '("rustic-")))

;;;***

;;;### (autoloads nil "rustic-lsp" "rustic-lsp.el" (0 0 0 0))
;;; Generated autoloads from rustic-lsp.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-lsp" '("rust" "lsp-rust-")))

;;;***

;;;### (autoloads nil "rustic-racer" "rustic-racer.el" (0 0 0 0))
;;; Generated autoloads from rustic-racer.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-racer" '("racer-src-button" "rustic-racer-")))

;;;***

;;;### (autoloads nil "rustic-util" "rustic-util.el" (0 0 0 0))
;;; Generated autoloads from rustic-util.el

(autoload 'rustic-format--enable-format-on-save "rustic-util" "\
Enable formatting using rustfmt when saving buffer.

\(fn)" t nil)

(autoload 'rustic-format--disable-format-on-save "rustic-util" "\
Disable formatting using rustfmt when saving buffer.

\(fn)" t nil)

(autoload 'rustic-cargo-fmt "rustic-util" "\
Use rustfmt via cargo.

\(fn)" t nil)

(autoload 'rustic-playpen "rustic-util" "\
Create a shareable URL for the contents of the current region, 
src-block or buffer on the Rust playpen.

\(fn BEGIN END)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "rustic-util" '("rustic-")))

;;;***

;;;### (autoloads nil nil ("rustic-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; rustic-autoloads.el ends here
