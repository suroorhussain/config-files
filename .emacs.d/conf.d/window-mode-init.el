
(when window-system
  (when (eq system-type 'darwin)
    (set-face-attribute 'default
                        nil
                        :font "DejaVu Sans Mono"
                        :height 120))
  (global-unset-key "\C-z")
  (set-exec-path-from-shell-PATH)
  (scroll-bar-mode -1)
  (set-fringe-mode 2))
