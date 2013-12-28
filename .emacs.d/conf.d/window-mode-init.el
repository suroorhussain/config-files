
(when window-system
  (when (eq system-type 'darwin)
    (set-face-attribute 'default
                        nil
                        :font "DejaVu Sans Mono"
                        :height 120))
  (global-unset-key "\C-z")
  (set-exec-path-from-shell-PATH)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (tooltip-mode -1)
  (set-fringe-mode 2)
  (auto-insert-mode 1)
  (require 'uniquify)
  (defvar uniquify-buffer-name-style 'reverse)
  (require 'midnight)
  (midnight-delay-set 'midnight-delay "4:30am")

  (require 'show-wspace))
