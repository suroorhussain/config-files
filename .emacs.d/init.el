(profiler-start 'cpu)
(setq gc-cons-threshold 20000000)
(require 'package)
(package-initialize)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'auto-compile)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

(add-to-list 'load-path "~/.emacs.d/libs")
(load "start")

(profiler-report)
