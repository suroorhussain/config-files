(require 'cedet)
(require 'cedet-files)
(setq semantic-load-turn-useful-things-on t)
(global-ede-mode 1)
(require 'semantic)
;; Activate semantic
(semantic-mode 1)

(require 'semantic/sb)
(require 'srecode)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-scheduler-mode 1)
(global-semantic-idle-completions-mode 1)
(global-semantic-decoration-mode 0)
(global-semantic-highlight-func-mode 0)
(global-semantic-stickyfunc-mode -1)
(global-semantic-idle-summary-mode 1)
(global-semantic-mru-bookmark-mode 1)

;; Try to make completions when not typing
'(semantic-complete-inline-analyzer-displayor-class (quote semantic-displayor-tooltip))
'(semantic-complete-inline-analyzer-idle-displayor-class (quote semantic-displayor-tooltip))
(global-semantic-idle-completions-mode 1)
(global-semantic-idle-scheduler-mode 1) ;The idle scheduler with automatically reparse buffers in idle time.
(global-semantic-idle-summary-mode 1) ;

(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
(semantic-add-system-include "/usr/include/GL" 'c++-mode)
(semantic-add-system-include "/usr/include" 'c++-mode)
(semantic-add-system-include "/usr/include/google" 'c++-mode)
