;;; bodil-cedet.el -- General CEDET init

(load-file "~/.emacs.d/site-lisp/cedet/cedet-devel-load.el")

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
(add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)

;; Enable Semantic
(semantic-mode 1)

;(global-semanticdb-minor-mode 1)


