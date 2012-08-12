;;; codestyle.el -- Indentation styles et al for all modes in one central location

;; Tab indentation is a disease; a cancer of this planet.
(set-default 'indent-tabs-mode nil)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Default indentation
(setq-default tab-width 4)
;; Javascript
(setq-default js2-basic-offset 2)
;; JSON
(setq-default js-indent-level 2)
;; Coffeescript
(setq coffee-tab-width 2)
;; Python
(setq python-indent-offset 2)

;; Default formatting style for C based modes
(setq c-default-style "java")

;; Enforce newline at EOF
(setq require-final-newline 't)

;; I mean, _seriously_...
(setq sentence-end-double-space nil)

;; Enforce proper whitespace
(add-to-list 'load-path (concat dotfiles-dir "site-lisp/ethan-wspace/lisp"))
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)
