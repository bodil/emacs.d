;;; bodil-js.el -- Javascript configuration

;; Setup slime-js
(require 'slime)
(slime-setup '(slime-fancy slime-repl slime-js))
(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))
(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)))

;; Coffeescript mode
(require 'coffee-mode)

;; JSLint for Flymake
(require 'flymake-lintnode)
(add-hook 'js2-mode-hook
          (lambda () flymake-mode 1))
(setq lintnode-location (expand-file-name "~/.emacs.d/site-lisp/lintnode"))
(lintnode-start)

