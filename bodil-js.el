;;; bodil-js.el -- Javascript configuration

;; Configure js2-mode
(require 'js2-mode)
(setq js2-bounce-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

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
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (define-key coffee-mode-map (kbd "M-r") 'coffee-compile-buffer)
  (define-key coffee-mode-map (kbd "M-R") 'coffee-compile-region))
(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;; JSLint for Flymake
(require 'flymake-lintnode)
(add-hook 'js2-mode-hook
          (lambda () flymake-mode 1))
(setq lintnode-location (expand-file-name "~/.emacs.d/site-lisp/lintnode"))
(lintnode-start)

