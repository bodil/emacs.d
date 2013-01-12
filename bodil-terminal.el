;;; bodil-terminal.el -- Terminals

;; Configure multi-term
(package-require 'multi-term)
(global-set-key (kbd "C-x C-m") 'multi-term)
(global-set-key (kbd "C-x m") 'multi-term-next)

;; Don't try to enable autopair in term-mode, it remaps the return key!
(add-hook 'term-mode-hook (lambda () (autopair-mode 0)))

;; Likewise, yasnippet breaks the tab key.
(add-hook 'term-mode-hook
          (lambda() (yas-minor-mode -1)))

(provide 'bodil-terminal)
