;;; bodil-terminal.el -- Terminals

;; Configure multi-term
(autoload 'multi-term "multi-term" nil t)
(autoload 'multi-term-next "multi-term" nil t)
(global-set-key (kbd "C-x C-m") 'multi-term)
(global-set-key (kbd "C-x m") 'multi-term-next)

;; Don't try to enable autopair in term-mode, it remaps the return key!
(add-hook 'term-mode-hook
          (lambda ()
            (autopair-mode 0)
            (hl-line-mode 0)))

;; Likewise, yasnippet breaks the tab key.
(add-hook 'term-mode-hook
          (lambda() (yas-minor-mode -1)))

(provide 'bodil-terminal)
