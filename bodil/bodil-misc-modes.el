;;; bodil-misc-modes.el -- Miscellaneous modes.

;; Fish shell
(autoload 'fish-mode "fish-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'interpreter-mode-alist '("fish" . fish-mode))
(add-hook 'fish-mode-hook (lambda () (setq tab-width 2)))

;; Standard ML
(package-require 'sml-mode)

(provide 'bodil-misc-modes)
