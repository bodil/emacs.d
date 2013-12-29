;;; bodil-misc-modes.el -- Miscellaneous modes.

;; Fish shell
(autoload 'fish-mode "fish-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'interpreter-mode-alist '("fish" . fish-mode))
(add-hook 'fish-mode-hook (lambda () (setq tab-width 2)))

;; Scala
(package-require 'scala-mode2)
(package-require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Standard ML
(package-require 'sml-mode)

(provide 'bodil-misc-modes)
