;;; bodil-flycheck.el --- Flycheck setup

(package-require 'flycheck)
(add-hook 'find-file-hook 'flycheck-mode)

(provide 'bodil-flycheck)
