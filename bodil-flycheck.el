;;; bodil-flycheck.el --- Flycheck setup

(autoload 'flycheck-mode "flycheck" nil t)
(add-hook 'find-file-hook 'flycheck-mode)

(provide 'bodil-flycheck)
