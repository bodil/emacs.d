;;; bodil-helm.el --- Helm will steer you in the right direction. Duh.

(package-require 'helm)
(global-set-key (kbd "<print>") 'helm-mini)

(provide 'bodil-helm)
