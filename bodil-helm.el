;;; bodil-helm.el --- Helm will steer you in the right direction. Duh.

(package-require 'helm)
(global-set-key (kbd "<print>") 'helm-mini)

;; Projectile support
(package-require 'helm-projectile)
(global-set-key (kbd "C-c <print>") 'helm-projectile)

(provide 'bodil-helm)
