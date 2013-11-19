;;; bodil-helm.el --- Helm will steer you in the right direction. Duh.

(package-require 'helm)
(global-set-key (kbd "<print>") 'helm-mini)

;; Projectile support
(package-require 'helm-projectile)
(global-set-key (kbd "C-c <print>") 'helm-projectile)

;; Backup
(package-require 'helm-backup)
(add-hook 'after-save-hook 'helm-backup-versioning)
(global-set-key (kbd "C-c b") 'helm-backup)
(setq helm-backup-path "~/.emacs.d/.helm-backup")

(provide 'bodil-helm)
