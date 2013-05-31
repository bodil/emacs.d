;;; bodil-flycheck.el --- Flycheck setup

(package-require 'flycheck)
(add-hook 'find-file-hook
          (lambda ()
            (when (not (equal 'emacs-lisp-mode major-mode))
              (flycheck-mode))))

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

(package-require 'flycheck-color-mode-line)

(eval-after-load "flycheck"
  '(progn
     (setq flycheck-highlighting-mode nil)
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
     (set-face-background 'flycheck-error-face "red")
     (set-face-foreground 'flycheck-error-face "black")
     (set-face-background 'flycheck-warning-face "orange")
     (set-face-foreground 'flycheck-warning-face "black")))

(provide 'bodil-flycheck)
