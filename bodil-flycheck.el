;;; bodil-flycheck.el --- Flycheck setup

(package-require 'flycheck)
(add-hook 'find-file-hook
          (lambda ()
            (when (not (equal 'emacs-lisp-mode major-mode))
              (flycheck-mode))))

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

(eval-after-load "flycheck"
  '(progn
     (setq flycheck-highlighting-mode nil)
     (set-face-background 'flycheck-error-face "red")
     (set-face-foreground 'flycheck-error-face "black")
     (set-face-background 'flycheck-warning-face "orange")
     (set-face-foreground 'flycheck-warning-face "black")))

(provide 'bodil-flycheck)
