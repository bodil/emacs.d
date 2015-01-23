;;; bodil-flycheck.el --- Flycheck setup

(package-require 'flycheck)
(add-hook 'find-file-hook
          (lambda ()
            (when (not (equal 'emacs-lisp-mode major-mode))
              (flycheck-mode))))

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)

(global-set-key
 (kbd "C-c c")
 (lambda () (interactive)
   (if flycheck-checker
       (progn
         (save-buffer)
         (flycheck-compile flycheck-checker)))
   (message
    "No checker selected for this buffer. Try M-x flycheck-select-checker")))

(package-require 'flycheck-color-mode-line)
(package-require 'flycheck-pos-tip)

(eval-after-load "flycheck"
  '(progn
     (setq flycheck-highlighting-mode 'symbols)
     (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
     (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)
     (set-face-background 'flycheck-error "#660000")
     (set-face-foreground 'flycheck-error nil)
     (set-face-background 'flycheck-warning "#775500")
     (set-face-foreground 'flycheck-warning nil)
     (require 'flycheck-color-mode-line)
     (set-face-background 'flycheck-color-mode-line-error-face "#660000")
     (set-face-background 'flycheck-color-mode-line-warning-face "#553300")
     (set-face-background 'flycheck-color-mode-line-info-face nil)
     (set-face-foreground 'flycheck-color-mode-line-error-face nil)
     (set-face-foreground 'flycheck-color-mode-line-warning-face nil)
     (set-face-foreground 'flycheck-color-mode-line-info-face nil)))

;;; Haskell specific

(package-require 'flycheck-haskell)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(provide 'bodil-flycheck)
