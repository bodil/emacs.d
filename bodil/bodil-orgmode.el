;;; bodil-orgmode.el --- org-mode settings

;; Stop org-mode from highjacking shift-cursor keys
(setq org-replace-disputed-keys t)

;; Always use visual-line-mode in org-mode
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)
            (set-visual-wrap-column 80)))

(provide 'bodil-orgmode)
