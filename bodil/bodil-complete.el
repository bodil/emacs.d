;;; complete.el -- Auto completion

(package-require 'company)
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key (kbd "C-\\") 'company-complete)

(setq company-global-modes '(not term-mode))

(setq company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-show-numbers t
      company-tooltip-align-annotations t
      company-require-match nil)

;;; Javascript

(package-require 'company-tern)
(add-to-list 'company-backends 'company-tern)

(provide 'bodil-complete)
