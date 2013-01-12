;;; markup.el -- HTML and friends

;; Useful major modes
(package-require 'less-css-mode)

;; nXhtml
(autoload 'nxhtml-mumamo-mode "autostart" nil t)
(add-to-list 'auto-mode-alist '("\\.\\(html\\|ejs\\|jsp\\)$" . nxhtml-mumamo-mode))
(eval-after-load "nxhtml-mode"
  '(setq mumamo-chunk-coloring 1
     rng-nxml-auto-validate-flag nil
     nxhtml-skip-welcome t))

;; Patch a mumamo bug which keeps giving annoying warnings
(eval-after-load "mumamo"
  '(setq mumamo-per-buffer-local-vars (delq 'buffer-file-name mumamo-per-buffer-local-vars)))

;; Key for renaming tags
(eval-after-load "nxhtml-mode"
  '(define-key nxhtml-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair))

(provide 'bodil-markup)
