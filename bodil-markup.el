;;; markup.el -- HTML and friends

;; Useful major modes
(package-require 'less-css-mode)

;; Colourise colour names in certain modes
(package-require 'rainbow-mode)
(dolist (mode '(css-mode less-css-mode html-mode nxhtml-mode nxhtml-mumamo-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            (lambda () (rainbow-mode))))

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

;; Some paredit for HTML
(package-require 'tagedit)
(define-key nxhtml-mode-map (kbd "C-<right>") 'tagedit-forward-slurp-tag)
(define-key nxhtml-mode-map (kbd "C-<left>") 'tagedit-forward-barf-tag)
(define-key nxhtml-mode-map (kbd "M-k") 'tagedit-kill-attribute)

;; Key for renaming tags
(eval-after-load "nxhtml-mode"
  '(define-key nxhtml-mode-map (kbd "C-c C-r") 'mc/mark-sgml-tag-pair))

;; Launch a static web server in the current project root
(require 'bodil-magit)
(defun http-server-in-project (port)
  (interactive "nPort: ")
  (let ((default-directory (git-current-root)))
    (start-process "SimpleHTTPServer" "*SimpleHTTPServer*"
                   "python" "-m" "SimpleHTTPServer" (number-to-string port))))

(provide 'bodil-markup)
