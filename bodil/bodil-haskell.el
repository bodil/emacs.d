;;; bodil-haskell.el --- FIRE ALL MONAD TRANSFORMERS

(package-require 'haskell-mode)
(package-require 'ghc)
(package-require 'shm)

;; Setup haskell-mode hooks
(eval-after-load "haskell-mode"
  '(custom-set-variables
    '(haskell-mode-hook
      '(turn-on-haskell-indentation
        turn-on-haskell-doc
        ;; structured-haskell-mode
        ))))

;; Use Unicode arrows in place of ugly ASCII arrows
(require 'bodil-defuns)
(defun setup-haskell-arrows (mode mode-map)
  (font-lock-replace-symbol mode "\\(->\\)" "→")
  (font-lock-replace-symbol mode "\\(<-\\)" "←")
  (font-lock-replace-symbol mode "\\(=>\\)" "⇒")

  (define-key mode-map (kbd "→") (lambda () (interactive) (insert "->")))
  (define-key mode-map (kbd "←") (lambda () (interactive) (insert "<-")))
  (define-key mode-map (kbd "⇒") (lambda () (interactive) (insert "=>"))))
(eval-after-load "haskell-mode"
  '(setup-haskell-arrows 'haskell-mode haskell-mode-map))

;; Setup haskell-interactive-mode
(eval-after-load "haskell-mode"
  '(progn
    (define-key haskell-mode-map (kbd "C-x C-d") nil)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
    (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-s") (lambda () (interactive) (haskell-process-do-type t)))
    (define-key haskell-mode-map (kbd "C-c M-.") nil)
    (define-key haskell-mode-map (kbd "C-c C-d") nil)))

;; Put ghc-show-info in a popup
(package-require 'popup)
(defun ghc-show-info-popup ()
  (interactive)
  (popup-tip (ghc-get-info (ghc-things-at-point))
             :around t :scroll-bar t))
(define-key haskell-mode-map (kbd "C-c TAB") 'ghc-show-info-popup)
(define-key haskell-mode-map (kbd "C-c C-i") 'ghc-show-info-popup)
(define-key haskell-mode-map (kbd "C-c C-S-i") 'ghc-show-info)

;; Use standard keybinding for inferior-haskell-find-definition
(define-key haskell-mode-map (kbd "M-.")
  (lambda () (interactive)
    (inferior-haskell-find-definition (haskell-ident-at-point))))

;; Run test suite
(defun haskell-mode-run-test-suite ()
  (interactive)
  (require 'compile)
  (compile (concat "cd " (projectile-project-root) "; cabal test")))
(define-key haskell-mode-map (kbd "C-c C-,") 'haskell-mode-run-test-suite)

;; Flycheck addons
(package-require 'flycheck-haskell)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;; Idris (for want of a better place to put it)
;; (package-require 'idris-mode)
;; (require 'idris-mode)
;; (add-to-list 'auto-mode-alist '("\\.idr$" . idris-mode))
;; (setup-haskell-arrows 'idris-mode idris-mode-map)

(provide 'bodil-haskell)
