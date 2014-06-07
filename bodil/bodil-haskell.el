;;; bodil-haskell.el --- FIRE ALL MONAD TRANSFORMERS

(package-require 'haskell-mode)
(package-require 'ghc)
(package-require 'shm)

;; Setup haskell-mode hooks
(eval-after-load "haskell-mode"
  '(custom-set-variables
    '(haskell-mode-hook
      '(turn-on-haskell-indentation
        turn-on-haskell-doc-mode
        ghc-init
        structured-haskell-mode
        (lambda () (add-to-list 'ac-sources 'ac-source-ghc))))))

;; Use Unicode arrows in place of ugly ASCII arrows
(require 'bodil-defuns)
(font-lock-replace-symbol 'haskell-mode "\\(->\\)" "→")
(font-lock-replace-symbol 'haskell-mode "\\(<-\\)" "←")
(font-lock-replace-symbol 'haskell-mode "\\(=>\\)" "⇒")

(define-key haskell-mode-map (kbd "→") (lambda () (interactive) (insert "->")))
(define-key haskell-mode-map (kbd "←") (lambda () (interactive) (insert "<-")))
(define-key haskell-mode-map (kbd "⇒") (lambda () (interactive) (insert "=>")))

;; Add a keybinding for (inferior-haskell-type t) to insert
;; inferred type signature for function at point
(define-key haskell-mode-map (kbd "C-c C-s")
  (lambda () (interactive)
    (let ((sym (haskell-ident-at-point)))
      (inferior-haskell-type sym t))))

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


;;; Idris (for want of a better place to put it)
(package-require 'idris-mode)
(require 'idris-mode)
(add-to-list 'auto-mode-alist '("\\.idr$" . idris-mode))

(font-lock-replace-symbol 'idris-mode "\\(->\\)" "→")
(font-lock-replace-symbol 'idris-mode "\\(<-\\)" "←")
(font-lock-replace-symbol 'idris-mode "\\(=>\\)" "⇒")

(define-key idris-mode-map (kbd "→") (lambda () (interactive) (insert "->")))
(define-key idris-mode-map (kbd "←") (lambda () (interactive) (insert "<-")))
(define-key idris-mode-map (kbd "⇒") (lambda () (interactive) (insert "=>")))


;;; PureScript cheat mode
(define-derived-mode purescript-mode haskell-mode "PureScript"
  "Major mode for PureScript")
(add-to-list 'auto-mode-alist (cons "\\.purs\\'" 'purescript-mode))


(provide 'bodil-haskell)
