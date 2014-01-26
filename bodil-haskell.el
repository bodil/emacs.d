;;; bodil-haskell.el --- ENGAGE MONAD TRANSFORMERS

(package-require 'haskell-mode)
(package-require 'ghc)

;; auto-complete source using ghc-doc
(defun ac-haskell-candidates ()
  (let ((pattern (buffer-substring (ghc-completion-start-point) (point)))
        (symbols (ghc-select-completion-symbol)))
    (all-completions pattern symbols)))

;; Setup auto-complete for haskell-mode
(eval-after-load "auto-complete"
  '(progn
     (add-to-list 'ac-modes 'haskell-mode)
     (ac-define-source ghc
       '((candidates . ac-haskell-candidates)))))

;; Setup haskell-mode hooks
(eval-after-load "haskell-mode"
  '(custom-set-variables
    '(haskell-mode-hook
      '(turn-on-haskell-indentation
        ghc-init
        (lambda () (add-to-list 'ac-sources 'ac-source-ghc))))))

;; Use Unicode arrows in place of ugly ASCII arrows
(define-key haskell-mode-map (kbd "→") (lambda () (interactive) (insert "->")))
(define-key haskell-mode-map (kbd "←") (lambda () (interactive) (insert "<-")))
(font-lock-add-keywords
 'haskell-mode `((,"\\(->\\)"
                  (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                           "→" 'decompose-region))))))
(font-lock-add-keywords
 'haskell-mode `((,"\\(<-\\)"
                  (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                           "←" 'decompose-region))))))

;; Add a keybinding for (inferior-haskell-type t) to insert
;; inferred type signature for function at point
(define-key haskell-mode-map (kbd "C-c C-s")
  (lambda () (interactive)
    (let ((sym (haskell-ident-at-point)))
      (inferior-haskell-type sym t))))

;;; Idris (for want of a better place to put it)
(package-require 'idris-mode)
(add-to-list 'auto-mode-alist '("\\.idr$" . idris-mode))


(provide 'bodil-haskell)
