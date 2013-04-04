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

(provide 'bodil-haskell)
