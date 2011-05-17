;;; bodil-haskell.el -- Haskell things, for great justice.

;; Install haskell-mode
(load (expand-file-name "~/.emacs.d/site-lisp/haskell-mode/haskell-site-file"))

;; Configure haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Make each piece of a camel case word count as a single word when
;; using commands operating on words.
(add-hook 'haskell-mode-hook '(lambda () (capitalized-words-mode t)))


