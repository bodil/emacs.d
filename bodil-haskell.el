;;; bodil-haskell.el -- Haskell things, for great justice.

;; Install haskell-mode
;(load (expand-file-name "~/.emacs.d/site-lisp/haskell-mode/haskell-site-file"))

;; Configure haskell-mode
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; Make each piece of a camel case word count as a single word when
;; using commands operating on words.
(add-hook 'haskell-mode-hook '(lambda () (capitalized-words-mode t)))

;; Load Scion if installed.
;; (Install it from https://github.com/hvr/scion)
(defvar scion-path (expand-file-name "~/.cabal/share/scion-0.2.0.2/emacs"))
(when (file-accessible-directory-p scion-path)
  (add-to-list 'load-path scion-path)
  (require 'scion)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (scion-mode 1)
              (scion-flycheck-on-save 1)))
  (setq scion-completing-read-function 'ido-completing-read))

