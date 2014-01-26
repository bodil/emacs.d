;;; bodil-smartparens.el

(require 'bodil-lisp)

(package-require 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)
(add-lisp-hook 'smartparens-strict-mode)

(show-smartparens-global-mode t)

(setq-default sp-hybrid-kill-entire-symbol nil)

;; I hate sp-highlight-pair-overlay so much
(setq sp-highlight-pair-overlay nil)

(define-key sp-keymap (kbd "<delete>") 'sp-delete-char)
(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-S-<right>") 'sp-backward-barf-sexp)
(define-key sp-keymap (kbd "C-S-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "M-s") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-S-s") 'sp-raise-sexp)
(define-key sp-keymap (kbd "M-p") 'sp-split-sexp)
(define-key sp-keymap (kbd "M-S-p") 'sp-join-sexp)
(define-key sp-keymap (kbd "M-t") 'sp-transpose-sexp)
(define-key sp-keymap (kbd "M-S-<left>") 'sp-backward-sexp)
(define-key sp-keymap (kbd "M-S-<right>") 'sp-forward-sexp)

(sp-with-modes bodil-lisp-modes
  (sp-local-pair "(" nil :bind "M-(")
  (sp-local-pair "[" nil :bind "M-[")
  (sp-local-pair "{" nil :bind "M-{")
  (sp-local-pair "\"" nil :bind "M-\""))

;; Add newline and position cursor appropriately when starting a
;; curly brace block in C like modes

(defun sp--my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent. "
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(sp-with-modes '(c-mode c++-mode js-mode js2-mode java-mode
                        typescript-mode perl-mode)
  (sp-local-pair "{" nil :post-handlers
                 '((sp--my-create-newline-and-enter-sexp "RET"))))

;; Insert space after pair if immediately followed by a word in Lisp modes

(defun sp--my-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (length (plist-get (sp-get-pair id) :close)))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(sp-with-modes bodil-lisp-modes
  (sp-local-pair "(" nil :post-handlers
                 '(:add sp--my-add-space-after-sexp-insertion)))


(provide 'bodil-smartparens)
