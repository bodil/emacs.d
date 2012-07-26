;;; complete.el -- Auto completion

(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories (concat dotfiles-dir "site-lisp/auto-complete/dict"))
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "C-\\") 'auto-complete)
(define-key ac-completing-map (kbd "TAB") nil)
(define-key ac-completing-map (kbd "RET") 'ac-complete)
(define-key ac-completing-map (kbd "C-\\") 'ac-complete)
;; TODO: I want PgUp/PgDn to work when AC is showing a candidate menu

;; Enable auto-complete for some modes not present by default in ac-modes
(setq ac-modes (append '(coffee-mode lolisp-mode) ac-modes))

;; Yasnippets, always
(eval-after-load "yasnippet"
  '(setq-default ac-sources (append '(ac-source-yasnippet) ac-sources)))


;;; Clojure

(eval-after-load "clojure-mode"
  '(add-hook 'clojure-mode-hook
             (lambda ()
               (require 'ac-source-nrepl)
               (setq ac-sources (append '(ac-source-nrepl) ac-sources)))))
