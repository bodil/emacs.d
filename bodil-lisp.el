;;; bodil-lisp.el -- Lisp specific configuration

;; SLIME
(require 'slime-autoloads)
(slime-setup '(slime-repl))

;; Make paredit play nice with eldoc
(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)

;; And make paredit play nice with SLIME:
;; Stop SLIME's REPL from grabbing DEL,
;; which is annoying when backspacing over a '('
(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

;; Hook SLIME into auto-complete
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)

;; Load clojure-mode
(require 'clojure-mode)

;; Define available lisps, in ASCENDING order of priority
(setq slime-lisp-implementations '())
;; Clojure: install if Leiningen is on the path
(let ((lein-path (executable-find "lein")))
  (if (and (not (null lein-path)) (file-executable-p lein-path))
      (add-to-list 'slime-lisp-implementations
                   (list 'clojure (list lein-path "repl")))))
;; SBCL: install if SBCL is on the path and slime-helper is installed
(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el"))
      (sbcl-path (executable-find "sbcl")))
  (if (and (file-readable-p slime-helper)
           (not (null sbcl-path))
           (file-executable-p sbcl-path))
      (progn
        (load slime-helper)
        (add-to-list 'slime-lisp-implementations
                     (list 'sbcl (list sbcl-path))))))

