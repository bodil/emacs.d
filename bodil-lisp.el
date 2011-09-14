;;; bodil-lisp.el -- Lisp specific configuration

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

;; Which lisp are we?
(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el"))
      (lein-path (executable-find "lein")))
  (cond
   ;; If quicklisp contains a slime-helper installation, go SBCL
   ((file-exists-p slime-helper)
    (load slime-helper)
    (setq inferior-lisp-program "sbcl"))
   ;; Else, if Leiningen is on the user's path, go Clojure
   ((and (not (null lein-path)) (file-executable-p lein-path))
    (setq inferior-lisp-program "lein repl"))))

