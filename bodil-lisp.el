;;; bodil-lisp.el -- Lisp specific configuration

;; SLIME
(require 'slime)
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

;; Enable auto-complete in lisp mode
(require 'auto-complete)
(add-to-list 'ac-modes 'lisp-mode)

;; Load clojure-mode
(require 'clojure-mode)
(add-hook 'clojure-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-yasnippet)
            (clojure-test-mode 1)
            (setq buffer-save-without-query t)))

;; Clojurecharge slime-selector
(define-key global-map (kbd "C-<home>") 'slime-selector)
(def-slime-selector-method ?j
  "most recently visited clojure-mode buffer."
  (slime-recently-visited-buffer 'clojure-mode))

;; Hijack runlol test reporting
(defun clojure-test-echo-results ()
  (runlol-stop-timer)
  (let ((failing clojure-test-failure-count))
    (let ((modeline-text (if (= failing 0) " TESTS OK " (format " FAIL:% 3d " failing))))
      (setq global-mode-string
            (propertize modeline-text 'face
                        (if (= failing 0) 'runlol-green-face 'runlol-red-face)))))
  (message
   (propertize
    (format "Ran %s tests. %s failures, %s errors."
            clojure-test-count clojure-test-failure-count
            clojure-test-error-count)
    'face
    (cond ((not (= clojure-test-error-count 0)) 'clojure-test-error-face)
          ((not (= clojure-test-failure-count 0)) 'clojure-test-failure-face)
          (t 'clojure-test-success-face)))))

;; highlight-parentheses-mode
(require 'highlight-parentheses)
(add-hook 'highlight-parentheses-mode-hook
          '(lambda () (setq autopair-handle-action-fns
                       (append (or autopair-handle-action-fns '(autopair-default-handle-action))
                               '((lambda (action pair pos-before) (hl-paren-color-update)))))))
(add-hook 'lisp-mode-hook (lambda () (highlight-parentheses-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (highlight-parentheses-mode 1)))

;; erefactor
(require 'erefactor)
(add-hook 'emacs-lisp-mode-hook
          (lambda () (define-key emacs-lisp-mode-map (kbd "C-c C-v") erefactor-map)))
(add-hook 'emacs-lisp-mode-hook 'erefactor-lazy-highlight-turn-on)
(add-hook 'lisp-interaction-mode-hook 'erefactor-lazy-highlight-turn-on)

;; Setup elisp-slime-nav so M-. and M-, work as well in elisp as they
;; do in SLIME
(require 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))

