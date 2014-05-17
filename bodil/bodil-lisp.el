;;; bodil-lisp.el -- Lisps

(require 'bodil-defuns)

(setq bodil-lisp-modes
      '(scheme-mode emacs-lisp-mode lisp-mode clojure-mode
                    lolisp-mode shen-mode bodol-mode))

(defun add-lisp-hook (func)
  (add-hooks bodil-lisp-modes func))

;; Setup C-c v to eval whole buffer in all lisps
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

;; Highlight sexp under cursor
(package-require 'highlight-parentheses)
(add-lisp-hook 'highlight-parentheses-mode)

;; Lambdas
(defun lambda-as-lambda (mode pattern)
  (font-lock-add-keywords
   mode `((,pattern
           (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                     "λ" 'decompose-region)))))))

;;; Emacs Lisp

(lambda-as-lambda 'emacs-lisp-mode "(\\(\\<lambda\\>\\)")

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

;;; Clojure

(package-require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.cljs?$" . clojure-mode))

(lambda-as-lambda 'clojure-mode "(\\(\\<fn\\>\\)")

;; nRepl
(package-require 'cider)
(eval-after-load "clojure-mode" '(require 'cider))
(setq nrepl-lein-command "lein")
(setq nrepl-server-command "echo \"lein repl :headless\" | $SHELL -l")

;; Run tests in nRepl
(defun nrepl-run-tests (ns)
  (interactive (list (nrepl-current-ns)))
  (save-buffer)
  (nrepl-load-current-buffer)
  (with-current-buffer "*nrepl*"
    (nrepl-send-string
     (format "(clojure.test/run-tests '%s)" ns)
     nrepl-buffer-ns (nrepl-handler (current-buffer)))))
(eval-after-load "clojure-mode"
  '(define-key clojure-mode-map (kbd "C-c C-,") 'nrepl-run-tests))

;;Kibit
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

;; Cljsbuild
(package-require 'cljsbuild-mode)


;;; Lolisp

(define-derived-mode lolisp-mode scheme-mode "Lolisp")
(define-key lolisp-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
(add-to-list 'auto-mode-alist '("\\.loli$" . lolisp-mode))

(lambda-as-lambda 'lolisp-mode "(\\(\\<lambda\\>\\)")


;;; Shen

(package-require 'shen-mode)
(add-to-list 'auto-mode-alist '("\\.shen$" . shen-mode))
(eval-after-load "shen-mode"
  '(progn
     (define-key shen-mode-map (kbd "C-x C-e") 'lisp-eval-last-sexp)
     (define-key shen-mode-map (kbd "C-c C-k")
       (lambda ()
         (interactive)
         (lisp-eval-string (buffer-string))))))
(lambda-as-lambda 'shen-mode "(\\(\\</\\.\\>\\)")


;; BODOL

(require 'bodol-mode)


;; LispyScript

(add-to-list 'auto-mode-alist '("\\.ls$" . clojure-mode))


;;; Various inferior lisps

;; Clojure REPL
(defun clojure-repl ()
  (interactive)
  (run-lisp "lein repl"))

;; ClojureScript REPL
(defun clojurescript-repl ()
  (interactive)
  (run-lisp "lein trampoline noderepl"))

;; ClojureScript REPL
(defun clojurescript-rhino-repl ()
  (interactive)
  (run-lisp "lein trampoline cljsbuild repl-rhino"))

;; Clojure-Py REPL
(defun clojurepy-repl ()
  (interactive)
  (run-lisp "/usr/local/bin/clojurepy"))

;; Lolisp REPL
(defun lolisp-repl ()
  (interactive)
  (run-lisp "/home/bodil/workspace/lolisp/lolisp.py"))

;; Shen REPL
(defun shen-repl ()
  (interactive)
  (run-lisp "shen"))

;; BODOL REPL
(defun bodol-repl ()
  (interactive)
  (run-lisp "/home/bodil/workspace/BODOL/repl"))

;; Switch a Clojure nrepl to ClojureScript

(defun nrepl-start-noderepl ()
  (interactive)
  (save-excursion
    (nrepl-switch-to-repl-buffer nil)
    (insert "(require 'cljs.repl.node) (cljs.repl.node/run-node-nrepl)")
    (nrepl-send-input)))

(provide 'bodil-lisp)
