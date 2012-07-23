;;; lisp.el -- Lisps

;; Paredit for all lisps
(autoload 'paredit-mode "paredit.el" nil t)
(dolist (mode '(scheme emacs-lisp lisp clojure))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            (lambda ()
              (autopair-mode -1)
              (paredit-mode 1))))

;; Setup C-c v to eval whole buffer in all lisps
(define-key lisp-mode-shared-map (kbd "C-c v") 'eval-buffer)

;;; Emacs Lisp

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

(autoload 'clojure-mode "clojure-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.cljs?$" . clojure-mode))

;; nRepl
(autoload 'nrepl-jack-in "clojure-mode" nil t)
(eval-after-load "clojure-mode" '(require 'nrepl))
(setq nrepl-lein-command "lein")
(setq nrepl-server-command "echo \"lein repl :headless\" | $SHELL -l")
