;;; bodil-paredit.el --- Paredit setup

;; Paredit for all lisps
(autoload 'paredit-mode "paredit.el" nil t)
(add-lisp-hook (lambda ()
                 (autopair-mode -1)
                 (paredit-mode 1)))

;; Make paredit play nice with eldoc
(eval-after-load "eldoc"
  '(eldoc-add-command
    'paredit-backward-delete
    'paredit-close-round))

;; Inverse M-(
(defun paredit-wrap-round-from-behind ()
  (interactive)
  (forward-sexp -1)
  (paredit-wrap-round)
  (insert " ")
  (forward-char -1))
(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd "M-)")
     'paredit-wrap-round-from-behind))

(provide 'bodil-paredit)
