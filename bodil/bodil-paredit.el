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

;; Duplicate sexp
(defun paredit-duplicate-after-point
  ()
  "Duplicates the content of the line that is after the point."
  (interactive)
  ;; skips to the next sexp
  (while (looking-at " ")
    (forward-char))
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (set-mark-command nil)
  (yank)
  (exchange-point-and-mark))

(eval-after-load "paredit"
  '(define-key paredit-mode-map (kbd "C-c C-d")
     'paredit-duplicate-after-point))

(provide 'bodil-paredit)
