;;; ac-source-tern.el --- Auto-complete source using tern-mode

(require 'auto-complete)

(defvar ac-tern-completions nil)
(make-variable-buffer-local 'ac-tern-completions)

(defun tern-auto-complete ()
  "Complete code at point."
  (interactive)
  (tern-run-query
   (lambda (result)
     (setq ac-tern-completions
           (sort (append (cdr (assoc 'completions result)) ()) 'string-lessp))
     (auto-complete))
   "completions" (point)))

(defun ac-tern-candidate ()
  ac-tern-completions)

(defun ac-tern-prefix ()
  (or (ac-prefix-symbol)
      (let ((c (char-before)))
        (when (eq ?\. c)
          (point)))))

(ac-define-source tern
  '((candidates . ac-tern-candidate)
    (requires . 0)
    (prefix . ac-tern-prefix)))

(provide 'ac-source-tern)
;;; ac-source-tern.el ends here
