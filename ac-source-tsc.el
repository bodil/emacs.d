;;; -*- lexical-binding: t -*-
;;; ac-source-tsc.el --- Auto-complete source using tsc-completion

(package-require 'json)
(package-require 'dash)

(require 'auto-complete)
(require 'json)
(require 'dash)

(defvar ac-tsc-completions nil)
(defvar ac-tsc-complete-request-point 0)

(defun json-parse (s)
  (let ((json-object-type 'plist))
    (json-read-from-string s)))

(defun tsc-ac-complete ()
  (interactive)
  (let ((cur-ac-sources ac-sources))
    (ac-invoke-tsc-complete
     (buffer-file-name) (point)
     (lambda ()
       (auto-complete (cons 'ac-source-tsc cur-ac-sources))))))

(defun tsc-ac-dot-complete ()
  "Insert dot and complete code at point."
  (interactive)
  (insert ".")
  (let ((cur-ac-sources ac-sources))
    (ac-invoke-tsc-complete
     (buffer-file-name) (point)
     (lambda ()
       (auto-complete (cons 'ac-source-tsc cur-ac-sources))))))

(defun tsc-execute (filename pos callback)
  (when (get-buffer "*tsc-completion*")
    (kill-buffer "*tsc-completion*"))
  (let ((proc
         (start-process "tsc-completion" "*tsc-completion*"
                        "tsc-completion"
                        filename
                        (prin1-to-string (- pos 1))
                        "true")))
    (set-process-sentinel
     proc
     (lambda (proc event)
       (when (string= "finished\n" event)
         (let ((result (with-current-buffer (process-buffer proc)
                         (json-parse (buffer-string)))))
           (funcall callback result)))))))

(defun ac-invoke-tsc-complete (fn pos callback)
  (save-buffer)
  (setq ac-tsc-complete-request-point pos)
  (setq ac-tsc-completions nil)
  (tsc-execute
   fn pos
   (lambda (result)
     (let ((entries (plist-get result :entries)))
       (setq ac-tsc-completions
             (-map (lambda (x) (plist-get x :name)) entries))
       (funcall callback)))))

(defun ac-tsc-candidate ()
  ac-tsc-completions)

(defun tsc-ac-completion-prefix ()
  (or (ac-prefix-default)
      (when (= ac-tsc-complete-request-point (point))
        ac-tsc-complete-request-point)))

(ac-define-source tsc
  '((candidates . ac-tsc-candidate)
    (prefix . tsc-ac-completion-prefix)
    (requires . -1)))

(defun tsc-ac-setup ()
  (interactive)
  (define-key typescript-mode-map "." 'tsc-ac-dot-complete))

(provide 'ac-source-tsc)
;;; ac-source-tsc.el ends here
