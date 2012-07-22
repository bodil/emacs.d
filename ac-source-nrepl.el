;;; ac-source-nrepl.el --- Auto-complete source using nrepl.el

(require 'nrepl)
(require 'auto-complete)

(defvar ac-nrepl-status 'idle)
(defvar ac-nrepl-completions nil)

(make-variable-buffer-local 'ac-nrepl-status)
(make-variable-buffer-local 'ac-nrepl-completions)

(defun ac-nrepl-complete-handler (response)
  (nrepl-dbind-response response (value ns out err status id)
    (when value
      (let ((completions (car (read-from-string value))))
        (setq ac-nrepl-status 'available)
        (setq ac-nrepl-completions completions)
        (ac-start :force-init t)
        (ac-update)))))

(defun ac-invoke-nrepl-complete (prefix)
  (let ((form (format "(complete.core/completions \"%s\" *ns*)" prefix)))
    (nrepl-send-string form (nrepl-current-ns)
                       'ac-nrepl-complete-handler)))

(defun ac-nrepl-candidate ()
  (cond
   ((eq ac-nrepl-status 'idle)
    (setq ac-nrepl-status 'waiting)
    (ac-invoke-nrepl-complete ac-prefix)
    nil)

   ((eq ac-nrepl-status 'waiting)
    nil)

   ((eq ac-nrepl-status 'available)
    (setq ac-nrepl-status 'idle)
    ac-nrepl-completions)))

(defun ac-nrepl-available ()
  (bufferp (get-buffer "*nrepl-connection*")))

(ac-define-source nrepl
  '((candidates . ac-nrepl-candidate)
    (available . ac-nrepl-available)))

(provide 'ac-source-nrepl)
;;; ac-source-nrepl.el ends here
