;;; bodil-python.el -- Python configuration

;; We want python-mode, not python.el
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(require 'ipython)

;; Pylint
(defun pylint-buffer-change-colorizing (start end length)
  "colorizing the region from start to end."
  (ansi-color-apply-on-region start end))

(defun pylint-check-current-buffer ()
  "Run pylint on current buffer."
  (interactive)
  (let (cbuffer-file pylint-buffer commandp)
    (setq cbuffer-file (buffer-file-name))
    (if (stringp cbuffer-file)
        (progn
          (setq pylint-buffer (get-buffer-create "*Pylint*"))
          (ansi-color-for-comint-mode-on)
          (switch-to-buffer pylint-buffer)
          (add-hook 'after-change-functions 'pylint-buffer-change-colorizing t t)
          (setq fullcommand (concat "pylint -f colorized " cbuffer-file))
          (setq commandp (start-process-shell-command "pylint" pylint-buffer fullcommand)))
      (message "This buffer did not visit any file."))))

