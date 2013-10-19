;;; bodil-nav.el -- Navigation things.

;; ace-jump-mode!
(package-require 'ace-jump-mode)
(global-set-key (kbd "C-ø") 'ace-jump-mode)
(global-set-key (kbd "M-ø") 'ace-jump-mode-pop-mark)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

;; Smart home key
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)

;; Subword mode (consider CamelCase chunks as words)
(global-subword-mode 1)

;; anzu: display incremental search stats in modeline
(package-require 'anzu)
(global-anzu-mode 1)

(provide 'bodil-nav)
