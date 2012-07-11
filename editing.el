;;; editing.el -- Miscellaneous editing features

;; Tab indentation is a disease; a cancer of this planet.
(set-default 'indent-tabs-mode nil)

;; Enable CUA selection mode; sorry, it stuck.
(cua-selection-mode t)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; Autopair mode
(require 'autopair)
(setq autopair-pair-criteria 'help-balance)
(autopair-global-mode)

;; Keep autopair from interfering with auto-complete
;(setq ac-use-overriding-local-map t)
; TODO: remove this if not using auto-complete

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

;; Duplicate start of line or region, from http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))
(defun duplicate-start-of-line ()
  (if (bolp)
      (progn
        (end-of-line)
        (duplicate-start-of-line)
        (beginning-of-line))
    (let ((text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
      (forward-line)
      (push-mark)
      (insert text)
      (open-line 1))))
(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning) end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))
(global-set-key (kbd "C-M-<down>") 'duplicate-start-of-line-or-region)

