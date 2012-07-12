;;; editing.el -- Miscellaneous editing features

;; Enable CUA selection mode; sorry, it stuck.
(cua-selection-mode t)
(setq delete-selection-mode t)

;; Autopair mode
(require 'autopair)
(setq autopair-pair-criteria 'help-balance)
(autopair-global-mode)

;; Keep autopair from interfering with auto-complete
;(setq ac-use-overriding-local-map t)
; TODO: remove this if not using auto-complete

;; mark-multiple <3 @magnars
(require 'mark-more-like-this)
(global-set-key (kbd "C-æ") 'mark-previous-like-this)
(global-set-key (kbd "C-'") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)

;; expand-region <3 @magnars
(require 'expand-region)
(global-set-key (kbd "C-+") 'er/expand-region)

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

