;;; editing.el -- Miscellaneous editing features

;; Enable CUA selection mode; sorry, it stuck.
(cua-selection-mode t)
(setq delete-selection-mode t)

;; Undo to C-z like a muggle; Android kbd doesn't do C-_
(global-set-key (kbd "C-z") 'undo)

;; Autopair mode
(package-require 'autopair)
(eval-after-load "autopair"
  '(setq autopair-pair-criteria 'always))
(autopair-global-mode)

;; Multiple cursors!
(package-require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-æ") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-'") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-'") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this-dwim)

;; expand-region <3 @magnars
(package-require 'expand-region)
(global-set-key (kbd "C-+") 'er/expand-region)

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

;; Some bindings for special characters
(global-set-key (kbd "M-l") (lambda () (interactive) (insert "\u03bb"))) ;lambda
(global-set-key (kbd "M-f") (lambda () (interactive) (insert "\u0192"))) ;function
(global-set-key (kbd "M--") (lambda () (interactive) (insert "\u2192"))) ;arrow

;; Joining lines
;; https://github.com/rejeep/emacs/blob/master/rejeep-defuns.el#L150-L158
(defun join-line-or-lines-in-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-line))))
(global-set-key (kbd "M-j") 'join-line-or-lines-in-region)

(provide 'bodil-editing)
