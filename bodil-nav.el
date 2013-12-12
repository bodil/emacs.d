;;; bodil-nav.el -- Navigation things.

(package-require 'dash)
(require 'dash)

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

;;; Project Explorer

(package-require 'helm)
(package-require 'project-explorer)
(require 'project-explorer)
(setq-default pe/width 28)

;; Touch tree to open
(define-key project-explorer-mode-map (kbd "<mouse-1>") 'pe/return)

;; Open project explorer with swipe from left margin
(global-set-key
 (kbd "<left-margin> <drag-mouse-1>")
 (lambda () (interactive)
   (-if-let (win (car (-keep 'get-buffer-window (pe/get-project-explorer-buffers))))
       (delete-window win)
     (project-explorer-open))))

;; Highlight current tree item
(add-hook 'project-explorer-mode-hook 'hl-line-mode)

(provide 'bodil-nav)
