;;; theme.el --- Appearance matters

;; Let's see what we're running on
(setq on-console (null window-system))

;; Show line numbers in buffers
(global-linum-mode t)
(setq linum-format (if on-console "%4d " "%4d"))

;; Redefine linum-on to ignore terminal buffers, because just turning
;; it off in term-mode-hook doesn't work.
(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))

;; Highlight current line
(global-hl-line-mode)

;; Set custom theme path
(setq custom-theme-directory (concat dotfiles-dir "themes"))
(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; Prepare colour themes
(defun theme-light ()
  (interactive)
  (load-theme 'adwaita)
  (set-face-background 'highlight "#dddddd")
  (set-face-foreground 'highlight nil)
  (set-face-background 'linum nil)
  (set-face-foreground 'linum nil))
(defun theme-dark ()
  (interactive)
  (load-theme 'bubbleberry))
(theme-dark)
