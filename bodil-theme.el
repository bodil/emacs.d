;;; theme.el --- Appearance matters

;; Let's see what we're running on
(setq on-console (null window-system))

;; No splash screen
(setq inhibit-startup-message t)

;; Some X11 setup
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Show line numbers in buffers
(global-linum-mode t)
(setq linum-format (if on-console "%4d " "%4d"))

;; Show column numbers in modeline
(setq column-number-mode t)

;; Redefine linum-on to ignore terminal buffers, because just turning
;; it off in term-mode-hook doesn't work.
(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))

;; Highlight current line
(global-hl-line-mode)

;; Highlight matching parens
(show-paren-mode 1)

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
  (load-theme 'bubbleberry t)
  (set-face-background 'linum nil))
(theme-dark)

;; Set default and presentation mode fonts
(setq default-frame-font "-unknown-Ubuntu Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
(set-frame-font default-frame-font)
(setq presentation-frame-font "-unknown-Ubuntu Mono-normal-normal-normal-*-21-*-*-*-m-0-iso10646-1")
(defun toggle-presentation-font ()
  (interactive)
  (set-frame-font (if (string= (frame-parameter nil 'font) default-frame-font)
                      presentation-frame-font default-frame-font)))
(global-set-key (kbd "C-<f9>") 'toggle-presentation-font)

;; Engage!
(require 'nyan-mode)
(nyan-mode 1)
