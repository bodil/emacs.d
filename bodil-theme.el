;;; bodil-theme.el -- Visual things

;; Set colour theme
(if (boundp 'custom-theme-load-path)
    (progn
      (add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/site-lisp/zenburn"))
      (load-theme 'zenburn))
  (progn
    (require 'color-theme-zenburn)
    (color-theme-zenburn)
    (set-face-underline-p 'highlight nil)
    ))
(set-face-background 'highlight "#323232")

;; Highlight current line
(setq global-hl-line-mode t)

;; Set font
(setq default-frame-font "-unknown-UbuntuBeta Mono-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")
(setq presentation-frame-font "-unknown-UbuntuBeta Mono-normal-normal-normal-*-21-*-*-*-m-0-iso10646-1")
(set-frame-font default-frame-font)
(set-default-font default-frame-font)

;; Bind a key for toggling font size for presentations
(global-set-key (kbd "C-<f9>") (lambda () (interactive)
   (set-frame-font (if (string= (frame-parameter nil 'font) default-frame-font) presentation-frame-font default-frame-font))))

;; Configure Todochiku icon theme
(setq todochiku-icons-directory "~/.emacs.d/todochiku-icons")
(setq todochiku-icons
      (quote ((default . "default.svg")
              (alert . "alert.svg")
              (bell . "bell.svg")
              (compile . "compile.svg")
              (irc . "chat.svg")
              (check . "check.svg")
              (emacs . "emacs.svg")
              (star . "star.svg")
              (social . "social.svg")
              (alarm . "alarm.svg")
              (music . "music.svg")
              (mail . "mail.svg")
              (term . "terminal.svg")
              (package . "package.svg"))))

;; Make a nice custom ECB layout
;; (require 'ecb)
;; (ecb-layout-define "bodil" left nil
;;   (ecb-set-directories-buffer)
;;   (ecb-split-ver 0.6153846153846154)
;;   (ecb-set-methods-buffer)
;;   (select-window (next-window)))

;; Show line numbers in buffers
(global-linum-mode t)
(setq linum-format "%4d")
(set-face-background 'linum "#3f3f3f")
(set-face-foreground 'linum "#606660")

;; Setup rfringe
(require 'rfringe)
(rfringe-show-region)

