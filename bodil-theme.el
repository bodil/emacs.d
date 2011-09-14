;;; bodil-theme.el -- Visual things

;; Set colour theme
(if (functionp 'load-theme)
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
(setq default-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(setq presentation-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")
(set-frame-font default-frame-font)

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

;; Prettify tab bar
(tabbar-mode t)
(tabbar-mwheel-mode t)

;; Setup tabbar groupings
(defun bodil-tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((memq major-mode '(html-mode css-mode js-mode js2-mode coffee-mode jade-mode))
     "Web")
    ((string-match "^term-mode" (symbol-name major-mode))
     "Shells")
    ((memq major-mode '(erc-mode))
     "ERC")
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((member (buffer-name) '("*scratch*" "*Messages*" "*Backtrace*" "*Compile-Log*"
                             "*Completions*"))
     "Common")
    
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Help"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))
(setq tabbar-buffer-groups-function 'bodil-tabbar-buffer-groups)

;; Don't display some buffers
(setq tabbar-buffer-list-function
      (lambda ()
        (remove-if
         (lambda (buffer)
           (let ((name (buffer-name buffer)))
             (or (string= name "*lintnode*")
                 (string= name "*Malabar Compilation*"))
             ))
         (tabbar-buffer-list))))

;; Make a nice custom ECB layout
(require 'ecb)
(ecb-layout-define "bodil" left nil
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.6153846153846154)
  (ecb-set-methods-buffer)
  (select-window (next-window)))

;; Show line numbers in buffers
(global-linum-mode t)
(setq linum-format "%4d")

;; Setup rfringe
(require 'rfringe)
(rfringe-show-region)

