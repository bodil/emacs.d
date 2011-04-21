;;; bodil-theme.el -- Visual things

;; Set colour theme
(require 'zenburn)
(color-theme-zenburn)
(set-face-underline-p 'highlight nil)
(set-face-background 'highlight "#323232")

;; Set font
(set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; Prettify tab bar
(tabbar-mode t)
(tabbar-mwheel-mode t)

;; Setup tabbar groupings
(defun bodil-tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((string-match "^circe-query-" (symbol-name major-mode))
     "IRC Queries"
     )
    ((string-match "^circe-channel-" (symbol-name major-mode))
     "IRC Channels"
     )
    ((string-match "^circe-server-" (symbol-name major-mode))
     "IRC Servers"
     )
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((member (buffer-name) '("*scratch*" "*Messages*" "*Backtrace*" "*Compile-Log*" "*Malabar Compilation*"
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


;; Re-enable menu bar
(menu-bar-mode)

;; Make a nice custom ECB layout
(require 'ecb)
(ecb-layout-define "bodil" left nil
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.6153846153846154)
  (ecb-set-methods-buffer)
  (select-window (next-window)))

;; Show line numbers in buffers
(global-linum-mode t)
