;;; bodil-theme.el -- Visual things

;; Set colour theme
(require 'color-theme)
(color-theme-zenburn)

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
    ((string-match "^circe-" (symbol-name major-mode))
     "IRC"
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

;; Maximise the Emacs window
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)
(Xlaunch (toggle-fullscreen))

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
