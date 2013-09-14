;;; bodil-revealjs.el -- Reveal.js integration, because we can.

(when (require 'xwidget nil t)

  (defun revealjsp (xwidget)
    (equal 'revealjs-mode
           (with-current-buffer (xwidget-buffer xwidget) major-mode)))

  (defun xwidget-event-handler ()
    "Receive xwidget event."
    (interactive)
    (xwidget-log "stuff happened to xwidget %S" last-input-event)
    (let*
        ((xwidget-event-type (nth 1 last-input-event))
         (xwidget (nth 2 last-input-event)))
      (funcall 'revealjs-webkit-callback xwidget xwidget-event-type)))

  (defun revealjs-webkit-callback (xwidget xwidget-event-type)
    (save-excursion
      (cond ((buffer-live-p (xwidget-buffer xwidget))
             (set-buffer (xwidget-buffer xwidget))
             (let* ((strarg  (nth 3 last-input-event)))
               (cond ((eq xwidget-event-type 'document-load-finished)
                      (xwidget-log "webkit finished loading: '%s'" (xwidget-webkit-get-title xwidget))
                      (if (revealjsp xwidget) (revealjs-adjust-width)
                        (xwidget-adjust-size-to-content xwidget)))
                     ((eq xwidget-event-type 'navigation-policy-decision-requested)
                      (if (string-match ".*#\\(.*\\)" strarg)
                          (xwidget-webkit-show-id-or-named-element xwidget (match-string 1 strarg))))
                     (t (xwidget-log "unhandled event:%s" xwidget-event-type)))))
            (t (xwidget-log "error: callback called for xwidget with dead buffer")))))

  (defun revealjs-open (url &optional new-window-flag)
    (interactive
     (browse-url-interactive-arg "Slide deck URL: "
                                 "http://localhost:1337/"))
    (let*
        ((bufname "*revealjs*")
         (buf (get-buffer-create bufname))
         xw)
      (setq xwidget-webkit-last-session-buffer (switch-to-buffer buf))
      (insert " ")
      (setq xw (xwidget-insert 1 'webkit-osr  bufname 1000 1000))
      (xwidget-put xw 'callback 'revealjs-webkit-callback)
      (revealjs-mode)
      (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url)
      (xwidget-webkit-fit-width)))

  (defun revealjs-reload ()
    (interactive)
    (let ((url (xwidget-webkit-current-url))
          (ses (xwidget-webkit-current-session)))
      (xwidget-webkit-goto-uri ses "http://example.com/")
      (xwidget-webkit-goto-uri ses url)))

  (defun revealjs-organise ()
    (interactive)
    (delete-other-windows (get-buffer-window (current-buffer)))
    (set-window-buffer (split-window-right) (xwidget-buffer (xwidget-webkit-current-session))))

  (defmacro revealjs-exec (script)
    `(xwidget-webkit-execute-script
      (xwidget-webkit-current-session)
      ,script))

  (defun revealjs-next-slide ()
    (interactive)
    (revealjs-exec "Reveal.navigateNext();"))

  (defun revealjs-prev-slide ()
    (interactive)
    (revealjs-exec "Reveal.navigatePrev();"))

  (defun revealjs-navigate-up ()
    (interactive)
    (revealjs-exec "Reveal.navigateUp();"))

  (defun revealjs-navigate-down ()
    (interactive)
    (revealjs-exec "Reveal.navigateDown();"))

  (defun revealjs-navigate-left ()
    (interactive)
    (revealjs-exec "Reveal.navigateLeft();"))

  (defun revealjs-navigate-right ()
    (interactive)
    (revealjs-exec "Reveal.navigateRight();"))

  (defun revealjs-overview ()
    (interactive)
    (revealjs-exec "Reveal.toggleOverview();"))

  (defun revealjs-adjust-width ()
    (interactive)
    (let ((win (get-buffer-window (xwidget-buffer (xwidget-webkit-current-session)))))
      (when (window-live-p win)
        (letrec ((left (car (window-inside-pixel-edges win)))
                 (right (caddr (window-inside-pixel-edges win)))
                 (width (- right left))
                 (top (cadr (window-inside-pixel-edges win)))
                 (bottom (cadddr (window-inside-pixel-edges win)))
                 (height (- bottom top)))
          (xwidget-webkit-adjust-size width height)
          (revealjs-exec
           (concat "document.body.style.width = \""
                   (int-to-string width)
                   "px\"; document.body.style.height = \""
                   (int-to-string height)
                   "\"; Reveal.layout();"))))))

  (defun revealjs-on-window-resize (frame)
    (let ((win (get-buffer-window (xwidget-buffer (xwidget-webkit-current-session)))))
      (when (eq frame (window-frame win))
        (revealjs-adjust-width))))

  (defvar revealjs-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<next>") 'revealjs-next-slide)
      (define-key map (kbd "<prior>") 'revealjs-prev-slide)
      (define-key map (kbd "<up>") 'revealjs-navigate-up)
      (define-key map (kbd "<down>") 'revealjs-navigate-down)
      (define-key map (kbd "<left>") 'revealjs-navigate-left)
      (define-key map (kbd "<right>") 'revealjs-navigate-right)
      (define-key map (kbd "SPC") 'revealjs-overview)
      (define-key map (kbd "r") 'revealjs-reload)
      map))

  (global-set-key (kbd "C-c g") 'revealjs-reload)
  (global-set-key (kbd "C-c o") 'revealjs-organise)
  (global-set-key (kbd "C-c <next>") 'revealjs-next-slide)
  (global-set-key (kbd "C-c <prior>") 'revealjs-prev-slide)

  (define-derived-mode revealjs-mode
    xwidget-webkit-mode "Reveal.js" "Webkit mode adapted for Reveal.js slides")

  (add-to-list 'window-size-change-functions 'revealjs-on-window-resize))

(provide 'bodil-revealjs)
