;;; bodil-erc.el -- ERC configuration

(require 'erc)

(when (file-exists-p "~/.emacs.d/private.el")
  (load-file "~/.emacs.d/private.el"))
(defun irc ()
  "Connect to IRC."
  (interactive)
  (dolist (elt znc-users)
    (let ((network (car elt)) (password (cadr elt)))
      (erc :server "bodil.wtf.la" :port 1337 :nick network :password (concat network ":" password)))))

;; Prioritise privmsgs maximally
(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p) 
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))

;; Desktop notifications on privmsg
(defun erc-desktop-notification-on-privmsg (proc msg)
  (interactive)
  (when (erc-current-nick-p (car (erc-response.command-args msg)))
    (todochiku-message (erc-response.sender msg) (erc-response.contents msg) (todochiku-icon 'irc))))
(add-hook 'erc-server-PRIVMSG-functions 'erc-desktop-notification-on-privmsg)

(setq erc-modules (quote (button completion dcc fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
(setq erc-track-exclude-server-buffer t)
(setq erc-track-exclude-types (quote ("JOIN" "KICK" "NICK" "PART" "QUIT" "MODE" "333" "353")))
(setq erc-track-position-in-mode-line t)
(setq erc-track-showcount nil)
(setq erc-track-switch-direction (quote importance))
(setq erc-header-line-uses-tabbar-p t)
(setq erc-auto-query (quote bury))
(setq erc-fill-function (quote erc-fill-static))
(setq erc-fill-static-center 15)
(setq erc-kill-buffer-on-part t)
(setq erc-kill-queries-on-quit t)
(setq erc-kill-server-buffer-on-quit t)
(setq erc-paranoid t)
(setq erc-query-display (quote window))
(setq erc-auto-query (quote bury))



