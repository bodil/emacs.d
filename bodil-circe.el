;; bodil-irc.el -- IRC configuration
(when (file-exists-p "~/.emacs.d/private.el")
  (load-file "~/.emacs.d/private.el"))
(autoload 'circe "circe" "Connect to an IRC server" t)

(setq circe-server-coding-system '(latin-1 . undecided)
      )

(eval-after-load "circe"
  '(progn
     (require 'lui-irc-colors)
     (add-to-list 'lui-pre-output-hook 'lui-irc-colors)
     ;; (add-to-list 'circe-receive-message-functions
     ;;              'fc-bitlbee-auth)
     ;; (add-to-list 'circe-receive-message-functions
     ;;              'bitmetv-auth)
     (add-to-list 'circe-receive-message-functions
                  'bitlbee-chat-notify)
     ))

(defun bitlbee-chat-notify (nick user host command args)
  "Notify using Todochiku when a Bitlbee contact sends a message."
  (with-circe-server-buffer
    (when (and (string= circe-server-network "bitlbee")
               (string= command "PRIVMSG")
               (circe-server-my-nick-p (car args))
               )
      (todochiku-message nick (car (cdr args)) (todochiku-icon 'irc))
      )))

;; (defun bitmetv-auth (nick user host command args)
;;   "Handle #bitmetv auth."
;;   (with-circe-server-buffer
;;     (when (string= circe-server-network "lostirc")
;;       (cond ((and (string= command "NOTICE")
;;                   (string= nick "NickServ")
;;                   (circe-server-my-nick-p (car args)))
;;              (cond ((string-match "This nickname is registered and protected" (car (cdr args)))
;;                     (circe-server-send (format "PRIVMSG NickServ :identify %s" lostirc-passwd))
;;                     (string-match "Password accepted - you are now recognized" (car (cdr args)))
;;                     (circe-server-send (format "PRIVMSG BitMeTV :!invite %s %s" bitmetv-uname bitmetv-irckey))
;;                     )))
;;             ((and (string= command "INVITE")
;;                   (string= nick "BitMeTV")
;;                   (circe-server-my-nick-p (car args)))
;;              (cond ((string= (car (cdr args)) "#bitmetv.announce")
;;                    (circe-server-send "JOIN #bitmetv.announce"))))
;;             ))))

;; (defun fc-bitlbee-auth (nick user host command args)
;;   "Authenticate to a bitlbee server."
;;   (when (and (string= command "JOIN")
;;              (circe-server-my-nick-p nick))
;;     (with-circe-server-buffer
;;       (when (string= circe-server-network "bitlbee")
;;         (circe-server-send
;;          (format "PRIVMSG &bitlbee :identify %s"
;;                  bitlbee-passwd))))))

(defun irc ()
  "Connect to IRC."
  (interactive)
  (dolist (elt znc-users)
    (let ((network (car elt)) (password (cadr elt)))
      (circe "bodil.wtf.la" "1337" network (concat network ":" password)))))


