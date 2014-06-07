;;; bodil-mail.el -- Zawinski's Law applies.

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq mail-user-agent 'mu4e-user-agent)

;; Maildir setup
(setq
 mu4e-maildir "~/.mail/fastmail"
 mu4e-sent-folder "/Sent Items"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder "/Trash"
 mu4e-refile-folder "/Archive"
 mu4e-get-mail-command "mbsync fastmail-sync"
 mu4e-update-interval nil
 mu4e-sent-messages-behavior 'delete
 mu4e-headers-include-related t
 mu4e-headers-skip-duplicates t)

;; User info
(setq
 user-mail-address "bodil@bodil.org"
 user-full-name "Bodil Stokke"
 message-signature "Bodil Stokke <bodil@bodil.org>"
 message-signature-file "~/.signature"
 mu4e-user-mail-address-list
 '("bodil@bodil.org"
   "bodil@fastmail.co.uk"
   "bodil@bodil.tv"
   "bodil@futureadlabs.com"
   "bodil.stokke@futureadlabs.com"
   "bodil.stokke@gmail.com")
 mu4e-attachment-dir "~/Downloads")

;; SMTP
(setq
 message-send-mail-function 'smtpmail-send-it
 smtpmail-stream-type 'ssl
 smtpmail-default-smtp-server "mail.messagingengine.com"
 smtpmail-smtp-server "mail.messagingengine.com"
 smtpmail-smtp-service 465
 smtpmail-queue-mail nil
 smtpmail-queue-dir "~/.mail/queue/cur")

;; Headers view
(setq
 mu4e-headers-fields
 '((:human-date . 10)
   (:flags . 5)
   ;; (:signature . 6)
   ;; (:mailing-list . 15)
   (:from-or-to . 22)
   (:subject))
 mu4e-headers-time-format "Tdy %H:%M"
 mu4e-headers-date-format "%e %b %y"
 mu4e-use-fancy-chars nil)

;; Message view
(setq
 mu4e-view-prefer-html t
 mu4e-view-show-images t)
(add-hook 'mu4e-view-mode-hook (lambda () (visual-line-mode)))
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(add-to-list
 'mu4e-view-actions
 '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; 1) messages to me@foo.com should be replied with From:me@foo.com
;; 2) messages to me@bar.com should be replied with From:me@bar.com
;; 3) all other mail should use From:me@cuux.com
;; (add-hook
;;  'mu4e-compose-pre-hook
;;  (defun my-set-from-address ()
;;    "Set the From address based on the To address of the original."
;;    (let ((msg mu4e-compose-parent-message)) ;; msg is shorter...
;;      (setq user-mail-address
;;            (cond
;;             ((mu4e-message-contact-field-matches msg :to "me@foo.com")
;;              "me@foo.com")
;;             ((mu4e-message-contact-field-matches msg :to "me@bar.com")
;;              "me@bar.com")
;;             (t "me@cuux.com"))))))

;; Compose view
(define-key mu4e-compose-mode-map (kbd "C-c s") 'mml-secure-message-sign-pgpmime)
(define-key mu4e-compose-mode-map (kbd "C-c e") 'mml-secure-message-encrypt-pgpmime)

(add-hook
 'mu4e-compose-mode-hook
 (lambda ()
   (set-fill-column 72)
   (flyspell-mode)))

(provide 'bodil-mail)
