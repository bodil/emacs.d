;;; bodil-mail.el -- Zawinski's Law applies.

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq mail-user-agent 'mu4e-user-agent)

;; Maildir setup
(setq
 mu4e-maildir "~/.mail"
 mu4e-sent-folder "/Sent"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder "/Trash"
 mu4e-refile-folder "/Archive"
 mu4e-get-mail-command "offlineimap"
 mu4e-update-interval nil
 mu4e-sent-messages-behavior 'delete
 mu4e-headers-include-related t
 mu4e-headers-skip-duplicates t
 mu4e-confirm-quit nil
 mu4e-maildir-shortcuts
 '(("/INBOX" . ?i)
   ("/Future Ad Labs" . ?f)
   ("/Lambda Ladies" . ?l)
   ("/LdnClj" . ?c)))

;; User info
(setq
 user-mail-address "bodil@bodil.org"
 user-full-name "Bodil Stokke"
 mu4e-compose-signature "Bodil Stokke <bodil@bodil.org>\n"
 mu4e-user-mail-address-list
 '("bodil@bodil.org"
   "bodil@fastmail.co.uk"
   "bodil@bodil.tv"
   "bodil@futureadlabs.com"
   "bodil.stokke@futureadlabs.com"
   "bodil.stokke@gmail.com")
 mu4e-attachment-dir "~/Downloads")

;; SMTP
(package-require 'async)
(require 'smtpmail-async)
(setq
 send-mail-function 'async-smtpmail-send-it
 message-send-mail-function 'async-smtpmail-send-it
 smtpmail-stream-type 'starttls
 smtpmail-default-smtp-server "mail.lol.camp"
 smtpmail-smtp-user "bodil@bodil.org"
 smtpmail-smtp-server "mail.lol.camp"
 smtpmail-smtp-service 587
 smtpmail-queue-mail nil
 smtpmail-queue-dir "~/.mail/Queue/cur")

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
 mu4e-html2text-command "w3m -dump -T text/html -cols 65536"
 mu4e-view-prefer-html t
 mu4e-view-show-images t)

(add-hook 'mu4e-view-mode-hook (lambda () (visual-line-mode)))
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(add-to-list
 'mu4e-view-actions
 '("ViewInBrowser" . mu4e-action-view-in-browser) t)

;; Compose view
(define-key mu4e-compose-mode-map (kbd "C-c s") 'mml-secure-message-sign-pgpmime)
(define-key mu4e-compose-mode-map (kbd "C-c e") 'mml-secure-message-encrypt-pgpmime)

(add-hook
 'mu4e-compose-pre-hook
 (lambda ()
   "Set the From address based on the To address of the original."
   (let ((msg mu4e-compose-parent-message))
     (when msg
       (setq user-mail-address
             (cond
              ((mu4e-message-contact-field-matches msg :to "@futureadlabs.com$")
               "bodil@futureadlabs.com")
              ((mu4e-message-contact-field-matches msg :to "@doge.enterprises$")
               "bodil@doge.enterprises")
              (t "bodil@bodil.org")))))))


(add-hook
 'mu4e-compose-mode-hook
 (lambda ()
   (visual-line-mode)
   (auto-fill-mode 0)
   (flyspell-mode)))

(provide 'bodil-mail)
