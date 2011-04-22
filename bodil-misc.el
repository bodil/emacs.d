;;; bodil-misc.el -- Other stuff

;; Obey Zawinski's Law
(setq gnus-select-method '(nnimap "gmail"
                                  (nnimap-address "imap.gmail.com")
                                  (nnimap-server-port 993)
                                  (nnimap-stream ssl)))
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "bodil@bodil.tv" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-local-domain "bodil.tv")

;; ....Google Maps? In an editor?
(require 'google-maps)

;; Yasnippet setup
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/site-lisp/yasnippet/snippets")

;; Org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-log-done t)
(setq org-directory (expand-file-name "~/Dropbox/Org"))
(setq org-mobile-directory (expand-file-name "~/Dropbox/MobileOrg"))
(setq org-mobile-inbox-for-pull (expand-file-name "~/Dropbox/MobileOrg/mobileorg.org"))
(setq org-agenda-files (list (expand-file-name "~/Dropbox/Org/bodil.org")))

;; G-client
(require 'g)

;; Persistent undo history
(require 'undohist)
(undohist-initialize)

;; Multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(setq multi-term-dedicated-select-after-open-p t)
(setq multi-term-switch-after-close (quote PREVIOUS))
(global-set-key (kbd "<f11>") 'multi-term-dedicated-toggle)


