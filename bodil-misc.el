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

;; Yasnippet setup
(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory '("~/.emacs.d/snippets" "~/.emacs.d/site-lisp/yasnippet/snippets"))
(mapc 'yas/load-directory yas/root-directory)
(setq yas/trigger-key "M-SPC")

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

;; Persistent undo history
(require 'undohist)
(undohist-initialize)

;; Multi-term
(require 'multi-term)
(setq multi-term-program "/bin/bash")
(setq multi-term-dedicated-select-after-open-p t)
(setq multi-term-switch-after-close (quote PREVIOUS))
(global-set-key (kbd "<f11>") 'multi-term-dedicated-toggle)
(setq term-unbind-key-list
      '("C-x" "C-h" "<ESC>"))
(setq term-bind-key-alist
      '(("C-c C-c" . term-interrupt-subjob)
        ("M-f" . term-send-forward-word)
        ("M-b" . term-send-backward-word)
        ("M-o" . term-send-backspace)
        ("M-M" . term-send-forward-kill-word)
        ("M-N" . term-send-backward-kill-word)
        ("M-r" . term-send-reverse-search-history)
        ("M-," . term-send-input)
        ("M-." . comint-dynamic-complete)))

;; w3m
(require 'w3m-load)

;; Magit
(require 'magit)
(require 'magit-svn)

;; I CAN HAS LOLCODE?
(require 'lolcode-mode)
(defvar ac-source-lolcode
  '((candidates . lolcode-lang-all)))
(add-to-list 'ac-modes 'lolcode-mode)
(add-hook 'lolcode-mode-hook
          (lambda ()
            (setq default-tab-width 2)
            (add-to-list 'ac-sources 'ac-source-lolcode)))


