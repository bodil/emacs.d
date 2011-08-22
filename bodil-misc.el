;;; bodil-misc.el -- Other stuff

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
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)

;; I CAN HAS LOLCODE?
(require 'lolcode-mode)
(require 'auto-complete)
(defvar ac-source-lolcode
  '((candidates . lolcode-lang-all)))
(add-to-list 'ac-modes 'lolcode-mode)
(add-hook 'lolcode-mode-hook
          (lambda ()
            (setq default-tab-width 2)
            (add-to-list 'ac-sources 'ac-source-lolcode)
            (add-to-list 'ac-sources 'ac-source-yasnippet)))

;; Remember position in file
(setq save-place-file (expand-file-name "~/.emacs.d/saveplace"))
(setq-default save-place t)
(require 'saveplace)

;; Auto-revert buffers
(global-auto-revert-mode t)

;; twittering-mode
(require 'twittering-mode)
(setq twittering-icon-mode t)
(setq twittering-use-master-password t)
(setq twittering-url-show-status nil)
(setq twittering-initial-timeline-spec-string
      '(":home"
        "bodiltv/steria-devs"
        ":replies"))

;; evernote-mode
(require 'evernote-mode)
(setq evernote-username "bodil")
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option
(global-unset-key (kbd "C-c e"))
(global-set-key "\C-cec" 'evernote-create-note)
(global-set-key "\C-ceo" 'evernote-open-note)
(global-set-key "\C-ces" 'evernote-search-notes)
(global-set-key "\C-ceS" 'evernote-do-saved-search)
(global-set-key "\C-cew" 'evernote-write-note)
(global-set-key "\C-cep" 'evernote-post-region)
(global-set-key "\C-ceb" 'evernote-browser)

;; Bongo
(autoload 'bongo "bongo"
  "Start Bongo by switching to a Bongo buffer." t)

;; Mingus
(autoload 'mingus "mingus" nil t)

;;; HTML mode things
(add-hook 'html-mode-hook (lambda () (hl-tags-mode t)))
(add-to-list 'ac-modes 'html-mode)


