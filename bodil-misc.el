;;; bodil-misc.el -- Other stuff

;; Start emacsclient server
(server-start)
;; And the edit server for Chrome's Edit With Emacs extension
(require 'edit-server)
(edit-server-start)

;; Yasnippet setup
(require 'yasnippet)
(yas/initialize)
(setq yas/root-directory '("~/.emacs.d/snippets"))
(mapc 'yas/load-directory yas/root-directory)
(setq yas/trigger-key "M-SPC")

;; Org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-log-done t)

;; Deft
(require 'deft)
(setq deft-extension "md"
      deft-directory (expand-file-name "~/Ubuntu One/Org/Deft/")
      deft-text-mode 'markdown-mode)

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

;; I CAN HAS LOLCODE?
(require 'lolcode-mode)
(add-hook 'lolcode-mode-hook
          (lambda ()
            (setq default-tab-width 2)))

;; Remember position in file
(setq save-place-file (expand-file-name "~/.emacs.d/saveplace"))
(setq-default save-place t)
(require 'saveplace)

;; Auto-revert buffers
(global-auto-revert-mode t)

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

;;; HTML mode things
(add-hook 'html-mode-hook (lambda () (hl-tags-mode t)))

;; Group the ibuffer list by vc-root
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (ibuffer-do-sort-by-alphabetic)))

;; (require 'rename-sgml-tag)
;; (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)

;; (require 'js2-rename-var)
;; (define-key js2-mode-map (kbd "C-c C-r") 'js2-rename-var)

;; Enable ethan-wspace
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/ethan-wspace/lisp"))
(require 'ethan-wspace)
(global-ethan-wspace-mode 1)
