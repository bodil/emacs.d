;;; bodil-vars.el -- Set variables I want persisted between hosts

;; Set some custom variables
(setq-default tab-width 4)
(setq column-number-mode t)
(setq delete-selection-mode t)
(setq ecb-auto-activate nil)
(setq ecb-layout-name "bodil")
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
(setq ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15" "bodil")))
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 0.25)
(setq whole-line-or-region-extensions-alist (quote ((copy-region-as-kill whole-line-or-region-copy-region-as-kill nil) (kill-region whole-line-or-region-kill-region nil) (kill-ring-save whole-line-or-region-kill-ring-save nil) (yank whole-line-or-region-yank nil) (comment-dwim whole-line-or-region-comment-dwim-2 nil))))
(setq smart-tab-completion-functions-alist (quote ((emacs-lisp-mode . auto-complete) (text-mode . dabbrev-completion))))
(setq smart-tab-using-auto-complete t)
(setq smart-tab-using-hippie-expand t)
(setq smart-tab-disabled-major-modes (quote (org-mode term-mode w3m-mode)))
(setq explicit-shell-file-name "/bin/bash")
(setq c-require-final-newline (quote ((c-mode . t) (c++-mode . t) (objc-mode . t) (java-mode) (idl-mode) (pike-mode) (awk-mode))))

;; I mean, _seriously_...
(setq sentence-end-double-space nil)

;; Enforce newline at EOF
(setq require-final-newline 't)

;; l10n stuff
(setq calendar-week-start-day 1) ; Week starts on Monday in sane countries
(setq european-calendar-style 't)
(setq ps-paper-type 'a4)

;; Drag browse-url kicking and screaming into the present
(setq browse-url-browser-function (quote browse-url-generic))
(setq browse-url-generic-program "xdg-open")

;; Setup buffers for iflipb to ignore
(setq iflipb-ignore-buffers '("*Messages*" "*Compile-Log*" "*Malabar Compilation*" "*scratch*"
                              "*lintnode*"))

;; Configure desktop saving
(setq desktop-restore-eager 5)
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

