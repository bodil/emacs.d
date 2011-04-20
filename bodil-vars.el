;;; bodil-vars.el -- Set variables I want persisted between hosts

;; Set some custom variables
(setq column-number-mode t)
(setq delete-selection-mode t)
(setq ecb-auto-activate t)
(setq ecb-layout-name "bodil")
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
(setq ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15" "bodil")))
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 0.25)
(setq global-smart-tab-mode t)
(setq inferior-lisp-program "cake repl")
(setq js2-bounce-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline t)
(setq todochiku-icons (quote ((default . "default.svg") (alert . "alert.svg") (bell . "bell.svg") (compile . "compile.svg") (irc . "chat.svg") (check . "check.svg") (emacs . "emacs.svg") (star . "star.svg") (social . "social.svg") (alarm . "alarm.svg") (music . "music.svg") (mail . "mail.svg") (term . "terminal.svg") (package . "package.svg"))))
(setq todochiku-icons-directory "~/.emacs.d/todochiku-icons")
(setq whole-line-or-region-extensions-alist (quote ((copy-region-as-kill whole-line-or-region-copy-region-as-kill nil) (kill-region whole-line-or-region-kill-region nil) (kill-ring-save whole-line-or-region-kill-ring-save nil) (yank whole-line-or-region-yank nil) (comment-dwim whole-line-or-region-comment-dwim-2 nil))))

;; I mean, _seriously_...
(setq sentence-end-double-space nil)

;; Enforce newline at EOF
(setq require-final-newline 't)

;; l10n stuff
(setq calendar-week-start-day 1) ; Week starts on Monday in sane countries
(setq european-calendar-style 't)
(setq ps-paper-type 'a4)

