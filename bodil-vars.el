;;; bodil-vars.el -- Set variables I want persisted between hosts

;; Set some custom variables
(setq-default tab-width 4)
(setq column-number-mode t)
(setq delete-selection-mode t)
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

;; Stop pausing the display update while processing input events
(setq redisplay-dont-pause t)

;; Recursive minibuffers
(setq enable-recursive-minibuffers t)

