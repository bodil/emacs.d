;;; sessions.el -- On the topic of preserving user state

;; Configure desktop saving
(setq desktop-restore-eager 5)
(setq desktop-save (quote if-exists))
(desktop-save-mode 1)

