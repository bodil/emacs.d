;; Set the debug option to enable a backtrace when a
;; problem occurs.
(setq debug-on-error t)

;; omg stfu
(setq warning-suppress-types nil)

;; Configure load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/slime"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/swank-js"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/malabar/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/ecb"))

;; Set colour theme
(require 'color-theme)
(color-theme-zenburn)

;; Set font
(set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; Prettify tab bar
(setq EmacsPortable-global-tabbar 't)
(tabbar-mode t)
(tabbar-mwheel-mode t)
(require 'tabbar-ruler)

;; Keybindings for cycling buffers in tab bar order
(setq tabbar-buffer-groups-function (lambda () (list "All")))
(global-set-key (kbd "C-<next>") 'tabbar-forward-tab)
(global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)

;; Keybindings for cycling buffers in last-used order using iflipb
(require 'iflipb)
(global-set-key (kbd "C-<end>") 'iflipb-next-buffer)
(global-set-key (kbd "C-<home>") 'iflipb-previous-buffer)

;; Redefine autocomplete key
(global-set-key (kbd "M-SPC") 'hippie-expand)

;; Redefine undo key
(global-set-key (kbd "C-z") 'undo)

;; Keybinding for replace-regexp
(global-set-key (kbd "C-c r") 'replace-regexp)

;; Maximise the Emacs window
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)
(toggle-fullscreen)

;; Re-enable menu bar
(menu-bar-mode)

;; Enable CUA selection mode; sorry, it stuck.
(cua-selection-mode t)

;; Enable whole-line-or-region
(require 'whole-line-or-region)
(whole-line-or-region-mode t)

;; I mean, _seriously_...
(setq sentence-end-double-space nil)

;; Enforce newline at EOF
(setq require-final-newline 't)

;; l10n stuff
(setq calendar-week-start-day 1) ; Week starts on Monday in sane countries
(setq european-calendar-style 't)
(setq ps-paper-type 'a4)

;; Up-to-date SLIME + slime-js
(require 'slime)
(slime-setup '(slime-fancy slime-repl slime-js))
(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))
(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)))

;; Handy SLIME keybinding
(global-set-key (kbd "C-c s") (lambda () (interactive) (slime-connect "127.0.0.1" "4005")))

;; CEDET/Malabar setup
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "~/.emacs.d/site-lisp/malabar/lib")
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; Make Malabar's autoimport behave more like Eclipse
(defun malabar-eclipse-import ()
  "Eclipse style import handling."
  (interactive)
  (malabar-import-all)
  (malabar-import-group-imports))
(global-set-key (kbd "C-c C-v z") 'malabar-eclipse-import)
(add-hook 'malabar-mode-hook
          (lambda () 
            (add-hook 'after-save-hook 'malabar-compile-file-silently
                      nil t)))

;; ECB setup
(require 'ecb)
(ecb-layout-define "bodil" left nil
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.6153846153846154)
  (ecb-set-methods-buffer)
  (select-window (next-window)))
(global-set-key (kbd "M-<left>") 'ecb-goto-window-ecb-by-smart-selection)
(global-set-key (kbd "M-<right>") 'ecb-goto-window-edit-by-smart-selection)

;; Smex setup
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Auto-insert spaces around operators in C-like languages
(defun my-c-mode-common-hook()
  (smart-insert-operator-hook)
  (local-unset-key (kbd "."))
  (local-unset-key (kbd ":"))
  (local-set-key (kbd "*") 'c-electric-star))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Load more scripts
(load-file "~/.emacs.d/bodil-python.el")

;; Set some custom variables
(setq column-number-mode t)
(setq delete-selection-mode t)
(setq ecb-auto-activate t)
(setq ecb-layout-name "bodil")
(setq ecb-primary-secondary-mouse-buttons (quote mouse-1--mouse-2))
(setq ecb-show-sources-in-directories-buffer (quote ("left7" "left13" "left14" "left15" "bodil")))
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 0.25)
(setq global-linum-mode t)
(setq global-smart-tab-mode t)
(setq inferior-lisp-program "cake repl")
(setq js2-bounce-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline t)
(setq todochiku-icons (quote ((default . "default.svg") (alert . "alert.svg") (bell . "bell.svg") (compile . "compile.svg") (irc . "chat.svg") (check . "check.svg") (emacs . "emacs.svg") (star . "star.svg") (social . "social.svg") (alarm . "alarm.svg") (music . "music.svg") (mail . "mail.svg") (term . "terminal.svg") (package . "package.svg"))))
(setq todochiku-icons-directory "~/.emacs.d/todochiku-icons")
(setq whole-line-or-region-extensions-alist (quote ((copy-region-as-kill whole-line-or-region-copy-region-as-kill nil) (kill-region whole-line-or-region-kill-region nil) (kill-ring-save whole-line-or-region-kill-ring-save nil) (yank whole-line-or-region-yank nil) (comment-dwim whole-line-or-region-comment-dwim-2 nil))))
