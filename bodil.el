;; Set the debug option to enable a backtrace when a
;; problem occurs.
(setq debug-on-error t)

;; Set colour theme
(require 'color-theme)
(color-theme-zenburn)

;; Set font
(set-frame-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; Prettify tab bar
(setq EmacsPortable-global-tabbar 't)
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

;; CEDET setup
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/cedet-1.0"))
;; (load "common/cedet.el")

(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/malabar/lisp"))
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "~/.emacs.d/malabar/lib")
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; ECB setup
(add-to-list 'load-path (expand-file-name "~/.emacs.d/ecb"))
(require 'ecb)
(ecb-layout-define "bodil" left nil
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.6153846153846154)
  (ecb-set-methods-buffer)
  (select-window (next-window)))
(global-set-key (kbd "M-<left>") 'ecb-goto-window-ecb-by-smart-selection)
(global-set-key (kbd "M-<right>") 'ecb-goto-window-edit-by-smart-selection)

(defun malabar-eclipse-import ()
  "Eclipse style import handling."
  (interactive)
  (malabar-import-all)
  (malabar-import-group-imports))
(global-set-key (kbd "C-c C-v z") 'malabar-eclipse-import)
