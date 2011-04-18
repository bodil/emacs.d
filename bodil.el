; Set colour theme
(require 'color-theme)
(color-theme-zenburn)

; Set font
(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

; Prettify tab bar
(setq EmacsPortable-global-tabbar 't)
(require 'tabbar-ruler)

; Keybindings for cycling buffers in tab bar order
(setq tabbar-buffer-groups-function (lambda () (list "All")))
(global-set-key (kbd "C-<next>") 'tabbar-forward-tab)
(global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)

; Keybindings for cycling buffers in last-used order using iflipb
(require 'iflipb)
(global-set-key (kbd "C-<end>") 'iflipb-next-buffer)
(global-set-key (kbd "C-<home>") 'iflipb-previous-buffer)

; Redefine autocomplete key
(global-set-key (kbd "M-SPC") 'hippie-expand)

; Redefine undo key
(global-set-key (kbd "C-z") 'undo)

; Keybinding for replace-regexp
(global-set-key (kbd "C-c r") 'replace-regexp)

; Maximise the Emacs window
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)
(toggle-fullscreen)

; Re-enable menu bar
(menu-bar-mode)

; Enable CUA selection mode; sorry, it stuck.
(cua-selection-mode 1)

; Enable whole-line-or-region
(require 'whole-line-or-region)
(whole-line-or-region-mode 1)


