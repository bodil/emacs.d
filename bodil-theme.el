;;; bodil-theme.el -- Visual things

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

;; Maximise the Emacs window
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)
(Xlaunch (toggle-fullscreen))

;; Re-enable menu bar
(menu-bar-mode)

;; Make a nice custom ECB layout
(require 'ecb)
(ecb-layout-define "bodil" left nil
  (ecb-set-directories-buffer)
  (ecb-split-ver 0.6153846153846154)
  (ecb-set-methods-buffer)
  (select-window (next-window)))

;; Show line numbers in buffers
(global-linum-mode t)
