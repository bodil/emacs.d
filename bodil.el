(require 'color-theme)
(color-theme-initialize)
(color-theme-zenburn)

(set-default-font "-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

(setq EmacsPortable-global-tabbar 't)
(require 'tabbar-ruler)

(setq tabbar-buffer-groups-function (lambda () (list "All")))
(global-set-key (kbd "C-x C-<right>") 'tabbar-forward-tab)
(global-set-key (kbd "C-x C-<left>") 'tabbar-backward-tab)

