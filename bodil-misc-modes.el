;;; bodil-misc-modes.el -- Miscellaneous modes.

;; Puppet config files
(package-require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; Varnish config files
(package-require 'vcl-mode)
(add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))

(provide 'bodil-misc-modes)
