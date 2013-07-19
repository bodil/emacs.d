;;; bodil-misc-modes.el -- Miscellaneous modes.

(package-require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(provide 'bodil-misc-modes)
