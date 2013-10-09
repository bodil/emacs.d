;;; bodil-misc-modes.el -- Miscellaneous modes.

;; Puppet config files
(package-require 'puppet-mode)
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

;; Varnish config files
(package-require 'vcl-mode)
(add-to-list 'auto-mode-alist '("\\.vcl$" . vcl-mode))

;; Scala
(package-require 'scala-mode2)
(add-to-list 'load-path (concat dotfiles-dir "site-lisp/ensime/elisp"))
(autoload 'ensime "ensime" nil t)
(autoload 'ensime-scala-mode-hook "ensime")
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(provide 'bodil-misc-modes)
