;;; bodil-python.el -- Python configuration

;; We want python-mode, not python.el
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(require 'ipython)
