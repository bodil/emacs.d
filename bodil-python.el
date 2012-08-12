;;; bodil-python.el -- Python configuration

 ;; Load explicitly to override Emacs's bundled python-mode
(load (concat dotfiles-dir "site-lisp/python-mode/python.el"))

;; Pytest bindings
(require 'pytest)
(add-hook
 'python-mode-hook
 (lambda ()
   (define-key python-mode-map (kbd "C-c C-,") 'pytest-run-file)))
