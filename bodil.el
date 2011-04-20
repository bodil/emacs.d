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

;; Load more scripts
(load-file "~/.emacs.d/bodil-theme.el")
(load-file "~/.emacs.d/bodil-python.el")
(load-file "~/.emacs.d/bodil-java.el")
(load-file "~/.emacs.d/bodil-js.el")
(load-file "~/.emacs.d/bodil-bindings.el")
(load-file "~/.emacs.d/bodil-vars.el")

