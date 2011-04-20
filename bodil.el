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
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/auto-complete"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/google-maps"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/circe"))

;; Load more scripts
(load-file "~/.emacs.d/bodil-theme.el")
(load-file "~/.emacs.d/bodil-lisp.el")
(load-file "~/.emacs.d/bodil-python.el")
(load-file "~/.emacs.d/bodil-java.el")
(load-file "~/.emacs.d/bodil-js.el")
(load-file "~/.emacs.d/bodil-misc.el")
(load-file "~/.emacs.d/bodil-irc.el")
(load-file "~/.emacs.d/bodil-bindings.el")
(load-file "~/.emacs.d/bodil-vars.el")

