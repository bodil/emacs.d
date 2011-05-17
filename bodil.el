;;; bodil.el -- User specific entry point

;; Set the debug option to enable a backtrace when a
;; problem occurs.
(setq debug-on-error t)

;; omg stfu
(setq warning-suppress-types nil)
(setq stack-trace-on-error t)

;; Start emacsclient server
(server-start)
;; And the edit server for Chrome's Edit With Emacs extension
(require 'edit-server)
(edit-server-start)

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

;; Some simple macros to more easily tell if we're running GNUEmacs or XEmacs
;; taken from the .emacs of sukria@online.fr | http://sukria.online.fr
(defmacro GNUEmacs (&rest x)
  (list 'if (not running-xemacs) (cons 'progn x)))
(defmacro XEmacs (&rest x)
  (list 'if running-xemacs (cons 'progn x)))
(defmacro Xlaunch (&rest x)
  (list 'if (eq window-system 'x) (cons 'progn x)))

;; Configure load path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/slime"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/swank-js"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/clojure-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/malabar/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/ecb"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/auto-complete"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/yasnippet"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/zenburn"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/wl"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/smart-tab"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/undohist"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/multi-term"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/coffee-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/rfringe"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/lintnode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/emacs-w3m"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/magit"))

;; Load more scripts
(load-file "~/.emacs.d/bodil-theme.el")
(load-file "~/.emacs.d/bodil-lisp.el")
(load-file "~/.emacs.d/bodil-python.el")
(load-file "~/.emacs.d/bodil-java.el")
(load-file "~/.emacs.d/bodil-js.el")
(load-file "~/.emacs.d/bodil-erc.el")
(load-file "~/.emacs.d/bodil-mail.el")
(load-file "~/.emacs.d/bodil-w3m.el")
(load-file "~/.emacs.d/bodil-misc.el")
(load-file "~/.emacs.d/bodil-bindings.el")
(load-file "~/.emacs.d/bodil-vars.el")

;; Load X-specific scripts
(Xlaunch (load-file "~/.emacs.d/bodil-x.el"))

