;;; bodil.el -- User specific entry point

;; Set the debug option to enable a backtrace when a
;; problem occurs.
(setq debug-on-error t)

;; omg stfu
(setq warning-suppress-types nil)
(setq stack-trace-on-error t)

;; Get hostname
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

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
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/haskell-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/haskell-mode-exts"))
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
(load "~/.emacs.d/bodil-theme")
(load "~/.emacs.d/bodil-lisp")
(load "~/.emacs.d/bodil-haskell")
(load "~/.emacs.d/bodil-python")
(load "~/.emacs.d/bodil-java")
(load "~/.emacs.d/bodil-js")
(load "~/.emacs.d/bodil-erc")
(load "~/.emacs.d/bodil-mail")
(load "~/.emacs.d/bodil-w3m")
(load "~/.emacs.d/bodil-misc")
(load "~/.emacs.d/bodil-bindings")
(load "~/.emacs.d/bodil-vars")

;; Load X-specific scripts
(Xlaunch (load "~/.emacs.d/bodil-x"))

