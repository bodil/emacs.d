;;; bodil.el -- User specific entry point

;; omg stfu
(setq debug-on-error nil)
(setq warning-suppress-types nil)
(setq stack-trace-on-error nil)

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

;; Add every subdirectory of ~/.emacs.d/site-lisp to the load path
(dolist (elt (delq nil
                   (mapcar
                    (lambda (filedesc)
                      (let ((filename (car filedesc))
                            (isdirectory (cadr filedesc)))
                        (if (and isdirectory
                                 (not (integerp (string-match "/\\.+$" filename))))
                            filename
                          nil)))
                    (directory-files-and-attributes "~/.emacs.d/site-lisp" t))))
  (add-to-list 'load-path elt))

;; Load more scripts
(load "~/.emacs.d/bodil-theme")
(load "~/.emacs.d/bodil-lisp")
(load "~/.emacs.d/bodil-haskell")
(load "~/.emacs.d/bodil-python")
;; (load "~/.emacs.d/bodil-java")
(load "~/.emacs.d/bodil-js")
(load "~/.emacs.d/bodil-erc")
(load "~/.emacs.d/bodil-mail")
(load "~/.emacs.d/bodil-w3m")
(load "~/.emacs.d/bodil-misc")
(load "~/.emacs.d/bodil-bindings")
(load "~/.emacs.d/bodil-vars")

;; Load X-specific scripts
(Xlaunch (load "~/.emacs.d/bodil-x"))

