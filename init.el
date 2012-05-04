;;; init.el --- Where all the magic begins
;;
;; This is the first thing to get loaded.
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;; omg stfu
(setq debug-on-error nil)
(setq warning-suppress-types nil)
(setq stack-trace-on-error nil)
(setq help-xref-following nil)

;; Get hostname
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

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

;; Load path etc.

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/elpa-to-submit"))
;; Add every subdirectory of ~/.emacs.d/site-lisp to the load path
(dolist
    (elt
     (delq nil
           (mapcar
            (lambda (filedesc)
              (let ((filename (car filedesc))
                    (isdirectory (cadr filedesc)))
                (if (and isdirectory
                         (not (integerp (string-match "/\\.+$" filename))))
                    filename
                  nil)))
            (directory-files-and-attributes
             (concat dotfiles-dir "/site-lisp") t))))
  (add-to-list 'load-path elt))

;; First of all, load proxy settings etc we might need depending on hostname.
(setq system-specific-immediate-config (concat dotfiles-dir system-name "-first.el"))
(if (file-exists-p system-specific-immediate-config) (load system-specific-immediate-config))

;; Turn off mouse interface early in startup to avoid momentary display
;; You really don't need these; trust me.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Load CEDET early to make sure we override the packaged version
(load "~/.emacs.d/bodil-cedet")

;; If on a Debian based system, let's run debian-startup
(if (and (file-exists-p "/usr/share/emacs/site-lisp/debian-startup.el")
         (not (functionp 'debian-startup)))
    (progn (add-to-list 'load-path "/usr/share/emacs/site-lisp")
           (load-file "/usr/share/emacs/site-lisp/debian-startup.el")
           (debian-run-directories "/etc/emacs/site-start.d")))

(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; ELPA
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(require 'starter-kit-elpa)

;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)

;; Load up starter kit customizations

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-lisp)

;; You can keep system- or user-specific customizations here
(setq system-specific-config (concat dotfiles-dir system-name ".el")
      user-specific-config (concat dotfiles-dir user-login-name ".el")
      user-specific-dir (concat dotfiles-dir user-login-name))
(add-to-list 'load-path user-specific-dir)

(if (file-exists-p system-specific-config) (load system-specific-config))
(if (file-exists-p user-specific-config) (load user-specific-config))
(if (file-exists-p user-specific-dir)
  (mapc #'load (directory-files user-specific-dir nil ".*el$")))

;; Load my scripts
(load "~/.emacs.d/bodil-theme")
(load "~/.emacs.d/bodil-lisp")
(load "~/.emacs.d/bodil-haskell")
(load "~/.emacs.d/bodil-python")
(load "~/.emacs.d/bodil-java")
(load "~/.emacs.d/bodil-js")
(load "~/.emacs.d/bodil-c")
(load "~/.emacs.d/bodil-w3m")
(load "~/.emacs.d/bodil-git")
(load "~/.emacs.d/bodil-misc")
(load "~/.emacs.d/bodil-bindings")
(load "~/.emacs.d/bodil-vars")

;; Load X-specific scripts
(Xlaunch (load "~/.emacs.d/bodil-x"))

(regen-autoloads)
(load custom-file 'noerror)

;;; init.el ends here
