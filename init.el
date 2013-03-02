;;; init.el --- Through victory, my chains are broken

;; When I was a child, I spake as a child,
;; I understood as a child, I thought as a child:
;; but when I became a man, I put away childish things.
;;   -- 1 Corinthians, 13:11
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Always ALWAYS use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Get hostname
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

;; Add .emacs.d to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)

;; Add every subdirectory of ~/.emacs.d/site-lisp to the load path
(dolist
    (project (directory-files (concat dotfiles-dir "site-lisp") t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Set paths to custom.el and loaddefs.el
(setq autoload-file (concat dotfiles-dir "loaddefs.el"))
(setq custom-file (concat dotfiles-dir "custom.el"))

;; Detect online status, from ESK

(require 'cl)
(defun esk-online? ()
  (if (and (functionp 'network-interface-list)
           (network-interface-list))
      (some (lambda (iface) (unless (equal "lo" (car iface))
                         (member 'up (first (last (network-interface-info
                                                   (car iface)))))))
            (network-interface-list))
    t))

;; ELPA
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)
(when (esk-online?)
  (unless package-archive-contents (package-refresh-contents)))

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))

;; Macro for X specific code
(defmacro Xlaunch (&rest x)
  (list 'if (eq window-system 'x) (cons 'progn x)))

;; Define subpackages by platform

(setq bodil-pkg-android
      '(bodil-defuns
        bodil-session
        bodil-editing
        bodil-snippets
        bodil-ido
        bodil-lisp
        bodil-paredit
        bodil-magit
        bodil-terminal
        bodil-orgmode
        bodil-codestyle
        bodil-dired
        bodil-helm))

(setq bodil-pkg-full
      '(bodil-defuns
        bodil-session
        bodil-theme
        bodil-editing
        bodil-snippets
        bodil-complete
        bodil-ido
        bodil-lisp
        bodil-paredit
        bodil-js
        bodil-markup
        bodil-c
        bodil-markdown
        bodil-python
        bodil-erlang
        bodil-magit
        bodil-terminal
        bodil-twitter
        bodil-orgmode
        bodil-flycheck
        bodil-codestyle
        bodil-dired
        bodil-helm))

;; Now load other things
(dolist (file (if (string-prefix-p "android" hostname) bodil-pkg-android bodil-pkg-full))
  (require file))
(Xlaunch (require 'bodil-x11))

;; Load custom settings
(load custom-file 'noerror)
