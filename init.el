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
(load-library "iso-transl")

;; Automatically save buffers before compiling
(setq compilation-ask-about-save nil)

;; Setup exec-path and PATH environment variable, in case shell has failed to do so
(let ((paths (mapcar (lambda (i) (concat (getenv "HOME") "/" i))
                     '("bin" "emacs/bin" ".cabal/bin" "node/bin"))))
  (setenv "PATH" (apply 'concat
                        (append (mapcar (lambda (i) (concat i ":")) paths)
                                (list (getenv "PATH")))))
  (dolist (path paths) (when (file-directory-p path)
                         (add-to-list 'exec-path path))))

;; Always ask for y/n keypress instead of typing out 'yes' or 'no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Get hostname
(setq hostname (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" (with-output-to-string (call-process "hostname" nil standard-output))))

;; Add .emacs.d/bodil to load-path
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path (concat dotfiles-dir "bodil"))

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
                  ;; ("marmalade" . "http://marmalade-repo.org/packages/")
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

(setq bodil-pkg-full
      '(bodil-defuns
        bodil-session
        bodil-theme
        bodil-powerline
        bodil-nav
        bodil-editing
        bodil-snippets
        bodil-complete
        bodil-ido
        bodil-lisp
        ;; bodil-paredit
        bodil-smartparens
        bodil-js
        bodil-markup
        ;; bodil-c
        bodil-markdown
        ;; bodil-python
        ;; bodil-erlang
        bodil-haskell
        bodil-magit
        bodil-terminal
        bodil-twitter
        bodil-orgmode
        bodil-flycheck
        bodil-codestyle
        bodil-dired
        bodil-helm
        bodil-project
        bodil-misc-modes
        bodil-multimodes
        bodil-revealjs
        bodil-chat
        bodil-eshell
        ;; bodil-mail
        ))

;; Now load other things
(dolist (file bodil-pkg-full)
  (require file))
(Xlaunch (require 'bodil-x11))

;; Load custom settings
(load custom-file 'noerror)
