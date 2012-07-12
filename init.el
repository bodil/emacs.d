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

;; ELPA
(setq package-user-dir (concat dotfiles-dir "elpa"))
(require 'package)
(dolist (source '(("melpa" . "http://melpa.milkbox.net/packages/")
                  ("marmalade" . "http://marmalade-repo.org/packages/")
                  ("elpa" . "http://tromey.com/elpa/")))
  (add-to-list 'package-archives source t))
(package-initialize)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name (concat dotfiles-dir "bak")))))

;; Macro for X specific code
(defmacro Xlaunch (&rest x)
  (list 'if (eq window-system 'x) (cons 'progn x)))

;; Now load other things
(dolist (file '("bodil-defuns.el"
                "bodil-session.el"
                "bodil-theme.el"
                "bodil-editing.el"
                "bodil-complete.el"
                "bodil-ido.el"
                "bodil-lisp.el"
                "bodil-js.el"
                "bodil-markup.el"
                "bodil-magit.el"
                "bodil-codestyle.el"))
  (load (concat dotfiles-dir file)))
(Xlaunch (load (concat dotfiles-dir "bodil-x11.el")))

;; Load custom settings
(load custom-file 'noerror)
