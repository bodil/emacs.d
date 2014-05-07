;;; bodil-eshell.el -- Eshell setup

(package-require 'f)
(require 'f)

(setq eshell-visual-commands
      '("less" "tmux" "htop" "top"))

(setq eshell-visual-subcommands
      '(("git" "log" "l" "diff" "show")))

;; Prompt from http://www.emacswiki.org/emacs/EshellPrompt
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun eshell/abbr-pwd ()
  (let ((home (getenv "HOME"))
        (path (eshell/pwd)))
    (if (f-ancestor-of? home path)
        (concat "~/" (f-relative path home))
      path)))

(defun shk-eshell-prompt ()
  (let ((header-bg "#161616"))
    (concat
     (with-face user-login-name :foreground "#d75faf")
     (with-face (concat "@" hostname) :foreground "#8700af")
     " "
     (with-face (eshell/abbr-pwd) :foreground "#008700")
     (if (= (user-uid) 0)
         (with-face "#" :foreground "red")
       (with-face "$" :foreground "#2345ba"))
     " ")))

(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-regexp "^[^#$\n]+[#$] ")

(setq eshell-cmpl-cycle-completions nil)

(provide 'bodil-eshell)
