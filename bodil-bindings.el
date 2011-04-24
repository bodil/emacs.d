;;; bodil-bindings.el -- Keybindings

;; Enable CUA selection mode; sorry, it stuck.
(cua-selection-mode t)

;; Enable whole-line-or-region
(require 'whole-line-or-region)
(whole-line-or-region-mode t)

;; Enable smart-tab mode
(require 'smart-tab)
(global-smart-tab-mode t)

;; Keybindings for cycling buffers in tab bar order
(global-set-key (kbd "C-<next>") 'tabbar-forward-tab)
(global-set-key (kbd "C-<prior>") 'tabbar-backward-tab)

;; And for cycling through tabbar groups
(global-set-key (kbd "C-M-<next>") 'tabbar-forward-group)
(global-set-key (kbd "C-M-<prior>") 'tabbar-backward-group)

;; Keybindings for cycling buffers in last-used order using iflipb
(require 'iflipb)
(global-set-key (kbd "C-<end>") 'iflipb-next-buffer)
(global-set-key (kbd "C-<home>") 'iflipb-previous-buffer)

;; Redefine undo key
(global-set-key (kbd "C-z") 'undo)

;; Keybinding for replace-regexp
(global-set-key (kbd "C-c r") 'replace-regexp)

;; Handy SLIME keybinding
(global-set-key (kbd "C-c s") (lambda () (interactive) (slime-connect "127.0.0.1" "4005")))

;; Open a shell
(global-set-key (kbd "C-x m") (lambda () (interactive) (multi-term)))

;; Bind a key that toggles to the Shells group and back,
;; opening a shell if none are open already.
(defun tabbar-select-tabset (name)
  "Switch to the named tabset."
  (let ((starting-tabset (tabbar-current-tabset)))
     (tabbar-forward-group)
     (while (and (not (string= (tabbar-current-tabset) name))
                 (not (eq starting-tabset (tabbar-current-tabset))))
       (tabbar-forward-group))
     (not (eq starting-tabset (tabbar-current-tabset)))))
(defun tabbar-toggle-tabset-or-execute (name failfunc)
  (if (string= (tabbar-current-tabset) name)
      (when (boundp 'tabbar-previous-tabset)
        (tabbar-select-tabset tabbar-previous-tabset))
    (progn
      (setq tabbar-previous-tabset (tabbar-current-tabset))
      (unless (tabbar-select-tabset name)
        (funcall failfunc)))))
(global-set-key (kbd "C-<f12>")
                (lambda () (interactive)
                  (tabbar-toggle-tabset-or-execute
                   "Shells"
                   (lambda () (multi-term)))))

;; Let's do the same as above for ERC - toggle there and back,
;; logging into our servers if necessary.
(global-set-key (kbd "C-<f11>")
                (lambda () (interactive)
                  (tabbar-toggle-tabset-or-execute
                   "ERC"
                   'irc)))

;; Use smex to provide ido-like interface for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Auto-insert spaces around operators in C-like languages
(defun my-c-mode-common-hook()
  (smart-insert-operator-hook)
  (local-unset-key (kbd "."))
  (local-unset-key (kbd ":"))
  (local-set-key (kbd "*") 'c-electric-star))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Configure keys to move comfortably between ECB and edit window
(global-set-key (kbd "M-<left>") 'ecb-goto-window-ecb-by-smart-selection)
(global-set-key (kbd "M-<right>") 'ecb-goto-window-edit-by-smart-selection)

;; Auto-Complete configuration
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")
(ac-config-default)

