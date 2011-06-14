;;; bodil-bindings.el -- Keybindings

;; Enable CUA selection mode; sorry, it stuck.
(cua-selection-mode t)

;; Enable whole-line-or-region
;(require 'whole-line-or-region)
;(whole-line-or-region-mode t)

;; Enable smart-tab mode
;(require 'smart-tab)
;(global-smart-tab-mode t)

;; Let's just enforce auto-complete in all modes when using smart-tab
;; (defun smart-tab-call-completion-function ()
;;   (auto-complete))

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
(global-set-key (kbd "<XF86Forward>") 'iflipb-next-buffer)
(global-set-key (kbd "<XF86Back>") 'iflipb-previous-buffer)

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

;; Configure keys to move comfortably between ECB and edit window
(global-set-key (kbd "M-<left>") 'ecb-goto-window-ecb-by-smart-selection)
(global-set-key (kbd "M-<right>") 'ecb-goto-window-edit-by-smart-selection)

;; Auto-Complete configuration
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")
(ac-config-default)
;; (define-key ac-mode-map (kbd "C-SPC") 'auto-complete)

;; Bind Flyspell completion key to M-\
(setq flyspell-auto-correct-binding (kbd "M-\\"))

;; Setup a function to jump to a symbol in the current file
(defun ido-goto-symbol ()
  "Will update the imenu index and then use ido to select a 
   symbol to navigate to"
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
   
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
   
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
   
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))
(global-set-key (kbd "C-t") 'ido-goto-symbol)

;; XF86Calculator invokes eval-expression
(global-set-key (kbd "<XF86Calculator>") 'eval-expression)

;; Autopair-mode for selected major modes
;; Note: lispy modes have paredit for this, and js2-mode does its own thing.
(require 'autopair)
(add-hook 'c-mode-common-hook (lambda () (autopair-mode)))
(add-hook 'python-mode-hook (lambda () (autopair-mode)))
(add-hook 'haskell-mode-hook (lambda () (autopair-mode)))
(add-hook 'ruby-mode-hook (lambda () (autopair-mode)))
(add-hook 'shell-mode-hook (lambda () (autopair-mode)))
;; Keep autopair from interfering with auto-complete
(setq ac-use-overriding-local-map t)

;; Duplicate start of line or region, from http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (duplicate-region)
    (duplicate-start-of-line)))
(defun duplicate-start-of-line ()
  (if (bolp)
      (progn
        (end-of-line)
        (duplicate-start-of-line)
        (beginning-of-line))
    (let ((text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
      (forward-line)
      (push-mark)
      (insert text)
      (open-line 1))))
(defun duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning)
                                 end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))
(global-set-key (kbd "C-M-<down>") 'duplicate-start-of-line-or-region)

;; Smart home key
(defun smart-beginning-of-line ()
  "Move point to first non-whitespace character or beginning-of-line.

Move point to the first non-whitespace character on this line.
If point was already at that position, move point to beginning of line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (and (= oldpos (point))
         (beginning-of-line))))
(global-set-key (kbd "<home>") 'smart-beginning-of-line)
(global-set-key (kbd "C-a") 'smart-beginning-of-line)


