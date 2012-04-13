;;; bodil-bindings.el -- Keybindings

;; Enable CUA selection mode; sorry, it stuck.
(cua-selection-mode t)

;; Keybindings for cycling buffers in last-used order using iflipb
(require 'iflipb)
(global-set-key (kbd "C-<next>") 'iflipb-next-buffer)
(global-set-key (kbd "C-<prior>") 'iflipb-previous-buffer)
(global-set-key (kbd "<XF86Forward>") 'iflipb-next-buffer)
(global-set-key (kbd "<XF86Back>") 'iflipb-previous-buffer)

;; Always newline-and-indent
(define-key global-map (kbd "RET") 'newline-and-indent)

;; multi-term navigation stuff
(require 'cl)
(defun multi-term-open-or-next ()
  (interactive)
  (if (multi-term-list)
      (multi-term-next)
    (multi-term)))
(global-set-key (kbd "C-x m") 'multi-term-open-or-next)

(defun multi-term-open-or-activate ()
  (interactive)
  (if (multi-term-list)
      (let ((term-buffer (car (multi-term-list))))
        (let ((term-window (get-buffer-window term-buffer 0)))
          (if term-window
              (select-window term-window)
            (set-window-buffer (selected-window) term-buffer))))
    (multi-term)))

(defun multi-term-get-visible ()
  (remove-if-not (lambda (buf) (get-buffer-window buf 0)) (multi-term-list)))

(defun multi-term-global-action (has-focus)
  (interactive)
  (if has-focus
      ;; If Emacs has focus:
      (if (multi-term-get-visible)
          (if (member* (window-buffer (selected-window)) (multi-term-list))
              ;; If the active window contains a term buffer:
              (lower-frame)
            ;; If a term is visible but not active:
            (multi-term-open-or-activate))
        ;; If no terms are visible:
        (multi-term-open-or-activate))
    ;; If Emacs hasn't got focus:
    (progn
      (raise-frame)
      (select-frame-set-input-focus (selected-frame))
      (multi-term-open-or-activate))))

;; Use smex to provide ido-like interface for M-x
(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is the old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Auto-Complete configuration
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/site-lisp/auto-complete/dict")
(ac-config-default)
(setq ac-auto-start nil)
(ac-set-trigger-key "TAB")
(define-key ac-completing-map "\t" 'ac-complete)
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map (kbd "C-h") 'ac-persist-help)
(setq ac-use-quick-help 0.5)

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

;; Use ido everywhere
(require 'ido-ubiquitous)
(setq ido-ubiquitous-enabled t)

;; XF86Calculator invokes eval-expression
(global-set-key (kbd "<XF86Calculator>") 'eval-expression)

;; Autopair-mode for selected major modes
;; Note: lispy modes have paredit for this.
(require 'autopair)
(setq autopair-pair-criteria 'always)
(add-hook 'c-mode-common-hook (lambda () (autopair-mode)))
(add-hook 'python-mode-hook (lambda () (autopair-mode)))
(add-hook 'haskell-mode-hook (lambda () (autopair-mode)))
(add-hook 'ruby-mode-hook (lambda () (autopair-mode)))
(add-hook 'shell-mode-hook (lambda () (autopair-mode)))
(add-hook 'coffee-mode-hook (lambda () (autopair-mode)))
(add-hook 'css-mode-hook (lambda () (autopair-mode)))
(add-hook 'jade-mode-hook (lambda () (autopair-mode)))
(add-hook 'js-mode-hook (lambda () (autopair-mode)))
(add-hook 'js2-mode-hook (lambda () (autopair-mode)))
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

;; Flymake bindings
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(require 'flymake-cursor)

;; ace-jump-mode
(global-set-key (kbd "C-ø") 'ace-jump-mode)
