;;; js.el -- Javascript and friends

;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(font-lock-add-keywords
 'js2-mode `(("\\<\\(function\\) *("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192") nil)))))

(setq js2-auto-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline t)
(setq js2-indent-on-enter-key t)
(setq js2-mirror-mode nil)
(setq js2-mode-indent-ignore-first-tab t)
(setq js2-global-externs
      '("module" "require" "__dirname" "process" "console" "define"
        "JSON" "$" "_"))

;; Use plain old js-mode for JSON, js2-mode doth protest too much
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Highlight variables in scope
;; http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode/js2-highlight-vars-mode
(autoload 'js2-highlight-vars-mode "js2-highlight-vars" nil t)
(add-hook 'js2-mode-hook (lambda () (js2-highlight-vars-mode)))

;; Install js2-refactor when js2-mode loads
(eval-after-load "js2-mode"
  '(progn (require 'js2-refactor)
          (define-key js2-mode-map (kbd "C-c C-r") 'js2r-rename-var)))


;;; Coffeescript
(autoload 'coffee-mode "coffee-mode" nil t)

(add-hook 'coffee-mode-hook
          (lambda ()
            (define-key coffee-mode-map (kbd "M-r") 'coffee-compile-buffer)
            (define-key coffee-mode-map (kbd "M-R") 'coffee-compile-region)
            (define-key coffee-mode-map (kbd "<tab>") 'coffee-indent)
            (define-key coffee-mode-map (kbd "<backtab>") 'coffee-unindent)))

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("\\.cson$" . coffee-mode))

;; Use js2-mode for displaying compiled CS
(setq coffee-js-mode 'js2-mode)

;; Patch coffee-mode so coffee-compile-region pops up a new
;; non-focused window instead of replacing the current buffer.
(eval-after-load "coffee-mode"
  '(defun coffee-compile-region (start end)
     "Compiles a region and displays the JS in another buffer."
     (interactive "r")
     (let ((buffer (get-buffer coffee-compiled-buffer-name)))
       (when buffer (kill-buffer buffer)))
     (call-process-region start end coffee-command nil
                          (get-buffer-create coffee-compiled-buffer-name) nil "-s" "-p" "--bare")
     (let ((buffer (get-buffer coffee-compiled-buffer-name)))
       (with-current-buffer buffer
         (funcall coffee-js-mode)
         (goto-char (point-min)))
       (display-buffer buffer))))

;; Handle backtabs and indenting regions
(defun coffee-indent-block ()
  (shift-region coffee-tab-width)
  (setq deactivate-mark nil))

(defun coffee-unindent-block ()
  (shift-region (- coffee-tab-width))
  (setq deactivate-mark nil))

(defun coffee-indent ()
  (interactive)
  (if (and (boundp 'ac-trigger-command-p) (ac-trigger-command-p last-command))
      (auto-complete)
    (if mark-active
        (coffee-indent-block)
      (indent-for-tab-command))))

(defun coffee-unindent ()
  (interactive)
  (if mark-active
      (coffee-unindent-block)
    (progn
      (indent-line-to (- (current-indentation) coffee-tab-width)))))


;; Roy

(autoload 'roy-mode "roy-mode" nil t)
(autoload 'roy-repl "roy-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.roy$" . roy-mode))
