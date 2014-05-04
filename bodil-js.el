;;; bodil-js.el -- Javascript and friends

;; js2-mode
(package-require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Edit ejs as js
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . js2-mode))

(font-lock-add-keywords
 'js2-mode `(("\\<\\(function\\) *("
              (0 (progn (compose-region (match-beginning 1)
                                        (match-end 1) "\u0192") nil)))))

(setq-default js2-mode-indent-ignore-first-tab t)
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-strict-var-hides-function-arg-warning nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning nil)
(setq-default js2-strict-cond-assign-warning nil)
(setq-default js2-strict-var-redeclaration-warning nil)
(setq-default js2-global-externs
      '("module" "require" "__dirname" "process" "console" "define"
        "JSON" "$" "_" "Backbone" "buster" "sinon" "moment" "_gaq"
        "Zenbox" "Mousetrap" "Comoyo"))

;; Don't override global M-j keybinding (join lines)
(eval-after-load "js2-mode"
  '(define-key js2-mode-map (kbd "M-j") nil))

;; Use plain old js-mode for JSON, js2-mode doth protest too much
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Enable tern-mode for js2-mode
(add-to-list 'load-path (concat dotfiles-dir "site-lisp/tern/emacs"))
(autoload 'tern-mode "tern" nil t)
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))

;; js2-refactor
(package-require 'js2-refactor)
(eval-after-load "js2-mode"
  '(progn
     (js2r-add-keybindings-with-prefix "C-c C-m")
     (define-key js2-mode-map (kbd "C-c C-g") 'js2r-add-to-globals-annotation)))

;; Skewer
(package-require 'skewer-mode)
(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

(eval-after-load "skewer-mode"
  '(define-key skewer-mode-map (kbd "C-c C-l")
     (lambda () (interactive)
       (save-buffer)
       (skewer-eval "setTimeout(function(){window.location.reload();}, 0);"))))

;;; Coffeescript
(package-require 'coffee-mode)

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


;;; Roy

(package-require 'roy-mode)
(add-to-list 'auto-mode-alist '("\\.roy$" . roy-mode))


;;; TypeScript

(package-require 'tss)
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(eval-after-load "typescript"
'(define-key typescript-mode-map (kbd "C-c C-k")
   (lambda () (interactive)
     (projectile-with-default-dir (projectile-project-root)
       (compile "npm -s run compile")))))
(add-to-list 'compilation-error-regexp-alist
             '("^\\(.+?\\)(\\([[:digit:]]+\\),\\([[:digit:]]+\\)): \\(.*\\)$"
               1 2 3 nil 1))

(eval-after-load "typescript"
'(define-key typescript-mode-map (kbd "C-c C-l")
   (lambda () (interactive)
     (projectile-with-default-dir (projectile-project-root)
       (eshell-command "npm start")))))

(require 'bodil-flycheck)
(require 'flycheck)
(flycheck-define-checker tslint
  "Use tslint to flycheck TypeScript code."
  :command ("tslint"
            "-f" source
            "-c" (eval (projectile-expand-root "tslint.json"))
            "-t" "prose")
  :error-patterns ((warning (file-name) "[" line ", " column "]: " (message)))
  :modes typescript-mode)
(add-to-list 'flycheck-checkers 'tslint)

(provide 'bodil-js)
