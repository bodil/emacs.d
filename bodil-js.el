;;; bodil-js.el -- Javascript configuration

;; js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js-indent-level 4)
(setq js2-auto-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline t)
(setq js2-indent-on-enter-key t)
(setq js2-mirror-mode nil)
(setq js2-mode-indent-ignore-first-tab t)
(setq js2-global-externs '("require" "__dirname" "process" "console"))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; Coffeescript mode
(require 'coffee-mode)
(defun coffee-custom ()
  "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 2)
  (define-key coffee-mode-map (kbd "M-r") 'coffee-compile-buffer)
  (define-key coffee-mode-map (kbd "M-R") 'coffee-compile-region))
(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))
(require 'auto-complete)
(add-to-list 'ac-modes 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.cson$" . coffee-mode))

;; JSLint for Flymake
(require 'flymake)
(defun flymake-jslint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "jslint" (list local-file))))
(setq flymake-err-line-patterns 
      (cons '("^  [[:digit:]]+ \\([[:digit:]]+\\),\\([[:digit:]]+\\): \\(.+\\)$"  
              nil 1 2 3)
            flymake-err-line-patterns))
(add-to-list 'flymake-allowed-file-name-masks
             '("\\.js\\'" flymake-jslint-init))

;; Patch coffee-mode so coffee-compile-region pops up a new
;; non-focused window instead of replacing the current buffer.
(defun coffee-compile-region (start end)
  "Compiles a region and displays the JS in another buffer."
  (interactive "r")

  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      (kill-buffer buffer)))

  (call-process-region start end coffee-command nil
                       (get-buffer-create coffee-compiled-buffer-name)
                       nil
                       "-s" "-p" "--bare")
  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (with-current-buffer buffer
      (funcall coffee-js-mode)
      (goto-char (point-min)))
    (display-buffer buffer)))

;; Setup jade-mode
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'ac-modes 'jade-mode)

;; Activate runlol integration
(require 'runlol)

;; Flymake CS files
(require 'flymake-coffee)
(add-hook 'coffee-mode-hook 'flymake-coffee-load)

