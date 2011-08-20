;;; bodil-js.el -- Javascript configuration

;; Configure js2-mode
(require 'js2-mode)
(setq js2-bounce-indent-p t)
(setq js2-cleanup-whitespace t)
(setq js2-enter-indents-newline t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))

;; Setup slime-js
(require 'slime)
(slime-setup '(slime-fancy slime-repl slime-js))
(add-hook 'js2-mode-hook
          (lambda ()
            (define-key js2-mode-map [f5] 'slime-js-reload)
            (slime-js-minor-mode 1)))
(add-hook 'css-mode-hook
          (lambda ()
            (define-key css-mode-map "\M-\C-x" 'slime-js-refresh-css)))

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
;; (require 'flymake-lintnode)
;; (add-hook 'js2-mode-hook
;;           (lambda () flymake-mode 1))
;; (setq lintnode-location (expand-file-name "~/.emacs.d/site-lisp/lintnode"))
;; (lintnode-start)

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

