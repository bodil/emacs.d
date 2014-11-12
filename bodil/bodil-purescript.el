;;; bodil-purescript.el -- PureScript Is Not Haskell

(require 'bodil-haskell)

(define-derived-mode purescript-mode haskell-mode "PureScript"
  "Major mode for PureScript")
(add-to-list 'auto-mode-alist (cons "\\.purs\\'" 'purescript-mode))
(setup-haskell-arrows 'purescript-mode purescript-mode-map)

(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker pulp
       "Use Pulp to flycheck PureScript code."
       :command ("pulp" "build" "--monochrome")
       :error-patterns
       ((error line-start
               (or (and "Error at " (file-name)    " line " line ", column " column ":"
                        (zero-or-more " "))
                   (and "\"" (file-name) "\" (line " line ", column " column "):"))
               (message (one-or-more (not (in "*"))))
               line-end))
       :modes purescript-mode)
     (add-to-list 'flycheck-checkers 'pulp)))

;; Generate a likely module name from the current file path
(package-require 'f)
(package-require 's)
(require 'f)
(require 's)
(defun purescript-module-name-from-current-buffer-file ()
  (let ((path (f-split (f-relative
                        (f-base (buffer-file-name))
                        (f-join (projectile-project-root) "src"))))
        (testpath (f-split (f-relative
                            (f-base (buffer-file-name))
                            (f-join (projectile-project-root) "test")))))
    (if (string= ".." (car path))
        (if (string= ".." (car testpath)) "Main" (s-join "." (cons "Test" testpath)))
        (s-join "." path))))

(provide 'bodil-purescript)
