;;; bodil-c.el --- C and C++ and other horrible things

;; Hook auto-complete into clang
(eval-after-load "cc-mode"
  '(progn
     (require 'auto-complete-clang-async)
     (setq clang-complete-executable
           (concat dotfiles-dir "site-lisp/clang-complete-async/clang-complete"))
     (when (not (file-exists-p clang-complete-executable))
       (warn (concat "The clang-complete executable doesn't exist - please run "
                     dotfiles-dir "setup.sh to compile it.")))
     ;; Add Qt4 includes to load path if installed
     (when (file-exists-p "/usr/include/qt4")
       (setq ac-clang-flags
             (mapcar (lambda (f) (concat "-I" f))
                     (directory-files "/usr/include/qt4" t "Qt\\w+"))))))

(add-hook 'c++-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-clang-async)
            (launch-completion-proc)))
