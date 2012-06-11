;;; bodil-java.el -- Java configuration

;; Fix annotation indentation
(require 'java-mode-indent-annotations)

;; Hook
(add-hook 'java-mode-hook
          (lambda () (java-mode-indent-annotations-setup)))

;; Gradle setup
(require 'gradle)
(defun gradle--with-git-root (func)
  (let ((default-directory (git-current-root)))
    (funcall func)))
(setq gradle-with-project-root-func 'gradle--with-git-root)

;; CEDET/Malabar setup
;; (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
;;                                   global-semanticdb-minor-mode
;;                                   global-semantic-idle-summary-mode
;;                                   global-semantic-mru-bookmark-mode))
;; (semantic-mode 1)
;; (require 'malabar-mode)
;; (setq malabar-groovy-lib-dir "~/.emacs.d/site-lisp/malabar/lib")
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; Make Malabar's autoimport behave more like Eclipse
;; (defun malabar-eclipse-import ()
;;   "Eclipse style import handling."
;;   (interactive)
;;   (malabar-import-all)
;;   (malabar-import-group-imports))
;; (add-hook 'malabar-mode-hook
;;           (lambda () (local-set-key (kbd "C-c C-v z") 'malabar-eclipse-import)))

;; Autocompile on save
;; (add-hook 'malabar-mode-hook
;;           (lambda () (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)))

;; Setup semantic sources for auto-complete
;; (add-hook 'malabar-mode-hook
;;           (lambda () (setq ac-sources '(ac-source-semantic ac-source-yasnippet))))
