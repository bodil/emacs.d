;;; bodil-c.el -- C/C++ setup

;; Get indentation right
(setq c-default-style "linux"
      c-basic-offset 4)

;; C mode hook
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Enable flymake
            ;(flymake-mode t)
            ;; Bind a key for switching between header and implementation files
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)
            ;; A key for jumping to a definition
            (local-set-key (kbd "M-.") 'semantic-complete-jump)))

(require 'cc-mode)

;; Semantic
(require 'semantic/bovine/c)

;; Qt setup
(add-to-list 'auto-mode-alist '("/usr/include/qt4" . c++-mode))
(semantic-add-system-include "/usr/include/qt4" 'c++-mode)
(add-to-list 'semantic-lex-c-preprocessor-symbol-file "/usr/include/qt4/Qt/qconfig.h")
(add-to-list 'semantic-lex-c-preprocessor-symbol-file "/usr/include/qt4/Qt/qconfig.h")
