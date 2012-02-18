;;; bodil-c.el -- C/C++ setup

;; Get indentation right
(setq c-default-style "linux"
      c-basic-offset 4)

;; C mode hook
(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Enable flymake
            ;(flymake-mode t)
            ;; Emable semantic
            (semantic-mode 1)
            ;; Bind a key for switching between header and implementation files
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)
            ;; A key for jumping to a definition
            (local-set-key (kbd "M-.") 'semantic-complete-jump)
            ;; Setup auto-complete sources
            (add-to-list 'ac-sources 'ac-source-semantic)))

(require 'cc-mode)

;; Semantic
(require 'semantic/bovine/c)
(global-semanticdb-minor-mode 1)
(global-semantic-idle-summary-mode 1)
(global-semantic-highlight-func-mode 1)
(global-semantic-stickyfunc-mode 1)

;; Configure Qt environment
(setq qt-include-directory "/usr/include/qt4")
(add-to-list 'auto-mode-alist (cons qt-include-directory 'c++-mode))
(dolist (file (directory-files qt-include-directory))
  (let ((path (expand-file-name file qt-include-directory)))
    (when (and (file-directory-p path)
               (not (or (equal file ".") (equal file ".."))))
      (progn
        (semantic-add-system-include path 'c++-mode)
        ; (add-to-list 'cc-search-directories path)
        ))))
(dolist (file (list "QtCore/qconfig.h" "QtCore/qconfig-dist.h" "QtCore/qconfig-large.h"
		    "QtCore/qconfig-medium.h" "QtCore/qconfig-minimal.h" "QtCore/qconfig-small.h"
		    "QtCore/qglobal.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (expand-file-name file qt-include-directory)))
(require 'qmake-mode)

