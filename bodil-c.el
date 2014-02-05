;;; bodil-c.el --- C and C++ and other horrible things

;; qmake-mode
(autoload 'qmake-mode "qmake" nil t)
(add-to-list 'auto-mode-alist '("\\.pro$" . qmake-mode))

;; Qt5 environment
(require 'semantic/bovine/c)
(setq qt-include-directory "/usr/include/qt")
;; (add-to-list 'auto-mode-alist (cons qt-source-directory 'c++-mode))
;; (add-to-list 'cc-search-directories qt-source-directory)

(setq cc-search-directories ())
(add-to-list 'auto-mode-alist (cons qt-include-directory 'c++-mode))
(dolist (file (directory-files qt-include-directory))
  (let ((path (expand-file-name file qt-include-directory)))
    (when (and (file-directory-p path)
               (not (or (equal file ".") (equal file ".."))))
      (progn
        (semantic-add-system-include path 'c++-mode)
        (add-to-list 'cc-search-directories path)))))

(dolist (file (list "QtCore/qconfig.h" "QtCore/qconfig-dist.h" "QtCore/qconfig-large.h"
                    "QtCore/qconfig-medium.h" "QtCore/qconfig-minimal.h" "QtCore/qconfig-small.h"
                    "QtCore/qglobal.h"))
  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (expand-file-name file qt-include-directory)))

(define-key c++-mode-map (kbd "C-c C-o") 'ff-find-other-file)
(define-key c++-mode-map (kbd "C-c C-k")
  (lambda () (interactive) (compile "make -k")))

(eval-after-load "qmake"
  '(define-key qmake-mode-map (kbd "C-c C-k")
     (lambda () (interactive) (compile "qmake; and make -k"))))

(provide 'bodil-c)
