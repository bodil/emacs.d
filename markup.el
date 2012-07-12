;;; markup.el -- HTML and friends

;; Key for renaming tags
(autoload 'rename-sgml-tag "rename-sgml-tag" nil t)
(define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)


