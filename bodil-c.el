;;; bodil-c.el --- C and C++ and other horrible things

;; QML mode
(autoload 'qml-mode "qml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.qml$" . qml-mode))

(provide 'bodil-c)
