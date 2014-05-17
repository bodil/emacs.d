;;; bodil-markdown.el -- Markdown setup

(package-require 'markdown-mode)

(setq-default markdown-command "pandoc -S -s --self-contained -f markdown -t html5 ")

(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

(provide 'bodil-markdown)
