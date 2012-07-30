;;; bodil-markdown.el -- Markdown setup

(when (not (package-installed-p 'markdown-mode))
  (package-install 'markdown-mode))

(setq-default markdown-command "pandoc -f markdown -t html5 ")

(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
