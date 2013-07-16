;;; bodil-snippets.el --- Yasnippets

(package-require 'popup)
(package-require 'yasnippet)
(require 'yasnippet)
(setq yas/root-directory (concat dotfiles-dir "snippets"))
(yas/global-mode 1)

;; Load Magnar's snippet helpers
(require 'snippet-helpers)
(require 'buster-snippet-helpers)

;; Packaged Clojure snippets
(package-require 'clojure-snippets)
(clojure-snippets-initialize)

(provide 'bodil-snippets)
