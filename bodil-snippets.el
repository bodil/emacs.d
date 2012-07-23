;;; bodil-snippets.el --- Yasnippets

(require 'yasnippet)
(setq yas/root-directory (concat dotfiles-dir "snippets"))
(yas/global-mode 1)

;; Load Magnar's snippet helpers
(require 'buster-snippet-helpers)
