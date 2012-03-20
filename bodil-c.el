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

