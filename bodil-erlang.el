;;; bodil-erlang.el --- <3 Joe Armstrong <3

(autoload 'erlang-mode "erlang" nil t)
(add-hook 'erlang-mode-hook
          (lambda ()
            (define-key erlang-mode-map (kbd "C-\\")
              'erlang-complete-tag)))

(autoload 'erlang-shell-mode "erlang" nil t)
(add-hook 'erlang-shell-mode-hook
          (lambda ()
            (define-key erlang-shell-mode-map (kbd "C-\\")
              'erlang-complete-tag)))
