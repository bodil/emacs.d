;;; bodil-erlang.el --- <3 Joe Armstrong <3

(package-require 'erlang)
(add-hook 'erlang-mode-hook
          (lambda ()
            (define-key erlang-mode-map (kbd "C-\\")
              'erlang-complete-tag)))

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            (define-key erlang-shell-mode-map (kbd "C-\\")
              'erlang-complete-tag)))

(provide 'bodil-erlang)
