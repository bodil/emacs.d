;;; magit.el -- Git better

(package-require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

;; Github integration
(package-require 'gist)

(provide 'bodil-magit)
