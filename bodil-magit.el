;;; magit.el -- Git better

(package-require 'magit)

(global-set-key (kbd "C-x g") 'magit-status)

;; Automatically refresh changes in status buffer
(package-require 'magit-inotify)

;; Github integration
(package-require 'gist)

(provide 'bodil-magit)
