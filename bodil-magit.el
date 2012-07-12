;;; magit.el -- Git better

(autoload 'magit-status "magit.el" nil t)
(global-set-key (kbd "C-x g") 'magit-status)
