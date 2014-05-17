;;; bodil-chat.el -- Jabber and other IM systems

(package-require 'jabber)

(require 'dash)

;; Thank you @puffnfresh https://gist.github.com/puffnfresh/4002033

(defvar hipchat-gid "62224")
(defvar hipchat-uid "553304")
(defvar hipchat-nickname "Bodil Stokke")
(defvar hipchat-autojoin '("future_ad_labs" "development"))

;; To join HipChat rooms easily
(defun hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join
   (jabber-read-account)
   (concat hipchat-gid "_" room "@conf.hipchat.com")
   hipchat-nickname
   t))

;; Mention nicknames in a way that HipChat clients will pickup
(defun hipchat-mention (nickname)
  (interactive
   (list (jabber-muc-read-nickname jabber-group "Nickname: ")))
  (insert (concat "@\"" nickname "\" ")))

(defun hipchat-connect ()
  (interactive)
  (jabber-connect (concat hipchat-gid "_" hipchat-uid) "chat.hipchat.com" nil)
  (-each hipchat-autojoin (lambda (room) (hipchat-join room))))

(provide 'bodil-chat)
