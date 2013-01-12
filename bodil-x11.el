;; x11.el -- X-specific stuff, only loaded if X is available

;; Maximise the Emacs window
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
(toggle-fullscreen)

;; Define a function for making desktop notifications
(require 'dbus)
(defun dbus-send-desktop-notification (summary body icon timeout)
  "call notification-daemon method METHOD with ARGS over dbus"
  (dbus-call-method
    :session                        ; use the session (not system) bus
    "org.freedesktop.Notifications" ; service name
    "/org/freedesktop/Notifications"   ; path name
    "org.freedesktop.Notifications" "Notify" ; Method
    "emacs"
    0 icon summary body
    '(:array)
    '(:array (:dict-entry "x-canonical-append" (:variant "allowed")))
    ':int32 timeout))

(provide 'bodil-x11)
