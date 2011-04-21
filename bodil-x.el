;; bodil-x.el -- X-specific stuff, only loaded if X is available

(require 'dbus)
(defun dbus-send-desktop-notification (summary body icon timeout)
  "call notification-daemon method METHOD with ARGS over dbus"
  (dbus-call-method
    :session                        ; use the session (not system) bus
    "org.freedesktop.Notifications" ; service name
    "/org/freedesktop/Notifications"   ; path name
    "org.freedesktop.Notifications" "Notify" ; Method
    "emacs"
    0
    icon
    summary
    body
    '(:array)
    '(:array (:dict-entry "x-canonical-append" (:variant "allowed")))
    ':int32 timeout))

