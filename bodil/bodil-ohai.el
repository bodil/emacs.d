;;; -*- lexical-binding: t -*-
;;; bodil-ohai.el -- A suitable opening screen.

(package-require 'dash)
(package-require 's)
(package-require 'f)
(package-require 'web)

(require 'dash)
(require 's)
(require 'f)
(require 'web)

(defun ohai--dilbert-url (cb)
  (web-http-get
   (lambda (con header data)
     (-if-let (url (s-match "/dyn/str_strip/[0-9/]+.strip.gif" data))
         (funcall cb (s-concat "http://dilbert.com" (car url)))
       (funcall cb "http://placepuppy.it/600/400")))
   :url "http://dilbert.com/"))

(defun ohai--load-url (url cb)
  (web-http-get
   (lambda (con header data)
     (let ((filename (make-temp-file
                      "loadurl" nil
                      (s-concat "." (ohai--guess-extension header url)))))
       (f-write-bytes (string-to-unibyte data) filename)
       (funcall cb filename)))
   :url url))

(defun ohai--display-image (img)
  (with-current-buffer (get-buffer "*scratch*")
    (insert-image img "ohai")
    (insert "\n\n")))

(defun ohai--guess-extension (header url)
  (let ((type (gethash 'content-type header))
        (ext (f-ext url)))
    (if header
        (cond
         ((string= type "image/gif") "gif")
         ((string= type "image/jpeg") "jpg")
         ((string= type "image/png") "png")
         ((string= type "image/svg") "svg"))
      ext)))

(defun ohai-dilbert ()
  (ohai--dilbert-url
   (lambda (url)
     (ohai--load-url url (lambda (imgpath) (ohai--display-image (create-image imgpath)))))))

;; (setq max-image-size nil)
(when (not on-console)
  (setq initial-buffer-choice (lambda () (ohai-dilbert) t)))

(provide 'bodil-ohai)
