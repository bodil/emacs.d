;;; -*- lexical-binding: t -*-
;;; bodil-ohai.el -- A suitable opening screen.

(package-require 'dash)
(package-require 's)
(package-require 'web)

(require 'dash)
(require 's)
(require 'web)

(defun ohai--load-and-map (url map-fn cb)
  (web-http-get
   (lambda (con header data)
     (funcall cb (funcall map-fn data header)))
   :url url))

(defun ohai--load-and-match (url regex format-str cb)
  (ohai--load-and-map
   url
   (lambda (data header)
     (-when-let (m (s-match regex data))
         (s-format format-str 'elt m)))
   cb))

(defun ohai--pick-first (fetchers cb)
  (-if-let (fetcher (car fetchers))
      (if (stringp fetcher) (funcall cb fetcher)
        (funcall fetcher
                 (lambda (url)
                   (if url (funcall cb url)
                     (ohai--pick-first (cdr fetchers) cb)))))
    (funcall cb nil)))

(defun ohai--load-image (url cb)
  (web-http-get
   (lambda (con header data)
     (funcall cb (create-image (string-to-unibyte data) nil t)))
   :url url))

(defun ohai--display-image (img)
  (with-current-buffer (get-buffer "*scratch*")
    (insert-image img "ohai")
    (insert "\n\n")))

(defun ohai-dilbert (cb)
  (ohai--load-and-match "http://dilbert.com/"
                        "/dyn/str_strip/[0-9/]+\.strip\\(\.sunday\\)?\.gif"
                        "http://dilbert.com$0" cb))

(defun ohai-imgur (search-term)
  (lambda (cb)
    (ohai--load-and-match
     (s-concat "http://imgur.com/search/score/day?q_size_px=med&q_size_mpx=med&q="
               (url-hexify-string search-term))
     "<div id=\"\\([0-9a-zA-Z]+\\)\" class=\"post\""
     "http://i.imgur.com/$1.jpg" cb)))

(defun ohai-placepuppy (cb)
  (cb "http://placepuppy.it/600/400"))

(setq-default
 ohai-sources
 (list 'ohai-dilbert
       (ohai-imgur "puppy")
       'ohai-placepuppy))

(defun ohai (&optional sources &optional cb)
  (interactive)
  (ohai--pick-first
   (or sources ohai-sources)
   (lambda (url)
     (ohai--load-image
      url (lambda (img)
            (ohai--display-image img)
            (when cb (funcall cb)))))))

(defun ohai-go-dilbert-and-puppies ()
  (ohai (list 'ohai-dilbert)
        (lambda ()
          (ohai (list (ohai-imgur "puppy")
                      'ohai-placepuppy)
                (lambda ()
                  (with-current-buffer (get-buffer "*scratch*")
                    (beginning-of-buffer)
                    (line-move 4))))))
  t)

(when (not on-console)
  (setq initial-buffer-choice 'ohai-go-dilbert-and-puppies))

(provide 'bodil-ohai)
