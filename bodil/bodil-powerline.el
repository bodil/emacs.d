;;; bodil-powerline-el -- Modeline fashion

(package-require 'powerline)
(require 'bodil-flycheck)
(require 'flycheck)

(defun powerline-nyan-theme ()
  (interactive)
  (setq-default
   mode-line-format
   '("%e"
     (:eval
      (let* ((active (powerline-selected-window-active))
         (mode-line (if active 'mode-line 'mode-line-inactive))
         (face1 (if active 'powerline-active1
              'powerline-inactive1))
         (face2 (if active 'powerline-active2
              'powerline-inactive2))
         (error-face '((t . (:foreground "#f44"))))
         (warning-face '((t . (:foreground "#f94"))))
         (info-face '((t . (:foreground "#ff4"))))
         (all-clear-face '((t . (:foreground "#4f4"))))
         (separator-left
          (intern (format "powerline-%s-%s"
                  powerline-default-separator
                  (car powerline-default-separator-dir))))
         (separator-right
          (intern (format "powerline-%s-%s"
                  powerline-default-separator
                  (cdr powerline-default-separator-dir))))
         (lhs (list
           (powerline-raw "%*%*" nil 'l)
           (powerline-buffer-id nil 'l)

           (when (and (boundp 'which-func-mode) which-func-mode)
             (powerline-raw which-func-format nil 'l))

           (powerline-raw " ")
           (funcall separator-left mode-line face1)

           (when (boundp 'erc-modified-channels-object)
             (powerline-raw erc-modified-channels-object
                    face1 'l))

           (powerline-major-mode face1 'l)
           (powerline-process face1)
           (powerline-minor-modes face1 'l)
           (powerline-narrow face1 'l)

           (powerline-raw " " face1)
           (funcall separator-left face1 face2)

           (powerline-vc face2 'r)))
         (rhs (list
           (powerline-raw global-mode-string face2 'r)

           (funcall separator-right face2 face1)

           (powerline-raw "%4l" face1 'l)
           (powerline-raw ":" face1 'l)
           (powerline-raw "%3c" face1 'r)

           (funcall separator-right face1 mode-line)
           (powerline-raw " ")
           (powerline-raw
            (cond ((flycheck-has-current-errors-p 'error)
               "\u26d4")
              ((flycheck-has-current-errors-p 'warning)
               "\u2691")
              ((flycheck-has-current-errors-p 'info)
               "\u2689")
              (t "\u263b"))
            (cond ((flycheck-has-current-errors-p 'error)
               error-face)
              ((flycheck-has-current-errors-p 'warning)
               warning-face)
              ((flycheck-has-current-errors-p 'info)
               info-face)
              (t all-clear-face)) 'r)
           (powerline-raw " ")

           (powerline-raw (nyan-create)))))
    (concat
     (powerline-render lhs)
     (powerline-fill face2 (powerline-width rhs))
     (powerline-render rhs)))))))
;;(powerline-nyan-theme)

(provide 'bodil-powerline)
