;;; theme.el --- Appearance matters

;; Let's see what we're running on
(setq on-console (null window-system))

;; No splash screen
(setq inhibit-startup-message t)

;; Don't defer screen updates when performing operations
(setq redisplay-dont-pause t)

;; Some X11 setup
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; Show line numbers in buffers
(global-linum-mode t)
(setq linum-format (if on-console "%4d " "%4d"))

;; Show column numbers in modeline
(setq column-number-mode t)

;; Redefine linum-on to ignore terminal buffers, because just turning
;; it off in term-mode-hook doesn't work.
(setq linum-disabled-modes
      '(term-mode slime-repl-mode magit-status-mode help-mode nrepl-mode
                  xwidget-webkit-mode revealjs-mode))
(defun linum-on ()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes))
    (linum-mode 1)))

;; Highlight current line
;(global-hl-line-mode)

;; git-gutter-fringe
(package-require 'git-gutter-fringe)
(require 'git-gutter-fringe)
(global-git-gutter-mode t)
;; (setq git-gutter-fr:side 'right-fringe)

;; Set custom theme path
(setq custom-theme-directory (concat dotfiles-dir "themes"))
(dolist
    (path (directory-files custom-theme-directory t "\\w+"))
  (when (file-directory-p path)
    (add-to-list 'custom-theme-load-path path)))

;; Install themes
;; (package-require 'zenburn-theme)
(package-require 'anti-zenburn-theme)

;; Prepare colour themes
(require 'term)

(defun theme-light ()
  (interactive)
  (load-theme 'anti-zenburn)
  (zenburn-with-color-variables
   (custom-theme-set-faces
    'anti-zenburn

    `(mode-line
      ((,class (:foreground ,zenburn-green+1
                            :background ,zenburn-bg-1
                            :box (:line-width -1 :style released-button)))
       (t :inverse-video t)))
    `(mode-line-buffer-id ((t (:foreground ,zenburn-yellow :weight bold))))
    `(mode-line-inactive
      ((t (:foreground ,zenburn-green-1
                       :background ,zenburn-bg-05
                       :box (:line-width -1 :style released-button)))))
    `(powerline-active1 ((t (:background ,zenburn-bg-05 :inherit mode-line))))
    `(powerline-active2 ((t (:background ,zenburn-bg+2 :inherit mode-line))))
    `(powerline-inactive1 ((t (:background ,zenburn-bg+1 :inherit mode-line-inactive))))
    `(powerline-inactive2 ((t (:background ,zenburn-bg+3 :inherit mode-line-inactive))))))
  (set-face-background 'default "#ffffff")
  (set-face-foreground 'default "#000000")
  (set-face-background 'region "#d4d4d4")
  (set-face-foreground 'region nil)
  (set-face-background 'linum "#f0f0f0")
  (set-face-background 'fringe "#f0f0f0")

  (set-face-foreground 'term-color-black "#ffffff")
  (set-face-foreground 'term-color-red "#f5666d")
  (set-face-foreground 'term-color-green "#3cb64a")
  (set-face-foreground 'term-color-yellow "#ce5c00")
  (set-face-foreground 'term-color-blue "#00578e")
  (set-face-foreground 'term-color-magenta "#d020f0")
  (set-face-foreground 'term-color-cyan "#6799cc")
  (set-face-foreground 'term-color-white "#000000"))

(defun theme-dark ()
  (interactive)
  ;; (load-theme 'bubbleberry t)
  ;; (load-theme 'zenburn t)
  (load-theme 'bclues t)
  ;; (set-face-background 'default "#222")
  ;; (set-face-background 'region "#374186")
  ;; (set-face-background 'fringe "#191919")
  ;; (set-face-background 'hl-line "#191919")
  ;; (set-face-background 'linum nil)
  ;; (set-face-foreground 'linum "#acacfc")
  ;; (set-face-foreground 'powerline-active1 "#aaa")
  ;; (set-face-foreground 'powerline-active2 "#ddd")

  (set-face-foreground 'term-color-black "#3f3f3f")
  (set-face-foreground 'term-color-red "#cc9393")
  (set-face-foreground 'term-color-green "#7f9f7f")
  (set-face-foreground 'term-color-yellow "#f0dfaf")
  (set-face-foreground 'term-color-blue "#8cd0d3")
  (set-face-foreground 'term-color-magenta "#dc8cc3")
  (set-face-foreground 'term-color-cyan "#93e0e3")
  (set-face-foreground 'term-color-white "#dcdccc"))

(theme-dark)

;; Get screen DPI
;; (actually, dots per decimeter)
(defun x11-dpdm ()
  (let ((xrandr
         (with-output-to-string
           (call-process "xrandr" nil standard-output))))
    (string-match "\\(.+\\) connected primary \\(.+\\)x.+ (.+) \\(.+\\)mm x .+mm" xrandr)
    (when (not (match-string 2 xrandr))
      (string-match "\\(.+\\) connected \\(.+\\)x.+ (.+) \\(.+\\)mm x .+mm" xrandr))
    (if (match-string 2 xrandr)
        (let ((pixels (string-to-number (match-string 2 xrandr)))
              (phys (string-to-number (match-string 3 xrandr))))
          (/ (* pixels 100) phys))
      250)))

(defun scale-font-size (font-size)
  (let ((target-dpi 480))
    (/ (* font-size (+ target-dpi (/ (- (x11-dpdm) target-dpi) 4))) target-dpi)))

;; Calculate default font size
(setq default-frame-font-size (scale-font-size 20))
(setq presentation-frame-font-size
      (truncate (* 1.25 default-frame-font-size)))

;; Build font descriptor strings
(defun font-desc (name size)
  (concat "-unknown-" name "-normal-normal-normal-*-"
          (number-to-string size) "-*-*-*-m-0-iso10646-1"))

;; Set default and presentation mode fonts
(defun default-frame-font ()
  (font-desc "Ubuntu Mono" default-frame-font-size))
(defun presentation-frame-font ()
  (font-desc "Ubuntu Mono" presentation-frame-font-size))
(set-frame-font (default-frame-font))

(defun toggle-presentation-mode ()
  (interactive)
  (if (string= (frame-parameter nil 'font) (default-frame-font))
      (progn
        (set-frame-font (presentation-frame-font))
        (theme-light))
    (progn
      (set-frame-font (default-frame-font))
      (theme-dark))))
(global-set-key (kbd "C-<f9>") 'toggle-presentation-mode)

(defun presentation-mode-increase-font-size ()
  (interactive)
  (setq presentation-frame-font-size (+ 1 presentation-frame-font-size))
  (set-frame-font (presentation-frame-font)))
(defun presentation-mode-decrease-font-size ()
  (interactive)
  (setq presentation-frame-font-size (- presentation-frame-font-size 1))
  (set-frame-font (presentation-frame-font)))
(global-set-key (kbd "C-?") 'presentation-mode-increase-font-size)
(global-set-key (kbd "C-M-?") 'presentation-mode-decrease-font-size)

;; Engage!
(package-require 'nyan-mode)
(nyan-mode 1)
(setq nyan-bar-length 16
      nyan-wavy-trail t)

;; Unclutter the modeline
(package-require 'diminish)
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(eval-after-load "ethan-wspace" '(diminish 'ethan-wspace-mode))
(eval-after-load "eldoc" '(diminish 'eldoc-mode))
(eval-after-load "rainbow-mode" '(diminish 'rainbow-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "autopair" '(diminish 'autopair-mode))
(eval-after-load "abbrev" '(diminish 'abbrev-mode))
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode))
(eval-after-load "js2-highlight-vars" '(diminish 'js2-highlight-vars-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode))
(eval-after-load "mmm-mode" '(diminish 'mmm-mode))
(eval-after-load "skewer-html" '(diminish 'skewer-html-mode))
(eval-after-load "skewer-mode" '(diminish 'skewer-mode))
(eval-after-load "auto-indent-mode" '(diminish 'auto-indent-minor-mode))
(eval-after-load "highlight-parentheses" '(diminish 'highlight-parentheses-mode))
(eval-after-load "subword" '(diminish 'subword-mode))
(eval-after-load "anzu" '(diminish 'anzu-mode))
(eval-after-load "cider" '(diminish 'cider-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "git-gutter" '(diminish 'git-gutter-mode))

(eval-after-load "js2-mode"
  '(defadvice js2-mode (after js2-rename-modeline activate)
     (setq mode-name "JS+")))
(eval-after-load "clojure-mode"
  '(defadvice clojure-mode (after clj-rename-modeline activate)
     (setq mode-name "Clj")))
(eval-after-load "typescript"
  '(defadvice typescript-mode (after typescript-rename-modeline activate)
     (setq mode-name "TS")))
(eval-after-load "nxhtml-mode"
  '(defadvice nxhtml-mode (after nxhtml-rename-modeline activate)
     (setq mode-name "HTML")))
(eval-after-load "js"
  '(defadvice js-mode (after js-rename-modeline activate)
     (setq mode-name "JS")))
(defadvice emacs-lisp-mode (after elisp-rename-modeline activate)
  (setq mode-name "ELisp"))

;; Powerline
(package-require 'powerline)
(defun powerline-nyan-theme ()
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1
                                   'powerline-inactive1))
                          (face2 (if active 'powerline-active2
                                   'powerline-inactive2))
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

                                (powerline-raw (nyan-create)))))
                     (concat
                      (powerline-render lhs)
                      (powerline-fill face2 (powerline-width rhs))
                      (powerline-render rhs)))))))
(powerline-nyan-theme)

(provide 'bodil-theme)
