(deftheme bubbleberry "bubbleberry")

;; Based on the theme used for LightTable (see: http://www.chris-granger.com/images/lightable/main.png )

(custom-theme-set-variables
 'bubbleberry
 '(fringe-mode 5 nil (fringe))
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945"))

(custom-theme-set-faces
 'bubbleberry
 
 ;; basic theming.
 '(default                          ((t (:foreground "#ABAEB3" :background "#222222" ))))
 '(region                           ((t (:background "#101010" ))))
 '(cursor                           ((t (:background "#ffffff" ))))
 '(fringe                           ((t (:background "#2f2f2f" :foreground "#ffffff" ))))
 '(linum                            ((t (:background "#202020" :foreground "#2f2f2f" :box nil :height 100 ))))
 '(minibuffer-prompt                ((t (:foreground "#9489C4" :weight bold ))))
 '(minibuffer-message               ((t (:foreground "#ffffff" ))))
 '(mode-line                        ((t (:foreground "#FFFFFF" :background "#191919" ))))
 '(mode-line-inactive               ((t (:foreground "#777777" :background "#303030" :weight light :box nil :inherit (mode-line )))))

 '(font-lock-keyword-face           ((t (:foreground "#3ca380"))))
 '(font-lock-type-face              ((t (:foreground "#484879"))))
 '(font-lock-constant-face          ((t (:foreground "#3F5C70"))))
 '(font-lock-variable-name-face     ((t (:foreground "#547B96"))))
 '(font-lock-builtin-face           ((t (:foreground "#6767AE"))))
 '(font-lock-string-face            ((t (:foreground "#699ABC"))))
 '(font-lock-comment-face           ((t (:foreground "#496b83"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "#666688"))))

 '(font-lock-function-name-face     ((t (:foreground "#3ca380"))))
 '(font-lock-doc-string-face        ((t (:foreground "#496B83"))))

 ;; easy defaults...
 '(tooltip ((default nil) (nil nil)))
 '(next-error ((t          (:inherit (region)))))
 '(query-replace ((t       (:inherit (isearch)))))
 '(button ((t              (:inherit (link)))))
 '(fixed-pitch ((t         (:family "Monospace")))) 
 '(variable-pitch ((t      (:family "Sans Serif"))))
 '(escape-glyph ((t        (:foreground "#FF6600"))))
 '(mode-line-emphasis ((t  (:weight bold))))
 '(mode-line-highlight ((t (:box nil (t (:inherit (highlight)))))))
  
 '(highlight 
   ((((class color) (min-colors 88) (background light)) (:background "#2a2a2a")) 
    (((class color) (min-colors 88) (background dark))  (:background "#2a2a2a")) 
    (((class color) (min-colors 16) (background light)) (:background "#2a2a2a")) 
    (((class color) (min-colors 16) (background dark))  (:background "#2a2a2a")) 
    (((class color) (min-colors 8))                     (:background "#000000"))))

 '(shadow 
   ((((class color grayscale) (min-colors 88) (background light)) (:foreground "#999999")) 
    (((class color grayscale) (min-colors 88) (background dark))  (:foreground "#999999"))
    (((class color) (min-colors 8) (background light))            (:foreground "#00ff00"))
    (((class color) (min-colors 8) (background dark))             (:foreground "#ffff00"))))
  
 '(trailing-whitespace
   ((((class color) (background light)) (:background "#ff0000"))
    (((class color) (background dark))  (:background "#ff0000")) (t (:inverse-video t))))
  
 '(link
   ((((class color) (min-colors 88) (background light)) (:underline t :foreground "#00b7f0")) 
    (((class color) (background light))                 (:underline t :foreground "#0044FF")) 
    (((class color) (min-colors 88) (background dark))  (:underline t :foreground "#0099aa"))
    (((class color) (background dark))                  (:underline t :foreground "#0099aa")) (t (:inherit (underline)))))
  
 '(link-visited 
   ((default                            (:inherit (link))) 
    (((class color) (background light)) (:inherit (link))) 
    (((class color) (background dark))  (:inherit (link)))))
  
 '(header-line 
   ((default                                      (:inherit (mode-line))) (((type tty)) (:underline t :inverse-video nil)) 
    (((class color grayscale) (background light)) (:box nil :foreground "#222222" :background "#bbbbbb")) 
    (((class color grayscale) (background dark))  (:box nil :foreground "#bbbbbb" :background "#222222")) 
    (((class mono) (background light))            (:underline t :box nil :inverse-video nil :foreground "#000000" :background "#ffffff")) 
    (((class mono) (background dark))             (:underline t :box nil :inverse-video nil :foreground "#ffffff" :background "#000000"))))
  
 '(isearch
   ((((class color) (min-colors 88) (background light)) (:foreground "#99ccee" :background "#444444")) 
    (((class color) (min-colors 88) (background dark))  (:foreground "#bb3311" :background "#444444")) 
    (((class color) (min-colors 16))                    (:foreground "#0088cc" :background "#444444"))
    (((class color) (min-colors 8))                     (:foreground "#0088cc" :background "#444444")) (t (:inverse-video t))))
  
 '(isearch-fail
   ((((class color) (min-colors 88) (background light)) (:background "#ffaaaa"))
    (((class color) (min-colors 88) (background dark))  (:background "#880000"))
    (((class color) (min-colors 16))                    (:background "#FF0000"))
    (((class color) (min-colors 8))                     (:background "#FF0000"))
    (((class color grayscale))                          (:foreground "#888888")) (t (:inverse-video t))))
  
 '(lazy-highlight
   ((((class color) (min-colors 88) (background light)) (:background "#77bbdd"))
    (((class color) (min-colors 88) (background dark)) (:background "#77bbdd"))
    (((class color) (min-colors 16)) (:background "#4499ee"))
    (((class color) (min-colors 8)) (:background "#4499ee")) (t (:underline t))))
  
 '(match
   ((((class color) (min-colors 88) (background light)) (:background "#3388cc"))
    (((class color) (min-colors 88) (background dark)) (:background "#3388cc"))
    (((class color) (min-colors 8) (background light)) (:foreground "#000000" :background "#FFFF00"))
    (((class color) (min-colors 8) (background dark)) (:foreground "#ffffff" :background "#0000FF")) 
    (((type tty) (class mono)) (:inverse-video t)) (t (:background "#888888"))))

 ;; show-paren-mode
 '(show-paren-match
   ((((class color) (min-colors 88) (background light)) (:background "#3388cc"))
    (((class color) (min-colors 88) (background dark)) (:background "#3388cc"))
    (((class color) (min-colors 8) (background light)) (:foreground "#000000" :background "#FFFF00"))
    (((class color) (min-colors 8) (background dark)) (:foreground "#ffffff" :background "#0000FF")) 
    (((type tty) (class mono)) (:inverse-video t)) (t (:background "#888888"))))
 
 ;; mark-multiple
 '(mm/master-face
   ((((class color) (min-colors 88) (background light)) (:background "#444444"))
    (((class color) (min-colors 88) (background dark)) (:background "#444444"))
    (((class color) (min-colors 8) (background light)) (:foreground "#000000" :background "#FFFF00"))
    (((class color) (min-colors 8) (background dark)) (:foreground "#ffffff" :background "#0000FF")) 
    (((type tty) (class mono)) (:inverse-video t)) (t (:background "#888888"))))
 '(mm/mirror-face
   ((((class color) (min-colors 88) (background light)) (:background "#444444"))
    (((class color) (min-colors 88) (background dark)) (:background "#444444"))
    (((class color) (min-colors 8) (background light)) (:foreground "#000000" :background "#FFFF00"))
    (((class color) (min-colors 8) (background dark)) (:foreground "#ffffff" :background "#0000FF")) 
    (((type tty) (class mono)) (:inverse-video t)) (t (:background "#888888"))))

 ;; MuMaMo
 '(mumamo-background-chunk-submode1 ((t (:background "#2a2a2a"))))
 '(mumamo-background-chunk-submode2 ((t (:background "#343434"))))

 ;; Terminal
 '(term-color-blue ((t (:background "#6c6cae" :foreground "#6c6cae"))))
 '(term-color-cyan ((t (:background "#8ac6f2" :foreground "#8ac6f2"))))
 '(term-color-green ((t (:background "#95e454" :foreground "#95e454"))))
 '(term-color-magenta ((t (:background "#ccaa8f" :foreground "#ccaa8f"))))
 '(term-color-red ((t (:background "#e5786d" :foreground "#e5786d"))))
 '(term-color-white ((t (:background "#f6f3e8" :foreground "#f6f3e8"))))
 '(term-color-yellow ((t (:background "#cae682" :foreground "#cae682"))))
)

(provide-theme 'bubbleberry)
