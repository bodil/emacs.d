;;; bodil-multimodes.el -- mmm-mode config

(package-require 'mmm-mode)
(require 'mmm-mode)
(setq mmm-global-mode 1)

;; Define active classes
(setq mmm-mode-ext-classes-alist
      '(;; JS inside HTML
        (html-mode nil html-js)
        ;; CSS inside HTML
        (html-mode nil html-css)))

(mmm-add-group
 'html-js-deck
 '((js-section
    :submode js-mode
    :face mmm-code-submode-face
    :front "<section[^>]*data-editor=\"text/javascript\"[^>]*>[ \t]*\n?"
    :back "[ \t]*</section>"
    :insert ((?j js-tag nil @ "<section data-editor=\"text/javascript\">\n"
                 @ "" _ "" @ "\n</section>" @)))))

(mmm-add-mode-ext-class 'html-mode nil 'html-js-deck)

;; <section data-editor="text/javascript"

;; Prefer js2-mode for inline JS
(mmm-add-to-major-mode-preferences 'javascript 'js2-mode t)

(provide 'bodil-multimodes)
