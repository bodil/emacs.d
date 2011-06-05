;;; bodil-java.el -- Java configuration

;; CEDET/Malabar setup
(setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
                                  global-semanticdb-minor-mode
                                  global-semantic-idle-summary-mode
                                  global-semantic-mru-bookmark-mode))
(semantic-mode 1)
(require 'malabar-mode)
(setq malabar-groovy-lib-dir "~/.emacs.d/site-lisp/malabar/lib")
(add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; Make Malabar's autoimport behave more like Eclipse
(defun malabar-eclipse-import ()
  "Eclipse style import handling."
  (interactive)
  (malabar-import-all)
  (malabar-import-group-imports))
(global-set-key (kbd "C-c C-v z") 'malabar-eclipse-import)

;; Auto-insert closing braces, from http://stackoverflow.com/questions/848624/smart-electric-brace-in-cc-modes-c-java-etc
(defun cheeso-looking-back-at-regexp (regexp)
  "calls backward-sexp and then checks for the regexp.  Returns t if it is found, else nil"
  (interactive "s")
  (save-excursion
    (backward-sexp)
    (looking-at regexp)))
(defun cheeso-looking-back-at-equals-or-array-init ()
  "returns t if an equals or [] is immediate preceding. else nil."
  (interactive)
  (cheeso-looking-back-at-regexp "\\(\\w+\\b *=\\|[[]]+\\)"))  
(defun cheeso-prior-sexp-same-statement-same-line ()
  "returns t if the prior sexp is on the same line. else nil"
  (interactive)
  (save-excursion
    (let ((curline (line-number-at-pos))
          (curpoint (point))
          (aftline (progn
                     (backward-sexp)
                     (line-number-at-pos))) )
      (= curline aftline))))
(defun cheeso-insert-open-brace ()
    "if point is not within a quoted string literal, insert an open brace, two newlines, and a close brace; indent everything and leave point on the empty line. If point is within a string literal, just insert a pair or braces, and leave point between them."
    (interactive)
    (cond
     ;; are we inside a string literan? 
     ((c-got-face-at (point) c-literal-faces)
      ;; if so, then just insert a pair of braces and put the point between them
      (self-insert-command 1)
      (insert "}")
      (backward-char))
     ;; was the last non-space an equals sign? or square brackets?  Then it's an initializer.
     ((cheeso-looking-back-at-equals-or-array-init)
      (self-insert-command 1)
      ;; all on the same line
      (insert "  };")
      (backward-char 3))
     ;; else, it's a new scope.
     ;; therefore, insert paired braces with an intervening newline, and indent everything appropriately.
     (t
      (self-insert-command 1)
      (c-indent-line-or-region)
      (end-of-line)
      (newline)
      (insert "}")
      ;;(c-indent-command) ;; not sure of the difference here
      (c-indent-line-or-region)
      (previous-line)
      (end-of-line)
      (newline-and-indent)
      ; point ends up on an empty line, within the braces, properly indented
      )))

;; Hook
(add-hook 'malabar-mode-hook
          (lambda ()
            ;; Activate the open brace stuff
            (local-set-key (kbd "{") 'cheeso-insert-open-brace)
            ;; Autocompile on save
            (add-hook 'after-save-hook 'malabar-compile-file-silently
                      nil t)))

