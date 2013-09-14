;;; bodol-mode.el --- Major mode for BODOL code.

;; Copyright 2013 Bodil Stokke

;; Author: Bodil Stokke <bodil@bodil.org>
;; Version: 0.0.1

;;; Commentary:
;;
;; This lets you edit BODOL files.

(require 'lisp-mode)

;;; Code:

(setq bodol-indent-methods
      '(("ƒ" . defun)
        ("defn" . defun)
        ("defun" . defun)
        ("λ" . defun)
        ("fn" . defun)
        ("lambda" . defun)
        ("assert" . defun)
        ("asserts" . defun)))

(defun bodol-indent-function (indent-point state)
  (let ((normal-indent (current-column)))
       (goto-char (1+ (elt state 1)))
       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
       (if (and (elt state 2)
                (not (looking-at "\\sw\\|\\s_")))
           ;; car of form doesn't seem to be a symbol
           (progn
            (if (not (> (save-excursion (forward-line 1) (point))
                        calculate-lisp-indent-last-sexp))
                (progn (goto-char calculate-lisp-indent-last-sexp)
                       (beginning-of-line)
                       (parse-partial-sexp (point)
                                           calculate-lisp-indent-last-sexp 0 t)))
            ;; Indent under the list or under the first sexp on the same
            ;; line as calculate-lisp-indent-last-sexp.  Note that first
            ;; thing on that line has to be complete sexp since we are
            ;; inside the innermost containing sexp.
            (backward-prefix-chars)
            (current-column))
           (let ((function (buffer-substring-no-properties (point)
                                                           (progn (forward-sexp 1) (point))))
                 method)
                (setq method (cdr (assoc function bodol-indent-methods)))
                (cond ((eq method 'defun)
                       (lisp-indent-defform state indent-point))
                      ((integerp method)
                       (lisp-indent-specform method state
                                             indent-point normal-indent))
                      (method
                       (funcall method indent-point state)))))))

;;;###autoload
(define-derived-mode bodol-mode lisp-mode "BODOL"
  "Major mode for BODOL"
  (setq lisp-indent-function 'bodol-indent-function))

;;;###autoload
(add-to-list 'auto-mode-alist (cons "\\.bodol\\'" 'bodol-mode))

(provide 'bodol-mode)
;;; bodol-mode.el ends here
