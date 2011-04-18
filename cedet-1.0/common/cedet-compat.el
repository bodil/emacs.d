;;; cedet-compat.el --- Compatibility across (X)Emacs versions

;; Copyright (C) 2009, 2010 Eric M. Ludlam
;; Copyright (C) 2004, 2008, 2010 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Keywords: compatibility
;; X-RCS: $Id: cedet-compat.el,v 1.10 2010/04/09 01:12:18 zappo Exp $

;; This file is not part of Emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides functions to allow running CEDET packages on
;; a variety of [X]Emacs versions.

(require 'inversion)
;;; Code:

(when (not (fboundp 'compare-strings))

;; XEmacs does not have the `compare-strings' function.  Here is an
;; implementation in Emacs Lisp, derived from the C implementation
;; found in src/fns.c, in GNU Emacs 21.3.1 sources.
;;;###autoload
(defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
  "Compare the contents of two strings.
In string STR1, skip the first START1 characters and stop at END1.
In string STR2, skip the first START2 characters and stop at END2.
END1 and END2 default to the full lengths of the respective strings.

Case is significant in this comparison if IGNORE-CASE is nil.

The value is t if the strings (or specified portions) match.
If string STR1 is less, the value is a negative number N;
  - 1 - N is the number of characters that match at the beginning.
If string STR1 is greater, the value is a positive number N;
  N - 1 is the number of characters that match at the beginning."
  (or start1 (setq start1 0))
  (or start2 (setq start2 0))
  (setq end1 (if end1
                 (min end1 (length str1))
               (length str1)))
  (setq end2 (if end2
                 (min end2 (length str2))
               (length str2)))
  (let ((i1 start1)
        (i2 start2)
        result c1 c2)
    (while (and (not result) (< i1 end1) (< i2 end2))
      (setq c1 (aref str1 i1)
            c2 (aref str2 i2)
            i1 (1+ i1)
            i2 (1+ i2))
      (if ignore-case
          (setq c1 (upcase c1)
                c2 (upcase c2)))
      (setq result (cond ((< c1 c2) (- i1))
                         ((> c1 c2) i1))))
    (or result
        (cond ((< i1 end1) (1+ (- i1 start1)))
              ((< i2 end2) (1- (- start1 i1)))
              (t)))
    ))

)

(if (not (fboundp 'booleanp))

;; XEmacs does not have booleanp, which is used as a :type specifier for
;; some slots of some classes in EIEIO.  Define it here.
;;;###autoload
(defun boolean-p (bool)
  "Return non-nil if BOOL is nil or t."
  (or (null bool) (eq bool t)))

)

;; subst-char-in-string is not found on the XEmacs <= 21.4.  Provide
;; here for compatibility.
(if (not (fboundp 'subst-char-in-string))

;;;###autoload    
(defun subst-char-in-string (fromchar tochar string &optional inplace)
  ;; From Emacs 21.3/lisp/subr.el
  "Replace FROMCHAR with TOCHAR in STRING each time it occurs.
Unless optional argument INPLACE is non-nil, return a new string."
  (let ((i (length string))
	(newstr (if inplace string (copy-sequence string))))
    (while (> i 0)
      (setq i (1- i))
      (if (eq (aref newstr i) fromchar)
	  (aset newstr i tochar)))
    newstr))

)

(defun cedet-split-string-1 (string &optional separators omit-nulls)
  "Like `split-string' in Emacs 22 and later.
STRING and SEPARATORS as with traditional `split-string' implementations.
Third argument OMIT-NULLS omits any strings that are zero length.

Copied verbatim from Emacs 23 CVS version subr.el."
  (let ((keep-nulls (not (if separators omit-nulls t)))
	(rexp (or separators split-string-default-separators))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< start (length string)))
      (setq notfirst t)
      (if (or keep-nulls (< start (match-beginning 0)))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (if (or keep-nulls (< start (length string)))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list)))

(when (not (fboundp 'find-coding-systems-region))
;; XEmacs does not currently have `find-coding-systems-region'. Here
;; is an emulation, which seems sufficient for CEDET's purposes.
  (defun find-coding-systems-region (begin end)
    "Mimic Emacs' find-coding-system-region for XEmacs.
Return a coding system between BEGIN and END."
    (if (stringp begin)
	(if (equal (charsets-in-string begin) '(ascii))
	    '(undecided)
	  (delete-if-not
	   #'(lambda (coding-system)
	       ;; Assume strings are always short enough that the
	       ;; condition-case is not worth it.
	       (query-coding-string begin coding-system t))
	 
	   (remove-duplicates
	    (append
	     (get-language-info current-language-environment 'coding-system)
	     (mapcar #'coding-system-name (coding-system-list)))
	    :test #'eq :from-end t)))
      (if (equal (charsets-in-region begin end) '(ascii))
	  '(undecided)
	(delete-if-not
	 #'(lambda (coding-system)
	     (condition-case nil
		 (query-coding-region begin end coding-system nil t t)
	       (text-conversion-error)))
	 (remove-duplicates
	  (append
	   (get-language-info current-language-environment 'coding-system)
	   (mapcar #'coding-system-name (coding-system-list)))
	  :test #'eq :from-end t)))))
  )


;;;###autoload
(if (or (featurep 'xemacs) (inversion-test 'emacs "22.0"))
    ;; For XEmacs, or older Emacs, we need a new split string.
    (defalias 'cedet-split-string 'cedet-split-string-1)
  ;; For newer emacs, then the cedet-split-string is the same
  ;; as the built-in one.
  (defalias 'cedet-split-string 'split-string))


;;;###autoload
(when (not (fboundp 'with-no-warnings))
  (put 'with-no-warnings 'lisp-indent-function 0)
  (defun with-no-warnings (&rest body)
    "Copied from `with-no-warnings' in Emacs 23.
Like `progn', but prevents compiler warnings in the body.
Note: Doesn't work if this version is being loaded."
    ;; The implementation for the interpreter is basically trivial.
    (car (last body))))


(if (not (fboundp 'called-interactively-p))
    (defmacro cedet-called-interactively-p (&optional arg)
      "Compat function.  Calls `interactive-p'"
      '(interactive-p))
  ;; Else, it is defined, but perhaps too old?
  (condition-case nil
      (progn
	;; This condition case also prevents this from running twice.
	(called-interactively-p nil)
	;; An alias for the real deal.
	(defalias 'cedet-called-interactively-p 'called-interactively-p))
    (error
     ;; Create a new one
     (defmacro cedet-called-interactively-p (&optional arg)
       "Revised from the built-in version to accept an optional arg."
       (case (eval arg)
	 (interactive '(interactive-p))
	 ((any nil) '(called-interactively-p))))
     )))


;;; TESTS
;;
;;;###autoload
(defun cedet-compat-utest ()
  "Test compatability functions."
  (interactive)
  (when (not noninteractive)
    ;; Interactive tests need to be called interactively.
    (call-interactively 'cedet-utest-interactivep))
  ;; Other...
  )

(defun cedet-utest-interactivep ()
  "Test that `cedet-called-interactively-p' works."
  (interactive)
  (unless (cedet-called-interactively-p 'interactive)
    (error "Failed interactive test"))
  (unless (cedet-called-interactively-p 'any)
    (error "Failed interactive any test"))
  (cedet-utest-interactivep-subfcn)
  (message "All CEDET called-interactively tests pass."))

(defun cedet-utest-interactivep-subfcn ()
  "Test that `cedet-called-interactively-p' works noninteractively."
  (when (cedet-called-interactively-p 'interactive)
    (error "Failed non-interactive test"))
  (when (cedet-called-interactively-p 'any)
    (error "Failed non-interactive any test")))

(provide 'cedet-compat)

;;; cedet-compat.el ends here
