;;; semantic-ectag-lang2.el --- Secondary language support
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-ectag-lang2.el,v 1.1 2009/03/10 00:30:37 zappo Exp $
;;
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
;; This file contains Exuberent CTags support for languages that
;; already have a regular Semantic parser.  These parsers can be used
;; on files not in buffers.

(require 'semantic-fw)
(require 'semantic-ectag-parse)

;;; Code:

;;; C/C++ Mode
;;
(defvar-mode-local c-mode semantic-ectag-lang "c"
  "Language name for Exuberent CTags.")

(defvar-mode-local c++-mode semantic-ectag-lang "c++"
  "Language name for Exuberent CTags.")

(defvar-mode-local c-mode semantic-ectag-lang-kind "cdegmnpsufvt"
  "Kinds of Exuberent CTags available.")

(defvar-mode-local c-mode semantic-ectag-lang-extra-flags
  '("--regex-c=/^[ \t]*#[ \t]*include[ \t]*[<\"]([\\/a-zA-Z0-9_.-]+)[>\"]/\\1/i,include/"
    "--regex-c++=/^[ \t]*#[ \t]*include[ \t]*[<\"]([\\/a-zA-Z0-9_.-]+)[>\"]/\\1/i,include/")
  "Add support for include files.
Support C and C++ when in c-mode, because emacs and ctags sometimes dissagree
on the major mode.")

(define-mode-local-override
  semantic-ectag-split-signature-summary c-mode (summary)
  "Convert the SUMMARY of function arguments into a list of tags.
These tags can be used as the argument list for a C function."
  (let* ((split (semantic-ectag-split-signature-summary-default summary))
	 (arg nil) (args nil))
    (dolist (S split)
      (setq arg
	    (cond
	     ((string-match
	       "^\\(struct\\|enum\\|union\\)\\s-+\\(\\w+\\)$" S)
	      ;; Two words, but first is "CLASS" or something.
	      (semantic-tag-new-variable
	       ""
	       (semantic-tag-new-type 
		(match-string 2 S)
		(match-string 1 S) nil nil)))
	     ((string-match
	       "^\\(struct\\|enum\\|union\\)\\s-+\\(\\w+\\)\\s-+\\(\\w+\\)$" S)
	      ;; Three words, first is "CLASS" or something.
	      (semantic-tag-new-variable
	       (match-string 3 S)
	       (semantic-tag-new-type 
		(match-string 2 S)
		(match-string 1 S) nil nil)))
	     ((string-match "^\\(\\w+\\)\\s-+\\(\\w+\\)$" S)
	      ;; Two words, a simple type and name.
	      (semantic-tag-new-variable
	       (match-string 2 S)
	       (match-string 1 S)))
	     ((string-match "^\\(\\w+\\)$" S)
	      ;; Only one word is a simple type.
	      (semantic-tag-new-variable
	       "" 
	       (match-string 1 S)))
	     ))
      (setq args (cons arg args))
      )
    (nreverse args)))

(define-mode-local-override
  semantic-ectag-set-language-attributes c-mode (tag parents)
  "Set some C specific attributs in TAG.
Uses PARENTS to determine if it is a constructor or destructor."
  (let ((lastname (car (reverse parents)))
	(name (semantic-tag-name tag))
	)
    (when (string= lastname name)
      (semantic-tag-put-attribute tag :constructor-flag t))
    (when (string= (concat "~" lastname) name)
      (setcar tag lastname)
      (semantic-tag-put-attribute tag :destructor-flag t))
    ))

;;; Emacs Lisp Mode
;;
(defvar-mode-local emacs-lisp-mode semantic-ectag-lang "lisp"
  "Language name for Exuberent CTags.")

(defvar-mode-local emacs-lisp-mode semantic-ectag-lang-kind "f"
  "Kinds of Exuberent CTags available.")

;;; SETUP
;;

;;;###autoload
(defun semantic-load-enable-secondary-exuberent-ctags-support ()
  "Enable exuberent ctags support as a secondary parser.
This is for semanticdb out-of-buffer parsing support.
Any mode that has been tested to work will be added to this function."
  (interactive)

  ;; Make sure that the version of ctags installed will work.
  (semantic-ectag-test-version)

  (semanticdb-enable-exuberent-ctags 'c-mode)
  (semanticdb-enable-exuberent-ctags 'emacs-lisp-mode)

  )


(provide 'semantic-ectag-lang2)
;;; semantic-ectag-lang2.el ends here
