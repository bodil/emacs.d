;;; semantic-ectag-util.el --- Utilities for Exuberent CTags and Semantic

;; Copyright (C) 2008, 2009, 2010 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-ectag-util.el,v 1.8 2010/04/09 02:01:20 zappo Exp $

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
;; Utilities for working the Exuberent CTags integration with Semantic.
;;
;; * Configuration for ctags.
;; * Running ctags.
;; * Decoding ctags output.

(eval-when-compile (require 'inversion))

;;; Code:

(defcustom semantic-ectag-program-list '("ctags-exuberant"
					 "ectags"
					 "ctags")
  "*List of possible exuberent ctags programs that coule be installed."
  :group 'semantic
  :type '(repeat file))

(defcustom semantic-ectag-program nil
  "The Exuberent CTags program to use."
  :group 'semantic
  :type 'program)

(defun semantic-ectag-program ()
  "Return our best guess at an exuberent ctags program."
  (or semantic-ectag-program
      (let ((pl semantic-ectag-program-list))
	(while (and pl (not semantic-ectag-program))
	  (condition-case nil
	      (progn
		(call-process (car pl) nil nil nil "--version")
		(setq semantic-ectag-program (car pl)))
	    (error nil))
	  (setq pl (cdr pl)))
	(or semantic-ectag-program "ctags"))))

;;; RUN CTAGS
;;
(defun semantic-ectag-run (&rest args)
  "Run Exuberent CTags, and return a buffer with the output.
ARGS are the arguments to pass to Exuberent CTags.
The returned buffer will be recycled in future calls to this function."
  (let ((b (get-buffer-create " *Semantic ECTags*"))
	(dd default-directory))
    (save-excursion
      (set-buffer b)
      (erase-buffer)
      (setq default-directory dd)
      (condition-case nil
	  (progn
	    (apply 'call-process (semantic-ectag-program) nil b nil
		   args)
	    b)
	(error nil)))
    ))

;;; Semi-automatic linguistic configuration
;;
;; Ask ctags what languages it supports, and what kinds there are.
(defun semantic-ectag-lang-and-kinds ()
  "Get all the language and kinds supported by ctags."
  (interactive)
  (let* ((b (semantic-ectag-run "--list-kinds=all"))
	 (lang nil)
	 (kinds nil))
    (save-excursion
      (set-buffer b)
      (goto-char (point-min))
      (while (not (eobp))
	(setq lang (buffer-substring (point) (point-at-eol)))
	(end-of-line)
	(forward-char 1)
	(setq kinds "")
	(while (looking-at "\\s-+")
	  (let* ((split (split-string (buffer-substring (point) (point-at-eol))
				     "  " t))
		 (letter (car split))
		 (word (car (cdr split))))
	    (when (member word
			  '("function definitions"
			    "functions"
			    "variables"
			    "variable definitions"
			    "type"
			    "types"
			    "classes"
			    "namespaces"))
	      (setq kinds (concat kinds letter))))
	  (end-of-line)
	  (forward-char 1))

	;; This is where we should auto-configure, but i'm not
	;; too happy with the mechanism yet.  Just dump messages
	;; for now.
	(message "Lang %s kinds= %s"
		 (downcase lang)
		 kinds)
	)
      )
    (switch-to-buffer-other-window b)
    (goto-char (point-min))
    ))

;;; Revision Test
;;
;; Make sure we have an up to date version of ctags.

(defun semantic-ectag-version ()
  "Get the revision number of ctags."
  (interactive)
  (let* ((b (semantic-ectag-run "--version"))
	 str ropt)
    (if (not b)
	(progn
	  (message "Could not find program %s"
		   semantic-ectag-program)
	  nil)
      (setq str (save-excursion
		  (set-buffer b)
		  (goto-char (point-min))
		  (if (re-search-forward "Exuberant Ctags \\([0-9.]+\\)," nil t)
		      (match-string 1)
		    nil)
		  )
	    ropt (save-excursion
		   (set-buffer b)
		   (goto-char (point-min))
		   (if (re-search-forward "\\+regex\\>" nil t)
		       t
		     nil)))
      (if (not str)
	  (let ((whatver
		 (save-excursion
		   (set-buffer b)
		   (goto-char (point-min))
		   (cond ((looking-at "ctags (?GNU Emacs")
			  "ctags that comes with Emacs")
			 (t
			  "unknown ctags version"))
		   )))
	    (message "Exuberent CTags not found.  Found %s" whatver)
	    nil)
	(when (cedet-called-interactively-p)
	  (message "Detected Exuberent CTags version : %s %s"
		   str
		   (if ropt
		       "with regex support"
		     "WITHOUT regex support")
		   ))
	(list str ropt) ))))

(defvar semantic-ectag-min-version "5.7"
  "Minimum version of Exuberent CTags we need.")

(defun semantic-ectag-test-version ()
  "Make sure the version of ctags we have is up to date."
  (let* ((vi (semantic-ectag-version))
	 (v (car vi))
	 (r (car (cdr vi))))
    (require 'inversion)
    (when (not v)
      (error "Exuberent CTags not found.  Use M-x semantic-ectag-version RET"))
    (when (inversion-check-version v nil semantic-ectag-min-version)
      (error "Version of CTags is %s.  Need at least %s"
	     v semantic-ectag-min-version))
    (when (not r)
      (error "CTags was not compiled with +regex support"))
    ))

(provide 'semantic-ectag-util)
;;; semantic-ectag-util.el ends here
