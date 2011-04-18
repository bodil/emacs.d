;;; semantic-util.el --- Utilities for use with semantic tag tables

;;; Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2008, 2009, 2010 Eric M. Ludlam

;; Author: Eric M. Ludlam <zappo@gnu.org>
;; Keywords: syntax
;; X-RCS: $Id: semantic-util.el,v 1.146 2010/04/12 11:09:37 zappo Exp $

;; This file is not part of GNU Emacs.

;; Semantic is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Semantic utility API for use with semantic tag tables.
;;

(require 'assoc)
(require 'semantic)
(eval-when-compile
  ;; Emacs 21
  (condition-case nil
      (require 'newcomment)
    (error nil))
  ;; Semanticdb calls
  (require 'semanticdb)
  )

;;; Code:

(defvar semantic-type-relation-separator-character '(".")
  "Character strings used to separate a parent/child relationship.
This list of strings are used for displaying or finding separators
in variable field dereferencing.  The first character will be used for
display.  In C, a type field is separated like this: \"type.field\"
thus, the character is a \".\".  In C, and additional value of \"->\"
would be in the list, so that \"type->field\" could be found.")
(make-variable-buffer-local 'semantic-type-relation-separator-character)

(defvar semantic-equivalent-major-modes nil
  "List of major modes which are considered equivalent.
Equivalent modes share a parser, and a set of override methods.
A value of nil means that the current major mode is the only one.")
(make-variable-buffer-local 'semantic-equivalent-major-modes)

;; These semanticdb calls will throw warnings in the byte compiler.
;; Doing the right thing to make them available at compile time
;; really messes up the compilation sequence.
(defun semantic-file-tag-table (file)
  "Return a tag table for FILE.
If it is loaded, return the stream after making sure it's ok.
If FILE is not loaded, check to see if `semanticdb' feature exists,
   and use it to get tags from files not in memory.
If FILE is not loaded, and semanticdb is not available, find the file
   and parse it."
  (save-match-data
    (if (find-buffer-visiting file)
	(with-current-buffer (find-buffer-visiting file)
	  (semantic-fetch-tags))
      ;; File not loaded
      (if (and (fboundp 'semanticdb-minor-mode-p)
	       (semanticdb-minor-mode-p))
	  ;; semanticdb is around, use it.
	  (semanticdb-file-stream file)
	;; Get the stream ourselves.
	(with-current-buffer (find-file-noselect file)
	  (semantic-fetch-tags))))))

(semantic-alias-obsolete 'semantic-file-token-stream
			 'semantic-file-tag-table)

(defun semantic-something-to-tag-table (something)
  "Convert SOMETHING into a semantic tag table.
Something can be a tag with a valid BUFFER property, a tag table, a
buffer, or a filename.  If SOMETHING is nil return nil."
  (cond
   ;; A list of tags
   ((and (listp something)
	 (semantic-tag-p (car something)))
    something)
   ;; A buffer
   ((bufferp something)
    (with-current-buffer something
      (semantic-fetch-tags)))
   ;; A Tag: Get that tag's buffer
   ((and (semantic-tag-with-position-p something)
	 (semantic-tag-in-buffer-p something))
    (with-current-buffer (semantic-tag-buffer something)
      (semantic-fetch-tags)))
   ;; Tag with a file name in it
   ((and (semantic-tag-p something)
	 (semantic-tag-file-name something)
	 (file-exists-p (semantic-tag-file-name something)))
    (semantic-file-tag-table
     (semantic-tag-file-name something)))
   ;; A file name
   ((and (stringp something)
	 (file-exists-p something))
    (semantic-file-tag-table something))
   ;; A Semanticdb table
   ((and (featurep 'semanticdb)
	 (semanticdb-minor-mode-p)
	 (semanticdb-abstract-table-child-p something))
    (semanticdb-refresh-table something)
    (semanticdb-get-tags something))
   ;; Semanticdb find-results
   ((and (featurep 'semanticdb)
	 (semanticdb-minor-mode-p)
	 (semanticdb-find-results-p something))
    (semanticdb-strip-find-results something))
   ;; NOTE: This commented out since if a search result returns
   ;;       empty, that empty would turn into everything on the next search.
   ;; Use the current buffer for nil
;;   ((null something)
;;    (semantic-fetch-tags))
   ;; don't know what it is
   (t nil)))

(semantic-alias-obsolete 'semantic-something-to-stream
			 'semantic-something-to-tag-table)

;;; Completion APIs
;;
;; These functions provide minibuffer reading/completion for lists of
;; nonterminals.
(defvar semantic-read-symbol-history nil
  "History for a symbol read.")

(defun semantic-read-symbol (prompt &optional default stream filter)
  "Read a symbol name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from.
FILTER is provides a filter on the types of things to complete.
FILTER must be a function to call on each element."
  (if (not default) (setq default (thing-at-point 'symbol)))
  (if (not stream) (setq stream (semantic-fetch-tags)))
  (setq stream
	(if filter
	    (semantic--find-tags-by-function filter stream)
	  (semantic-brute-find-tag-standard stream)))
  (if (and default (string-match ":" prompt))
      (setq prompt
	    (concat (substring prompt 0 (match-end 0))
		    " (default: " default ") ")))
  (completing-read prompt stream nil t ""
		   'semantic-read-symbol-history
		   default))

(defun semantic-read-variable (prompt &optional default stream)
  "Read a variable name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tokens to complete from."
  (semantic-read-symbol
   prompt default
   (or (semantic-find-tags-by-class
	'variable (or stream (current-buffer)))
       (error "No local variables"))))

(defun semantic-read-function (prompt &optional default stream)
  "Read a function name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tags to complete from."
  (semantic-read-symbol
   prompt default
   (or (semantic-find-tags-by-class
	'function (or stream (current-buffer)))
       (error "No local functions"))))

(defun semantic-read-type (prompt &optional default stream)
  "Read a type name from the user for the current buffer.
PROMPT is the prompt to use.
Optional arguments:
DEFAULT is the default choice.  If no default is given, one is read
from under point.
STREAM is the list of tags to complete from."
  (semantic-read-symbol
   prompt default
   (or (semantic-find-tags-by-class
	'type (or stream (current-buffer)))
       (error "No local types"))))


;;; Interactive Functions for
;;
(defun semantic-describe-tag (&optional tag)
  "Describe TAG in the minibuffer.
If TAG is nil, describe the tag under the cursor."
  (interactive)
  (if (not tag) (setq tag (semantic-current-tag)))
  (semantic-fetch-tags)
  (if tag (message (semantic-format-tag-summarize tag))))


;;; Putting keys on tags.
;;
(defun semantic-add-label (label value &optional tag)
  "Add a LABEL with VALUE on TAG.
If TAG is not specified, use the tag at point."
  (interactive "sLabel: \nXValue (eval): ")
  (if (not tag)
      (progn
	(semantic-fetch-tags)
	(setq tag (semantic-current-tag))))
  (semantic--tag-put-property tag (intern label) value)
  (message "Added label %s with value %S" label value))

(defun semantic-show-label (label &optional tag)
  "Show the value of LABEL on TAG.
If TAG is not specified, use the tag at point."
  (interactive "sLabel: ")
  (if (not tag)
      (progn
	(semantic-fetch-tags)
	(setq tag (semantic-current-tag))))
  (message "%s: %S" label (semantic--tag-get-property tag (intern label))))


;;; Hacks
;;
;; Some hacks to help me test these functions
(defun semantic-describe-buffer-var-helper (varsym buffer)
  "Display to standard out the value of VARSYM in BUFFER."
  (let ((value (with-current-buffer buffer
		 (symbol-value varsym))))
    (cond
     ((and (consp value)
	   (< (length value) 10))
      ;; Draw the list of things in the list.
      (princ (format "  %s:  #<list of %d items>\n"
		     varsym (length value)))
      (data-debug-insert-stuff-list
       value "    " )
      )
     (t
      ;; Else do a one-liner.
      (data-debug-insert-thing
       value " " (concat " " (symbol-name varsym) ": "))
      ))))

(defun semantic-describe-buffer ()
  "Describe the semantic environment for the current buffer."
  (interactive)
  (let ((buff (current-buffer))
	)

    (with-output-to-temp-buffer (help-buffer)
      (help-setup-xref (list #'semantic-describe-buffer)
                       (cedet-called-interactively-p 'interactive))
      (with-current-buffer standard-output
	(princ "Semantic Configuration in ")
	(princ (buffer-name buff))
	(princ "\n\n")

	(princ "Buffer specific configuration items:\n")
	(let ((vars '(major-mode
		      semantic-case-fold
		      semantic-tag-expand-function
		      semantic-parser-name
		      semantic-parse-tree-state
		      semantic-lex-analyzer
		      semantic-lex-reset-hooks
		      semantic-lex-syntax-modifications
		      )))
	  (dolist (V vars)
	    (semantic-describe-buffer-var-helper V buff)))

	(princ "\nGeneral configuration items:\n")
	(let ((vars '(semantic-inhibit-functions
		      semantic-init-hook
		      semantic-init-db-hook
		      semantic-unmatched-syntax-hook
		      semantic--before-fetch-tags-hook
		      semantic-after-toplevel-bovinate-hook
		      semantic-after-toplevel-cache-change-hook
		      semantic-before-toplevel-cache-flush-hook
		      semantic-dump-parse
		      semantic-type-relation-separator-character
		      semantic-command-separation-character
		      )))
	  (dolist (V vars)
	    (semantic-describe-buffer-var-helper V buff)))

	(princ "\n\n")
	(mode-local-describe-bindings-2 buff)
	)))
  )

(defun semantic-assert-valid-token (tok)
  "Assert that TOK is a valid token."
  (if (semantic-tag-p tok)
      (if (semantic-tag-with-position-p tok)
	  (let ((o  (semantic-tag-overlay tok)))
	    (if (and (semantic-overlay-p o)
		     (not (semantic-overlay-live-p o)))
		(let ((debug-on-error t))
		  (error "Tag %s is invalid!" (semantic-tag-name tok)))
	      ;; else, tag is OK.
	      ))
	;; Positionless tags are also ok.
	)
    (let ((debug-on-error t))
      (error "Not a semantic tag: %S" tok))))

(defun semantic-sanity-check (&optional cache over notfirst)
  "Perform a sanity check on the current buffer.
The buffer's set of overlays, and those overlays found via the cache
are verified against each other.
CACHE, and OVER are the semantic cache, and the overlay list.
NOTFIRST indicates that this was not the first call in the recursive use."
  (interactive)
  (if (and (not cache) (not over) (not notfirst))
      (setq cache semantic--buffer-cache
	    over (semantic-overlays-in (point-min) (point-max))))
  (while cache
    (let ((chil (semantic-tag-components-with-overlays (car cache))))
      (if (not (memq (semantic-tag-overlay (car cache)) over))
	  (message "Tag %s not in buffer overlay list."
		   (semantic-format-tag-concise-prototype (car cache))))
      (setq over (delq (semantic-tag-overlay (car cache)) over))
      (setq over (semantic-sanity-check chil over t))
      (setq cache (cdr cache))))
  (if (not notfirst)
      ;; Strip out all overlays which aren't semantic overlays
      (let ((o nil))
	(while over
	  (when (and (semantic-overlay-get (car over) 'semantic)
		     (not (eq (semantic-overlay-get (car over) 'semantic)
			      'unmatched)))
	    (setq o (cons (car over) o)))
	  (setq over (cdr over)))
	(when (cedet-called-interactively-p 'any)
	  (message "Remaining overlays: %S" o))))
  over)

(provide 'semantic-util)

;;; Minor modes
;;
(require 'semantic-util-modes)

;;; semantic-util.el ends here
