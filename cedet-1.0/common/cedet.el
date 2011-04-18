;;; cedet.el --- Setup CEDET environment

;; Copyright (C) 2007, 2008, 2009, 2010 by Eric M. Ludlam
;; Copyright (C) 2002, 2003, 2004, 2005, 2006 by David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: CEDET developers <http://sf.net/projects/cedet>
;; Created: 09 Dec 2002
;; Keywords: syntax
;; X-RCS: $Id: cedet.el,v 1.42 2010/07/17 13:34:12 zappo Exp $

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
;; This library automatically setups your [X]Emacs to use CEDET tools.
;;
;; First download the latest CEDET distribution, provided in a
;; cedet-<VERSION>.tar.gz tarball, from the project page at:
;; <http://sf.net/projects/cedet>.
;;  
;; Unpack the tarball in a directory of your choice.  It will install
;; the following directory tree:
;;
;;   cedet
;;     |
;;     +- common
;;     |
;;     +- cogre
;;     |
;;     +- ede
;;     |
;;     +- eieio
;;     |
;;     +- semantic
;;     |
;;     +- speedbar
;;     |
;;     +- srecode
;;     |
;;     \- contrib
;;
;; Then, add the following into your ~/.emacs startup file:
;;
;;   (load-file "<INSTALL-PATH>/cedet/common/cedet.el")
;;
;; Once loaded, you can enable additional feature.  For example,
;; this will enable some basic and advance features:
;;
;;   (load-file "<INSTALL-PATH>/cedet/common/cedet.el")
;;   (global-ede-mode t)
;;   (semantic-load-enable-code-helpers)
;;   (global-srecode-minor-mode 1)
;;
;; See the INSTALL file for more.

;;
;; That's it!
;;

;;; Code:
(when (featurep 'cedet)
  (error "CEDET Version %s already loaded." cedet-version))

(eval-when-compile
  (require 'cl)
  )

(defconst cedet-version "1.0"
  "Current version of CEDET.")

(defconst cedet-emacs-min-version "21.1"
  "Minimum version of GNU Emacs supported by CEDET.")
(defconst cedet-xemacs-min-version "21.4"
  "Minimum version of XEmacs supported by CEDET.")

(defconst cedet-packages
  `(
    ;;PACKAGE   MIN-VERSION      INSTALLDIR  DOCDIR
    (cedet         ,cedet-version "common"   "common" 	   )
    (eieio         "1.3"           nil       "eieio"       )
    (semantic      "2.0"           nil       "semantic/doc")
    (srecode       "1.0"           nil       "srecode"     ) 
    (ede           "1.0"           nil       "ede"    	   )    
    (speedbar      "1.0.3"         nil       "speedbar"    )
    (cogre         "1.0"           nil       "cogre"  	   )
    (cedet-contrib "1.0"           "contrib" nil           )
    )
  "Table of CEDET packages to install.")

;; This file must be in "<INSTALL-DIR>/cedet/common"!
(let ((default-directory
        (file-name-directory
         (or load-file-name (buffer-file-name)))))
  
  ;; Add "<INSTALL-DIR>/cedet/common" to `load-path'.
  (add-to-list 'load-path default-directory)
  ;;(message "%S added to `load-path'" default-directory)
  ;; Require the inversion library.
  (require 'inversion)
  
  ;; Require specific Emacs versions
  (inversion-require-emacs cedet-emacs-min-version
			   cedet-xemacs-min-version)

  ;; Go up to the parent "<INSTALL-DIR>/cedet" directory.
  (let ((default-directory (expand-file-name ".."))
        package min-version installdir docdir)

    ;; Add the CEDET packages subdirectories to the `load-path' if
    ;; necessary.
    (dolist (package-spec cedet-packages)
      (setq package     (nth 0 package-spec)
            min-version (nth 1 package-spec)
            installdir  (nth 2 package-spec)
	    docdir      (nth 3 package-spec)
	    )
      ;; Add package to load path
      (when installdir
        (setq installdir (expand-file-name installdir)))
      (inversion-add-to-load-path package min-version installdir)
      ;; Add doc to Info path
      (when docdir
	(let ((fulldocpath (expand-file-name docdir default-directory)))
	  ;; Set up one of the info paths depending on if info is
	  ;; loaded yet.	  
	  (if (featurep 'info)
	      (progn
		(condition-case nil ; Not all emacs versions have this.
		    (info-initialize)
		  (error nil))
		(add-to-list 'Info-directory-list fulldocpath))
	    (add-to-list 'Info-default-directory-list fulldocpath))
	  )))

    ;; Force EIEIO to load so that the autoloads work.
    (require 'eieio)

    ;; Then run every package setup.
    (message "Setting up CEDET packages...")
    (dolist (package-spec cedet-packages)
      (setq package (nth 0 package-spec))
      (condition-case err
	  (progn
	    (require (intern (format "%s-load" package)))
	    )
	(error
	 (message "%s" (error-message-string err)))))
    (message "Setting up CEDET packages...done")
    ))

(eval-when-compile
  (require 'inversion))

(defun cedet ()
  "Display basic information/help about CEDET.

Also output the results of `cedet-version-print'.

See also function `cedet-version'."
  (interactive)
  (with-output-to-temp-buffer "*CEDET*"
    (princ "You have invoked the `cedet' command.

CEDET is a Collection of Emacs Development Environment Tools.
CEDET is made up of several tools.

Project Management:  EDE
  EDE is a project managment system.  It can either create Makefiles
  for your project, or identify different pre-existing project styles
  including Automake, Make, SCons, CMake, Emacs or Linux.

  (global-ede-mode 1)

  Use M-x ede-new RET to create new projects.

Code Completion, Smart Jump, Context Sensitive Help:  Semantic
  Semantic is the infrastructure upon which helpful context sensitive
  tools can be built.  Those tools include:
  * Smart Completion
  * Smart Help/Jump/Navigation
  * Symbol Reference tools

  The Semantic Manual can help setup and use a wide suite of these tools.
  For CEDET distributed independently of Emacs, see semantic-load.el

Code Generation, Template Insertion:  SRecode
  SRecode, or the Semantic Re-Coder is a template system for code generation.
  Templates can be used for code snippets, or to convert tags from
  Semantic back into code for applications.

  (global-srecode-minor-mode 1)

  to enable the SRecode Menu for code generation.

UML and other structured diagrams:  COGRE
  COGRE is a Connected Graph Editor.

  Use M-x cogre RET to create a new diagram using the keyboard and mouse.

  Use M-x cogre-uml-quick-class RET to generate a UML diagram from source
  code of your OO program.

  COGRE requires that the 'dot' program is installed for
  performing diagram layout.

CLOS For Emacs: EIEIO
  EIEIO is a CLOS clone for Emacs that allows you to write Emacs Lisp
   programs in an object oriented way.")
    (princ "\n\n")
    (cedet-version-print)
    (princ "\n\n\nC-h f cedet-version RET\n  for details on output format.")))

(defun cedet-version ()
  "Display all active versions of CEDET and Dependant packages.

The PACKAGE column is the name of a given package from CEDET.

REQUESTED VERSION is the version requested by the CEDET load script.
See `cedet-packages' for details.

FILE VERSION is the version number found in the source file
for the specified PACKAGE.

LOADED VERSION is the version of PACKAGE current loaded in Emacs
memory and (presumably) running in this Emacs instance.  Value is X
if the package has not been loaded."
  (interactive)
  (with-output-to-temp-buffer "*CEDET*"
    (cedet-version-print)
    (princ "\n\n\nC-h f cedet-version RET\n  for details on output format.")))

(defun cedet-version-print ()
  "Print the versions of CEDET packages to standard out.
See `cedet-version' for details."
  (princ "CEDET Version:\t") (princ cedet-version)
  (princ "\n  \t\t\tRequested\tFile\t\tLoaded")
  (princ "\n  Package\t\tVersion\t\tVersion\t\tVersion")
  (princ "\n  ----------------------------------------------------------")
  (let ((p cedet-packages))
    (while p
      (let ((sym (symbol-name (car (car p)))))
	(princ "\n  ")
	(princ sym)
	(princ ":\t")
	(if (< (length sym) 5)
	    (princ "\t"))
	(if (< (length sym) 13)
	    (princ "\t"))
	(let ((reqver (nth 1 (car p)))
	      (filever (car (inversion-find-version sym)))
	      (loadver (when (featurep (car (car p)))
			 (symbol-value (intern-soft (concat sym "-version"))))))
	  (princ reqver)
	  (if (< (length reqver) 8) (princ "\t"))
	  (princ "\t")
	  (if (string= filever reqver)
	      ;; I tried the words "check" and "match", but that
	      ;; just looked lame.
	      (princ "ok\t")
	    (princ filever)
	    (if (< (length filever) 8) (princ "\t")))
	  (princ "\t")
	  (if loadver
	      (if (string= loadver reqver)
		  (princ "ok")
		(princ loadver))
	    (princ "Not Loaded"))
	  ))
      (setq p (cdr p))))
  (princ "\n"))

(provide 'cedet)

;;; cedet.el ends here
