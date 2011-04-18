;;; semantic-ectag-lang.el --- Exuberent Ctags per-language support

;; Copyright (C) 2008, 2009, 2010 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; X-RCS: $Id: semantic-ectag-lang.el,v 1.12 2010/04/09 02:26:05 zappo Exp $

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
;; Support various languages via Exuberent CTags.
;;
;; Support requires:
;;  * Specification of tag 'kind' to get
;;  * Signature parsing.

(require 'semantic-fw)
(require 'semantic-ectag-parse)

;;; Code:
(defun semantic-ectag-simple-setup ()
  "Default way to add exuberent ctags support in a language hook.
Any mode that has `semantic-ectag-lang' and `semantic-ectag-lang-kind'
can be support with this simple setup."
  (semantic-ectag-setup-parse-table)
  (setq imenu-create-index-function 'semantic-create-imenu-index))

(defmacro semantic-ectag-add-language-support (mode name kinds)
  "Add simple language support via exuberent ctags.
MODE is the mode to support.
NAME is the exuberent ctags language name.
KINDS are the kinds of tags to generate from exuberent ctags."
  `(progn
       (defvar-mode-local ,mode semantic-ectag-lang ,name
	 "Language name for Exuberent CTags.")
       (defvar-mode-local ,mode semantic-ectag-lang-kind ,kinds
	 "Kinds of Exuberent CTags available.")))

;;; MODE SUPPORT
;;
(semantic-ectag-add-language-support sh-mode "sh" "f")
(semantic-ectag-add-language-support asm-mode "asm" "dlmt")
;(semantic-ectag-add-language-support asp-mode "asp" "cfsv")
;(semantic-ectag-add-language-support awk-mode "awk" "f")
(semantic-ectag-add-language-support basic-mode "basic" "cfltvg")
;(semantic-ectag-add-language-support cobol-mode "cobol" "dfgpPs")
;(semantic-ectag-add-language-support eiffel-mode "eiffel" "cfl")
(semantic-ectag-add-language-support fortran-mode "fortran" "fikpstv") ; L for local variable info.
;(semantic-ectag-add-language-support lua-mode "lua" "f")
(semantic-ectag-add-language-support pascal-mode "pascal" "fp")
(semantic-ectag-add-language-support perl-mode "perl" "cflpsd")
(semantic-ectag-add-language-support python-mode "python" "cfmvi")
;(semantic-ectag-add-language-support rexx-mode "rexx" "s")
;(semantic-ectag-add-language-support sql-mode "sql" "s")
(semantic-ectag-add-language-support tcl-mode "tcl" "cmp")
;(semantic-ectag-add-language-support vera-mode "vera" "cdfgmPTv")
;(semantic-ectag-add-language-support verilog-mode "verilog" "cfm")

;;; BUFFER PARSING HOOKS
;;
;; We cannot blindly enable the buffer support for languages that
;; can only get tags from ctags.  The user must enable them via this
;; fcn instead.

;;;###autoload
(defun semantic-load-enable-primary-exuberent-ctags-support ()
  "Enable all ectag supported parsers for new languages.
This is support for any language that does not have a regular
semantic parser."
  (interactive)

  ;; Make sure that the version of ctags installed will work.
  (semantic-ectag-test-version)

  ;; Mode Hooks for enabling parsing with ectag as the main parser.
  (add-hook 'sh-mode-hook 'semantic-ectag-simple-setup)

  ;; Support for the following is untested.  Once tested, move up
  ;; to the tested section.
  (add-hook 'asm-mode-hook 'semantic-ectag-simple-setup)
  ;;(add-hook 'basic-mode-hook 'semantic-ectag-simple-setup)
  (add-hook 'fortran-mode-hook 'semantic-ectag-simple-setup)
  ;;(add-hook 'lua-mode-hook 'semantic-ectag-simple-setup)
  (add-hook 'pascal-mode-hook 'semantic-ectag-simple-setup)
  (add-hook 'perl-mode-hook 'semantic-ectag-simple-setup)
  (add-hook 'python-mode-hook 'semantic-ectag-simple-setup)
  ;;(add-hook 'rexx-mode-hook 'semantic-ectag-simple-setup)
  (add-hook 'tcl-mode-hook 'semantic-ectag-simple-setup)
  ;;(add-hook 'vera-mode-hook 'semantic-ectag-simple-setup)
  ;;(add-hook 'verilog-mode-hook 'semantic-ectag-simple-setup)
  )

(provide 'semantic-ectag-lang)
;;; semantic-ectag-lang.el ends here
