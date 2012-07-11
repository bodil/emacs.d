;;; buster-snippet-helpers.el --- Making snippet code look snazzy since 2011

;; Copyright (C) 2011 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: snippets

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Helper methods used in buster-snippets

;;; Code:

(defun chop-suffix (suffix s)
  "Remove string 'suffix' if it is at end of string 's'"
  (let ((pos (- (length suffix))))
    (if (and (>= (length s) (length suffix))
             (string= suffix (substring s pos)))
        (substring s 0 pos)
      s)))

(defun chop-test-suffix (s)
  "Remove -test or _test from end of S"
  (chop-suffix "-test" (chop-suffix "_test" s)))

(defun buffer-file-name-body ()
  "Buffer file name stripped of directory and extension"
  (if (buffer-file-name)
      (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))
    (cadr (reverse (split-string (dired-current-directory) "/")))))

(defun split-name (s)
  "Split name into list of words"
  (split-string
   (let ((case-fold-search nil))
     (downcase
      (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1 \\2" s)))
   "[^A-Za-z0-9]+"))

(defun mapcar-head (fn-head fn-rest list)
  "Like MAPCAR, but applies a different function to the first element."
  (if list
      (cons (funcall fn-head (car list)) (mapcar fn-rest (cdr list)))))

(defun lower-camel-case (s)
  "Convert string 's' to camelCase string."
  (mapconcat 'identity (mapcar-head
                        '(lambda (word) (downcase word))
                        '(lambda (word) (capitalize (downcase word)))
                        (split-name s)) ""))

(defun upper-camel-case (s)
  "Convert string 's' to CamelCase string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-name s)) ""))

(defun snake-case (s)
  "Convert string 's' to snake_case string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (downcase word))
                        (split-name s)) "_"))

(defun capitalized-words (s)
  "Convert string 's' to Capitalized Words string."
  (mapconcat 'identity (mapcar
                        '(lambda (word) (capitalize (downcase word)))
                        (split-name s)) " "))

(defun comma-if-looking-at-whitespace-and-quotes ()
  (if (looking-at "\\(\\s \\|\n\\)+\"") "," ""))

;; Guess lib-folder
(defun buster--guess-lib-folder ()
  (if (buffer-file-name)
      (let ((test-dir (file-name-directory (buffer-file-name))))
        (if (string-match-p "/test/" test-dir)
            (let ((lib-dir (buster--lib-folder-with-same-nesting test-dir)))
              (if (file-exists-p lib-dir) lib-dir))))))

(defun buster--lib-folder-with-same-nesting (test-dir)
  (let ((lib-dir (replace-regexp-in-string ".+/test/\\(.*\\)" "lib/\\1" test-dir)))
    (concat (buster--path-out-of-test lib-dir) lib-dir)))

(defun buster--path-out-of-test (lib-dir)
  (mapconcat 'identity (mapcar
                        '(lambda (word) "../")
                        (split-string lib-dir "/" t)) ""))

;; Shortcut globals for iife
(defun buster--shortcuts-for-globals (globals)
  (mapconcat 'identity (mapcar
                        'buster--global-shortcut
                        (buster--params globals)) ", "))

(defun buster--last-word (s)
  (car (last (split-string s "\\." t))))

(defun buster--first-char (s)
  (car (split-string s "" t)))

(defun buster--global-shortcut (s)
  (if (string-match-p "jquery" s)
      "$"
    (upcase (buster--first-char (buster--last-word s)))))

(defun buster--params (s)
  (split-string s "[, ]" t))

;; Maybe use strict
(defun buster--maybe-use-strict ()
  (if buster-use-strict
      "\"use strict\";\n\n"
    ""))

;; Maybe add local asserts
(defun buster--maybe-add-local-asserts ()
  (if (not buster-exposed-asserts)
      "var assert = buster.assert;\nvar refute = buster.refute;\n\n"
    ""))

(provide 'buster-snippet-helpers)
;;; buster-snippet-helpers.el ends here
