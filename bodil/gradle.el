;;; gradle.el --- Run gradle from emacs

;; Copyright (C) 2012 Vedat Hallac

;; Authors: Vedat Hallac
;; Version: 1.0.2
;; Created: 2012/05/11
;; Keywords: gradle

;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

(require 'compile)
(require 'crm)
(require 'cl)

(defgroup gradle nil
  "gradle customizations"
  :prefix "gradle-"
  :group 'tools)

(defcustom gradle-executable "gradle"
  "If gradle is not in `exec-path', this should contain the full
path to gradle executable."
  :type 'string
  :group 'gradle)

(defcustom gradle-auto-discover-tasks t
  "Control if gradle tasks should be discovered automatically the first time.
When nil, the gradle tasks must be discovered explicitly using
the `gradle-discover-tasks'. Otherwise, `gradle-run' will
automatically run `gradle-discover-tasks' when it is executed the
first time for each directory it is executed in."
  :type 'boolean
  :group 'gradle)

(defcustom gradle-with-project-root-func 'gradle--with-current-directory
  "Function used to switch to project root.
 For custom functions, the specified function should accept a
single lambda argument, change current directory to the project's
root directory, and execute the argument."
  :tag "Gradle Execute in Project Root Function"
  :type '(choice (function-item :tag "Use current directory" gradle--with-current-directory)
                 (function-item :tag "Use eclim" gradle--with-eclim-project-root)
                 (function-item :tag "Use project-root" gradle--with-project-root-project-root)
                 (function-item :tag "Use eproject" gradle--with-eproject-project-root)
                 (function-item :tag "Custom function"))
  :group 'gradle)

(defvar gradle-options nil
  "The cache of gradle command line options.")

(defvar gradle-tasks-for-path nil
  "The cache of gradle task lists for each project root.
The elements of this list are cons pairs where the car is the
path and the cdr is the list of tasks.")

(defvar gradle-run-history nil
  "The history list for gradle-run inputs")

(defconst gradle-task-indent-amount 4
  "The number of spaces used to indent dependent tasks.
This value may need modification only if the output of 'gradle
tasks' command changes in a future release.")

(defmacro gradle--with-project-root (&rest body)
  "Execute BODY with current directory set to project root.
The project root is determined according to
`gradle-with-project-root-func' custom variable."
  `(,gradle-with-project-root-func
    '(lambda () ,@body)))

(defun gradle--with-current-directory (func)
  (funcall func))

(defun gradle--with-eclim-project-root (func)
  "Execute `func' inside eclim's project root"
  (eval-when-compile
    (require 'eclim))
  (let ((default-directory (eclim--project-dir)))
    (funcall func)))

(defun gradle--with-project-root-project-root (func)
  "Execute `func' inside eclim's project root"
  (eval-when-compile
    (require 'project-root))
  (with-project-root
   (funcall func)))

(defun gradle--with-eproject-root (func)
  "Execute `func' inside eclim's project root"
  (eval-when-compile
    (require 'eproject))
  (let ((default-directory (eproject-root)))
    (funcall func)))

(defun gradle--executable-path ()
  (executable-find gradle-executable))

(defun gradle--parse-options (help-output)
  "Parse the output from 'gradle --help', and generate a list of options."
  (mapcan '(lambda (line)
             (let ((start 0))
               (loop while (string-match "\\(--[^,[:space:]]+\\)" line start)
                     do (setq start (match-end 0))
                     collecting (match-string 1 line))))
          (split-string output "\n+")))

(defun gradle--cache-options ()
  "Run \"gradle --help\", parse the output, and cache the result.
This function stores the list of command line options in
`gradle-options' variable only once. Since gradle command line
options are independent of the build file, there is no way to
reconstruct the cache."
  (when (null gradle-options)
    (let* ((output (shell-command-to-string
                    (concat (gradle--executable-path) " --help"))))
      (setq gradle-options (gradle--parse-options output)))))

(defun gradle--get-options ()
  "Return a list of gradle command line options."
  (unless gradle-options
    (gradle--cache-options))
  gradle-options)

(defun gradle--split-tasks-with-headings (tasks-output)
  "Parse the output from 'gradle tasks', and split the result into a more suitable form.
The output will be a list of lists, where each element of the
list is a task group descriptor, and each descriptor is list
where the first element is the group header, and the second
element is a string describing the tasks."
  (mapcar '(lambda (str)
             (let ((split-str (split-string str "\n-+\n")))
               (when (= 2 (length split-str))
                 split-str)))
          (split-string tasks-output "\n\n")))

(defun gradle--get-tasks (tasks-output-lines)
  "Extract a list of tasks from a partial output line list."
  (let ((prev-have-desc nil)
        (prev-indent 0))
    (delq nil
          (mapcar
           '(lambda (line)
              ;; This is disgusting, really. The whole parsing is made quite
              ;; complicated due to two tings: descriptions can have multiple
              ;; lines with random number of spaces before first word, and
              ;; dependent tasks can be indented.
              ;; What we do here is try to figure out which lines are continuing
              ;; descriptions, and which lines are task specifiers.
              (let ((cur-indent (if (string-match "^[[:space:]]+" line)
                                    (match-end 0)
                                  0))
                    (have-desc (string-match "^[[:space:]]*[^[:space:]]+ - " line)))
                (when (or (not prev-have-desc)
                          (and (<= cur-indent (+ prev-indent
                                                 gradle-task-indent-amount))
                               (= 0 (mod cur-indent gradle-task-indent-amount))))
                  (setq prev-have-desc have-desc)
                  (setq prev-indent cur-indent)
                  (when (string-match "^[[:space:]]*\\([^[:space:]]+\\)" line)
                    (match-string 1 line)))))
           tasks-output-lines))))

(defun gradle--parse-tasks (tasks-output)
  "Parse the output from 'gradle tasks', and generate a list of tasks."
  (mapcan '(lambda (desc)
             (when (and desc
                        (cdr desc)
                        (string-match "\\([[:alpha:][:space:]]+\\) tasks" (car desc)))
               (gradle--get-tasks (split-string (cadr desc) "\n+"))))
          (gradle--split-tasks-with-headings tasks-output)))

(defun gradle--cache-task-list (root)
  "Run gradle, get a list of tasks, parse the output, and cache the result.
This function stores the list of tasks associated with the
specified directory, ROOT."
  (message "Running gradle. Please wait...")
  ;; I need to run 'gradle tasks' twice in the code below. For
  ;; multi-project setups the output of 'gradle tasks --all' does not
  ;; contain ouput of 'gradle tasks', so I need to execute both to get
  ;; the full list.
  ;;
  ;; I may need to revisit this code to eliminate one of the
  ;; executions for single project setups, but the extra wait should
  ;; be OK for now.
  (let* ((default-directory root)
         (gradle-cmdline (concat (gradle--executable-path) " -q --no-color tasks"))
         (output (shell-command-to-string gradle-cmdline))
         (tasks (gradle--parse-tasks output))
         (output-all (shell-command-to-string (concat gradle-cmdline " --all")))
         (tasks-all (gradle--parse-tasks output-all))
         (combined-tasks (delete-dups (append tasks tasks-all)))
         (old-cache (assoc root gradle-tasks-for-path)))
    (when old-cache
      (delq old-cache gradle-tasks-for-path))
    (add-to-list 'gradle-tasks-for-path (cons root combined-tasks))
    (message "Done")
    combined-tasks))

(defun gradle--get-task-list ()
  "Return the task list associated with the project root.
The project root discovery is controlled by the custom variable
`gradle-with-project-root-func'."
  (gradle--with-project-root
   (let* ((task-list (assoc default-directory gradle-tasks-for-path)))
     (if (and gradle-auto-discover-tasks
              (null task-list))
         (gradle--cache-task-list default-directory)
       (cdr task-list)))))

(defun gradle-discover-tasks ()
  "Rerun gradle to get a list of tasks, parse the output and cache the results.
This function uses the current project root to run gradle. The
project root discovery is controlled by the custom variable
`gradle-with-project-root-func'."
  (interactive)
  (gradle--with-project-root
   (gradle--cache-task-list default-directory))
  nil)

(defun gradle--input-commandline ()
  (let ((crm-separator " ")
        (crm-local-completion-map (copy-keymap crm-local-completion-map)))
    (define-key crm-local-completion-map " " 'self-insert-command)
    (completing-read-multiple "gradle "
                              (append (gradle--get-options)
                                      (gradle--get-task-list))
                              nil nil nil gradle-run-history)))

(defun gradle-run (tasks)
  (interactive (list (gradle--input-commandline)))
  (gradle--with-project-root
   (compile (concat (gradle--executable-path)
                    (when tasks
                      (mapconcat 'identity (cons "" tasks) " "))))))

(provide 'gradle)
