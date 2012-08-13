;;; pytest.el --- integration with py.test
;;; version: 0.1

;; Copyright (C) 2011 Antonio Cuni
;; Author: Antonio Cuni <anto.cuni@gmail.com>

;; Description
;; -----------
;;
;; This file contains some useful functions to run py.test from within emacs.
;;
;; If you are editing a test file (i.e., a file named test_*.py), you can run
;; pytest-run-file to start py.test on that file.  You will be able to edit
;; the actual py.test command to run in the minibuffer, so you can add all the
;; flags you want (e.g., -s or --pdb).
;;
;; If you are editing a non-test file, pytest-run-file by default will start
;; py.test on the directory where the file is in.
;;
;; If you are editing a test file and want to run the very specific test you
;; are editing, you can run pytest-run-method, which will add the
;; corresponding "-k" option to py.test.  You can still edit the command in
;; the minibuffer before running it.
;;
;; If you want to re-run the last py.test command, you can use
;; pytest-run-again.  By default, pytest-run-again does not ask you to edit
;; the minibuffer, but you can still do it by prefixing pytest-run-again with
;; C-u.
;;
;; The py.test process is run inside an ansi term provided by term.el: this
;; means that if you use --pdb or explicitly have a pdb.set_trace() in your
;; code, the (Pdb) prompt will "just work", including the colored output and
;; the TAB-completion provided by e.g. pdb++.
;;
;; Default keybindings
;; -------------------
;;
;; By default, the functions are bound to the "C-x t" map, which is unused in
;; the default emacs settings.
;;
;; key             binding
;; ---             -------
;; C-x t f         pytest-run-file
;; C-x t m         pytest-run-method
;; C-x t t         pytest-run-again
;; C-u C-x t t     pytest-run-again (after editing in the minibuffer)


(require 'term)

;; (defvar ctl-x-t-map (make-sparse-keymap)
;;   "Keymap for subcommands of C-x t.")
;; (define-key ctl-x-map "t" ctl-x-t-map)
;; (define-key ctl-x-t-map "t" 'pytest-run-again)
;; (define-key ctl-x-t-map "f" 'pytest-run-file)
;; (define-key ctl-x-t-map "m" 'pytest-run-method)

(defvar pytest-run-history nil)

(defconst pytest-def-re "def \\(test_[A-Za-z0-9_]+\\)")

(defun pytest-term-sentinel (proc msg)
  (term-sentinel proc msg)
  (when (memq (process-status proc) '(signal exit))
    (with-selected-window (get-buffer-window "*pytest*")
      (setq buffer-read-only t)
      (local-set-key "q" 'quit-window)
      (local-set-key "g" 'pytest-run-again)
      (goto-char (point-max))
      (when (> (count-lines (point-min) (point-max))
               (window-total-height))
        (scroll-down (- (/ (window-total-height) 2) 2))))))

(defun pytest-run (cmdline show-prompt)
  (let ((cmdline (if show-prompt
                     (read-shell-command "Run: " cmdline
                                         'pytest-run-history)
                   cmdline))
        (buffer (get-buffer-create "*pytest*")))
    (add-to-list 'pytest-run-history cmdline)
    (display-buffer buffer)
    (with-current-buffer buffer
      (if (get-buffer-process (current-buffer))
          (term-kill-subjob))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert cmdline)
      (newline)
      (term-ansi-make-term "*pytest*" "/bin/sh" nil "-c" cmdline)
      (term-char-mode)
      (let ((proc (get-buffer-process buffer)))
        ; override the default sentinel set by term-ansi-make-term
        (set-process-sentinel proc 'pytest-term-sentinel)))))


;; (defun pytest-arg-from-path (path)
;;   (let ((filename (file-name-nondirectory path)))
;;     (if (or (string-match "test_.*\\.py$" filename)
;;             (string-match ".*_test\\.py$" filename))
;;         path
;;       (file-name-directory path))))

(defun pytest-current-function-name ()
  (save-excursion
    (if (search-backward-regexp pytest-def-re)
        (match-string 1)
      nil)))

(defun pytest-run-file ()
  "Run py.test on the current file."
  (interactive)
  (let ((cmdline (format "cd; py.test %s "
                         ;(pytest-arg-from-path (buffer-file-name))
                         (buffer-file-name))))
    (save-buffer)
    (pytest-run cmdline nil)))

(defun pytest-run-method ()
  "Run py.test on the current test.

If invokes py.test by adding \"-k funcname\", where funcname is the
name of the test_* function you are editing.
"
  (interactive)
  (let ((cmdline (format "cd; py.test %s -k %s "
                         ;(pytest-arg-from-path (buffer-file-name))
                         (buffer-file-name)
                         (pytest-current-function-name))))
    (pytest-run cmdline t)))

(defun pytest-run-again ()
  "Re-run the last py.test command.

If prefixed by C-u, it lets you to edit the command in the
minibuffer before executing it.
"
  (interactive)
  (if (not pytest-run-history)
      (message "No preceding pytest commands in history")
    (let ((cmdline (car pytest-run-history))
          (show-prompt (equal current-prefix-arg '(4))))
      (pytest-run cmdline show-prompt))))

(provide 'pytest)
