;;; magit.el -- Git better

(autoload 'magit-status "magit.el" nil t)
(autoload 'magit-get-top-dir "magit.el" nil t)
(global-set-key (kbd "C-x g") 'magit-status)


;; Utility functions
(defcustom git-project-files-filter-history-variable 'git-project-files-filter-history
  "History list to use for the regexp filter for selecting git project files.
The value of this variable should be a symbol; that symbol
is used as a variable to hold a history list for replacement
strings or patterns."
  :group 'matching
  :type 'symbol)

(defun git-current-root ()
  "Get the root of the current git tree."
  (magit-get-top-dir (file-name-directory (or load-file-name buffer-file-name))))

(defun git-project-files ()
  "List all files in the current git tree, excluding ignored files."
  (let ((top-dir (git-current-root)))
    (remove (replace-regexp-in-string "/$" "" top-dir)
            (mapcar
             (lambda (elt) (expand-file-name elt top-dir))
             (split-string (let ((default-directory top-dir))
                             (shell-command-to-string "git ls-files -co -X .gitignore")) "\n")))))

(defun git-project-files-by-regexp ()
  "List all files in the current git tree, excluding ignored files,
filtered by a user-provided regexp."
  (let ((default-filter (if (boundp git-project-files-filter-history-variable)
                            (or (car (symbol-value git-project-files-filter-history-variable)) "")
                          "")))
    (let ((filter-regexp
           (read-from-minibuffer (format "Filter project files on regexp: [%s] " default-filter)
                                 nil nil nil git-project-files-filter-history-variable nil t)))
      (if (zerop (length filter-regexp)) (setq filter-regexp default-filter))
      (add-to-history git-project-files-filter-history-variable filter-regexp)
      (delq nil (mapcar (lambda (path) (if (null (string-match filter-regexp path))
                                      nil path)) (git-project-files))))))

;; Find file in Git repo using ido
(defun ido-find-file-in-git-tree--prepare-key (path)
  (replace-regexp-in-string "\\(|\\|/\\)$" ""
                            (replace-regexp-in-string top-dir ""
                                                      (replace-regexp-in-string "\\(.*?\\)\\([^/]+?\\)$" "\\2|\\1" path))))
(defun ido-find-file-in-git-tree ()
  "Use ido to select a file from the current git tree."
  (interactive)
  (let ((top-dir (git-current-root))
        (project-files (git-project-files))
        (tbl (make-hash-table :test 'equal))
        (ido-list '()))
    (mapc (lambda (path)
            (let ((key (ido-find-file-in-git-tree--prepare-key path)))
              (puthash key path tbl)
              (push key ido-list)))
          project-files)
    (find-file (gethash (ido-completing-read "project-files: " ido-list) tbl))))
(global-set-key (kbd "C-c C-f") 'ido-find-file-in-git-tree)
;; unbind conflicting key in dirty nxml-mode
(eval-after-load "nxml-mode"
  '(define-key nxml-mode-map "\C-c\C-f" nil))

;; Repo-wide query-replace-regexp
(defun query-replace-regexp-in-git-tree (from-string to-string &optional delimited start end)
  "Perform a query-replace-regexp on all files in the current git tree."
  (interactive
   (let ((common
          (query-replace-read-args
           (concat "Query replace" (if current-prefix-arg " word" "")) nil)))
     (list (nth 0 common) (nth 1 common) (nth 2 common))))
  (save-excursion
    (let ((file-list (git-project-files-by-regexp)))
      (dolist (file file-list)
        (let ((buffer (get-file-buffer file)))
          (if (and buffer (with-current-buffer buffer buffer-read-only))
              (error "File `%s' is visited read-only" file))))
      (tags-query-replace from-string to-string delimited 'file-list))))

(provide 'bodil-magit)
