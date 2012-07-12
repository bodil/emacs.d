;;; js2-highlight-vars.el --- highlight occurrences of the variable under cursor

;; Copyright (C) 2009  Free Software Foundation, Inc.

;; Author:  Mihai Bazon <mihai.bazon@gmail.com>

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

(require 'js2-mode)

(js2-deflocal js2-highlight-vars-mode nil)

(defface js2-highlight-vars-face
  `((((class color) (background light))
     (:background "light green"))
    (((class color) (background dark))
     (:background "royal blue")))
  "Face for highlighting variables"
  :group 'js2-mode)

(defface js2-highlight-vars-second-face
  `((((class color) (background light))
     (:background "light pink"))
    (((class color) (background dark))
     (:background "blue violet")))
  "Face for highlighting variables"
  :group 'js2-mode)

(defvar js2-highlight-vars-local-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n")       'js2-highlight-vars-next)
    (define-key map (kbd "C-<down>")  'js2-highlight-vars-next)
    (define-key map (kbd "M-p")       'js2-highlight-vars-prev)
    (define-key map (kbd "C-<up>")    'js2-highlight-vars-prev)
    (define-key map (kbd "M-r")       'js2-highlight-vars-rename)
    map))

(js2-deflocal js2--highlight-vars-tokens nil)
(js2-deflocal js2--highlight-vars-current-token nil)
(js2-deflocal js2--highlight-vars-current-token-name nil)
(js2-deflocal js2--highlight-vars-post-command-timer nil)

(defun js2--do-highlight-vars ()
  "Highlight variable under cursor within the defining scope"
  (interactive)
  (setq js2--highlight-vars-post-command-timer nil)
  (unless js2--highlight-vars-tokens
    (let ((node (js2-node-at-point))
          (tokens nil)
          name
          scope)
      (unless (js2-name-node-p node)
        (setq node (js2-node-at-point (- (point) 1))))
      (when (and node (js2-name-node-p node))
        (setq scope (js2-node-get-enclosing-scope node)
              name (js2-name-node-name node)
              js2--highlight-vars-current-token (js2-node-abs-pos node)
              js2--highlight-vars-current-token-name name)
        (setq scope (js2-get-defining-scope scope name))
        (js2-with-unmodifying-text-property-changes
          (js2-visit-ast
           scope
           (lambda (node end-p)
             (when (and (not end-p)
                        (js2-name-node-p node)
                        (string= name (js2-name-node-name node)))
               (let* ((beg (js2-node-abs-pos node))
                      (end (+ beg (js2-node-len node)))
                      (new-scope (js2-node-get-enclosing-scope node))
                      (new-scope (js2-get-defining-scope new-scope name))
                      (ovl (make-overlay beg end)))
                 (add-to-list 'tokens beg t)
                 (overlay-put ovl 'keymap js2-highlight-vars-local-keymap)
                 (overlay-put ovl 'face
                              (if (eq new-scope scope)
                                  'js2-highlight-vars-face
                                'js2-highlight-vars-second-face))
                 (overlay-put ovl 'evaporate t)
                 (overlay-put ovl 'js2-highlight-vars t)))
             t)))
        (setq js2--highlight-vars-tokens tokens)
        (top-level)))))

(defun js2-highlight-vars-next ()
  (interactive)
  (let ((inhibit-point-motion-hooks t)
        (diff (- (point) js2--highlight-vars-current-token))
        (next (catch 'done
                (dolist (pos js2--highlight-vars-tokens)
                  (when (> pos (point))
                    (throw 'done pos))))))
    (when next
      (setq js2--highlight-vars-current-token next)
      (goto-char next)
      (forward-char diff))))

(defun js2-highlight-vars-prev ()
  (interactive)
  (let ((inhibit-point-motion-hooks t)
        (diff (- (point) js2--highlight-vars-current-token))
        (prev (catch 'done
                (dolist (pos (reverse js2--highlight-vars-tokens))
                  (when (and (< pos (point))
                             (not (= pos js2--highlight-vars-current-token)))
                    (throw 'done pos))))))
    (when prev
      (setq js2--highlight-vars-current-token prev)
      (goto-char prev)
      (forward-char diff))))

(defun js2-highlight-vars-rename (new-name)
  (interactive "*sRename variable to: ")
  (let ((len (length js2--highlight-vars-current-token-name))
        (inhibit-point-motion-hooks t)
        (ovl (make-overlay 1 1))
        (all nil)
        doit)
    (overlay-put ovl 'face 'highlight)
    (dolist (pos (mapcar (lambda(pos)
                           (let ((m (make-marker)))
                             (set-marker m pos))) js2--highlight-vars-tokens))
      (goto-char pos)
      (move-overlay ovl pos (+ pos len))
      (setq doit (if all
                     ?y
                   (read-char "Replace this occurrence? (y/n/!)")))
      (when (= doit ?!)
        (setq all t
              doit ?y))
      (when (= doit ?y)
        (insert new-name)
        (delete-char len)))
    (delete-overlay ovl)))

(defun js2--unhighlight-vars (&rest ignore)
  (setq js2--highlight-vars-tokens nil
        js2--highlight-vars-current-token nil)
  (remove-overlays (point-min) (point-max)
                   'js2-highlight-vars t))

(defun js2-highlight-vars-post-command-hook ()
  (ignore-errors
    (let* ((overlays (overlays-at (point)))
           (ovl (and overlays
                     (catch 'found
                       (dolist (ovl overlays)
                         (when (overlay-get ovl 'js2-highlight-vars)
                           (throw 'found ovl)))
                       nil))))
      (if (and ovl
               (string= js2--highlight-vars-current-token-name
                        (buffer-substring (overlay-start ovl)
                                          (overlay-end ovl))))
          (setq js2--highlight-vars-current-token (overlay-start ovl))
        (js2--unhighlight-vars)
        (when js2--highlight-vars-post-command-timer
          (cancel-timer js2--highlight-vars-post-command-timer))
        (setq js2--highlight-vars-post-command-timer
              (run-with-timer 0.5 nil 'js2--do-highlight-vars))))))

;;;###autoload
(define-minor-mode js2-highlight-vars-mode
  "Minor mode that highlights occurrences of the variable under
cursor in js2-mode buffers"
  nil " vars" nil
  (if js2-highlight-vars-mode
      (progn
        (add-hook 'post-command-hook 'js2-highlight-vars-post-command-hook nil t))
    (remove-hook 'post-command-hook 'js2-highlight-vars-post-command-hook t)
    (js2--unhighlight-vars)
    (kill-local-variable js2--highlight-vars-tokens)
    (kill-local-variable js2--highlight-vars-current-token)))

(provide 'js2-highlight-vars)
