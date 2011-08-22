(defface show-tag-match-face
  '((t (:background "#333355")))
  "Highlight matching tags.")

(defvar hl-tags-start-overlay nil)
(make-variable-buffer-local 'hl-tags-start-overlay)

(defvar hl-tags-end-overlay nil)
(make-variable-buffer-local 'hl-tags-end-overlay)

(defun hl-tags-context ()
  (save-excursion
    (let ((ctx (sgml-get-context)))
      (and ctx
           (if (eq (sgml-tag-type (car ctx)) 'close)
               (cons (sgml-get-context) ctx)
             (cons ctx (progn
                         (sgml-skip-tag-forward 1)
                         (backward-char 1)
                         (sgml-get-context))))))))

(defun hl-tags-update ()
  (let ((ctx (hl-tags-context)))
    (if (null ctx)
        (hl-tags-hide)
      (hl-tags-show)
      (move-overlay hl-tags-end-overlay
                    (sgml-tag-start (caar ctx))
                    (sgml-tag-end (caar ctx)))
      (move-overlay hl-tags-start-overlay
                    (sgml-tag-start (cadr ctx))
                    (sgml-tag-end (cadr ctx))))))

(defun hl-tags-show ()
  (unless hl-tags-start-overlay
    (setq hl-tags-start-overlay (make-overlay 1 1)
          hl-tags-end-overlay (make-overlay 1 1))
    (overlay-put hl-tags-start-overlay 'face 'show-tag-match-face)
    (overlay-put hl-tags-end-overlay 'face 'show-tag-match-face)))

(defun hl-tags-hide ()
  (when hl-tags-start-overlay
    (delete-overlay hl-tags-start-overlay)
    (delete-overlay hl-tags-end-overlay)))

(define-minor-mode hl-tags-mode
  "Toggle hl-tags-mode."
  nil "" nil
  (if hl-tags-mode
      (add-hook 'post-command-hook 'hl-tags-update nil t)
    (remove-hook 'post-command-hook 'hl-tags-update t)
    (hl-tags-hide)))

(provide 'hl-tags-mode)
