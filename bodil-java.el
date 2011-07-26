;;; bodil-java.el -- Java configuration

;; Setup eclim
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/emacs-eclim/vendor"))
(require 'eclim)

(setq eclim-auto-save t)
(setq eclim-executable (expand-file-name "~/eclipse/eclim"))

;; Add eclim + yasnippet ac-source
(require 'auto-complete)
(defvar ac-eclim-yas-mark 'nil)
(defun ac-eclim-yas-candidates ()
  (let (lista)
    (with-no-warnings
      (when eclim-auto-save (save-buffer))
      (loop for c in (eclim/java-complete)
            do
            (if (not (equal (nth 0 c) "f"))
                (push (nth 1 c) lista)
              (let ((texto (nth 2 c))
                    aux)
                (push (concat 
                       (replace-regexp-in-string "^\\([^(]*(\\).*" "\\1" texto)
                       (replace-regexp-in-string "[^ <]+\\(?:<[^>]+>\\)? \\([^ ,]+\\)\\(,?\\)" "${\\1}\\2"
                                                 (replace-regexp-in-string "^[^(]*(\\([^)]*\\)).*$" "\\1"  texto))
                       ")")
                      lista)))))
    lista))
(defun yas/expand-eclim-snippet ()
  (let ((snip (buffer-substring-no-properties ac-eclim-yas-mark (point))))
    (delete-region ac-eclim-yas-mark (point))
    (yas/expand-snippet snip)))
(defun eclim-yas-init ()
  (setq ac-eclim-yas-mark (point)))
(ac-define-source eclim-yas
  '((candidates . ac-eclim-yas-candidates)
    (init . eclim-yas-init)
    ;; (requires . 0)
    ;; (prefix . c-dot)
    (action . yas/expand-eclim-snippet)
    (symbol . "f")))
(ac-define-source eclim-yas-dot
  '((candidates . ac-eclim-yas-candidates)
    (init . eclim-yas-init)
    (requires . 0)
    (prefix . c-dot)
    (action . yas/expand-eclim-snippet)
    (symbol . "f")))

;; Fix annotation indentation
(require 'java-mode-indent-annotations)

;; Hook
(add-hook 'java-mode-hook
          (lambda ()
            (eclim-mode t)
            (setq ac-sources '(ac-source-eclim-yas-dot ac-source-eclim-yas ac-source-yasnippet ac-source-filename))
            (java-mode-indent-annotations-setup)))

;; Bindings
(define-key eclim-mode-map (kbd "M-.") 'eclim-java-find-declaration)
(define-key eclim-mode-map (kbd "C-SPC") 'ac-start)

;; CEDET/Malabar setup
;; (setq semantic-default-submodes '(global-semantic-idle-scheduler-mode
;;                                   global-semanticdb-minor-mode
;;                                   global-semantic-idle-summary-mode
;;                                   global-semantic-mru-bookmark-mode))
;; (semantic-mode 1)
;; (require 'malabar-mode)
;; (setq malabar-groovy-lib-dir "~/.emacs.d/site-lisp/malabar/lib")
;; (add-to-list 'auto-mode-alist '("\\.java\\'" . malabar-mode))

;; Make Malabar's autoimport behave more like Eclipse
;; (defun malabar-eclipse-import ()
;;   "Eclipse style import handling."
;;   (interactive)
;;   (malabar-import-all)
;;   (malabar-import-group-imports))
;; (add-hook 'malabar-mode-hook
;;           (lambda () (local-set-key (kbd "C-c C-v z") 'malabar-eclipse-import)))

;; Autocompile on save
;; (add-hook 'malabar-mode-hook
;;           (lambda () (add-hook 'after-save-hook 'malabar-compile-file-silently nil t)))

;; Setup semantic sources for auto-complete
;; (add-hook 'malabar-mode-hook
;;           (lambda () (setq ac-sources '(ac-source-semantic ac-source-yasnippet))))

