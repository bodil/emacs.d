;;; lolcode.el -- LOLCODE major mode

(require 'auto-complete-config)

(defgroup lol-mode nil
  "A CoffeeScript major mode."
  :group 'languages)

(defcustom lol-interpreter-command "lci"
  "The LOLCODE interpreter to use. Must be on your path and accept source input on stdin."
  :type 'string
  :group 'lol-mode)

(defcustom lol-output-buffer-name "*LOLCODE-OUTPUT*"
  "The name of the scratch buffer used when executing LOLCODE."
  :type 'string
  :group 'lol-mode)



(defvar lol-type-face-keywords
  '("I HAS A" "CAN HAS" "R" "ITZ")
  "")

(defvar lol-preprocessor-face-keywords
  '("HAI" "KTHXBYE")
  "")

(defvar lol-builtin-face-keywords
  '("IM IN YR" "IM OUTTA YR" "O RLY" "YA RLY" "NO WAI" "OIC" "GTFO" "MEBBE BOTH OF" "OMG" "WTF" "OMGWTF" "HOW DUZ I" "IF U SAY SO" "YR")
"")

(defvar lol-logic-keywords
  '("WON" "NOT" "AN" "BOTH SAEM" "SUM" "PRODUKT" "DIFF" "QUOSHUNT" "OF" "MOD" "BIGGR" "SMALLR" "SMOOSH" "MKAY" "MAEK" "A NOOB" "IS NOW A NOOB" "DIFFRINT")
  "")

(defvar lol-win-fail
  '("WIN" "FAIL")
  "")

(defvar lol-function-face-keywords
  '("VISIBLE" "GIMMEH" "BTW" "OBTW" "TLDR")
  "")

(defvar lol-type-face-regexp (regexp-opt lol-type-face-keywords 'words))
(defvar lol-builtin-face-regexp (regexp-opt lol-builtin-face-keywords 'words))
(defvar lol-function-face-regexp (regexp-opt lol-function-face-keywords 'words))
(defvar lol-preprocessor-face-regexp (regexp-opt lol-preprocessor-face-keywords 'words))
(defvar lol-win-fail-regexp (regexp-opt lol-win-fail 'words))
(defvar lol-logic-regexp (regexp-opt lol-logic-keywords 'words))

(setq lol-font-lock-keywords
      `(
	(,lol-preprocessor-face-regexp . font-lock-preprocessor-face)
	(,lol-type-face-regexp . font-lock-type-face)
	(,lol-logic-regexp . font-lock-builtin-face)
	(,lol-builtin-face-regexp . font-lock-keyword-face)
	(,lol-function-face-regexp . font-lock-function-name-face)
	(,lol-win-fail-regexp . font-lock-variable-name-face)
))

;; omg caps!
(defun lol-mode-self-insert-command (&optional n)
  "Like `self-insert-command', but uppercase the the typed character."
  (interactive "p")
  (insert-char (upcase last-command-char) n))

;; Execute the current buffer using lci
(defun lol-execute-buffer ()
  (interactive)
  (save-excursion
    (lol-execute-region (point-min) (point-max))))

(defun lol-execute-region (start end)
  (interactive "r")

  (let ((buffer (get-buffer lol-output-buffer-name)))
    (when buffer
      (kill-buffer buffer)))

  (call-process-region start end lol-interpreter-command nil
                       (get-buffer-create lol-output-buffer-name)
                       nil
                       "-")
  (let ((buffer (get-buffer lol-output-buffer-name)))
    (display-buffer buffer)))

(defun lol-execute-buffer-or-region ()
  (interactive)
  (if (region-active-p)
      (lol-execute-region (mark) (point))
    (lol-execute-buffer)))

;; Mode maps.
(defvar lol-mode-map
  (let ((map (make-sparse-keymap)))
    (substitute-key-definition 'self-insert-command
                               'lol-mode-self-insert-command
                               map global-map)
    (define-key map (kbd "C-c C-c") 'lol-execute-buffer-or-region)
    map)
  "Keymap used in Haskell mode.")

(define-derived-mode lol-mode fundamental-mode
  "LOLCODE MODEZZZ"
  "TEH EMACZ MODE 4 EDIT0RZING LOLCODE."

  (setq font-lock-defaults '((lol-font-lock-keywords)))
  (set (make-local-variable 'comment-start) "BTW "))

;; Any .lol or .lolz files open with this major mode
(setq auto-mode-alist (cons '("\\.lol$" . lol-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lolz$" . lol-mode) auto-mode-alist))

;; Define an auto-complete source for lolcode keywords
(defvar ac-source-lolcode
  '((candidates . (append
                   lol-type-face-keywords
                   lol-preprocessor-face-keywords
                   lol-builtin-face-keywords
                   lol-logic-keywords
                   lol-win-fail
                   lol-function-face-keywords))))
(add-to-list 'ac-modes 'lol-mode)

(add-hook 'lol-mode-hook (lambda () (setq ac-sources '(ac-source-lolcode ac-source-words-in-buffer ac-source-yasnippet))))
