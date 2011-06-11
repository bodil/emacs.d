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
(defun caps-mode-self-insert-command (&optional n)
  "Like `self-insert-command', but uppercase the the typed character."
  (interactive "p")
  (insert-char (upcase last-command-char) n))
(defvar caps-mode-map nil)
(when (fboundp 'define-minor-mode)
  (define-minor-mode caps-mode
    "Toggle caps mode.
With no argument, this command toggles the mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.
When caps mode is enabled, all letters are inserted in their
capitalized form."
    :init-value nil
    :lighter " OMGCAPS"
    (setq caps-mode-map
          (let ((map (make-sparse-keymap)))
            (substitute-key-definition 'self-insert-command
                                       'caps-mode-self-insert-command
                                       map global-map)
            map))
    (if caps-mode
        (add-to-list 'minor-mode-map-alist (cons 'caps-mode caps-mode-map))
      (setq minor-mode-map-alist
            (delete (assoc 'caps-mode minor-mode-map-alist)
                    minor-mode-map-alist)))))

(define-derived-mode lol-mode fundamental-mode
  "LOLCODE MODEZZZ"
  "TEH EMACZ MODE 4 EDIT0RZING LOLCODE."

  (setq font-lock-defaults '((lol-font-lock-keywords)))
  (caps-mode t)
  (setq ac-sources '(ac-source-lolcode ac-source-words-in-buffer ac-source-yasnippet)))

;; Any .lol or .lolz files open with this major mode
(setq auto-mode-alist (cons '("\\.lol$" . lol-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lolz$" . lol-mode) auto-mode-alist))

;; Define an auto-complete source for lolcode keywords
(require 'auto-complete-config)
(defvar ac-source-lolcode
  '((candidates . (append
                   lol-type-face-keywords
                   lol-preprocessor-face-keywords
                   lol-builtin-face-keywords
                   lol-logic-keywords
                   lol-win-fail
                   lol-function-face-keywords))))
(add-to-list 'ac-modes 'lol-mode)

