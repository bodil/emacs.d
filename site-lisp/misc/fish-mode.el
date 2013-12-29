(defcustom fish-mode-hook nil
  "Hook of fish-mode")

(defconst fish-font-lock-keywords-1
  (list
   '("\\<\\(a\\(?:lias\\|nd\\)\\|b\\(?:egin\\|g\\|ind\\|lock\\|reak\\(?:point\\)?\\|uiltin\\)\\|c\\(?:ase\\|d\\|o\\(?:m\\(?:mand\\(?:line\\)?\\|plete\\)\\|nt\\(?:ains\\|inue\\)\\|unt\\)\\)\\|d\
ir[hs]\\|e\\(?:cho\\|lse\\|mit\\|nd\\|val\\|x\\(?:ec\\|it\\)\\)\\|f\\(?:g\\|ish\\(?:_\\(?:config\\|indent\\|p\\(?:ager\\|rompt\\)\\|right_prompt\\|update_completions\\)\\|d\\)?\\|or\\|\
unc\\(?:ed\\|save\\|tions?\\)\\)\\|h\\(?:elp\\|istory\\)\\|i\\(?:f\\|satty\\)\\|jobs\\|m\\(?:ath\\|imedb\\)\\|n\\(?:extd\\|ot\\)\\|o\\(?:pen\\|r\\)\\|p\\(?:opd\\|revd\\|sub\\|\\(?:ush\\
\|w\\)d\\)\\|r\\(?:andom\\|e\\(?:ad\\|turn\\)\\)\\|s\\(?:et\\(?:_color\\)?\\|ource\\|tatus\\|witch\\)\\|t\\(?:est\\|rap\\|ype\\)\\|u\\(?:limit\\|mask\\)\\|vared\\|while\\)\\>"
    . font-lock-builtin-face)
   '("\\$\\([[:alpha:]_][[:alnum:]_]*\\)" . font-lock-variable-name-face)))

(defvar fish-mode-syntax-table
  (let ((fish-mode-syntax-table (make-syntax-table)))
    (modify-syntax-entry ?# "<")
    (modify-syntax-entry ?\n ">")
    (modify-syntax-entry ?\" "\"\"")
    (modify-syntax-entry ?\' "\"'")))

(defun fish-indent-line ()
  "Indent current line."
  (interactive)
  (beginning-of-line)

  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*end")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) tab-width)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*end")
                (progn
                  (forward-line -1)
                  (setq cur-indent (- (current-indentation) tab-width))
                  (if (< cur-indent 0)
                      (setq cur-indent 0))
                  (forward-line 1)
                  (indent-line-to cur-indent)
                  (setq not-indented nil))
              ; TODO: one-line function
              (if (looking-at "[ \t]*\\(begin\\|case\\|else\\|for\\|function \\|if\\|switch\\|while\\)")
                  (progn
                    (setq cur-indent (+ (current-indentation) tab-width))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
        (if cur-indent
            (indent-line-to cur-indent)
          (indent-line-to 0)))))

(defun fish-mode ()
  (interactive)
  (kill-all-local-variables)
  (setq-local indent-line-function 'fish-indent-line)
  (setq-local font-lock-defaults '(fish-font-lock-keywords-1))
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+[\t ]*")
  (run-hooks 'fish-mode-hook))

(add-to-list 'auto-mode-alist '("\\.fish\\'" . fish-mode))
(add-to-list 'interpreter-mode-alist '("fish" . fish-mode))

(provide 'fish-mode)
