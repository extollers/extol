(defvar extol-mode-hook nil)

(defvar extol-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-c C-c") 'comment-region)
    map)
  "Keymap for Extol major mode")

(add-to-list 'auto-mode-alist '("\\.xtl\\'" . extol-mode))

(defvar extol-font-lock-keywords
  (list
   (cons (concat "\\(\\<" (regexp-opt '("pred" "dcg" "dcg2" "contract" "test" "fun" "define")) "\\>\\|[!:]\\)") font-lock-keyword-face)
   '("\\(\\<\\(0'\\\\?.\\|[0-9]+\\)\\>\\|'[^']+'\\)" . font-lock-constant-face)
   '("\\(\\<[a-z][A-Za-z0-9_$]*\\>\\)" . font-lock-function-name-face)
   '("\\<\\([A-Z_][A-Za-z0-9_$]*\\)\\>" . font-lock-variable-name-face)
   ) "Extol mode highlighting")

(defun extol-indent-line ()
  "Indent current line as Extol code"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond
     ((bobp)
      (indent-line-to 0))
     ((looking-at " *$")
      (indent-line-to (save-excursion
                        (re-search-backward "[^ \t]")
                        (current-indentation))))
     ((looking-at " *\\(pred\\|dcg2?\\|test\\|fun\\) ")
      (indent-line-to 0))
     ((looking-at " *%")
      (indent-line-to (save-excursion (forward-line 1) (current-indentation))))
     (t
      (indent-line-to
       (let ((nesting (car (syntax-ppss))) (unindent (looking-at " *;")))
         (save-excursion
           (re-search-backward "^ *[^ \r\n%]")
           (beginning-of-line)
           (let* ((prev-indent (current-indentation))
                  (prev-nesting (car (syntax-ppss)))
                  (nesting-indent (* 4 (- nesting prev-nesting)))
                  (extra-indent (if (looking-at ".*\\(:\\|-->\\)$\\| *;") (cond ((<= nesting-indent 0) (+ (- nesting-indent) 4)) (t 0)) 0)))
             (+ prev-indent nesting-indent extra-indent (if unindent -4 0)))))))))
  (if (bolp)
          (re-search-forward "^[ \t]*")))

(defvar extol-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?% "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for Extol mode")

(defun extol-mode ()
  "Major mode for Extol files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table extol-mode-syntax-table)
  (use-local-map extol-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(extol-font-lock-keywords))
  (set (make-local-variable 'indent-line-function) 'extol-indent-line)
  (setq major-mode 'extol-mode)
  (setq mode-name "XTL")
  (setq-local comment-start "%")
  (setq-local indent-tabs-mode nil)
  (run-hooks 'wpdl-mode-hook))

(provide 'extol-mode)
