(require 'subr-x)

(defvar cj-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C/C++ style comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Chars are the same as strings
    (modify-syntax-entry ?' "\"" table)
    ;; Treat <> as punctuation (needed to highlight C++ keywords
    ;; properly in template syntax)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?% "." table)
    table))

(defun cj-types ()
  '("Unit" "Bool"
    "Rune" "Byte"
    ;; Integer types
    "IntNative" "Int8" "Int16" "Int32" "Int64"
    "UIntNative" "UInt8" "UInt16" "UInt32" "UInt64"
    ;; Floating point types
    "Float16" "Float32" "Float64"
    ;; Array Type
    "VArray"
    ;; atom
    "AtomicInt8" "AtomicInt16" "AtomicInt32" "AtomicInt64" "AtomicUInt8" "AtomicUInt16" "AtomicUInt32" "AtomicUInt64"
    "AtomicBool"
    "AtomicReference" "AtomicOptionReference"
    ;; concurrent
    "ConcurrentHashMap"
    "BlockingQueue" "ArrayBlockingQueue" "NonBlockingQueue"
    ))

(defun cj-keywords ()
  '("unsafe"         "where"       "class"
    "break"       "case"        "catch"
    "const"       "continue"    "sealed"
    "new"         "private"     "prop"
    "do"          "else"        "enum"        
    "foreign"      "false"       "init"
    "for"         "var"         "reref"
    "func"        "main"        "if"
    "inline"      "import"      "spawn"
    "in"          "extend"      "mut"
    "return"      "static"      "struct"
    "match"       "true"        "try"
    "macro"       "var"         "while"
    "open"        "override"    "let"
    "interface"   "unless"      "operator"
    "public"      "quote"       "is"
    "as"          "package"     "internal"
    "protected"   "throw"       "finally"
    "from"
))

(defun cj-font-lock-keywords ()
  (list
   `("#.*include \\(\\(<\\|\"\\).*\\(>\\|\"\\)\\)" . (1 font-lock-string-face))
   `(,(regexp-opt (cj-keywords) 'symbols) . font-lock-keyword-face)
   `(,(regexp-opt (cj-types) 'symbols) . font-lock-type-face)))

(defun cj--space-prefix-len (line)
  (- (length line)
     (length (string-trim-left line))))

(defun cj--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun cj--desired-indentation ()
  (let ((cur-line (string-trim-right (thing-at-point 'line t)))
        (prev-line (string-trim-right (cj--previous-non-empty-line)))
        (indent-len 4))
    (cond
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      (cj--space-prefix-len prev-line))
     ((string-suffix-p "{" prev-line)
      (+ (cj--space-prefix-len prev-line) indent-len))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- (cj--space-prefix-len prev-line) indent-len) 0))
     (t (cj--space-prefix-len prev-line)))))

(defun cj-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((current-indentation
            (cj--space-prefix-len (thing-at-point 'line t)))
           (desired-indentation
            (cj--desired-indentation))
           (n (max (- (current-column) current-indentation) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

;;;###autoload
(define-derived-mode cj-mode prog-mode "Simple Cangjie"
  "Simple major mode for Cangjie."
  :syntax-table cj-mode-syntax-table
  (setq-local font-lock-defaults '(cj-font-lock-keywords))
  (setq-local indent-line-function 'cj-indent-line)
  (setq-local comment-start "// "))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cj\\'" . cj-mode))

(provide 'cj-mode)
