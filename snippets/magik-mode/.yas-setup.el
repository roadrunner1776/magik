;;; .yas-setup.el --- YASnippet helper functions for Magik snippets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'compat)
(require 'subr-x)
(require 'warnings)
(require 'yasnippet)

;; Suppress the warnings about modifying the buffer via a snippet
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

(defgroup magik-yasnippet nil
  "Customise Magik YASnippet group."
  :tag   "Magik YASnippet"
  :group 'magik)

(defcustom magik-yasnippet-default-pragma "_pragma(classify_level=, topic={}, usage={})"
  "Default pragma to use if no previous pragma could be found."
  :group 'magik-yasnippet
  :type  'string)

(defcustom magik-yasnippet-default-package-name "user"
  "Default package name."
  :group 'magik-yasnippet
  :type  'string)

(defcustom magik-yasnippet-documentation-style 'sw-method-doc
  "Choose between \\'sw-method-doc\\', \\'type-doc\\', or nil.
\\'sw-method-doc\\' for Smallworld method documentation style.
\\'type-doc\\' for type-based documentation.
nil to disable documentation."
  :group 'magik-yasnippet
  :type  '(choice (const :tag "Smallworld method documentation style" sw-method-doc)
                  (const :tag "Type-based documentation" type-doc)
                  (const :tag "No documentation" nil)))

(defcustom magik-yasnippet-default-documentation "\t## \n\t## \n\t## "
  "Default documentation string to insert."
  :group 'magik-yasnippet
  :type  'string)

(defcustom magik-yasnippet-sw-method-doc-documentation 'default
  "Default sw-method-doc documentation string to insert."
  :group 'magik-yasnippet
  :type  '(choice (const :tag "Use default documentation" default)
                  (string :tag "Custom sw-method-doc")))

(defcustom magik-yasnippet-type-doc-documentation 'default
  "Default type-doc documentation string to insert."
  :group 'magik-yasnippet
  :type  '(choice (const :tag "Use default documentation" default)
                  (string :tag "Custom type-doc")))

(defconst magik-yasnippet--class-name-regexp "\\(\\(\\sw\\|_\\)+\\)"
  "The regexp to use for the Magik class name.")

(defun magik-yasnippet--documentation-string (value)
  "Return the documentation string based on VALUE.
If VALUE is \\'default, return `magik-yasnippet-default-documentation'.
Otherwise, return VALUE."
  (if (eq value 'default)
      magik-yasnippet-default-documentation
    value))

(defun magik-yasnippet-documentation (&optional untabbed)
  "Insert the documentation string based on `magik-yasnippet-documentation-style'.
If UNTABBED is non-nil remove the tabs from the documentation string.
When documentation style is nil (disabled), it kills the current line."
  (if magik-yasnippet-documentation-style
      (let ((documentation-string (pcase magik-yasnippet-documentation-style
                                    (`sw-method-doc (magik-yasnippet--documentation-string magik-yasnippet-sw-method-doc-documentation))
                                    (`type-doc (magik-yasnippet--documentation-string magik-yasnippet-type-doc-documentation)))))
        (if untabbed
            (replace-regexp-in-string "\t" "" documentation-string)
          documentation-string))
    (kill-line)))

(defun magik-yasnippet-pragma ()
  "Search for the previous pragma in the buffer.
If a previous pragma is found, return it as a string.
If no pragma is found, return the default pragma
defined by `magik-yasnippet-default-pragma`."
  (save-excursion
    (if (re-search-backward "^_pragma([^)]*)" nil t)
        (match-string-no-properties 0)
      (if (re-search-forward "^_pragma([^)]*)" nil t)
          (match-string-no-properties 0)
        magik-yasnippet-default-pragma))))

(defun magik-yasnippet-pragma-snippet (end-of-cursor?)
  "Create the pragma snippet in a string.
END-OF-CURSOR? inserts a `$0' at the end of the snippet.
Returns a list with a snippet string and the amount of $ inserted internally/"
  (let ((snippet-content (if (magik-yasnippet--pragma-p)
                             (list (magik-yasnippet-pragma) 0)
                           (if (get 'magik-yasnippet-default-pragma 'custom-set)
                               (list magik-yasnippet-default-pragma 0)
                             (list "_pragma(classify_level=$1, topic={$2}, usage={$3})" 3)))))
    (when end-of-cursor?
      (setcar snippet-content (concat (car snippet-content) "$0")))
    snippet-content))

(defun magik-yasnippet--pragma-p ()
  "Search for a in the buffer if found return t."
  (save-excursion
    (if (re-search-backward "^_pragma([^)]*)" nil t)
        t
      (if (re-search-forward "^_pragma([^)]*)" nil t)
          t
        nil))))

(defun magik-yasnippet-prev-class-name ()
  "Search for the previous class name in the buffer.
The function searches backward in the buffer for the most recent
class name in the context of `_method`, `def_slotted_exemplar`, `def_mixin`,
`def_indexed_exemplar`, `.define_shared_constant`, `.define_shared_variable`,
`.define_slot_access`, `.define_pseudo_slot`,
`.define_slot_externally_writable`, `.define_slot_externally_readable`.
Returns the class name as a string, or nil if no class name is found."
  (save-excursion
    (let ((start-point (point))
          (patterns
           `((,(concat "_method[ \t]+" magik-yasnippet--class-name-regexp "\\.") . 1)
             (,(concat "def_slotted_exemplar\\s-*(\\(\\s-\\|\n\\)*:" magik-yasnippet--class-name-regexp) . 2)
             (,(concat "def_mixin\\s-*(\\(\\s-\\|\n\\)*:" magik-yasnippet--class-name-regexp) . 2)
             (,(concat "def_indexed_exemplar\\s-*(\\(\\s-\\|\n\\)*:" magik-yasnippet--class-name-regexp) . 2)
             (,(concat "^" magik-yasnippet--class-name-regexp "\\.define_shared_constant") . 1)
             (,(concat "^" magik-yasnippet--class-name-regexp "\\.define_shared_variable") . 1)
             (,(concat "^" magik-yasnippet--class-name-regexp "\\.define_slot_access") . 1)
             (,(concat "^" magik-yasnippet--class-name-regexp "\\.define_slot_externally_writable") . 1)
             (,(concat "^" magik-yasnippet--class-name-regexp "\\.define_slot_externally_readable") . 1)
             (,(concat "^" magik-yasnippet--class-name-regexp "\\.define_pseudo_slot") . 1)))
          (closest-pos nil)
          (closest-name nil))
      (dolist (pattern patterns)
        (goto-char start-point)
        (when (re-search-backward (car pattern) nil t)
          (let ((pos (point))
                (class-name (match-string-no-properties (cdr pattern))))
            (when (or (null closest-pos)
                      (> pos closest-pos))
              (setq closest-pos pos
                    closest-name class-name)))))
      closest-name)))

(defun magik-yasnippet-prev-class-name-with-dot ()
  "Return the class name as a string (postfixed with a dot `.`)."
  (when-let* ((class-name (magik-yasnippet-prev-class-name)))
    (concat class-name ".")))

(defun magik-yasnippet-prev-class-name-as-symbol ()
  "Return the class name as a symbol (prefixed with a colon `:`)."
  (when-let* ((class-name (magik-yasnippet-prev-class-name)))
    (concat ":" class-name)))

(defun magik-yasnippet-filename ()
  "Return the current buffer's filename without the `.magik` extension.
If the buffer is not visiting a file, return an empty string."
  (if-let* ((buffer-file (buffer-file-name))
            (name (and (string-suffix-p ".magik" buffer-file)
                       buffer-file)))
      (file-name-sans-extension (file-name-nondirectory name))
    ""))

(defun magik-yasnippet-filename-as-symbol ()
  "Return the filename as a symbol (prefixed with a colon `:`)."
  (concat ":" (magik-yasnippet-filename)))

(defun magik-yasnippet-prev-slotted-exemplar-slots (dollar-start-count)
  "Search for the previous `def_slotted_exemplar` and return slot names."
  (save-excursion
    (when-let* ((slotted-loc (and (re-search-backward "def_slotted_exemplar" nil t)
                                  (match-beginning 0))))
      (goto-char slotted-loc)
      (when-let* ((dollar-loc (and (re-search-forward "\\$" nil t)
                                   (match-beginning 0))))
        (let (slots)
          (goto-char slotted-loc)
          (while (re-search-forward "{\\s-*:\\s-*\\(\\sw+\\)\\s-*,\\s-*\\(_unset\\)\\s-*}" dollar-loc t)
            (push (match-string-no-properties 1) slots))
          (when slots
            (setq slots (nreverse slots))
            (concat
             (string-join
              (cl-mapcar
               (lambda (slot i)
                 (format "%s.%s << ${%d:value}"
                         (if (= i 0) "\t" "\n\t")
                         slot
                         (cl-incf dollar-start-count)))
               slots
               (number-sequence 0 (1- (length slots))))))))))))

(defun magik-yasnippet--after-point-empty-p ()
  "Return t if the line after point is empty or contain only whitespace."
  (save-excursion
    (re-search-forward "^\\s-*$" (line-end-position) t)))

(defun magik-yasnippet--line-after-point-contains-method-p ()
  "Return t if the text after point in the current line has _method."
  (save-excursion
     (re-search-forward "_method" (line-end-position) t)))
;;; .yas-setup.el ends here
