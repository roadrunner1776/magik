;;; .yas-setup.el --- YASnippet helper functions for Magik snippets -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'yasnippet)

(defgroup magik-yasnippet nil
  "Customise Magik YASnippet group."
  :tag   "Magik YASnippet"
  :group 'magik)

(defcustom magik-yasnippet-default-pragma "_pragma(classify_level=, topic={}, usage={})"
  "Default pragma to use if no previous pragma could be found."
  :group 'magik-yasnippet
  :type  'string)

(defconst magik-yasnippet--class-name-regexp "\\(\\(\\sw\\|_\\)+\\)" "The regexp to use for the Magik class name.")

(defun magik-yasnippet-prev-pragma ()
  "Search for the previous pragma in the buffer.
If a previous pragma is found, return it as a string.
If no pragm` is found, return the default pragma
defined by `magik-yasnippet-default-pragma`."
  (save-excursion
    (if (re-search-backward "^_pragma([^)]*)" nil t)
        (match-string-no-properties 0)
      magik-yasnippet-default-pragma)))

(defun magik-yasnippet-prev-class-name ()
  "Search for the previous class name in the buffer.
The function searches backward in the buffer for the most recent
class name in the context of `_method`, `def_slotted_exemplar`, `def_mixin`,
`def_indexed_exemplar`, `.define_shared_constant`, `.define_shared_variable`,
`.define_slot_access`, `.define_pseudo_slot`,
`.define_slot_externally_writable`, `.define_slot_externally_readable`.
Returns the class name as a string, or nil if no class name is found."
  (save-excursion
    (or
     (when (re-search-backward (concat "_method[ \t]+" magik-yasnippet--class-name-regexp "\\.") nil t)
       (match-string-no-properties 1))
     (when (re-search-backward (concat "def_slotted_exemplar\\s-*(\\(\\s-\\|\n\\)*:" magik-yasnippet--class-name-regexp) nil t)
       (match-string-no-properties 2))
     (when (re-search-backward (concat "def_mixin\\s-*(\\(\\s-\\|\n\\)*:" magik-yasnippet--class-name-regexp) nil t)
       (match-string-no-properties 2))
     (when (re-search-backward (concat "def_indexed_exemplar\\s-*(\\(\\s-\\|\n\\)*:" magik-yasnippet--class-name-regexp) nil t)
       (match-string-no-properties 2))
     (when (re-search-backward (concat "^" magik-yasnippet--class-name-regexp "\\.define_shared_constant") nil t)
       (match-string-no-properties 1))
     (when (re-search-backward (concat "^" magik-yasnippet--class-name-regexp "\\.define_shared_variable") nil t)
       (match-string-no-properties 1))
     (when (re-search-backward (concat "^" magik-yasnippet--class-name-regexp "\\.define_slot_access") nil t)
       (match-string-no-properties 1))
     (when (re-search-backward (concat "^" magik-yasnippet--class-name-regexp "\\.define_slot_externally_writable") nil t)
       (match-string-no-properties 1))
     (when (re-search-backward (concat "^" magik-yasnippet--class-name-regexp "\\.define_slot_externally_readable") nil t)
       (match-string-no-properties 1))
     (when (re-search-backward (concat "^" magik-yasnippet--class-name-regexp "\\.define_pseudo_slot") nil t)
       (match-string-no-properties 1)))))

(defun magik-yasnippet-prev-class-name-with-dot ()
  "Return the class name as a string (postfixed with a dot `.`)."
  (let ((class-name (magik-yasnippet-prev-class-name)))
    (when class-name
      (concat class-name "."))))

(defun magik-yasnippet-prev-class-name-as-symbol ()
  "Return the class name as a symbol (prefixed with a colon `:`)."
  (let ((class-name (magik-yasnippet-prev-class-name)))
    (when class-name
      (concat ":" class-name))))

(defun magik-yasnippet-filename ()
  "Return the current buffer's filename without the `.magik` extension.
If the buffer is not visiting a file, return an empty string."
  (let ((name (if (buffer-file-name)
                  (file-name-nondirectory (buffer-file-name))
                "")))
    (if (string-match "\\.magik$" name)
        (setq name (substring name 0 (- (length name) 6))))))

(defun magik-yasnippet-filename-as-symbol ()
  "Return the filename as a symbol (prefixed with a colon `:`)."
  (let ((name (magik-yasnippet-filename)))
    (concat ":" name)))

(defun magik-yasnippet-prev-slotted-exemplar-slots ()
  "Search for the previous `def_slotted_exemplar` and return slot names."
  (let ((slot_count 1)
        (slot_name nil)
        (slotted_loc nil)
        (dollar_loc nil)
        (more_slots nil)
        (result ""))
    (save-excursion
      (when (re-search-backward "\\(def_slotted_exemplar\\)" nil t)
        (setq slotted_loc (match-beginning 0))
        (goto-char slotted_loc)
        (when (re-search-forward "\\(\\$\\)" nil t)
          (setq dollar_loc (match-beginning 0)))
        (setq more_slots t)))

    (while more_slots
      (save-excursion
        (goto-char slotted_loc)
        (if (re-search-forward "{\\s-*:\\s-*\\(\\sw+\\)\\s-*,\\s-*\\(_unset\\)\\s-*}" dollar_loc t slot_count)
            (progn
              (setq
               slot_count (1+ slot_count)
               slot_name (match-string-no-properties 1)
               result (concat result
                              (if (= slot_count 2)
                                  (concat "\t." slot_name " << ")
                                (concat "\n\t." slot_name " << ")))))
          (setq more_slots nil)
          (when (> slot_count 1)
            (setq result (concat result "\n"))))))
    result))

(defun magik-yassnippet-module-name ()
  "Recursively search for the module.def and return the module name."
  (let ((current-dir (file-name-directory (buffer-file-name))))
    (catch 'module-found
      (while current-dir
        (let ((module-file (expand-file-name "module.def" current-dir)))
          (when (file-exists-p module-file)
            (throw 'module-found
                   (with-temp-buffer
                     (insert-file-contents module-file)
                     (goto-char (point-min))
                     (current-word)))))
        ;; Move to the parent directory
        (setq current-dir (if (equal current-dir "/")
                              nil
                            (file-name-directory (directory-file-name current-dir)))))
      nil))) ;; Return nil if no module.def is found

;;; .yas-setup.el ends here
