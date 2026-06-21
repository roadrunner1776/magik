;;; magik-completion.el --- Completion-at-point for Magik  -*- lexical-binding: t; -*-

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides `completion-at-point-functions' (CAPF) for Magik source buffers
;; and Magik session buffers.  This integrates with modern completion
;; frameworks (corfu, vertico, company-mode via cape, etc.) without
;; requiring any specific UI package.
;;
;; Completion sources:
;;  - Magik keywords and built-in constants
;;  - Local variables and parameters (buffer-local scan)
;;  - Symbols from a running Magik session (via method_finder / CB process)

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'magik-utils)

;; Tree-sitter functions (Emacs 29+, only used when available)
(declare-function treesit-parser-list "treesit")
(declare-function treesit-node-at "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-children "treesit")
(declare-function treesit-node-child-by-field-name "treesit")
(declare-function treesit-node-text "treesit")
(declare-function treesit-node-start "treesit")

(defgroup magik-completion nil
  "Completion-at-point support for Magik."
  :group 'magik)

(defcustom magik-completion-enabled t
  "When non-nil, enable Magik `completion-at-point'.
Set to nil before mode activation to disable, or use
`magik-completion-mode' to toggle interactively."
  :type 'boolean
  :group 'magik-completion)

(defcustom magik-completion-enable-keywords t
  "When non-nil, include Magik keywords in completion candidates."
  :type 'boolean
  :group 'magik-completion)

(defcustom magik-completion-enable-variables t
  "When non-nil, include local variables from the current buffer."
  :type 'boolean
  :group 'magik-completion)

(defcustom magik-completion-insert-params t
  "When non-nil, insert method parameters as a yasnippet after completion."
  :type 'boolean
  :group 'magik-completion)

(defcustom magik-completion-insert-optional-params nil
  "When non-nil, include optional parameters in the yasnippet."
  :type 'boolean
  :group 'magik-completion)

(defcustom magik-completion-insert-gather-param t
  "When non-nil, include the gather parameter in the yasnippet."
  :type 'boolean
  :group 'magik-completion)

;;; --- Keyword table ---

(defconst magik-completion--keywords
  '("_abstract" "_allresults" "_and" "_andif" "_block" "_catch"
    "_class" "_clone" "_constant" "_continue" "_default" "_div"
    "_dynamic" "_elif" "_else" "_endblock" "_endcatch" "_endif"
    "_endlock" "_endloop" "_endmethod" "_endproc" "_endprotect"
    "_endtry" "_false" "_finally" "_for" "_gather" "_global"
    "_handling" "_if" "_import" "_is" "_isnt" "_iter" "_leave"
    "_local" "_lock" "_loop" "_loopbody" "_maybe" "_method" "_mod"
    "_not" "_optional" "_or" "_orif" "_over" "_package" "_pragma"
    "_private" "_proc" "_protect" "_protection" "_recursive"
    "_return" "_scatter" "_self" "_super" "_then" "_thisthread"
    "_throw" "_true" "_try" "_unset" "_when" "_while" "_with"
    "_xor" "_xorif")
  "List of Magik language keywords for completion.")

(defconst magik-completion--builtins
  '("bag" "char16_vector" "concurrent_hash_map" "condition" "date"
    "equality_hash_table" "equality_set" "float" "gis_program_manager"
    "hash_table" "integer" "property_list" "queue" "rope" "set"
    "simple_vector" "smallworld_product" "sorted_collection" "stack"
    "sw_module_manager" "symbol" "system")
  "List of commonly used Magik built-in names for completion.")

;;; --- Variable scanning ---

(defun magik-completion--scan-local-variables ()
  "Scan the current method/proc body for local variable assignments.
Uses tree-sitter for scope-aware scanning when available,
falls back to regex-based scanning otherwise.
Returns a list of variable name strings."
  (if (and (fboundp 'treesit-parser-list)
           (treesit-parser-list))
      (magik-completion--ts-scan-variables)
    (magik-completion--regex-scan-variables)))

(defun magik-completion--ts-scan-variables ()
  "Scan variables using tree-sitter for accurate scope detection.
Returns a list of variable name strings visible at point."
  (let ((variables '())
        (node (treesit-node-at (point))))
    ;; Walk up to find the enclosing method/proc/block
    (when-let* ((scope-node (magik-completion--ts-enclosing-scope node)))
      ;; Collect parameters from the method/proc signature
      (setq variables (magik-completion--ts-collect-params scope-node variables))
      ;; Collect local variables and assignments within scope, before point
      (setq variables (magik-completion--ts-collect-locals scope-node variables)))
    (delete-dups variables)))

(defun magik-completion--ts-enclosing-scope (node)
  "Find the enclosing method, procedure, or block NODE."
  (let ((current node))
    (while (and current
                (not (member (treesit-node-type current)
                             '("method" "procedure" "block" "loop"))))
      (setq current (treesit-node-parent current)))
    current))

(defun magik-completion--ts-collect-params (scope-node variables)
  "Collect parameter names from SCOPE-NODE into VARIABLES list.
Returns the updated VARIABLES list."
  (let ((params (treesit-node-children scope-node)))
    (dolist (child params)
      (when (equal (treesit-node-type child) "parameters")
        (dolist (param (treesit-node-children child))
          (when (member (treesit-node-type param) '("identifier" "parameter"))
            (let ((name (treesit-node-text param t)))
              (unless (or (string-prefix-p "_" name)
                          (member name variables))
                (push name variables))))))))
  variables)

(defun magik-completion--ts-collect-locals (scope-node variables)
  "Collect local variables from SCOPE-NODE that appear before point.
Returns the updated VARIABLES list."
  (let ((cursor-pos (point)))
    (magik-completion--ts-walk-for-assignments scope-node cursor-pos variables)))

(defun magik-completion--ts-walk-for-assignments (node limit variables)
  "Walk NODE tree collecting variable names assigned before LIMIT position.
Returns the updated VARIABLES list."
  (when (and node (< (treesit-node-start node) limit))
    (let ((type (treesit-node-type node)))
      (cond
       ;; Assignment: var << expr
       ((equal type "assignment")
        (when-let* ((target (treesit-node-child-by-field-name node "variable"))
                    (_ (< (treesit-node-start target) limit)))
          (let ((name (treesit-node-text target t)))
            (unless (or (string-prefix-p "_" name)
                        (member name variables))
              (push name variables)))))
       ;; Local variable declaration
       ((equal type "variable_declaration")
        (dolist (child (treesit-node-children node))
          (when (and (equal (treesit-node-type child) "identifier")
                     (< (treesit-node-start child) limit))
            (let ((name (treesit-node-text child t)))
              (unless (or (string-prefix-p "_" name)
                          (member name variables))
                (push name variables))))))
       ;; For loop: _for vars _over ...
       ((equal type "iterator")
        (dolist (child (treesit-node-children node))
          (when (and (equal (treesit-node-type child) "identifier")
                     (< (treesit-node-start child) limit))
            (let ((name (treesit-node-text child t)))
              (unless (or (string-prefix-p "_" name)
                          (member name variables))
                (push name variables))))))))
    ;; Recurse into children
    (dolist (child (treesit-node-children node))
      (when (< (treesit-node-start child) limit)
        (setq variables (magik-completion--ts-walk-for-assignments child limit variables)))))
  variables)

(defun magik-completion--regex-scan-variables ()
  "Scan variables using regex (fallback when tree-sitter unavailable).
Returns a list of variable name strings."
  (let ((variables '())
        (limit (point))
        (method-start (save-excursion
                        (or (re-search-backward
                             "^\\s-*\\(_method\\|_proc\\|_block\\)" nil t)
                            (point-min))))
        (case-fold-search nil))
    (save-excursion
      ;; Find _local declarations
      (goto-char method-start)
      (while (re-search-forward
              "\\_<_local\\s-+\\([a-z_][a-z0-9_!?]*\\)" limit t)
        (let ((var (match-string-no-properties 1)))
          (unless (member var variables)
            (push var variables))))
      ;; Find << assignments (often introduces variables)
      (goto-char method-start)
      (while (re-search-forward
              "\\b\\([a-z_][a-z0-9_!?]*\\)\\s-*<<" limit t)
        (let ((var (match-string-no-properties 1)))
          (unless (or (member var variables)
                      (string-prefix-p "_" var))
            (push var variables))))
      ;; Find _for loop variables
      (goto-char method-start)
      (while (re-search-forward
              "\\_<_for\\s-+\\([a-z_][a-z0-9_!?, ]*\\)\\s-+_over" limit t)
        (let ((vars-str (match-string-no-properties 1)))
          (dolist (v (split-string vars-str "[, \t]+" t))
            (unless (member v variables)
              (push v variables))))))
    ;; Find method parameters
    (save-excursion
      (goto-char method-start)
      (when (re-search-forward
             "^\\s-*\\(_private\\s-+\\)?\\(_iter\\s-+\\)?_method\\s-+\\S-+\\.\\S-+\\s-*(\\([^)]*\\))"
             (min (+ method-start 500) (point-max)) t)
        (let ((params-str (match-string-no-properties 3)))
          (dolist (p (split-string params-str "[, \t]+" t))
            (let ((clean (replace-regexp-in-string "\\`_optional\\s-+" "" p)))
              (setq clean (replace-regexp-in-string "\\`_gather\\s-+" "" clean))
              (unless (or (string-prefix-p "_" clean)
                          (string-empty-p clean)
                          (member clean variables))
                (push clean variables)))))))
    (nreverse variables)))

(defun magik-completion--scan-slots ()
  "Scan for slot names in the current file's exemplar definition.
Returns a list of slot name strings."
  (let ((slots '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "{\\s-*:\\([a-z_][a-z0-9_!?]*\\)\\s-*," nil t)
        (let ((slot (match-string-no-properties 1)))
          (unless (member slot slots)
            (push slot slots)))))
    (nreverse slots)))

;;; --- Prefix detection ---

(defun magik-completion--bounds ()
  "Return the bounds (BEG . END) of the Magik symbol at point.
Returns nil if point is inside a comment or string."
  (let ((syntax (syntax-ppss)))
    (when (and (not (nth 3 syntax))   ; not in string
               (not (nth 4 syntax)))  ; not in comment
      (let ((end (point))
            (beg (save-excursion
                   (skip-chars-backward "a-zA-Z0-9_!?:")
                   ;; include leading underscore for keywords
                   (when (eq (char-before) ?_)
                     (backward-char))
                   (point))))
        (when (< beg end)
          (cons beg end))))))

(defun magik-completion--slot-bounds ()
  "Return bounds if point is completing a slot reference (after `.')."
  (let ((syntax (syntax-ppss)))
    (when (and (not (nth 3 syntax))
               (not (nth 4 syntax)))
      (let ((end (point))
            (beg (save-excursion
                   (skip-chars-backward "a-zA-Z0-9_!?")
                   (point))))
        (when (and (< beg end)
                   (> beg (point-min))
                   (eq (char-before beg) ?.)
                   ;; slot access: preceding char before . is not a word char
                   ;; (i.e. it's `.slot` not `obj.method`)
                   (let ((pre-dot (char-before (1- beg))))
                     (or (null pre-dot)
                         (memq pre-dot '(?\s ?\t ?\n ?\( ?, ?\;)))))
          (cons beg end))))))

;;; --- CAPF functions ---

(defun magik-completion-at-point-keywords ()
  "Completion-at-point function for Magik keywords."
  (when magik-completion-enable-keywords
    (when-let* ((bounds (magik-completion--bounds)))
      (let ((beg (car bounds))
            (end (cdr bounds))
            (prefix (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (when (string-prefix-p "_" prefix)
          (list beg end magik-completion--keywords
                :exclusive 'no
                :company-kind (lambda (_) 'keyword)))))))

(defun magik-completion-at-point-builtins ()
  "Completion-at-point function for Magik built-in names."
  (when magik-completion-enable-keywords
    (when-let* ((bounds (magik-completion--bounds)))
      (let ((beg (car bounds))
            (prefix (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (unless (or (string-prefix-p "_" prefix)
                    (and (> beg (point-min))
                         (eq (char-before beg) ?.)))
          (list beg (cdr bounds) magik-completion--builtins
                :exclusive 'no
                :company-kind (lambda (_) 'constant)))))))

(defun magik-completion-at-point-variables ()
  "Completion-at-point function for local variables and parameters."
  (when magik-completion-enable-variables
    (when-let* ((bounds (magik-completion--bounds)))
      (let ((beg (car bounds))
            (prefix (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (unless (or (string-prefix-p "_" prefix)
                    (and (> beg (point-min))
                         (eq (char-before beg) ?.)))
          (when-let* ((vars (magik-completion--scan-local-variables)))
            (list beg (cdr bounds) vars
                  :exclusive 'no
                  :company-kind (lambda (_) 'variable))))))))

(defun magik-completion-at-point-slots ()
  "Completion-at-point function for exemplar slot references."
  (when-let* ((bounds (magik-completion--slot-bounds))
              (slots (magik-completion--scan-slots)))
    (list (car bounds) (cdr bounds) slots
          :exclusive 'no
          :company-kind (lambda (_) 'field))))

;;; --- Class Browser integration ---

(defvar-local magik-completion--cb-process nil
  "Dedicated CB process for completion in this buffer.")

(defvar-local magik-completion--cb-buffer-name nil
  "Buffer name for the dedicated completion CB process.")

(defvar-local magik-completion--cb-candidates nil
  "Result slot: set by the filter when CB responds.")

(defvar-local magik-completion--cb-filter-str ""
  "Accumulator for CB process filter output.")

(defvar magik-completion--class-cache nil
  "Cache: list of all class/exemplar names from CB.")

(defvar magik-completion--class-cache-loaded nil
  "Non-nil when the class cache has been populated.")

(defvar magik-completion--global-cache nil
  "Cache: list of all global/dynamic names from CB.")

(defvar magik-completion--global-cache-loaded nil
  "Non-nil when the global cache has been populated.")

(defvar magik-completion--method-cache nil
  "Cache: cons (KEY . CANDIDATES) for method completion.
KEY is \"class.first-char\" to detect when to re-query.")

(defcustom magik-completion-enable-cb t
  "When non-nil, use the Class Browser for method/class/global completion."
  :type 'boolean
  :group 'magik-completion)

(defcustom magik-completion-cb-timeout 2.0
  "Timeout in seconds to wait for CB process responses."
  :type 'number
  :group 'magik-completion)

(defcustom magik-completion-cb-max-methods 1000
  "Maximum number of methods to retrieve from the CB per query."
  :type 'integer
  :group 'magik-completion)

(declare-function magik-cb-get-process-create "magik-cb")
(declare-function magik-cb-is-running "magik-cb")
(declare-function magik-cb-temp-file-name "magik-cb")
(declare-function magik-current-method-name "magik-mode")
(declare-function magik-transmit-region "magik-mode")

(defvar magik-cb-coding-system)
(defvar magik-session-buffer)
(defvar magik-session-prompt)
(defvar magik-cb-in-keyword)

;;; --- CB process management ---

(defun magik-completion--cb-buffer ()
  "Return the buffer name for the completion CB process."
  (or magik-completion--cb-buffer-name
      (setq magik-completion--cb-buffer-name
            (concat " *cb*" (buffer-name) "*completion*"))))

(defun magik-completion--gis-buffer ()
  "Return the active Magik session buffer name, or nil."
  (when (boundp 'magik-session-buffer)
    (when-let* ((buf magik-session-buffer)
                (_ (get-buffer buf))
                (_ (get-buffer-process buf)))
      buf)))

(defun magik-completion--ensure-cb-process ()
  "Ensure a dedicated CB process is running for completion.
Returns the process object or nil if it cannot be started."
  (when (and magik-completion-enable-cb
             (require 'magik-cb nil t))
    (if (and magik-completion--cb-process
             (process-live-p magik-completion--cb-process))
        magik-completion--cb-process
      ;; Try to start one
      (when-let* ((gis-buf (magik-completion--gis-buffer)))
        (let* ((smallworld-gis (buffer-local-value
                                'magik-smallworld-gis (get-buffer gis-buf)))
               (cb-buf (magik-completion--cb-buffer)))
          (condition-case nil
              (let ((proc (cl-letf (((symbol-function 'magik-cb-mode)
                                     #'fundamental-mode))
                            (magik-cb-get-process-create
                             cb-buf
                             #'magik-completion--cb-filter
                             smallworld-gis gis-buf nil))))
                (when (and proc (process-live-p proc))
                  (setq magik-completion--cb-process proc
                        magik-completion--cb-buffer-name
                        (buffer-name (process-buffer proc)))
                  proc))
            (error nil)))))))

;;; --- CB filter ---

(defun magik-completion--cb-filter (proc str)
  "Process filter for the completion CB process PROC.
Accumulates STR until a control char signals end of output,
then parses the temp file."
  (when-let* ((buf (process-buffer proc))
              (_ (buffer-live-p buf)))
    (with-current-buffer buf
        (setq magik-completion--cb-filter-str
              (concat magik-completion--cb-filter-str str))
        (let ((coding-system-for-read (if (boundp 'magik-cb-coding-system)
                                          magik-cb-coding-system
                                        'utf-8)))
          (cond
           ;; \C-e signals method list output ready
           ((string-match "\C-e" magik-completion--cb-filter-str)
            (setq magik-completion--cb-filter-str "")
            (let ((buffer-read-only nil))
              (erase-buffer)
              (insert-file-contents (magik-cb-temp-file-name proc) nil nil nil t))
            (setq magik-completion--cb-candidates
                  (magik-completion--parse-methods)))
           ;; \C-c signals class list output ready
           ((string-match "\C-c" magik-completion--cb-filter-str)
            (setq magik-completion--cb-filter-str "")
            (let ((buffer-read-only nil))
              (erase-buffer)
              (insert-file-contents (magik-cb-temp-file-name proc) nil nil nil t))
            (setq magik-completion--cb-candidates
                  (magik-completion--parse-classes))))))))

;;; --- CB output parsing ---

(defun magik-completion--parse-methods ()
  "Parse method finder output in current buffer.
Returns a list of propertized candidate strings."
  (let ((candidates '())
        (i 0)
        (limit magik-completion-cb-max-methods)
        (in-kw (if (boundp 'magik-cb-in-keyword) magik-cb-in-keyword "  IN  "))
        (regexp nil))
    (setq regexp (concat "^\\([^ \t\n]+\\)" in-kw "\\([^ \t\n]+\\)[  \t]+\\(.*\\)\n\\(.*\n\\)\n"
                         "\\(\\(?:[ \t]+##.*\n\\)*\\)"))
    (goto-char (point-min))
    (save-match-data
      (while (and (< i limit)
                  (re-search-forward regexp nil t))
        (let* ((method-raw (match-string-no-properties 1))
               (class (match-string-no-properties 2))
               (classify (match-string-no-properties 3))
               (args-str (match-string-no-properties 4))
               (doc (when-let* ((raw (match-string-no-properties 5))
                                ((not (string-empty-p raw))))
                      (replace-regexp-in-string "^[ \t]+## ?" "" raw)))
               (parsed-args (magik-completion--parse-args-line
                             (match-beginning 4)))
               (annotation (magik-completion--format-annotation
                            class classify args-str))
               ;; Strip trailing () or << from method name for insertion
               (start-sig (cond
                           ((string-suffix-p "()" method-raw) "(")
                           ((string-suffix-p "<<" method-raw) nil)
                           ((or (car parsed-args) (cadr parsed-args) (caddr parsed-args)) "(")
                           (t nil)))
               (method (cond
                        ((string-suffix-p "()" method-raw)
                         (substring method-raw 0 -2))
                        (t method-raw))))
          (unless (or (string-empty-p method)
                      (string-match-p "\\`\\s-" method)
                      (member method candidates))
            (push (propertize method
                              'magik-class class
                              'magik-annotation annotation
                              'magik-documentation doc
                              'magik-args (car parsed-args)
                              'magik-optional (cadr parsed-args)
                              'magik-gather (caddr parsed-args)
                              'magik-start-signature start-sig)
                  candidates)
            (cl-incf i)))))
    (nreverse candidates)))

(defun magik-completion--parse-args-line (pt)
  "Parse the arguments line from the CB output starting at PT.
Returns a list (REQUIRED OPTIONAL GATHER)."
  (save-excursion
    (goto-char pt)
    (let ((case-fold-search nil)
          args optional gather opt)
      (if (looking-at "\\$\\| *$")
          (list nil nil nil)
        (forward-char 1) ;; skip leading space
        (while (not (or (looking-at "$") (eolp)))
          (cond
           ((looking-at "\\(OPT \\)?GATH \\([^ \t\n]+\\)")
            (setq gather (list (match-string-no-properties 2)))
            (goto-char (match-end 0)))
           ((looking-at "OPT ")
            (setq opt t)
            (goto-char (match-end 0)))
           ((> (skip-syntax-forward "w_") 0)
            (let ((name (buffer-substring-no-properties
                         (save-excursion (skip-syntax-backward "w_") (point))
                         (point))))
              (if opt
                  (push name optional)
                (push name args)))
            (when (eq (following-char) ?\s)
              (forward-char 1)))
           (t (goto-char (line-end-position)))))
        (list (nreverse args) (nreverse optional) gather)))))

(defun magik-completion--format-annotation (class classify args-str)
  "Format annotation from CLASS, CLASSIFY, and ARGS-STR."
  (let ((parts (list class)))
    (when (and classify (string-match "iter" classify))
      (push "(iter)" parts))
    (when (and args-str (not (string-match "\\`\\s-*\\$" args-str)))
      (let ((clean (string-trim args-str)))
        (unless (string-empty-p clean)
          (push (concat "(" clean ")") parts))))
    (string-join (nreverse parts) " ")))

(defun magik-completion--parse-classes ()
  "Parse class/family output in current buffer.
Returns a list of class name strings."
  (let ((candidates '())
        (regexp "\\(\\S-+:\\)\\(\\S-+\\)"))
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward regexp nil t)
        (let ((name (match-string-no-properties 2)))
          (unless (member name candidates)
            (push name candidates)))))
    (nreverse candidates)))

;;; --- CB synchronous queries ---

(defun magik-completion--cb-query (command)
  "Send COMMAND string to the CB process and wait for a response.
Returns the candidates list or nil on timeout."
  (when-let* ((proc (magik-completion--ensure-cb-process))
              (buf (process-buffer proc))
              (_ (buffer-live-p buf)))
    (with-current-buffer buf
      (setq magik-completion--cb-candidates 'pending
            magik-completion--cb-filter-str ""))
    (process-send-string proc command)
    ;; Synchronous wait with timeout
    (let ((deadline (+ (float-time) magik-completion-cb-timeout)))
      (while (and (eq (buffer-local-value 'magik-completion--cb-candidates buf)
                      'pending)
                  (< (float-time) deadline)
                  (process-live-p proc))
        (accept-process-output proc 0.05)))
    (let ((result (buffer-local-value 'magik-completion--cb-candidates buf)))
      (if (eq result 'pending) nil result))))

(defun magik-completion--query-methods (class prefix)
  "Query methods on CLASS starting with PREFIX from CB.
Returns list of method name strings."
  (let* ((char (if (string-empty-p prefix) "" (substring prefix 0 1)))
         (cache-key (concat class "." char)))
    ;; Use cached result if same class+char
    (if (and magik-completion--method-cache
             (equal cache-key (car magik-completion--method-cache)))
        (cdr magik-completion--method-cache)
      (let* ((cmd (concat "method_name ^" char "\n"
                          "unadd class \nadd class " class "$\n"
                          "method_cut_off " (number-to-string magik-completion-cb-max-methods) "\n"
                          "override_flags\nshow_classes\nshow_args\nshow_comments\n"
                          "print_curr_methods\nshow_topics\n"))
             (result (magik-completion--cb-query cmd)))
        (when result
          (setq magik-completion--method-cache (cons cache-key result)))
        result))))

(defun magik-completion--query-classes ()
  "Query all classes from the CB.  Caches the result."
  (if magik-completion--class-cache-loaded
      magik-completion--class-cache
    (let* ((cmd "dont_override_flags\npr_family sw:object\n")
           (result (magik-completion--cb-query cmd)))
      (when result
        (setq magik-completion--class-cache result
              magik-completion--class-cache-loaded t))
      result)))

(defun magik-completion--query-globals ()
  "Query all globals from the CB.  Caches the result."
  (if magik-completion--global-cache-loaded
      magik-completion--global-cache
    (let* ((cmd (concat "method_name ^\n"
                        "unadd class \nadd class <global>\n"
                        "method_cut_off " (number-to-string magik-completion-cb-max-methods) "\n"
                        "override_flags\nshow_classes\nshow_args\n"
                        "print_curr_methods\nshow_topics\n"))
           (result (magik-completion--cb-query cmd)))
      (when result
        (setq magik-completion--global-cache result
              magik-completion--global-cache-loaded t))
      result)))

;;; --- Exemplar type inference ---

(defun magik-completion--infer-exemplar ()
  "Infer the exemplar type of the object before the dot at point.
Returns exemplar name string or nil."
  (save-excursion
    ;; Point is after the partial method name; skip back to the dot.
    (skip-chars-backward "a-zA-Z0-9_!?")
    (when (eq (char-before) ?.)
      (backward-char)
      (let* ((end (point))
             (beg (progn (skip-chars-backward "a-zA-Z0-9_!?") (point)))
             (variable (buffer-substring-no-properties beg end)))
        (cond
         ((string-empty-p variable) nil)
         ;; _self, _clone, _super -> current exemplar
         ((member variable '("_self" "_clone" "_super"))
          (when (fboundp 'magik-current-method-name)
            (cadr (magik-current-method-name))))
         ;; Check typed assignment patterns
         ((magik-completion--infer-from-assignment variable))
         ;; Check if it's a known class name
         ((and magik-completion--class-cache
               (member variable magik-completion--class-cache))
          variable)
         ;; Fallback: pass variable name directly to CB.
         ;; The method_finder handles unknown classes gracefully
         ;; and this covers globals like gis_program_manager.
         (t variable))))))

(defun magik-completion--infer-from-assignment (variable)
  "Try to infer exemplar type of VARIABLE from assignments in the buffer."
  (or (magik-completion--infer-from-param-doc variable)
      (save-excursion
        (let ((case-fold-search nil))
          (cond
           ;; var << Type.new(...)
           ((re-search-backward
             (concat (regexp-quote variable) "\\s-*<<[ \t\n]*\\(\\S-+\\)\\.new")
             nil t)
            (match-string-no-properties 1))
           ;; Integer literal
           ((re-search-backward
             (concat (regexp-quote variable) "\\s-*<<[ \t\n]*[-+]?[0-9]+\\b")
             nil t)
            "integer")
           ;; Float literal
           ((re-search-backward
             (concat (regexp-quote variable) "\\s-*<<[ \t\n]*[-+]?[0-9]*\\.[0-9]+")
             nil t)
            "float")
           ;; String literal
           ((re-search-backward
             (concat (regexp-quote variable) "\\s-*<<[ \t\n]*\"")
             nil t)
            "char16_vector")
           ;; Simple vector literal
           ((re-search-backward
             (concat (regexp-quote variable) "\\s-*<<[ \t\n]*{")
             nil t)
            "simple_vector")
           (t nil))))))

(defun magik-completion--infer-from-param-doc (variable)
  "Infer type of VARIABLE from `## @param {type} name' doc comments.
Searches backward for the enclosing _method and scans its doc block."
  (save-excursion
    (let ((case-fold-search nil)
          (start (point)))
      (when-let* ((method-pos (re-search-backward "\\_<_method\\_>" nil t)))
        (goto-char start)
        (when (re-search-backward
               (concat "##\\s-*@param\\s-*{\\([^}]+\\)}\\s-+"
                       (regexp-quote variable))
               method-pos t)
          (match-string-no-properties 1))))))

;;; --- Method bounds detection ---

(defun magik-completion--method-bounds ()
  "Return (BEG . END) for method name after a dot, or nil.
Detects `object.meth' patterns and returns bounds of `meth'."
  (let ((syntax (syntax-ppss)))
    (when (and (not (nth 3 syntax))
               (not (nth 4 syntax)))
      (let ((end (point))
            (beg (save-excursion
                   (skip-chars-backward "a-zA-Z0-9_!?")
                   (point))))
        (when (and (< beg end)
                   (> beg (point-min))
                   (eq (char-before beg) ?.)
                   ;; Ensure there's a word/symbol before the dot
                   (let ((pre-dot (char-before (1- beg))))
                     (and pre-dot
                          (memq (char-syntax pre-dot) '(?w ?_)))))
          (cons beg end))))))

;;; --- Yasnippet post-completion ---

(declare-function yas-expand-snippet "yasnippet")

(defun magik-completion--build-param-snippet (candidate)
  "Build a yasnippet template string from CANDIDATE's argument properties.
Returns a snippet string like \"(${1:arg1}, ${2:arg2})\" or nil."
  (when magik-completion-insert-params
    (let* ((args (get-text-property 0 'magik-args candidate))
           (optional-raw (get-text-property 0 'magik-optional candidate))
           (gather-raw (get-text-property 0 'magik-gather candidate))
           (optional (and magik-completion-insert-optional-params optional-raw))
           ;; Only include gather when there are no skipped optional params
           ;; before it — you can't pass gather args without first providing
           ;; all positional optional args.
           (gather (and magik-completion-insert-gather-param
                        gather-raw
                        (or (null optional-raw) optional)
                        gather-raw))
           (start-sig (get-text-property 0 'magik-start-signature candidate))
           (all-params (append args
                               optional
                               (when gather
                                 gather)))
           (idx 0))
      (cond
       (start-sig
        (if all-params
            (let ((fields (mapcar (lambda (p)
                                    (cl-incf idx)
                                    (format "${%d:%s}" idx p))
                                  all-params)))
              (concat start-sig (string-join fields ", ") ")$0"))
          "()"))
       ((string-suffix-p "<<" candidate)
        (when-let* ((val (or (car args) (car optional-raw))))
          (concat " " (format "${1:%s}" val) "$0")))))))

(defun magik-completion--doc-buffer (candidate)
  "Return a documentation buffer for CANDIDATE, or nil if none available."
  (when-let* ((doc (get-text-property 0 'magik-documentation candidate)))
    (with-current-buffer (get-buffer-create " *magik-completion-doc*")
      (erase-buffer)
      (insert doc)
      (current-buffer))))

(defun magik-completion--exit-function (candidate status)
  "Exit function for method completion.
CANDIDATE is the completed string.
Inserts parameters as yasnippet when STATUS is `finished'."
  (when (and (eq status 'finished)
             magik-completion-insert-params
             (require 'yasnippet nil t)
             (fboundp 'yas-expand-snippet))
    (when-let* ((snippet (magik-completion--build-param-snippet candidate)))
      (yas-expand-snippet snippet))))

;;; --- CB-backed CAPF functions ---

(defun magik-completion-at-point-methods ()
  "Completion-at-point function for methods via Class Browser."
  (when magik-completion-enable-cb
    (when-let* ((bounds (magik-completion--method-bounds)))
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (prefix (buffer-substring-no-properties beg end))
             (exemplar (save-excursion
                         (goto-char beg)
                         (magik-completion--infer-exemplar))))
        (when exemplar
          (when-let* ((methods (magik-completion--query-methods exemplar prefix)))
            (list beg end methods
                  :exclusive 'no
                  :company-kind (lambda (_) 'method)
                  :annotation-function
                  (lambda (c)
                    (when-let* ((ann (get-text-property 0 'magik-annotation c)))
                      (concat " " ann)))
                  :company-doc-buffer #'magik-completion--doc-buffer
                  :exit-function #'magik-completion--exit-function)))))))

(defun magik-completion-at-point-classes ()
  "Completion-at-point function for class/exemplar names via CB."
  (when magik-completion-enable-cb
    (when-let* ((bounds (magik-completion--bounds)))
      (let ((beg (car bounds))
            (prefix (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (unless (or (string-prefix-p "_" prefix)
                    ;; Not after a dot — that's a method, not a class
                    (and (> beg (point-min))
                         (eq (char-before beg) ?.)))
          (when-let* ((classes (magik-completion--query-classes)))
            (list beg (cdr bounds) classes
                  :exclusive 'no
                  :company-kind (lambda (_) 'class))))))))

(defun magik-completion-at-point-globals ()
  "Completion-at-point function for globals/dynamics via CB."
  (when magik-completion-enable-cb
    (when-let* ((bounds (magik-completion--bounds)))
      (let ((prefix (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (when (string-prefix-p "!" prefix)
          (when-let* ((globals (magik-completion--query-globals)))
            (list (car bounds) (cdr bounds) globals
                  :exclusive 'no
                  :company-kind (lambda (_) 'variable))))))))

(defun magik-completion-at-point-global-procedures ()
  "Completion-at-point function for global procedures via CB."
  (when magik-completion-enable-cb
    (when-let* ((bounds (magik-completion--bounds)))
      (let ((beg (car bounds))
            (prefix (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (unless (or (string-prefix-p "_" prefix)
                    (and (> beg (point-min))
                         (eq (char-before beg) ?.)))
          (when-let* ((global-procedures (magik-completion--query-globals)))
            (list beg (cdr bounds) global-procedures
                  :exclusive 'no
                  :exit-function #'magik-completion--exit-function
                  :company-kind (lambda (_) 'method))))))))

;;; --- Condition completion ---

(defvar magik-completion--condition-cache nil
  "Cache: list of condition name strings from CB.")

(defvar magik-completion--condition-cache-loaded nil
  "Non-nil when the condition cache has been populated.")

(defun magik-completion--condition-bounds ()
  "Return bounds if point is after `condition.raise(:'  or similar.
Returns (BEG . END) of the condition name being typed, or nil."
  (let ((syntax (syntax-ppss)))
    (when (and (not (nth 3 syntax))
               (not (nth 4 syntax)))
      (save-excursion
        (let ((end (point))
              (beg (progn (skip-chars-backward "a-zA-Z0-9_!?") (point))))
          (when (and (> end beg)
                     (eq (char-before beg) ?:)
                     (save-excursion
                       (goto-char (1- beg))
                       (re-search-backward
                        "condition\\.raise(\\s-*\\="
                        (line-beginning-position) t)))
            (cons beg end)))))))

(defun magik-completion--query-conditions ()
  "Query all condition names from the CB.  Caches the result."
  (if magik-completion--condition-cache-loaded
      magik-completion--condition-cache
    (let* ((cmd (concat "method_name ^\n"
                        "unadd class \nadd class <condition>\n"
                        "method_cut_off " (number-to-string magik-completion-cb-max-methods) "\n"
                        "override_flags\nshow_classes\nshow_args\n"
                        "print_curr_methods\nshow_topics\n"))
           (result (magik-completion--cb-query cmd)))
      (when result
        (setq magik-completion--condition-cache result
              magik-completion--condition-cache-loaded t))
      result)))

(defun magik-completion-at-point-conditions ()
  "Completion-at-point function for condition names after `condition.raise(:'."
  (when magik-completion-enable-cb
    (when-let* ((bounds (magik-completion--condition-bounds))
                (conditions (magik-completion--query-conditions)))
      (list (car bounds) (cdr bounds) conditions
            :exclusive 'no
            :company-kind (lambda (_) 'enum-member)))))

;;; --- Cache invalidation ---

(defun magik-completion-invalidate-cache (&rest _args)
  "Invalidate all CB completion caches.
Intended to be called after transmitting code to the session."
  (interactive)
  (setq magik-completion--class-cache nil
        magik-completion--class-cache-loaded nil
        magik-completion--global-cache nil
        magik-completion--global-cache-loaded nil
        magik-completion--condition-cache nil
        magik-completion--condition-cache-loaded nil
        magik-completion--method-cache nil))

;;; --- Setup ---

(defvar magik-completion--capf-functions
  '(magik-completion-at-point-conditions
    magik-completion-at-point-global-procedures
    magik-completion-at-point-globals
    magik-completion-at-point-classes
    magik-completion-at-point-methods
    magik-completion-at-point-slots
    magik-completion-at-point-variables
    magik-completion-at-point-builtins
    magik-completion-at-point-keywords)
  "List of Magik CAPF functions, lowest priority first.")

(defun magik-completion--enable ()
  "Add Magik CAPF functions to the current buffer."
  (dolist (fn magik-completion--capf-functions)
    (add-hook 'completion-at-point-functions fn nil t))
  (when (fboundp 'magik-transmit-region)
    (advice-add 'magik-transmit-region :after #'magik-completion-invalidate-cache)))

(defun magik-completion--disable ()
  "Remove Magik CAPF functions from the current buffer."
  (dolist (fn magik-completion--capf-functions)
    (remove-hook 'completion-at-point-functions fn t))
  (when (fboundp 'magik-transmit-region)
    (advice-remove 'magik-transmit-region #'magik-completion-invalidate-cache)))

(define-minor-mode magik-completion-mode
  "Toggle Magik `completion-at-point' support in the current buffer."
  :lighter " MagikC"
  (if magik-completion-mode
      (magik-completion--enable)
    (magik-completion--disable)))

(defun magik-completion-setup ()
  "Add Magik CAPF functions to the current buffer.
Intended to be called from `magik-mode-hook' or `magik-session-mode-hook'.
Respects `magik-completion-enabled'."
  (when magik-completion-enabled
    (magik-completion-mode 1)))

(provide 'magik-completion)
;;; magik-completion.el ends here
