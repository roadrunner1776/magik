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
(declare-function treesit-node-text "treesit")
(declare-function treesit-node-start "treesit")

(defgroup magik-completion nil
  "Completion-at-point support for Magik."
  :group 'magik)

(defcustom magik-completion-enable-keywords t
  "When non-nil, include Magik keywords in completion candidates."
  :type 'boolean
  :group 'magik-completion)

(defcustom magik-completion-enable-variables t
  "When non-nil, include local variables from the current buffer."
  :type 'boolean
  :group 'magik-completion)

(defcustom magik-completion-enable-characters t
  "When non-nil, offer Magik character-literal names after `%'."
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
  (mapcar (lambda (name) (propertize name 'magik-package "sw"))
          '("DummyClassForMixinMethodTable" "ace_theme_definition"
            "alternative_access" "array_list" "attribute_layout"
            "auth!access" "base64_mime_converter" "bignum"
            "binary_operator" "byte_rope" "char16_string" "char16_vector"
            "character" "class_info_manager" "collection_export_engine"
            "colour_builder" "colour_value_manager" "compiler"
            "compiler_info" "compound_command" "concurrent_hash_map"
            "coords2h_rope" "coords3d_rope" "coords_rope"
            "crc32_checksum" "credential_store"
            "dash_pattern_value_manager" "database_pool_task_owner"
            "database_undoable_command" "dataset_spatial_object_stream"
            "dd_derived_field" "ds!areaindex" "ds!lanrootmap"
            "ds!lanrootmap7" "ds_environment" "ds_simple_vector"
            "dynamic_environment" "fill_percentage_value_manager" "float"
            "float_rope" "generic_bit_rope" "gis_id" "global_variable"
            "hatch_pattern_builder" "hatch_pattern_value_manager"
            "heavy_thread" "indexed_pool_manager" "int!zip_output_stream"
            "int64_rope" "integer_rope" "iter_perform_procedure"
            "java_affine_transform" "java_buffered_image"
            "java_file_dialog" "java_hash_set" "java_linked_hash_map"
            "java_logger" "java_message_proxy" "java_skip_list_set"
            "java_string" "jvm" "layout_document_properties"
            "lazy_record_collection_stream" "light_thread"
            "line_end_style_value_manager"
            "line_join_style_value_manager" "line_width_value_manager"
            "linked_list" "lru_cache_element" "magik_digest"
            "magik_input_method_event" "magik_url"
            "map_extent_prompt_dialog"
            "map_plugin_view_defaults_options_panel"
            "map_plugin_view_interaction_options_panel" "method"
            "method_overwrites" "method_table" "mixin" "moj_stack_frame"
            "options_dialog" "ordered_geometry_set" "osgi_bundle_manager"
            "output" "package" "paper_size" "pbkdf2_digest"
            "perform_procedure" "pixel_coords_rope" "pragma"
            "predicate_any_all" "predicate_count_helper"
            "predicate_join_helper" "predicate_navigate" "probe"
            "probe_chain" "procedure" "random" "rational_b_spline"
            "rational_b_spline_or_arc" "redacted_string"
            "render_controller" "rope" "rope_mixin"
            "sample_detail_filter" "sector" "sector_rope" "sector_z"
            "short_integer_rope" "simple_xml_handler" "simple_xml_parser"
            "simple_xml_serial_parser" "simple_xml_serial_reader"
            "simple_xml_thing_ns" "slot_access_procedure"
            "slot_descriptor" "spawner" "sub_package" "sw_cron_engine"
            "sw_custom_draw_styler" "sw_custom_geometry_styler"
            "sw_generic_task_owner" "sw_geometry_styler"
            "sw_gui_task_owner" "sw_job_engine" "sw_job_engine_helper"
            "sw_mapped_geom_style_discriminator"
            "sw_mapping_message_style_discriminator"
            "sw_parallel_task_owner" "sw_regexp"
            "sw_simple_style_discriminator"
            "sw_subcode_style_discriminator" "sw_task_owner"
            "sw_uncacheable_style_geometry_styler"
            "swdp_spatial_object_stream" "symbol" "sys!association"
            "sys!java_magik_condition" "sys!java_magik_exception"
            "sys!value_holder" "sys_slot_procedure" "system"
            "tabular_choice_lister" "timer" "timer_task" "uint64_rope"
            "unset" "xml_output_simple" "zip_entry" "zip_output_stream"))
  "List of commonly used Magik built-in names for completion.
All of these live in the `sw' package, so each name is tagged with
that via a `magik-package' text property, for doc-buffer lookups.")

(defconst magik-completion--character-names
  '("nul" "soh" "stx" "etx" "eot" "enq" "ack" "bell" "bel" "alert"
    "backspace" "bs" "tab" "ht" "newline" "lf" "linefeed" "vt"
    "verticaltab" "formfeed" "ff" "return" "cr" "so" "si" "dle" "dc1"
    "dc2" "dc3" "dc4" "nak" "syn" "etb" "can" "em" "sub" "escape" "esc"
    "fs" "gs" "rs" "us" "space" "blank" "delete" "del" "rubout" "csi"
    "nobreakspace" "exclamdown" "cent" "sterling" "currency" "yen"
    "brokenbar" "section" "diaeresis" "copyright" "ordfeminine"
    "guillemotleft" "notsign" "softhyphen" "registered" "macron"
    "degree" "plusminus" "superscripttwo" "superscriptthree" "acute"
    "mu" "paragraph" "centeredperiod" "cedilla" "superscriptone"
    "masculine" "guillemotright" "onequarter" "onehalf" "threequarters"
    "questiondown" "Agrave" "Aacute" "Acircumflex" "Atilde"
    "Adiaeresis" "Aring" "AE" "Ccedilla" "Egrave" "Eacute"
    "Ecircumflex" "Ediaeresis" "Igrave" "Iacute" "Icircumflex"
    "Idiaeresis" "Eth" "Ntilde" "Ograve" "Oacute" "Ocircumflex"
    "Otilde" "Odiaeresis" "multiply" "Ooblique" "Ugrave" "Uacute"
    "Ucircumflex" "Udiaeresis" "Yacute" "Thorn" "ssharp"
    "germandoubles" "agrave" "aacute" "acircumflex" "atilde"
    "adiaeresis" "aring" "ae" "ccedilla" "egrave" "eacute"
    "ecircumflex" "ediaeresis" "igrave" "iacute" "icircumflex"
    "idiaeresis" "eth" "ntilde" "ograve" "oacute" "ocircumflex"
    "otilde" "odiaeresis" "division" "oslash" "ugrave" "uacute"
    "ucircumflex" "udiaeresis" "yacute" "thorn" "ydiaeresis" "ind"
    "nel" "ssa" "esa" "hts" "htj" "vts" "pld" "plu" "ri" "ss2" "ss3"
    "dcs" "pu1" "pu2" "sts" "cch" "mw" "spa" "epa" "st" "osc" "pm"
    "apc" "nbs")
  "List of Magik `%name' character-literal names for completion.")

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

(defconst magik-completion--ts-param-scopes '("method" "procedure")
  "Tree-sitter node types that carry their own parameter list.")

(defconst magik-completion--ts-local-scopes '("method" "procedure" "block")
  "Tree-sitter node types that delimit a local variable scope.")

(defun magik-completion--ts-scan-variables ()
  "Scan variables using tree-sitter for accurate scope detection.
Returns a list of variable name strings visible at point."
  (let ((variables '())
        (node (treesit-node-at (point))))
    ;; Params come from the enclosing method/procedure even in a nested block.
    (when-let* ((scope (magik-completion--ts-enclosing-scope
                        node magik-completion--ts-param-scopes)))
      (setq variables (magik-completion--ts-collect-params scope variables)))
    (when-let* ((scope (magik-completion--ts-enclosing-scope
                        node magik-completion--ts-local-scopes)))
      (setq variables (magik-completion--ts-walk-for-assignments
                       scope (point) variables)))
    (delete-dups variables)))

(defun magik-completion--ts-enclosing-scope (node types)
  "Return the closest ancestor of NODE whose type is in TYPES."
  (let ((current node))
    (while (and current
                (not (member (treesit-node-type current) types)))
      (setq current (treesit-node-parent current)))
    current))

(defun magik-completion--ts-filter-children (node type)
  "Return NODE's children of TYPE."
  (seq-filter (lambda (child) (equal (treesit-node-type child) type))
              (treesit-node-children node)))

(defun magik-completion--ts-add-names (nodes limit variables)
  "Add the text of NODES to VARIABLES, skipping keywords and duplicates.
When LIMIT is non-nil, only nodes starting before LIMIT are added.
Returns the updated VARIABLES list."
  (dolist (node nodes)
    (when (or (null limit) (< (treesit-node-start node) limit))
      (let ((name (treesit-node-text node t)))
        (unless (or (string-prefix-p "_" name)
                    (member name variables))
          (push name variables)))))
  variables)

(defun magik-completion--ts-collect-params (scope-node variables)
  "Collect parameter names from SCOPE-NODE into VARIABLES list.
Parameters are the `argument' children of a method or procedure node.
Returns the updated VARIABLES list."
  (magik-completion--ts-add-names
   (magik-completion--ts-filter-children scope-node "argument")
   nil variables))

(defun magik-completion--ts-assignment-targets (node)
  "Return the `variable' nodes assigned to by assignment NODE.
The left-hand side is NODE's first child: a single variable or a
parenthesized tuple of variables."
  (let ((lhs (car (treesit-node-children node))))
    (cond
     ((null lhs) nil)
     ((equal (treesit-node-type lhs) "variable")
      (list lhs))
     ((equal (treesit-node-type lhs) "parenthesized_expression")
      (magik-completion--ts-filter-children lhs "variable")))))

(defun magik-completion--ts-walk-for-assignments (node limit variables)
  "Walk NODE tree collecting variable names assigned before LIMIT position.
Returns the updated VARIABLES list."
  (when (and node (< (treesit-node-start node) limit))
    (let ((type (treesit-node-type node)))
      (cond
       ;; Assignment: var << expr, (a, b) << expr
       ((equal type "assignment")
        (setq variables (magik-completion--ts-add-names
                         (magik-completion--ts-assignment-targets node)
                         limit variables)))
       ;; _local/_constant/_import declarations and _for loop variables
       ((member type '("local" "constant" "import" "iterator"))
        (setq variables (magik-completion--ts-add-names
                         (magik-completion--ts-filter-children node "identifier")
                         limit variables)))))
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
      (goto-char method-start)
      (while (re-search-forward
              "\\_<_local\\s-+\\([a-z_][a-z0-9_!?]*\\)" limit t)
        (let ((var (match-string-no-properties 1)))
          (unless (member var variables)
            (push var variables))))
      (goto-char method-start)
      (while (re-search-forward
              "\\b\\([a-z_][a-z0-9_!?]*\\)\\s-*<<" limit t)
        (let ((var (match-string-no-properties 1)))
          (unless (or (member var variables)
                      (string-prefix-p "_" var))
            (push var variables))))
      (goto-char method-start)
      (while (re-search-forward
              "\\_<_for\\s-+\\([a-z_][a-z0-9_!?, ]*\\)\\s-+_over" limit t)
        (let ((vars-str (match-string-no-properties 1)))
          (dolist (v (split-string vars-str "[, \t]+" t))
            (unless (member v variables)
              (push v variables)))))
      (goto-char method-start)
      (while (re-search-forward
              "\\_<_import\\s-+\\([a-z_][a-z0-9_!?, \t]*\\)" limit t)
        (dolist (v (split-string (match-string-no-properties 1) "[, \t]+" t))
          (unless (or (string-prefix-p "_" v)
                      (member v variables))
            (push v variables)))))
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

(defun magik-completion--exemplar-definition-region (exemplar)
  "Return the (BEG . END) region defining EXEMPLAR in the current buffer.
Finds the `def_slotted_exemplar'/`def_indexed_exemplar' form for
EXEMPLAR, ending at the next `$' terminator.  Returns nil when
EXEMPLAR is nil, empty, or not defined in this buffer."
  (when (and exemplar (not (string-empty-p exemplar)))
    (save-excursion
      (goto-char (point-min))
      (when (re-search-forward
             (concat "def_\\(?:slotted\\|indexed\\)_exemplar\\s-*([ \t\n]*:"
                     (regexp-quote exemplar) "\\_>")
             nil t)
        (cons (match-beginning 0)
              (or (re-search-forward "^\\$" nil t) (point-max)))))))

(defun magik-completion--scan-slots ()
  "Scan for slot names of the exemplar the method at point belongs to.
Slots are read from the matching exemplar definition when it can be
found in the buffer; otherwise the whole buffer is scanned.
Returns a list of slot name strings."
  (let* ((exemplar (when (fboundp 'magik-current-method-name)
                     (cadr (magik-current-method-name))))
         (region (magik-completion--exemplar-definition-region exemplar))
         (slots '()))
    (save-excursion
      (goto-char (or (car region) (point-min)))
      (while (re-search-forward
              "{\\s-*:\\([a-z_][a-z0-9_!?]*\\)\\s-*," (cdr region) t)
        (let ((slot (match-string-no-properties 1)))
          (unless (member slot slots)
            (push slot slots)))))
    (nreverse slots)))

;;; --- Prefix detection ---

(defun magik-completion--available-p ()
  "Return non-nil when completion may be offered at point.
In Magik session buffers, completion is limited to the command
input area after the last prompt."
  (or (not (derived-mode-p 'magik-session-mode))
      (magik-completion--session-input-p)))

(defun magik-completion--session-input-p ()
  "Return non-nil when point is after the last session prompt."
  (and (boundp 'magik-session-prompt)
       magik-session-prompt
       (save-excursion
         (let ((pos (point)))
           (goto-char (point-max))
           (and (re-search-backward magik-session-prompt nil t)
                (>= pos (match-end 0)))))))

(defun magik-completion--bounds ()
  "Return the bounds (BEG . END) of the Magik symbol at point.
Returns nil if point is inside a comment or string."
  (let ((syntax (syntax-ppss)))
    (when (and (magik-completion--available-p)
               (not (nth 3 syntax))   ; not in string
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
    (when (and (magik-completion--available-p)
               (not (nth 3 syntax))
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

(defun magik-completion--after-char-p (pos char)
  "Return non-nil if POS is immediately preceded by CHAR."
  (and (> pos (point-min))
       (eq (char-before pos) char)))

(defun magik-completion--character-bounds ()
  "Return bounds if point is completing a `%name' character literal.
Unlike sibling bounds functions, this allows an empty prefix (BEG == END)
so the full candidate list appears immediately after a bare `%'."
  (let ((syntax (syntax-ppss)))
    (when (and (magik-completion--available-p)
               (not (nth 3 syntax))
               (not (nth 4 syntax)))
      (let* ((end (point))
             (beg (save-excursion
                    (skip-chars-backward "a-zA-Z0-9")
                    (point))))
        (when (magik-completion--after-char-p beg ?%)
          (cons beg end))))))

(defun magik-completion--global-prefix-p (beg prefix)
  "Return non-nil if PREFIX starting at BEG is eligible for global completion.
This excludes `_' keywords and slot/method access after a `.'."
  (not (or (string-prefix-p "_" prefix)
           (and (> beg (point-min))
                (eq (char-before beg) ?.)))))

(defun magik-completion--typed-package (prefix)
  "Return the package qualifier typed before a `:' in PREFIX, or nil.
A leading colon with nothing before it (e.g. \":p\") is a symbol
literal, not a package qualifier, so that case returns nil."
  (when-let* ((colon (string-match ":" prefix))
              ((> colon 0)))
    (substring prefix 0 colon)))

(defun magik-completion--qualify-candidate (package candidate)
  "Return CANDIDATE prefixed with \"PACKAGE:\".
Carries over CANDIDATE's own text properties onto the whole result,
so code inspecting properties at position 0 (e.g. `magik-package',
`magik-args') keeps working regardless of the added qualifier."
  (let ((qualified (concat package ":" candidate)))
    (add-text-properties 0 (length qualified) (text-properties-at 0 candidate) qualified)
    qualified))

(defun magik-completion--kind-annotation (kind)
  "Return an :annotation-function labeling candidates \"(magik- KIND)\"."
  (let ((text (format " (magik-%s)" kind)))
    (lambda (_) text)))

;;; --- CAPF functions ---

(defun magik-completion-at-point-slots ()
  "Completion-at-point function for exemplar slot references."
  (when-let* ((bounds (magik-completion--slot-bounds))
              (slots (magik-completion--scan-slots)))
    (list (car bounds) (cdr bounds) slots
          :exclusive 'no
          :company-kind (lambda (_) 'slot)
          :annotation-function (magik-completion--kind-annotation 'slot))))

(defun magik-completion-at-point-character ()
  "Completion-at-point function for `%name' character literals."
  (when magik-completion-enable-characters
    (when-let* ((bounds (magik-completion--character-bounds)))
      (list (car bounds) (cdr bounds) magik-completion--character-names
            :exclusive 'no
            :company-kind (lambda (_) 'character)
            :annotation-function (magik-completion--kind-annotation 'character)))))

;;; --- Class Browser integration ---

(defvar-local magik-completion--cb-process nil
  "Dedicated CB process for completion in this buffer.")

(defvar-local magik-completion--cb-buffer-name nil
  "Buffer name for the dedicated completion CB process.")

(defvar-local magik-completion--cb-candidates nil
  "Result slot: set by the filter when CB responds.")

(defvar-local magik-completion--cb-filter-str ""
  "Accumulator for CB process filter output.")

(defvar-local magik-completion--cb-ready-p nil
  "When non-nil, a predicate checking whether cb is ready.
Checks whetherthe current query's response
\(`magik-completion--cb-filter-str'\) is complete.
Used for queries whose responses aren't signalled by a control character.")

(defvar-local magik-completion--cb-parse-fn nil
  "When non-nil, the parser to use for the current query's response.
overriding the default control-character dispatch.")

(defvar-local magik-completion--cb-on-response nil
  "Callback for the in-flight query; see `magik-completion--cb-dispatch'.")

(defvar-local magik-completion--cb-queue nil
  "FIFO of (COMMAND READY-P PARSE-FN ON-RESPONSE) queue.
queries waiting behind the in-flight one on this connection.")

(defvar-local magik-completion--cb-generation 0
  "Bumped on every dispatch; lets a timeout tell if it's stale.")

(defvar magik-completion--class-cache nil
  "Cache: list of all class/exemplar names from CB.")

(defvar magik-completion--class-cache-loaded nil
  "Non-nil when the class cache has been populated.")

(defvar magik-completion--class-fetch-pending nil
  "Non-nil while a background class-cache fetch is in flight.")

(defvar magik-completion--global-cache nil
  "Cache: list of all global/dynamic names from CB.")

(defvar magik-completion--global-cache-loaded nil
  "Non-nil when the global cache has been populated.")

(defvar magik-completion--global-fetch-pending nil
  "Non-nil while a background global-cache fetch is in flight.")

(defvar magik-completion--method-cache nil
  "Cache: cons (KEY . CANDIDATES) for method completion.
KEY is \"class.first-char\" to detect when to re-query.")

(defvar magik-completion--method-fetch-pending nil
  "Cache key currently being fetched in the background, or nil.")

(defcustom magik-completion-enable-cb t
  "When non-nil, use the Class Browser for method/class/global completion."
  :type 'boolean
  :group 'magik-completion)

(defcustom magik-completion-cb-timeout 20.0
  "Seconds to wait for a CB response before giving up and restarting.
Non-blocking, so this can be generous: a fresh CB can be slow to
answer its first query while `method_finder' loads its database."
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
(declare-function magik-product-transmit-buffer "magik-product")
(declare-function magik-module-transmit-buffer "magik-module")
(declare-function magik-loadlist-transmit-buffer "magik-loadlist")
(declare-function magik-transmit-region "magik-mode")
(declare-function magik-session-buffer-alist-sorted "magik-session")

(defvar magik-cb-coding-system)
(defvar magik-session-buffer-alist)
(defvar magik-session-prompt)
(defvar magik-cb-in-keyword)

;;; --- CB process management ---

(defun magik-completion--cb-buffer ()
  "Return the buffer name for the completion CB process."
  (or magik-completion--cb-buffer-name
      (setq magik-completion--cb-buffer-name
            (concat " *cb*" (buffer-name) "*completion*"))))

(defun magik-completion--gis-buffer ()
  "Return the lowest-numbered live Magik session buffer name, or nil."
  (when (fboundp 'magik-session-buffer-alist-sorted)
    (cl-loop for (_num . buf) in (magik-session-buffer-alist-sorted)
             when (and buf (get-buffer buf) (get-buffer-process buf))
             return buf)))

(defun magik-completion--gis-session-idle-p (gis-buf)
  "Return non-nil if the session in GIS-BUF is idle at its prompt.
Checked before starting a CB connection, since that blocks until
the session answers."
  (with-current-buffer gis-buf
    (when-let* ((proc (get-buffer-process gis-buf))
                (mark-pos (marker-position (process-mark proc))))
      (and (boundp 'magik-session-prompt)
           magik-session-prompt
           (save-excursion
             (goto-char mark-pos)
             (and (re-search-backward magik-session-prompt nil t)
                  (save-match-data
                    (save-excursion
                      (goto-char (match-end 0))
                      (skip-chars-forward " \t\n")
                      (>= (point) mark-pos)))))))))

(defun magik-completion--ensure-cb-process ()
  "Ensure a dedicated CB process is running."
  (when (and magik-completion-enable-cb
             (require 'magik-cb nil t))
    (if (and magik-completion--cb-process
             (process-live-p magik-completion--cb-process))
        magik-completion--cb-process
      ;; Try to start one, but only once the session is idle; otherwise
      ;; this would block inside `magik-cb-get-process-create'.
      (when-let* ((gis-buf (magik-completion--gis-buffer))
                  ((magik-completion--gis-session-idle-p gis-buf)))
        (let* ((smallworld-gis (buffer-local-value
                                'magik-smallworld-gis (get-buffer gis-buf)))
               (cb-buf (magik-completion--cb-buffer)))
          (condition-case nil
              (let ((proc (cl-letf (((symbol-function 'magik-cb-mode)
                                     'fundamental-mode))
                            (magik-cb-get-process-create
                             cb-buf
                             #'magik-completion--cb-filter
                             smallworld-gis gis-buf nil))))
                (when (and proc (process-live-p proc))
                  (setq magik-completion--cb-process proc
                        magik-completion--cb-buffer-name
                        (buffer-name (process-buffer proc)))
                  ;; Buffer may be reused after a restart; start clean.
                  (with-current-buffer (process-buffer proc)
                    (setq magik-completion--cb-candidates nil
                          magik-completion--cb-filter-str ""
                          magik-completion--cb-ready-p nil
                          magik-completion--cb-parse-fn nil
                          magik-completion--cb-on-response nil
                          magik-completion--cb-queue nil))
                  proc))
            (error nil)))))))

;;; --- CB filter ---

(defun magik-completion--cb-filter (proc str)
  "Process filter for the completion CB process PROC.
Accumulates STR until a control char signals end of output,
then parses the temp file.  When `magik-completion--cb-ready-p' is set,
uses it and `magik-completion--cb-parse-fn' instead, for queries whose
responses come directly over the connection with no control char."
  (when-let* ((buf (process-buffer proc))
              (_ (buffer-live-p buf)))
    (with-current-buffer buf
        (setq magik-completion--cb-filter-str
              (concat magik-completion--cb-filter-str str))
        (let ((coding-system-for-read (if (boundp 'magik-cb-coding-system)
                                          magik-cb-coding-system
                                        'utf-8)))
          (cond
           (magik-completion--cb-ready-p
            (when (funcall magik-completion--cb-ready-p magik-completion--cb-filter-str)
              (let ((result (funcall magik-completion--cb-parse-fn
                                     magik-completion--cb-filter-str)))
                (setq magik-completion--cb-filter-str ""
                      magik-completion--cb-ready-p nil
                      magik-completion--cb-parse-fn nil)
                (magik-completion--cb-deliver result))))
           ;; \C-e signals method list output ready
           ((string-match "\C-e" magik-completion--cb-filter-str)
            (setq magik-completion--cb-filter-str "")
            (let ((buffer-read-only nil))
              (erase-buffer)
              (insert-file-contents (magik-cb-temp-file-name proc) nil nil nil t))
            (magik-completion--cb-deliver (magik-completion--parse-methods)))
           ;; \C-c signals class list output ready
           ((string-match "\C-c" magik-completion--cb-filter-str)
            (setq magik-completion--cb-filter-str "")
            (let ((buffer-read-only nil))
              (erase-buffer)
              (insert-file-contents (magik-cb-temp-file-name proc) nil nil nil t))
            (magik-completion--cb-deliver (magik-completion--parse-classes))))))))

;;; --- CB dispatch/delivery ---
;;
;; One connection, one command at a time (no request id to match a
;; reply to); these serialize queries onto a per-connection queue and
;; deliver each result via callback instead of blocking the caller.

(defun magik-completion--cb-dispatch (buf command ready-p parse-fn on-response)
  "Send COMMAND on the CB process owning BUF, without blocking.
ON-RESPONSE gets the parsed result, or nil on timeout.  READY-P and
PARSE-FN override the default dispatch, see `magik-completion--cb-filter'."
  (let ((generation
         (with-current-buffer buf
           (setq magik-completion--cb-candidates 'pending
                 magik-completion--cb-filter-str ""
                 magik-completion--cb-ready-p ready-p
                 magik-completion--cb-parse-fn parse-fn
                 magik-completion--cb-on-response on-response)
           (cl-incf magik-completion--cb-generation))))
    (process-send-string (get-buffer-process buf) command)
    (run-at-time magik-completion-cb-timeout nil
                 'magik-completion--cb-timeout buf generation)))

(defun magik-completion--cb-timeout (buf generation)
  "Give up on BUF's in-flight query if GENERATION is still current.
Restart the connection so a late reply can't corrupt the next one."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (and (= generation magik-completion--cb-generation)
                 (eq magik-completion--cb-candidates 'pending))
        (let ((callbacks (delq nil (cons magik-completion--cb-on-response
                                         (mapcar (lambda (q) (nth 3 q))
                                                 magik-completion--cb-queue))))
              (proc (get-buffer-process buf)))
          (setq magik-completion--cb-on-response nil
                magik-completion--cb-queue nil
                magik-completion--cb-candidates nil)
          (when proc (delete-process proc))
          (dolist (callback callbacks) (funcall callback nil)))))))

(defun magik-completion--cb-deliver (result)
  "Store RESULT as the in-flight query's outcome and notify its caller.
Then dispatch the next queued query on this connection, if any."
  (setq magik-completion--cb-candidates result)
  (when-let* ((callback magik-completion--cb-on-response))
    (setq magik-completion--cb-on-response nil)
    (funcall callback result))
  (when-let* ((next (pop magik-completion--cb-queue)))
    (apply 'magik-completion--cb-dispatch (current-buffer) next)))

(declare-function corfu--post-command "corfu")

(defun magik-completion--nudge-doc-display ()
  "Redisplay doc for the current candidate without restarting completion.
No-op unless Corfu popupinfo is the active frontend."
  (when (and completion-in-region-mode
             (bound-and-true-p corfu-popupinfo-mode)
             (fboundp 'corfu--post-command))
    (ignore-errors (corfu--post-command))))

(defun magik-completion--cb-query-async (command callback &optional ready-p parse-fn no-refresh)
  "Send COMMAND to the CB without blocking.
Return t if dispatched or queued, nil if no CB is available; CALLBACK
gets the result later.  Unless NO-REFRESH (for doc-only fetches),
completion is recomputed to show it.  READY-P/PARSE-FN override the
default dispatch, see `magik-completion--cb-filter'."
  (when-let* ((proc (magik-completion--ensure-cb-process))
              (buf (process-buffer proc))
              (_ (buffer-live-p buf)))
    (let* ((requester (current-buffer))
           (on-response
            (lambda (result)
              (run-at-time
               0 nil
               (lambda ()
                 (when (buffer-live-p requester)
                   (with-current-buffer requester
                     (funcall callback result)
                     (if no-refresh
                         (magik-completion--nudge-doc-display)
                       (when (and completion-in-region-mode
                                  (fboundp 'completion-at-point))
                         (ignore-errors (completion-at-point)))))))))))
      (if (eq (buffer-local-value 'magik-completion--cb-candidates buf) 'pending)
          (with-current-buffer buf
            (setq magik-completion--cb-queue
                  (append magik-completion--cb-queue
                          (list (list command ready-p parse-fn on-response)))))
        (magik-completion--cb-dispatch buf command ready-p parse-fn on-response)))
    t))

(defun magik-completion--cb-cached-fetch (cache-var loaded-var pending-var command)
  "Return CACHE-VAR's value once LOADED-VAR is set.
Otherwise fetch COMMAND in the background (unless PENDING-VAR is
already set) and return nil.  Args are variable symbols, not values,
to drive several CB caches."
  (cond
   ((symbol-value loaded-var) (symbol-value cache-var))
   ((symbol-value pending-var) nil)
   (t
    (set pending-var t)
    (unless (magik-completion--cb-query-async
             command
             (lambda (result)
               (set pending-var nil)
               (when result
                 (set cache-var result)
                 (set loaded-var t))))
      ;; No CB available, so the callback never runs; clear it ourselves.
      (set pending-var nil))
    nil)))

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
           ((looking-at "[[:alnum:]_?!]+")
            (let ((name (match-string-no-properties 0)))
              (goto-char (match-end 0))
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
Returns a list of class name strings, each propertized with its
package under `magik-package' (e.g. \"sw\" for \"sw:rope\')."
  (let ((candidates '())
        (regexp "\\(\\S-+\\):\\(\\S-+\\)"))
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward regexp nil t)
        (let ((package (match-string-no-properties 1))
              (name (match-string-no-properties 2)))
          (unless (member name candidates)
            (push (propertize name 'magik-package package) candidates)))))
    (nreverse candidates)))

;;; --- Class comment queries ---
;;
;; `get_class_info comments <class>' answers directly over the connection
;; with no control character: a line count N, then exactly N lines of
;; comment text (or a single "method finder: Invalid class" line for an
;; unknown class).  This needs its own readiness/parsing logic instead of
;; the usual \C-e / \C-c + temp-file dispatch.

(defun magik-completion--class-comment-nth-line-end (str n)
  "Return the buffer position in STR just after its Nth newline.
Returns nil if STR has fewer than N newlines."
  (let ((pos 0))
    (catch 'done
      (dotimes (_ n)
        (let ((next (string-match "\n" str pos)))
          (unless next (throw 'done nil))
          (setq pos (1+ next))))
      pos)))

(defun magik-completion--class-comment-ready-p (str)
  "Return non-nil once STR is a complete get_class_info comments response."
  (when-let* ((first-nl (string-match "\n" str)))
    (let ((count-str (substring str 0 first-nl)))
      (if (string-match-p "\\`[0-9]+\\'" count-str)
          (magik-completion--class-comment-nth-line-end
           str (1+ (string-to-number count-str)))
        t))))

(defun magik-completion--parse-class-comment (str)
  "Extract the comment text from a get_class_info comments response STR.
Returns nil if the class has no comment or wasn't found."
  (when-let* ((first-nl (string-match "\n" str))
              (count-str (substring str 0 first-nl))
              ((string-match-p "\\`[0-9]+\\'" count-str))
              (n (string-to-number count-str))
              ((not (zerop n)))
              (body-end (magik-completion--class-comment-nth-line-end str (1+ n))))
    (string-trim (substring str (1+ first-nl) body-end))))

(defvar magik-completion--class-comment-cache (make-hash-table :test 'equal)
  "Cache: qualified class name -> its comment text (or `none') from CB.")

(defvar magik-completion--class-comment-fetch-pending nil
  "Qualified class name currently being fetched, or nil.")

(defun magik-completion--query-class-comment (class)
  "Return CLASS's own comment from the CB, caching by CLASS.
Kicks off a background fetch and returns nil when not yet cached."
  (let ((cached (gethash class magik-completion--class-comment-cache 'missing)))
    (cond
     ((eq cached 'none) nil)
     ((not (eq cached 'missing)) cached)
     ((equal magik-completion--class-comment-fetch-pending class) nil)
     (t
      (setq magik-completion--class-comment-fetch-pending class)
      (unless (magik-completion--cb-query-async
               (concat "get_class_info comments " class "\n")
               (lambda (result)
                 (setq magik-completion--class-comment-fetch-pending nil)
                 (puthash class (or result 'none) magik-completion--class-comment-cache))
               #'magik-completion--class-comment-ready-p
               #'magik-completion--parse-class-comment
               t)
        ;; No CB available right now: nothing will clear the flag.
        (setq magik-completion--class-comment-fetch-pending nil))
      nil))))

;;; --- CB queries ---

(defun magik-completion--query-methods (class prefix)
  "Return cached methods on CLASS starting with PREFIX.
Otherwise fetch in the background and return nil."
  (let* ((char (if (string-empty-p prefix) "" (substring prefix 0 1)))
         (cache-key (concat class "." char)))
    (cond
     ((and magik-completion--method-cache
           (equal cache-key (car magik-completion--method-cache)))
      (cdr magik-completion--method-cache))
     ((equal magik-completion--method-fetch-pending cache-key) nil)
     (t
      (setq magik-completion--method-fetch-pending cache-key)
      (unless (magik-completion--cb-query-async
               (concat "method_name ^" char "\n"
                       "unadd class \nadd class " class "$\n"
                       "method_cut_off " (number-to-string magik-completion-cb-max-methods) "\n"
                       "override_flags\nshow_classes\nshow_args\nshow_comments\n"
                       "print_curr_methods\nshow_topics\n")
               (lambda (result)
                 (setq magik-completion--method-fetch-pending nil)
                 (when result
                   (setq magik-completion--method-cache (cons cache-key result)))))
        ;; No CB available right now: nothing will clear the flag.
        (setq magik-completion--method-fetch-pending nil))
      nil))))

(defun magik-completion--query-classes ()
  "Return all classes from the CB, fetching in the background first."
  (magik-completion--cb-cached-fetch
   'magik-completion--class-cache
   'magik-completion--class-cache-loaded
   'magik-completion--class-fetch-pending
   "dont_override_flags\npr_family sw:object\n"))

(defun magik-completion--query-globals ()
  "Return all globals from the CB, fetching in the background first."
  (magik-completion--cb-cached-fetch
   'magik-completion--global-cache
   'magik-completion--global-cache-loaded
   'magik-completion--global-fetch-pending
   (concat "method_name ^\n"
           "unadd class \nadd class <global>\n"
           "method_cut_off " (number-to-string magik-completion-cb-max-methods) "\n"
           "override_flags\nshow_classes\nshow_args\nshow_comments\n"
           "print_curr_methods\nshow_topics\n")))

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
         ;; Fallback: pass the name straight to CB, which covers globals
         ;; like gis_program_manager and handles unknown classes gracefully.
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
    (when (and (magik-completion--available-p)
               (not (nth 3 syntax))
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

;;; --- Yasnippet template candidates ---

(defcustom magik-completion-enable-snippets t
  "When non-nil, offer yasnippet template keys as completion candidates.
Completing a key expands the snippet template."
  :type 'boolean
  :group 'magik-completion)

(declare-function yas--all-templates "yasnippet")
(declare-function yas--get-snippet-tables "yasnippet")
(declare-function yas--template-key "yasnippet")

(defun magik-completion--snippet-templates ()
  "Return the yasnippet templates active in the current buffer."
  (when (and (bound-and-true-p yas-minor-mode)
             (require 'yasnippet nil t))
    (yas--all-templates (yas--get-snippet-tables major-mode))))

(defun magik-completion--snippet-lookup (key)
  "Return the active yasnippet template whose key is KEY, or nil."
  (seq-find (lambda (template)
              (equal (yas--template-key template) key))
            (magik-completion--snippet-templates)))

(defun magik-completion--snippet-exit-function (candidate status)
  "Expand the yasnippet template with key CANDIDATE when STATUS is `finished'."
  (when (and (eq status 'finished)
             (fboundp 'yas-expand-snippet))
    (when-let* ((template (magik-completion--snippet-lookup candidate)))
      (delete-region (- (point) (length candidate)) (point))
      (yas-expand-snippet template))))

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
           ;; Gather only valid if no optional params were skipped before it.
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

(defun magik-completion--qualified-class-name (candidate)
  "Return CANDIDATE qualified with its package, e.g. \"sw:rope\'.
If CANDIDATE is already package-qualified it's used as-is;
otherwise this falls back to its `magik-package' text property.
`get_class_info' requires a package-qualified class name."
  (cond
   ((string-match-p ":" candidate) candidate)
   ((get-text-property 0 'magik-package candidate)
    (concat (get-text-property 0 'magik-package candidate) ":" candidate))
   (t candidate)))

(defun magik-completion--class-doc-buffer (candidate)
  "Return a documentation buffer for class CANDIDATE, or nil if none available."
  (when-let* ((doc (magik-completion--query-class-comment
                     (magik-completion--qualified-class-name candidate))))
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
                    (concat (when-let* ((ann (get-text-property 0 'magik-annotation c)))
                              (concat " " ann))
                            " (magik-method)"))
                  :company-doc-buffer 'magik-completion--doc-buffer
                  :exit-function 'magik-completion--exit-function)))))))

(defun magik-completion--tag-kind (candidates kind)
  "Return CANDIDATES with a `magik-kind' text property set to KIND.
Existing text properties on each candidate (e.g. `magik-package') are
preserved."
  (mapcar (lambda (c) (propertize c 'magik-kind kind)) candidates))

(defun magik-completion--tag-global (candidate)
  "Tag global CANDIDATE with its `magik-kind'.
Bang-delimited names (e.g. \"!terminal!\") are Magik's convention for
dynamic variables, so they're tagged `variable'; everything else is a
global procedure, tagged `method'."
  (propertize candidate 'magik-kind
              (if (and (string-prefix-p "!" candidate)
                       (string-suffix-p "!" candidate)
                       (> (length candidate) 1))
                  'variable
                'method)))

(defun magik-completion--symbol-table (qualifiable plain)
  "Return a completion table over the union of QUALIFIABLE and PLAIN.
QUALIFIABLE candidates (built-ins, classes, globals) also match
package-qualified forms like \"sw:rope\" when the typed text has a
`package:' qualifier, by generating qualified candidate strings on the
fly (see `magik-completion--qualify-candidate').  PLAIN candidates
\(local variables, snippet keys) are never package-qualified."
  (lambda (string pred action)
    (complete-with-action
     action
     (append
      (if-let* ((package (magik-completion--typed-package string)))
          (mapcar (lambda (c) (magik-completion--qualify-candidate package c))
                  qualifiable)
        qualifiable)
      plain)
     string pred)))

(defun magik-completion--symbol-kind (candidate)
  "Return CANDIDATE's `magik-kind' text property, for :company-kind."
  (get-text-property 0 'magik-kind candidate))

(defun magik-completion--symbol-annotation (candidate)
  "Return an annotation for CANDIDATE based on its `magik-kind'."
  (funcall (magik-completion--kind-annotation
            (magik-completion--symbol-kind candidate))
           candidate))

(defun magik-completion--symbol-doc-buffer (candidate)
  "Return a documentation buffer for CANDIDATE based on its `magik-kind'."
  (pcase (magik-completion--symbol-kind candidate)
    ((or 'constant 'class) (magik-completion--class-doc-buffer candidate))
    ((or 'method 'variable) (magik-completion--doc-buffer candidate))))

(defun magik-completion--symbol-exit-function (candidate status)
  "Exit function for CANDIDATE based on its `magik-kind'.
STATUS is passed through to the kind-specific handler."
  (pcase (magik-completion--symbol-kind candidate)
    ('method (magik-completion--exit-function candidate status))
    ('snippet (magik-completion--snippet-exit-function candidate status))))

(defun magik-completion-at-point-symbol ()
  "Completion-at-point function for identifiers.
Covers keywords, built-ins, local variables, classes, global
procedures/dynamics, and yasnippet template keys."
  (when-let* ((bounds (magik-completion--bounds))
              (beg (car bounds))
              (end (cdr bounds))
              ;; `%name' is handled by `magik-completion-at-point-character';
              ;; `@' is excluded outright.
              ((not (or (magik-completion--after-char-p beg ?%)
                        (magik-completion--after-char-p beg ?@))))
              (prefix (buffer-substring-no-properties beg end)))
    (cond
     ((string-prefix-p "_" prefix)
      (when magik-completion-enable-keywords
        (list beg end magik-completion--keywords
              :exclusive 'no
              :company-kind (lambda (_) 'keyword)
              :annotation-function (magik-completion--kind-annotation 'keyword))))
     ((magik-completion--global-prefix-p beg prefix)
      (let ((qualifiable
             (append
              (when magik-completion-enable-keywords
                (magik-completion--tag-kind magik-completion--builtins 'constant))
              (when magik-completion-enable-cb
                (magik-completion--tag-kind (magik-completion--query-classes) 'class))
              (when magik-completion-enable-cb
                (mapcar 'magik-completion--tag-global
                        (magik-completion--query-globals)))))
            (plain
             (append
              (when magik-completion-enable-variables
                (magik-completion--tag-kind
                 (magik-completion--scan-local-variables) 'variable))
              (when magik-completion-enable-snippets
                (magik-completion--tag-kind
                 (delq nil (mapcar 'yas--template-key
                                    (magik-completion--snippet-templates)))
                 'snippet)))))
        (when (or qualifiable plain)
          (list beg end (magik-completion--symbol-table qualifiable plain)
                :exclusive 'no
                :company-kind 'magik-completion--symbol-kind
                :annotation-function 'magik-completion--symbol-annotation
                :company-doc-buffer 'magik-completion--symbol-doc-buffer
                :exit-function 'magik-completion--symbol-exit-function)))))))

;;; --- Condition completion ---

(defvar magik-completion--condition-cache nil
  "Cache: list of condition name strings from CB.")

(defvar magik-completion--condition-cache-loaded nil
  "Non-nil when the condition cache has been populated.")

(defvar magik-completion--condition-fetch-pending nil
  "Non-nil while a background condition-cache fetch is in flight.")

(defun magik-completion--condition-bounds ()
  "Return bounds if point is after `condition.raise(:'  or similar.
Returns (BEG . END) of the condition name being typed, or nil."
  (let ((syntax (syntax-ppss)))
    (when (and (magik-completion--available-p)
               (not (nth 3 syntax))
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
  "Return all condition names from the CB, fetching in the background first."
  (magik-completion--cb-cached-fetch
   'magik-completion--condition-cache
   'magik-completion--condition-cache-loaded
   'magik-completion--condition-fetch-pending
   (concat "method_name ^\n"
           "unadd class \nadd class <condition>\n"
           "method_cut_off " (number-to-string magik-completion-cb-max-methods) "\n"
           "override_flags\nshow_classes\nshow_args\n"
           "print_curr_methods\nshow_topics\n")))

(defun magik-completion-at-point-conditions ()
  "Completion-at-point function for condition names after `condition.raise(:'."
  (when magik-completion-enable-cb
    (when-let* ((bounds (magik-completion--condition-bounds))
                (conditions (magik-completion--query-conditions)))
      (list (car bounds) (cdr bounds) conditions
            :exclusive 'no
            :company-kind (lambda (_) 'enum-member)
            :annotation-function (magik-completion--kind-annotation 'enum-member)))))

;;; --- Cache invalidation ---

(defun magik-completion-invalidate-cache ()
  "Invalidate all CB completion caches.
Can be called after loading code in the session."
  (interactive)
  (magik-completion--invalidate-cache))

(defun magik-completion--invalidate-cache (&rest _args)
  "Invalidate all CB completion caches.
Intended to be called after transmitting code to the session."
  (setq magik-completion--class-cache nil
        magik-completion--class-cache-loaded nil
        magik-completion--class-fetch-pending nil
        magik-completion--global-cache nil
        magik-completion--global-cache-loaded nil
        magik-completion--global-fetch-pending nil
        magik-completion--condition-cache nil
        magik-completion--condition-cache-loaded nil
        magik-completion--condition-fetch-pending nil
        magik-completion--method-cache nil
        magik-completion--method-fetch-pending nil
        magik-completion--class-comment-fetch-pending nil)
  (clrhash magik-completion--class-comment-cache))

(defun magik-completion--reset-session-state (&rest _args)
  "Invalidate caches and kill all dedicated completion CB buffers.
Also clears the dedicated-CB-connection state of every buffer with
`magik-completion-mode' enabled.
Invalidating cache twice helped with some caching issues."
  (magik-completion-invalidate-cache)
  (dolist (buf (buffer-list))
    (when (buffer-local-value 'magik-completion-mode buf)
      (with-current-buffer buf
        (setq magik-completion--cb-process nil
              magik-completion--cb-buffer-name nil
              magik-completion--cb-candidates nil
              magik-completion--cb-filter-str ""
              magik-completion--cb-ready-p nil
              magik-completion--cb-parse-fn nil
              magik-completion--cb-on-response nil
              magik-completion--cb-queue nil)))
    (let ((name (buffer-name buf)))
      (when (and name
                 (string-prefix-p " *cb*" name)
                 (string-suffix-p "*completion*" name))
        (when-let* ((proc (get-buffer-process buf)))
          (delete-process proc))
        (kill-buffer buf))))
  (magik-completion-invalidate-cache))

;;; --- Setup ---

(defvar magik-completion--capf-functions
  '(magik-completion-at-point-conditions
    magik-completion-at-point-methods
    magik-completion-at-point-slots
    magik-completion-at-point-character
    magik-completion-at-point-symbol)
  "List of Magik CAPF functions, lowest priority first.")

(defvar magik-completion--transmit-hooks
  '(magik-product-transmit-buffer-post-hook
    magik-module-transmit-buffer-post-hook
    magik-loadlist-transmit-buffer-post-hook
    magik-transmit-region-post-hook))

;;;###autoload
(define-minor-mode magik-completion-mode
  "Toggle Magik completion in the current buffer."
  :lighter " MagikComp"
  (if magik-completion-mode
      (magik-completion--local-enable)
    (magik-completion--local-disable)))

(defun magik-completion--local-enable ()
  "Add the Magik CAPF functions to `completion-at-point-functions'."
  (dolist (fn magik-completion--capf-functions)
    (add-hook 'completion-at-point-functions fn nil t)))

(defun magik-completion--local-disable ()
  "Remove the Magik CAPF functions from `completion-at-point-functions'."
  (dolist (fn magik-completion--capf-functions)
    (remove-hook 'completion-at-point-functions fn t)))

;;;###autoload
(define-globalized-minor-mode global-magik-completion-mode
  magik-completion-mode
  magik-completion--turn-on
  (if global-magik-completion-mode
      (magik-completion--global-enable)
    (magik-completion--global-disable)))

(defun magik-completion--turn-on ()
  "Turn on `magik-completion-mode' in Magik source and session buffers."
  (when (derived-mode-p 'magik-base-mode 'magik-session-mode)
    (magik-completion-mode 1)))

(defun magik-completion--global-enable ()
  "Add the hooks backing `global-magik-completion-mode'."
  (add-hook 'magik-session-kill-process-post-hook
            'magik-completion--reset-session-state)
  (add-hook 'magik-session-start-process-post-hook
            'magik-completion--reset-session-state)
  (add-hook 'magik-session-set-priority-post-hook
            'magik-completion--reset-session-state)
  (dolist (hook-var magik-completion--transmit-hooks)
    (add-hook hook-var 'magik-completion-invalidate-cache)))

(defun magik-completion--global-disable ()
  "Remove the hooks backing `global-magik-completion-mode'."
  (remove-hook 'magik-session-kill-process-post-hook
               'magik-completion--reset-session-state)
  (remove-hook 'magik-session-start-process-post-hook
               'magik-completion--reset-session-state)
  (remove-hook 'magik-session-set-priority-post-hook
               'magik-completion--reset-session-state)
  (dolist (hook-var magik-completion--transmit-hooks)
    (remove-hook hook-var 'magik-completion-invalidate-cache)))

(global-magik-completion-mode 1)

(provide 'magik-completion)
;;; magik-completion.el ends here
