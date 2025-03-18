;;; magik-mode.el --- Emacs major mode for Smallworld Magik files

;; Package-Version: 0.4.1
;; Package-Requires: ((emacs "24.4") (compat "28.1") (yasnippet "0.14.0"))
;; URL: https://github.com/roadrunner1776/magik
;; Keywords: languages

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

;; This package provides a major mode for editing Smallworld Magik files.

;;; Code:

(eval-when-compile
  (require 'font-lock)
  (defvar msb-menu-cond)
  (defvar ac-sources)
  (defvar ac-prefix)
  (defvar ac-modes)
  (require 'magik-indent)
  (require 'magik-electric)
  (require 'magik-pragma)
  (require 'magik-doc-gen))

(require 'compat)
(require 'imenu)
(require 'yasnippet)
(require 'magik-template)

(defgroup magik nil
  "Customise Magik Language group."
  :group 'smallworld
  :group 'languages)

(defcustom magik-transmit-debug-p nil
  "*If t, \"#DEBUG\" patterns get stripped out of things being transmitted to magik."
  :group 'magik
  :type  'boolean)

(defcustom magik-under-as-char t
  "*Non-nil means that the _ (underline) should be treated as word char."
  :group 'magik
  :type  'boolean)

(defcustom magik-mark-method-exchange t
  "*If t, `magik-mark-method' (\\[magik-mark-method]) leaves point at end of marked method region.
nil leaves point at start of marked method region.
Standard Emacs marking commands like `mark-paragraph' and `mark-sexp'
\(on keys \\[mark-paragraph] and \\[mark-sexp] respectively)
normally leave point at the start of the region.
But to keep most users happy, this is defaulted to t as this is
what most Smallworld users expect.
Users can also swap the point and mark positions using \\[exchange-point-and-mark]."
  :group 'magik
  :type  'boolean)

(defcustom magik-work-buffer nil
  "*Name of Magik buffer to be used for development work."
  :group 'magik
  :type  'string)

(defcustom magik-auto-abbrevs t
  "*User option which enables magik abbreviation expansion automatically."
  :group 'magik
  :type  'boolean)

(defcustom magik-sw-menu-file-max-length 90
  "*The maximum length of the displayed path in the SW -> Magik Files submenu."
  :group 'magik
  :type  'integer)

(define-derived-mode magik-base-mode prog-mode "Magik"
  "Generic major mode for editing Magik files.

This is a generic major mode intended to be inherited by
concrete implementations."
  :group 'magik
  :interactive nil

  (compat-call setq-local
               magik-template-file-type (magik-template-file-type)
               paragraph-start (concat "^$\\|" page-delimiter)
               paragraph-separate paragraph-start
               require-final-newline mode-require-final-newline
               comment-start "#"
               comment-end ""
               comment-column 8
               comment-start-skip "#+ *"
               comment-multi-line nil
               parse-sexp-ignore-comments nil
               magik-transmit-debug-mode-line-string " #DEBUG"
               imenu-generic-expression imenu-generic-expression
               imenu-create-index-function 'magik-imenu-create-index-function
               imenu-syntax-alist '((?_ . "w"))
               ac-sources (append '(
                                    magik-ac-class-method-source
                                    magik-ac-dynamic-source
                                    magik-ac-global-source
                                    magik-ac-object-source
                                    magik-ac-raise-condition-source)
                                  (and (boundp 'ac-sources)
                                       ac-sources))

               outline-regexp "\\(^\\(_abstract +\\|\\)\\(_private +\\|\\)\\(_iter +\\|\\)_method.*\\|.*\.\\(def_property\\|add_child\\)\\|.*\.define_\\(shared_variable\\|shared_constant\\|slot_access\\|slot_externally_\\(read\\|writ\\)able\\|property\\|interface\\|method_signature\\).*\\|^\\(\t*#+\>[^>]\\|def_\\(slotted\\|indexed\\)_exemplar\\|def_mixin\\|#% text_encoding\\|_global\\|read_\\(message\\|translator\\)_patch\\).*\\)")

  (when magik-auto-abbrevs (abbrev-mode 1))

  (imenu-add-menubar-index))

;;;###autoload
(define-derived-mode magik-mode magik-base-mode "Magik"
  "Major mode for editing Magik code.

\\{magik-mode-map}"
  :group 'magik
  :abbrev-table nil
  :syntax-table nil

  (compat-call setq-local
               font-lock-defaults '((magik-font-lock-keywords
                                     magik-font-lock-keywords-1
                                     magik-font-lock-keywords-2
                                     magik-font-lock-keywords-3
                                     magik-font-lock-keywords-4
                                     magik-font-lock-keywords-5)
                                    nil t
                                    ((?_ . "w"))
                                    magik-goto-code
                                    (font-lock-fontify-buffer-function   . magik-font-lock-fontify-buffer)
                                    (font-lock-fontify-region-function   . magik-font-lock-fontify-region)
                                    (font-lock-unfontify-buffer-function . magik-font-lock-unfontify-buffer))
               indent-line-function 'magik-indent-line))

(defvar magik-menu nil
  "Keymap for the Magik buffer menu bar.")

(easy-menu-define magik-menu magik-base-mode-map
  "Menu for Magik Mode."
  `(,"Magik"
    [,"Transmit Method"   magik-transmit-method :active (magik-utils-buffer-mode-list 'magik-session-mode)
     :keys "<f7>,   <f2> <f7>,   <f2> m"]
    [,"Transmit Region"   magik-transmit-region :active (magik-utils-buffer-mode-list 'magik-session-mode)
     :keys "<f8>,   <f2> <f8>,   <f2> r"]
    [,"Transmit Buffer"   magik-transmit-buffer         (magik-utils-buffer-mode-list 'magik-session-mode)]
    [,"Transmit Chunk"    magik-transmit-$-chunk        (magik-utils-buffer-mode-list 'magik-session-mode)]
    [,"Transmit Thing"    magik-transmit-thing          (magik-utils-buffer-mode-list 'magik-session-mode)]
    "---"
    [,"Copy Region to Work Buffer"  magik-copy-region-to-buffer   t]
    [,"Copy Method to Work Buffer"  magik-copy-method-to-buffer   t]
    [,"Set Work Buffer Name"        magik-set-work-buffer-name    t]
    "---"
    [,"Electric Template" magik-explicit-electric-space           t]
    [,"Mark Method"       magik-mark-method               :active t :keys "C-M-h,   <f9>"]
    [,"Copy Method"       magik-copy-method               :active t :keys "<f4> c,   <f6>"]
    [,"Compare Method between Windows" magik-compare-methods      t]
    [,"Compare Method using Ediff"     magik-ediff-methods        t]
    "---"
    [,"Add Debug Statement"         magik-add-debug-statement     t]
    [,"Trace Statement"             magik-trace-curr-statement    t]
    [,"Symbol Complete"   magik-symbol-complete         (magik-utils-buffer-mode-list 'magik-session-mode)]
    "---"
    [,"Comment Region"           magik-comment-region          t]
    [,"Uncomment Region"         magik-uncomment-region        t]
    [,"Fill Comment"             magik-fill-public-comment     t]
    "---"
    [,"Check sw-method-docs for method"      magik-single-sw-method-docs t]
    [,"Check sw-method-docs for file"        magik-file-sw-method-docs   t]
    [,"Check pragma for method/def_slotted_exemplar" magik-single-pragma t]
    [,"Check pragma for file"                        magik-file-pragma   t]
    "---"
    (,"Toggle.."
     [,"Method Name Display"      magik-method-name-mode
      :active t
      :style toggle
      :selected magik-method-name-mode]
     [,"Electric Magik Mode"  magik-electric-mode
      :active t
      :style toggle
      :selected magik-electric-mode]
     [,"#DEBUG Statements"          magik-toggle-transmit-debug-p
      :active t
      :style toggle
      :selected magik-transmit-debug-p]
     [,"Point at End of Marked Region"  magik-mark-method-exchange-mode
      :active t
      :style toggle
      :selected magik-mark-method-exchange]
     ,"Options.."
     [,"Transmit Method = Move to End"    (magik-transmit-method-eom-mode 'end)
      :active t
      :style radio
      :selected (eq magik-transmit-method-eom-mode 'end)]
     [,"Transmit Method = Do Not Move Point"   (magik-transmit-method-eom-mode nil)
      :active t
      :style radio
      :selected (eq magik-transmit-method-eom-mode nil)]
     [,"Transmit Method = On Repeat, Move to End" (magik-transmit-method-eom-mode 'repeat)
      :active t
      :style radio
      :selected (eq magik-transmit-method-eom-mode 'repeat)
      ])
    [,"Customize"            magik-customize               t]))

(defvar magik-imenu-expression
  `(
    (nil
     "^\\s-*\\(_abstract\\(\n\\|\\s-\\)+\\)?\\(_private\\(\n\\|\\s-\\)+\\)?\\(_iter\\(\n\\|\\s-\\)+\\)?_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\.\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\)" magik-imenu-method-name 9)
    (,"Public Methods"
     "^\\s-*_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\.\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" magik-imenu-method-name 1)
    (,"Iterators"
     "^\\s-*\\(_abstract\\(\n\\|\\s-\\)+\\)?\\(_private\\(\n\\|\\s-\\)+\\)?_iter\\(\n\\|\\s-\\)+_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\.\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" magik-imenu-method-name 6)
    (,"Private"
     "^\\s-*_private\\(\n\\|\\s-\\)+\\(_iter\\(\n\\|\\s-\\)+\\)?_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\.\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" magik-imenu-method-name 4)
    (,"Abstract"
     "^\\s-*_abstract\\(\n\\|\\s-\\)+\\(_private\\(\n\\|\\s-\\)+\\)?\\(_iter\\(\n\\|\\s-\\)+\\)?_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\.\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" magik-imenu-method-name 6)
    (,"show/write/print/trace"
     "_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\.\\(show\\|write\\|print\\|debug_print\\|trace\\)\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" magik-imenu-method-name 1)
    (,"new/init"
     "_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\.\\(new\\|init\\)\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" magik-imenu-method-name 1)
    (,"Procedures"
     "\\b_\\sw+\\(\n\\|\\s-\\)+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\s-*<<\\(\n\\|\\s-\\)*_proc\\s-*(" 2) ;unnamed, use variable assignment
    (,"Procedures"
     "_proc\\s-*\\(@\\s-*\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\s-*(" magik-imenu-method-name 1) ;named using @
    (,"Condition"
     "^\\s-*condition.define_condition([ \t\n]*:\\s-*\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" 1)
    (,"Properties"
     "^\\s-*\\(.+\\)\\.def\\(\\|ine\\)_property([ \t\n]*:\\s-*\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" 3)
    (,"Shared Variables"
     "^\\s-*\\(.+\\)\\.define_shared_variable([ \t\n]*:\\s-*\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" 2)
    (,"Shared Constants"
     "^\\s-*\\(.+\\)\\.define_shared_constant([ \t\n]*:\\s-*\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" 2)
    (,"Slot Access"
     "^\\s-*\\(.+\\)\\.define_slot_\\(access\\|externally_readable\\|externally_writable\\)([ \t\n]*:\\s-*\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" 3)
    (,"Pseudo Slots"
     "^\\s-*\\(.+\\)\\.define_pseudo_slot([ \t\n]*:\\s-*\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" 2) ; define_slot_externally_* rarely used.
    (,"Mixins"
     "^\\s-*def_mixin([ \t\n]*:\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" 1)
    (,"Operators"
     "^\\s-*define_binary_operator_case([ \t\n]*:\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" 1)
    (,"Arrays"
     "^\\s-*_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\s-*?\\[" magik-imenu-method-name 1)
    (,"Exemplars"
     "^\\s-*def_\\(slott\\|index\\)ed_exemplar([ \t\n]*:\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" 2) ;; def_enumeration not used.
    (,"Globals"
     "^\\s-*_global\\(\n\\|\\s-\\)+\\(_constant\\(\n\\|\\s-\\)+\\)?\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" 4)
    (,"Packages"
     "^\\s-*_package[ \t\n]*\\(\\sw+\\)" 1))
  "Imenu expression for Magik mode.  See `magik-imenu-create-index-function'.")

;;; Font-lock configuration
;;
;; Do not use :inherit because we want Emacs 20 support.
;;
;; On Windows Emacsen 21 and earlier, there is a GDI Object based memory leak.
;; This has been tracked down to creating italic forms of the font.
;; However, bold italic forms do not appear affected,
;; so all color based italic fonts are also made bold too.
(defgroup magik-faces nil
  "Fontification colours for Magik."
  :group 'magik)

;; font-lock-variable-use-face was introduced in Emacs 29.1.
(unless (member `font-lock-variable-use-face (face-list))
  (put 'font-lock-variable-use-face 'face-alias 'font-lock-variable-name-face))

(defface magik-argument-face
  '((t (:inherit font-lock-variable-use-face)))
  "Font Lock mode face used to display arguments for methods."
  :group 'magik-faces)

(defface magik-boolean-face
  '((t (:inherit font-lock-variable-name-face)))
  "Font Lock mode face used to display boolean and kleenean references."
  :group 'magik-faces)

;; font-lock-punctuation-face was introduced in Emacs 29.1.
(unless (member `font-lock-punctuation-face (face-list))
  (put 'font-lock-punctuation-face 'face-alias nil))

;; font-lock-bracket-face was introduced in Emacs 29.1.
(unless (member `font-lock-bracket-face (face-list))
  (put 'font-lock-bracket-face 'face-alias 'font-lock-punctuation-face))

(defface magik-bracket-face
  '((t (:inherit font-lock-bracket-face)))
  "Font Lock mode face used to display brackets and parens."
  :group 'magik-faces)

(defface magik-character-face
  '((t (:inherit font-lock-constant-face)))
  "Font Lock mode face used to display characters."
  :group 'magik-faces)

(defface magik-class-face
  '((t (:inherit font-lock-type-face)))
  "Font Lock mode face used to display exemplars."
  :group 'magik-faces)

(defface magik-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Font Lock mode face used to display comments."
  :group 'magik-faces)

(defface magik-constant-face
  '((t (:inherit font-lock-constant-face)))
  "Font Lock mode face used to display constants."
  :group 'magik-faces)

;; font-lock-delimiter-face was introduced in Emacs 29.1.
(unless (member `font-lock-delimiter-face (face-list))
  (put 'font-lock-delimiter-face 'face-alias 'font-lock-punctuation-face))

(defface magik-delimiter-face
  '((t (:inherit font-lock-delimiter-face)))
  "Font Lock mode face used to display delimiters."
  :group 'magik-faces)

(defface magik-doc-face
  '((t (:inherit font-lock-doc-face)))
  "Font Lock mode face used to display documentation."
  :group 'magik-faces)

(defface magik-keyword-operators-face
  '((t (:inherit font-lock-keyword-face)))
  "Font Lock mode face used to display Magik operator keywords."
  :group 'magik-faces)

(defface magik-keyword-statements-face
  '((t (:inherit font-lock-keyword-face)))
  "Font Lock mode face used to display Magik statement keywords."
  :group 'magik-faces)

(defface magik-keyword-loop-face
  '((t (:inherit font-lock-keyword-face)))
  "Font Lock mode face used to display Magik loop keywords."
  :group 'magik-faces)

(defface magik-keyword-arguments-face
  '((t (:inherit font-lock-keyword-face)))
  "Font Lock mode face used to display Magik argument keywords."
  :group 'magik-faces)

(defface magik-dynamic-face
  '((t (:inherit font-lock-variable-name-face)))
  "Font Lock mode face used to display dynamic variables."
  :group 'magik-faces)

(defface magik-global-face
  '((t (:inherit font-lock-variable-name-face)))
  "Font Lock mode face used to display global variables."
  :group 'magik-faces)

(defface magik-global-reference-face
  '((t (:inherit font-lock-constant-face)))
  "Font Lock mode face used to display global references."
  :group 'magik-faces)

(defface magik-keyword-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Font Lock mode face used to display Magik variable keywords."
  :group 'magik-faces)

(defface magik-keyword-obsolete-face
  '((t (:inherit font-lock-warning-face)))
  "Font Lock mode face used to display obsolete Magik keywords."
  :group 'magik-faces)

(defface magik-method-face
  '((t (:inherit font-lock-function-name-face)))
  "Font Lock mode face used to display method names & method/procedure keywords."
  :group 'magik-faces)

(defface magik-label-face
  '((t (:inherit font-lock-variable-name-face)))
  "Font Lock mode face used to display labels for loops."
  :group 'magik-faces)

;; font-lock-number-face was introduced in Emacs 29.1 as new face without any inheritance.
(unless (member `font-lock-number-face (face-list))
  (put 'font-lock-number-face 'face-alias 'font-lock-constant-face))

(defface magik-number-face
  '((t (:inherit font-lock-number-face)))
  "Font Lock mode face used to display numbers."
  :group 'magik-faces)

;; font-lock-operator-face was introduced in Emacs 29.1.
(unless (member `font-lock-operator-face (face-list))
  (put 'font-lock-operator-face 'face-alias 'font-lock-punctuation-face))

(defface magik-operator-face
  '((t (:inherit font-lock-operator-face)))
  "Font Lock mode face used to display operators."
  :group 'magik-faces)

(defface magik-pragma-face
  '((t (:inherit font-lock-builtin-face)))
  "Font Lock mode face used to display pragma directives."
  :group 'magik-faces)

(defface magik-procedure-face
  '((t (:inherit font-lock-function-name-face)))
  "Font Lock mode face used to display procedure calls."
  :group 'magik-faces)

(defface magik-slot-face
  '((t (:inherit font-lock-variable-name-face)))
  "Font Lock mode face used to display slots."
  :group 'magik-faces)

(defface magik-symbol-face
  '((t (:inherit font-lock-constant-face)))
  "Font Lock mode face used to display symbols."
  :group 'magik-faces)

(defface magik-string-face
  '((t (:inherit font-lock-string-face)))
  "Font Lock mode face used to display strings."
  :group 'magik-faces)

(defface magik-variable-face
  '((t (:inherit font-lock-variable-name-face)))
  "Font Lock mode face used to display variables."
  :group 'magik-faces)

(defface magik-warning-face
  '((t (:inherit font-lock-warning-face)))
  "Font Lock mode face used to display warning statements."
  :group 'magik-faces)

(defface magik-write-face
  '((t (:inherit font-lock-warning-face)))
  "Font Lock mode face used to display write() statements."
  :group 'magik-faces)

(defconst magik-regexp
  '(("method" .
     "^[_abstract\s|_private\s|_iter\s]*?_method")
    ("method-with-arguments" .
     "^[_abstract\s|_private\s|_iter\s]*?_method.*(\\([\0-\377[:nonascii:]]*?\\))")
    ("assignment-method" .
     "^[_abstract\s|_private\s|_iter\s]*?_method.*<<\s?\\(.*\\)")
    ("endmethod" .
     "^\\s-*_endmethod\\s-*\\(\n\\$\\s-*\\)?$")
    ("method-argument" .
     "_gather\\|_scatter\\|_optional")
    ("pragma" .
     "^_pragma(.*)")
    ("def_slotted_exemplar" .
     "^[sw:]?def_slotted_exemplar(.*"))
  "List of regexp strings which can be used for searching.
In a buffer searching for a Magik-specific.")

(defvar magik-keyword-kleenean
  '("false" "true" "maybe")
  "List of keywords relating to kleenean values to highlight for font-lock.")

(defvar magik-keyword-constants
  '("unset" "constant")
  "List of keywords relating to constant values to highlight for font-lock.
The \"no_way\" constant is treated as a special case in this Magik mode
because it does not have an _ preceding like all the other Magik keywords.")

(defvar magik-keyword-operators
  '("and" "andif" "div" "is" "isnt" "cf" "mod" "not" "or" "orif" "xor")
  "List of keywords relating to operators to highlight for font-lock.")

(defvar magik-keyword-class
  '("self" "super" "clone")
  "List of keywords relating to exemplars to highlight for font-lock.")

(defvar magik-keyword-methods
  '("abstract" "private" "method" "endmethod" "primitive")
  "List of keywords relating to methods to highlight for font-lock.")

(defvar magik-keyword-procedures
  '("proc" "endproc")
  "List of keywords relating to procedures to highlight for font-lock.")

(defvar magik-keyword-statements
  '("block" "endblock" "catch" "throw" "endcatch"
    "if" "then" "elif" "else" "endif"
    "lock" "endlock" "protect" "locking" "protection" "endprotect"
    "try" "endtry" "when" "handling" "with" "using"
    "pragma" "package" "default" "thisthread")
  "List of keywords relating to statements to highlight for font-lock.")

(defvar magik-keyword-loop
  '("iter" "continue" "for" "loop" "endloop" "loopbody" "over" "leave" "finally" "while")
  "List of keywords relating to loops to highlight for font-lock.")

(defvar magik-keyword-arguments
  '("gather" "scatter" "allresults" "optional" "return")
  "List of keywords relating to arguments to highlight for font-lock.")

(defvar magik-keyword-variable
  '("dynamic" "global" "import" "local" "class" "recursive")
  "List of keywords relating to variables to highlight for font-lock.")

(defvar magik-keyword-obsolete
  '("concat" "case" "endcase" "otherwise" "void")
  "List of obsolete/unimplemented keywords to highlight for font-lock.")

(defvar magik-other-keywords '("def_indexed_exemplar" "def_slotted_exemplar")
  "List of other Magik `keywords'.")

(defvar magik-warnings
  '("TODO" "DEBUG" "FIXME" "sys!slot" "sys!perform" "sys!perform_iter")
  "List of Magik Warnings.")

(defcustom magik-font-lock-keywords-1
  (list
   (cons (concat "\\<_" (regexp-opt magik-keyword-kleenean  t) "\\>") ''magik-boolean-face)
   (cons (concat "\\<no_way\\|_" (regexp-opt magik-keyword-constants t) "\\>") ''magik-constant-face)
   (cons (concat "\\<_"
                 (regexp-opt (append magik-keyword-operators
                                     magik-keyword-class
                                     magik-keyword-methods
                                     magik-keyword-procedures
                                     magik-keyword-statements
                                     magik-keyword-loop
                                     magik-keyword-arguments
                                     magik-keyword-variable)
                             t)
                 "\\>")
         'font-lock-keyword-face)
   (cons (concat "\\<\\(" (mapconcat 'identity magik-other-keywords "\\|") "\\)\\>")
         'font-lock-keyword-face))
  "Font lock setting for 1st level of Magik fontification.
Fontifies all Magik keywords in the same face except Magik
constants which use the `font-lock-constant-face' face."
  :group 'magik
  :type 'sexp)

(defcustom magik-font-lock-keywords-2
  (list
   '("\\b_method\\s-*\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\.\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)"
     (1 'magik-class-face)
     (3 'magik-method-face))
   '("\\<!\\sw+\\!\\>" .  'magik-dynamic-face)
   '("\\<:\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?" . 'magik-symbol-face)
   '("\\<\\(write\\|print\\|debug_print\\)\\s-*(" 1 'magik-write-face)
   (list (concat "\\<\\("
                 (mapconcat 'identity magik-warnings "\\|")
                 "\\)")
         0 ''magik-warning-face t)
   '("^\\s-*##.*$" 0 'magik-doc-face t))
  "Font lock setting for 2nd level of Magik fontification.
Fontifies certain Magik language features like symbols, dynamics but does
NOT fontify ANY Magik Keywords."
  :group 'magik
  :type 'sexp)

(defcustom magik-font-lock-keywords-3
  (append magik-font-lock-keywords-1 magik-font-lock-keywords-2 nil)
  "Combines 1st and 2nd level fontification levels.
See `magik-font-lock-keywords-1' and `magik-font-lock-keywords-2'."
  :group 'magik
  :type 'sexp)

(defcustom magik-font-lock-keywords-4
  (append
   magik-font-lock-keywords-3
   (list
    (cons (concat "\\<_" (regexp-opt magik-keyword-operators  t) "\\>") ''magik-keyword-operators-face)
    (cons (concat "\\<_" (regexp-opt magik-keyword-class      t) "\\>") ''magik-class-face)
    (cons (concat "\\<_" (regexp-opt magik-keyword-methods    t) "\\>") ''magik-method-face)
    (cons (concat "\\<_" (regexp-opt magik-keyword-procedures t) "\\>") ''magik-procedure-face)
    (cons (concat "\\<_" (regexp-opt magik-keyword-statements t) "\\>") ''magik-keyword-statements-face)
    (cons (concat "\\<_" (regexp-opt magik-keyword-loop       t) "\\>") ''magik-keyword-loop-face)
    (cons (concat "\\<_" (regexp-opt magik-keyword-arguments  t) "\\>") ''magik-keyword-arguments-face)
    (cons (concat "\\<_" (regexp-opt magik-keyword-variable   t) "\\>") ''magik-keyword-variable-face)
    (cons (concat "\\<_" (regexp-opt magik-keyword-obsolete   t) "\\>") ''magik-keyword-obsolete-face)

    '("^_pragma\\s-*\\(([^)]*)\\)" 1 'magik-pragma-face)
    ;; methods
    '("\\(\\sw\\|\\s$\\)\\.\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\(\\s-*(\\)" 2 'magik-method-face)
    ;; procedures
    '("\\<\\(\\sw+\\)\\(\\s-*(\\)" 1 'magik-procedure-face)
    '("^\\(def_slotted_exemplar\\|def_indexed_exemplar\\)\\>" 0 'magik-class-face t)
    '("^\\(\\sw+\\)\\.define_\\(shared_constant\\|shared_variable\\|slot_access\\)\\>" 1 'magik-class-face)
    '("\\Sw\\(\\.\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\>" 1 'magik-slot-face)
    '("\\<\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\?\\>" 0 'magik-boolean-face t)
    '("_for\\s-+\\(\\sw+\\)" 1 'magik-variable-face) ;; _for loop variable
    '("@\\s-*\\sw+" 0 'magik-global-reference-face t)
    '(">>" 0 'magik-keyword-arguments-face t)))
  "Font lock setting for 4th level of Magik fontification.
As 1st level but also fontifies all Magik keywords according their
different classifications, e.g. loop keywords are fontified in the same face."
  :group 'magik
  :type 'sexp)

(defcustom magik-font-lock-keywords-5
  (append magik-font-lock-keywords-4 nil)
  "Provides an easy user configurable level for personal fontification of styles.
Based from `magik-font-lock-keywords-4'."
  :group 'magik
  :type 'sexp)

(defcustom magik-font-lock-keywords magik-font-lock-keywords-1
  "Default fontification of Magik."
  :group 'magik
  :type 'sexp)

;;Could have coded the following to be generic but this is more maintainable.
;;Have added support for 3d arrays although there are none in Core at present.
;;Also used for returning regexps for normal methods and ()<< and ()^<< methods too
(defvar magik-goto-class-method-alist
  '(("[]"      . "\\[[^\],]+\\]\\s-*$")
    ("[]<<"    . "\\[[^\],]+\\]\\s-*<<")
    ("[]^<<"   . "\\[[^\],]+\\]\\s-*\\^<<")
    ("[,]"     . "\\[[^\],]+,[^\]]+\\]\\s-*$")
    ("[,]<<"   . "\\[[^\],]+,[^\]]+\\]\\s-*<<")
    ("[,]^<<"  . "\\[[^\],]+,[^\]]+\\]\\s-*\\^<<")
    ("[,,]"    . "\\[[^\],]+,[^\],]+,[^\]]+\\]\\s-*$")
    ("[,,]<<"  . "\\[[^\],]+,[^\],]+,[^\]]+\\]\\s-*<<")
    ("[,,]^<<" . "\\[[^\],]+,[^\],]+,[^\]]+\\]\\s-*\\^<<")
    ("<<"      . "<<")
    ("^<<"     . "^<<")
    ("()"      . "(")
    ("()<<"    . "([^\)]*)\\s-*<<")
    ("()^<<"   . "([^\)]*)\\s-*^<<")
    (""        . "\r?\n"))
  "Alist to help searching for method types.")

(defvar magik-transmit-method-eom-alist
  (list (cons "Transmit Method = Do Not Move Point"    nil)
        (cons "Transmit Method = Move to End"     'end)
        (cons "Transmit Method = On Repeat, Move to End" 'repeat))
  "Alist of options for variable `magik-transmit-method-eom-mode'.")

(defcustom magik-transmit-method-eom-mode nil
  "Variable storing setting of \\[magik-transmit-method-eom-mode]."
  :group  'magik
  :type  (list 'choice
               (list 'const ':tag "Transmit Method = Do Not Move Point" nil)
               (list 'const ':tag "Transmit Method = Move to End" 'end)
               (list 'const ':tag "Transmit Method = On Repeat, Move to End" 'repeat)))

(defcustom magik-method-name-mode nil
  "Variable storing setting of \\[magik-method-name-mode].
This variable exists to allow a user to set the mode before the Smallworld
code is loaded."
  ;;Use of integers is a standard way of forcing minor modes on and off.
  :group 'magik
  :type '(choice (const :tag "On" 1)
                 (const :tag "Off" -1)))

(defvar-local magik-method-name ""
  "Variable storing method name at which point it in.
Used by \\[magik-method-name-mode].")

(defvar-local magik-transmit-debug-mode-line-string nil
  "Mode-line string to use when transmitting of #DEBUG statements is enabled.")

(defvar magik-method-name-set-text-function 'magik-method-name-set-text-properties
  "Function to use for setting the Mode line to include Method name.
Function takes two arguments BUFFER and METHOD.")

;; consider enabling refresh using auto-complete's 10 minute refresh idle timer?
(defvar magik-ac-object-source-cache nil
  "Cache of all Magik Objects for use in auto-complete-mode.
Once initialised this variable is not refreshed.")

(defvar magik-ac-object-source
  '((init       . magik-ac-object-source-init)
    (candidates . magik-ac-object-source-cache)
    (prefix     . magik-object)
    (requires   . 3)
    (symbol     . "o"))
  "Auto-complete mode source definition for listing all Magik Objects.
Use auto-complete mode \"o\" symbol convention to represent an object.")

(defvar magik-ac-class-method-source-cache nil
  "Cache of all Magik methods on current class for use in auto-complete-mode.")

(defvar magik-ac-class-method-source
  '((init       . magik-ac-object-source-init)
    (candidates . magik-ac-class-method-source)
    (prefix     . magik-method)
    (symbol     . "f"))
  "Auto-complete mode source definition for listing methods on a given class.
Use auto-complete mode \"f\" symbol convention to represent a function, method.")

(defvar magik-ac-raise-condition-source-cache nil
  "Cache of all Magik Conditions for use in auto-complete-mode.")

(defvar magik-ac-raise-condition-source
  '((init       . magik-ac-raise-condition-source-init)
    (candidates . magik-ac-raise-condition-source-cache)
    (prefix     . magik-condition)
    (symbol     . "c"))
  "Auto-complete mode source definition for listing known conditions.
Uses auto-complete \"c\" symbol convention to represent a condition!")

(defvar magik-ac-global-source-cache nil
  "Cache of all Magik Globals for use in auto-complete-mode.
Once initialised this variable is not refreshed.")

(defvar magik-ac-dynamic-source
  '((init       . magik-ac-global-source-init)
    (candidates . magik-ac-global-source-cache)
    (prefix     . magik-dynamic)
    (symbol     . "d"))
  "Auto-complete mode source definition for listing Magik language dynamics.
Use auto-complete mode \"d\" symbol convention to represent.")

(defvar magik-ac-global-source
  '((init       . magik-ac-global-source-init)
    (candidates . magik-ac-global-source-cache)
                                        ;(requires   . 3)
    (symbol     . "g"))
  "Auto-complete mode source definition for listing all Magik Globals.
Use auto-complete mode \"g\" symbol convention to represent a global.")

(defun magik-customize ()
  "Open Customization buffer for Magik Mode."
  (interactive)
  (customize-group 'magik))

;;; Functions

(defun magik-expand-abbrev ()
  (save-excursion
    (let*
        ((toks (progn (insert ? )  ;; so that the token closes!
                      (prog1
                          (magik-tokenise-region-no-eol (line-beginning-position) (point))
                        (delete-char -1))))
         (last-tok (car (last toks)))
         (last-tok-pos (cdr last-tok)))
      (backward-word 1)
      (if (and (eq (point) last-tok-pos)
               (/= (preceding-char) ?.))
          (insert ?_))
      (if (and (derived-mode-p 'magik-base-mode)
               (looking-at "_else\\|_elif\\|_finally\\|_using\\|_with\\|_when\\|_protection\\|_end"))
          (magik-indent-command)))))

;;Actually only used by the Magik-Patch minor mode but we need a hook here
;;because a function must be referred to in font-lock-defaults.
(defvar magik-goto-code-function 'point-min
  "Function used to place point on the line immediately preceding Magik code.")

(defun magik-goto-code ()
  "Goto start of code."
  (funcall magik-goto-code-function))

(defun magik-font-lock-fontify-buffer ()
  (let ((verbose (if (numberp font-lock-verbose)
                     (> (buffer-size) font-lock-verbose)
                   font-lock-verbose))
        (code-start (save-excursion (magik-goto-code))))
    (with-temp-message
        (when verbose
          (format "Fontifying %s..." (buffer-name)))
      ;; Make sure we have the right `font-lock-keywords' etc.
      (unless font-lock-mode
        (font-lock-set-defaults))
      ;; Make sure we fontify etc. in the whole buffer.
      (save-restriction
        (widen)
        (condition-case nil
            (save-excursion
              (save-match-data
                (font-lock-fontify-region code-start (point-max) verbose)
                (font-lock-after-fontify-buffer)
                (setq font-lock-fontified t)))
          ;; We don't restore the old fontification, so it's best to unfontify.
          (quit (font-lock-unfontify-buffer))))
      ;; Make sure we undo `font-lock-keywords' etc.
      (unless font-lock-mode
        (font-lock-unset-defaults)))))

(defun magik-font-lock-unfontify-buffer ()
  "Make sure we unfontify etc.  in the whole buffer."
  (save-restriction
    (widen)
    (font-lock-unfontify-region (save-excursion (magik-goto-code)) (point-max))
    (font-lock-after-unfontify-buffer)
    (setq font-lock-fontified nil)))

(defun magik-font-lock-fontify-region (beg end loudly)
  (let*
      ((modified (buffer-modified-p))
       (buffer-undo-list t)
       (inhibit-read-only t)
       (inhibit-point-motion-hooks t)
       (inhibit-modification-hooks t)
       deactivate-mark buffer-file-name buffer-file-truename
       (old-syntax-table (syntax-table))
       (code-start (save-excursion (magik-goto-code))))
    (unwind-protect
        (save-restriction
          (widen)
          ;; Use the fontification syntax table, if any.
          (when font-lock-syntax-table
            (set-syntax-table font-lock-syntax-table))
          ;; check to see if we should expand the beg/end area for
          ;; proper multiline matches
          (when (and (boundp 'font-lock-multiline)
                     font-lock-multiline
                     (> beg code-start)
                     (get-text-property (1- beg) 'font-lock-multiline))
            ;; We are just after or in a multiline match.
            (setq beg (or (previous-single-property-change
                           beg 'font-lock-multiline)
                          code-start))
            (goto-char beg)
            (setq beg (line-beginning-position)))
          (when (and (boundp 'font-lock-multiline) font-lock-multiline)
            (setq end (or (text-property-any end (point-max)
                                             'font-lock-multiline nil)
                          (point-max))))
          (goto-char end)
          (setq end (line-beginning-position 2))
          (if (and (>= end code-start) (< beg code-start))
              (setq beg code-start))
          (when (and (>= beg code-start)
                     (>= end code-start))
            ;; Now do the fontification.
            (font-lock-unfontify-region beg end)
            (unless font-lock-keywords-only
              (font-lock-fontify-syntactically-region beg end loudly))
            (font-lock-fontify-keywords-region beg end loudly)))
      ;; Clean up.
      (set-syntax-table old-syntax-table))
    (if (and (not modified) (buffer-modified-p))
        (set-buffer-modified-p nil))))

(defun magik-toggle-transmit-debug-p (&optional arg)
  "Toggle transmission of #DEBUG statements in Magik code.
Optional argument ARG .."
  (interactive "P")
  (setq magik-transmit-debug-p
        (if (null arg)
            (not magik-transmit-debug-p)
          (> (prefix-numeric-value arg) 0)))
  (message
   (if magik-transmit-debug-p
       "Magik DEBUG statements on"
     "Magik DEBUG statements off")))

(defun magik-add-debug-statement ()
  "Add a debug statement at the current line of magik."
  (interactive)
  (let
      ((var (magik-utils-find-tag-default))
       (pos (point))
       line
       col
       tb)
    (save-excursion
      (or var (error "No current variable to print"))
      (magik-backward-method)
      (push-mark nil t)
      (while
          (not (or (eobp)
                   (looking-at "^\\s-*\\(_abstract\\(\n\\|\\s-\\)+\\)?\\(_private\\(\n\\|\\s-\\)+\\)?\\(_iter\\(\n\\|\\s-\\)+\\)?_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\.\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)")))
        (forward-line 1))
      (setq tb (match-string-no-properties 6))
      (setq line (count-lines (point) pos)))
    (back-to-indentation)
    (setq col (current-column))
    (beginning-of-line)
    (indent-to col)
    (insert (format "#DEBUG show(\"%s:Line %d\",:%s,%s)\n" tb line var var))))

;; bound to CR in magik mode.
(defun magik-newline ()
  "Insert a newline and indent.  (To insert a newline and not indent, use \\[electric-newline-and-maybe-indent])."
  (interactive "*")
  (when (derived-mode-p 'magik-session-mode)
    (error "Your Magik shell buffer has got into magik-mode! To recover, type `M-x magik-session-mode'.  Please report this bug"))
  (if abbrev-mode (save-excursion (expand-abbrev)))
  (if (save-excursion
        (back-to-indentation)
        (looking-at "[]})]\\|_else\\|_finally\\|_using\\|_with\\|_when\\|_protection\\|_end"))
      (magik-indent-command))
  (newline-and-indent))

(defun magik-indent-line ()
  "Indent the current line as Magik code."
  (let ((indent (magik-calc-indent))
        (pos (- (point-max) (point)))
        beg change)
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (setq change (- indent (current-column)))
    (if (zerop change)
        nil
      (delete-region beg (point))
      (indent-to indent))
    ;; if the initial point was within the inden, leave point at the indent,
    ;; otherwise back to where we where
    (if (> (- (point-max) pos) (point))
        (goto-char (- (point-max) pos)))))

;; bound to TAB in magik mode.
(defun magik-indent-command ()
  "Indent the current line as Magik code, or hop between pragma fields."
  (interactive "*")
  (let ((magik-pragma-brackets (magik-pragma-line-p)))
    (if (consp magik-pragma-brackets)
        (magik-electric-pragma-tab magik-pragma-brackets)
      (magik-indent-line))))

(defun magik-method-name-type (name)
  "Return cons cell describing the method name parts (NAME . TYPE).
For normal methods:
  NAME is the method name root
  TYPE is ^<< or << or () or ()<< or ()^<< or empty string.

For array methods
  NAME is nil
  Type is [] or []<< or []^<< or [,] or [,]<< or [,]^<< etc."
  (if (eq (elt name 0) ?\[)
      (cons nil name)
    (save-match-data
      (if (string-match "\\(()^<<\\|()<<\\|^<<\\|()\\|<<\\)$" name)
          (cons (substring name 0 (match-beginning 1)) (match-string 1 name))
        (cons name "")))))

(defun magik-goto-class-method (method &optional class)
  "Find the specified METHOD and CLASS in the current buffer.
If more than one definition is found in the buffer, you will be
given the opportunity to visit each definition.
Also the search string is added to isearch mode's regexp ring so that
you can use \\[isearch-forward-regexp] and use \\[isearch-ring-retreat] to recall the search."
  (interactive
   (list
    (read-string "Method Name: " (current-word))
    (if current-prefix-arg
        (read-string "Class Name: "
                     (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))))

  (if class nil
    (setq class (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))))
  (let* ((method-cons (magik-method-name-type method))
         (method-root (car method-cons))
         (method-type (cdr method-cons))
         search-str)
    (cond
     ((string-equal class "<global>")
      ;; look for _global definitions
      (if (re-search-forward (setq search-str (concat (regexp-quote method) "\\s-*<<")) nil t)
          (magik-goto-class-method-loop search-str method)
        (error "Can't find method '%s'" method)))
     ((string-equal class "<condition>")
      ;; look for condition.define_condition
      (if (re-search-forward (setq search-str (concat "condition.define_condition(\\(\n\\|\\s-\\)*:" (regexp-quote method) "\\(\n\\|\\s-\\)*,")) nil t)
          (magik-goto-class-method-loop search-str method)
        (error "Can't find method '%s'" method)))
     ((null method-root);(eq (elt method 0) ?\[)
      ;; look for array definitions: [] []<< []^<< [,] [,]<< [,]^<<
      (or (setq search-str (cdr (assoc method-type magik-goto-class-method-alist)))
          (error "Can't find method '%s'" method))
      (if (re-search-forward (setq search-str (concat class search-str)) nil t)
          (magik-goto-class-method-loop search-str method)
        (error "Can't find method '%s'" method)))
     ((re-search-forward
       ;; look for an ordinary _method constructs.
       (setq search-str
             (concat
              "^"
              "\\(_abstract\\s-+\\)?"
              "\\(_private\\s-+\\)?"
              "\\(_iter\\s-+\\)?"
              "_method\\s-+"
              class
              "\\."
              (regexp-quote method-root) "\\s-*" (or (cdr (assoc method-type magik-goto-class-method-alist)) method-type)))
       nil t)
      (magik-goto-class-method-loop search-str (concat class "." method)))
     ;; look for other method constructors.
     ((re-search-forward
       (setq search-str (concat "^" class
                                ".\\(define_\\(shared_constant\\|shared_variable\\|slot_access\\|property\\)\\|def_property\\)"
                                "\\s-*([ \t\r\n]*:" (regexp-quote method-root)))
       nil t)
      (magik-goto-class-method-loop search-str (concat class "." method)))
     (t
      (error "Can't find method, '%s', in class, '%s'" method class)))))

(defun magik-goto-class-method-loop (search-str arg)
  "Loop over subsequent definitions of ARG.
Add SEARCH-STR to `regexp-search-ring'.
After quitting this loop, you can use \\[isearch-forward-regexp] and use \\[isearch-ring-retreat] to recall this search."
  ;;I would like to use the isearch functionality but I can't work out
  ;;how to control isearch programmatically.
  (let ((continue-p t)
        (pt (point))
        (prompt       (concat (format "Warning: Goto next definition of '%s'" arg) " "))
        (start-prompt (concat (format "Warning: Goto first definition of '%s'" arg) " "))
        next)
    (save-excursion
      (setq next (re-search-forward search-str nil t)))
    (if (null next)
        nil ;only single definition found exit here.
      (beep)
      (isearch-update-ring search-str t)
      (while continue-p
        (cond ((null next)
               ;;No additional definitions found
               (if (not (y-or-n-p start-prompt))
                   (setq continue-p nil)
                 (beep)
                 (goto-char pt)
                 (setq continue-p t)))
              ((y-or-n-p prompt)
               (goto-char next))
              (t
               (setq continue-p nil)))
        (save-excursion
          (setq next (re-search-forward search-str nil t))))
      (where-is 'isearch-forward-regexp))))

(defun magik-safe-backward-method ()
  "Put point at beginning of previous method without errors."
  (interactive)
  (magik-backward-method t))

(defun magik-backward-method (&optional noerror)
  "Put point at beginning of this method.
Optional argument NOERROR ..."
  (interactive)

  (when (re-search-backward "^\\s-*\\(_abstract\\(\n\\|\\s-\\)+\\)?\\(_private\\(\n\\|\\s-\\)+\\)?\\(_iter\\(\n\\|\\s-\\)+\\)?_method\\s-+" nil noerror)
    (while
        (and (not (bobp))
             (progn
               (forward-line -1)
               (looking-at "^\\(\\s-*\\(_abstract\\|_private\\|_iter\\|_pragma\\|#\\)\\)\\|$"))))
    (while (not (looking-at "^\\s-*\\(_method\\|_iter\\|_private\\|_abstract\\|_pragma\\)"))
      (forward-line 1))
    t))

(defun magik-safe-forward-method ()
  "Put point at beginning of next method without errors."
  (interactive)
  (magik-forward-method t))

(defun magik-forward-method (&optional noerror)
  "Put point at beginning of the next method.
Optional argument NOERROR ..."
  (interactive)
  (save-match-data
    (and (not (eobp))
         (save-excursion
           (beginning-of-line)
           (looking-at "^\\(\\s-*\\(_abstract\\|_private\\|_iter\\|_method\\|_pragma\\|#\\)\\)\\|$"))
         (magik-forward-endmethod noerror))

    (when (re-search-forward "^\\s-*\\(_abstract\\(\n\\|\\s-\\)+\\)?\\(_private\\(\n\\|\\s-\\)+\\)?\\(_iter\\(\n\\|\\s-\\)+\\)?_method\\s-+" nil noerror)
      (forward-line 1)
      (while (and (not (bobp))
                  (progn
                    (forward-line -1)
                    (looking-at "^\\(\\s-*\\(_abstract\\|_private\\|_iter\\|_method\\|_pragma\\|#\\)\\)\\|$"))))
      (while (not (looking-at "^\\s-*\\(_method\\|_iter\\|_private\\|_abstract\\|_pragma\\)"))
        (forward-line 1)))))

(defun magik-forward-endmethod (&optional noerror)
  "Put point at beginning of line after next _endmethod (including $).
Optional argument NOERROR ..."
  (interactive)
  (save-match-data
    (when (re-search-forward (cdr (assoc "endmethod" magik-regexp)) nil noerror)
      (forward-line 1)
      t)))

(defun magik-mark-method (&optional nomsg)
  "Mark the current method.
Repeated commands extends the marked region to include the next method as well.
Optional argument NOMSG ..."
  (interactive)
  (if (eq last-command 'magik-mark-method)
      (let (pt)
        (or magik-mark-method-exchange (exchange-point-and-mark))
        (setq pt (mark))
        (magik-forward-endmethod t)
        (set-mark (point))
        (goto-char pt))
    (magik-forward-endmethod t)
    (push-mark (point) nomsg t)
    (magik-backward-method t))
  (if magik-mark-method-exchange (exchange-point-and-mark))
  (mark))

(defun magik-copy-method ()
  "Copy the current method into `kill-ring'.
If the last command was \\[magik-mark-method] then that region will be copied instead."
  (interactive)
  (if (and (eq last-command 'magik-mark-method) mark-active)
      (kill-new (buffer-substring (point) (mark)))
    (save-excursion
      (save-match-data
        (magik-mark-method)
        (kill-new (buffer-substring (point) (mark)))))))

(defun magik-function-convert (x)
  "Convert Lisp object X type into an equivalent Magik object type."
  (cond ((eq x 'unset)
         "_unset")
        ((or (eq x 'false) (eq x nil))
         "_false")
        ((or (eq x 'true) (eq x t))
         "_true")
        ((stringp x)
         (concat "\"" x "\""))
        ((numberp x)
         (number-to-string x))
        ((symbolp x)
         (concat ":" (symbol-name x)))
        (t
         x)))

(defun magik-function (cmd &rest args)
  "Generate Magik code from the supplied arguments.
e.g. (magik-function \"system.test\" \"file\" \\='unset 4) returns the string
     system.test(\\\"file\\\", _unset, 4)
Argument CMD ...
Optional argument ARGS ..."

                                        ;process arg types: nil, string, other...
  (setq args (mapcar 'magik-function-convert args))
                                        ;bring the command together adding commas between the arguments
  (concat cmd "(" (mapconcat 'identity args ", ") ")\n"))

(defun magik-gis-error-goto (&optional gis)
  "Goto the previous magik error.
Optional argument GIS ..."
  (interactive)
  (let ((gis (magik-utils-get-buffer-mode gis
                                          'magik-session-mode
                                          "Enter Magik Session buffer:"
                                          magik-session-buffer
                                          'magik-session-buffer-alist-prefix-function))
        pt)
    (with-current-buffer gis
      (goto-char (point-max))
      (forward-line -1)
      (cond ((equal (current-word) "True")
             nil) ;;Code loading successful.
            ((re-search-backward (concat "^\\*\\*\\*\\*.*" "on line" " \\([0-9]+\\)$")
                                 (save-excursion (re-search-backward magik-session-prompt nil t)) t)
             (setq pt (point)))
            (t ;no "on line" errors found.
             nil)))

    (if pt
        (progn
          (pop-to-buffer gis)
          (goto-char pt)
          (magik-gis-error-goto)))))

(defun magik-perform-replace-no-set-mark (from to regexp-flag)
  "Like `perform-replace' but without setting the mark.
Also without `query' or `delimited' flags."
  (let ((literal (not regexp-flag))
        (search-function (if regexp-flag 're-search-forward 'search-forward)))
    (while (and (not (eobp))
                (funcall search-function from nil t))
      (replace-match to t literal))))

(defun magik-transmit-method-eom-mode (arg)
  "Toggle whether to move the cursor to the end of the method after transmitting.
If nil, leave point where it is,
If t or \\='end, move point to end of method,
If \\='repeat, move point to end of method on 2nd and later uses of the command.
Argument ARG ..."
  (interactive
   (list
    (cdr (assoc (completing-read (concat "Transmit Method EOM Mode:" " ")
                                 magik-transmit-method-eom-alist
                                 nil t)
                magik-transmit-method-eom-alist))))
  (setq magik-transmit-method-eom-mode arg)

  (message
   (cond ((null magik-transmit-method-eom-mode)
          "After transmit method the cursor position will be unaffected")
         ((eq magik-transmit-method-eom-mode 'end)
          "After transmit method cursor will move to end of method")
         ((eq magik-transmit-method-eom-mode 'repeat)
          "After transmit method, cursor will move to end of method when command is repeated."))))

(defun magik-mark-method-exchange-mode (&optional arg)
  "Toggle whether the cursor is placed at the begin or end of the marked region.
See `magik-mark-method-exchange' for more details."
  (interactive "P")
  (setq magik-mark-method-exchange
        (if (null arg)
            (not magik-mark-method-exchange)
          (> (prefix-numeric-value arg) 0)))
  (message
   (if magik-mark-method-exchange
       "Cursor will be placed at end of marked region."
     "Cursor will be placed at start of marked region.")))

(defun magik-transmit-buffer ()
  "Send the buffer to the process running in the buffer in `magik-session-buffer'."
  (interactive)
  (magik-transmit-region (point-min) (point-max))
  (message "Code loaded from %s" (or (buffer-file-name) (buffer-name))))
(defalias 'transmit-buffer-to-magik 'magik-transmit-buffer)

(defun magik-transmit-thing ()
  "Transmit the top-level Magik programming construct surrounding point.
The construct can be a method, a proc, a def_slotted_exemplar or whatever.
The rule is that the thing must start against the left margin."
  (interactive)
  (let
      ((original-point (point))
       (beg (point))
       (stack nil))
    (if (re-search-backward "^\\w" nil t)
        (progn
          (setq beg (point))
          (forward-line -1)
          (while
              (and (not (bobp))
                   (looking-at "[ \t]*#\\|_pragma\\|_private\\|_iter\\|_if\\|_over\\|_for\\|[ \t]*usage"))
            (setq beg (point))
            (forward-line -1))
          (goto-char beg)
          (while
              (and (not (eq (point) (point-max)))
                   (or (< (point) original-point)
                       stack))
            (dolist (tok (magik-tokenise-line))
              (cond
               ((assoc (car tok) magik-begins-and-ends)
                (push (car tok) stack))
               ((assoc (car tok) magik-ends-and-begins)
                (if (equal (cdr (assoc (car stack) magik-begins-and-ends)) (car tok))
                    (pop stack)
                  (goto-char (cdr tok))
                  (error "Found '%s' when expecting '%s'"
                         (car tok)
                         (cdr (assoc (car stack) magik-begins-and-ends)))))))
            (forward-line))
          (if (< (point) original-point)
              (progn
                (goto-char original-point)
                (error "Don't know what to transmit"))
            (magik-transmit-region beg (point)))))
    (goto-char original-point)))
(defalias 'transmit-thing-to-magik 'magik-transmit-thing)

(defun magik-transmit-$-chunk ()
  "Send the current $ chunk to magik."
  ;;Would like to use prefix key for number of statements but that classhes
  ;;with use of prefix to identify GIS session to send to.
  (interactive)
  (save-excursion
    (save-match-data
      (let ((pt (point))
            (end (or (re-search-forward "^\\$" nil t)
                     (goto-char (point-max)))))
        (goto-char pt)
        (magik-transmit-region
         (or (re-search-backward "^\\$" nil t)
             (goto-char (point-min)))
         end)))))

(defun magik-transmit-method ()
  "Send the current method to magik.
The location of point is determined by
variable `magik-transmit-method-eom-mode'.
If \\='repeat, then repeated calls to this function behaves like this:
  First use of command will leave point where it is,
  Repeat use will move point to the end of last transmitted method.
If \\='end, then point is left at the end of the method.
Otherwise, point is left where it is."
  (interactive)
  ;;DEBUG (message "this %s, last %s" this-command last-command)
  (if (eq last-command 'magik-transmit-method-first) (magik-forward-endmethod))
  (let ((magik-mark-method-exchange nil)
        mark)
    (save-excursion
      (setq mark (magik-mark-method t))
      (magik-transmit-region (point) mark))
    (cond ((eq magik-transmit-method-eom-mode 'end)
           (goto-char mark))
          ((eq magik-transmit-method-eom-mode 'repeat)
           ;;Use this-command and last-command to track repetition.
           (cond ((eq last-command 'magik-transmit-method-nth)
                  (goto-char mark)
                  (setq this-command 'magik-transmit-method-nth))
                 ((eq last-command 'magik-transmit-method-first)
                  (goto-char mark)
                  (setq this-command 'magik-transmit-method-nth))
                 ((eq this-command 'magik-transmit-method)
                  (setq this-command 'magik-transmit-method-first))))
          (t nil))
    mark))

(defalias 'transmit-method-to-magik 'magik-transmit-method)

(defun magik-transmit-region (beg end)
  "Send region from BEG to END via a temp file to Magik in a shell.
Uses load_file to send the temp file.
If this command is repeated before the previous file has been processed by
Magik, another file shall be written."
  (interactive "r")
  (magik-transmit-string
   (buffer-substring-no-properties beg end)
   (save-excursion
     (goto-char beg)
     (beginning-of-line)
     (magik-package-line))
   (lambda (f) (magik-function "load_file" f 'unset (or (buffer-file-name) 'unset)))
   (lambda (f) (magik-function "system.unlink" f 'false 'true))
   beg))
(defalias 'transmit-region-to-magik 'magik-transmit-region)

(defun magik-package-line ()
  "Return the _package line if one exists."
  ;;Find package name stripping off any surrounding white-space like ^M characters
  ;;Start from the current line and search backwards.
  ;;We are not usually interested in _package statements after point.
  (save-match-data
    (if (re-search-backward "^\\s-*\\(_package \\w+\\)\\s-*$" nil t)
        (concat (buffer-substring-no-properties
                 (match-beginning 1) (match-end 1))
                "\n"))))

(defun magik-transmit-string (str package do-magik-command tidy-magik-command &optional start gis process)
  "Generalised function to send code to Magik via a temporary file.
If this command is repeated before the previous file has been processed by
Magik, another file shall be written."
  (let* ((gis (magik-utils-get-buffer-mode gis
                                           'magik-session-mode
                                           "Enter Magik Session buffer:"
                                           magik-session-buffer
                                           'magik-session-buffer-alist-prefix-function))
         (process (barf-if-no-gis gis process))
         (orig-buf  (buffer-name))
         (orig-file (or (buffer-file-name) ""))
         (position  (if start (number-to-string start) "1"))
         (filename (make-temp-file (concat "magik-transmit-"
                                           (user-login-name)
                                           (number-to-string (process-id process)))))
         (package (or package "\n")) ;; Need a newline to ensure fixed number of lines for `gis-goto-error'
         (coding-system buffer-file-coding-system))

    (with-current-buffer (get-buffer-create " *transmit magik debug*")
      (erase-buffer)
      (setq buffer-file-coding-system coding-system)
      (insert "write(\"**** Emacs: buffer=" orig-buf
              " file=" orig-file
              " position=" position
              "\")\n$\n"
              package
              str)
      (goto-char (point-min))
      (when magik-transmit-debug-p
        (magik-perform-replace-no-set-mark "#DEBUG" "" nil))
      (write-region (point-min) (point-max) filename nil 'xxx))
    (message "Transmitting to %s" gis)
    (process-send-string
     process
     (concat
      "_protect\n"
      (funcall do-magik-command filename)
      "_protection\n"
      (funcall tidy-magik-command filename)
      "_endprotect\n"
      "$\n"))
    gis))

(defun magik-drag-n-drop-load (gis filename)
  "Interface to Drag and Drop GIS mode.
Called by `magik-session-drag-n-drop-load' when a Magik FILENAME is dropped.
Argument FILENAME ..."
  (let ((process (barf-if-no-gis gis)))
    (message "Transmitting to %s" gis)
    (process-send-string
     process
     (concat
      (magik-function "load_file" filename)
      "$\n"))))

(defun magik-method-name-mode (&optional arg)
  "Toggle display of current method in mode line.
Toggles the value of variable `magik-method-name-mode'.
With a positive numeric ARG, display method name on mode line,
With a negative numeric arg, remove  method name from the mode line."
  (interactive "P")
  (setq magik-method-name-mode
        (if (null arg)
            (not magik-method-name-mode)
          (> (prefix-numeric-value arg) 0)))
  (dolist (buf (magik-utils-buffer-mode-list 'magik-mode))
    (with-current-buffer buf
      (magik-method-name-set)
      (force-mode-line-update)))
  (if magik-method-name-mode
      (add-hook 'post-command-hook 'magik-method-name-set)
    (remove-hook 'post-command-hook 'magik-method-name-set))
  (message
   (if magik-method-name-mode
       "Method name display on."
     "Method name display off.")))

(defun magik-method-name-set-text-properties (buf method)
  "Return string combining BUF and METHOD suitable for display in mode-line."
  ;;propertize only on Emacs 21 so we use add-text-properties
  (add-text-properties 0 (length buf)
                       (list
                        'face '(:weight bold)
                        'help-echo
                        (purecopy "mouse-1: previous buffer, mouse-3: next buffer")
                        'local-map mode-line-buffer-identification-keymap)
                       buf)
  (add-text-properties 0 (length method)
                       (list
                        'face '(:inverse-video t)
                        'help-echo
                        (purecopy "mouse-1: previous buffer, mouse-3: next buffer")
                        'local-map mode-line-buffer-identification-keymap)
                       method)
  (concat buf method))

(defun magik-method-name-postfix (&optional pt)
  "Return the postfix string of the current method.
Where the type of the method is the method's postfix characters:
 ()
 <<
 ^<<
 []
 [,]
 ()<<
 ()^<<

If PT is given, goto that char position."
  (save-excursion
    (and pt (goto-char pt))
    (skip-syntax-forward "-")
    (save-match-data
      (cond ((eq (following-char) ?\()
             (condition-case err
                 (progn
                   (forward-sexp 1)
                   (skip-syntax-forward "-")
                   (cond ((looking-at "\\^<<")      "()^<<")
                         ((eq (following-char) ?<)  "()<<")
                         (t "()")))
               (error "()")))
            ((eq (following-char) ?<)  "<<")
            ((looking-at "\\^<<")      "^<<")
            ((looking-at "\\[[^\],]+,")
             (if (re-search-forward "\\^?<<" (line-end-position) t)
                 (concat "[,]" (match-string-no-properties 0))
               "[,]"))
            ((eq (following-char) ?\[)
             (if (re-search-forward "\\^?<<" (line-end-position) t)
                 (concat "[]" (match-string-no-properties 0))
               "[]"))
            (t "")))))

(defun magik-current-package-name ()
  "Return the package name from the most recent _package line, or \"sw\"."
  (save-excursion
    (save-match-data
      (if (re-search-backward "^\\s-*_package \\(\\w+\\)\\s-*$" nil t)
          (buffer-substring-no-properties (match-beginning 1) (match-end 1))
        "sw"))))

(defun magik-current-method-name ()
  "Return current method and exemplar names as a list (METHOD EXEMPLAR PACKAGE)."
  (let ((this-syntax-table (copy-syntax-table magik-base-mode-syntax-table))
        (package (magik-current-package-name))
        (exemplar "")
        (name "")
        pt end)
    (save-excursion
      (save-match-data
        (modify-syntax-entry ?_ "w" this-syntax-table)
        (set-syntax-table this-syntax-table)
        (cond ((save-excursion
                 ;;method creation methods, currently ignores pragma
                 (setq pt (point))
                 (beginning-of-line)
                 (and
                  (re-search-backward "^\\s-*\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\.def\\(ine_\\(slot_access\\|shared_constant\\|shared_variable\\|property\\)\\|_property\\)\\s-*([ \t\n]*:\\s-*\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)" nil t)
                  (<= (point) pt)
                  (progn
                    (goto-char (match-end 3))
                    (skip-syntax-forward "-")
                    (forward-sexp 1)
                    t)
                  (>= (point) pt)))
               (setq exemplar (match-string-no-properties 1)
                     name     (match-string-no-properties 5)))
              ((save-excursion
                 ;;arrays
                 (beginning-of-line)
                 (setq pt (point))
                 (and (magik-forward-endmethod t)
                      (setq end (point))
                      (magik-backward-method t)
                      (<= (point) pt)
                      (re-search-forward "^.*\\b_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\s-*?\\[" end t)))
               (setq exemplar (match-string-no-properties 1)
                     name     (magik-method-name-postfix (match-end 1))))
              ((save-excursion
                 ;;normal methods
                 (beginning-of-line)
                 (setq pt (point))
                 (and (magik-forward-endmethod t)
                      (magik-backward-method t)
                      (<= (point) pt)
                      (re-search-forward "^.*\\b_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\.\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\(\\s-*\\^?<<\\)?" nil t)))
               (setq exemplar (match-string-no-properties 1)
                     name     (car (magik-imenu-method-name 3))))
              (t nil))))
    (list name exemplar package)))

(defun magik-method-name-set ()
  "Set the name of the current Magik method in `magik-method-name'."
  (cond ((not (derived-mode-p 'magik-base-mode))
         nil)
        ((not magik-method-name-mode)
         (setq mode-line-buffer-identification
               (default-value 'mode-line-buffer-identification)))
        (t
         (condition-case err
             (setq magik-method-name (funcall magik-method-name-set-text-function
                                              (format "%-12s " (buffer-name))
                                              (car (magik-current-method-name)))
                   mode-line-buffer-identification 'magik-method-name)
           (error
            (setq magik-method-name "")))
         (set-syntax-table magik-base-mode-syntax-table))))

(defun magik-trace-curr-statement ()
  "Add a trace statement for the current statement."
  (interactive "*")
  (back-to-indentation)
  (let
      ((col (current-column))
       (str (gsub (buffer-substring-no-properties
                   (point) (line-end-position)) "\"" "\" + %\" + \"")))
    (beginning-of-line)
    (indent-to col)
    (insert "write(\""
            ;; (make-string col ? ) ;;;; withdrawn.
            "+++ "
            str
            " +++\")\n")))

(defun magik-fill-public-comment ()
  "Fill a comment paragraph."
  (interactive "*")
  (save-excursion
    (if (progn
          (back-to-indentation)
          (looking-at "\\(##?\\)[ \t]+[^ \t\n]"))
        (let*
            ((comment-str (match-string 1))
             (regexp-str (concat
                          "[ \t]*"
                          comment-str
                          "[ \t]+[^ \t\n]"))
             beg
             (fill-prefix
              (concat (progn
                        (back-to-indentation)
                        (buffer-substring-no-properties (line-beginning-position) (point)))
                      comment-str
                      " "))
             (fill-column (+ (current-column) 63)))
          (beginning-of-line)
          (while
              (and (looking-at regexp-str)
                   (zerop (forward-line -1))))
          (if (not (looking-at regexp-str))
              (forward-line 1))
          (setq beg (point))
          (while (and (looking-at regexp-str)
                      (zerop (forward-line 1))))
          (fill-region-as-paragraph beg (point))))))

;;; Mods to do commenting and uncommenting in magik code (Sarfaraz).
;; AJM: TODO Use comment-dwim what about Emacs 19 and 20???
(defun magik-comment (nlines)
  "Puts NLINES in first column of ARG lines at point."
  (interactive "p")
  (while (and (> nlines 0) (not (eobp)))
    (cl-decf nlines)
    (beginning-of-line 1)
    (insert-char ?# 1)
    (forward-line 1)))

(defun magik-un-comment (nlines)
  "Deletes NLINES in first column of ARG lines at point."
  (interactive "*p")
  (while (and (> nlines 0) (not (eobp)))
    (cl-decf nlines)
    (beginning-of-line 1)
    (skip-chars-forward "\t")
    (if (char-equal (char-after (point)) ?#)
        (delete-char 1))
    (forward-line 1)))

(defun magik-comment-region()
  "Puts # in first column of each line in the region."
  (interactive "*")
  (save-excursion
    (if (> (point) (mark t))
        (exchange-point-and-mark))
    (magik-comment (count-lines (point) (mark t)))))

(defun magik-uncomment-region()
  "Remove # in first column of each line in the region."
  (interactive "*")
  (save-excursion
    (if (> (point) (mark t))
        (exchange-point-and-mark))
    (magik-un-comment (count-lines (point) (mark t)))))

(defun magik-symbol-complete (&optional buffer)
  "Perform completion on Magik symbol preceding point.
The symbol is compared against the symbols that exist in the Magik
process running in the BUFFER named in the variable, `magik-session-buffer'.

With a prefix arg, ask user for GIS buffer to use."
  (interactive "*")
  ;; the actual completion is done by the process filter: gis-filter-completion-action
  (setq buffer (magik-utils-get-buffer-mode buffer
                                            'magik-session-mode
                                            "Enter Magik Session buffer:"
                                            magik-session-buffer
                                            'magik-session-buffer-alist-prefix-function))
  (barf-if-no-gis buffer)

  (if (equal (magik-utils-curr-word) "")
      (message "Doing a completion on the empty string would take too long")
    (if (<= (length (magik-utils-curr-word)) 2)
        (message "Symbol is already complete or is too short."))
    (process-send-string
     (get-buffer-process buffer)
     (concat "symbol_table.emacs_write_completions(\"" (magik-utils-curr-word) "\")\n$\n"))))

(defun magik-compare-methods (ignore-whitespace)
  "Compare Methods in two windows using \\[compare-windows].

A prefix arg means ignore changes in whitespace.
The variable `compare-windows-whitespace' controls how whitespace is skipped.
If `compare-ignore-case' is non-nil, changes in case are also ignored.
Argument IGNORE-WHITESPACE ..."
  (interactive "P")
  (magik-backward-method t)
  (push-mark nil t)
  (other-window 1)
  (magik-backward-method t)
  (push-mark nil t)
  (other-window 1)
  (compare-windows ignore-whitespace))

(defun magik-ediff-regions-wordwise-internal (buffer-A reg-A-beg reg-A-end buffer-B reg-B-beg reg-B-end job-name)
  (require 'ediff) ; ediff-regions-internal is not an autoloaded function...
  (let ((args (list buffer-A reg-A-beg reg-A-end buffer-B reg-B-beg reg-B-end nil job-name 'word-mode)))
    (setq args (append args (list nil)))
    (apply 'ediff-regions-internal args)))

(defun magik-ediff-methods (&optional buffer-A buffer-B)
  "Compare Methods using Ediff in two windows using \\[ediff-regions-wordwise].
Using a prefix arg in Emacsen 22 or later will offer the user the ability to
modify the marked regions before running the Ediff.
Optional argument BUFFER-A ...
Optional argument BUFFER-B ..."
  (interactive)
  (save-excursion
    (let (reg-A-beg reg-A-end reg-B-beg reg-B-end)
      (set-buffer (or buffer-A (setq buffer-A (current-buffer))))
      (magik-mark-method)
      (setq reg-A-beg (region-beginning)
            reg-A-end (region-end))
      (set-buffer (or buffer-B (setq buffer-B (progn (other-window 1) (current-buffer)))))
      (magik-mark-method)
      (setq buffer-B  (current-buffer)
            reg-B-beg (region-beginning)
            reg-B-end (region-end))
      (if current-prefix-arg
          ;; Emacs 22 calls a new function, ediff-clone-buffer-for-region-comparison
          ;; that unfortunately, has no means of overriding its interactive component.
          ;; magik-ediff-regions-wordwise-internal has been written to call the main
          ;; ediff-regions-internal function directly to avoid the user interaction.
          (ediff-regions-wordwise buffer-A buffer-B)
        (magik-ediff-regions-wordwise-internal buffer-A reg-A-beg reg-A-end
                                               buffer-B reg-B-beg reg-B-end
                                               'magik-ediff-methods)))))

(defun magik-set-work-buffer-name (buffer)
  "Set `magik-work-buffer'.
Argument BUFFER ..."
  (interactive "BBuffer: ")
  (setq magik-work-buffer buffer)
  (with-current-buffer (get-buffer-create magik-work-buffer)
    (magik-mode)))

(defun magik-copy-method-to-buffer (&optional buffer)
  "Copy method to BUFFER.
If BUFFER is nil, use buffer name stored in variable `magik-work-buffer'.
Otherwise create a sensibly named buffer based on the class name of the method."
  (interactive)
  (save-excursion
    (save-match-data
      (magik-forward-endmethod)
      (magik-backward-method)
      (push-mark nil t)
      (while
          (not (or (eobp)
                   (looking-at "^\\s-*\\(_abstract\\(\n\\|\\s-\\)+\\)?\\(_private\\(\n\\|\\s-\\)+\\)?\\(_iter\\(\n\\|\\s-\\)+\\)?_method\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\.")))
        (forward-line 1))
      (goto-char (match-end 0))
      (setq buffer (or buffer
                       magik-work-buffer
                       (concat (match-string-no-properties 7)
                               ".magik")))

      (cond ((eq (following-char) ?\()
             (or (search-forward ")" nil t)
                 (error "Can't find closing round ) bracket")))
            ((eq (following-char) ?\[)
             (or (search-forward "]" nil t)
                 (error "Can't find closing square ] bracket"))))

      (magik-forward-endmethod)
      (magik-copy-region-to-buffer (mark t) (point) buffer))))

(defun magik-copy-region-to-buffer (start end &optional buffer)
  "Copy region to BUFFER.
If BUFFER is nil, use buffer name stored in variable `magik-work-buffer'
otherwise prompt user.
Argument START ...
Argument END ..."
  (interactive "r")
  (setq buffer (or buffer magik-work-buffer (read-buffer "Buffer: ")))

  (let ((code (buffer-substring start end)))
    (set-buffer (get-buffer-create buffer))
    (goto-char (point-max))
    (or (looking-at "^") (insert "\n"))
    (insert code "\n")
    (set-buffer-modified-p nil)
    (magik-mode))
  (display-buffer buffer))

(defun magik-file-pragma ()
  "Search file for missing pragmas."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'magik-base-mode)
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "def_slotted_exemplar" magik-regexp)) nil t)
        (magik-parse-pragma))
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "method" magik-regexp)) nil t)
        (magik-parse-pragma))))))

(defun magik-single-pragma ()
  "Search last def_slotted_exemplar/method for missing pragma."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'magik-base-mode)
      (forward-line)
      (let ((starting-point (line-number-at-pos))
            (exemplar-point nil)
            (method-point nil))
        (when (not (equal (search-backward-regexp (cdr (assoc "def_slotted_exemplar" magik-regexp)) nil t) nil))
          (setq exemplar-point (line-number-at-pos)))
        (goto-line starting-point)
        (when (not (equal (search-backward-regexp (cdr (assoc "method" magik-regexp)) nil t) nil))
          (setq method-point (line-number-at-pos)))
        (when (or (not (equal exemplar-point nil))
                  (not (equal method-point nil)))
          (when (or (and (not (equal exemplar-point nil))
                         (> exemplar-point (line-number-at-pos)))
                    (equal method-point nil))
            (goto-line exemplar-point))
          (magik-parse-pragma)))))))

(defun magik-parse-pragma ()
  "Helper function for inserting pragma."
  (let ((ending-point (line-number-at-pos))
        (starting-point 0)
        (search-result nil))
    (save-excursion
      (search-backward-regexp "^\\$" nil t)
      (setq starting-point (line-number-at-pos)
            search-result (search-forward-regexp (cdr (assoc "pragma" magik-regexp)) nil t))
      (when (or (and (not (equal search-result nil))
                     (< ending-point (line-number-at-pos)))
                (equal search-result nil))
        (magik-write-pragma ending-point)
        t))))

(defun magik-write-pragma (ending-point)
  "Writer function for inserting pragma.
Argument ENDING-POINT ..."
  (goto-line ending-point)
  (magik-insert-pragma))

;;; Imenu configuration functions
(defun magik-imenu-method-name (index)
  "Helper function for creating the correct name for a method in Imenu.
Used by `magik-imenu-expression' and `magik-imenu-create-index-function'
to return a syntactically correct method name for the Imenu menu entry.
Argument INDEX ..."
  (let ((beg (match-beginning index))
        (name (match-string-no-properties index)))
    (cons (concat name (magik-method-name-postfix (match-end index))) beg)))

(defun magik-imenu-create-index-function ()
  "Return an index of the current buffer as an alist.

Uses `magik-imenu-expression' as the alist with elements that look like this:
 (MENU-TITLE REGEXP INDEX).
or
 (MENU-TITLE REGEXP FUNCTION ARGS).

MENU-TITLE is a string used as the title for the submenu or nil if the
entries are not nested.

REGEXP is a regexp that should match a construct in the buffer that is
to be displayed in the menu; i.e., function or variable definitions,
etc.  It contains a substring which is the name to appear in the
menu.  See the info section on Regexps for more information.

INDEX points to the substring in REGEXP that contains the name (of the
function, variable or type) that is to appear in the menu.

FUNCTION and ARGS are usedto generate the name based upon the result
of the regexp match.

Returns an index of the current buffer as an alist.  The elements in
the alist look like: (INDEX-NAME . INDEX-POSITION).  They may also be
nested index lists like (INDEX-NAME . INDEX-ALIST) depending on
PATTERNS.

This function is modified from `imenu--generic-function' to basically
provide extra control over the name that appears in the index."

  (let ((index-alist (list 'dummy))
        prev-pos beg
        (case-fold-search imenu-case-fold-search)
        (old-table (syntax-table))
        (table (copy-syntax-table (syntax-table)))
        (slist imenu-syntax-alist))
    ;; Modify the syntax table used while matching regexps.
    (while slist
      ;; The character(s) to modify may be a single char or a string.
      (if (numberp (caar slist))
          (modify-syntax-entry (caar slist) (cdar slist) table)
        (mapc (lambda (c)
                (modify-syntax-entry c (cdar slist) table))
              (caar slist)))
      (setq slist (cdr slist)))
    (goto-char (point-max))
    (unwind-protect     ; for syntax table
        (save-match-data
          (set-syntax-table table)
          ;; map over the elements of imenu-generic-expression
          ;; (typically functions, variables ...)
          (mapc
           (lambda (pat)
             (let ((menu-title (car pat))
                   (regexp (nth 1 pat))
                   (index (nth 2 pat))   ;index and function occupy 3 element
                   (function (nth 2 pat))
                   (rest (cdddr pat)))
               ;; Go backwards for convenience of adding items in order.
               (goto-char (point-max))
               (while (re-search-backward regexp nil t)
                 ;; Add this sort of submenu only when we've found an
                 ;; item for it, avoiding empty, duff menus.
                 (unless (assoc menu-title index-alist)
                   (push (list menu-title) index-alist))

                 ;; menu is the desired submenu,
                 ;; starting with its title (or nil).
                 (let ((menu (assoc menu-title index-alist))
                       item)
                   (if (functionp function)
                       (setq item (apply function rest))
                     ;;We have a simply index
                     (setq beg (match-beginning index))
                     (if imenu-use-markers
                         (setq beg (copy-marker beg)))
                     (setq item (cons (match-string-no-properties index)
                                      beg)))
                   ;; Insert the item unless it is already present.
                   (unless (member item (cdr menu))
                     (setcdr menu
                             (cons item (cdr menu))))))))
           magik-imenu-expression)
          (set-syntax-table old-table)))
    ;; Sort each submenu by position.
    ;; This is in case one submenu gets items from two different regexps.
    (let ((tail index-alist))
      (while tail
        (if (listp (car tail))
            (setcdr (car tail)
                    (sort (cdr (car tail)) 'imenu--sort-by-position)))
        (setq tail (cdr tail))))
    (let ((main-element (assq nil index-alist)))
      (nconc (delq main-element (delq 'dummy index-alist))
             (cdr main-element)))))

(defun magik-ac-exemplar-near-point ()
  "Get current exemplar near cursor position."
  (save-excursion
    (save-match-data
      (let ((pt (1- (magik-ac-method-prefix)))
            variable
            exemplar)
        (goto-char pt)
        ;; Usefully skip over various syntax types:
        (if (not (zerop (skip-syntax-backward "w_().")))
            (setq variable (buffer-substring-no-properties (point) pt)))
        (if variable
            (setq exemplar (cond ((equal variable "_self")
                                  (or (cadr (magik-current-method-name))
                                      (file-name-sans-extension (buffer-name))))
                                 ((member variable magik-ac-object-source-cache)
                                  variable)
                                 ((re-search-backward (concat (regexp-quote variable) "\\s-*^?<<[ \t\n]*\\(\\S-+\\)\\.new") nil t)
                                  (buffer-substring-no-properties (match-beginning 1) (match-end 1)))
                                 (t
                                  nil))))
        exemplar))))

(defun magik-ac-class-method-source ()
  "List of methods on a class.
Uses a cache variable `magik-ac-class-method-source-cache'.
All the methods beginning with the first character are returned and
stored in the cache.  Thus subsequent characters refining the match are
handled by auto-complete refining the list of all possible matches,
without recourse to the class browser."
  (let ((exemplar (magik-ac-exemplar-near-point))
        (ac-prefix ac-prefix))
    (if exemplar
        (progn
          (setq ac-prefix (concat exemplar  "." (if (> (length ac-prefix) 0) (substring ac-prefix 0 1))))
          (if (and magik-ac-class-method-source-cache
                   (equal (concat " " ac-prefix) (car magik-ac-class-method-source-cache)))
              ;; Reuse cache.
              magik-ac-class-method-source-cache
            ;; reset cache
            (setq magik-ac-class-method-source-cache (magik-cb-ac-method-candidates)))))))

(defun magik-ac-object-source-init ()
  "Initialisation function for obtaining all Magik Objects.
For use in auto-complete-mode."
  (if (magik-cb-ac-start-process)
      (let ((ac-prefix "sw:object"))
        (setq magik-ac-object-source-cache (magik-cb-ac-class-candidates)))))

(defun magik-ac-object-prefix ()
  "Detect if point is at a possible object, allowing for a package: prefix."
  (let (pt)
    (cond
     ((re-search-backward "\\(\\sw+:\\)\\(\\sw+\\)\\=" nil t)
      (match-beginning 2))
     ((and (re-search-backward "\\Sw\\(\\sw+\\)\\=" nil t)
           (not (eq (following-char) ?.))
           (setq pt (match-beginning 1))
           (not (equal ":" (buffer-substring-no-properties pt (1+ pt)))))
      pt)
     (t nil))))

(defun magik-ac-method-prefix ()
  "Detect if point is at . method point."
  (if (re-search-backward "\\(_self\\|_clone\\|\\S-\\)\\.\\(\\sw+\\)\\=" nil t)
      (match-beginning 2)))

(defun magik-ac-raise-condition-source-init ()
  "Initialisation function for obtaining all Magik Conditions.
For use in auto-complete-mode.  Once initialised this variable is not refreshed."
  (if (magik-cb-ac-start-process)
      (let ((ac-prefix "<condition>."))
        (if magik-ac-raise-condition-source-cache
            ;; consider enabling refresh using auto-complete's 10 minute refresh idle timer?
            magik-ac-raise-condition-source-cache
          (setq magik-ac-raise-condition-source-cache (magik-cb-ac-method-candidates))))))

(defun magik-ac-raise-condition-prefix ()
  "Detect if point is at a condition.raise."
  (if (re-search-backward "condition\\.raise(\\s-*:\\(\\sw+\\)\\=" nil t)
      (match-beginning 1)))

(defun magik-ac-global-source-init ()
  "Initialisation function for obtaining all Magik Conditions.
For use in auto-complete-mode.  Once initialised this variable is not refreshed."
  (if (magik-cb-ac-start-process)
      (let ((ac-prefix "<global>."))
        (if magik-ac-global-source-cache
            ;; consider enabling refresh using auto-complete's 10 minute refresh idle timer?
            magik-ac-global-source-cache
          (setq magik-ac-global-source-cache (magik-cb-ac-method-candidates))))))

(defun magik-ac-dynamic-prefix ()
  "Detect if point is at !..! dynamic point."
  (let (pt)
    (if (and (re-search-backward "\\Sw\\(!\\sw*\\)\\=" nil t)
             (not (eq (following-char) ?.))
             (setq pt (match-beginning 1))
             (not (equal ":" (buffer-substring-no-properties pt (1+ pt)))))
        pt)))

(defun magik-ac-complete ()
  "Auto-complete command for Magik entities."
  (interactive)
  (let ((ac 'auto-complete))
    (when (fboundp ac)
      (funcall ac '(
                    magik-ac-class-method-source
                    magik-ac-raise-condition-source
                    magik-ac-dynamic-source
                    magik-ac-object-source
                    magik-ac-global-source)))))

;;; Smallworld Compatibility functions
(defalias 'magik-point-on-pragma-line-p 'pragma-line-p)

(defun magik-translate-old-vec-notation ()
  "Search for the next \"vec(\" in the current buffer.
Translate it and the closing bracket into the new \"{...}\" notation."
  (interactive)
  (re-search-forward "\\<vec(")
  (backward-char 4)
  (delete-char 3)
  (save-excursion
    (forward-sexp)
    (delete-char -1)
    (insert "\}"))
  (delete-char 1)
  (insert "\{"))
(put 'magik-translate-old-vec-notation 'disabled t)

(defun magik--in-string-or-comment-p ()
  "Return non-nil if point is inside a string or comment."
  (syntax-ppss-context (syntax-ppss)))

(defun magik-yas-maybe-expand ()
  "Expand `yasnippet` if possible, otherwise insert a space.
Prevents expansion inside strings and comments."
  (interactive)
  (if (or (magik--in-string-or-comment-p)
          (not (yas-expand)))
      (self-insert-command 1)))

;;; Package initialisation
(define-abbrev-table 'magik-base-mode-abbrev-table
  (mapcar (lambda (str)
            (list str str 'magik-expand-abbrev))
          (append magik-keyword-constants magik-keyword-operators
                  magik-keyword-class magik-keyword-statements
                  magik-keyword-methods magik-keyword-procedures
                  magik-keyword-loop magik-keyword-arguments
                  magik-keyword-variable magik-keyword-kleenean))
  "Abbrev table for Magik mode."
  :regexp "\\<\\([+[:word:]]+\\)")

(if magik-under-as-char
    (modify-syntax-entry ?_ "w" magik-base-mode-syntax-table))
(modify-syntax-entry ?\\ "." magik-base-mode-syntax-table) ;; \ is not an escape character in magik mode.
(modify-syntax-entry ?? "w" magik-base-mode-syntax-table)
(modify-syntax-entry ?! "w" magik-base-mode-syntax-table)
;; char intro
(modify-syntax-entry ?% "/" magik-base-mode-syntax-table)
;; multi quote
(modify-syntax-entry ?| "$" magik-base-mode-syntax-table)
;; colon is now part of a word due to the introduction of packages.
;;       Consequently symbols now include the initial :
(modify-syntax-entry ?: "w" magik-base-mode-syntax-table) ;cf \ = "/" in TeX mode.
;; comments
(modify-syntax-entry ?# "<" magik-base-mode-syntax-table)
(modify-syntax-entry ?\n ">" magik-base-mode-syntax-table)
(modify-syntax-entry ?+ "." magik-base-mode-syntax-table)
(modify-syntax-entry ?- "." magik-base-mode-syntax-table)
(modify-syntax-entry ?* "." magik-base-mode-syntax-table)
(modify-syntax-entry ?/ "." magik-base-mode-syntax-table)
(modify-syntax-entry ?= "." magik-base-mode-syntax-table)
(modify-syntax-entry ?$ "." magik-base-mode-syntax-table)
(modify-syntax-entry ?< "." magik-base-mode-syntax-table)
(modify-syntax-entry ?> "." magik-base-mode-syntax-table)
(modify-syntax-entry ?& "." magik-base-mode-syntax-table)
(modify-syntax-entry ?\" "\"" magik-base-mode-syntax-table)

;;; package setup via setting of variable before load.
(and magik-method-name-mode
     (magik-method-name-mode magik-method-name-mode))
(and magik-work-buffer
     (magik-set-work-buffer-name magik-work-buffer))
(and magik-transmit-method-eom-mode
     (magik-transmit-method-eom-mode magik-transmit-method-eom-mode))

;;This functionality does not actually require a function to enable as it
;;purely controlled via the value of the variable magik-mark-method-exchange
;;(and magik-mark-method-exchange
;;     (magik-mark-method-exchange-mode magik-mark-method-exchange))

;; This function is defined in magik-patch minor mode but used in magik-mode-hook
;; We define it here in case a user stores magik-mode-hook in their .emacs and also
;; switches between the Emacs development environment setup and Emacs customer setup.
(or (functionp 'magik-patch-maybe-turn-on-patch-mode) ; only define it if undefined.
    (defalias 'magik-patch-maybe-turn-on-patch-mode 'ignore))

;;; Package registration

;;;###autoload
(or (assoc "\\.magik\\'" auto-mode-alist)
    (push '("\\.magik\\'" . magik-mode) auto-mode-alist))

(or (assq 'magik-transmit-debug-p minor-mode-alist)
    (push '(magik-transmit-debug-p magik-transmit-debug-mode-line-string) minor-mode-alist))

;;; speedbar configuration
(with-eval-after-load 'speedbar
  (speedbar-add-supported-extension ".magik"))

;;MSB configuration
(defun magik-msb-configuration ()
  "Add Magik files to msb menu, supposes that msb is already loaded."
  (let* ((l (length msb-menu-cond))
         (last (nth (1- l) msb-menu-cond))
         (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
         (handle (1- (nth 1 last))))
    (setcdr precdr (list
                    (list
                     '(derived-mode-p 'magik-base-mode)
                     handle
                     "Magik Files (%d)")
                    last))))

(with-eval-after-load 'msb
  (magik-msb-configuration))

;;Auto-complete configuration
(defun magik-ac-configuration ()
  "Configure Magik package for auto-complete mode."
  (ac-define-prefix 'magik-dynamic 'magik-ac-dynamic-prefix)
  (ac-define-prefix 'magik-condition 'magik-ac-raise-condition-prefix)
  (ac-define-prefix 'magik-object 'magik-ac-object-prefix)
  (ac-define-prefix 'magik-method 'magik-ac-method-prefix)
  (setq ac-modes (append (list 'magik-mode) ac-modes)))

(with-eval-after-load 'auto-complete
  (magik-ac-configuration))

;;Flycheck configuration
(with-eval-after-load 'flycheck
  (require 'magik-lint))

;;YASnippet configuration
(defun magik--snippets-initialize ()
  "Initialize the Magik snippets."
  (let ((snip-dir (expand-file-name "snippets" (file-name-directory (or load-file-name (buffer-file-name))))))
    (when (boundp 'yas-snippet-dirs)
      (add-to-list 'yas-snippet-dirs snip-dir t))
    (yas-load-directory snip-dir)))

(with-eval-after-load 'yasnippet
  (magik--snippets-initialize))

(progn
  ;; ------------------------ magik mode ------------------------

  (define-key magik-mode-map "\r" 'magik-newline)
  (define-key magik-mode-map "\n" 'newline)
  (define-key magik-mode-map "\t" 'magik-indent-command)
  (define-key magik-mode-map "#"  'magik-electric-hash)

  (define-key magik-base-mode-map " " 'magik-yas-maybe-expand)
  (define-key magik-base-mode-map "/" 'magik-electric-pragma-slash)
  (define-key magik-base-mode-map "\\" 'magik-electric-pragma-backslash)

  (define-key magik-base-mode-map "\C-\M-h" 'magik-mark-method) ;standard key mapping
  (define-key magik-base-mode-map [M-up] 'magik-safe-backward-method)
  (define-key magik-base-mode-map [M-down] 'magik-safe-forward-method)

  (define-key magik-base-mode-map (kbd "<f2> <up>") 'magik-safe-backward-method)
  (define-key magik-base-mode-map (kbd "<f2> <down>") 'magik-safe-forward-method)
  (define-key magik-base-mode-map (kbd "<f2> $") 'magik-transmit-$-chunk)
  (define-key magik-base-mode-map (kbd "<f2> D") 'magik-file-sw-method-docs)
  (define-key magik-base-mode-map (kbd "<f2> d") 'magik-single-sw-method-docs)
  (define-key magik-base-mode-map (kbd "<f2> P") 'magik-file-pragma)
  (define-key magik-base-mode-map (kbd "<f2> p") 'magik-single-pragma)

  (define-key magik-base-mode-map (kbd "<f4> <f4>") 'magik-symbol-complete)
  (define-key magik-base-mode-map (kbd "<f4> c") 'magik-copy-method)
  (define-key magik-base-mode-map (kbd "<f4> e") 'magik-ediff-methods)
  (define-key magik-base-mode-map (kbd "<f4> <f3>") 'magik-cb-magik-ediff-methods)
  (define-key magik-base-mode-map (kbd "<f4> m") 'magik-copy-method-to-buffer)
  (define-key magik-base-mode-map (kbd "<f4> n") 'magik-set-work-buffer-name)
  (define-key magik-base-mode-map (kbd "<f4> r") 'magik-copy-region-to-buffer)
  (define-key magik-base-mode-map (kbd "<f4> s") 'magik-add-debug-statement)
  (define-key magik-base-mode-map (kbd "<f4> w") 'magik-compare-methods))

(provide 'magik-mode)
;;; magik-mode.el ends here
