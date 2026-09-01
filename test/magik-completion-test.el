;;; magik-completion-test.el --- Tests for magik-completion.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `magik-completion.el', covering snippet building and
;; args-line parsing with the various `magik-completion-insert-*' options.

;;; Code:

(require 'test-helper)
(require 'cl-lib)
(require 'magik-mode)
(require 'magik-session)
(require 'magik-completion)

;;; Helpers

(defun magik-completion-test--candidate (args optional gather start-sig)
  "Return a propertized completion candidate string for testing.
ARGS is a list of required arg names, OPTIONAL a list of optional arg names,
GATHER a list with the gather arg name, START-SIG is \"(\" or nil."
  (propertize "test_method"
              'magik-args args
              'magik-optional optional
              'magik-gather gather
              'magik-start-signature start-sig))

(defmacro magik-completion-test--with-settings (params optional gather &rest body)
  "Eval BODY with the three insert-* defcustoms bound to PARAMS, OPTIONAL, GATHER."
  (declare (indent 3))
  `(let ((magik-completion-insert-params ,params)
         (magik-completion-insert-optional-params ,optional)
         (magik-completion-insert-gather-param ,gather))
     ,@body))

;;; magik-completion--build-param-snippet
;;; --- insert-params disabled

(ert-deftest magik-completion--build-param-snippet--insert-params-nil-returns-nil ()
  "When `magik-completion-insert-params' is nil, no snippet is produced."
  (let ((cand (magik-completion-test--candidate '("a_stream") nil nil "(")))
    (magik-completion-test--with-settings nil t t
      (should-not (magik-completion--build-param-snippet cand)))))

;;; --- slot-like methods (no start-sig)

(ert-deftest magik-completion--build-param-snippet--no-start-sig-returns-nil ()
  "Slot-like methods without parens produce no snippet."
  (let ((cand (magik-completion-test--candidate nil nil nil nil)))
    (magik-completion-test--with-settings t t t
      (should-not (magik-completion--build-param-snippet cand)))))

;;; --- required params only

(ert-deftest magik-completion--build-param-snippet--required-only ()
  "Required params produce a snippet with numbered fields."
  (let ((cand (magik-completion-test--candidate '("a_stream") nil nil "(")))
    (magik-completion-test--with-settings t t t
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:a_stream})$0")))))

(ert-deftest magik-completion--build-param-snippet--multiple-required ()
  "Multiple required params all appear as numbered fields."
  (let ((cand (magik-completion-test--candidate '("thing" "iter_method") nil nil "(")))
    (magik-completion-test--with-settings t t t
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:thing}, ${2:iter_method})$0")))))

;;; --- no params (method takes parens but nothing inside)

(ert-deftest magik-completion--build-param-snippet--no-params-inserts-parens ()
  "Method with start-sig but no params inserts empty parens."
  (let ((cand (magik-completion-test--candidate nil nil nil "(")))
    (magik-completion-test--with-settings t t t
      (should (equal (magik-completion--build-param-snippet cand) "()")))))

;;; --- optional params only

(ert-deftest magik-completion--build-param-snippet--optional-only-included ()
  "Optional-only method with insert-optional=t produces snippet."
  (let ((cand (magik-completion-test--candidate nil '("dataset_name") nil "(")))
    (magik-completion-test--with-settings t t t
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:dataset_name})$0")))))

(ert-deftest magik-completion--build-param-snippet--optional-only-excluded ()
  "Optional-only method with insert-optional=nil produces empty parens."
  (let ((cand (magik-completion-test--candidate nil '("dataset_name") nil "(")))
    (magik-completion-test--with-settings t nil t
      (should (equal (magik-completion--build-param-snippet cand) "()")))))

(ert-deftest magik-completion--build-param-snippet--multiple-optional-included ()
  "Multiple optional params all appear when included."
  (let ((cand (magik-completion-test--candidate nil '("name" "value") nil "(")))
    (magik-completion-test--with-settings t t t
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:name}, ${2:value})$0")))))

;;; --- gather param

(ert-deftest magik-completion--build-param-snippet--gather-only-included ()
  "Gather-only method includes the gather param when setting is on."
  (let ((cand (magik-completion-test--candidate nil nil '("args") "(")))
    (magik-completion-test--with-settings t t t
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:args})$0")))))

(ert-deftest magik-completion--build-param-snippet--gather-only-excluded ()
  "Gather-only method with insert-gather=nil produces empty parens."
  (let ((cand (magik-completion-test--candidate nil nil '("args") "(")))
    (magik-completion-test--with-settings t t nil
      (should (equal (magik-completion--build-param-snippet cand) "()")))))

;;; --- required + gather

(ert-deftest magik-completion--build-param-snippet--required-and-gather ()
  "Required params followed by gather all appear in the snippet."
  (let ((cand (magik-completion-test--candidate '("thing" "iter_method") nil '("args") "(")))
    (magik-completion-test--with-settings t t t
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:thing}, ${2:iter_method}, ${3:args})$0")))))

(ert-deftest magik-completion--build-param-snippet--required-and-gather-no-gather ()
  "Gather excluded when insert-gather=nil; required params still shown."
  (let ((cand (magik-completion-test--candidate '("thing" "iter_method") nil '("args") "(")))
    (magik-completion-test--with-settings t t nil
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:thing}, ${2:iter_method})$0")))))

;;; --- optional + gather  (the sw_action.copy case)

(ert-deftest magik-completion--build-param-snippet--optional-and-gather-both-included ()
  "Optional before gather: both included when insert-optional=t."
  (let ((cand (magik-completion-test--candidate nil '("new_name") '("new_properties") "(")))
    (magik-completion-test--with-settings t t t
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:new_name}, ${2:new_properties})$0")))))

(ert-deftest magik-completion--build-param-snippet--optional-excluded-suppresses-gather ()
  "Gather suppressed when preceding optional params are excluded.
Passing gather args without the optional positional arg is a positional
error in Magik, so we fall back to empty parens rather than a misleading
snippet."
  (let ((cand (magik-completion-test--candidate nil '("new_name") '("new_properties") "(")))
    (magik-completion-test--with-settings t nil t
      (should (equal (magik-completion--build-param-snippet cand) "()")))))

;;; --- required + optional + gather

(ert-deftest magik-completion--build-param-snippet--all-param-types-included ()
  "All param types appear in order when all settings are on."
  (let ((cand (magik-completion-test--candidate '("a") '("b") '("c") "(")))
    (magik-completion-test--with-settings t t t
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:a}, ${2:b}, ${3:c})$0")))))

(ert-deftest magik-completion--build-param-snippet--required-optional-excluded-gather-suppressed ()
  "Gather suppressed when optional excluded, even with required params present."
  (let ((cand (magik-completion-test--candidate '("a") '("b") '("c") "(")))
    (magik-completion-test--with-settings t nil t
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:a})$0")))))

(ert-deftest magik-completion--build-param-snippet--required-with-gather-excluded ()
  "Only required params shown when gather excluded, no optional involved."
  (let ((cand (magik-completion-test--candidate '("a") nil '("c") "(")))
    (magik-completion-test--with-settings t t nil
      (should (equal (magik-completion--build-param-snippet cand)
                     "(${1:a})$0")))))

;;; magik-completion--parse-args-line

(defun magik-completion-test--parse-args (line)
  "Parse LINE as a CB args line in a temp buffer using `magik-cb-mode' syntax.
Returns (ARGS OPTIONAL GATHER)."
  (with-temp-buffer
    (when (fboundp 'magik-cb-mode) (magik-cb-mode))
    (insert " " line "\n")
    (magik-completion--parse-args-line (point-min))))

(ert-deftest magik-completion--parse-args-line--empty ()
  (should (equal (magik-completion-test--parse-args "") '(nil nil nil))))

(ert-deftest magik-completion--parse-args-line--required-only ()
  (should (equal (magik-completion-test--parse-args "a_stream")
                 '(("a_stream") nil nil))))

(ert-deftest magik-completion--parse-args-line--multiple-required ()
  (should (equal (magik-completion-test--parse-args "thing iter_method")
                 '(("thing" "iter_method") nil nil))))

(ert-deftest magik-completion--parse-args-line--optional-only ()
  (should (equal (magik-completion-test--parse-args "OPT dataset_name")
                 '(nil ("dataset_name") nil))))

(ert-deftest magik-completion--parse-args-line--gather-only ()
  (should (equal (magik-completion-test--parse-args "GATH args")
                 '(nil nil ("args")))))

(ert-deftest magik-completion--parse-args-line--optional-gather ()
  (should (equal (magik-completion-test--parse-args "OPT new_name GATH new_properties")
                 '(nil ("new_name") ("new_properties")))))

(ert-deftest magik-completion--parse-args-line--required-optional-gather ()
  (should (equal (magik-completion-test--parse-args "thing iter_method OPT GATH args")
                 '(("thing" "iter_method") nil ("args")))))

(ert-deftest magik-completion--parse-args-line--question-mark-suffixed-names ()
  "Required param names ending in `?' must be kept whole, not truncated."
  (should (equal (magik-completion-test--parse-args "yes? no?")
                 '(("yes?" "no?") nil nil))))

(ert-deftest magik-completion--parse-args-line--bang-suffixed-optional-name ()
  "Optional param names ending in `!' must be kept whole, not truncated."
  (should (equal (magik-completion-test--parse-args "OPT flag!")
                 '(nil ("flag!") nil))))

(ert-deftest magik-completion--parse-args-line--question-mark-names-without-cb-mode ()
  "Parsing must not depend on `magik-cb-mode' syntax table being active.
The real CB completion process buffer is created in `fundamental-mode'
\(see `magik-completion--ensure-cb-process'), where `?' and `!' are not
word-syntax characters, so the parser must not rely on `skip-syntax-forward'."
  (with-temp-buffer
    (insert " yes? no?\n")
    (should (equal (magik-completion--parse-args-line (point-min))
                   '(("yes?" "no?") nil nil)))))

(ert-deftest magik-completion--parse-args-line--gather-no-trailing-newlines ()
  "Gather param name must not include trailing newlines from the CB buffer."
  (with-temp-buffer
    (when (fboundp 'magik-cb-mode) (magik-cb-mode))
    (insert " GATH args\n\n        ## Some comment\n")
    (let ((result (magik-completion--parse-args-line (point-min))))
      (should (equal (caddr result) '("args"))))))

;;; magik-completion--doc-buffer

(defun magik-completion-test--candidate-with-doc (doc)
  "Return a candidate string with DOC as `magik-documentation' text property."
  (propertize "write_on" 'magik-documentation doc))

(ert-deftest magik-completion--doc-buffer--returns-nil-without-doc ()
  "Candidates without documentation return nil."
  (let ((cand (propertize "write_on")))
    (should-not (magik-completion--doc-buffer cand))))

(ert-deftest magik-completion--doc-buffer--returns-buffer-with-doc ()
  "Candidates with documentation return a live buffer."
  (let* ((cand (magik-completion-test--candidate-with-doc "Writes _self to A_STREAM."))
         (buf (magik-completion--doc-buffer cand)))
    (should (bufferp buf))
    (should (buffer-live-p buf))))

(ert-deftest magik-completion--doc-buffer--buffer-contains-doc ()
  "The returned buffer's contents equal the documentation string."
  (let* ((doc "Writes _self to A_STREAM.\nReturns _self.")
         (cand (magik-completion-test--candidate-with-doc doc))
         (buf (magik-completion--doc-buffer cand)))
    (should (equal (with-current-buffer buf (buffer-string)) doc))))

(ert-deftest magik-completion--doc-buffer--multiline-doc-preserved ()
  "Multi-line documentation is stored verbatim."
  (let* ((doc "First line.\nSecond line.\nThird line.")
         (cand (magik-completion-test--candidate-with-doc doc))
         (buf (magik-completion--doc-buffer cand)))
    (should (equal (with-current-buffer buf (buffer-string)) doc))))

(ert-deftest magik-completion--doc-buffer--reuses-named-buffer ()
  "Successive calls reuse the same ` *magik-completion-doc*' buffer."
  (let* ((buf1 (magik-completion--doc-buffer
                (magik-completion-test--candidate-with-doc "first")))
         (buf2 (magik-completion--doc-buffer
                (magik-completion-test--candidate-with-doc "second"))))
    (should (eq buf1 buf2))
    (should (equal (with-current-buffer buf2 (buffer-string)) "second"))))

;;; Slot scanning

(defconst magik-completion-test--two-exemplars "def_slotted_exemplar(:other_thing,
	{
		{:other_slot, _unset}
	})
$

def_slotted_exemplar(:my_thing,
	{
		{:owner, _unset},
		{:size, 1}
	})
$

_method my_thing.compute()
	_return .
_endmethod
$
"
  "Magik source with two exemplar definitions and one method.")

(ert-deftest magik-completion--scan-slots--scoped-to-current-exemplar ()
  "Inside a method, only the slots of that method's exemplar are offered."
  (skip-unless (require 'magik-mode nil t))
  (with-temp-buffer
    (insert magik-completion-test--two-exemplars)
    (goto-char (point-min))
    (search-forward "_return .")
    (let ((slots (magik-completion--scan-slots)))
      (should (member "owner" slots))
      (should (member "size" slots))
      (should-not (member "other_slot" slots)))))

(ert-deftest magik-completion--scan-slots--falls-back-to-whole-buffer ()
  "Outside any method the whole buffer is scanned."
  (with-temp-buffer
    (insert "def_slotted_exemplar(:a_thing,\n\t{\n\t\t{:a_slot, _unset}\n\t})\n$\n")
    (goto-char (point-max))
    (should (member "a_slot" (magik-completion--scan-slots)))))

(ert-deftest magik-completion--exemplar-definition-region--not-found ()
  "Unknown or empty exemplar names yield no region."
  (with-temp-buffer
    (insert "def_slotted_exemplar(:a_thing, {})\n$\n")
    (should-not (magik-completion--exemplar-definition-region "b_thing"))
    (should-not (magik-completion--exemplar-definition-region ""))
    (should-not (magik-completion--exemplar-definition-region nil))))

;;; Package-qualified completion

(ert-deftest magik-completion--typed-package--qualified-prefix ()
  "A `package:name' prefix returns the package."
  (should (equal (magik-completion--typed-package "sw:pseu") "sw")))

(ert-deftest magik-completion--typed-package--leading-colon-is-not-a-package ()
  "A leading colon (symbol literal, e.g. \":p\") is not a package qualifier."
  (should-not (magik-completion--typed-package ":p")))

(ert-deftest magik-completion--typed-package--no-colon-returns-nil ()
  "A prefix with no colon has no package qualifier."
  (should-not (magik-completion--typed-package "pseu")))

(ert-deftest magik-completion-at-point-symbol--symbol-literal-offers-nothing ()
  "Typing a symbol literal like \":p\" offers no builtin completions."
  (with-temp-buffer
    (magik-mode)
    (insert ":p")
    (let ((capf (magik-completion-at-point-symbol)))
      (should-not (try-completion ":p" (nth 2 capf) nil)))))

(ert-deftest magik-completion-at-point-symbol--package-qualified-prefix-matches ()
  "Typing \"sw:rop\" still offers package-qualified builtin completions."
  (with-temp-buffer
    (magik-mode)
    (insert "sw:rop")
    (let ((capf (magik-completion-at-point-symbol)))
      (should (try-completion "sw:rop" (nth 2 capf) nil)))))

;;; Character-literal completion

(ert-deftest magik-completion-at-point-character--bare-percent-offers-full-list ()
  "A bare `%' offers the full character-name list immediately."
  (with-temp-buffer
    (magik-mode)
    (insert "%")
    (let ((capf (magik-completion-at-point-character)))
      (should capf)
      (should (equal (list (nth 0 capf) (nth 1 capf)) (list (point) (point))))
      (should (member "nul" (nth 2 capf)))
      (should (member "newline" (nth 2 capf)))
      (should (member "nbs" (nth 2 capf))))))

(ert-deftest magik-completion-at-point-character--narrows-to-matching-name ()
  "Typing \"%new\" narrows to \"newline\"."
  (with-temp-buffer
    (magik-mode)
    (insert "%new")
    (let ((capf (magik-completion-at-point-character)))
      (should (equal (try-completion "new" (nth 2 capf)) "newline")))))

(ert-deftest magik-completion-at-point-character--not-after-percent-offers-nothing ()
  "A plain identifier with no preceding `%' offers no character candidates."
  (with-temp-buffer
    (magik-mode)
    (insert "new")
    (should-not (magik-completion-at-point-character))))

(ert-deftest magik-completion-at-point-character--disabled-by-defcustom ()
  "No character candidates when `magik-completion-enable-characters' is nil."
  (with-temp-buffer
    (magik-mode)
    (insert "%new")
    (let ((magik-completion-enable-characters nil))
      (should-not (magik-completion-at-point-character)))))

(ert-deftest magik-completion-at-point-character--string-offers-nothing ()
  "A `%name' inside a string literal offers no character candidates."
  (with-temp-buffer
    (magik-mode)
    (insert "\"text %new")
    (should-not (magik-completion-at-point-character))))

(ert-deftest magik-completion-at-point-character--comment-offers-nothing ()
  "A `%name' inside a comment offers no character candidates."
  (with-temp-buffer
    (magik-mode)
    (insert "# %new")
    (should-not (magik-completion-at-point-character))))

(ert-deftest magik-completion-at-point-symbol--percent-context-offers-nothing ()
  "Typing after `%' offers no keyword/builtin/variable/snippet candidates."
  (with-temp-buffer
    (magik-mode)
    (insert "%new")
    (should-not (magik-completion-at-point-symbol))))

(ert-deftest magik-completion-at-point-symbol--percent-underscore-offers-nothing ()
  "Typing \"%_self\" offers no keyword candidates."
  (with-temp-buffer
    (magik-mode)
    (insert "%_self")
    (should-not (magik-completion-at-point-symbol))))

(ert-deftest magik-completion--character-bounds--percent-at-buffer-start ()
  "A `%' as the very first character of the buffer still yields bounds."
  (with-temp-buffer
    (magik-mode)
    (insert "%")
    (should (equal (magik-completion--character-bounds) (cons (point) (point))))))

(ert-deftest magik-completion--character-bounds--not-after-percent-returns-nil ()
  "No bounds are returned without a preceding `%'."
  (with-temp-buffer
    (magik-mode)
    (insert "new")
    (should-not (magik-completion--character-bounds))))

(ert-deftest magik-completion-at-point-symbol--backspace-after-percent-falls-back ()
  "Removing the `%' cleanly falls back to ordinary symbol completion."
  (with-temp-buffer
    (magik-mode)
    (insert "%new")
    (should-not (magik-completion-at-point-symbol))
    (delete-char -4)
    (insert "new")
    (should (magik-completion-at-point-symbol))))

(ert-deftest magik-completion-at-point-symbol--at-context-offers-nothing ()
  "Typing after `@' offers no keyword/builtin/variable/snippet candidates."
  (with-temp-buffer
    (magik-mode)
    (insert "@new")
    (should-not (magik-completion-at-point-symbol))))

(ert-deftest magik-completion-at-point-symbol--backspace-after-at-falls-back ()
  "Removing the `@' cleanly falls back to ordinary symbol completion."
  (with-temp-buffer
    (magik-mode)
    (insert "@new")
    (should-not (magik-completion-at-point-symbol))
    (delete-char -4)
    (insert "new")
    (should (magik-completion-at-point-symbol))))

;;; Yasnippet template candidates

(defmacro magik-completion-test--with-snippet-buffer (&rest body)
  "Eval BODY in a temp buffer with a test yasnippet defined and enabled.
Skips the test when yasnippet is unavailable."
  (declare (indent 0))
  `(progn
     (skip-unless (require 'yasnippet nil t))
     (with-temp-buffer
       (yas-define-snippets 'fundamental-mode
                            '(("mkey" "left ${1:x} right" "test snippet")))
       (yas-minor-mode 1)
       ,@body)))

(ert-deftest magik-completion-at-point-symbol--offers-snippet-keys ()
  "Snippet keys matching the prefix are offered as candidates."
  (magik-completion-test--with-snippet-buffer
    (insert "mk")
    (let ((capf (magik-completion-at-point-symbol)))
      (should capf)
      (should (try-completion "mkey" (nth 2 capf))))))

(ert-deftest magik-completion-at-point-symbol--snippets-disabled-by-defcustom ()
  "No snippet candidates when `magik-completion-enable-snippets' is nil."
  (magik-completion-test--with-snippet-buffer
    (insert "mk")
    (let ((magik-completion-enable-snippets nil)
          (magik-completion-enable-keywords nil)
          (magik-completion-enable-variables nil)
          (magik-completion-enable-cb nil))
      (should-not (magik-completion-at-point-symbol)))))

(ert-deftest magik-completion--snippet-exit-function--expands-template ()
  "Completing a snippet key replaces it with the expanded template."
  (magik-completion-test--with-snippet-buffer
    (insert "mkey")
    (magik-completion--snippet-exit-function "mkey" 'finished)
    (should (string-match-p "\\`left .* right\\'" (buffer-string)))
    (should-not (string-match-p "mkey" (buffer-string)))))

(ert-deftest magik-completion--snippet-exit-function--ignores-non-finished ()
  "No expansion happens unless completion finished."
  (magik-completion-test--with-snippet-buffer
    (insert "mkey")
    (magik-completion--snippet-exit-function "mkey" 'exact)
    (should (equal (buffer-string) "mkey"))))

;;; Session input-area gating

(defvar magik-session-prompt)

(ert-deftest magik-completion--session-input-p--after-prompt ()
  "Point in the command input area counts as typeable."
  (let ((magik-session-prompt "Magik> "))
    (with-temp-buffer
      (insert "loading...\nMagik> hello")
      (should (magik-completion--session-input-p)))))

(ert-deftest magik-completion--session-input-p--in-scrollback ()
  "Point inside session output is not typeable."
  (let ((magik-session-prompt "Magik> "))
    (with-temp-buffer
      (insert "loading...\nMagik> hello")
      (goto-char (point-min))
      (should-not (magik-completion--session-input-p)))))

(ert-deftest magik-completion--session-input-p--no-prompt-yet ()
  "Without a prompt in the buffer there is no typeable area."
  (let ((magik-session-prompt "Magik> "))
    (with-temp-buffer
      (insert "starting session...")
      (should-not (magik-completion--session-input-p)))))

(ert-deftest magik-completion--available-p--non-session-buffer ()
  "Completion is available in ordinary buffers regardless of prompts."
  (with-temp-buffer
    (insert "some code")
    (should (magik-completion--available-p))))

;;; magik-completion--ts-scan-variables

(defconst magik-completion-test--ts-method "_method my_thing.compute(a_stream, count, _optional flags, _gather rest)
	_local total << 0
	_local (lo, hi) << (0, 10)
	_constant limit << 5
	_import counter
	(x, y) << compute_pair()
	_for item, idx _over a_stream.elements()
	_loop
		total +<< item
	_endloop
	outcome
_endmethod
$
"
  "Magik source used by the tree-sitter variable scan tests.")

(defmacro magik-completion-test--with-ts-buffer (content search &rest body)
  "Eval BODY in a buffer with CONTENT parsed as Magik.
Point is placed at the end of the first occurrence of SEARCH.
Skips the test when the Magik tree-sitter grammar is unavailable."
  (declare (indent 2))
  `(progn
     (skip-unless (and (require 'treesit nil t)
                       (fboundp 'treesit-available-p)
                       (treesit-available-p)
                       (treesit-language-available-p 'magik)))
     (with-temp-buffer
       (insert ,content)
       (treesit-parser-create 'magik)
       (goto-char (point-min))
       (search-forward ,search)
       ,@body)))

(ert-deftest magik-completion--ts-scan-variables--method-parameters ()
  "Required, optional and gather parameters are all offered."
  (magik-completion-test--with-ts-buffer magik-completion-test--ts-method "outcome"
    (let ((vars (magik-completion--ts-scan-variables)))
      (dolist (param '("a_stream" "count" "flags" "rest"))
        (should (member param vars))))))

(ert-deftest magik-completion--ts-scan-variables--local-declarations ()
  "_local and _constant declarations are collected, including tuples."
  (magik-completion-test--with-ts-buffer magik-completion-test--ts-method "outcome"
    (let ((vars (magik-completion--ts-scan-variables)))
      (dolist (var '("total" "lo" "hi" "limit"))
        (should (member var vars))))))

(ert-deftest magik-completion--ts-scan-variables--assignments ()
  "Assigned variables are collected, including tuple assignment targets."
  (magik-completion-test--with-ts-buffer magik-completion-test--ts-method "outcome"
    (let ((vars (magik-completion--ts-scan-variables)))
      (should (member "x" vars))
      (should (member "y" vars)))))

(ert-deftest magik-completion--ts-scan-variables--loop-variables ()
  "_for loop variables are collected."
  (magik-completion-test--with-ts-buffer magik-completion-test--ts-method "outcome"
    (let ((vars (magik-completion--ts-scan-variables)))
      (should (member "item" vars))
      (should (member "idx" vars)))))

(ert-deftest magik-completion--ts-scan-variables--respects-point ()
  "Variables declared after point are not offered, parameters are."
  (magik-completion-test--with-ts-buffer magik-completion-test--ts-method
      "_local total << 0"
    (let ((vars (magik-completion--ts-scan-variables)))
      (should (member "a_stream" vars))
      (should (member "total" vars))
      (should-not (member "limit" vars))
      (should-not (member "item" vars)))))

(ert-deftest magik-completion--ts-scan-variables--inside-loop-body ()
  "Parameters and outer locals stay visible inside a loop body."
  (magik-completion-test--with-ts-buffer magik-completion-test--ts-method
      "total +<< item"
    (let ((vars (magik-completion--ts-scan-variables)))
      (dolist (var '("a_stream" "rest" "total" "lo" "item"))
        (should (member var vars))))))

(ert-deftest magik-completion--ts-scan-variables--import-variables ()
  "_import declarations are collected."
  (magik-completion-test--with-ts-buffer magik-completion-test--ts-method "outcome"
    (should (member "counter" (magik-completion--ts-scan-variables)))))

(ert-deftest magik-completion--regex-scan-variables--import-variables ()
  "_import declarations are collected by the regex fallback scan."
  (with-temp-buffer
    (insert magik-completion-test--ts-method)
    (goto-char (point-min))
    (search-forward "outcome")
    (let ((vars (magik-completion--regex-scan-variables)))
      (should (member "counter" vars))
      (should (member "a_stream" vars))
      (should (member "total" vars)))))

(ert-deftest magik-completion--ts-scan-variables--no-keywords-or-rhs ()
  "Keywords and right-hand sides of assignments are not offered."
  (magik-completion-test--with-ts-buffer magik-completion-test--ts-method "outcome"
    (let ((vars (magik-completion--ts-scan-variables)))
      (should-not (member "compute_pair" vars))
      (should-not (seq-some (lambda (v) (string-prefix-p "_" v)) vars)))))

;;; CB async query plumbing

(defmacro magik-completion-test--with-fake-cb (proc-var &rest body)
  "Bind PROC-VAR to a live `cat' process standing in for the CB.
Its buffer is wired up the way `magik-completion--cb-filter' expects,
and `magik-completion--ensure-cb-process' is stubbed to return it for
the duration of BODY.  Anything written to the fake process is echoed
back verbatim, which is what lets a test control fake CB responses by
choosing what it sends."
  (declare (indent 1))
  `(let* ((cb-buf (generate-new-buffer " *test-cb*"))
          (,proc-var (make-process :name "magik-completion-test-cb"
                                    :buffer cb-buf
                                    :command '("cat")
                                    :connection-type 'pipe
                                    :filter #'magik-completion--cb-filter)))
     (unwind-protect
         (cl-letf (((symbol-function 'magik-completion--ensure-cb-process)
                    (lambda () ,proc-var)))
           ,@body)
       (when (process-live-p ,proc-var) (delete-process ,proc-var))
       (when (buffer-live-p cb-buf) (kill-buffer cb-buf)))))

(defun magik-completion-test--wait-until (predicate proc &optional timeout)
  "Pump process/timer output until PREDICATE is non-nil or TIMEOUT
\(default 2\) seconds pass.  Returns PREDICATE's final value."
  (let ((deadline (+ (float-time) (or timeout 2))))
    (while (and (not (funcall predicate))
                (< (float-time) deadline))
      (if (process-live-p proc)
          (accept-process-output proc 0.05)
        (sit-for 0.05)))
    (funcall predicate)))

(ert-deftest magik-completion--cb-query-async--returns-before-cb-replies ()
  "Dispatching a query returns immediately; the result only reaches the
callback once the (fake) CB actually replies."
  (magik-completion-test--with-fake-cb proc
    (let (result)
      (should (magik-completion--cb-query-async
               "PING\n" (lambda (r) (setq result r))
               (lambda (str) (string-match-p "\n" str)) #'string-trim))
      (should-not result)
      (should (magik-completion-test--wait-until (lambda () result) proc))
      (should (equal result "PING")))))

(ert-deftest magik-completion--cb-query-async--queues-second-query-while-first-in-flight ()
  "A second query issued before the first resolves is queued and
serviced afterwards, rather than being sent concurrently (which would
desync the single-command-at-a-time CB connection) or silently
dropped."
  (magik-completion-test--with-fake-cb proc
    (let (result-a result-b)
      (should (magik-completion--cb-query-async
               "AAA\n" (lambda (r) (setq result-a r))
               (lambda (str) (string-match-p "\n" str)) #'string-trim))
      (should (magik-completion--cb-query-async
               "BBB\n" (lambda (r) (setq result-b r))
               (lambda (str) (string-match-p "\n" str)) #'string-trim))
      (should (magik-completion-test--wait-until
               (lambda () (and result-a result-b)) proc))
      (should (equal result-a "AAA"))
      (should (equal result-b "BBB")))))

(ert-deftest magik-completion--cb-query-async--timeout-delivers-nil-and-restarts ()
  "A CB that never replies delivers nil once `magik-completion-cb-timeout'
elapses, instead of hanging forever, and its connection is torn down
so stale output can't corrupt a later query."
  (let ((magik-completion-cb-timeout 0.1))
    (magik-completion-test--with-fake-cb proc
      (let ((result 'never-called))
        (should (magik-completion--cb-query-async
                 "SILENCE" (lambda (r) (setq result r))
                 (lambda (_str) nil) #'identity))
        (should (magik-completion-test--wait-until
                 (lambda () (not (eq result 'never-called))) proc))
        (should-not result)
        (should-not (process-live-p proc))))))

(ert-deftest magik-completion--cb-cached-fetch--dispatches-once-then-caches ()
  "The first call marks a fetch pending and dispatches it; a call made
before that fetch resolves must not dispatch again; once the fetch's
callback delivers a result, the cache is populated and later calls
return it directly, still without re-dispatching."
  (let ((cache-var (make-symbol "cache"))
        (loaded-var (make-symbol "loaded"))
        (pending-var (make-symbol "pending"))
        (dispatch-count 0)
        (delivered-callback nil))
    (set cache-var nil)
    (set loaded-var nil)
    (set pending-var nil)
    (cl-letf (((symbol-function 'magik-completion--cb-query-async)
               (lambda (_command callback &rest _)
                 (setq dispatch-count (1+ dispatch-count)
                       delivered-callback callback)
                 t)))
      (should-not (magik-completion--cb-cached-fetch
                   cache-var loaded-var pending-var "CMD\n"))
      (should (symbol-value pending-var))
      (should-not (symbol-value loaded-var))
      (should-not (magik-completion--cb-cached-fetch
                   cache-var loaded-var pending-var "CMD\n"))
      (should (= dispatch-count 1))
      (funcall delivered-callback '("a" "b"))
      (should-not (symbol-value pending-var))
      (should (symbol-value loaded-var))
      (should (equal (symbol-value cache-var) '("a" "b")))
      (should (equal (magik-completion--cb-cached-fetch
                      cache-var loaded-var pending-var "CMD\n")
                      '("a" "b")))
      (should (= dispatch-count 1)))))

;;; magik-completion--gis-session-idle-p

(defmacro magik-completion-test--with-session-process (mark-pos &rest body)
  "Eval BODY with a live process attached to the current buffer,
its `process-mark' set to MARK-POS."
  (declare (indent 1))
  `(let ((proc (make-process :name "magik-completion-test-session"
                              :buffer (current-buffer)
                              :command '("cat")
                              :connection-type 'pipe
                              :noquery t)))
     (unwind-protect
         (progn
           (set-marker (process-mark proc) ,mark-pos)
           ,@body)
       (when (process-live-p proc) (delete-process proc)))))

(ert-deftest magik-completion--gis-session-idle-p--at-prompt-is-idle ()
  "A session buffer ending at a fresh prompt is idle."
  (with-temp-buffer
    (setq-local magik-session-prompt (regexp-opt '("Magik> ")))
    (insert "some output\nMagik> ")
    (magik-completion-test--with-session-process (point-max)
      (should (magik-completion--gis-session-idle-p (current-buffer))))))

(ert-deftest magik-completion--gis-session-idle-p--mid-output-is-busy ()
  "A session buffer whose last line isn't a prompt is busy."
  (with-temp-buffer
    (setq-local magik-session-prompt (regexp-opt '("Magik> ")))
    (insert "Magik> some_long_running_command()\nstill working...\n")
    (magik-completion-test--with-session-process (point-max)
      (should-not (magik-completion--gis-session-idle-p (current-buffer))))))

(ert-deftest magik-completion--gis-session-idle-p--no-prompt-seen-yet-is-busy ()
  "A session buffer that has never shown a prompt (e.g. still starting
up) is busy."
  (with-temp-buffer
    (setq-local magik-session-prompt (regexp-opt '("Magik> ")))
    (insert "opening database...\n")
    (magik-completion-test--with-session-process (point-max)
      (should-not (magik-completion--gis-session-idle-p (current-buffer))))))

(ert-deftest magik-completion--gis-session-idle-p--unsent-input-after-prompt-is-still-idle ()
  "Regression test: text the user has typed but not yet submitted,
sitting after the process mark, must not be mistaken for the session
being busy -- this is exactly the case when completion is requested
from within the session buffer itself."
  (with-temp-buffer
    (setq-local magik-session-prompt (regexp-opt '("Magik> ")))
    (insert "some output\nMagik> ")
    (magik-completion-test--with-session-process (point-max)
      (insert "pseudo_area.new_fo")
      (should (magik-completion--gis-session-idle-p (current-buffer))))))

;;; Pending-flag reset when no CB is available

(ert-deftest magik-completion--cb-cached-fetch--no-cb-available-clears-pending ()
  "When `magik-completion--cb-query-async' can't dispatch at all (e.g.
the session is busy, so `magik-completion--ensure-cb-process' refuses
to start a connection), the pending flag must not get stuck: a later
call has to be able to retry once the session is free again."
  (let ((cache-var (make-symbol "cache"))
        (loaded-var (make-symbol "loaded"))
        (pending-var (make-symbol "pending")))
    (set cache-var nil)
    (set loaded-var nil)
    (set pending-var nil)
    (cl-letf (((symbol-function 'magik-completion--cb-query-async)
               (lambda (&rest _) nil)))
      (should-not (magik-completion--cb-cached-fetch
                   cache-var loaded-var pending-var "CMD\n"))
      (should-not (symbol-value pending-var))
      ;; A later call (session free again) must be able to try once more.
      (should-not (magik-completion--cb-cached-fetch
                   cache-var loaded-var pending-var "CMD\n"))
      (should-not (symbol-value pending-var)))))

;;; Session-restart cleanup

(ert-deftest magik-completion--reset-session-state--clears-buffer-local-cb-state ()
  "Resetting session state clears a buffer's dedicated CB connection,
so a stale process handle can't be reused after a restart."
  (magik-completion-test--with-fake-cb proc
    (with-temp-buffer
      (magik-completion-mode 1)
      (setq magik-completion--cb-process proc
            magik-completion--cb-buffer-name (buffer-name (process-buffer proc))
            magik-completion--cb-candidates '("stale")
            magik-completion--cb-filter-str "partial"
            magik-completion--cb-ready-p #'ignore
            magik-completion--cb-parse-fn #'identity
            magik-completion--cb-on-response #'ignore
            magik-completion--cb-queue '(("CMD\n" nil nil ignore)))
      (magik-completion--reset-session-state)
      (should-not magik-completion--cb-process)
      (should-not magik-completion--cb-buffer-name)
      (should-not magik-completion--cb-candidates)
      (should (equal magik-completion--cb-filter-str ""))
      (should-not magik-completion--cb-ready-p)
      (should-not magik-completion--cb-parse-fn)
      (should-not magik-completion--cb-on-response)
      (should-not magik-completion--cb-queue))))

(ert-deftest magik-completion--reset-session-state--ignores-buffers-without-completion-mode ()
  "Buffers without `magik-completion-mode' enabled are left untouched."
  (magik-completion-test--with-fake-cb proc
    (with-temp-buffer
      (setq magik-completion--cb-process proc)
      (magik-completion--reset-session-state)
      (should (eq magik-completion--cb-process proc)))))

(ert-deftest magik-completion--reset-session-state--still-kills-named-cb-buffers ()
  "The pre-existing kill-by-name-pattern behavior for dedicated CB
subprocess buffers still holds alongside the new per-buffer reset."
  (magik-completion-test--with-fake-cb proc
    (with-current-buffer (process-buffer proc)
      (rename-buffer " *cb*fake.magik*completion*"))
    (magik-completion--reset-session-state)
    (should-not (get-buffer " *cb*fake.magik*completion*"))))

;;; global-magik-completion-mode

(defmacro magik-completion-test--with-global-mode (&rest body)
  "Eval BODY with `global-magik-completion-mode' enabled.
Always disables it again afterwards, regardless of errors, so a
failing assertion here cannot leak the global toggle into unrelated
tests that run later in the same Emacs process."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (global-magik-completion-mode 1)
         ,@body)
     (global-magik-completion-mode -1)))

(ert-deftest global-magik-completion-mode--enables-in-existing-magik-mode-buffer ()
  "Enabling the global mode turns on the local mode in a Magik buffer
that already exists at the time it's enabled."
  (with-temp-buffer
    (magik-mode)
    (magik-completion-test--with-global-mode
      (should magik-completion-mode))))

(ert-deftest global-magik-completion-mode--enables-in-new-magik-mode-buffer ()
  "A Magik buffer created after the global mode is already on gets the
local mode turned on automatically."
  (magik-completion-test--with-global-mode
    (with-temp-buffer
      (magik-mode)
      (should magik-completion-mode))))

(ert-deftest global-magik-completion-mode--enables-in-magik-session-mode-buffer ()
  "The global mode also turns on the local mode in Magik session buffers."
  (magik-completion-test--with-global-mode
    (with-temp-buffer
      (magik-session-mode)
      (should magik-completion-mode))))

(ert-deftest global-magik-completion-mode--does-not-enable-in-unrelated-buffer ()
  "Buffers whose major mode is unrelated to Magik are left alone."
  (magik-completion-test--with-global-mode
    (with-temp-buffer
      (fundamental-mode)
      (should-not magik-completion-mode))))

(ert-deftest global-magik-completion-mode--disabling-turns-off-local-mode ()
  "Disabling the global mode turns the local mode back off in a buffer
where it was enabled through it."
  (with-temp-buffer
    (magik-mode)
    (unwind-protect
        (progn
          (global-magik-completion-mode 1)
          (should magik-completion-mode)
          (global-magik-completion-mode -1)
          (should-not magik-completion-mode))
      (global-magik-completion-mode -1))))

(provide 'magik-completion-test)
;;; magik-completion-test.el ends here
