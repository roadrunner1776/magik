;;; magik-completion-test.el --- Tests for magik-completion.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `magik-completion.el', covering snippet building and
;; args-line parsing with the various `magik-completion-insert-*' options.

;;; Code:

(require 'test-helper)
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
error in Magik — so we fall back to empty parens rather than a misleading
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

(provide 'magik-completion-test)
;;; magik-completion-test.el ends here
