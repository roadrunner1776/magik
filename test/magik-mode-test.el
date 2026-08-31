;;; magik-mode-test.el --- Tests for magik-mode.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests covering pure helpers and buffer-local functions in
;; `magik-mode.el': method name parsing, imenu indexing, package
;; detection, mark-method navigation, and type conversion.

;;; Code:

(require 'test-helper)
(require 'magik-mode)

;;; magik-method-name-type

(ert-deftest magik-method-name-type--plain-method ()
  (should (equal (magik-method-name-type "run") '("run" . ""))))

(ert-deftest magik-method-name-type--assignment ()
  (should (equal (magik-method-name-type "name<<") '("name" . "<<"))))

(ert-deftest magik-method-name-type--boot-assignment ()
  ;; The ^ is considered part of the name; "<<" is the type suffix.
  ;; Boot assignment is only detected with the "()" prefix: "name()^<<"
  (should (equal (magik-method-name-type "name^<<") '("name^" . "<<"))))

(ert-deftest magik-method-name-type--parenthesised ()
  (should (equal (magik-method-name-type "run()") '("run" . "()"))))

(ert-deftest magik-method-name-type--parenthesised-assignment ()
  (should (equal (magik-method-name-type "run()<<") '("run" . "()<<"))))

(ert-deftest magik-method-name-type--parenthesised-boot-assignment ()
  (should (equal (magik-method-name-type "run()^<<") '("run" . "()^<<"))))

(ert-deftest magik-method-name-type--array-simple ()
  (should (equal (magik-method-name-type "[]") '(nil . "[]"))))

(ert-deftest magik-method-name-type--array-assignment ()
  (should (equal (magik-method-name-type "[]<<") '(nil . "[]<<"))))

(ert-deftest magik-method-name-type--array-comma ()
  (should (equal (magik-method-name-type "[,]") '(nil . "[,]"))))

;;; magik-function-convert

(ert-deftest magik-function-convert--unset ()
  (should (equal (magik-function-convert 'unset) "_unset")))

(ert-deftest magik-function-convert--false-symbol ()
  (should (equal (magik-function-convert 'false) "_false")))

(ert-deftest magik-function-convert--nil ()
  (should (equal (magik-function-convert nil) "_false")))

(ert-deftest magik-function-convert--true-symbol ()
  (should (equal (magik-function-convert 'true) "_true")))

(ert-deftest magik-function-convert--t ()
  (should (equal (magik-function-convert t) "_true")))

(ert-deftest magik-function-convert--string ()
  (should (equal (magik-function-convert "hello") "\"hello\"")))

(ert-deftest magik-function-convert--number ()
  (should (equal (magik-function-convert 42) "42")))

(ert-deftest magik-function-convert--symbol-to-magik-symbol ()
  (should (equal (magik-function-convert 'foo) ":foo")))

;;; magik-function

(ert-deftest magik-function--no-args ()
  (should (equal (magik-function "system.name") "system.name()\n")))

(ert-deftest magik-function--multiple-args ()
  (should (equal (magik-function "system.test" "file" 'unset 4)
                 "system.test(\"file\", _unset, 4)\n")))

;;; magik-current-package-name

(ert-deftest magik-current-package-name--finds-package ()
  (with-temp-buffer
    (magik-mode)
    (insert "_package user\n")
    (insert "_method foo.bar()\n")
    (insert "_endmethod\n")
    (goto-char (point-max))
    (should (equal (magik-current-package-name) "user"))))

(ert-deftest magik-current-package-name--defaults-to-sw ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.bar()\n")
    (insert "_endmethod\n")
    (goto-char (point-max))
    (should (equal (magik-current-package-name) "sw"))))

;;; magik-method-name-postfix

(ert-deftest magik-method-name-postfix--plain ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.bar\n")
    (goto-char (point-min))
    (search-forward "bar")
    (should (equal (magik-method-name-postfix) ""))))

(ert-deftest magik-method-name-postfix--parenthesised ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.bar(a, b)\n")
    (goto-char (point-min))
    (search-forward "bar")
    (should (equal (magik-method-name-postfix) "()"))))

(ert-deftest magik-method-name-postfix--assignment ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.bar << val\n")
    (goto-char (point-min))
    (search-forward "bar")
    (should (equal (magik-method-name-postfix) "<<"))))

(ert-deftest magik-method-name-postfix--boot-assignment ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.bar ^<< val\n")
    (goto-char (point-min))
    (search-forward "bar")
    (should (equal (magik-method-name-postfix) "^<<"))))

;;; magik-mark-method (navigation)

(ert-deftest magik-mark-method--marks-single-method ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.bar()\n")
    (insert "  write(1)\n")
    (insert "_endmethod\n")
    (goto-char (point-min))
    (forward-line 1)
    (magik-mark-method t)
    (let ((region (buffer-substring-no-properties (point) (mark))))
      (should (string-match-p "_method" region))
      (should (string-match-p "_endmethod" region)))))

;;; magik--in-string-or-comment-p

(ert-deftest magik--in-string-or-comment-p--not-in-string ()
  (with-temp-buffer
    (magik-mode)
    (insert "x << 1")
    (goto-char 3)
    (should-not (magik--in-string-or-comment-p))))

(ert-deftest magik--in-string-or-comment-p--in-comment ()
  (with-temp-buffer
    (magik-mode)
    (insert "x # comment here")
    (goto-char 8)
    (should (eq (magik--in-string-or-comment-p) 'comment))))

(ert-deftest magik--in-string-or-comment-p--in-string ()
  (with-temp-buffer
    (magik-mode)
    (insert "x << \"hello\"")
    (goto-char 9)
    (should (eq (magik--in-string-or-comment-p) 'string))))

;;; magik-imenu-create-index-function

(ert-deftest magik-imenu-create-index-function--indexes-methods ()
  (with-temp-buffer
    (magik-mode)
    (insert "_pragma(classify_level=basic)\n")
    (insert "_method rope.size\n")
    (insert "  >> _self.len\n")
    (insert "_endmethod\n")
    (insert "$\n")
    (insert "_pragma(classify_level=basic)\n")
    (insert "_method rope.new(init)\n")
    (insert "  >> _clone.init(init)\n")
    (insert "_endmethod\n")
    (insert "$\n")
    (let ((index (magik-imenu-create-index-function)))
      ;; Should find at least the two methods
      (should (>= (length index) 2))
      ;; Check that the method names are present somewhere in the index
      (let ((all-names (mapcar #'car (apply #'append
                                            (mapcar (lambda (x)
                                                      (if (listp (cdr x))
                                                          (cdr x)
                                                        (list x)))
                                                    index)))))
        (should (member "size" all-names))
        (should (member "new()" all-names))))))

;;; magik-text-encoding-face (font-lock)

(ert-deftest magik-text-encoding-face--spaced ()
  (with-temp-buffer
    (let ((font-lock-maximum-decoration t))
      (magik-mode)
      (insert "#% text_encoding = iso8859_1\n")
      (insert "_package user\n")
      (font-lock-ensure)
      (goto-char (point-min))
      (should (eq (get-text-property (point) 'face) 'magik-text-encoding-face)))))

(ert-deftest magik-text-encoding-face--no-spaces ()
  (with-temp-buffer
    (let ((font-lock-maximum-decoration t))
      (magik-mode)
      (insert "#%text_encoding=utf8\n")
      (insert "_package user\n")
      (font-lock-ensure)
      (goto-char (point-min))
      (should (eq (get-text-property (point) 'face) 'magik-text-encoding-face)))))

(ert-deftest magik-text-encoding-face--not-fontified-without-tag ()
  (with-temp-buffer
    (let ((font-lock-maximum-decoration t))
      (magik-mode)
      (insert "_package user\n")
      (font-lock-ensure)
      (goto-char (point-min))
      (should-not (eq (get-text-property (point) 'face) 'magik-text-encoding-face)))))

;;; outline-regexp

(ert-deftest magik-outline-regexp--matches-with-space ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "#% text_encoding = iso8859_1"))))

(ert-deftest magik-outline-regexp--matches-without-space ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "#%text_encoding=utf8"))))

;; _method variants

(ert-deftest magik-outline-regexp--matches-method-definition ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "_method foo.bar()"))))

(ert-deftest magik-outline-regexp--matches-plain-method ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "_method some_engine.some_method_name()"))))

(ert-deftest magik-outline-regexp--matches-private-method ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "_private _method my_exemplar.some_slot"))))

(ert-deftest magik-outline-regexp--matches-abstract-private-iter-method ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "_abstract _private _iter _method foo.bar()"))))

;; .def_property / .add_child

(ert-deftest magik-outline-regexp--matches-def-property ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "my_exemplar.def_property( :foo, :bar )"))))

(ert-deftest magik-outline-regexp--matches-add-child ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "a_tree.add_child( a_child )"))))

;; .define_* variants

(ert-deftest magik-outline-regexp--matches-define-shared-variable ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p
             outline-regexp
             "my_dialog.define_shared_variable(:active?,  _true, :public)"))))

(ert-deftest magik-outline-regexp--matches-define-shared-constant ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p
             outline-regexp
             "my_dialog.define_shared_constant( :help_id, 60560, :public )"))))

(ert-deftest magik-outline-regexp--matches-define-slot-access ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p
             outline-regexp
             "my_exemplar.define_slot_access(:some_slot, :writable)"))))

(ert-deftest magik-outline-regexp--matches-define-slot-externally-readable ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p
             outline-regexp
             "my_exemplar.define_slot_externally_readable(:some_slot)"))))

(ert-deftest magik-outline-regexp--matches-define-slot-externally-writable ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p
             outline-regexp
             "my_exemplar.define_slot_externally_writable(:log?)"))))

(ert-deftest magik-outline-regexp--matches-define-property ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p
             outline-regexp
             "my_exemplar.define_property( :some_property, _unset)"))))

(ert-deftest magik-outline-regexp--matches-define-interface ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p
             outline-regexp
             "my_exemplar.define_interface(:exported_properties, _unset)"))))

(ert-deftest magik-outline-regexp--matches-define-method-signature ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p
             outline-regexp
             "my_exemplar.define_method_signature( :some_method|()|, _unset)"))))

;; def_slotted_exemplar / def_indexed_exemplar / def_mixin, _global,
;; read_message_patch / read_translator_patch

(ert-deftest magik-outline-regexp--matches-def-slotted-exemplar ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p
             outline-regexp
             "def_slotted_exemplar(:my_exemplar, {{:some_slot, _unset}}, {:some_mixin})"))))

(ert-deftest magik-outline-regexp--matches-def-mixin ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "def_mixin(:my_mixin)"))))

(ert-deftest magik-outline-regexp--matches-global ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "_global my_global << _proc @my_global( some_arg )"))))

(ert-deftest magik-outline-regexp--matches-read-message-patch ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "read_message_patch(:my_application)"))))

;; "#>" doc-comment heading markers.  A single "#" plus one or more ">"
;; denotes increasingly nested heading levels, mirroring the "*", "**",
;; "***" convention of Outline mode.

(ert-deftest magik-outline-regexp--matches-single-level-doc-marker ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "#> Heading"))))

(ert-deftest magik-outline-regexp--matches-double-gt-doc-marker ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "#>> _self.some_method()"))))

(ert-deftest magik-outline-regexp--matches-double-hash-double-gt-doc-marker ()
  (with-temp-buffer
    (magik-mode)
    (should (string-match-p outline-regexp "##>> _self.some_other_method(arg,"))))

;; Negative cases

(ert-deftest magik-outline-regexp--does-not-match-plain-code ()
  (with-temp-buffer
    (magik-mode)
    (should-not (string-match-p outline-regexp "a << b + c"))))

(ert-deftest magik-outline-regexp--define-branch-dot-is-a-literal-dot ()
  "The \".*\\.\" before \"def_property\"/\"define_*\" must require a real
literal \".\" separator, not match on an arbitrary character in its place."
  (with-temp-buffer
    (magik-mode)
    (should-not (string-match-p outline-regexp "obj?def_property(:foo)"))
    (should (string-match-p outline-regexp "obj.def_property(:foo)"))))

(provide 'magik-mode-test)
;;; magik-mode-test.el ends here
