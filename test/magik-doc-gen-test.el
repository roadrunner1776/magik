;;; magik-doc-gen-test.el --- Tests for magik-doc-gen.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests covering documentation generation in `magik-doc-gen.el'.

;;; Code:

(require 'test-helper)
(require 'magik-mode)
(require 'magik-doc-gen)

;;; magik--write-sw-method-doc

(ert-deftest magik--write-sw-method-doc--inserts-params-when-no-doc ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.bar(a, b)\n")
    (insert "  >> a + b\n")
    (insert "_endmethod\n")
    ;; missing-parameters, starting-point (line), documentation-found
    (magik--write-sw-method-doc '("a" "b") 2 0)
    (let ((text (buffer-string)))
      (should (string-match-p "##\n" text))
      (should (string-match-p "## A" text))
      (should (string-match-p "## B" text)))))

(ert-deftest magik--write-sw-method-doc--appends-to-existing-doc ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.bar(a, b)\n")
    (insert "\t## Existing doc\n")
    (insert "\t## A\n")
    (insert "  >> a + b\n")
    (insert "_endmethod\n")
    ;; Only b is missing, doc starts at line 2, 2 lines of doc exist
    (magik--write-sw-method-doc '("b") 4 2)
    (let ((text (buffer-string)))
      (should (string-match-p "## B" text)))))

;;; magik--write-method-type-doc

(ert-deftest magik--write-method-type-doc--inserts-param-and-return ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.bar(x)\n")
    (insert "  >> x * 2\n")
    (insert "_endmethod\n")
    (magik--write-method-type-doc '("x") 2 0 t)
    (let ((text (buffer-string)))
      (should (string-match-p "@param {:} x" text))
      (should (string-match-p "@return {:}" text)))))

(ert-deftest magik--write-method-type-doc--skips-return-when-told ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.bar(x)\n")
    (insert "  x.do_something()\n")
    (insert "_endmethod\n")
    (magik--write-method-type-doc '("x") 2 0 nil)
    (let ((text (buffer-string)))
      (should (string-match-p "@param {:} x" text))
      (should-not (string-match-p "@return" text)))))

;;; magik-single-method-sw-method-doc (integration)

(ert-deftest magik-single-method-sw-method-doc--adds-missing-params ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method rope.new(size, fill)\n")
    (insert "\t##\n")
    (insert "\t>> _clone.init(size, fill)\n")
    (insert "_endmethod\n")
    (insert "$\n")
    (goto-char (point-min))
    (forward-line 2)
    (magik-single-method-sw-method-doc)
    (let ((text (buffer-string)))
      (should (string-match-p "## SIZE" text))
      (should (string-match-p "## FILL" text)))))

(ert-deftest magik-single-method-sw-method-doc--skips-documented-params ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method rope.new(size, fill)\n")
    (insert "\t##\n")
    (insert "\t## SIZE - the initial size\n")
    (insert "\t##\n")
    (insert "\t>> _clone.init(size, fill)\n")
    (insert "_endmethod\n")
    (insert "$\n")
    (goto-char (point-min))
    (forward-line 3)
    (magik-single-method-sw-method-doc)
    (let ((text (buffer-string)))
      ;; SIZE was already documented, only FILL should be added
      (should (string-match-p "## FILL" text))
      ;; SIZE should appear only once (the existing one)
      (should (= 1 (cl-count-if
                     (lambda (line) (string-match-p "## SIZE" line))
                     (split-string text "\n")))))))

;;; magik-single-method-type-doc (integration)

(ert-deftest magik-single-method-type-doc--adds-param-annotations ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method foo.calc(a, b)\n")
    (insert "\t##\n")
    (insert "\t>> a + b\n")
    (insert "_endmethod\n")
    (insert "$\n")
    (goto-char (point-min))
    (forward-line 2)
    (magik-single-method-type-doc)
    (let ((text (buffer-string)))
      (should (string-match-p "@param {:} a" text))
      (should (string-match-p "@param {:} b" text))
      (should (string-match-p "@return {:}" text)))))

;;; magik--slotted-exemplar-slots

(ert-deftest magik--slotted-exemplar-slots--finds-slots ()
  (with-temp-buffer
    (magik-mode)
    (insert "def_slotted_exemplar(:my_obj,\n")
    (insert "  {\n")
    (insert "    {:slot_a, _unset},\n")
    (insert "    {:slot_b, _unset}\n")
    (insert "  }, {})\n")
    (insert "$\n")
    (goto-char (point-min))
    (let ((slots (magik--slotted-exemplar-slots (point-min))))
      (should (member "slot_a" slots))
      (should (member "slot_b" slots)))))

(ert-deftest magik--slotted-exemplar-slots--returns-nil-when-no-slots ()
  (with-temp-buffer
    (magik-mode)
    (insert "def_slotted_exemplar(:my_obj,\n")
    (insert "  {}, {})\n")
    (insert "$\n")
    (goto-char (point-min))
    (let ((slots (magik--slotted-exemplar-slots (point-min))))
      (should (null slots)))))

(provide 'magik-doc-gen-test)
;;; magik-doc-gen-test.el ends here
