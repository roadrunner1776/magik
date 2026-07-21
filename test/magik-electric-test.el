;;; magik-electric-test.el --- Tests for magik-electric.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the electric template system in `magik-electric.el'.

;;; Code:

(require 'test-helper)
(require 'magik-mode)
(require 'magik-indent)
(require 'magik-pragma)
(require 'magik-electric)

;;; Template lookup

(ert-deftest magik-electric--template-alist-has-method ()
  (should (assoc "method" magik-electric-templates)))

(ert-deftest magik-electric--template-alist-has-if ()
  (should (assoc "if" magik-electric-templates)))

(ert-deftest magik-electric--template-alist-has-proc ()
  (should (assoc "proc" magik-electric-templates)))

(ert-deftest magik-electric--template-alist-has-def-slotted-exemplar ()
  (should (assoc "def_slotted_exemplar" magik-electric-templates)))

;;; magik-electric-insert-template-line (unit pieces)

(ert-deftest magik-electric-insert-template-line--inserts-plain-string ()
  (with-temp-buffer
    (magik-mode)
    (magik-electric-insert-template-line "test" "_if " 0)
    (should (string-match-p "_if " (buffer-string)))))

(ert-deftest magik-electric-insert-template-line--inserts-dollar ()
  (with-temp-buffer
    (magik-mode)
    (magik-electric-insert-template-line "test" 'dollar 0)
    (should (string-match-p "\\$" (buffer-string)))))

(ert-deftest magik-electric-insert-template-line--indents-tab-prefixed ()
  (with-temp-buffer
    (magik-mode)
    (let ((magik-indent-level 8)
          (indent-tabs-mode nil))
      (magik-electric-insert-template-line "test" "\tbody" 4)
      ;; Should be indented to col + magik-indent-level = 12
      (should (string-match-p "^            body" (buffer-string))))))

(ert-deftest magik-electric-insert-template-line--prev-class-name-from-method ()
  (with-temp-buffer
    (magik-mode)
    (insert "_method rope.size\n")
    (insert "  body\n")
    (insert "_endmethod\n")
    ;; Position at end; template line should find "rope"
    (magik-electric-insert-template-line "test" 'prev_class_name 0)
    (should (string-match-p "rope\\." (buffer-string)))))

(ert-deftest magik-electric-insert-template-line--prev-class-name-from-def-slotted ()
  (with-temp-buffer
    (magik-mode)
    (insert "def_slotted_exemplar(:my_class,\n")
    (insert "  {},{})\n")
    (insert "$\n")
    ;; Position at end
    (magik-electric-insert-template-line "test" 'prev_class_name 0)
    (should (string-match-p "my_class\\." (buffer-string)))))

(ert-deftest magik-electric-insert-template-line--prev-pragma-copies ()
  (with-temp-buffer
    (magik-mode)
    (insert "_pragma(classify_level=basic, topic={rope}, usage={subclassable})\n")
    (insert "_method rope.size\n")
    (goto-char (point-max))
    (magik-electric-insert-template-line "test" '(prev_pragma) 0)
    (should (string-match-p "_pragma(classify_level=basic" (buffer-string)))))

(ert-deftest magik-electric-insert-template-line--prev-pragma-uses-default ()
  (with-temp-buffer
    (magik-mode)
    ;; No prior pragma in buffer
    (insert "_method rope.size\n")
    (goto-char (point-max))
    (let ((magik-electric-default-pragma "_pragma(classify_level=, topic={}, usage={})"))
      (magik-electric-insert-template-line "test" '(prev_pragma) 0)
      (should (string-match-p "_pragma(classify_level=" (buffer-string))))))

;;; magik-electric-insert-template (full template expansion)

(ert-deftest magik-electric-insert-template--expands-if ()
  (with-temp-buffer
    (magik-mode)
    (insert "if")
    (magik-electric-insert-template "if")
    (let ((text (buffer-string)))
      (should (string-match-p "_if" text))
      (should (string-match-p "_then" text))
      (should (string-match-p "_endif" text)))))

(ert-deftest magik-electric-insert-template--expands-proc ()
  (with-temp-buffer
    (magik-mode)
    (insert "proc")
    (magik-electric-insert-template "proc")
    (let ((text (buffer-string)))
      (should (string-match-p "_proc()" text))
      (should (string-match-p "_endproc" text)))))

(ert-deftest magik-electric-insert-template--expands-loop ()
  (with-temp-buffer
    (magik-mode)
    (insert "loop")
    (magik-electric-insert-template "loop")
    (let ((text (buffer-string)))
      (should (string-match-p "_loop" text))
      (should (string-match-p "_endloop" text)))))

(ert-deftest magik-electric-insert-template--expands-protect ()
  (with-temp-buffer
    (magik-mode)
    (insert "protect")
    (magik-electric-insert-template "protect")
    (let ((text (buffer-string)))
      (should (string-match-p "_protect" text))
      (should (string-match-p "_protection" text))
      (should (string-match-p "_endprotect" text)))))

;;; magik-electric-hash (comment alignment)

(ert-deftest magik-electric-hash--aligns-with-previous-comment ()
  (with-temp-buffer
    (magik-mode)
    (insert "    # first comment\n")
    (insert "  ")
    ;; Simulate inserting # then calling alignment
    (let ((last-command-event ?#))
      (magik-electric-hash 1))
    ;; The # on line 2 should be aligned with the # on line 1 (column 4)
    (forward-line -1)  ;; we're on line 2 now after insertion
    (beginning-of-line)
    (back-to-indentation)
    (should (= (current-column) 4))))

(provide 'magik-electric-test)
;;; magik-electric-test.el ends here
