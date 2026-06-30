;;; magik-loadlist-test.el --- Tests for magik-loadlist.el and magik-msg.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests covering loadlist parsing and message file navigation.

;;; Code:

(require 'test-helper)
(require 'magik-loadlist)
(require 'magik-msg)

;;; magik-loadlist-file-data

(ert-deftest magik-loadlist-file-data--lowercases-name ()
  (let ((result (magik-loadlist-file-data "My_File")))
    (should (equal (car result) "my_file"))
    ;; Second element stores original if case differs
    (should (equal (cadr result) "My_File"))))

(ert-deftest magik-loadlist-file-data--nil-when-same-case ()
  (let ((result (magik-loadlist-file-data "my_file")))
    (should (equal (car result) "my_file"))
    (should (null (cadr result)))))

(ert-deftest magik-loadlist-file-data--optional-data ()
  (let ((result (magik-loadlist-file-data "foo" 42)))
    (should (= (caddr result) 42))))

;;; magik-loadlist-ignore

(ert-deftest magik-loadlist-ignore--matches-default-patterns ()
  (let ((magik-loadlist-ignore-regexp-list '(".*\\.o" "load_list\\.txt")))
    (should (magik-loadlist-ignore "foo.o"))
    (should (magik-loadlist-ignore "load_list.txt"))
    (should-not (magik-loadlist-ignore "my_module.magik"))))

(ert-deftest magik-loadlist-ignore--empty-list-ignores-nothing ()
  (let ((magik-loadlist-ignore-regexp-list nil))
    (should-not (magik-loadlist-ignore "anything.txt"))))

;;; magik-loadlist-buffer-list

(ert-deftest magik-loadlist-buffer-list--parses-simple-list ()
  (with-temp-buffer
    (insert "module_a\n")
    (insert "module_b\n")
    (let ((result (magik-loadlist-buffer-list)))
      (should (= 2 (length result)))
      (should (equal (caar result) "module_b"))
      (should (equal (caadr result) "module_a")))))

(ert-deftest magik-loadlist-buffer-list--skips-comments ()
  (with-temp-buffer
    (insert "# This is a comment\n")
    (insert "module_a\n")
    (insert "# Another comment\n")
    (insert "module_b\n")
    (let ((result (magik-loadlist-buffer-list)))
      (should (= 2 (length result))))))

(ert-deftest magik-loadlist-buffer-list--strips-magik-extension ()
  (with-temp-buffer
    (insert "my_module.magik\n")
    (let ((result (magik-loadlist-buffer-list)))
      (should (equal (caar result) "my_module")))))

(ert-deftest magik-loadlist-buffer-list--strips-inline-comments ()
  (with-temp-buffer
    (insert "module_a # inline comment\n")
    (let ((result (magik-loadlist-buffer-list)))
      (should (equal (caar result) "module_a")))))

(ert-deftest magik-loadlist-buffer-list--trailing-backslash-preserved ()
  "Trailing backslash is preserved (the eq/substring check is a no-op)."
  (with-temp-buffer
    (insert "subdir\\\n")
    (let ((result (magik-loadlist-buffer-list)))
      (should (equal (caar result) "subdir\\")))))

(ert-deftest magik-loadlist-buffer-list--skips-blank-lines ()
  (with-temp-buffer
    (insert "\n")
    (insert "   \n")
    (insert "module_a\n")
    (let ((result (magik-loadlist-buffer-list)))
      (should (= 1 (length result))))))

;;; magik-msg navigation

(ert-deftest magik-msg-forward-message--finds-next ()
  (with-temp-buffer
    (magik-msg-mode)
    (insert ":msg_one\n")
    (insert "First message text\n")
    (insert "\n")
    (insert ":msg_two\n")
    (insert "Second message text\n")
    (goto-char (point-min))
    (magik-msg-forward-message)
    (should (looking-at ":msg_two"))))

(ert-deftest magik-msg-backward-message--finds-previous ()
  (with-temp-buffer
    (magik-msg-mode)
    (insert ":msg_one\n")
    (insert "First message text\n")
    (insert "\n")
    (insert ":msg_two\n")
    (insert "Second message text\n")
    (goto-char (point-max))
    (magik-msg-backward-message)
    (should (looking-at ":msg_two"))))

(ert-deftest magik-msg-forward-message--stays-at-end-if-no-next ()
  (with-temp-buffer
    (magik-msg-mode)
    (insert ":msg_one\n")
    (insert "Only message\n")
    (goto-char (point-min))
    (magik-msg-forward-message)
    ;; Should be at end of buffer since there's no next message
    (should (= (point) (point-max)))))

(provide 'magik-loadlist-test)
;;; magik-loadlist-test.el ends here
