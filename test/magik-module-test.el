;;; magik-module-test.el --- Tests for magik-module.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for font-lock highlighting in `magik-module-mode'.

;;; Code:

(require 'test-helper)
(require 'magik-module)

(defun magik-module-test--face-at (content target)
  "Return face at start of TARGET in a fontified magik-module-mode buffer with CONTENT."
  (with-temp-buffer
    (magik-module-mode)
    (insert content)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward target)
    (get-text-property (match-beginning 0) 'face)))

(defun magik-module-test--face-p (pos face)
  "Return non-nil if FACE is among the face properties at POS."
  (let ((f (get-text-property pos 'face)))
    (if (listp f) (memq face f) (eq face f))))

;;; Module name face

(ert-deftest magik-module-font-lock--plain-name-has-name-face ()
  "A module name followed by a version number gets name face."
  (should (eq (magik-module-test--face-at "pieces_tests\t1\n" "pieces_tests")
              'magik-module-name-face)))

(ert-deftest magik-module-font-lock--name-prefixed-with-keyword-has-name-face ()
  "Module name `test_core' gets name face, not partial keyword face.
Regression: the `test' keyword rule without a word-boundary anchor previously
matched `test' as a prefix of `test_core', overriding the name face for those
four characters."
  (should (eq (magik-module-test--face-at "test_core\t1\n" "test_core")
              'magik-module-name-face)))

(ert-deftest magik-module-font-lock--no-char-in-test_core-name-has-keyword-face ()
  "No character inside the `test_core' module name gets keyword face."
  (with-temp-buffer
    (magik-module-mode)
    (insert "test_core\t1\n")
    (font-lock-ensure)
    (should-not
     (let ((found nil))
       (dotimes (i (length "test_core") found)
         (when (magik-module-test--face-p (+ (point-min) i)
                                          'magik-module-keyword-face)
           (setq found t)))))))

;;; Keyword face

(ert-deftest magik-module-font-lock--test-keyword-has-keyword-face ()
  "`test' on its own line gets keyword face."
  (should (eq (magik-module-test--face-at "test\n\tname\tfoo\nend\n" "test")
              'magik-module-keyword-face)))

(ert-deftest magik-module-font-lock--description-has-keyword-face ()
  "`description' at the start of a line gets keyword face."
  (should (eq (magik-module-test--face-at "description\n\tsome text\nend\n"
                                          "description")
              'magik-module-keyword-face)))

(ert-deftest magik-module-font-lock--requires-has-keyword-face ()
  "`requires' at the start of a line gets keyword face."
  (should (eq (magik-module-test--face-at "requires\n\tboard_manager\nend\n"
                                          "requires")
              'magik-module-keyword-face)))

(ert-deftest magik-module-font-lock--end-has-keyword-face ()
  "`end' closing a block gets keyword face."
  (should (eq (magik-module-test--face-at "description\n\tsome text\nend\n" "end")
              'magik-module-keyword-face)))

(ert-deftest magik-module-font-lock--hidden-has-keyword-face ()
  (should (eq (magik-module-test--face-at "hidden\n" "hidden")
              'magik-module-keyword-face)))

;;; Version number face

(ert-deftest magik-module-font-lock--version-number-has-number-face ()
  "The version number after a module name gets number face."
  (should (eq (magik-module-test--face-at "my_module\t1\n" "1")
              'magik-number-face)))

(provide 'magik-module-test)
;;; magik-module-test.el ends here
