;;; magik-trn-test.el --- Tests for magik-trn.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests covering font-lock fontification in `magik-trn.el'.

;;; Code:

(require 'test-helper)
(require 'magik-trn)

;;; auto-mode-alist

(ert-deftest magik-trn--auto-mode-alist ()
  (should (eq (assoc-default "foo.trn" auto-mode-alist 'string-match) 'magik-trn-mode)))

;;; magik-trn-text-encoding-face (font-lock)

(ert-deftest magik-trn-text-encoding-face--spaced ()
  (with-temp-buffer
    (magik-trn-mode)
    (insert "#% text_encoding = iso8859_1\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'magik-trn-text-encoding-face))))

(ert-deftest magik-trn-text-encoding-face--no-spaces ()
  (with-temp-buffer
    (magik-trn-mode)
    (insert "#%text_encoding=utf8\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'magik-trn-text-encoding-face))))

;;; magik-trn-comment-face (font-lock)

(ert-deftest magik-trn-comment-face--generic-comment-line ()
  (with-temp-buffer
    (magik-trn-mode)
    (insert "# (c) 2024 Example Company. All Rights Reserved.\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'magik-trn-comment-face))))

(ert-deftest magik-trn-comment-face--column-header-comment ()
  (with-temp-buffer
    (magik-trn-mode)
    (insert "# external_name\ttable_name\tunset\ten_gb\tstring\ttranslation\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'magik-trn-comment-face))))

(ert-deftest magik-trn-comment-face--text-encoding-not-generic-comment ()
  "The text_encoding line must keep its own face, not the generic comment face."
  (with-temp-buffer
    (magik-trn-mode)
    (insert "#% text_encoding = utf8\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should-not (eq (get-text-property (point) 'face) 'magik-trn-comment-face))))

;;; magik-trn-type-face (font-lock)

(ert-deftest magik-trn-type-face--external-name ()
  (with-temp-buffer
    (magik-trn-mode)
    (insert "external_name\tfoo\tunset\ten_gb\tFoo\tFoo\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'magik-trn-type-face))))

(ert-deftest magik-trn-type-face--enumerator ()
  (with-temp-buffer
    (magik-trn-mode)
    (insert "enumerator\tfoo\tunset\ten_gb\tFoo\tFoo\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'magik-trn-type-face))))

(ert-deftest magik-trn-type-face--field-value ()
  (with-temp-buffer
    (magik-trn-mode)
    (insert "field_value\tfoo\tbar\ten_gb\tFoo\tFoo\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'magik-trn-type-face))))

(ert-deftest magik-trn-type-face--not-fontified-mid-line ()
  "The type keyword must only be recognised in the first column."
  (with-temp-buffer
    (magik-trn-mode)
    (insert "foo\texternal_name\tbar\ten_gb\tFoo\tFoo\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should-not (eq (get-text-property (point) 'face) 'magik-trn-type-face))))

(ert-deftest magik-trn-type-face--key1-not-fontified ()
  "Only the type column itself should be fontified, not the following field."
  (with-temp-buffer
    (magik-trn-mode)
    (insert "external_name\tfoo\tunset\ten_gb\tFoo\tFoo\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (forward-char (length "external_name\t"))
    (should-not (eq (get-text-property (point) 'face) 'magik-trn-type-face))))

;;; face inheritance

(ert-deftest magik-trn-comment-face--inherits-shared-face ()
  (should (eq (face-attribute 'magik-trn-comment-face :inherit) 'magik-comment-face)))

(ert-deftest magik-trn-text-encoding-face--inherits-shared-face ()
  (should (eq (face-attribute 'magik-trn-text-encoding-face :inherit) 'magik-text-encoding-face)))

(ert-deftest magik-trn-type-face--inherits-shared-face ()
  (should (eq (face-attribute 'magik-trn-type-face :inherit) 'magik-class-face)))

(provide 'magik-trn-test)
;;; magik-trn-test.el ends here
