;;; magik-ts-mode-test.el --- Tests for magik-ts-mode.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for magik-ts-mode's tree-sitter font-lock rules.  Entirely
;; skipped when tree-sitter or the magik grammar is not available (e.g.
;; Emacs < 29.1, or the grammar not compiled/installed) rather than
;; erroring, since neither is guaranteed in every environment this test
;; suite runs in.

;;; Code:

(require 'test-helper)

(defvar magik-ts-mode-test--available
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (ignore-errors (require 'magik-ts-mode nil t))
       (fboundp 'treesit-ready-p)
       (treesit-ready-p 'magik))
  "Non-nil if the magik tree-sitter grammar is available for testing.")

(defun magik-ts-mode-test--skip-unless-available ()
  "Skip the current test unless the magik tree-sitter grammar is available."
  (unless magik-ts-mode-test--available
    (ert-skip "magik tree-sitter grammar is not available")))

(defun magik-ts-mode-test--face-at-point-min (code)
  "Insert CODE, activate `magik-ts-mode', and return the face at point-min."
  (with-temp-buffer
    (insert code)
    (magik-ts-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (get-text-property (point) 'face)))

;;; magik-text-encoding-face (font-lock)

(ert-deftest magik-ts-mode--text-encoding-face-spaced ()
  (magik-ts-mode-test--skip-unless-available)
  (should (eq (magik-ts-mode-test--face-at-point-min
               "#% text_encoding = iso8859_1\n_package user\n")
              'magik-text-encoding-face)))

(ert-deftest magik-ts-mode--text-encoding-face-no-spaces ()
  (magik-ts-mode-test--skip-unless-available)
  (should (eq (magik-ts-mode-test--face-at-point-min
               "#%text_encoding=utf8\n_package user\n")
              'magik-text-encoding-face)))

(ert-deftest magik-ts-mode--plain-comment-still-uses-comment-face ()
  (magik-ts-mode-test--skip-unless-available)
  (should (eq (magik-ts-mode-test--face-at-point-min
               "# just a regular comment\n_package user\n")
              'magik-comment-face)))

(provide 'magik-ts-mode-test)
;;; magik-ts-mode-test.el ends here
