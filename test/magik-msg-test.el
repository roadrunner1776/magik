;;; magik-msg-test.el --- Tests for magik-msg.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests covering font-lock fontification in `magik-msg.el'.

;;; Code:

(require 'test-helper)
(require 'magik-msg)

;;; magik-msg-text-encoding-face (font-lock)

(ert-deftest magik-msg-text-encoding-face--spaced ()
  (with-temp-buffer
    (magik-msg-mode)
    (insert "#% text_encoding = iso8859_1\n")
    (insert ":foo\tBar\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'magik-msg-text-encoding-face))))

(ert-deftest magik-msg-text-encoding-face--no-spaces ()
  (with-temp-buffer
    (magik-msg-mode)
    (insert "#%text_encoding=utf8\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should (eq (get-text-property (point) 'face) 'magik-msg-text-encoding-face))))

(ert-deftest magik-msg-text-encoding-face--not-fontified-without-tag ()
  (with-temp-buffer
    (magik-msg-mode)
    (insert ":foo\tBar\n")
    (font-lock-ensure)
    (goto-char (point-min))
    (should-not (eq (get-text-property (point) 'face) 'magik-msg-text-encoding-face))))

;;; face inheritance

(ert-deftest magik-msg-text-encoding-face--inherits-shared-face ()
  (should (eq (face-attribute 'magik-msg-text-encoding-face :inherit) 'magik-text-encoding-face)))

(ert-deftest magik-msg-comment-face--inherits-shared-face ()
  (should (eq (face-attribute 'magik-msg-comment-face :inherit) 'magik-comment-face)))

(provide 'magik-msg-test)
;;; magik-msg-test.el ends here
