;;; magik-pragma-test.el --- Tests for magik-pragma.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests covering pragma-line detection and the classify_level
;; toggle in `magik-pragma.el'.

;;; Code:

(require 'test-helper)
(require 'magik-pragma)

;;; magik-pragma-line-p

(ert-deftest magik-pragma-line-p--detects-single-line-pragma ()
  (with-temp-buffer
    (insert "_pragma(classify_level=basic, topic={}, usage={})\n")
    (goto-char (point-min))
    (search-forward "basic")
    (let ((brackets (magik-pragma-line-p)))
      (should (consp brackets))
      (should (< (car brackets) (cdr brackets))))))

(ert-deftest magik-pragma-line-p--detects-multiline-pragma ()
  (with-temp-buffer
    (insert "_pragma(classify_level=basic,\n")
    (insert "        topic={common},\n")
    (insert "        usage={subclassable})\n")
    (goto-char (point-min))
    (search-forward "topic")
    (should (consp (magik-pragma-line-p)))))

(ert-deftest magik-pragma-line-p--rejects-non-pragma-line ()
  (with-temp-buffer
    (insert "_method foo.bar(arg)\n")
    (insert "  body\n")
    (insert "_endmethod\n")
    (goto-char (point-min))
    (forward-line 1)
    (should-not (magik-pragma-line-p))))

;;; magik-pragma-electric-toggle (classify_level)

(ert-deftest magik-pragma-electric-toggle--cycles-classify-level-forward ()
  (with-temp-buffer
    (insert "_pragma(classify_level=basic)")
    (goto-char (point-min))
    (search-forward "classify_level=")
    (goto-char (match-beginning 0))
    (forward-char 1)
    (magik-pragma-electric-toggle 'forward)
    (should (string-match-p "classify_level=advanced" (buffer-string)))))

(ert-deftest magik-pragma-electric-toggle--cycles-classify-level-backward ()
  (with-temp-buffer
    (insert "_pragma(classify_level=advanced)")
    (goto-char (point-min))
    (search-forward "classify_level=")
    (goto-char (match-beginning 0))
    (forward-char 1)
    (magik-pragma-electric-toggle 'backward)
    (should (string-match-p "classify_level=basic" (buffer-string)))))

(ert-deftest magik-pragma-electric-toggle--wraps-around ()
  (with-temp-buffer
    (insert "_pragma(classify_level=debug)")
    (goto-char (point-min))
    (search-forward "classify_level=")
    (goto-char (match-beginning 0))
    (forward-char 1)
    (magik-pragma-electric-toggle 'forward)
    (should (string-match-p "classify_level=basic" (buffer-string)))))

;;; magik-pragma-if-match-replace-with-next

(ert-deftest magik-pragma-if-match-replace-with-next--replaces-match ()
  (with-temp-buffer
    (insert "basic")
    (goto-char (point-min))
    (should (looking-at " *basic *"))
    (magik-pragma-if-match-replace-with-next
     '(basic (looking-at " *basic *") nil)
     '(advanced (looking-at " *advanced *") nil)
     nil)
    (should (equal (buffer-string) "advanced"))))

(provide 'magik-pragma-test)
;;; magik-pragma-test.el ends here
