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

(provide 'magik-mode-test)
;;; magik-mode-test.el ends here
