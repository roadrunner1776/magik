;;; magik-indent-test.el --- Tests for magik-indent.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the tokeniser and indent calculator in `magik-indent.el'.

;;; Code:

(require 'test-helper)
(require 'magik-indent)

(defun magik-indent-test--token-strings ()
  "Return the bare token strings of the current line, no positions."
  (mapcar #'car (magik-tokenise-line)))

;;; magik-tokenise-line

(ert-deftest magik-tokenise-line--splits-identifier-call ()
  (with-temp-buffer
    (insert "_method foo.bar(arg)")
    (goto-char (point-min))
    (should (equal (magik-indent-test--token-strings)
                   '("point-min" "_method" "foo" "." "bar" "(" "arg" ")" "\n")))))

(ert-deftest magik-tokenise-line--keeps-string-as-single-token ()
  (with-temp-buffer
    (insert "x << \"hello world\"")
    (goto-char (point-min))
    (let ((toks (magik-indent-test--token-strings)))
      (should (member "<<" toks))
      (should (member "\"hello world\"" toks)))))

(ert-deftest magik-tokenise-line--strips-trailing-comment ()
  (with-temp-buffer
    (insert "x + y # a trailing comment")
    (goto-char (point-min))
    (let ((toks (magik-indent-test--token-strings)))
      (should (member "x" toks))
      (should (member "+" toks))
      (should (member "y" toks))
      (should-not (cl-some (lambda (s) (string-match-p "comment" s)) toks)))))

(ert-deftest magik-tokenise-line--keywords-keep-underscore ()
  (with-temp-buffer
    (insert "_if a _then b _endif")
    (goto-char (point-min))
    (let ((toks (magik-indent-test--token-strings)))
      (should (member "_if" toks))
      (should (member "_then" toks))
      (should (member "_endif" toks)))))

(ert-deftest magik-tokenise-region--recognises-operators ()
  (with-temp-buffer
    (insert "a + b * c\n")
    (let ((toks (mapcar #'car
                        (magik-tokenise-region (point-min) (point-max)))))
      (should (member "+" toks))
      (should (member "*" toks)))))

(ert-deftest magik-tokenise-region-no-eol--drops-newline-token ()
  (with-temp-buffer
    (insert "a + b")
    (let ((toks (mapcar #'car
                        (magik-tokenise-region-no-eol (point-min) (point-max)))))
      (should-not (member "\n" toks)))))

;;; magik-calc-indent

(ert-deftest magik-calc-indent--method-header-at-zero ()
  (with-temp-buffer
    (insert "_method foo.bar()")
    (goto-char (point-min))
    (should (= 0 (magik-calc-indent)))))

(ert-deftest magik-calc-indent--end-method-at-zero ()
  (with-temp-buffer
    (insert "_method foo.bar()\n")
    (insert "    write(:hi)\n")
    (insert "_endmethod\n")
    (goto-char (point-min))
    (forward-line 2)
    (should (= 0 (magik-calc-indent)))))

(ert-deftest magik-calc-indent--body-inside-proc-is-indented ()
  (let ((magik-indent-level 8))
    (with-temp-buffer
      (insert "_proc\n")
      (insert "    body\n")
      (insert "_endproc\n")
      (goto-char (point-min))
      (forward-line 1)
      (should (= 8 (magik-calc-indent))))))

;;; More magik-calc-indent scenarios

(ert-deftest magik-calc-indent--dollar-at-zero ()
  (with-temp-buffer
    (insert "_method foo.bar()\n")
    (insert "  body\n")
    (insert "_endmethod\n")
    (insert "$\n")
    (goto-char (point-min))
    (forward-line 3)
    (should (= 0 (magik-calc-indent)))))

(ert-deftest magik-calc-indent--if-then-indents-body ()
  (let ((magik-indent-level 8))
    (with-temp-buffer
      (insert "_method foo.bar()\n")
      (insert "  _if x\n")
      (insert "  _then\n")
      (insert "    body\n")
      (goto-char (point-min))
      (forward-line 3)
      (should (= (+ 2 8) (magik-calc-indent))))))

(ert-deftest magik-calc-indent--endif-matches-if ()
  (with-temp-buffer
    (insert "_method foo.bar()\n")
    (insert "  _if x\n")
    (insert "  _then\n")
    (insert "    body\n")
    (insert "  _endif\n")
    (goto-char (point-min))
    (forward-line 4)
    (should (= 2 (magik-calc-indent)))))

(ert-deftest magik-calc-indent--loop-body-indented ()
  (let ((magik-indent-level 8))
    (with-temp-buffer
      (insert "_method foo.bar()\n")
      (insert "  _loop\n")
      (insert "    body\n")
      (goto-char (point-min))
      (forward-line 2)
      (should (= (+ 2 8) (magik-calc-indent))))))

(ert-deftest magik-calc-indent--endloop-matches-loop ()
  (with-temp-buffer
    (insert "_method foo.bar()\n")
    (insert "  _loop\n")
    (insert "    body\n")
    (insert "  _endloop\n")
    (goto-char (point-min))
    (forward-line 3)
    (should (= 2 (magik-calc-indent)))))

(ert-deftest magik-calc-indent--comment-aligns-with-previous-comment ()
  (with-temp-buffer
    (insert "_method foo.bar()\n")
    (insert "    # first comment\n")
    (insert "    # second comment\n")
    (goto-char (point-min))
    (forward-line 2)
    (should (= 4 (magik-calc-indent)))))

(ert-deftest magik-calc-indent--protect-body-indented ()
  (let ((magik-indent-level 8))
    (with-temp-buffer
      (insert "_method foo.bar()\n")
      (insert "  _protect\n")
      (insert "    body\n")
      (goto-char (point-min))
      (forward-line 2)
      (should (= (+ 2 8) (magik-calc-indent))))))

(ert-deftest magik-calc-indent--protection-matches-protect ()
  (with-temp-buffer
    (insert "_method foo.bar()\n")
    (insert "  _protect\n")
    (insert "    body\n")
    (insert "  _protection\n")
    (goto-char (point-min))
    (forward-line 3)
    (should (= 2 (magik-calc-indent)))))

(ert-deftest magik-calc-indent--try-with-indents ()
  (let ((magik-indent-level 8))
    (with-temp-buffer
      (insert "_method foo.bar()\n")
      (insert "  _try _with cond\n")
      (insert "    body\n")
      (goto-char (point-min))
      (forward-line 2)
      (should (= (+ 2 8) (magik-calc-indent))))))

(ert-deftest magik-calc-indent--when-indents ()
  (let ((magik-indent-level 8))
    (with-temp-buffer
      (insert "_method foo.bar()\n")
      (insert "  _try\n")
      (insert "    body\n")
      (insert "  _when err\n")
      (insert "    handler\n")
      (goto-char (point-min))
      (forward-line 4)
      (should (= (+ 2 8) (magik-calc-indent))))))

(ert-deftest magik-calc-indent--block-body-indented ()
  (let ((magik-indent-level 8))
    (with-temp-buffer
      (insert "_method foo.bar()\n")
      (insert "  _block\n")
      (insert "    body\n")
      (goto-char (point-min))
      (forward-line 2)
      (should (= (+ 2 8) (magik-calc-indent))))))

(ert-deftest magik-calc-indent--endblock-matches-block ()
  (with-temp-buffer
    (insert "_method foo.bar()\n")
    (insert "  _block\n")
    (insert "    body\n")
    (insert "  _endblock\n")
    (goto-char (point-min))
    (forward-line 3)
    (should (= 2 (magik-calc-indent)))))

(ert-deftest magik-calc-indent--catch-body-indented ()
  (let ((magik-indent-level 8))
    (with-temp-buffer
      (insert "_method foo.bar()\n")
      (insert " _catch :tag\n")
      (insert "    body\n")
      (goto-char (point-min))
      (forward-line 2)
      ;; Body indents to column of :tag (after _catch + space)
      (should (= 8 (magik-calc-indent))))))

(ert-deftest magik-calc-indent--continuation-line-indented ()
  (let ((magik-indent-level 8))
    (with-temp-buffer
      (insert "_method foo.bar()\n")
      (insert "  x << a +\n")
      (insert "    b\n")
      (goto-char (point-min))
      (forward-line 2)
      ;; Continuation should indent relative to the operator
      (let ((indent (magik-calc-indent)))
        (should (> indent 2))))))

;;; magik-tokenise-line edge cases

(ert-deftest magik-tokenise-line--symbols-with-pipe ()
  (with-temp-buffer
    (insert "x << :|hello world|")
    (goto-char (point-min))
    (let ((toks (magik-indent-test--token-strings)))
      (should (member "<<" toks)))))

(ert-deftest magik-tokenise-line--chevron-operator ()
  (with-temp-buffer
    (insert ">> result")
    (goto-char (point-min))
    (let ((toks (magik-indent-test--token-strings)))
      (should (member ">>" toks)))))

(ert-deftest magik-tokenise-line--gather-parameter ()
  (with-temp-buffer
    (insert "_method foo.bar(_gather args)")
    (goto-char (point-min))
    (let ((toks (magik-indent-test--token-strings)))
      (should (member "_gather" toks))
      (should (member "args" toks)))))

(provide 'magik-indent-test)
;;; magik-indent-test.el ends here
