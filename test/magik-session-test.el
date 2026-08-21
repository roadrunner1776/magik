;;; magik-session-test.el --- Tests for magik-session.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for command parsing and overlay-based history folding in
;; `magik-session.el'.

;;; Code:

(require 'test-helper)
(require 'magik-session)

;;; magik-session-parse-gis-command

(ert-deftest magik-session-parse-gis-command--simple-command ()
  (should (equal (magik-session-parse-gis-command "/usr/bin/gis")
                 '("/usr/bin/gis"))))

(ert-deftest magik-session-parse-gis-command--with-arguments ()
  (should (equal (magik-session-parse-gis-command "/usr/bin/gis -e /tmp/env")
                 '("/usr/bin/gis" "-e" "/tmp/env"))))

(ert-deftest magik-session-parse-gis-command--quoted-argument ()
  (should (equal (magik-session-parse-gis-command "/usr/bin/gis \"path with spaces\"")
                 '("/usr/bin/gis" "path with spaces"))))

(ert-deftest magik-session-parse-gis-command--single-quoted-argument ()
  (should (equal (magik-session-parse-gis-command "/usr/bin/gis 'path with spaces'")
                 '("/usr/bin/gis" "path with spaces"))))

(ert-deftest magik-session-parse-gis-command--trailing-whitespace-ignored ()
  (should (equal (magik-session-parse-gis-command "/usr/bin/gis -a   ")
                 '("/usr/bin/gis" "-a"))))

(ert-deftest magik-session-parse-gis-command--env-var-expansion ()
  (let ((process-environment (cons "TEST_DIR=/opt/magik" process-environment)))
    (should (equal (magik-session-parse-gis-command "$TEST_DIR/gis")
                   '("/opt/magik/gis")))))

;;; magik-session-buffer-alist-remove

(ert-deftest magik-session-buffer-alist-remove--removes-entry ()
  (with-temp-buffer
    (let ((magik-session-buffer-alist (list (cons 1 (buffer-name)))))
      (magik-session-buffer-alist-remove)
      (should (null (cdr (car magik-session-buffer-alist)))))))

(ert-deftest magik-session-buffer-alist-remove--returns-key ()
  (with-temp-buffer
    (let ((magik-session-buffer-alist (list (cons 3 (buffer-name)))))
      (should (= 3 (magik-session-buffer-alist-remove))))))

(ert-deftest magik-session-buffer-alist-remove--returns-nil-when-absent ()
  (with-temp-buffer
    (let ((magik-session-buffer-alist nil))
      (should-not (magik-session-buffer-alist-remove)))))

;;; magik-session-beginning-of-line

(ert-deftest magik-session-beginning-of-line--moves-past-the-prompt ()
  (with-temp-buffer
    (magik-session-mode)
    (insert "Magik> print(1)")
    (let ((last-command nil))
      (magik-session-beginning-of-line 1))
    (should (equal (buffer-substring-no-properties (point) (point-max))
                   "print(1)"))))

(ert-deftest magik-session-beginning-of-line--repeated-goes-to-column-zero ()
  "Repeating the command moves point right to the beginning of the line."
  (with-temp-buffer
    (magik-session-mode)
    (insert "Magik> print(1)")
    (let ((last-command 'magik-session-beginning-of-line))
      (magik-session-beginning-of-line 1))
    (should (= (point) (line-beginning-position)))))

(ert-deftest magik-session-beginning-of-line--is-shift-select-aware ()
  "Both preconditions for `shift-select-mode' must hold on C-S-a.
Emacs only shift-translates C-S-a to C-a while C-S-a is unbound, and the
command it lands on only extends the region when its interactive spec
starts with `^'."
  (with-temp-buffer
    (magik-session-mode)
    (should-not (lookup-key magik-session-mode-map (kbd "C-S-a")))
    (let ((spec (cadr (interactive-form (key-binding (kbd "C-a"))))))
      (should (stringp spec))
      (should (string-prefix-p "^" spec)))))

(ert-deftest magik-session-beginning-of-line--shift-selection-selects-command ()
  "Pressing C-S-a selects back to the prompt, as it does in a shell buffer.
The buffer has to be displayed, otherwise the key cannot be executed."
  (let ((buffer (get-buffer-create "*magik-session-test*"))
        (shift-select-mode t)
        (transient-mark-mode t))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer buffer)
          (magik-session-mode)
          (erase-buffer)
          (insert "Magik> print(1)")
          (goto-char (point-max))
          (deactivate-mark)
          (execute-kbd-macro (kbd "C-S-a"))
          (should (region-active-p))
          (should (equal (buffer-substring-no-properties
                          (region-beginning) (region-end))
                         "print(1)")))
      (kill-buffer buffer))))

(provide 'magik-session-test)
;;; magik-session-test.el ends here
