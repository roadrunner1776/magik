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

;;; Overlay-based history folding

(ert-deftest magik-session-display-history--creates-overlays ()
  "Verify that display-history creates invisible overlays."
  (with-temp-buffer
    (magik-session-mode)
    (add-to-invisibility-spec '(magik-session-fold . t))
    ;; Simulate 3 commands in the buffer with prompt-based detection
    (insert "MagikSF> cmd1\noutput1\nMagikSF> cmd2\noutput2\nMagikSF> cmd3\noutput3\n")
    ;; Display last 2 commands (should fold everything before cmd2)
    (magik-session-display-history 2)
    ;; Check overlays exist
    (let ((ovs (overlays-in (point-min) (point-max))))
      (should (cl-some (lambda (ov) (overlay-get ov 'magik-session-fold)) ovs)))))

(ert-deftest magik-session-undisplay-history--removes-overlays ()
  "Verify that undisplay-history removes all fold overlays."
  (with-temp-buffer
    (magik-session-mode)
    (add-to-invisibility-spec '(magik-session-fold . t))
    (insert "some text\nmore text\n")
    ;; Manually create a fold overlay
    (let ((ov (make-overlay (point-min) 10)))
      (overlay-put ov 'invisible 'magik-session-fold)
      (overlay-put ov 'magik-session-fold t))
    ;; Verify it exists
    (should (cl-some (lambda (ov) (overlay-get ov 'magik-session-fold))
                     (overlays-in (point-min) (point-max))))
    ;; Now undisplay
    (magik-session-undisplay-history nil)
    ;; Verify gone
    (should-not (cl-some (lambda (ov) (overlay-get ov 'magik-session-fold))
                         (overlays-in (point-min) (point-max))))))

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

(provide 'magik-session-test)
;;; magik-session-test.el ends here
