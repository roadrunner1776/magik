;;; magik-version-test.el --- Tests for magik-version.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for `magik-version-selection', in particular where the
;; `(invalid)' marker is inserted for entries whose directory does not
;; exist on disk.  The marker must be appended at the true end of the
;; line, after the existing path text, rather than spliced into the
;; middle of the line ahead of the path.

;;; Code:

(require 'test-helper)
(require 'magik-version)
(require 'magik-session)

(defun magik-version-test--write-version-file (file entries)
  "Write a gis version FILE containing the header and ENTRIES.
Each element of ENTRIES is a list (NAME VERSION PATH)."
  (with-temp-buffer
    (insert magik-version-file-header)
    (dolist (entry entries)
      (insert (apply #'format magik-version-file-format entry)))
    (write-region (point-min) (point-max) file nil 'silent)))

(defmacro magik-version-test--with-selection-buffer (bindings &rest body)
  "Run BODY with the version selection buffer current.
BINDINGS is a `let' bindings list, typically binding
`magik-version-file' and `magik-version-validate-entries', in effect
while `magik-version-selection' is invoked.  The resulting
\"* version selection*\" buffer is killed afterwards even if BODY
signals."
  (declare (indent 1))
  `(let ,bindings
     (unwind-protect
         (progn
           (magik-version-selection)
           (with-current-buffer (get-buffer (concat "*" magik-session-buffer-default-name
                                                      " version selection*"))
             ,@body))
       (let ((buf (get-buffer (concat "*" magik-session-buffer-default-name
                                       " version selection*"))))
         (when buf
           (let ((kill-buffer-query-functions nil))
             (kill-buffer buf)))))))

;;; magik-version-selection -- valid entries are left untouched

(ert-deftest magik-version-selection--valid-path-not-marked-invalid ()
  "An entry whose directory exists on disk gets no `(invalid)' marker."
  (let* ((root (make-temp-file "magik-version-test-" t))
         (version-file (expand-file-name "gis_version.txt" root))
         (real-dir (expand-file-name "real_product" root)))
    (unwind-protect
        (progn
          (make-directory real-dir t)
          (magik-version-test--write-version-file
           version-file (list (list "valid_product" "1.0.0" real-dir)))
          (magik-version-test--with-selection-buffer
              ((magik-version-file version-file)
               (magik-version-validate-entries t))
            (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p (regexp-quote real-dir) contents))
              (should-not (string-match-p (regexp-quote magik-version-invalid-string)
                                           contents)))))
      (delete-directory root t))))

;;; magik-version-selection -- invalid entries get `(invalid)' appended
;;; at the true end of the line, not spliced in front of the path

(ert-deftest magik-version-selection--missing-path-marked-invalid-at-end-of-line ()
  "An entry whose directory does not exist gets `(invalid)' appended
after the path, with the original path text left fully intact."
  (let* ((root (make-temp-file "magik-version-test-" t))
         (version-file (expand-file-name "gis_version.txt" root))
         (missing-dir (expand-file-name "does_not_exist_product" root)))
    (unwind-protect
        (progn
          (magik-version-test--write-version-file
           version-file (list (list "missing_product" "2.0.0" missing-dir)))
          (magik-version-test--with-selection-buffer
              ((magik-version-file version-file)
               (magik-version-validate-entries t))
            (goto-char (point-min))
            (should (re-search-forward "^. missing_product\\s-+2\\.0\\.0\\s-+\\(.*\\)$" nil t))
            (let ((rest-of-line (match-string-no-properties 1)))
              ;; The path is fully intact and is immediately followed by
              ;; the invalid marker at the end of the line -- not
              ;; interrupted midway by an inserted "(invalid) " prefix.
              (should (equal rest-of-line
                              (concat missing-dir " " magik-version-invalid-string)))
              (should (string-suffix-p magik-version-invalid-string rest-of-line)))))
      (delete-directory root t))))

;;; magik-version-selection -- already-marked entries are left alone

(ert-deftest magik-version-selection--already-invalid-not-duplicated ()
  "A line whose path already contains `(invalid)' is left exactly as-is."
  (let* ((root (make-temp-file "magik-version-test-" t))
         (version-file (expand-file-name "gis_version.txt" root))
         (missing-dir (expand-file-name "already_marked_product" root))
         (already-marked-path (concat missing-dir " " "(invalid)")))
    (unwind-protect
        (progn
          (magik-version-test--write-version-file
           version-file (list (list "already_marked" "3.0.0" already-marked-path)))
          (magik-version-test--with-selection-buffer
              ((magik-version-file version-file)
               (magik-version-validate-entries t))
            (goto-char (point-min))
            (should (re-search-forward "^. already_marked\\s-+3\\.0\\.0\\s-+\\(.*\\)$" nil t))
            (let ((rest-of-line (match-string-no-properties 1)))
              ;; No duplicate marker: exactly one "(invalid)", matching
              ;; the original untouched path field.
              (should (equal rest-of-line already-marked-path))
              (should-not (string-match-p
                           (concat magik-version-invalid-string
                                   "\\s-*" magik-version-invalid-string)
                           rest-of-line)))))
      (delete-directory root t))))

;;; magik-version-selection -- validation disabled leaves entries alone

(ert-deftest magik-version-selection--validation-disabled-leaves-missing-path-alone ()
  "When `magik-version-validate-entries' is nil, even a nonexistent
path is left unmarked."
  (let* ((root (make-temp-file "magik-version-test-" t))
         (version-file (expand-file-name "gis_version.txt" root))
         (missing-dir (expand-file-name "unvalidated_missing_product" root)))
    (unwind-protect
        (progn
          (magik-version-test--write-version-file
           version-file (list (list "unvalidated_product" "4.0.0" missing-dir)))
          (magik-version-test--with-selection-buffer
              ((magik-version-file version-file)
               (magik-version-validate-entries nil))
            (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p (regexp-quote missing-dir) contents))
              (should-not (string-match-p (regexp-quote magik-version-invalid-string)
                                           contents)))))
      (delete-directory root t))))

(provide 'magik-version-test)
;;; magik-version-test.el ends here
