;;; magik-utils-test.el --- Tests for magik-utils.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests covering the pure helpers in `magik-utils.el'.

;;; Code:

(require 'test-helper)
(require 'magik-utils)

;;; magik-utils-substitute-in-file-name

(ert-deftest magik-utils-substitute-in-file-name--expands-percent-syntax ()
  (let ((process-environment (cons "MAGIK_TEST_ROOT=/tmp/magik" process-environment)))
    (should (equal (magik-utils-substitute-in-file-name "%MAGIK_TEST_ROOT%/lib")
                   "/tmp/magik/lib"))))

(ert-deftest magik-utils-substitute-in-file-name--leaves-plain-paths-alone ()
  (should (equal (magik-utils-substitute-in-file-name "/plain/path")
                 "/plain/path")))

;;; magik-utils-file-name-display

(ert-deftest magik-utils-file-name-display--returns-input-when-short ()
  (should (equal (magik-utils-file-name-display "/a/b/c" 100) "/a/b/c")))

(ert-deftest magik-utils-file-name-display--truncates-long-paths ()
  (let ((result (magik-utils-file-name-display
                 "/very/long/path/with/many/components/file.el" 20 "...")))
    (should (string-match-p "\\.\\.\\." result))
    (should (string-suffix-p "file.el" result))))

;;; magik-utils-locate-all-dominating-file

(ert-deftest magik-utils-locate-all-dominating-file--collects-every-match ()
  (let* ((root (make-temp-file "magik-test-" t))
         (sub (expand-file-name "sub/sub2" root))
         (marker "marker.def"))
    (unwind-protect
        (progn
          (make-directory sub t)
          (write-region "" nil (expand-file-name marker root))
          (write-region "" nil (expand-file-name marker
                                                 (expand-file-name "sub" root)))
          (let ((found (magik-utils-locate-all-dominating-file sub marker)))
            (should (= 2 (length found)))
            (should (cl-every (lambda (p) (string-match-p marker p)) found))))
      (delete-directory root t))))

(ert-deftest magik-utils-locate-all-dominating-file--returns-nil-when-absent ()
  (let ((root (make-temp-file "magik-test-" t)))
    (unwind-protect
        (should-not (magik-utils-locate-all-dominating-file root "does-not-exist.def"))
      (delete-directory root t))))

;;; magik-utils--first-word-of-file

(ert-deftest magik-utils--first-word-of-file--skips-leading-comments ()
  (let ((file (make-temp-file "magik-test-")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# header comment\n")
            (insert "# another comment\n")
            (insert "  module_name  trailing\n"))
          (should (equal (magik-utils--first-word-of-file file) "module_name")))
      (delete-file file))))

(ert-deftest magik-utils--first-word-of-file--returns-nil-for-missing-file ()
  (should-not (magik-utils--first-word-of-file
               (expand-file-name "no-such-file" temporary-file-directory))))

;;; magik-utils-buffer-mode-list-predicate-p

(ert-deftest magik-utils-buffer-mode-list-predicate-p--nil-predicate-is-true ()
  (should (magik-utils-buffer-mode-list-predicate-p nil)))

(ert-deftest magik-utils-buffer-mode-list-predicate-p--function-result ()
  (should (magik-utils-buffer-mode-list-predicate-p (lambda () t)))
  (should-not (magik-utils-buffer-mode-list-predicate-p (lambda () nil))))

(defvar magik-utils-test--pred-true t)
(defvar magik-utils-test--pred-false nil)

(ert-deftest magik-utils-buffer-mode-list-predicate-p--bound-variable ()
  (should (magik-utils-buffer-mode-list-predicate-p 'magik-utils-test--pred-true))
  (should-not (magik-utils-buffer-mode-list-predicate-p 'magik-utils-test--pred-false)))

;;; which-file

(ert-deftest which-file--locates-file-in-given-path ()
  (let* ((dir (make-temp-file "magik-test-" t))
         (file "needle.el"))
    (unwind-protect
        (progn
          (write-region "" nil (expand-file-name file dir))
          (should (file-equal-p (which-file file nil (list dir))
                                (expand-file-name file dir))))
      (delete-directory dir t))))

(ert-deftest which-file--returns-nil-when-not-found ()
  (let ((dir (make-temp-file "magik-test-" t)))
    (unwind-protect
        (should-not (which-file "missing.el" nil (list dir)))
      (delete-directory dir t))))

(provide 'magik-utils-test)
;;; magik-utils-test.el ends here
