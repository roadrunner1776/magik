;;; test-helper.el --- Test helper for magik-mode  -*- lexical-binding: t; -*-

;;; Commentary:

;; Shared setup for the ERT test suite.  Puts the project root on
;; `load-path' so individual test files can `require' the modules
;; under test without depending on the user's environment.

;;; Code:

(require 'ert)

(defvar magik-mode-test--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name))))
  "Absolute path to the magik-mode source root.")

(add-to-list 'load-path magik-mode-test--root)

(require 'compat)

(provide 'test-helper)
;;; test-helper.el ends here
