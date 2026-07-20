;; -*- mode: eask; lexical-binding: t -*-

(package "magik-mode"
         "0.6.5"
         "Emacs major mode for Smallworld Magik files")

(website-url "https://github.com/roadrunner1776/magik")
(keywords "languages")

(package-file "magik-mode.el")
(files "*.el" "snippets")

(script "test" "eask test ert test/test-helper.el ./test/*-test.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "24.4")
(depends-on "compat")
(when (>= emacs-major-version 28)
  (depends-on "flycheck"))
(depends-on "yasnippet")

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
