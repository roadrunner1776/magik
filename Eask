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

;; do not try to compile magik-lint.el if we are below 28.2
(when (version< emacs-version "28.2")
  (add-hook 'eask-before-compile-hook
    (lambda ()
      (delete-file "magik-lint.el")
      (eask-info "(Removed magik-lint, since it would not compile in Emacs below 28.2)"))))

;; do not try to compile magik-ts-mode.el if we are below 29.1
(when (version< emacs-version "29.1")
  (add-hook 'eask-before-compile-hook
    (lambda ()
      (delete-file "magik-ts-mode.el")
      (eask-info "(Removed magik-ts-mode, since it would not compile in Emacs below 29.1)"))))

;; Work around GNU bug#80180: Emacs 31's loaddefs-gen force-loads a
;; scraped file with `load-suffixes' restricted to ".el" when it hits an
;; unrecognized top-level macro (e.g. our autoloaded
;; `global-magik-completion-mode' call). That restriction leaks into any
;; autoload triggered while loading the file (e.g. `pcase'), which on
;; distributions shipping gzip-compressed *.el next to *.elc (Debian,
;; and CI's Nix-packaged Emacs) forces jka-compr to decompress its own
;; source, recursing until Emacs's "Recursive load" guard trips.
;; Loading the real jka-compr now, before that restriction is ever in
;; effect, avoids the self-referential bootstrap.
(require 'jka-compr)

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
