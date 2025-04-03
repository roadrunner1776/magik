(package "magik-mode"
         "0.4.1"
         "Emacs major mode for Smallworld Magik files")

(website-url "https://github.com/roadrunner1776/magik")
(keywords "languages")

(package-file "magik-mode.el")
(files "*.el" "snippets")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "24.4")
(depends-on "compat")
(depends-on "yasnippet")

;; do not try to compile magik-ts-mode.el if we are below 29.1
(when (version< emacs-version "29.1")
  (add-hook 'eask-before-compile-hook
            (lambda ()
              (delete-file "magik-ts-mode.el")
              (eask-info "(Removed magik-ts-mode, since it would not compile in Emacs below 29.1)"))))

(if (require 'auto-complete nil 'noerror)
  (add-hook 'eask-before-compile-hook
    (lambda ()
      (delete-file "magik-cb-ac.el")
      (eask-info "(Removed magik autocomplete classbrowser since the user doesn't have the autocomplete package)")))
  (depends-on "auto-complete"))

(if (require 'flycheck nil 'noerror)
    (add-hook 'eask-before-compile-hook
              (lambda ()
                (delete-file "magik-lint.el")
                (eask-info "(Removed magik lint since the user doesn't have the flycheck package)")))
  (depends-on "flycheck"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
