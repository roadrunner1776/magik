(package "magik-mode"
         "0.3.3"
         "Emacs major mode for Smallworld Magik files")

(website-url "https://github.com/roadrunner1776/magik")
(keywords "languages")

(package-file "magik-mode.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "24.4")
(depends-on "auto-complete")
(depends-on "compat")
(depends-on "flycheck")
(depends-on "yasnippet")

;; do not try to compile magik-treesit.el if we are below 29.1
(when (version< emacs-version "29.1")
  (add-hook 'eask-before-compile-hook
    (lambda ()
      (delete-file "magik-treesit.el")
      (eask-info "(Removed magik-treesit, since it would not compile in Emacs below 29.1)"))))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
