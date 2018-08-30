# magik-mode: Emacs major mode for Smallworld Magik files

## Installation

These packages are available on [MELPA](https://melpa.org/). 
See [Emacs Wiki](https://www.emacswiki.org/emacs/InstallingPackages) for instructions on how to set up and install packages.

The alternative, and recommended, way of installing [magik-mode](https://github.com/roadrunner1776/magik) is using [use-package](https://github.com/jwiegley/use-package):

``` emacs-lisp
(use-package magik
  :ensure t
  :mode (("\\.magik\\'". magik-mode)))
```

