# magik-mode: Emacs major mode for Smallworld Magik files

## Installation

These packages are available on [MELPA](https://melpa.org/). 
See [Emacs Wiki](https://www.emacswiki.org/emacs/InstallingPackages) for instructions on how to set up and install packages.

The alternative, and recommended, way of installing [magik-mode](https://github.com/roadrunner1776/magik) is using [use-package](https://github.com/jwiegley/use-package):
``` emacs-lisp
(use-package magik-mode
  :ensure t
  :config
  (magik-global-bindings)
  (magik-menu-set-menus))
```

## Features

*Global keys*

Global keys are set by calling `(magik-global-bindings)` after the packages has been loaded.

<kbd>F2</kbd><kbd>s</kbd>: Open Magik version selection

### magik-version

Major mode for managing multiple Magik environments.

<kbd>Return</kbd> Select the current selected line as the current active environment.

<kbd>a</kbd> Open gis_aliases file of selected version. Will prompt for layered product to use if selected version has more than one aliases file available.

<kbd>+</kbd> Add a new entry to the currently open file.

### magik-aliases

Major mode for editing Magik aliases files.

<kbd>Shift</kbd>-</kbd>Return</kbd> Run a Magik session for the selected alias.

### magik-session

Major mode for running Magik session as a direct sub-process.

### magik-mode

Major prog mode for editing Magik code.

### magik-electric-mode

Minor mode for electic Magik.

### magik-cb

Major mode for running the Magik Class Browser.

### magik-module

Major mode for editing Magik module.def files.

### magik-product

Major mode for editing Magik product.def files.

### magik-msg

Major mode for editing Magik Message files.
