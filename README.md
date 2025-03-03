# magik-mode: Emacs major mode for Smallworld Magik files

[![GNU Emacs](https://raw.githubusercontent.com/minad/corfu/screenshots/emacs.svg)](https://www.gnu.org/software/emacs/) [![MELPA](https://melpa.org/packages/magik-mode-badge.svg)](https://melpa.org/#/magik-mode) [![MELPA Stable](https://stable.melpa.org/packages/magik-mode-badge.svg)](https://stable.melpa.org/#/magik-mode)

## Content

1. [Installation](#installation)
2. [Features](#features)
3. [Usage with Smallworld 4.x or older](#usage-with-smallworld-4x-or-older)
4. [Side Effects](#side-effects)
5. [Familiar with SW 4.x EMACS? Some Tipps for you!](#familiar-with-sw-4x-emacs-some-tipps-for-you)

## Installation

These packages are available on [MELPA](https://melpa.org/).
See [Emacs Wiki](https://www.emacswiki.org/emacs/InstallingPackages) for instructions on how to set up and install packages.

The alternative, and recommended, way of installing [magik-mode](https://github.com/roadrunner1776/magik) is using [use-package](https://github.com/jwiegley/use-package):

```emacs-lisp
(use-package magik-mode
  :ensure t
  :config
  (magik-global-bindings)
  (magik-menu-set-menus))
```

## Features

### Global keys

Global keys are set by calling `(magik-global-bindings)` after the packages has been loaded.

| Key | Description |
| :---: | --- |
| `F2``s` | Open Magik version selection |

### magik-version

Major mode for managing multiple Magik environments.

| Key | Description |
| :---: | --- |
| `Return` | Select the current selected line as the current active environment. |
| `a` | Open gis_aliases file of selected version. Will prompt for layered product to use if selected version has more than one aliases file available. |
| `+` | Add a new entry to the currently open file. |

### magik-aliases

Major mode for editing Magik aliases files.

| Key | Description |
| :---: | --- |
| `Shift`-`Return` | Run a Magik session for the selected alias. |

### magik-session

Major mode for running Magik session as a direct sub-process.

### magik-mode

Major prog mode for editing Magik code.

Support for outline-minor mode. Try: `(outline-minor-mode)`

Support for imenu. Try: `(add-hook 'magik-mode-hook 'imenu-add-menubar-index)`

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

### magik-trn

Major mode for editing Magik Translation files.

### magik-lint

[Magik-lint](https://github.com/StevenLooman/sonar-magik/tree/master/magik-lint) support.

To enable automatic linting in `magik-mode` buffers, the following conditions have to be met:

* Package [flycheck](https://flycheck.org) needs to be installed and loaded.
* `magik-lint-VERSION.jar` ([download](https://github.com/StevenLooman/sonar-magik/releases)) has to be installed in `~/.emacs.d/magik-lint/` (location can be customized with the variable `magik-lint-jar-file`).
* The `java` executable path should be in `exec-path`, or the variable `flycheck-magik-lint-java-executable` has to be set. `flycheck-magik-lint-java-executable` will automatically be set when the environment variable `JAVA_HOME` is set.

* `flycheck-mode` has to be enabled for `magik-mode` buffers. Or use `global-flycheck-mode` to enable it for all buffers.

## Usage with Smallworld 4.x or older

If you plan to use this package with Smallworld-Versions 4.x or older, you should consider the following points:

* Customize the variable `magik-session-auto-insert-dollar` to non nil
* You might customize the variable `magik-aliases-layered-products-file` to "$SMALLWORLD_GIS/product/config/LAYERED_PRODUCTS". But if you want to use the EMACS for Smallworld 5.x as well, it's easier to create the directory `$SMALLWORLD_GIS/../smallworld_registry` and copy or soft-link the original LAYERED_PRODUCTS file into that directory - so you have the same structure as under Smallworld 5.x.
* There is no support (yet) for the Smallworld dev-tools. So if you want to do things like `f4`-`d` to start debugging a method, you may still want to use the EMACS which has been delivered with the Smallworld 4.x (or older) software.
* Some more things which are at least partly not supported by Smallworld 5.x are not supported (e.g. `deep-print`)

## Side Effects

Some keys bindings are changed with respect to a standard EMACS installation, at least when using `(magik-global-bindings)`:

| Key | Function in standard EMACS | Change in Magik Mode package |
| :---: | --- | --- |
| `F2` | `2C-command`|globally changed to prefix key |
| `F3` | `kmacro-start-macro-or-insert-counter` | globally changed to prefix key |
| `F4` | `kmacro-end-or-call-macro` | used in magik-mode and magik-session-mode as prefix key |

The reason for that is, that many Magik developpers are familiar with these bindings from former EMACS installations.

For quick usage of the keyboard-macro functions you may e.g. bind the `Ctrl`-`F2` and `Ctrl`-`F4` combinations by putting the following lines into your  `.emacs` file:

```emacs-lisp
(global-set-key [C-f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [C-f4] 'kmacro-end-or-call-macro)
```

## Familiar with SW 4.x EMACS? Some Tipps for you

If you've been using the EMACS delivered by GE with Smallworld version 4.3 and earlier, you might want to customize some variables with default values changed with respect to the former "GE"-EMACS:

| actual<br/>variable | actual<br>default | former<br/>variable | former<br/>default | Remarks |
| :---: | --- | --- | --- | --- |
| magik-electric-mode | t | electric-magik-mode | nil | If set (non nil), typing the `Space` after a keyworld like `for` or `if` inserts the matching control structure.<br/>If not set (nil), you have to type `F2`-`Space` to achive the same behaviour. |
| mouse-drag-copy-region | nil | dto. | t | Paste a mouse selection (even to other programs).<br/>(Default has been changed in standard EMACS) |
| show-paren-mode | nil | dto | t | if point is on opening or after closeing bracket, show the matching one.<br/>(GE's EMACS had done this customization) |

Eventually you'll find some more customazations in of the former GE's EMACS' installation in emacs/site-lisp/smallworld/sw_defaults.el, but you'll have to check, whether these will work the the EMACS installation you are using now.
