# magik-mode: Emacs major mode for Smallworld Magik files

[![GNU Emacs](https://raw.githubusercontent.com/roadrunner1776/magik/master/emacs.svg)](https://www.gnu.org/software/emacs/) [![MELPA](https://melpa.org/packages/magik-mode-badge.svg)](https://melpa.org/#/magik-mode) [![MELPA Stable](https://stable.melpa.org/packages/magik-mode-badge.svg)](https://stable.melpa.org/#/magik-mode)

## Content

1. [Installation](#installation)
2. [Features](#features)
3. [Usage with Smallworld 4.x or older](#usage-with-smallworld-4x-or-older)
4. [Side effects](#side-effects)
5. [Familiar with SW 4.x EMACS? Some tips for you!](#familiar-with-sw-4x-emacs-some-tips-for-you)

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

### Automatic completion support

Automatic completion support is no longer provided by this package. Please refer to the [magik-company](https://github.com/reinierkof/magik-company) package for this functionality.

### Global keys

Global keys are set by calling `(magik-global-bindings)` after the package has been loaded.

| Key | Description |
| :---: | --- |
| <kbd>F6</kbd> | Copy current method to kill-ring |
| <kbd>F7</kbd> | Transmit current method to Magik session |
| <kbd>F8</kbd> | Transmit current region to Magik session |
| <kbd>F9</kbd> | Mark current method |
| <kbd>F2</kbd>-<kbd>Return</kbd> | Transmit thing at point to Magik session |
| <kbd>F2</kbd>-<kbd>F7</kbd> | Transmit current method to Magik session |
| <kbd>F2</kbd>-<kbd>F8</kbd> | Transmit current region to Magik session |
| <kbd>F2</kbd>-<kbd>#</kbd> | Comment region |
| <kbd>F2</kbd>-<kbd>Esc</kbd> <kbd>#</kbd> | Uncomment region |
| <kbd>F2</kbd>-<kbd>b</kbd> | Transmit buffer to Magik session |
| <kbd>F2</kbd>-<kbd>e</kbd> | Toggle `magik-electric-mode` |
| <kbd>F2</kbd>-<kbd>m</kbd> | Transmit current method to Magik session |
| <kbd>F2</kbd>-<kbd>q</kbd> | Fill public comment |
| <kbd>F2</kbd>-<kbd>r</kbd> | Transmit current region to Magik session |
| <kbd>F2</kbd>-<kbd>s</kbd> | Open Magik version selection |
| <kbd>F2</kbd>-<kbd>t</kbd> | Trace current statement |
| <kbd>F2</kbd>-<kbd>Space</kbd> | Explicitly trigger electric space |
| <kbd>F2</kbd>-<kbd>Tab</kbd> | Hippie expand |
| <kbd>F2</kbd>-<kbd>z</kbd> | Start a Magik session |
| <kbd>F3</kbd>-<kbd>F3</kbd> | Open Class Browser |
| <kbd>F3</kbd>-<kbd>b</kbd> | Paste method and class into Class Browser |
| <kbd>F3</kbd>-<kbd>c</kbd> | Paste class name into Class Browser |
| <kbd>F3</kbd>-<kbd>j</kbd> | Jump to source of method under point |
| <kbd>F3</kbd>-<kbd>m</kbd> | Paste method name into Class Browser |
| <kbd>F3</kbd>-<kbd>/</kbd> | Open Class Browser and clear search |

### magik-version

Major mode for managing multiple Magik environments.

| Key | Description |
| :---: | --- |
| <kbd>Return</kbd> | Select the current selected line as the current active environment. |
| <kbd>Space</kbd> or <kbd>Down</kbd> | Move to the next environment. |
| <kbd>a</kbd> | Open gis_aliases file of selected version. Will prompt for layered product to use if selected version has more than one aliases file available. |
| <kbd>+</kbd> | Add a new entry to the currently open file. |
| <kbd>o</kbd> | Open the `gis_version.txt` file to add or edit the file itself. |
| <kbd>q</kbd> | Quit the buffer. |

### magik-aliases

Major mode for editing Magik aliases files.

| Key | Description |
| :---: | --- |
| <kbd>Shift</kbd>-<kbd>Return</kbd> | Run a Magik session for the selected alias. |
| <kbd>Space</kbd> or <kbd>Down</kbd> | Move to the next alias. |
| <kbd>q</kbd> | Quit the buffer. |

### magik-session

Major mode for running a Magik session as a direct sub-process. Provides command recall, history folding, traceback navigation, output filtering, and drag-and-drop file loading.

| Key | Description |
| :---: | --- |
| <kbd>Return</kbd> | Insert newline, or send command when at the prompt. |
| <kbd>C-a</kbd> | Move to beginning of line, or after the prompt. |
| <kbd>M-p</kbd> | Recall previous command from history. |
| <kbd>M-n</kbd> | Recall next command from history. |
| <kbd>F2</kbd>-<kbd>↑</kbd> or <kbd>F2</kbd>-<kbd>C-p</kbd> | Fold buffer to show command history. |
| <kbd>F2</kbd>-<kbd>↓</kbd> or <kbd>F2</kbd>-<kbd>C-n</kbd> | Unfold buffer. |
| <kbd>F2</kbd>-<kbd>=</kbd> | Print last traceback. |
| <kbd>F2</kbd>-<kbd>f</kbd> | Toggle output filter. |
| <kbd>F2</kbd>-<kbd>p</kbd> | Recall previous command matching current input. |
| <kbd>F2</kbd>-<kbd>n</kbd> | Recall next command matching current input. |
| <kbd>F4</kbd>-<kbd>F4</kbd> | Complete symbol at point. |
| <kbd>F4</kbd>-<kbd>↑</kbd> | Navigate to previous traceback frame. |
| <kbd>F4</kbd>-<kbd>↓</kbd> | Navigate to next traceback frame. |
| <kbd>F4</kbd>-<kbd>$</kbd> | Start an external shell. |
| <kbd>F4</kbd>-<kbd>g</kbd> | Go to the file referenced in the current error. |
| <kbd>F4</kbd>-<kbd>m</kbd> | Copy current method to work buffer. |
| <kbd>F4</kbd>-<kbd>r</kbd> | Copy current region to work buffer. |
| <kbd>F4</kbd>-<kbd>s</kbd> | Insert a debug statement. |
| <kbd>F4</kbd>-<kbd>w</kbd> | Set work buffer name. |
| <kbd>F4</kbd>-<kbd>P</kbd> | Print traceback. |
| <kbd>F4</kbd>-<kbd>S</kbd> | Save traceback to file. |
| <kbd>F8</kbd> | Send command at point. |
| <kbd>C-c C-c</kbd> | Kill the Magik session process. |
| <kbd>C-c C-\</kbd> | Send quit signal to the session. |
| <kbd>C-c C-z</kbd> | Send stop signal to the session. |
| <kbd>C-c C-d</kbd> | Send EOF to the session. |

### magik-mode

Major prog mode for editing Magik code. Provides syntax highlighting, smart indentation, method navigation, pragma management, and direct transmission to a Magik session.

Support for outline-minor mode. Try: `(outline-minor-mode)`

Support for imenu. Try: `(add-hook 'magik-mode-hook 'imenu-add-menubar-index)`

| Key | Description |
| :---: | --- |
| <kbd>Return</kbd> | Newline with indentation. |
| <kbd>Tab</kbd> | Indent current line, or cycle through pragma fields. |
| <kbd>#</kbd> | Smart comment insertion. |
| <kbd>Space</kbd> | Expand yasnippet template at point (if any). |
| <kbd>/</kbd> | Cycle pragma option forward. |
| <kbd>\</kbd> | Cycle pragma option backward. |
| <kbd>C-M-h</kbd> | Mark current method; repeat to extend selection. |
| <kbd>M-↑</kbd> or <kbd>F2</kbd>-<kbd>↑</kbd> | Move to previous method. |
| <kbd>M-↓</kbd> or <kbd>F2</kbd>-<kbd>↓</kbd> | Move to next method. |
| <kbd>F2</kbd>-<kbd>$</kbd> | Transmit current `$` chunk to session. |
| <kbd>F2</kbd>-<kbd>d</kbd> | Check documentation for method at point. |
| <kbd>F2</kbd>-<kbd>D</kbd> | Check documentation for all methods in file. |
| <kbd>F2</kbd>-<kbd>p</kbd> | Check pragma for method at point. |
| <kbd>F2</kbd>-<kbd>P</kbd> | Check pragma for all methods in file. |
| <kbd>F4</kbd>-<kbd>F4</kbd> | Complete symbol at point. |
| <kbd>F4</kbd>-<kbd>c</kbd> | Copy current method to kill-ring. |
| <kbd>F4</kbd>-<kbd>e</kbd> | Compare methods between windows with ediff. |
| <kbd>F4</kbd>-<kbd>m</kbd> | Copy current method to work buffer. |
| <kbd>F4</kbd>-<kbd>n</kbd> | Set work buffer name. |
| <kbd>F4</kbd>-<kbd>r</kbd> | Copy current region to work buffer. |
| <kbd>F4</kbd>-<kbd>s</kbd> | Insert a debug statement. |
| <kbd>F4</kbd>-<kbd>w</kbd> | Compare methods between windows. |

### magik-ts-mode

Major Mode for Using Tree-sitter with Magik

You can use <kbd>M</kbd>-<kbd>x</kbd> `treesit-install-language-grammar` to install and build the grammar, or download the latest prebuilt artifact from [GitHub](https://github.com/krn-robin/tree-sitter-magik/releases/latest) for your operating system.
If downloading from GitHub, place the extracted file in `~/.emacs.d/tree-sitter`.

With `magik-ts-mode` enabled, you can use <kbd>M</kbd>-<kbd>x</kbd> `treesit-explore-mode` to view the syntax tree of the current buffer.

For more information about Tree-sitter, see the Mastering Emacs [tutorial](https://www.masteringemacs.org/article/how-to-get-started-tree-sitter).

### magik-cb

Major mode for the Magik Class Browser. Searches for methods and classes using a running `method_finder` process and lets you jump directly to source.

**Global keys** (set by `magik-global-bindings`):

| Key | Description |
| :---: | --- |
| <kbd>F3</kbd>-<kbd>F3</kbd> | Open Class Browser. |
| <kbd>F3</kbd>-<kbd>b</kbd> | Paste method and class name into Class Browser. |
| <kbd>F3</kbd>-<kbd>c</kbd> | Paste class name into Class Browser. |
| <kbd>F3</kbd>-<kbd>j</kbd> | Jump to source of method at point. |
| <kbd>F3</kbd>-<kbd>m</kbd> | Paste method name into Class Browser. |
| <kbd>F3</kbd>-<kbd>/</kbd> | Open Class Browser and clear the search. |

**Keys inside the Class Browser buffer:**

| Key | Description |
| :---: | --- |
| <kbd>Space</kbd> or <kbd>F3</kbd>-<kbd>h</kbd> | Quit the Class Browser. |
| <kbd>;</kbd> or <kbd>F3</kbd>-<kbd>s</kbd> | Edit topics and flags. |
| <kbd>/</kbd> | Clear the search field. |
| <kbd>Tab</kbd> | Cycle to the next input field. |
| <kbd>Mouse-2</kbd> or <kbd>F3</kbd>-<kbd>j</kbd> | Jump to source. |
| <kbd>F3</kbd>-<kbd>↑</kbd> | Fold (collapse) the result list. |
| <kbd>F3</kbd>-<kbd>↓</kbd> | Unfold (expand) the result list. |
| <kbd>F3</kbd>-<kbd>$</kbd> | Open GIS shell. |
| <kbd>F3</kbd>-<kbd>f</kbd> | Show family tree of class under point. |
| <kbd>F3</kbd>-<kbd>F</kbd> or <kbd>F3</kbd>-<kbd>o</kbd> | Toggle override flags. |
| <kbd>F3</kbd>-<kbd>T</kbd> | Toggle override topics. |
| <kbd>F3</kbd>-<kbd>t</kbd> | Toggle all topics. |
| <kbd>F3</kbd>-<kbd>2</kbd> | Toggle 200-result limit. |
| <kbd>F3</kbd>-<kbd>g</kbd> | Switch to associated GIS session buffer. |
| <kbd>F3</kbd>-<kbd>l</kbd> | Cycle to next inheritance setting. |
| <kbd>F3</kbd>-<kbd>r</kbd> | Reset Class Browser. |

### magik-module

Major mode for editing Magik `module.def` files. Provides commands to load and manage modules from within Emacs.

| Key | Description |
| :---: | --- |
| <kbd>F2</kbd>-<kbd>b</kbd> | Load (transmit) the module to the Magik session. |
| <kbd>F2</kbd>-<kbd>c</kbd> | Compile messages for the module. |
| <kbd>F2</kbd>-<kbd>d</kbd> | Reload the module definition. |
| <kbd>F2</kbd>-<kbd>m</kbd> | Toggle the save-magikc option. |
| <kbd>F2</kbd>-<kbd>r</kbd> | Toggle the force-reload option. |
| <kbd>F2</kbd>-<kbd>R</kbd> | Remove the module from the session. |

### magik-product

Major mode for editing Magik `product.def` files.

| Key | Description |
| :---: | --- |
| <kbd>F2</kbd>-<kbd>b</kbd> | Load (transmit) the product to the Magik session. |
| <kbd>F2</kbd>-<kbd>r</kbd> | Reinitialise the product. |

### magik-msg

Major mode for editing Magik message files (`.msg`, `.hmsg`). Provides navigation between messages and direct compilation.

| Key | Description |
| :---: | --- |
| <kbd>F2</kbd>-<kbd>↑</kbd> | Move to previous message. |
| <kbd>F2</kbd>-<kbd>↓</kbd> | Move to next message. |
| <kbd>F2</kbd>-<kbd>b</kbd> | Transmit buffer to Magik session. |
| <kbd>F2</kbd>-<kbd>c</kbd> | Compile module messages. |
| <kbd>F2</kbd>-<kbd>m</kbd> | Mark current message. |

### magik-loadlist

Major mode for editing Magik `load_list.txt` and `patch_list.txt` files. Provides syntax highlighting that distinguishes file stems from subdirectory entries, and can synchronise the list against the actual directory contents.

| Key | Description |
| :---: | --- |
| <kbd>F2</kbd>-<kbd>b</kbd> | Transmit the load list to the Magik session (`load_file_list`). |
| <kbd>C-c r</kbd> | Refresh the buffer contents from its directory, prompting to add, remove, or update entries. With a prefix argument, accept all changes without prompting. |

### magik-trn

Major mode for editing Magik translation files (`.trn`).

| Key | Description |
| :---: | --- |
| <kbd>F2</kbd>-<kbd>b</kbd> | Transmit translation buffer to Magik session. |

### magik-doc-gen

Automatically fills in missing documentation stubs in Magik methods and exemplars. Two documentation formats are supported:

**sw-method-doc** — the native Magik convention. Parameter names are written in `## UPPERCASE` lines inside the method body. Missing parameters are detected by comparing the method signature against existing `##` lines and inserted automatically.

**type-doc** — a structured annotation style using `## @param {:} name` for parameters, `## @return {:}` for return values, and `## @slot {:} name` for exemplar slots.

Commands are bound in `magik-mode` and accessible via <kbd>F2</kbd>-<kbd>d</kbd> / <kbd>F2</kbd>-<kbd>D</kbd>:

| Key | Command | Description |
| :---: | --- | --- |
| <kbd>F2</kbd>-<kbd>d</kbd> | `magik-single-method-sw-method-doc` | Insert missing sw-method-doc stubs for the method at point. |
| <kbd>F2</kbd>-<kbd>D</kbd> | `magik-file-sw-method-doc` | Insert missing sw-method-doc stubs for all methods in the file. |

Additional commands available via <kbd>M</kbd>-<kbd>x</kbd>:

| Command | Description |
| --- | --- |
| `magik-single-method-type-doc` | Insert missing type-doc stubs for the method at point. |
| `magik-file-type-doc` | Insert missing type-doc stubs for all methods and exemplar slots in the file. |
| `magik-single-exemplar-type-doc` | Insert missing `@slot` stubs for the exemplar nearest point. |

### magik-lint

[Magik-lint](https://github.com/StevenLooman/sonar-magik/tree/master/magik-lint) support.

To enable automatic linting in `magik-mode` buffers, the following conditions have to be met:

* Package [flycheck](https://flycheck.org) needs to be installed and loaded.
* `magik-lint-VERSION.jar` ([download](https://github.com/StevenLooman/sonar-magik/releases)) has to be installed in `~/.emacs.d/magik-lint/` (location can be customized with the variable `magik-lint-jar-file`).
* The `java` executable path should be in `exec-path`, or the variable `flycheck-magik-lint-java-executable` has to be set. `flycheck-magik-lint-java-executable` will automatically be set when the environment variable `JAVA_HOME` is set.
* `flycheck-mode` has to be enabled for `magik-mode` buffers. Or use `global-flycheck-mode` to enable it for all buffers.

### YASnippet

`yasnippet` is a required dependency and its snippets are loaded automatically.
In `magik-mode`, `magik-ts-mode`, and `magik-session-mode`, the <kbd>Space</kbd> key is bound to `magik-yas-maybe-expand`: if the word before point matches a snippet key it expands the snippet, otherwise it inserts a regular space.
Use <kbd>Tab</kbd> to jump between fields inside an expanded snippet.

The snippets below are available in `magik-mode`, `magik-ts-mode`, and `magik-session-mode`. `magik-module-mode`, `magik-product-mode`, and `magik-trn-mode` have their own dedicated snippet sets listed after.

**magik-mode / magik-ts-mode / magik-session-mode snippets, organised into groups:**

#### Methods

| Key | Snippet |
| --- | --- |
| `method` | `_method` / `_endmethod` with pragma and docstring |
| `private` | `_private _method` / `_endmethod` |
| `abstract` | `_abstract _method` / `_endmethod` |
| `iter` | `_iter _method` / `_endmethod` with `_loopbody` |

#### Control structures

| Key | Snippet |
| --- | --- |
| `if` | `_if` / `_then` / `_endif` |
| `ife` | `_if` / `_then` / `_else` / `_endif` |
| `ifel` | `_if` / `_then` / `_elif` / `_endif` |
| `ifele` | `_if` / `_then` / `_elif` / `_else` / `_endif` |
| `if_ol` | `_if ... _then ... _endif` (one line) |
| `elif` | `_elif` / `_then` |
| `for` | `_for` / `_over` / `_loop` / `_endloop` |
| `over` | `_over` / `_loop` / `_endloop` |
| `while` | `_while` / `_loop` / `_endloop` |
| `loop` | `_loop` / `_endloop` |
| `loopbody` | `_loopbody(...)` |
| `block` | `_block` / `_endblock` |
| `protect` | `_protect` / `_protection` / `_endprotect` |
| `protect_locking` | `_protect` with `_lock` / `_endlock` inside |
| `lock` | `_lock` / `_endlock` |
| `try` | `_try` / `_when` / `_endtry` |
| `try_with` | `_try` / `_when` / `_with` / `_endtry` |
| `catch` | `_catch` / `_endcatch` |
| `throw` | `_throw` / `_endthrow` |
| `throw_with` | `_throw` / `_with` / `_endthrow` |
| `proc` | `_proc()` / `_endproc` |
| `proc_ol` | `_proc(...) ... _endproc` (one line) |

#### Object-oriented

| Key | Snippet |
| --- | --- |
| `def_slotted_exemplar` | Full exemplar definition with slots and parents |
| `def_indexed_exemplar` | Indexed exemplar definition |
| `def_mixin` | Mixin definition |
| `construct` | Filled `new()` and `init()` method pair with slot assignments |
| `clone` | `_clone.$0` |
| `.` | `_self.$0` |
| `super` | `_super.$0` |
| `remex` | `remex(:filename)` using the current file name |
| `add_child` | `add_child(...)` call |

#### Slots and properties

| Key | Snippet |
| --- | --- |
| `slot` | `{:name, _unset}` slot entry |
| `define_slot_access` | `define_slot_access(...)` |
| `define_slot_externally_readable` | `define_slot_externally_readable(...)` |
| `define_slot_externally_writable` | `define_slot_externally_writable(...)` |
| `define_pseudo_slot` | `define_pseudo_slot(...)` |
| `define_shared_constant` | `define_shared_constant(...)` |
| `define_shared_variable` | `define_shared_variable(...)` |
| `define_print_attributes` | `define_print_attributes(...)` |
| `define_show_attributes` | `define_show_attributes(...)` |
| `define_binary_operator_case` | `define_binary_operator_case(...)` |
| `def_property` | Property definition |

#### Documentation and pragmas

| Key | Snippet |
| --- | --- |
| `pragma` | `_pragma(classify_level=, topic={}, usage={})` |
| `doc` | Single `##` docstring line |
| `dob` | Multi-line docstring block |

#### Conditions and messages

| Key | Snippet |
| --- | --- |
| `define_condition` | `condition.define_condition(...)` |
| `raise` | `condition.raise(...)` |
| `message_handler` | `message_handler.new(...)` using the current class name |

#### Keywords

| Key | Snippet |
| --- | --- |
| `pack` | `_package <name>` |

**magik-module-mode snippets:**

| Key | Snippet |
| --- | --- |
| `new` | New `module.def` skeleton |
| `description` | `description` block |
| `requires` | `requires` dependency entry |
| `optional` | `optional` dependency entry |
| `required_by` | `required_by` entry |
| `install_requires` | `install_requires` entry |
| `requires_datamodel` | `requires_datamodel` entry |
| `requires_java` | `requires_java` entry |
| `language` | `language <name>` |
| `templates` | `templates` block |
| `test` | `test` block |
| `tests_modules` | `tests_modules` block |
| `condition_message_accessor` | `condition_message_accessor <name>` |
| `dnt` | `do_not_translate` |
| `hidd` | `hidden` |
| `messages` | `messages` block (deprecated) |

**magik-product-mode snippets:**

| Key | Snippet |
| --- | --- |
| `new` | New `product.def` skeleton |
| `description` | `description` block |
| `title` | `title` block |
| `version` | `version major.minor.patch` |
| `requires` | `requires` dependency entry |
| `dnt` | `do_not_translate` |

**magik-trn-mode snippets:**

| Key | Snippet |
| --- | --- |
| `enumerator` | `enumerator` translation block |
| `external_name` | `external_name` translation block |
| `field_value` | `field_value` translation block |

### magik-electric-mode

Minor mode providing the older electric template system. Use <kbd>F2</kbd>-<kbd>Space</kbd> to trigger an electric expansion explicitly at point, regardless of whether `magik-electric-mode` is enabled.
Toggle the mode with <kbd>F2</kbd>-<kbd>e</kbd> or <kbd>M</kbd>-<kbd>x</kbd> `magik-electric-mode`.

## Usage with Smallworld 4.x or older

If you plan to use this package with Smallworld-Versions 4.x or older, you should consider the following points:

* Customize the variable `magik-session-auto-insert-dollar` to non-nil.
* You might customize the variable `magik-aliases-layered-products-file` to `"$SMALLWORLD_GIS/product/config/LAYERED_PRODUCTS"`.
If you also want to use EMACS for Smallworld 5.x, it's easier to create the directory `$SMALLWORLD_GIS/../smallworld_registry` and copy or soft-link the original LAYERED_PRODUCTS file there. This ensures the same structure as in Smallworld 5.x.
* There is no support (yet) for the Smallworld dev-tools. So if you want to do things like <kbd>F4</kbd>-<kbd>d</kbd> to start debugging a method, you may still want to use the EMACS which has been delivered with the Smallworld 4.x (or older) software.
* Some more things which are at least partly not supported by Smallworld 5.x are not supported (e.g. `deep-print`)

## Side effects

Some keys bindings are changed with respect to a standard EMACS installation, at least when using `(magik-global-bindings)`:

| Key | Function in standard EMACS | Change in Magik Mode package |
| :---: | --- | --- |
| <kbd>F2</kbd> | `2C-command`| globally changed to prefix key |
| <kbd>F3</kbd> | `kmacro-start-macro-or-insert-counter` | globally changed to prefix key |
| <kbd>F4</kbd> | `kmacro-end-or-call-macro` | used in magik-mode and magik-session-mode as prefix key |

The reason for that is, that many Magik developers are familiar with these bindings from former EMACS installations.

For quick usage of the keyboard-macro functions you may e.g. bind the <kbd>Ctrl</kbd>-<kbd>F2</kbd> and <kbd>Ctrl</kbd>-<kbd>F4</kbd> combinations by putting the following lines into your  `.emacs` file:

```emacs-lisp
(global-set-key [C-f3] 'kmacro-start-macro-or-insert-counter)
(global-set-key [C-f4] 'kmacro-end-or-call-macro)
```

## Familiar with SW 4.x EMACS? Some tips for you

If you've been using the EMACS delivered by GE with Smallworld version 4.3 and earlier, you might want to customize some variables with default values changed with respect to the former "GE"-EMACS:

| actual variable | actual default | former variable | former default | Remarks |
| :---: | --- | --- | --- | --- |
| `magik-electric-mode` | t | `electric-magik-mode` | nil | If non-nil, typing <kbd>Space</kbd> after keywords like `if` inserts the matching control structure. If nil, you have to use <kbd>F2</kbd>-<kbd>Space</kbd> to achieve the same behaviour. |
| `mouse-drag-copy-region` | nil | dto. | t | Paste a mouse selection (even to other programs). (Default has been changed in standard EMACS) |
| `show-paren-mode` | nil | dto. | t | if point is on opening or after closing bracket, show the matching one. (GE's EMACS had done this customization) |

Eventually you'll find some more customazations in of the former GE's EMACS' installation in emacs/site-lisp/smallworld/sw_defaults.el, but you'll have to check, whether these will work the the EMACS installation you are using now.
