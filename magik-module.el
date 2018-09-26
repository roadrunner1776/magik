;;; magik-module.el --- mode for editing Magik module.def files.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl)
		   (require 'easymenu)
		   (require 'font-lock)
		   (require 'magik-utils)
		   (require 'magik-shell))

(defgroup magik-module nil
  "Customise Magik module.def files group."
  :group 'magik
  :group 'tools)

(defcustom magik-module-mode-hook nil
  "*Hook to run after Module Mode is set."
  :group 'module
  :type  'hook)

(defcustom magik-module-option-save-magikc t
  "*If t, save .magikc files when loading module."
  :group 'smallworld
  :type  'boolean)

(defcustom magik-module-option-force-reload t
  "*If t, save .magikc files when loading module."
  :group 'smallworld
  :type  'boolean)

(defvar magik-module-mode-map (make-sparse-keymap)
  "Keymap for Magik module.def files")

(defvar magik-module-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik module.def buffers")

(fset 'magik-module-f2-map   magik-module-f2-map)

(define-key magik-module-mode-map [f2]    'magik-module-f2-map)

(define-key magik-module-f2-map   "b"     'magik-module-transmit-buffer)
(define-key magik-module-f2-map   "c"     'magik-module-compile-messages)
(define-key magik-module-f2-map   "d"     'magik-module-reload-module-definition)
(define-key magik-module-f2-map   "s"     'magik-module-toggle-save-magikc)
(define-key magik-module-f2-map   "r"     'magik-module-toggle-force-reload)
(define-key magik-module-f2-map   "R"     'magik-module-toggle-remove-module)

(defvar magik-module-menu nil
  "Keymap for the Magik module.def buffer menu bar")

(easy-menu-define magik-module-menu magik-module-mode-map
  "Menu for Module mode."
  `(,"Module"
    [,"Load"
     magik-module-transmit-buffer
     :active (magik-utils-buffer-mode-list 'magik-shell-mode)
     :keys "f2 b"]
    [,"Reload definition"
     magik-module-reload-module-definition
     :active (magik-utils-buffer-mode-list 'magik-shell-mode)
     :keys "f2 d"]
    [,"Compile messages"
     magik-module-compile-messages
     :active (magik-utils-buffer-mode-list 'magik-shell-mode)
     :keys "f2 c"]
    [,"Remove"
     magik-module-remove-module
     :active (magik-utils-buffer-mode-list 'magik-shell-mode)
     :keys "f2 R"]
    (,"Set Options..."
     [,"Set :save_magikc? to _false"
      (magik-module-toggle-save-magikc -1)
      :active (magik-utils-buffer-mode-list 'magik-shell-mode)
      :style radio
      :selected (null module-option-save-magikc)
      :keys "f2 s, M-- M-1 f2 s"]
     [,"Set :save_magikc? to _true"
      (magik-module-toggle-save-magikc 1)
      :active (magik-utils-buffer-mode-list 'magik-shell-mode)
      :style radio
      :selected module-option-save-magikc
      :keys "f2 s, M-1 f2 s"]
     "---"
     [,"Set :force_reload? to _false"
      (magik-module-toggle-force-reload -1)
      :active (magik-utils-buffer-mode-list 'magik-shell-mode)
      :style radio
      :selected (null module-option-force-reload)
      :keys "f2 r, M-- M-1 f2 r"]
     [,"Set :force_reload? to :prerequisites"
      (magik-module-toggle-force-reload 'prerequisites)
      :active (magik-utils-buffer-mode-list 'magik-shell-mode)
      :style radio
      :selected (eq module-option-force-reload 'prerequisites)
      :keys "C-u f2 r"]
     [,"Set :force_reload? to _true"
      (magik-module-toggle-force-reload 1)
      :active (magik-utils-buffer-mode-list 'magik-shell-mode)
      :style radio
      :selected (eq module-option-force-reload t)
      :keys "f2 r, M-1 f2 r"])
    "---"
    [,"Customize"                     magik-module-customize   t]
    [,"Help"                          magik-module-help        t]))

(define-key magik-module-mode-map [f1] 'magik-module-help)

(defvar magik-module-mode-syntax-table nil
  "Syntax table in use in Module Mode buffers.")

;; Imenu configuration
(defvar magik-module-imenu-generic-expression
  '(
    (nil "^\\(\\sw+\\)\\s-*\n\\(.\\|\n\\)*\nend\\s-*$" 1)
    )
  "Imenu generic expression for Magik Message mode.  See `imenu-generic-expression'.")

;; Font-lock configuration
(defcustom magik-module-font-lock-keywords
  (list
   '("^end\\s-*$" . font-lock-keyword-face)
   '("^hidden$" . font-lock-keyword-face)
   '("^\\(language\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-type-face))
   '("^\\(\\sw+\\)\\s-*$" . font-lock-variable-name-face)
   '("^\\(\\sw+\\s-*\\sw*\\)\\s-*\\([0-9]*\\s-*[0-9]*\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-constant-face))
   )
  "Default fontification of module.def files."
  :group 'module
  :type 'sexp)

(defun magik-module-help ()
  "Display help on how to use the Module Mode interface."
  (interactive)
  (sw-help-open sw-help-module-id))

(defun magik-module-customize ()
  "Open Customization buffer for Module Mode."
  (interactive)
  (customize-group 'magik-module))

;;;###autoload
(defun magik-module-mode ()
  "Major mode for editing Magik module.def files.

You can customise Module Mode with the `module-mode-hook'."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)

  (use-local-map magik-module-mode-map)
  (easy-menu-add magik-module-menu)
  (set-syntax-table magik-module-mode-syntax-table)

  (setq major-mode 'magik-module-mode
	mode-name "Module"
	require-final-newline t
	imenu-generic-expression magik-module-imenu-generic-expression
	font-lock-defaults
	'(magik-module-font-lock-keywords
	  nil t))

  (run-hooks 'magik-module-mode-hook))

(defun magik-module-toggle-save-magikc (arg)
  "Toggle saving of .magikc files when loading module."
  (interactive "P")
  (setq magik-module-option-save-magikc
	(if (null arg)
	    (not magik-module-option-save-magikc)
	  (> (prefix-numeric-value arg) 0)))
  (message "Set :save_magikc? to %s"
	   (magik-function-convert magik-module-option-save-magikc)))

(defun magik-module-toggle-force-reload (arg)
  "Toggle force_reload? option when loading module.
If called with a non-integer prefix key then the :prerequisites
option is set."
  (interactive "P")
  (setq magik-module-option-force-reload
	(cond ((null arg)
	       (not magik-module-option-force-reload))
	      ((symbolp arg)
	       arg)
	      ((and current-prefix-arg (not (integerp current-prefix-arg)))
	       'prerequisites)
	      (t
	       (> (prefix-numeric-value arg) 0))))

  (message "Set :force_reload? option to %s"
	   (magik-function-convert magik-module-option-force-reload)))

(defun magik-module-name ()
  "Return current Module's name as a string."
  (save-excursion
    (goto-char (point-min))
    (current-word)))

(defun magik-module-reload-module-definition (&optional gis)
  "Reload the module definition in the GIS process."
  (interactive)
  (let* ((gis (magik-utils-get-buffer-mode gis
					   'magik-shell-mode
					   "Enter Gis process buffer:"
					   magik-shell-buffer
					   'magik-shell-buffer-alist-prefix-function))
	 (module (intern (concat "|" (magik-module-name) "|")))
	 (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat
      (magik-function "sw_module_manager.reload_module_definition" module) ;include version number?
      "$\n"))
    gis))

(defun magik-module-compile-messages (&optional gis)
  "Compile all the Module's messages in the GIS process."
  (interactive)
  (let* ((gis (magik-utils-get-buffer-mode gis
					   'magik-shell-mode
					   "Enter Gis process buffer:"
					   magik-shell-buffer
					   'magik-shell-buffer-alist-prefix-function))
	 (module (intern (concat "|" (magik-module-name) "|")))
	 (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat
      (magik-function
       "_proc(module_name, version)
	 _if (a_module << sw_module_manager.module(module_name, version, _true)) _isnt _unset
	 _then
	   sw_module_manager.compile_messages(a_module)
	 _endif
       _endproc"
       module 'unset) ;include version number?
      "\n$\n"))
    gis))

(defun magik-module-remove-module (&optional gis)
  "Remove the module definition from the GIS process.
If module definition is not known to the Magik GIS it is loaded as
a standalone module."
  (interactive)
  (let* ((gis (magik-utils-get-buffer-mode gis
					   'magik-shell-mode
					   "Enter Gis process buffer:"
					   magik-shell-buffer
					   'magik-shell-buffer-alist-prefix-function))
	 (module (intern (concat "|" (magik-module-name) "|")))
	 (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat
      (magik-function "sw_module_manager.remove_module" module)
      "$\n"))
    gis))

(defun magik-module-transmit-load-module (filename process)
  "Load the module into the GIS process.
If module definition is not known to the Magik GIS it is loaded as
a standalone module."
  (let ((module (intern (concat "|" (magik-module-name) "|"))))
    (process-send-string
     process
     (concat
      "_try\n"
      (magik-function "sw_module_manager.load_module" module 'unset
		      'save_magikc?  magik-module-option-save-magikc
		      'force_reload? magik-module-option-force-reload)
      "_when sw_module_no_such_module\n"
      (magik-function "sw_module_manager.load_standalone_module_definition" filename
		      'save_magikc?  magik-module-option-save-magikc
		      'force_reload? magik-module-option-force-reload)
      "_endtry\n"
      "$\n"))))

(defun magik-module-transmit-buffer (&optional gis)
  "Send current buffer to GIS."
  (interactive)
  (let* ((gis (magik-utils-get-buffer-mode gis
					   'magik-shell-mode
					   "Enter Gis process buffer:"
					   magik-shell-buffer
					   'magik-shell-buffer-alist-prefix-function))
	 (process (barf-if-no-gis gis))
	 (filename (buffer-file-name)))
    (display-buffer gis t)
    (magik-module-transmit-load-module filename process)
    gis))

(defun magik-module-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop GIS mode.
Called by `gis-drag-n-drop-load' when a Module file is dropped."
  (let ((process (barf-if-no-gis gis)))
    (magik-module-transmit-load-module filename process)
    gis))

;;; Package initialisation
(if magik-module-mode-syntax-table
    nil
  (setq magik-module-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" magik-module-mode-syntax-table)
  (modify-syntax-entry ?# "<" magik-module-mode-syntax-table)
  (modify-syntax-entry ?\n ">" magik-module-mode-syntax-table))

;;; Package registration

(or (assoc "module\\.def$" auto-mode-alist)
    (push '("module\\.def$" . magik-module-mode) auto-mode-alist))

(provide 'magik-module)
;;; module.el ends here
