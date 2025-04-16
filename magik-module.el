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

(eval-when-compile
  (require 'easymenu)
  (require 'font-lock)
  (require 'magik-utils)
  (require 'magik-session))

(require 'compat)

(defgroup magik-module nil
  "Customise Magik module.def files group."
  :group 'magik
  :group 'tools)

(defcustom magik-module-option-save-magikc t
  "*If t, save .magikc files when loading module."
  :group 'smallworld
  :type  'boolean)

(defcustom magik-module-option-force-reload t
  "*If t, save .magikc files when loading module."
  :group 'smallworld
  :type  'boolean)

;; Imenu configuration
(defvar magik-module-imenu-generic-expression
  '((nil "^\\(\\sw+\\)\\s-*\n\\(.\\|\n\\)*\nend\\s-*$" 1))
  "Imenu generic expression for Magik Message mode.
See `imenu-generic-expression'.")

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
     (2 font-lock-constant-face)))
  "Default fontification of module.def files."
  :group 'module
  :type 'sexp)

(defun magik-module-customize ()
  "Open Customization buffer for Module Mode."
  (interactive)
  (customize-group 'magik-module))

;;;###autoload
(define-derived-mode magik-module-mode nil "Module"
  "Major mode for editing Magik module.def files.

You can customize Module Mode with the `magik-module-mode-hook`.

\\{magik-module-mode-map}"

  :group 'magik
  :abbrev-table nil

  (compat-call setq-local
               require-final-newline t
               imenu-generic-expression magik-module-imenu-generic-expression
               font-lock-defaults '(magik-module-font-lock-keywords nil t)))

(defvar magik-module-menu nil
  "Keymap for the Magik module.def buffer menu bar.")

(easy-menu-define magik-module-menu magik-module-mode-map
  "Menu for Module mode."
  `(,"Module"
    [,"Load"               magik-module-transmit-buffer          (magik-utils-buffer-mode-list 'magik-session-mode)]
    [,"Reload definition"  magik-module-reload-module-definition (magik-utils-buffer-mode-list 'magik-session-mode)]
    [,"Compile messages"   magik-module-compile-messages         (magik-utils-buffer-mode-list 'magik-session-mode)]
    [,"Remove"             magik-module-remove-module            (magik-utils-buffer-mode-list 'magik-session-mode)]
    (,"Set Options..."
     [,"Set :save_magikc? to _false"
      (magik-module-toggle-save-magikc -1)
      :active (magik-utils-buffer-mode-list 'magik-session-mode)
      :style radio
      :selected (null module-option-save-magikc)
      :keys "M-- M-1 <f2> m,   <f2> m"]
     [,"Set :save_magikc? to _true"
      (magik-module-toggle-save-magikc 1)
      :active (magik-utils-buffer-mode-list 'magik-session-mode)
      :style radio
      :selected module-option-save-magikc
      :keys "M-1 <f2> m,   <f2> m"]
     "---"
     [,"Set :force_reload? to _false"
      (magik-module-toggle-force-reload -1)
      :active (magik-utils-buffer-mode-list 'magik-session-mode)
      :style radio
      :selected (null module-option-force-reload)
      :keys "M-- M-1 <f2> r,   <f2> r"]
     [,"Set :force_reload? to :prerequisites"
      (magik-module-toggle-force-reload 'prerequisites)
      :active (magik-utils-buffer-mode-list 'magik-session-mode)
      :style radio
      :selected (eq module-option-force-reload 'prerequisites)
      :keys "C-u <f2> r"]
     [,"Set :force_reload? to _true"
      (magik-module-toggle-force-reload 1)
      :active (magik-utils-buffer-mode-list 'magik-session-mode)
      :style radio
      :selected (eq module-option-force-reload t)
      :keys "M-1 <f2> r,   <f2> r"])
    "---"
    [,"Customize"                     magik-module-customize   t]))

(defvar magik-module-mode-syntax-table nil
  "Syntax table in use in Module Mode buffers.")

(defun magik-module-toggle-save-magikc (boolean)
  "Toggle saving of .magikc files when loading module using BOOLEAN."
  (interactive "P")
  (setq magik-module-option-save-magikc
        (if (null boolean)
            (not magik-module-option-save-magikc)
          (> (prefix-numeric-value boolean) 0)))
  (message "Set :save_magikc? to %s"
           (magik-function-convert magik-module-option-save-magikc)))

(defun magik-module-toggle-force-reload (boolean)
  "Toggle force_reload? option when loading module using BOOLEAN.
If called with a non-integer prefix key then the :prerequisites
option is set."
  (interactive "P")
  (setq magik-module-option-force-reload
        (cond ((null boolean)
               (not magik-module-option-force-reload))
              ((symbolp boolean)
               boolean)
              ((and current-prefix-arg (not (integerp current-prefix-arg)))
               'prerequisites)
              (t
               (> (prefix-numeric-value boolean) 0))))

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
                                           'magik-session-mode
                                           "Enter Magik Session buffer:"
                                           magik-session-buffer
                                           'magik-session-buffer-alist-prefix-function))
         (module (intern (concat "|" (magik-module-name) "|")))
         (process (barf-if-no-gis gis)))
    (message "%s reloaded in buffer %s." (magik-module-name) gis)
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
                                           'magik-session-mode
                                           "Enter Magik Session buffer:"
                                           magik-session-buffer
                                           'magik-session-buffer-alist-prefix-function))
         (module (intern (concat "|" (magik-module-name) "|")))
         (process (barf-if-no-gis gis)))
    (message "Compiled messages for %s in buffer %s." (magik-module-name) gis)
    (display-buffer gis t)
    (process-send-string
     process
     (concat
      (magik-function
       "_proc(module_name, version)
   _if (a_module << sw_module_manager.module(module_name, version, _true)) _isnt _unset
   _then
     a_module.compile_messages()
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
                                           'magik-session-mode
                                           "Enter Magik Session buffer:"
                                           magik-session-buffer
                                           'magik-session-buffer-alist-prefix-function))
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
  "Load the module FILENAME into the GIS PROCESS.
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
                                           'magik-session-mode
                                           "Enter Magik Session buffer:"
                                           magik-session-buffer
                                           'magik-session-buffer-alist-prefix-function))
         (process (barf-if-no-gis gis))
         (filename (buffer-file-name)))
    (message "%s loaded in buffer %s." (magik-module-name) gis)
    (display-buffer gis t)
    (magik-module-transmit-load-module filename process)
    gis))

(defun magik-module-drag-n-drop-load (gis filename)
  "Interface to Drag and Drop GIS mode.
Called by `magik-session-drag-n-drop-load' when a Module FILENAME is dropped."
  (let ((process (barf-if-no-gis gis)))
    (magik-module-transmit-load-module filename process)
    gis))

;;; Package initialisation
(modify-syntax-entry ?_ "w" magik-module-mode-syntax-table)
(modify-syntax-entry ?# "<" magik-module-mode-syntax-table)
(modify-syntax-entry ?\n ">" magik-module-mode-syntax-table)

;;; Package registration

;;;###autoload
(add-to-list 'auto-mode-alist '("module.def\\'" . magik-module-mode))

(defvar magik-module-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik module.def buffers.")

(progn
  ;; ------------------------ magik module mode ------------------------

  (fset 'magik-module-f2-map   magik-module-f2-map)

  (define-key magik-module-mode-map [f2]    'magik-module-f2-map)

  (define-key magik-module-f2-map   "b"     'magik-module-transmit-buffer)
  (define-key magik-module-f2-map   "c"     'magik-module-compile-messages)
  (define-key magik-module-f2-map   "d"     'magik-module-reload-module-definition)
  (define-key magik-module-f2-map   "m"     'magik-module-toggle-save-magikc)
  (define-key magik-module-f2-map   "r"     'magik-module-toggle-force-reload)
  (define-key magik-module-f2-map   "R"     'magik-module-remove-module))

(provide 'magik-module)
;;; magik-module.el ends here
