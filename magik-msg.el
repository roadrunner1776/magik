;;; magik-msg.el --- mode for editing Magik msg and hmsg Message files.

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
  (defvar msb-menu-cond)

  (require 'magik-utils)
  (require 'magik-session))

(defgroup magik-msg nil
  "Customise Magik Messages group."
  :group 'magik
  :group 'tools)

;; Imenu configuration
(defvar magik-msg-imenu-generic-expression
  '((nil "^:\\(\\sw+\\)" 1) ;;Normal messages
    (nil "^:\\s$\\(\\S$+\\)\\s$" 1) ;; | | Quoted messages
    ("Groups" "^+\\s-+\\(\\sw+\\)" 1))
  "Imenu generic expression for Magik Message mode.
See `imenu-generic-expression'.")

;; Font-lock configuration
(defcustom magik-msg-font-lock-keywords
  (list
   '("^\\(:\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)\\s-+\\(:\\sw+\\)"
     (1 font-lock-function-name-face)
     (3 font-lock-constant-face))
   '("^\\([+]\\)\\s-+\\(\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?\\)"
     (1 font-lock-type-face)
     (2 font-lock-keyword-face))
   '("^:\\sw*\\(\\s$\\S$*\\s$\\sw*\\)?" . font-lock-function-name-face)
   '("^#%\\s-*text_encoding.*$" . font-lock-warning-face)
   '("#[0-9]+" . font-lock-variable-name-face)
   '("#.*" . font-lock-comment-face))
  "Default fontification of Magik Messages."
  :group 'msg
  :type 'sexp)

(defun magik-msg-customize ()
  "Open Customization buffer for Msg Mode."
  (interactive)
  (customize-group 'msg))

(defun magik-msg-forward-message ()
  "Put point at beginning of line of next message."
  (interactive)
  (if (not (looking-at "^:"))
      (re-search-forward "^:" nil t)
    (forward-char 1)
    (or (re-search-forward "^:" nil t)
        (goto-char (point-max))))
  (beginning-of-line))

(defun magik-msg-backward-message ()
  "Put point at beginning of line of previous message."
  (interactive)
  (if (not (looking-at "^:"))
      (re-search-backward "^:" nil t)
    (backward-char 1)
    (or (re-search-backward "^:" nil t)
        (goto-char (point-min))))
  (beginning-of-line))

(defun magik-msg-mark-message ()
  "Mark the current message."
  ;; also returns the value of mark.
  (interactive)
  (push-mark (point))
  (magik-msg-forward-message)
  (if (save-match-data (string-match "hmsg$" (buffer-name)))
      nil
    (re-search-backward "^\n" (- (point) 1) t))
  (push-mark (point) nil t)
  (magik-msg-backward-message))

;;;###autoload
(define-derived-mode magik-msg-mode nil "Message"
  "Major mode for editing Magik Message files.

You can customize msg-mode with the `magik-msg-mode-hook`.

\\{magik-msg-mode-map}"
  :group 'magik
  :abbrev-table nil

  (compat-call setq-local
               require-final-newline t
               imenu-generic-expression magik-msg-imenu-generic-expression
               font-lock-defaults '(magik-msg-font-lock-keywords nil t)
               outline-regexp "^:\\(\\sw+\\).*"))

(defvar magik-msg-menu nil
  "Keymap for the Magik Message buffer menu bar.")

(easy-menu-define magik-msg-menu magik-msg-mode-map
  "Menu for msg mode."
  `(,"Message"
    [,"Transmit Buffer"      magik-msg-transmit-buffer         (magik-utils-buffer-mode-list 'magik-session-mode)]
    [,"Compile Message File" magik-msg-compile-module-messages (magik-utils-buffer-mode-list 'magik-session-mode)]
    [,"Next"                 magik-msg-forward-message         t]
    [,"Previous"             magik-msg-backward-message        t]
    [,"Mark Message"         magik-msg-mark-message            t]
    "---"
    [,"Customize"            magik-msg-customize               t]))

(defun magik-msg-transmit-buffer (&optional gis)
  "Send the buffer to the GIS process.
The GIS process used is either that given by BUF or
the variable `magik-session-buffer'."
  (interactive)
  (let ((gis (magik-utils-get-buffer-mode gis
                                          'magik-session-mode
                                          "Enter Magik Session buffer:"
                                          magik-session-buffer
                                          'magik-session-buffer-alist-prefix-function))
        (process (barf-if-no-gis gis))
        (filename (buffer-file-name)))
    ;; Load messages
    (message "%s loaded in buffer %s." filename gis)
    (process-send-string
     process
     (format
      "_proc(file)
	 message_handler.compile_message_file(file)
	 _local message_handler_name << system.split_filename(system.pathname_components(file))
	 _if message_handler_name _isnt _unset
	 _then sw:message_handler.new(message_handler_name).load_message_file(file)
	 _endif
      _endproc(%S)\n$\n"
      filename))
    gis))

(defun magik-msg-compile-module-messages (&optional gis)
  "Compile all messages for a specific module.
Only compile with the module this buffer is assocaiated with in a GIS process.
The GIS process used is either that given by BUF or
the variable `magik-session-buffer'."
  (interactive)
  (let ((gis (magik-utils-get-buffer-mode gis
                                          'magik-session-mode
                                          "Enter Magik Session buffer:"
                                          magik-session-buffer
                                          'magik-session-buffer-alist-prefix-function))
        (process (barf-if-no-gis gis))
        (directory (file-name-directory (buffer-file-name))))
    ;; Load messages
    (message "Compiling all module messages in %s. " gis)
    (process-send-string
     process
     (format
      "_proc(directory)
   _handling sw_module_already_defined, sw_module_moved_module _with procedure

   module << sw_module_manager.locate_module(directory)
   _if module _isnt _unset
   _then sw_module_manager.compile_messages(module)
   _endif
      _endproc(%S)\n$\n"
      directory))
    gis))

(defun magik-msg-drag-n-drop-load (gis filename)
  "Interface to Drag and Drop GIS mode.
Called by `magik-session-drag-n-drop-load' when a Msg FILENAME is dropped."
  (let ((process (barf-if-no-gis gis)))
    (message "%s loaded in buffer %s." filename gis)
    (process-send-string
     process
     (concat
      (magik-function "load_message_file" filename 'image_override)
      "$\n"))))

(defvar magik-msg-multi-gis-processes nil
  "Note whether more than one GIS has been used.")

;;; Package initialisation
(modify-syntax-entry ?: "w" magik-msg-mode-syntax-table)
(modify-syntax-entry ?_ "w" magik-msg-mode-syntax-table)
(modify-syntax-entry ?? "w" magik-msg-mode-syntax-table)
(modify-syntax-entry ?! "w" magik-msg-mode-syntax-table)
;; multi quote
(modify-syntax-entry ?| "$" magik-msg-mode-syntax-table)
;; variable intro
(modify-syntax-entry ?# "/" magik-msg-mode-syntax-table)

;;; Package registration

;;;###autoload
(or (assoc "\\.msg$" auto-mode-alist)
    (push '("\\.msg$" . magik-msg-mode) auto-mode-alist))
(or (assoc "\\.hmsg$" auto-mode-alist)
    (push '("\\.hmsg$" . magik-msg-mode) auto-mode-alist))

;; speedbar configuration
(with-eval-after-load 'speedbar
  (speedbar-add-supported-extension ".msg")
  (speedbar-add-supported-extension ".hmsg"))

;;MSB configuration
(defun magik-msg-msb-configuration ()
  "Add Msg files to msb menu, supposes that msb is already loaded."
  (let* ((l (length msb-menu-cond))
         (last (nth (1- l) msb-menu-cond))
         (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
         (handle (1- (nth 1 last))))
    (setcdr precdr (list
                    (list
                     '(derived-mode-p 'magik-msg-mode)
                     handle
                     "Msg Files (%d)")
                    last))))

(with-eval-after-load 'msb
  (magik-msg-msb-configuration))

(defvar magik-msg-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik Message buffers.")

(progn
  ;; ------------------------ magik msg mode ------------------------

  (fset 'magik-msg-f2-map   magik-msg-f2-map)

  (define-key magik-msg-mode-map [f2]    'magik-msg-f2-map)

  (define-key magik-msg-f2-map    [down] 'magik-msg-forward-message)
  (define-key magik-msg-f2-map    [up]   'magik-msg-backward-message)
  (define-key magik-msg-f2-map    "b"    'magik-msg-transmit-buffer)
  (define-key magik-msg-f2-map    "c"    'magik-msg-compile-module-messages)
  (define-key magik-msg-f2-map    "m"    'magik-msg-mark-message))

(provide 'magik-msg)
;;; magik-msg.el ends here
