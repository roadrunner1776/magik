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
  (require 'cl)
  (require 'easymenu)
  (require 'font-lock)
  (defvar msb-menu-cond)

  (require 'magik-utils)
  (require 'magik-shell))

(defgroup magik-msg nil
  "Customise Magik Messages group."
  :group 'magik
  :group 'tools)

(defcustom magik-msg-mode-hook nil
  "*Hook to run after MSG mode is set."
  :group 'msg
  :type  'hook)

(defvar magik-msg-mode-map (make-sparse-keymap)
  "Keymap for Magik Message files")

(defvar magik-msg-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik Message buffers")

(fset 'magik-msg-f2-map   magik-msg-f2-map)

(define-key magik-msg-mode-map [f2]    'magik-msg-f2-map)

(define-key magik-msg-f2-map    [down] 'magik-msg-forward-message)
(define-key magik-msg-f2-map    [up]   'magik-msg-backward-message)
(define-key magik-msg-f2-map    "b"    'magik-msg-transmit-buffer)
(define-key magik-msg-f2-map    "c"    'magik-msg-compile-module-messages)
(define-key magik-msg-f2-map    "m"    'magik-msg-mark-message)

(defvar magik-msg-menu nil
  "Keymap for the Magik Message buffer menu bar")

(easy-menu-define magik-msg-menu magik-msg-mode-map
  "Menu for msg mode."
  `(,"Message"
    [,"Transmit Buffer"                  magik-msg-transmit-buffer
					 :active (magik-utils-buffer-mode-list 'magik-shell-mode)
					 :keys "f2 b"]
    [,"Compile Message File"             magik-msg-compile-module-messages
					 :active (magik-utils-buffer-mode-list 'magik-shell-mode)
					 :keys "f2 c"]
    [,"Next"                             magik-msg-forward-message
					 :active t
					 :keys "f2 down"]
    [,"Previous"                         magik-msg-backward-message
					 :active t
					 :keys "f2 up"]
    [,"Mark Message"                     magik-msg-mark-message
					 :active t
					 :keys "f2 m"]
    "---"
    [,"Customize"                        magik-msg-customize        t]
    [,"Help"                             magik-msg-help             t]))

(define-key magik-msg-mode-map [f1] 'magik-msg-help)

(defvar magik-msg-mode-syntax-table nil
  "Syntax table in use in MSG-mode buffers.")

;; Imenu configuration
(defvar magik-msg-imenu-generic-expression
  '(
    (nil "^:\\(\\sw+\\)" 1) ;;Normal messages
    (nil "^:\\s$\\(\\S$+\\)\\s$" 1) ;; | | Quoted messages
    ("Groups" "^+\\s-+\\(\\sw+\\)" 1)
    )
  "Imenu generic expression for Magik Message mode.  See `imenu-generic-expression'.")

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
   '("#.*" . font-lock-comment-face)
   )
  "Default fontification of Magik Messages."
  :group 'msg
  :type 'sexp)

;; Help
(defun magik-msg-help ()
  "Display help on how to use the Msg Mode interface."
  (interactive)
  (sw-help-open sw-help-msg-id))

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
(defun magik-msg-mode ()
  "Major mode for editing Magik Message files.

You can customise msg-mode with the msg-mode-hook."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'outline-regexp)

  (use-local-map magik-msg-mode-map)
  (easy-menu-add magik-msg-menu)
  (set-syntax-table magik-msg-mode-syntax-table)

  (setq major-mode 'magik-msg-mode
	mode-name "Message"
	require-final-newline t
	imenu-generic-expression magik-msg-imenu-generic-expression
	font-lock-defaults
	'(magik-msg-font-lock-keywords
	  nil t)
	outline-regexp "^:\\(\\sw+\\).*")

  (run-hooks 'magik-msg-mode-hook))

(defun magik-msg-transmit-buffer (&optional gis)
  "Send the buffer to the GIS process.
The GIS process used is either that given by BUF or the variable `gis-buffer'."
  (interactive)
  (let ((gis (magik-utils-get-buffer-mode gis
					  'magik-shell-mode
					  "Enter Magik process buffer:"
					  magik-shell-buffer
					  'magik-shell-buffer-alist-prefix-function))
	(process (barf-if-no-gis gis))
	(filename (buffer-file-name)))
    ;; Load messages
    (message "%s loaded in buffer %s." filename gis)
    (process-send-string
     process
     (concat
      (magik-function "message_handler.compile_message_file" filename)
      "\n$\n"))
    gis))

(defun magik-msg-compile-module-messages (&optional gis)
  "Compile all messages asociated with the module this buffer is assocaiated with in a GIS process.
The GIS process used is either that given by BUF or the variable `gis-buffer'."
  (interactive)
  (let ((gis (magik-utils-get-buffer-mode gis
					  'magik-shell-mode
					  "Enter Magik process buffer:"
					  magik-shell-buffer
					  'magik-shell-buffer-alist-prefix-function))
	(process (barf-if-no-gis gis))
	(directory (file-name-directory (buffer-file-name))))
    ;; Load messages
    (message "Compiling all module messages in %s. " gis)
    (process-send-string
     process
     (format
      "_proc(directory)
	 module << sw_module_manager.locate_module(directory)
	 sw_module_manager.compile_messages(module)
      _endproc(%S)\n$\n"
      directory))
    gis))

(defun magik-msg-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop GIS mode.
Called by `gis-drag-n-drop-load' when a Msg file is dropped."
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
(if magik-msg-mode-syntax-table
    ()
  (setq magik-msg-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?: "w" magik-msg-mode-syntax-table)
  (modify-syntax-entry ?_ "w" magik-msg-mode-syntax-table)
  (modify-syntax-entry ?? "w" magik-msg-mode-syntax-table)
  (modify-syntax-entry ?! "w" magik-msg-mode-syntax-table)
  ;; multi quote
  (modify-syntax-entry ?| "$" magik-msg-mode-syntax-table)
  ;; variable intro
  (modify-syntax-entry ?# "/" magik-msg-mode-syntax-table))

;;; Package registration

(or (assoc "\\.msg$" auto-mode-alist)
    (push '("\\.msg$" . magik-msg-mode) auto-mode-alist))
(or (assoc "\\.hmsg$" auto-mode-alist)
    (push '("\\.hmsg$" . magik-msg-mode) auto-mode-alist))

;; speedbar configuration
(eval-after-load 'speedbar
  '(progn
     (speedbar-add-supported-extension ".msg")
     (speedbar-add-supported-extension ".hmsg")))

;;MSB configuration
(defun magik-msg-msb-configuration ()
  "Adds Msg files to msb menu, supposes that msb is already loaded."
  (let* ((l (length msb-menu-cond))
	 (last (nth (1- l) msb-menu-cond))
	 (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
	 (handle (1- (nth 1 last))))
    (setcdr precdr (list
		    (list
		     '(eq major-mode 'magik-msg-mode)
		     handle
		     "Msg Files (%d)")
		    last))))

(eval-after-load 'msb
  '(magik-msg-msb-configuration))

(provide 'magik-msg)
;;; magik-msg.el ends here
