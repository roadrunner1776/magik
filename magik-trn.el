;;; magik-trn.el --- mode for editing Magik Translation files. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Sebastiaan Speck

;; Author: Sebastiaan Speck <sebastiaanspeck@github>

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
  (require 'yasnippet)
  (defvar msb-menu-cond)

  (require 'magik-utils)
  (require 'magik-session))

(defgroup magik-trn nil
  "Customise Magik Translations group."
  :group 'magik
  :group 'tools)

(defcustom magik-trn-mode-hook nil
  "*Hook to run after TRN mode is set."
  :group 'trn
  :type  'hook)

(defvar magik-trn-mode-map (make-sparse-keymap)
  "Keymap for Magik Translation files.")

(defvar magik-trn-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik Translation buffers.")

(fset 'magik-trn-f2-map   magik-trn-f2-map)

(define-key magik-trn-mode-map [f2]    'magik-trn-f2-map)

(define-key magik-trn-f2-map    "b"    'magik-trn-transmit-buffer)
(define-key magik-trn-mode-map  " "    yas-maybe-expand)

(defvar magik-trn-menu nil
  "Keymap for the Magik Translation buffer menu bar.")

(easy-menu-define magik-trn-menu magik-trn-mode-map
  "Menu for trn mode."
  `(,"Translation"
     [,"Transmit Buffer"      magik-trn-transmit-buffer         (magik-utils-buffer-mode-list 'magik-session-mode)]
     "---"
     [,"Customize"            magik-trn-customize               t]))

(defun magik-trn-customize ()
  "Open Customization buffer for Trn Mode."
  (interactive)
  (customize-group 'trn))

;;;###autoload
(defun magik-trn-mode ()
  "Major mode for editing Magik Translation files.

You can customise trn-mode with the trn-mode-hook.

\\{magik-trn-mode-map}"

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'indent-tabs-mode)

  (use-local-map magik-trn-mode-map)
  (easy-menu-add magik-trn-menu)

  (setq major-mode 'magik-trn-mode
    mode-name "Translation"
    require-final-newline t
    indent-tabs-mode t)

  (run-hooks 'magik-trn-mode-hook))

(defun magik-trn-transmit-buffer (&optional gis)
  "Send the buffer to the GIS process.
The GIS process used is either that given by BUF or the variable `gis-buffer'."
  (interactive)
  (let ((gis (magik-utils-get-buffer-mode gis
               'magik-session-mode
               "Enter Magik process buffer:"
               magik-session-buffer
               'magik-session-buffer-alist-prefix-function))
         (process (barf-if-no-gis gis))
         (filename (buffer-file-name)))
    ;; Load messages
    (message "%s loaded in buffer %s." filename gis)
    (process-send-string
      process
      (concat
        (magik-function "text_translator.load_tabbed" filename)
        "\n$\n"))
    gis))

;;; Package registration

;;;###autoload
(or (assoc "\\.trn$" auto-mode-alist)
  (push '("\\.trn$" . magik-trn-mode) auto-mode-alist))

;; speedbar configuration
(with-eval-after-load 'speedbar
  (speedbar-add-supported-extension ".trn"))

;;MSB configuration
(defun magik-trn-msb-configuration ()
  "Add Trn files to msb menu, supposes that msb is already loaded."
  (let* ((l (length msb-menu-cond))
          (last (nth (1- l) msb-menu-cond))
          (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
          (handle (1- (nth 1 last))))
    (setcdr precdr (list
                     (list
                       '(eq major-mode 'magik-trn-mode)
                       handle
                       "Trn Files (%d)")
                     last))))

(with-eval-after-load 'msb
  (magik-trn-msb-configuration))

(provide 'magik-trn)
;;; magik-trn.el ends here
