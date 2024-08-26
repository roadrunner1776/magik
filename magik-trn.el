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
  (require 'easymenu))

(require 'magik-session)
(require 'magik-utils)

;;;###autoload
(define-derived-mode magik-trn-mode text-mode "Translation"
  "Major mode for editing Magik Translation files."
  :group 'magik
  :abbrev-table nil
  :syntax-table nil

  (setq-local require-final-newline t
    indent-tabs-mode t))

(defvar magik-trn-menu nil
  "Keymap for the Magik Translation buffer menu bar.")

(defvar magik-trn-mode-map (make-sparse-keymap)
  "Keymap for Magik Translation files.")

(easy-menu-define magik-trn-menu magik-trn-mode-map
  "Menu for trn mode."
  `(,"Translation"
     [,"Transmit Buffer"      magik-trn-transmit-buffer         (magik-utils-buffer-mode-list 'magik-session-mode)]))

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

;; ------------------------ magik trn mode -------------------------
(define-key magik-trn-mode-map (kbd "<f2> b") 'magik-trn-transmit-buffer)

(provide 'magik-trn)
;;; magik-trn.el ends here
