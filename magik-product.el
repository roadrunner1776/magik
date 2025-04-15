;;; magik-product.el --- mode for editing Magik product.def files.

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

(defgroup magik-product nil
  "Customise Magik product.def files group."
  :group 'magik
  :group 'tools)

;; Imenu configuration
(defvar magik-product-imenu-generic-expression
  '((nil "^\\(\\sw+\\)\\s-*\n\\(.\\|\n\\)*\nend\\s-*$" 1))
  "Imenu generic expression for Magik Message mode.
See `imenu-generic-expression'.")

;; Font-lock configuration
(defcustom magik-product-font-lock-keywords
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
  "Default fontification of product.def files."
  :group 'product
  :type 'sexp)

(defun magik-product-customize ()
  "Open Customization buffer for Product Mode."
  (interactive)
  (customize-group 'product))

;;;###autoload
(define-derived-mode magik-product-mode nil "Product"
  "Major mode for editing Magik product.def files.

You can customize Product Mode with the `magik-product-mode-hook`.

\\{magik-product-mode-map}"
  :group 'magik
  :abbrev-table nil
  :syntax-table nil

  (compat-call setq-local
               require-final-newline t
               imenu-generic-expression magik-product-imenu-generic-expression
               font-lock-defaults '(magik-product-font-lock-keywords nil t)))

(defvar magik-product-menu nil
  "Keymap for the Magik product.def buffer menu bar.")

(easy-menu-define magik-product-menu magik-product-mode-map
  "Menu for Product mode."
  `(,"Product"
    [,"Add product"          magik-product-transmit-buffer (magik-utils-buffer-mode-list 'magik-session-mode)]
    [,"Reinitialise product" magik-product-reinitialise    (magik-utils-buffer-mode-list 'magik-session-mode)]
    "---"
    [,"Customize"            magik-product-customize       t]))

(defvar magik-product-mode-syntax-table nil
  "Syntax table in use in Product Mode buffers.")

(defun magik-product-name ()
  "Return current Product's name as a string."
  (save-excursion
    (goto-char (point-min))
    (current-word)))

(defun magik-product-reinitialise (&optional gis)
  "Reinitialise this product in GIS."
  (interactive)
  (let* ((gis (magik-utils-get-buffer-mode gis
                                           'magik-session-mode
                                           "Enter Magik Session buffer:"
                                           magik-session-buffer
                                           'magik-session-buffer-alist-prefix-function))
         (process (barf-if-no-gis gis)))
    (display-buffer gis t)
    (process-send-string
     process
     (concat ;; the .products[] interface is used for backwards compatibility.
      "smallworld_product"
      ".products[:|" (magik-product-name) "|]"
      ".reinitialise()\n$\n"))
    gis))

(defun magik-product-transmit-add-product (filename process)
  "Add the product FILENAME to the GIS PROCESS."
  (process-send-string
   process
   (concat
    (magik-function "smallworld_product.add_product" filename)
    "\n$\n")))

(defun magik-product-transmit-buffer (&optional gis)
  "Send current buffer to GIS."
  (interactive)
  (let* ((gis (magik-utils-get-buffer-mode gis
                                           'magik-session-mode
                                           "Enter Magik Session buffer:"
                                           magik-session-buffer
                                           'magik-session-buffer-alist-prefix-function))
         (process (barf-if-no-gis gis))
         (filename (buffer-file-name)))
    (pop-to-buffer gis t)
    (magik-product-transmit-add-product filename process)
    gis))

(defun magik-product-drag-n-drop-load (gis filename)
  "Interface to Drag and Drop GIS mode.
Called by `magik-session-drag-n-drop-load' when a Product FILENAME is dropped."
  (let ((process (barf-if-no-gis gis)))
    (magik-product-transmit-add-product filename process)
    gis))

;;; Package initialisation
(modify-syntax-entry ?_ "w" magik-product-mode-syntax-table)
(modify-syntax-entry ?# "<" magik-product-mode-syntax-table)
(modify-syntax-entry ?\n ">" magik-product-mode-syntax-table)

;;; Package registration

;;;###autoload
(add-to-list 'auto-mode-alist '("product.def\\'" . magik-product-mode))

(defvar magik-product-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik product.def buffers.")

(progn
  ;; ------------------------ magik product mode ------------------------

  (fset 'magik-product-f2-map   magik-product-f2-map)

  (define-key magik-product-mode-map [f2]    'magik-product-f2-map)

  (define-key magik-product-f2-map    "b"    'magik-product-transmit-buffer)
  (define-key magik-product-f2-map    "r"    'magik-product-reinitialise))

(provide 'magik-product)
;;; magik-product.el ends here
