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

(eval-when-compile (require 'cl)
		   (require 'easymenu)
		   (require 'font-lock)
		   (require 'magik-utils)
		   (require 'magik-shell))

(defgroup magik-product nil
  "Customise Magik product.def files group."
  :group 'magik
  :group 'tools)

(defcustom magik-product-mode-hook nil
  "*Hook to run after Product Mode is set."
  :group 'product
  :type  'hook)

(defvar magik-product-mode-map (make-sparse-keymap)
  "Keymap for Magik product.def files")

(defvar magik-product-f2-map (make-sparse-keymap)
  "Keymap for the F2 function key in Magik product.def buffers")

(fset 'magik-product-f2-map   magik-product-f2-map)

(define-key magik-product-mode-map [f2]    'magik-product-f2-map)

(define-key magik-product-f2-map    "b"    'magik-product-transmit-buffer)

(defvar magik-product-menu nil
  "Keymap for the Magik product.def buffer menu bar")

(easy-menu-define magik-product-menu magik-product-mode-map
  "Menu for Product mode."
  `(,"Product"
    [,"Add product"                      magik-product-transmit-buffer
					 :active (magik-utils-buffer-mode-list 'magik-shell-mode)
					 :keys "f2 b"]
    [,"Reinitialise product"             product-reinitialise
					 :active (magik-utils-buffer-mode-list 'magik-shell-mode)
					 :keys "f2 r"]
    "---"
    [,"Customize"                        magik-product-customize   t]
    [,"Help"                             magik-product-help        t]))

(define-key magik-product-mode-map [f1] 'magik-product-help)

(defvar magik-product-mode-syntax-table nil
  "Syntax table in use in Product Mode buffers.")

;; Imenu configuration
(defvar magik-product-imenu-generic-expression
  '(
    (nil "^\\(\\sw+\\)\\s-*\n\\(.\\|\n\\)*\nend\\s-*$" 1)
    )
  "Imenu generic expression for Magik Message mode.  See `imenu-generic-expression'.")

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
     (2 font-lock-constant-face))
   )
  "Default fontification of product.def files."
  :group 'product
  :type 'sexp)

(defun magik-product-help ()
  "Display help on how to use the Product Mode interface."
  (interactive)
  (sw-help-open sw-help-product-id))

(defun magik-product-customize ()
  "Open Customization buffer for Product Mode."
  (interactive)
  (customize-group 'product))

;;;###autoload
(defun magik-product-mode ()
  "Major mode for editing Magik product.def files.

You can customise Product Mode with the `product-mode-hook'."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)

  (use-local-map magik-product-mode-map)
  (easy-menu-add magik-product-menu)
  (set-syntax-table magik-product-mode-syntax-table)

  (setq major-mode 'magik-product-mode
	mode-name "Product"
	require-final-newline t
	imenu-generic-expression magik-product-imenu-generic-expression
	font-lock-defaults
	'(magik-product-font-lock-keywords
	  nil t))

  (run-hooks 'magik-product-mode-hook))

(defun magik-product-name ()
  "Return current Product's name as a string."
  (save-excursion
    (goto-char (point-min))
    (current-word)))

(defun magik-product-reinitialise (&optional gis)
  "Reinitialise this product in GIS."
  (interactive)
  (let* ((gis (magik-utils-get-buffer-mode gis
					   'magik-shell-mode
					   "Enter Magik process buffer:"
					   magik-shell-buffer
					   'magik-shell-buffer-alist-prefix-function))
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
  "Add the product to the GIS process."
  (process-send-string
   process
   (concat
    (magik-function "smallworld_product.add_product" filename)
    "\n$\n")))

(defun magik-product-transmit-buffer (&optional gis)
  "Send current buffer to GIS."
  (interactive)
  (let* ((gis (magik-utils-get-buffer-mode gis
					   'magik-shell-mode
					   "Enter Magik process buffer:"
					   magik-shell-buffer
					   'magik-shell-buffer-alist-prefix-function))
	 (process (barf-if-no-gis gis))
	 (filename (buffer-file-name)))
    (pop-to-buffer gis t)
    (magik-product-transmit-add-product filename process)
    gis))

(defun magik-product-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop GIS mode.
Called by `gis-drag-n-drop-load' when a Product file is dropped."
  (let ((process (barf-if-no-gis gis)))
    (magik-product-transmit-add-product filename process)
    gis))

;;; Package initialisation
(if magik-product-mode-syntax-table
    nil
  (setq magik-product-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" magik-product-mode-syntax-table)
  (modify-syntax-entry ?# "<" magik-product-mode-syntax-table)
  (modify-syntax-entry ?\n ">" magik-product-mode-syntax-table))

;;; Package registration

(or (assoc "product\\.def$" auto-mode-alist)
    (push '("product\\.def$" . magik-product-mode) auto-mode-alist))

(provide 'magik-product)
;;; magik-product.el ends here
