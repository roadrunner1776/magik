;;; magik-loadlist.el --- mode for editing Magik load_list.txt files.

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
  (require 'cl))

(require 'font-lock)

(defgroup magik-loadlist nil
  "Customise Magik load_list.txt files group."
  :group 'magik)

(defcustom magik-loadlist-mode-hook nil
  "*Hook to run after loadlist mode is set."
  :group 'magik-loadlist
  :type  'hook)

(defcustom magik-loadlist-ignore-regexp-list '("\\..*")
  "List of Regexps used to miss certain files from load_list.txt files.
Intial ^ and final $ is automatically added in `loadlist-ignore'."
  :group 'magik-loadlist
  :type  '(repeat regexp))

(defvar magik-loadlist-mode-map (make-sparse-keymap)
  "Keymap for Magik load_list.txt files")

(define-key magik-loadlist-mode-map (kbd "<f2> b")      'magik-loadlist-transmit)
(define-key magik-loadlist-mode-map "\C-cr" 'magik-loadlist-refresh-contents)

(defvar magik-loadlist-menu nil
  "Keymap for the Magik loadlist buffer menu bar")

(easy-menu-define magik-loadlist-menu magik-loadlist-mode-map
  "Menu for loadlist mode."
  `(,"Loadlist"
    [,"Refresh Buffer from Directory"    magik-loadlist-refresh-contents :active t]
    "---"
    [,"Transmit Buffer"                  magik-loadlist-transmit         :active t :keys "f2 b"]
    "---"
    [,"Customize"                        magik-loadlist-customize        t]
    [,"Help"                             magik-loadlist-help             t]))

(define-key magik-loadlist-mode-map [f1] 'magik-loadlist-help)

(defvar magik-loadlist-mode-syntax-table nil
  "Syntax table in use in loadlist mode buffers.")

;; Font-lock configuration
(defcustom magik-loadlist-font-lock-keywords
  (list
   '("^.+\\([\\/]\\)" 0 font-lock-keyword-face)
   '("^.+"            0 font-lock-variable-name-face)
   )
  "Default fontification of load_list.txt files."
  :group 'magik-loadlist
  :type 'sexp)

(defun magik-loadlist-help ()
  "Display help on how to use the Loadlist Mode interface."
  (interactive)
  (sw-help-open sw-help-loadlist-id))

(defun magik-loadlist-customize ()
  "Open Customization buffer for Loadlist Mode."
  (interactive)
  (customize-group 'magik-loadlist))

;;;###autoload
(defun magik-loadlist-mode ()
  "Major mode for editing Magik load_list.txt files.

You can customise magik-loadlist-mode with the magik-loadlist-mode-hook."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)

  (use-local-map magik-loadlist-mode-map)
  (easy-menu-add magik-loadlist-menu)
  (set-syntax-table magik-loadlist-mode-syntax-table)

  (setq major-mode 'magik-loadlist-mode
	mode-name "loadlist"
	require-final-newline t
	font-lock-defaults
	'(magik-loadlist-font-lock-keywords
	  nil t))

  (run-hooks 'magik-loadlist-mode-hook))

(defun magik-loadlist-buffer-list ()
  "Return contents of loadlist buffer."
  (goto-char (point-min))
  (let (start
	contents file)
    (while (not (eobp))
      (skip-syntax-forward "-")
      (if (eq (following-char) ?#)
	  (forward-line)
	(setq start (point))
	(if (search-forward "#" (line-end-position) t)
	    (backward-char)
	  (end-of-line))
	(skip-syntax-backward "-")
	(setq file (buffer-substring-no-properties start (point)))
	(cond ((equal file "")
	       (setq file nil))
	      ((eq (substring file -1) ?\\)
	       (aset file (1- (length file)) ?/))
	      ((and (> (length file) 5)
		    (equal (substring file -6) ".magik"))
	       (setq file (substring file 0 (- (length file) 6))))
	      (t nil))
	(if file (push (magik-loadlist-file-data file (line-beginning-position)) contents))
	(forward-line)))
    contents))

(defun magik-loadlist-file-data (file &optional data)
  "Return list of data describing FILE."
  (let ((lc (downcase file)))
    (list lc
	  (if (equal file lc) nil file) ;used to check for case differences for UNIX
	  data)))

(defun magik-loadlist-ignore (file)
  "Return t if FILE matches any regexps from magik-loadlist-ignore-regexp-list.
Regexp does not need to include ^ or $."
  (loop for r in magik-loadlist-ignore-regexp-list
	for match = (string-match (concat "^" r "$") file)
	if match return t))

(defun magik-loadlist-directory-list (&optional dir)
  "Return contents of directory."
  (let ((contents (directory-files-and-attributes (or dir default-directory)))
	files)
    (setq contents (delq (assoc ".." contents) contents)) ; Remove .. directory from list
    (setq contents (delq (assoc "."  contents) contents)) ; Remove .  directory from list
    (save-match-data
      (loop for a in contents
	    for f = (car a)
	    if (magik-loadlist-ignore f)
	    do (progn
		 (princ (format "Ignored '%s'" f))
		 (princ "\n"))
	    else if (string-match "\\.magik$" f) ; a .magik file
	    do (push (magik-loadlist-file-data
		      (substring f 0 (- (length f) 6)))
		     files)
	    else if (cadr a) ; a subdirectory
	    do (push (magik-loadlist-file-data (concat f "/")) files)
	    end)
      files)))

(defun magik-loadlist-refresh-contents (arg &optional dir)
  "Replace contents of loadlist buffer with contents of its directory.
With a prefix arg accept all changes without prompting."
  (interactive "*P")
  (unless dir
    (setq dir (file-name-directory (buffer-file-name))))
  ;;Do not bother prompting if buffer is empty.
  (if (zerop (buffer-size)) (setq arg t))

  (let ((buflist (magik-loadlist-buffer-list))
	updated
	buf-i
	newlist)
    (with-output-to-temp-buffer "*loadlist changes*"
      (dolist (i (magik-loadlist-directory-list dir))
	(cond ((setq buf-i (assoc (elt i 0) buflist))
	       (let ((real-file    (elt i 1))
		     (replace-file (or (elt i 1) (elt i 0)))
		     (real-str     (elt buf-i 1))
		     (len  (length (elt buf-i 0)))
		     (pt   (elt buf-i 2)))
		 (if (and (or real-file real-str)
			  (not (equal real-file real-str)))
		     (progn
		       (goto-char pt)
		       (delete-char len)
		       (insert replace-file)
		       (princ (format "Updated '%s' with '%s'"
				      (or real-str (elt buf-i 0))
				      replace-file))
		       (princ "\n")
		       (setq updated t)))
		 (setcdr buf-i nil)))
	      (t
	       (push (or (elt i 1) (elt i 0)) newlist))))
      (dolist (d (reverse buflist))
	(when (elt d 2)
	  (goto-char (elt d 2))
	  (beginning-of-line)
	  (let ((prompt (concat (format "Remove '%s'"
					(or (elt d 1) (elt d 0))) " ")))
	    (when (or arg (y-or-n-p prompt))
	      (kill-line 1)
	      (princ prompt)
	      (princ "\n")
	      (setq updated t)))))
      (goto-char (point-max))
      (or (looking-at "$") (insert "\n"))
      (dolist (n newlist)
	(let ((prompt (concat (format "Append '%s'"
				      n) " ")))
	  (when (or arg (y-or-n-p prompt))
	    (insert n "\n")
	    (princ prompt)
	    (princ "\n")
	    (setq updated t))))
      (unless updated
	(princ "No changes required in buffer")
	(princ "\n")))))

(defun magik-loadlist-transmit (&optional gis)
  "Load the loadlist.txt into the GIS process."
  (interactive)
  (let* ((gis (magik-utils-get-buffer-mode gis
					   'magik-shell-mode
					   "Enter Magik process buffer:"
					   magik-shell-buffer
					   'magik-shell-buffer-alist-prefix-function))
	 (process (barf-if-no-gis gis))
	 (dir  (file-name-directory buffer-file-name))
	 (file (file-name-nondirectory buffer-file-name)))
    (message "%s loaded in buffer %s." file gis)
    (process-send-string
     process
     (concat
      (magik-function "load_file_list" dir 'unset file)
      "$\n"))))

(defun magik-loadlist-gis-drag-n-drop-load (gis filename)
  "Interface to Drag 'n' Drop GIS mode.
Called by `gis-drag-n-drop-load' when a load_list.txt file is dropped."
  (let ((process (barf-if-no-gis gis))
	(dir  (file-name-directory filename))
	(file (file-name-nondirectory filename)))
    (message "%s loaded in buffer %s." filename gis)
    (process-send-string
     process
     (concat
      (magik-function "load_file_list" dir 'unset file)
      "$\n"))))

;;; Package initialisation
(if magik-loadlist-mode-syntax-table
    nil
  (setq magik-loadlist-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" magik-loadlist-mode-syntax-table)
  (modify-syntax-entry ?# "<" magik-loadlist-mode-syntax-table)
  (modify-syntax-entry ?\n ">" magik-loadlist-mode-syntax-table))

;;; Package registration
;;;###autoload
(or (assoc "load_list\\.txt$" auto-mode-alist)
    (push '("load_list\\.txt$" . magik-loadlist-mode) auto-mode-alist))

(provide 'magik-loadlist)
;;; magik-loadlist.el ends here
