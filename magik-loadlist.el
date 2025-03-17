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

(require 'compat)
(require 'font-lock)

(defgroup magik-loadlist nil
  "Customise Magik load_list.txt files group."
  :group 'magik)

(defcustom magik-loadlist-ignore-regexp-list '("\\..*")
  "List of Regexps used to miss certain files from load_list.txt files.
Initial ^ and final $ is automatically added in `loadlist-ignore'."
  :group 'magik-loadlist
  :type  '(repeat regexp))

;; Font-lock configuration
(defcustom magik-loadlist-font-lock-keywords
  (list
   '("^.+\\([\\/]\\)" 0 font-lock-keyword-face)
   '("^.+"            0 font-lock-variable-name-face))
  "Default fontification of load_list.txt files."
  :group 'magik-loadlist
  :type 'sexp)

(defun magik-loadlist-customize ()
  "Open Customization buffer for Loadlist Mode."
  (interactive)
  (customize-group 'magik-loadlist))

;;;###autoload
(define-derived-mode magik-loadlist-mode nil "Loadlist"
  "Major mode for editing Magik load_list.txt files.

You can customize magik-loadlist-mode with the magik-loadlist-mode-hook.

\\{magik-loadlist-mode-map}"
  :group 'magik
  :abbrev-table nil

  (compat-call setq-local
               require-final-newline t
               font-lock-defaults '(magik-loadlist-font-lock-keywords nil t)))

(defvar magik-loadlist-menu nil
  "Keymap for the Magik loadlist buffer menu bar.")

(easy-menu-define magik-loadlist-menu magik-loadlist-mode-map
  "Menu for loadlist mode."
  `(,"Loadlist"
    [,"Refresh Buffer from Directory"    magik-loadlist-refresh-contents t]
    "---"
    [,"Transmit Buffer"                  magik-loadlist-transmit         t]
    "---"
    [,"Customize"                        magik-loadlist-customize        t]))

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
  "Return list of DATA describing FILE."
  (let ((lc (downcase file)))
    (list lc
          (if (equal file lc) nil file) ;used to check for case differences for UNIX
          data)))

(defun magik-loadlist-ignore (file)
  "Return t if FILE matches any regexps from magik-loadlist-ignore-regexp-list.
Regexp does not need to include ^ or $."
  (cl-loop for r in magik-loadlist-ignore-regexp-list
           for match = (string-match (concat "^" r "$") file)
           if match return t))

(defun magik-loadlist-directory-list (&optional dir)
  "Return contents of DIR."
  (let ((contents (directory-files-and-attributes (or dir default-directory)))
        files)
    (setq contents (delq (assoc ".." contents) contents)) ; Remove .. directory from list
    (setq contents (delq (assoc "."  contents) contents)) ; Remove .  directory from list
    (save-match-data
      (cl-loop for a in contents
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
  "Replace contents of loadlist buffer with contents of its DIR.
With a prefix ARG accept all changes without prompting."
  (interactive "*P")
  (unless dir
    (setq dir (file-name-directory (buffer-file-name))))
  ;;Do not bother prompting if buffer is empty.
  (if (zerop (buffer-size)) (setq arg t))

  (let ((buflist (magik-loadlist-buffer-list))
        updated
        buf-i
        newlist)
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
            (setq updated t)))))
    (goto-char (point-max))
    (or (looking-at "$") (insert "\n"))
    (dolist (n newlist)
      (let ((prompt (concat (format "Append '%s'"
                                    n) " ")))
        (when (or arg (y-or-n-p prompt))
          (insert n "\n")
          (setq updated t))))
    (if (not updated)
        (message "No changes required in buffer")
      (message "Finished updating buffer"))))

(defun magik-loadlist-transmit (&optional gis)
  "Load the loadlist.txt into the GIS process."
  (interactive)
  (let* ((dir  (file-name-directory buffer-file-name))
         (file (file-name-nondirectory buffer-file-name))
         (gis (magik-utils-get-buffer-mode gis
                                           'magik-session-mode
                                           "Enter Magik Session buffer:"
                                           magik-session-buffer
                                           'magik-session-buffer-alist-prefix-function))
         (process (barf-if-no-gis gis)))
    (message "%s loaded in buffer %s." file gis)
    (process-send-string
     process
     (concat
      (magik-function "load_file_list" dir 'unset file)
      "$\n"))))

(defun magik-loadlist-drag-n-drop-load (gis filename)
  "Interface to Drag and Drop GIS mode.
Called by `magik-session-drag-n-drop-load' when a load_list.txt FILENAME
is dropped."
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
(modify-syntax-entry ?_ "w" magik-loadlist-mode-syntax-table)
(modify-syntax-entry ?# "<" magik-loadlist-mode-syntax-table)
(modify-syntax-entry ?\n ">" magik-loadlist-mode-syntax-table)

;;; Package registration
;;;###autoload
(or (assoc "load_list\\.txt$" auto-mode-alist)
    (push '("load_list\\.txt$" . magik-loadlist-mode) auto-mode-alist))

(progn
  ;; ------------------------ magik loadlist mode  ------------------------

  (define-key magik-loadlist-mode-map (kbd "<f2> b")      'magik-loadlist-transmit)
  (define-key magik-loadlist-mode-map "\C-cr" 'magik-loadlist-refresh-contents))

(provide 'magik-loadlist)
;;; magik-loadlist.el ends here
