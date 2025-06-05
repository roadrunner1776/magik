;;; magik-doc-gen.el --- documentation generation for Magik files  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Reinier Koffijberg

;; Author: Reinier Koffijberg <reinierkof@github>

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

(defconst magik-regexp
  '(("method" .
     "^[_abstract\s|_private\s|_iter\s]*?_method")
    ("method-with-arguments" .
     "^[_abstract\s|_private\s|_iter\s]*?_method.*(\\([\0-\377[:nonascii:]]*?\\))")
    ("assignment-method" .
     "^[_abstract\s|_private\s|_iter\s]*?_method.*<<\s?\\(.*\\)")
    ("endmethod" .
     "^\\s-*_endmethod\\s-*\\(\n\\$\\s-*\\)?$")
    ("method-argument" .
     "_gather\\|_scatter\\|_optional")
    ("pragma" .
     "^_pragma(.*)")
    ("def_slotted_exemplar" .
     "^[sw:]?def_slotted_exemplar(.*"))
  "List of regexp strings to search for a Magik string in a buffer.")

(defun magik-file-sw-method-doc ()
  "Search a file for missing parameters in the methods.
Complete the documentation using sw-method-doc format."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'magik-base-mode)
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "method-with-arguments" magik-regexp)) nil t)
        (magik--parse-sw-method-doc (match-string 1)))
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "assignment-method" magik-regexp)) nil t)
        (magik--parse-sw-method-doc (match-string 1)))))))

(defun magik-single-method-sw-method-doc ()
  "Search last method for missing parameters.
Complete the documentation using sw-method-doc format."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'magik-base-mode)
      (forward-line)
      (search-backward-regexp (cdr (assoc "method-with-arguments" magik-regexp)) nil t)
      (search-forward-regexp (cdr (assoc "method-with-arguments" magik-regexp)) nil t)
      (if (not (equal (match-string 1) nil))
          (magik--parse-sw-method-doc (match-string 1))
        (search-backward-regexp (cdr (assoc "assignment-method" magik-regexp)) nil t)
        (search-forward-regexp (cdr (assoc "assignment-method" magik-regexp)) nil t)
        (unless (equal (match-string 1) nil)
          (magik--parse-sw-method-doc (match-string 1))))))))

(defun magik-file-type-doc ()
  "Search a file for missing parameters in the methods and slots in exemplar.
Complete the documentation using type-doc format."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'magik-base-mode)
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "method-with-arguments" magik-regexp)) nil t)
        (magik--parse-method-type-doc (match-string 1)))
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "assignment-method" magik-regexp)) nil t)
        (magik--parse-method-type-doc (match-string 1)))
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "def_slotted_exemplar" magik-regexp)) nil t)
        (save-excursion
          (magik--parse-exemplar-type-doc))
        (forward-line))))))

(defun magik-single-method-type-doc ()
  "Search for the closest method definition for missing parameters.
Complete the documentation using type-doc format."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'magik-base-mode)
      (forward-line)
      (let ((start-point (point))
            (method-regex (cdr (assoc "method-with-arguments" magik-regexp)))
            (assignment-regex (cdr (assoc "assignment-method" magik-regexp)))
            closest-point match match-point)
        (dolist (regex (list method-regex assignment-regex))
          (goto-char start-point)
          (when (search-backward-regexp regex nil t)
            (let ((current-point (point)))
              (when (or (not closest-point) (< (- start-point current-point) closest-point))
                (setq closest-point (- start-point current-point)
                      match-point current-point
                      match (match-string 1))))))
        (when match
          (goto-char match-point)
          (magik--parse-method-type-doc match)))))))

(defun magik-single-exemplar-type-doc ()
  "Search for the closest exemplar for missing parameters.
Complete the documentation using type-doc format."
  (interactive)
  (save-excursion
    (when (derived-mode-p 'magik-base-mode)
      (if (or (search-backward-regexp (cdr (assoc "def_slotted_exemplar" magik-regexp)) nil t)
              (search-forward-regexp (cdr (assoc "def_slotted_exemplar" magik-regexp)) nil t))
          (magik--parse-exemplar-type-doc)
        (message "No exemplar found in the current buffer.")))))

(defun magik--parse-sw-method-doc (method-string)
  "Helper function for inserting sw-method-doc for a single method.
Argument METHOD-STRING is the string with the method contents."
  (let ((parameters (mapcar (lambda (x) (string-trim (replace-regexp-in-string (cdr (assoc "method-argument" magik-regexp)) "" x))) (split-string method-string "(\\|)\\|,")))
        (parameters-in-documentation '())
        (missing-parameters '())
        (documentation nil)
        (documentation-found 0)
        (starting-point (+ 1 (line-number-at-pos))))
    (while (not (looking-at (cdr (assoc "endmethod" magik-regexp))))
      (if (and (not (looking-at "^\t##$"))
               (looking-at "^\t##"))
          (progn
            (setq documentation-found (+ 1 documentation-found)
                  documentation (split-string (string-trim (replace-regexp-in-string "## " "" (buffer-substring-no-properties (point) (line-end-position)))) " "))
            (dolist (comment documentation)
              (when (and (equal (string-match-p "[[:upper:]]" comment) 0) (equal (upcase comment) comment))
                (push comment parameters-in-documentation))))
        (when (looking-at "^\t##$")
          (setq documentation-found (+ 1 documentation-found))))
      (forward-line))
    (setq starting-point (+ documentation-found starting-point)
          parameters-in-documentation (delq nil (delete-dups parameters-in-documentation))
          missing-parameters (delq nil (delete-dups missing-parameters)))

    (dolist (parameter parameters)
      (when (and (not (equal parameter "")) (not (member (upcase parameter) parameters-in-documentation)))
        (push parameter missing-parameters)))
    (setq missing-parameters (reverse missing-parameters))

    (magik--write-sw-method-doc missing-parameters starting-point documentation-found)))

(defun magik--slotted-exemplar-slots (exemplar-loc)
  "Retrieve the slots of the nearest exemplar.
Argument EXEMPLAR-LOC is the buffer position of the exemplar definition."
  (let ((slot-count 1)
        (slot-name nil)
        (dollar-loc nil)
        (more-slots nil)
        (slots '()))
    (save-excursion
      (when (re-search-forward "\\(\\$\\)" nil t)
        (setq dollar-loc (match-beginning 0)
              more-slots t)))
    (while more-slots
      (save-excursion
        (goto-char exemplar-loc)
        (if (re-search-forward "{\\s-*:\\(\\sw+\\)\\s-*,\\s-*\\(_unset\\)\\s-*}" dollar-loc t slot-count)
            (progn
              (setq slot-count (+ 1 slot-count)
                    slot-name (match-string 1))
              (push slot-name slots))
          (setq more-slots nil))))
    slots))

(defun magik--parse-method-type-doc (method-string)
  "Helper function for inserting type-doc for a single method.
Argument METHOD-STRING is the string with the method contents."
  (let ((parameters (mapcar (lambda (x) (string-trim (replace-regexp-in-string (cdr (assoc "method-argument" magik-regexp)) "" x))) (split-string method-string "(\\|)\\|,")))
        (parameters-in-documentation '())
        (missing-parameters '())
        (documentation nil)
        (documentation-found 0)
        (write-return t)
        (starting-point (+ 1 (line-number-at-pos))))
    (while (not (looking-at (cdr (assoc "endmethod" magik-regexp))))
      (if (and (not (looking-at "^\t##$")) (looking-at "^\t##"))
          (progn
            (setq documentation-found (+ 1 documentation-found)
                  documentation (split-string (string-trim (replace-regexp-in-string "## " "" (buffer-substring-no-properties (point) (line-end-position)))) " "))
            (when (and (>= (length documentation) 3) (string-match-p "@param" (nth 0 documentation)))
              (push (nth 2 documentation) parameters-in-documentation))
            (when (and (>= (length documentation) 1) (string-match-p "@return" (nth 0 documentation)))
              (setq write-return nil)))
        (when (looking-at "^\t##$")
          (setq documentation-found (+ 1 documentation-found))))
      (forward-line))
    (setq starting-point (+ documentation-found starting-point)
          parameters-in-documentation (delq nil (delete-dups parameters-in-documentation))
          missing-parameters (delq nil (delete-dups missing-parameters)))

    (dolist (parameter parameters)
      (when (and (not (equal parameter "")) (not (member parameter parameters-in-documentation)))
        (push parameter missing-parameters)))
    (setq missing-parameters (reverse missing-parameters))

    (magik--write-method-type-doc missing-parameters starting-point documentation-found write-return)))

(defun magik--parse-exemplar-type-doc ()
  "Check a single exemplar for type-doc."
  (let ((starting-point (- (line-number-at-pos) 1))
        (slots (magik--slotted-exemplar-slots (point)))
        (slots-in-documentation `())
        (missing-slots `())
        (documentation nil)
        (documentation-found 0))
    (while (and (not (looking-at (cdr (assoc "pragma" magik-regexp))))
                (not (bobp))
                (not (looking-at "^\\s-*$")))
      (if (and (not (looking-at "^##$")) (looking-at "^##"))
          (progn
            (setq documentation-found (+ 1 documentation-found)
                  documentation (split-string (string-trim (replace-regexp-in-string "## " "" (buffer-substring-no-properties (point) (line-end-position)))) " "))
            (when (and (>= (length documentation) 3) (string-match-p "@slot" (nth 0 documentation)))
              (push (nth 2 documentation) slots-in-documentation))))
      (when (looking-at "^##$")
        (setq documentation-found (+ 1 documentation-found)))
      (forward-line -1))
    (setq starting-point (+ starting-point 1)
          slots-in-documentation (delq nil (delete-dups slots-in-documentation))
          missing-slots (delq nil (delete-dups missing-slots)))

    (dolist (slot slots)
      (when (and (not (equal slot "")) (not (member slot slots-in-documentation)))
        (push slot missing-slots)))

    (magik--write-exemplar-type-doc missing-slots starting-point documentation-found)))

(defun magik--write-exemplar-type-doc (missing-slots starting-point documentation-found)
  "Writer function for inserting type-doc for an exemplar.
Argument MISSING-SLOTS is a list of slots that need documentation.
Argument STARTING-POINT is the line number where it should be inserted.
Argument DOCUMENTATION-FOUND is the count of existing documentation lines."
  (goto-char (point-min))
  (forward-line (1- starting-point))

  (when (equal documentation-found 0)
    (insert "##\n"))

  (insert (mapconcat (lambda (slot) (format "## @slot {:} %s\n" slot)) missing-slots)))

(defun magik--write-method-type-doc (missing-parameters starting-point documentation-found write-return)
  "Writer function for inserting type-doc for a single method.
Argument MISSING-PARAMETERS is a list of parameters that need documentation.
Argument STARTING-POINT is the line number where it should be inserted.
Argument DOCUMENTATION-FOUND is the count of existing documentation lines.
Argument WRITE-RETURN indicates whether to insert @return documentation."
  (let ((comment-line (concat "\t##\n"))
        (return-line (concat "\t## @return {:}\n")))
    (if (or (equal documentation-found 0)
            (equal documentation-found 1))
        (progn
          (goto-char (point-min))
           (forward-line (1- starting-point))
          (when (equal documentation-found 0)
            (insert comment-line))
          (insert (mapconcat (lambda (parameter) (format "\t## @param {:} %s\n" parameter)) missing-parameters))
          (when write-return
            (insert return-line)))
      (goto-char (point-min))
      (forward-line (- starting-point 2))
      (insert (mapconcat (lambda (parameter) (format "\t## @param {:} %s\n" parameter)) missing-parameters)))))

(defun magik--write-sw-method-doc (missing-parameters starting-point documentation-found)
  "Writer function for inserting sw-method-doc for a single method.
Argument MISSING-PARAMETERS is a list of parameters that need documentation.
Argument STARTING-POINT is the line number where it should be inserted.
Argument DOCUMENTATION-FOUND is the count of existing documentation lines."
  (if (or (equal documentation-found 0)
          (equal documentation-found 1))
      (progn
        (goto-char (point-min))
        (forward-line (1- starting-point))
        (when (equal documentation-found 0)
          (insert "\t##\n"))
        (insert (mapconcat (lambda (parameter) (format "\t## %s\n" (upcase parameter))) missing-parameters))
        (insert "\t##\n"))
    (goto-char (point-min))
    (forward-line (- starting-point 2))
    (insert (mapconcat (lambda (parameter) (format "\t## %s\n" (upcase parameter))) missing-parameters))))

(provide 'magik-doc-gen)
;;; magik-doc-gen.el ends here
