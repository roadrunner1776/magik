;;; magik-doc-gen.el --- comment generation for Magik files  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Reinier Koffijberg

;; Author:  <reinier.koffijberg@RDS>

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

(defun magik-file-sw-method-docs ()
  "Search file for missing parameters in the methods and complete the comments."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'magik-base-mode)
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "method-with-arguments" magik-regexp)) nil t)
        (magik-parse-sw-method-docs (match-string 1)))
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "assignment-method" magik-regexp)) nil t)
        (magik-parse-sw-method-docs (match-string 1)))))))

(defun magik-single-sw-method-docs ()
  "Search last method for missing parameters and complete the comments."
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'magik-base-mode)
      (forward-line)
      (search-backward-regexp (cdr (assoc "method-with-arguments" magik-regexp)) nil t)
      (search-forward-regexp (cdr (assoc "method-with-arguments" magik-regexp)) nil t)
      (if (not (equal (match-string 1) nil))
          (magik-parse-sw-method-docs (match-string 1))
        (search-backward-regexp (cdr (assoc "assignment-method" magik-regexp)) nil t)
        (search-forward-regexp (cdr (assoc "assignment-method" magik-regexp)) nil t)
        (unless (equal (match-string 1) nil)
          (magik-parse-sw-method-docs (match-string 1))))))))

(defun magik-slotted-exemplar-slots (exemplar-loc)
  "Retrieve the slots of the nearest exemplar.
Argument EXEMPLAR-LOC ..."
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

(defun magik-file-type-docs ()
  "Search a file for missing parameters in the methods and slots in exemplar.
then complete with Typedocs format format"
  (interactive)
  (save-excursion
    (cond
     ((derived-mode-p 'magik-base-mode)
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "method-with-arguments" magik-regexp)) nil t)
        (magik-parse-method-type-docs (match-string 1)))
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "assignment-method" magik-regexp)) nil t)
        (magik-parse-method-type-docs (match-string 1)))
      (goto-char (point-min))
      (while (search-forward-regexp (cdr (assoc "def_slotted_exemplar" magik-regexp)) nil t)
        (save-excursion
          (magik-parse-exemplar-type-docs))
        (forward-line))))))

(defun magik-parse-exemplar-type-docs ()
  "Check exemplar for type-docs."
  (let ((starting-point (- (line-number-at-pos) 1))
        (slots (magik-slotted-exemplar-slots (point)))
        (slots-in-comments `())
        (missing-slots `())
        (comments nil)
        (comments-found 0))
    (while (and (not (looking-at (cdr (assoc "pragma" magik-regexp))))
                (> (point) (point-min))
                (not (looking-at "^\\s-*$")))
      (if (and (not (looking-at "^##$")) (looking-at "^##"))
          (progn
            (setq comments-found (+ 1 comments-found)
                  comments (split-string (string-trim (replace-regexp-in-string "## " "" (buffer-substring-no-properties (point) (line-end-position)))) " "))
            (when (and (>= (length comments) 3) (string-match-p "@slot" (nth 0 comments)))
              (push (nth 2 comments) slots-in-comments))))
      (when (looking-at "^##$")
        (setq comments-found (+ 1 comments-found)))
      (forward-line -1))
    (setq starting-point (+ starting-point 1)
          slots-in-comments (delq nil (delete-dups slots-in-comments))
          missing-slots (delq nil (delete-dups missing-slots)))

    (dolist (slot slots)
      (when (and (not (equal slot "")) (not (member slot slots-in-comments)))
        (push slot missing-slots)))

    (magik-write-exemplar-type-docs missing-slots starting-point comments-found)))

(defun magik-write-exemplar-type-docs (missing-slots starting-point comments-found)
  "Writer function for inserting exmplar-type-docs.
Argument MISSING-SLOTS ...
Argument STARTING-POINT ...
Argument COMMENTS-FOUND ..."
  (goto-char (point-min))
  (forward-line (1- starting-point))

  (when (equal comments-found 0)
    (insert "##\n"))

  (dolist (slot missing-slots)
    (insert (concat "## @slot {:} " slot "\n"))))

(defun magik-single-method-type-docs ()
  "Search for the closest method definition.
Then complete the comments if parameters are missing."
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
          (magik-parse-method-type-docs match)))))))

(defun magik-single-exemplar-type-docs ()
  "Search closest exemplar for missing parameters and complete the comments."
  (interactive)
  (save-excursion
    (when (derived-mode-p 'magik-base-mode)
      (if (or (search-backward-regexp (cdr (assoc "def_slotted_exemplar" magik-regexp)) nil t)
              (search-forward-regexp (cdr (assoc "def_slotted_exemplar" magik-regexp)) nil t))
          (magik-parse-exemplar-type-docs)
        (message "No exemplar found in the current buffer.")))))

(defun magik-parse-method-type-docs (method-string)
  "Helper function for inserting method-type-docs.
Argument METHOD-STRING ..."
  (let ((parameters (mapcar (lambda (x) (string-trim (replace-regexp-in-string (cdr (assoc "method-argument" magik-regexp)) "" x))) (split-string method-string "(\\|)\\|,")))
        (parameters-in-comments '())
        (missing-parameters '())
        (comments nil)
        (comments-found 0)
        (write-return t)
        (starting-point (+ 1 (line-number-at-pos))))
    (while (not (looking-at (cdr (assoc "endmethod" magik-regexp))))
      (if (and (not (looking-at "^\t##$"))
               (looking-at "^\t##"))
          (progn
            (setq comments-found (+ 1 comments-found)
                  comments (split-string (string-trim (replace-regexp-in-string "## " "" (buffer-substring-no-properties (point) (line-end-position)))) " "))
            (when (and (>= (length comments) 3) (string-match-p "@param" (nth 0 comments)))
              (push (nth 2 comments) parameters-in-comments))
            (when (and (>= (length comments) 1) (string-match-p "@return" (nth 0 comments)))
              (setq write-return nil)))
        (when (looking-at "^\t##$")
          (setq comments-found (+ 1 comments-found))))
      (forward-line))
    (setq starting-point (+ comments-found starting-point)
          parameters-in-comments (delq nil (delete-dups parameters-in-comments))
          missing-parameters (delq nil (delete-dups missing-parameters)))

    (dolist (parameter parameters)
      (when (and (not (equal parameter "")) (not (member parameter parameters-in-comments)))
        (push parameter missing-parameters)))
    (setq missing-parameters (reverse missing-parameters))

    (magik-write-method-type-docs missing-parameters starting-point comments-found write-return)))

(defun magik-write-method-type-docs (missing-parameters starting-point comments-found write-return)
  "Writer function for inserting method type docs.
Argument MISSING-PARAMETERS ...
Argument STARTING-POINT ...
Argument COMMENTS-FOUND ...
Argument WRITE-RETURN ..."
  (let ((comment-line (concat "\t##\n"))
        (return-line (concat "\t## @return {:}\n")))
    (if (or (equal comments-found 0)
            (equal comments-found 1))
        (progn
          (goto-char (point-min))
          (forward-line (1- starting-point))
          (when (equal comments-found 0)
            (insert comment-line))
          (dolist (parameter missing-parameters)
            (insert (concat "\t## @param {:} " parameter "\n")))
          (when write-return (insert return-line)))
      (progn
        (goto-char (point-min))
        (forward-line (- starting-point 2))
        (dolist (parameter missing-parameters)
          (insert (concat "\t## @param {:} " parameter "\n")))))))

(defun magik-parse-sw-method-docs (method-string)
  "Helper function for inserting sw-method-docs.
Argument METHOD-STRING ..."
  (let ((parameters (mapcar (lambda (x) (string-trim (replace-regexp-in-string (cdr (assoc "method-argument" magik-regexp)) "" x))) (split-string method-string "(\\|)\\|,")))
        (parameters-in-comments '())
        (missing-parameters '())
        (comments nil)
        (comments-found 0)
        (starting-point (+ 1 (line-number-at-pos))))
    (while (not (looking-at (cdr (assoc "endmethod" magik-regexp))))
      (if (and (not (looking-at "^\t##$")) (looking-at "^\t##"))
          (progn
            (setq comments-found (+ 1 comments-found)
                  comments (split-string (string-trim (replace-regexp-in-string "## " "" (buffer-substring-no-properties (point) (line-end-position)))) " "))
            (dolist (comment comments)
              (when (and (equal (string-match-p "[[:upper:]]" comment) 0) (equal (upcase comment) comment))
                (push comment parameters-in-comments))))
        (when (looking-at "^\t##$")
          (setq comments-found (+ 1 comments-found))))
      (forward-line))
    (setq starting-point (+ comments-found starting-point)
          parameters-in-comments (delq nil (delete-dups parameters-in-comments))
          missing-parameters (delq nil (delete-dups missing-parameters)))

    (dolist (parameter parameters)
      (when (and (not (equal parameter "")) (not (member (upcase parameter) parameters-in-comments)))
        (push parameter missing-parameters)))
    (setq missing-parameters (reverse missing-parameters))

    (magik-write-sw-method-docs missing-parameters starting-point comments-found)))

(defun magik-write-sw-method-docs (missing-parameters starting-point comments-found)
  "Writer function for inserting sw-method-docs.
Argument MISSING-PARAMETERS ...
Argument STARTING-POINT ...
Argument COMMENTS-FOUND ..."
  (if (or (equal comments-found 0)
          (equal comments-found 1))
      (progn
        (goto-char (point-min))
        (forward-line (1- starting-point))
        (when (equal comments-found 0)
          (insert "\t##\n"))
        (dolist (parameter missing-parameters)
          (insert (concat "\t## " (upcase parameter) "\n")))
        (insert "\t##\n"))
    (progn
      (goto-char (point-min))
      (forward-line (- starting-point 2))
      (dolist (parameter missing-parameters)
        (insert (concat "\t## " (upcase parameter) "\n"))))))

(provide 'magik-doc-gen)
;;; magik-doc-gen.el ends here
