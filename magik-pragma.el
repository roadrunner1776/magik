;;; magik-pragma.el --- tool for filling in Magik pragma statements.

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
(require 'magik-utils)

(defgroup magik-pragma nil
  "Magik Pragma features."
  :group 'magik)

(defcustom magik-pragma-topics-file nil
  "Location of the pragma-topics file."
  :group 'magik-pragma
  :type '(choice (file)
                 (const nil)))

(defcustom magik-pragma-default-topics-filename "pragma_topics"
  "Default name of the pragma-topics file."
  :group 'magik-pragma
  :type 'string)

;;;;;;;;;;;;;;;;;;;; User interface ;;;;;;;;;;;;;;;;;;;

(defun magik-electric-pragma-tab (pragma-brackets)
  "Hop from one pragma field to the next.
This command handles multiline _pragma statements."
  ;; try to work out which field we're in.
  (let ((start-bracket (car pragma-brackets))
        (end-bracket (cdr pragma-brackets)))
    (save-match-data
      (if (re-search-forward "\\s-*\\sw+\\s-*=" end-bracket t) ;find the start of the next option
          (goto-char (match-end 0))
        (goto-char start-bracket)
        (search-forward "=" end-bracket t)))))

(defun magik-electric-pragma-slash (arg)
  "Insert the char, `/', unless the current line starts with `_pragma'.
In which case we toggle back through the various pragma options."
  (interactive "*p")
  (magik-pragma-electric-toggle-option arg 'forward))

(defun magik-electric-pragma-backslash (arg)
  "Insert the char, `\\', unless the current line starts with `_pragma'.
In which case we toggle back through the various pragma options."
  (interactive "*p")
  (magik-pragma-electric-toggle-option arg 'backward))

;;;;;;;;;;;;;;;;;;;; generic functions ;;;;;;;;;;;;;;;;;;;

(defun magik-pragma-line-p ()
  "Determine if point is on a _pragma line.
Returns a cons cell with locations of the start and end
brackets of the _pragma statement if point is somewhere in a pragma statement
or nil otherwise.
Note that this command does handle a multiline _pragma statement."
  (save-excursion
    (save-match-data
      (let* ((pt (point))
             (end-bracket (search-forward ")" (save-excursion (forward-line 3) (point)) t))
             (start-bracket (and end-bracket (condition-case err
                                                 (progn
                                                   (backward-sexp)
                                                   (point))
                                               (error nil)))))
        (and start-bracket
             end-bracket
             (goto-char start-bracket)
             (forward-line 0)
             (looking-at "_pragma(")
             (>= pt (point))      ;;ensure original point location is after start of _pragma
             (<= pt end-bracket)  ;;and before the final bracket.
             (cons start-bracket end-bracket))))))

(defun magik-pragma-do-if-match (list &optional default-elem reverse)
  "Given an LIST of elems execute each match until it returns t.
The list is in the format (NAME MATCH FUNCTION [OTHER...])
If MATCH returns t eval the corresponding FUNCTION with the first arg being the
elem of the matching element and the second arg being the next elem in the list.
The optional arguments OTHER may be used by FUNCTION to modify its behaviour.
E.g. pragma-if-match-replace-with-next uses the 4th arg to specify
the subexpression to replace.

Optional arg DEFAULT-ELEM (DEFAULT MATCH FUNCTION [OTHER...])
is used if no matches are obtained from the list.
If this matches then the  default's function is called with next elem set to
the first elem of the list, or last if REVERSE is t.
Optional arg REVERSE reverses the given list.

This can be thought to be equivalent to creating a cond construct using
the MATCH in the list as the tests and the FUNCTION as the form to be
evualated when MATCH is true.  The extra bit this provides is that
the called function knows what the next elem would be.
Also being able to make up a data structure means that it is easy to
add new things to test for.

Returns nil if no change or the list (CURRENT-ELEM NEXT-ELEM) elements."
  (if reverse
      (setq list (reverse list)))
  (let* ((len          (1- (length list)))
         (first-elem   (elt list 0))
         (current-elem nil)
         (next-elem    first-elem)
         (n -1)
         (fn nil))
    (while (and (<= n len)
                (progn
                  (setq n (1+ n)
                        current-elem (elt list n)
                        next-elem (if (eq n len) first-elem (elt list (1+ n)))
                        fn (caddr current-elem))
                  (not (eval (cadr current-elem))))))
    (cond ((and (symbolp fn) (fboundp fn))
           (funcall fn current-elem next-elem reverse)
           (list current-elem next-elem))
          (fn
                                        ;fn is not a function so we evaluate it. The form can
                                        ;can access current-elem, next-elem and reverse since we are still inside the let. I think...
           (eval fn)
           (list current-elem next-elem))
          ((and default-elem
                (eval (cadr default-elem)))
           (funcall (caddr default-elem) default-elem first-elem reverse)
           (list default-elem first-elem))
          (t nil))))

(defun magik-pragma-if-match-replace-with-next (current next reverse)
  "Remove the current match region and insert the car of the NEXT element.
The optional fourth item of CURRENT specifies a subexpression of the match.
It says to replace just that subexpression instead of the whole match.
The element follows that described in pragma-do-if-match."
  (save-excursion
    (let ((match-num (or (elt current 3) 0))
          (key (car next)))
      (delete-region (match-beginning match-num) (match-end match-num))
      (insert (if (symbolp key)
                  (symbol-name key)
                key)))))


;;;;;;;;;;;;;;;;;;;; Pragma deprecated magik code ;;;;;;;;;;;;;;;;;;;
;;When user de/selects a classify_level of deprecated then in addition
;;remove/insert a comment template.

(defvar magik-pragma-deprecated-template-start
  "## -------Deprecated------
"
  "Start of deprecated method templates.
This is used for searching for the start of a template.")

(defvar magik-pragma-deprecated-template-end
  "## -----------------------
"
  "End of deprecated method templates.
This is used for searching for the end of a template.")

(defvar magik-pragma-deprecated-template
  (concat magik-pragma-deprecated-template-start
          "## Reason     : <why>
## Use instead: <other method>
## Action     : <action to be taken - use / for options>
## Deprecated : <timestamp>
"
          magik-pragma-deprecated-template-end)
  "Template for inserting into comment header for deprecated methods.")

(defvar magik-pragma-deprecated-template-re nil
  "Regexp that matches any indented Template for deprecated methods.")

;;Make a regexp to match template when indented. ie. insert \s-* at front of every none empty line.
(if magik-pragma-deprecated-template-re  ;;Already defined so do nothing.
    nil
  (setq magik-pragma-deprecated-template-re magik-pragma-deprecated-template)
  (let (start)
    (while (string-match "^\\(\\s-*\\)\\S-+" magik-pragma-deprecated-template-re start)
      (setq magik-pragma-deprecated-template-re (replace-match "\\s-*" nil t magik-pragma-deprecated-template-re 1))
      (setq start (1+ (match-end 0))))))

(defvar magik-pragma-deprecated-action-list
  '(("Remove at next release."
     (looking-at " *Remove at next release. *")       magik-pragma-if-match-replace-with-next)
    ("Restricted at next release."
     (looking-at " *Restricted at next release. *")   magik-pragma-if-match-replace-with-next))
  "Used to control behaviour for the Action field in the Magik deprecated template.
The format follows that described in pragma-do-if-match.")

(defun magik-pragma-deprecated-action-toggle (direction)
  "Toggle the current deprecated action option.
Uses DIRECTION to determine if we need to go backwards."
  (goto-char (match-end 0))
  (magik-pragma-do-if-match magik-pragma-deprecated-action-list
                            '(default  (looking-at "<.*>") magik-pragma-if-match-replace-with-next)
                            (eq direction 'backward)))

(defun magik-pragma-goto-magik-deprecated-template ()
  "Goto the point at which the template should be placed."
  ;;limit is set so that the searching only looks at the next non-blank line.
  (let ((limit (save-excursion (skip-chars-forward " \n") (end-of-line) (point))))
    (cond ((looking-at "\\s-*##")
           (forward-line 0)
           t)
          ((re-search-forward "_method" limit t)
           (forward-line 1)
           t)
          ((re-search-forward "(" limit t)
           ;;This is intended to catch lines with define_shared_constant, define_shared_variable,
           ;; define_slot_access, def_mixin, def_indexed_mixin, new_slotted_exemplar, new_indexed_exemplar
           ;; i.e. anything which has a ( following it and which could include a ## comment
           ;; between the ( and the matching ).
           (backward-char) ; to place point in front of '(' ready for forward-sexp call
           ;;if following search fails then the default is to insert immediately before this line
           (re-search-forward "\\s-*##" (save-excursion (forward-sexp) (point)) t)
           (forward-line 0)
           t)
          (t
           ;;By default insert immediately after the pragma line
           (forward-line 0)
           t))))

(defun magik-pragma-insert-deprecated-template ()
  "Insert the template for deprecated methods."
  (save-excursion
    (save-match-data
      (search-forward ")") ; find end of _pragma statement
      (delete-horizontal-space)
      (if (eobp) ;protect against being at the end of the buffer
          (insert "\n")
        (forward-line 1))
      ;;Now search for a suitable place to insert the template
      (and (magik-pragma-goto-magik-deprecated-template)
           (not (looking-at (concat "\\s-*" magik-pragma-deprecated-template-start)))
           (let ((start (point))
                 (column (current-indentation))
                 (template (copy-sequence magik-pragma-deprecated-template)))
             (string-match "<timestamp>" template)
             (setq template (replace-match (format-time-string "%d %B %Y") t t template))
             ;;Insert the template setting read-only property on the start and end text fields
             (insert template)
             (indent-region start (point) column)
             (message "Use toggle keys, \\\\ and /, on 'Action' line to choose action."))))))

(defun magik-pragma-remove-magik-deprecated-template ()
  "Remove the template for deprecated methods.
If the text to be removed has been modified then the user is asked whether they
wish to remove it otherwise the template is removed silently."
  (save-excursion
    (search-forward ")") ; find end of _pragma statement
    (forward-line 1)
    (if (magik-pragma-goto-magik-deprecated-template)
        (let ((start nil)
              (end nil))
          (if (looking-at magik-pragma-deprecated-template-re)
              ;;No changes made just remove whole template
              (delete-region (match-beginning 0) (match-end 0))
            (and (looking-at (concat "\\s-*" magik-pragma-deprecated-template-start))
                 (setq start (match-beginning 0)))
            (setq end (re-search-forward (concat "\\s-*" magik-pragma-deprecated-template-end) nil t))
            (and start
                 end
                 (y-or-n-p "Remove modified deprecated comments? ")
                 (delete-region start end)))))))

;;;;;;;;;;;;;;;;;;;; Pragma toggle options ;;;;;;;;;;;;;;;;;;;

(defvar magik-pragma-electric-toggle-list
  '(
    (classify-level (looking-at "c?lassify_level") magik-pragma-if-match-insert-classify_level)
    (usage          (looking-at "u?sage")          magik-pragma-if-match-insert-usage)
    (topic          (looking-at "t?opic")          magik-pragma-if-match-do-the-electric-pragma-topics))
  "The list used to control behaviour for the various fields in _pragma.
The format follows that described in pragma-do-if-match.")

(defvar magik-pragma-classify_level-list
  '((basic      (looking-at " *basic *")      magik-pragma-if-match-replace-with-next)
    (advanced   (looking-at " *advanced *")   magik-pragma-if-match-replace-with-next)
    (restricted (looking-at " *restricted *") magik-pragma-if-match-replace-with-next)
    (deprecated (looking-at " *deprecated *") magik-pragma-if-match-replace-with-next)
    (debug      (looking-at " *debug *")      magik-pragma-if-match-replace-with-next))
  "The list used to control behaviour for the classify_level field in _pragma.
The format follows that described in pragma-do-if-match.")

(defvar magik-pragma-usage-list
  '(("{subclassable}"
     (looking-at" *{ *subclassable *} *")                          magik-pragma-if-match-replace-with-next)
    ("{redefinable}"
     (looking-at " *{ *redefinable *} *")                          magik-pragma-if-match-replace-with-next)
    ("{redefinable, subclassable}"
     (looking-at " *{ *redefinable *, *subclassable *} *")         magik-pragma-if-match-replace-with-next)
    ("{external}"
     (looking-at " *{ *external *} *")                             magik-pragma-if-match-replace-with-next)
    ("{not_international}"
     (looking-at " *{ *not_international *} *")                    magik-pragma-if-match-replace-with-next)
    ("{not_international, subclassable}"
     (looking-at " *{ *not_international *, *subclassable *} *")   magik-pragma-if-match-replace-with-next)
    ("{not_international, redefinable}"
     (looking-at " *{ *not_international *, *redefinable *} *")    magik-pragma-if-match-replace-with-next)
    ("{not_international, redefinable, subclassable}"
     (looking-at "{not_international, redefinable, subclassable}") magik-pragma-if-match-replace-with-next))
  "The list used to control behaviour for the usage field in _pragma.
The format follows that described in pragma-do-if-match.")

(defun magik-pragma-electric-toggle-option (arg direction)
  "Insert the char, `/', unless the current line starts with `_pragma'.
In which case we toggle through the various pragma options."
  (save-match-data
    (let ((magik-pragma-brackets (magik-pragma-line-p)))
      (cond ((consp magik-pragma-brackets)
             (let ((curr-pos (point))
                   (start-bracket (car magik-pragma-brackets))
                   (end-bracket (cdr magik-pragma-brackets))
                   option-pos)
               (goto-char (1+ start-bracket))
               (re-search-forward "\\s-*" nil t)
               (setq option-pos (point))

               ;;loop over the positions where each option starts and check to see which option
               ;;point is currently located in. This loop copes with multiline pragmas and sensibly
               ;;handles the cases when point is in a whitespace section between , and the start of the next option
               (search-forward "=" end-bracket t) ; skip over current option
               (while (and (re-search-forward "\\s-*\\(\\sw+\\)\\s-*=" end-bracket t)
                           (goto-char (match-beginning 0))        ;found start of next option including preceding space
                           (<= (point) curr-pos)                  ;test if point is in this option or a later one.
                           (setq option-pos (match-beginning 1))) ;store true start of option
                 (search-forward "=" end-bracket t))              ; LOOP: skip passed current option

               (goto-char option-pos)
               (magik-pragma-electric-toggle direction)))
            ((save-excursion
               (beginning-of-line)
               (looking-at "\\s-*## Action\\s-+: "))
             (magik-pragma-deprecated-action-toggle direction))
            (t
             (self-insert-command arg))))))

(defun magik-pragma-electric-toggle (direction)
  "Toggle the values for the different fields used in the pragma line.

DIRECTION indicates whether the values should change \\='forward or \\='backward
relative the current setting and available values."
  ;;Handle the case where the pragma line is completely empty separately.
  (if (save-excursion (beginning-of-line) (looking-at "_pragma()"))
      (progn
        ;;Insert classify_level and place point between ( and c.
        (delete-region (match-beginning 0) (match-end 0))
        (insert "_pragma(classify_level=)")
        (backward-char 16)))

  (magik-pragma-do-if-match magik-pragma-electric-toggle-list nil (eq direction 'backward)))

(defun magik-pragma-if-match-insert-classify_level (current next reverse)
  "Insert the classify_level according to the CURRENT setting.
Also adds a template in the comment section if the classify_level is set
to deprecated.  When the classify_level is changed from deprecrated then
the template is removed.  However, if data has been changed in the fields of
the template then the user is asked if they wish to remove the contents of
the deprecated template."
  ;;Ensure point stays immediately after = by searching for = and doing the replace inside save-excursion
  (search-forward "=")
  (save-excursion
    (let ((res (magik-pragma-do-if-match magik-pragma-classify_level-list
                                         '(default (looking-at "\\([^,]*\\),") magik-pragma-if-match-replace-with-next 1)
                                         reverse)))
      (cond ((eq (caadr res) 'deprecated)
             ;;next element is deprecated i.e. user has just selected deprecated
             (magik-pragma-insert-deprecated-template))
            ((eq (caar res) 'deprecated)
             ;;current element is deprecated i.e. user has just deselected deprecated
             (magik-pragma-remove-magik-deprecated-template))
            (t nil)))))

(defun magik-pragma-if-match-insert-usage (current next reverse)
  "Insert the usage according to the current setting."
  ;;Ensure point stays immediately after = by searching for = and doing the replace inside save-excursion
  (search-forward "=")
  (save-excursion
    (magik-pragma-do-if-match magik-pragma-usage-list
                              '(default (looking-at "{.*}") magik-pragma-if-match-replace-with-next)
                              reverse)))

;;;;;;;;;;;;;;;;;;;; Topic Select Mode ;;;;;;;;;;;;;;;;;;;

(defvar magik-pragma-window-configuration nil
  "Window configuration to return to after topic selection mode.")

(defun magik-pragma-if-match-do-the-electric-pragma-topics (current next reverse)
  "Select pragma topics from a menu."
  (let* ((buffer-dir (if buffer-file-name (file-name-directory buffer-file-name) default-directory))
         (magik-pragma-files (when buffer-dir (magik-utils-locate-all-dominating-file buffer-dir magik-pragma-default-topics-filename)))
         (product-pragma-file (when magik-pragma-topics-file (expand-file-name magik-pragma-topics-file)))
         topics pos)
    (re-search-forward "= *")
    (setq pos (point))
    (unless (eq (following-char) ?{)
      (insert "{")
      (search-forward ",")
      (backward-char)
      (insert "}"))
    (goto-char pos)
    (forward-char)
    (while (looking-at " *\\(\\w+\\)")
      (push (cons (match-string 1) t) topics)
      (goto-char (match-end 0))
      (looking-at " *,")
      (goto-char (match-end 0)))
    (goto-char pos)
    (setq magik-pragma-window-configuration (current-window-configuration))
    (pop-to-buffer "*topic-selection*")
    (erase-buffer)
    (magik-pragma-topic-select-mode)
    (insert "T O P I C   S E L E C T I O N

y or m - mark a line           n or u - unmark a line
e      - edit the topic list
SPC    - move down a line
RET    - accept the selection
q      - quit

-----------------------------------------------
")
    (mapc 'insert-file-contents magik-pragma-files)
    (and product-pragma-file
         (not (member product-pragma-file magik-pragma-files))
         (file-exists-p product-pragma-file)
         (insert-file-contents product-pragma-file))

    (unless (or magik-pragma-files product-pragma-file)
      (error "There is no pragma_topics file"))

    (goto-char (point-min))
    (while (not (eobp))
      (let ((topic (when (looking-at "\\s-*\\S-+\\s-+\\(\\S-+\\)")
                     (match-string 1))))
        (beginning-of-line)
        (insert (if (assoc topic topics)
                    "> "
                  "  ")))
      (forward-line))))

(define-derived-mode magik-pragma-topic-select-mode nil "Topic Select"
  "Major mode for selecting topics in pragmas.

\\{magik-pragma-topic-select-mode-map}"
  :group 'magik
  :abbrev-table nil
  :syntax-table nil)

(defun magik-pragma-topic-select-mark ()
  "Mark a line to indicate that the process should be run."
  (interactive "*")
  (magik-pragma-topic-replace-char ">"))

(defun magik-pragma-topic-replace-char (character)
  "Add the one CHARACTER string to the beginning of the current line.
Also moves down a line.
Beep if not looking at \"[ >] (\""
  (if (not (looking-at "[ >] "))
      (beep)
    (beginning-of-line)
    (insert character)
    (delete-char 1)
    (forward-line)))

(defun magik-pragma-topic-select-unmark ()
  "Remove the mark from the current line."
  (interactive "*")
  (magik-pragma-topic-replace-char " "))

(defun magik-pragma-topic-select-quit ()
  "Quit from topic selection by restoring the window configuration."
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration magik-pragma-window-configuration))

(defun magik-pragma-topic-select-select ()
  "Put the selected topics back into the pragma."
  (interactive)
  (goto-char (point-min))
  (let ((str ""))
    (while (not (eobp))
      (when (looking-at ">\\s-*\\S-+\\s-+\\(\\S-+\\)")
        (setq str (concat str (match-string 1) ", ")))
      (forward-line))
    (when (not (equal str ""))
      (setq str (substring str 0 (- (length str) 2))))
    (kill-buffer (current-buffer))
    (set-window-configuration magik-pragma-window-configuration)
    (forward-char)
    (delete-region (point) (progn (search-forward "}") (backward-char) (point)))
    (insert str)))

(defun magik-pragma-topic-edit ()
  "Edit the pragma_topics file."
  (interactive)
  (let* ((buffer-dir (if buffer-file-name (file-name-directory buffer-file-name) default-directory))
         (magik-pragma-dir (when buffer-dir (locate-dominating-file buffer-dir magik-pragma-default-topics-filename))))
    (cond (magik-pragma-dir
           (find-file (expand-file-name magik-pragma-default-topics-filename magik-pragma-dir)))
          (magik-pragma-topics-file
           (find-file (expand-file-name magik-pragma-topics-file)))
          (t
           (error "There is no pragma_topics file")))))

(progn
  ;; ------------------------ magik pragma topic select mode ------------------------

  (define-key magik-pragma-topic-select-mode-map "y"  'magik-pragma-topic-select-mark)
  (define-key magik-pragma-topic-select-mode-map "n"  'magik-pragma-topic-select-unmark)
  (define-key magik-pragma-topic-select-mode-map "m"  'magik-pragma-topic-select-mark)
  (define-key magik-pragma-topic-select-mode-map "u"  'magik-pragma-topic-select-unmark)
  (define-key magik-pragma-topic-select-mode-map " "  'forward-line)
  (define-key magik-pragma-topic-select-mode-map "q"  'magik-pragma-topic-select-quit)
  (define-key magik-pragma-topic-select-mode-map "\r" 'magik-pragma-topic-select-select)
  (define-key magik-pragma-topic-select-mode-map "e"  'magik-pragma-topic-edit))

(provide 'magik-pragma)
;;; magik-pragma.el ends here
