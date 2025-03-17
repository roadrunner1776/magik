;;; magik-template.el --- Provide a simple template file capability for Magik mode

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

;; Provide a simple template file capability for Magik mode.
;; This code has been written to provide an interface to multiple sets of template files.
;;
;; Note that this does NOT make use of any of the generic Emacs template packages.
;; I did have a brief look at the available packages but found that the interface I
;; needed to support our internal Patch and Change note templates required a user choice
;; of template that could not be determined from the location of the new file or the file name alone.
;;
;; Note all templates must reside in a single directory.
;; You can make use of the template set feature to provide a the user a choice of a limited
;; number of templates.
;;
;; Quick start
;; ===========
;; 1. Create a template file template_default.magik
;;    and place it in a suitable central directory.
;; 2. Set the variable magik-template-dir to that directory.
;; 3. Run:
;;    (magik-template-file-type-alist-add "Template" t magik-template-file-type-templates-default)
;; Now when you attempt to create a new Magik file the contents of this template
;; is used.
;;
;; Features
;; ========
;; Additional templates
;; Creating another template set
;; Auto modification of new file
;;
;; Simple addition of a template
;; -----------------------------
;; To add another template the default set, "Template":
;; 1. Create the template file and place it in the template directory.
;; 2. Run the following command:
;;    (magik-template-file-type-alist-add "Template"
;;                               t
;;                               '((another "Another" "template_another_default.magik")))
;; 3. You may need to modify the function, magik-template-file-type-p, so that it can recognise
;;    the different templates.
;;
;; Creating another template set
;; =============================
;; To create another template set you may need to write some function(s) that can distinguish
;; between the templates.  You will then add them to the beginning of magik-template-file-type-hook.
;; You will also need to write a function that will return t when a new filename is created
;; that should use this template set.
;; For example, in magik-patch.el to create a template set called "Patch":
;; 1. Created a function to match Patch filenames, magik-patch-file-number.
;; 2. Added magik-template-file-type-change-note-p and magik-template-file-type-patch-p to the hook,
;;    magik-template-file-type-hook.
;; 3. Run
;;    (magik-template-file-type-alist-add "Patch"
;;                               'magik-patch-file-number
;;                               '((patch  "Patch"  "template_magik_patch.magik")
;;                                 (change "Change" "template_change_note.magik")))
;;
;; Auto modification of new file
;; =============================
;; Once a new file is initialised with a template, the file may be further modified
;; by adding an appropriate function to the hook variable, magik-template-initialise-hook.
;;
;; These globals hold a record (as a string) of what the templates and
;; magik-patch-options files held last time we looked so that we know
;; when to go to the trouble of parsing them again.
;;

;;; Code:

;;Variables

(defvar magik-template-dir nil
  "*Location of Magik template files.")

(defvar magik-template-alist nil
  "Alist of types of template files.
Each element is a cons cell: (TYPE . FILE)
When you add to this alist to enable additional templates,
you will also have to add an appropriate entry to
magik-template-file-type-alist to make the user
selection pick up the new template file.")

(defvar-local magik-template-file-type nil
  "Type of Magik file being edited.
This variable is buffer local and is used to identify the type of Magik file
it is, i.e. which Template file it came from.

The default value of this variable is used for communicating the type of
template the user wants when the buffer/file does not exist
to the `file-not-found-hook' function \\[magik-template-maybe-insert].")

(defvar magik-template-file-type-templates-default '((default "Default" "template_default.magik"))
  "Default list for \\[magik-template-file-type-alist-add].
The order is important since the first in the list is the default template used
when the user presses [return] when asked to select a template.
If you want to change the order and hence the default used.")

(defvar magik-template-file-type-default 'default
  "The default Magik Template file type to use.

This must equal a key in magik-template-alist and be a value of
a key in magik-template-alist.")

(defvar magik-template-file-type-alist nil
  "Alist of valid Magik file types.
This is used to give the User a choice of template to use when they do \\[find-file]
with a Magik file name but when the file does not exist yet.
  (LABEL . (FUNCTION . ((OPTION . KEY) ... )))
where
LABEL    is the string label used in the prompt to the user.
FUNCTION is a function used to test the type of templates to use for the
         new file set to t for the default template options.
OPTION   is the string displayed for the user to select the KEY.
KEY      is the type of template file to use as given in magik-template-alist.")

(defun magik-template-file-type-alist-add (label function options &optional replace-or-append)
  "Provide interface for adding new magik-template-file-type collections.
A template file may be listed in multiple label groups.
The FUNCTION modifies `magik-template-file-type-alist' & `magik-template-alist'.

Each OPTION is a list (SYMBOL LABEL FILE)

where SYMBOL is the list symbol representing this type,
      LABEL  is the User visible label for when the user selects this type,
      FILE   is the file name for this template found in magik-template-dir.

The optional argument REPLACE-OR-APPEND is either nil, \\='replace or \\='append.
If nil, then OPTIONS are prepended to the LABEL entry
of `magik-template-file-type-alist';
  \\='append  appended
  \\='replace replace  any existing OPTIONS of the LABEL entry in
                       `magik-template-file-type-alist'."

  ;;Add each template file to the alist of template file names. Avoids duplication
  (mapc #'(lambda (x) (add-to-list 'magik-template-alist (cons (elt x 0) (elt x 2)))) options)

  ;;Create main template key if not already there
  (or (assoc label magik-template-file-type-alist)
      (add-to-list 'magik-template-file-type-alist (cons label (cons function nil))))

  ;;Append/prepend/replace this set of templates under the given label.
  (let* ((replace-p (eq replace-or-append 'replace))
         (append-p  (eq replace-or-append 'append))
         (alist-entry (assoc label magik-template-file-type-alist))
         (choice-list (and (not replace-p)         ;;i.e set to nil if replace.
                           (cddr alist-entry))))
    (mapc #'(lambda (x) (add-to-list 'choice-list
                                     (cons (elt x 1) (elt x 0))
                                     append-p))
          (if append-p
              options
            ;;preserve user supplied order if prepending.
            (reverse options)))

    (setcdr alist-entry (cons function choice-list))))

;;Hooks

(defvar magik-template-initialise-hook nil
  "Hook for initialising a template file.")

(defvar magik-template-file-type-hook nil
  "List of functions used to determine magik-template-file-type.
Each function is run in turn until one function returns the non-nil value
to use for magik-patch-file-type.")

;;Functions

(defun magik-template-file (type &optional dir)
  "Return the path to the template of type TYPE.
Use optional DIR to search for the template"
  (let ((file (cdr (assoc type magik-template-alist)))
        (template-dir (or dir magik-template-dir)))
    (and file template-dir (concat (file-name-as-directory template-dir) file))))

(defun magik-template-file-type ()
  "Return the file type according to the contents of the buffer."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (run-hook-with-args-until-success 'magik-template-file-type-hook (buffer-name)))))

(defun magik-template-initialise (type)
  "Insert template text from TYPE.
Only the text in the template starting from a line matching ^# will be inserted."
  (if type   ;; to strip out the preamble from the template.
      (progn
        (goto-char (point-min))
        (or (re-search-forward "^#" nil t)
            (error "The template file, %s, doesn't seem to have column-1 hash, #, character" (magik-template-file type)))
        (delete-region (point-min) (1- (point)))))
  (goto-char (point-max))
  (run-hooks 'magik-template-initialise-hook))

(defun magik-template-maybe-insert ()
  "This is used in the hook file-not-found-hook.
The hook is called without arguments.

The `magik-template-file-type' variable is first used to determine the type.
Its default value may also be set since this is the only way hook
the magik-patch hook functions can request the file type to use because
`magik-template-file-type' is a buffer local variable and creation of
a new variable kills all local variables.
The default value is always reset to nil by this function.

The variable `magik-template-file-type-default' can be used by a user
to predefine a template type to use for normal magik files."

  (set-auto-mode t) ;ensure major mode is selected for the buffer but only using the file name
  (let* ((type (or magik-template-file-type
                   (default-value 'magik-template-file-type))) ;default value used by magik-patch
         (template-file nil))
    (if (derived-mode-p 'magik-base-mode)
        (progn
          (cl-loop for type-data in magik-template-file-type-alist
                   for label         = (car type-data)
                   for prompt        = (concat (format "Magik %s type:" label) " ")
                   for test-function = (cadr type-data)
                   for option-list   = (cddr type-data)
                   until type
                   if (or (and (functionp test-function) (funcall test-function))
                          (eq t test-function))
                   do (setq type (cdr (if (eq (length option-list) 1)
                                          (car option-list)
                                        (assoc (completing-read prompt
                                                                option-list
                                                                nil t nil nil
                                                                (caar option-list))
                                               option-list)))))
          (setq magik-template-file-type (or type magik-template-file-type-default))

          (setq-default magik-template-file-type nil) ; reset default value
          (setq template-file (magik-template-file magik-template-file-type))
          (if (and template-file
                   (file-exists-p template-file))
              (let ((buffer-undo-list t)) ;prevent user accidentally undoing the insertion!
                (erase-buffer)
                (insert-file-contents template-file)
                (magik-template-initialise magik-template-file-type)))))))
(add-hook 'find-file-not-found-hooks 'magik-template-maybe-insert)

;;Ideally this function would be a bit more intelligent so that it did not require
;; modification every time a new template was made and would also reduce the need for
;; hand crafted hooks for other template sets.
;; A possible algorithm would be to cache each template and then
;; use compare-windows to identify a unique initial string for each template.
(defun magik-template-file-type-p (_buffer-name)
  "Hook function that identifies default Magik files.

Modify this function to return a suitable match for the various templates
you have.

This hook should come last."
  (if (re-search-forward "^_package " nil t)
      'default))

(add-hook  'magik-template-file-type-hook 'magik-template-file-type-p t)

;;;;;;;;;;;;;;;;;;
;;Build template structure.
;;;;;;;;;;;;;;;;;;
;;uncomment the following line if you want the default template structure
;;defined when you load this file.
;;Note that magik-template-dir still needs to be set too.
;;(magik-template-file-type-alist-add "Template" t magik-template-file-type-templates-default)

(provide 'magik-template)
;;; magik-template.el ends here
