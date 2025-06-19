;;; magik-version.el --- Interface to the gis_version environment switching tool.

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

(require 'magik-utils)
(require 'magik-mode)

(defgroup magik-version nil
  "Multiple Magik Environments."
  :tag "Magik Environment"
  :group 'magik)

(defcustom magik-version-frame-title-format '(multiple-frames "%b"
                                                              ("" invocation-name "@" system-name " " magik-version-current))
  "*The frame title string to use when a version has been selected."
  :group 'magik-version
  :type  'sexp)

(defcustom magik-version-icon-title-format magik-version-frame-title-format
  "*The icon title string to use when a version has been selected."
  :group 'magik-version
  :type  'sexp)

(defcustom magik-version-file (concat user-emacs-directory "gis_version.txt")
  "*A file containing locations of GIS versions.
This provides an alternative interface to a gis_version program."
  :group 'magik-version
  :type  '(choice (file)
                  (const nil)))

(defcustom magik-version-match "^[* ] \\(\\S-+\\)\\s-*\\(\\S-+\\)\\s-*\\(.*\\)"
  "*Regexp matching valid versions.
Listed by `magik-version' or `magik-version-file'."
  :group 'magik-version
  :type  'regexp)

(defcustom magik-version-invalid-string "(invalid)"
  "*The string marking an invalid gis_version entry."
  :group 'magik-version
  :type  'string)

(defcustom magik-version-help "Select a Smallworld Core Product Installation.\n\nThe product you select will define the environment for any new Smallworld\nsessions that are started.\n\nTo make the selection, move the cursor to the line you want and press RETURN.\nOr press 'a' to open the gis_aliases file.\n\n\nPress 'q' to exit and do nothing.\n\n"
  "Help text displayed at top of gis_version buffer."
  :group 'magik-version
  :type  'string)

(defcustom magik-version-help-file-add "To add a new installation, press '+'.\nTo open the gis_version file, press 'o'.\n"
  "Help text for Adding to `gis-version-file' displayed after `gis-version-help'."
  :group 'magik-version
  :type  'string)

(defgroup magik-version-faces nil
  "Faces for displaying text in a Magik version selection buffer."
  :group 'magik-version)

(defface magik-version-active-face
  '((t :inherit magik-variable-face))
  "Font Lock mode face used to display the active line."
  :group 'magik-version-faces)

(defface magik-version-invalid-face
  '((t :inherit magik-warning-face))
  "Font Lock mode face used to display an invalid line."
  :group 'magik-version-faces)

(defface magik-version-name-face
  '((t :inherit magik-method-face))
  "Font Lock mode face used to display the name."
  :group 'magik-version-faces)

(defface magik-version-number-face
  '((t :inherit magik-number-face))
  "Font Lock mode face used to display the version."
  :group 'magik-version-faces)

(defcustom magik-version-font-lock-keywords
  (list
   '("^.*(invalid).*$" . 'magik-version-invalid-face)
   '("^\\([*]\\s-+\\S-+\\)\\s-+\\(\\S-+\\)"
     (1 'magik-version-active-face)
     (2 'magik-version-number-face))
   '("^ \\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)"
     (1 'magik-version-name-face)
     (2 'magik-version-number-face))
   '("^\\S-.*" . 'magik-doc-face))
  "Default fontification of gis_version."
  :group 'magik-version
  :type 'sexp)

(defvar magik-version-file-format "  %-17s   %-23s   %s\n"
  "*Header format to use for a newly created gis version file.")

(defvar magik-version-file-header
  (concat (format magik-version-file-format "name" "version" "directory")
          (format magik-version-file-format
                  "-----------------"
                  "-----------------------"
                  "--------------------------------"))
  "*Header string to use for a newly created gis version file.")

(defvar magik-version-sw-path-list nil
  "Stores list of Smallworld directories added to PATH.")

(defvar-local magik-version-current nil
  "Current gis_version stream.")

(defvar magik-smallworld-gis-current nil
  "Current selected Smallworld GIS directory.")

(defvar magik-version-position nil
  "A position in the gis version buffer above which the user shouldn't click.")

(defun magik-version-customize ()
  "Open Customization buffer for magik-version Mode."
  (interactive)
  (customize-group 'magik-version))

(defun magik-version-run ()
  "Run GIS command in selected version."
  (interactive)
  (beginning-of-line)
  (let* ((definition (magik-version-at-version-definition))
         (stream (nth 0 definition))
         (smallworld-gis (nth 2 definition))
         (buffer (concat "*gis " stream "*")))
    (setq magik-smallworld-gis smallworld-gis)
    (magik-session buffer)
    (setq magik-version-current stream
          magik-smallworld-gis-current smallworld-gis)))

(defun magik-version-gis-aliases ()
  "Open gis_aliases file of selected version.
Will prompt for layered product to use if selected version
has more than one aliases file available."
  ;;Does not cope with 'partial stream versions' where the directory components list 2 (or more directories)
  (interactive)
  (beginning-of-line)
  (let* ((version-list (magik-version-at-version-definition))
          (stream (car version-list))
          (smallworld-gis (nth 2 version-list))
          (lp-alist (magik-aliases-layered-products-file (magik-aliases-expand-file magik-aliases-layered-products-file smallworld-gis) smallworld-gis))
          alias-file)
    (when (not stream)
      (error "Invalid selection"))
    (cond ((null lp-alist) nil)
      ((eq (length lp-alist) 1)
        (setq alias-file (file-name-concat (cdar lp-alist) "config" "gis_aliases")))
      (t
        (let* ((lp (completing-read "Select a Layered Product with gis_aliases file: " lp-alist nil t))
                (path (cdr (assoc lp lp-alist))))
          (when path
            (setq alias-file (file-name-concat path "config" "gis_aliases"))))))
    (when alias-file
      (kill-buffer (current-buffer))
      (find-file alias-file)
      (setq-local magik-smallworld-gis smallworld-gis
                  magik-version-current stream
                  buffer-read-only t)
      (magik-aliases-next)
      (set-buffer-modified-p nil))))

(defun magik-version-next ()
  "Move point to next valid version listed."
  (interactive)
  (forward-line 1)
  (save-match-data
    (while (and (not (eobp))
                (or (looking-at (concat "^.*" magik-version-invalid-string))
                    (not (looking-at magik-version-match))
                    (< (point) magik-version-position)))
      (forward-line 1))
    (if (not (eobp))
        (beginning-of-line)
      (goto-char magik-version-position)
      (re-search-forward magik-version-match nil t)
      (beginning-of-line))))

(defun magik-version-current ()
  "Return the current gis_version."
  (interactive)
  (when magik-version-file
    magik-version-current))

;;;###autoload
(define-derived-mode magik-version-mode nil "Environment"
  "Major mode for editing gis_version files.

\\{magik-version-mode-map}"
  :group 'magik
  :abbrev-table nil

  (compat-call setq-local
               magik-version-position nil
               buffer-undo-list t
               font-lock-defaults '(magik-version-font-lock-keywords nil t (("-" . "w"))))

  (add-hook 'menu-bar-update-hook #'magik-versions-update-menu nil t))

(defvar magik-version-menu nil
  "Keymap for the gis_version buffer menu bar.")

(easy-menu-define magik-version-menu magik-version-mode-map
  "Menu for gis_version mode."
  `(,"Environment"
    [,"Select"                    magik-version-select      t]
    [,"Run"                       magik-version-run         t]
    [,"Next"                      magik-version-next        t]
    [,"Open aliases file"         magik-version-gis-aliases t]
    [,"Quit"                      magik-version-quit        t]
    "---"
    (,"Definitions")
    "---"
    [,"Add new installation"      magik-version-file-add    magik-version-file]
    [,"Open gis aliases file"     magik-version-file-open t]
    "---"
    [,"Customize"                 magik-version-customize   t]))

(add-hook 'magik-version-select-hook #'magik-aliases-update-menu)

(defun magik-version-smallworld-gis-p (path)
  "Return t if PATH points to a Smallworld installation."
  (file-directory-p (file-name-concat (file-name-directory path) "config")))

(defun magik-version-read-smallworld-gis-completion (string predicate flag)
  "Provide directory completion for finding Smallworld installations.
Repeated TAB and \\[minibuffer-completion-help] still provide
directory listing so users can navigate a directory structure looking
for a Smallworld installation.  Only when `gis-version-smallworld-gis-p'
returns t for a given path will the path be considered to be a real Smallworld
installation directory suitable for selection."
  (if (magik-version-smallworld-gis-p string)
      (cond ((eq flag #'lambda) t)
            ((null flag)       t)
            (t            string))
    ;;    (cl-letf (((symbol-function `read-smallworld-gis-predicate) (d) (equal d (file-name-directory d))))
    (let ((root (file-name-directory string))
          (completions (all-completions string
                                        'read-file-name-internal)))
      ;;            #'(lambda (d) (equal d (file-name-directory d))))))
      ;;            'read-smallworld-gis-predicate)))
      (cond ((or (eq this-command 'minibuffer-completion-help)
                 (and flag (eq this-command 'minibuffer-complete)))
             ;;Provide directory completions for user feedback ONLY
             (mapcar (function (lambda (d) (concat root d))) completions))
            (flag
             ;; all-completions. Do not want to return anything here
             ;; otherwise any directory is accepted after a Return
             nil)
            (t
             ;;try-completion
             (setq completions
                   (try-completion (file-name-nondirectory string)
                                   (mapcar 'list completions)))
             (if (eq completions t)
                 string
               (concat (or root "") completions)))))))

(defun magik-version-read-smallworld-gis ()
  "Prompt for a valid value for SMALLWORLD_GIS."
  (let ((path
         (file-truename (read-directory-name "Enter product directory for Core installation: "))))
    (setq path (directory-file-name path))
    (if (eq system-type 'windows-nt)
        (subst-char-in-string ?/ ?\\ path t))
    path))

(defun magik-version-file-add (root name version)
  "Add a new entry to the file given by `gis-version-file'."
  (interactive
   (let* ((_ok (or magik-version-file
                  (error "File interface is not being used")))
          (root (magik-version-read-smallworld-gis))
          (product-version-file (file-name-concat (file-name-as-directory root) "config" "PRODUCT_VERSION"))
          name version)
     (if (file-exists-p product-version-file)
         (with-current-buffer (get-buffer-create " *product_version*")
           (erase-buffer)
           (insert-file-contents product-version-file)
           (goto-char (point-min))
           (setq version (current-word)
                 name    version)))
     (list root
           (read-no-blanks-input "Enter name for this installation: " name)
           (read-no-blanks-input "Enter version number of this installation: " version))))

  (or magik-version-file
      (error "File interface is not being used"))
  (with-current-buffer (find-file-noselect magik-version-file)
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (format magik-version-file-format name version root))
      (save-buffer)))
  (when (derived-mode-p 'magik-version-mode)
    (magik-version-selection)))

(defun magik-version-file-open ()
  "Open the magik-version-file to edit."
  (interactive
   (when (not (file-exists-p magik-version-file))
     (call-interactively 'magik-version-file-create)))
  (find-file magik-version-file))

(defun magik-version-file-create ()
  "Create a gis version format file based upon the current environment.
Called if no magik-version program exists or `gis-version-file' is nil.
Will set `gis-version-file' to FILE."
  (interactive)
  (find-file magik-version-file)
  (when (not (file-exists-p magik-version-file))
    (insert magik-version-file-header)
    (call-interactively 'magik-version-file-add)
    (save-buffer))
  nil)

(defun magik-version-selection ()
  "Display a list of possible gis products for the user to choose between."
  (interactive
   (when (not (file-exists-p magik-version-file))
     (call-interactively 'magik-version-file-create)))
  (set-buffer (get-buffer-create "*gis version selection*"))
  (magik-version-mode)

  (setq buffer-read-only nil)
  (erase-buffer)
  (insert magik-version-help)
  (if (and magik-version-file magik-version-help-file-add)
      (insert "\n" magik-version-help-file-add "\n"))

  (save-excursion
    (save-match-data
      (insert-file-contents magik-version-file)
      (goto-char (point-min))

      (if (search-forward "-------" nil t) (forward-line 1)) ;skip a header
      (while (re-search-forward magik-version-match nil t)
        (beginning-of-line)
        (forward-char 1)
        (delete-char -1)
        (insert " ")
        (cond ((string-match magik-version-invalid-string (match-string-no-properties 3))
               nil)
              ((file-exists-p (match-string-no-properties 3))
               nil)
              (t
               (goto-char (match-beginning 3))
               (insert magik-version-invalid-string " "))))))

  (if (stringp magik-version-current)
      (save-excursion
        (save-match-data
          (if (re-search-forward (concat "^. " magik-version-current " ") nil t)
              (progn
                (beginning-of-line)
                (delete-char 1)
                (insert "*"))))))

  (compat-call setq-local magik-version-position (point))
  (save-match-data
    (if (search-forward "-------" nil t) (setq magik-version-position (point)))) ;skip a header

  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (pop-to-buffer (current-buffer) nil t)
  (magik-version-next))

(defun magik-version-quit ()
  "Quit, without selecting anything, gis version selection mode."
  (interactive)
  (kill-buffer (current-buffer)))

(defun magik-version-display-title ()
  "Modify the Frame and Icon titles according to the Environment."
  (interactive)
  (if magik-version-frame-title-format
      (setq frame-title-format magik-version-frame-title-format))
  (if magik-version-icon-title-format
      (setq icon-title-format  magik-version-icon-title-format)))

(defun magik-version-disable-read-only-mode ()
  "Like `read-only-mode', but does nothing in magik-version-mode."
  (interactive)
  (message "%s" (substitute-command-keys "Can't switch this buffer to edit. Use \\\[magik-version-file-open] if you want to edit this file.")))

(defun magik-version-mouse-select (click)
  "Choose product using mouse event CLICK."
  (interactive "e")
  (mouse-set-point click)
  (beginning-of-line)
  (magik-version-select))

(defun magik-version-select (&optional selected-definition)
  "Store the gis product name in the global variable `magik-version-current'.
So that `F2 z' will set the correct product's environment before starting
the gis.  The frame and icon title strings will be modified according to
`magik-version-frame-title-format' and `magik-version-icon-title-format'.
SELECTED-DEFINITION is the definition using the easy-menu or the current line."
  (interactive)
  (let* ((definition (or selected-definition
                         (magik-version-at-version-definition)))
         (stream (car definition))
         (smallworld-gis (nth 2 definition)))
    (kill-buffer (current-buffer))
    (setq magik-smallworld-gis smallworld-gis
          magik-smallworld-gis-current smallworld-gis)
    (setq-default magik-version-current stream)
    (run-hooks 'magik-version-select-hook)
    (magik-version-display-title)
    (message "The current installation for this Emacs is now %s." stream)))

(defun magik-version-at-version-definition ()
  "Return version details if the point is at a version definition.
The return value is a list (STREAM VERSION SMALLWORLD_GIS), or nil if
no (valid) match is found."
  (if (< (point) magik-version-position)
      (error "No Environment at this point"))
  (if (save-excursion
        (beginning-of-line)
        (search-forward magik-version-invalid-string (line-end-position) t))
      (error "You have selected an (invalid) Environment"))
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (when (looking-at magik-version-match)
        (list (match-string-no-properties 1)
              (match-string-no-properties 2)
              (match-string-no-properties 3))))))

(defun magik-version-call-process-windows (&rest args)
  "Run Windows command and return the environment variables it sets up."
  (let ((default-directory temporary-file-directory) ;avoid command shell complaining about cwd being a UNC path
        (w32-quote-process-args t)   ;ensure quoting of arguments
        (win32-quote-process-args t));
    (apply 'call-process args)))

(defun magik-version-call-process-unix (command)
  "Run unix COMMAND and return the environment variables it sets up."
  (call-process "/bin/sh" nil t nil "-c"
                (concat "SHELL=/bin/sh ; export SHELL ; " command " ; env")))

(defun magik-version-prepend-sw-paths (orig new)
  "Ensure Smallworld directories are prepended to PATH variable.
For magik-version code to work, the SMALLWORLD paths need to be
prepended to PATH.
On UNIX, PATH is modified to have SMALLWORLD appended.
On Windows it is prepended.

Also sets `gis-version-sw-path-list' to be the list of directories added to PATH
by the current Smallworld version."
  (save-match-data
    (let ((new-list  (delq nil (parse-colon-path new)))
          (orig-list (delq nil (parse-colon-path orig)))
          sw-list
          cnt)
      ;; Collect new paths added in sw-list
      (dolist (p new-list)
        (cond ((not (member p orig-list))
               (setq sw-list (append sw-list (list p))))
              ((and (> (setq cnt (cl-count p new-list :test 'equal)) 1)
                    (not (eq cnt (cl-count p orig-list :test 'equal))))
               ;;Found mismatching numbers of multiple entries
               ;;selected version has added more
               (or (member p sw-list)
                   (setq sw-list (append sw-list (list p)))))
              (t
               nil)))
      ;;remove previous SW paths using magik-version-sw-list
      ;;remove new SW paths first then prepend them.
      (dolist (p (append sw-list magik-version-sw-path-list))
        (if (member p new-list)
            (setq new-list (delete p new-list))))
      (setq magik-version-sw-path-list (cl-copy-list sw-list)
            new (mapconcat 'directory-file-name
                           (append sw-list new-list)
                           path-separator))
      (subst-char-in-string ?/ ?\\ new))))

(defun magik-version-header-string ()
  "Insert a string describing the gis_version status.
Used before running a GIS process."
  (if magik-version-current
      (insert (format "Gis Environment: %s\n" magik-version-current))
    (insert "\n** There is no currently selected gis product.\n** (Attempting to run anyway).\n\n")))

(defun magik-versions-list ()
  "Return a list of valid version definitions as (STREAM VERSION SMALLWORLD_GIS)."
  (let (list)
    (save-excursion
      (save-match-data
        (goto-char (point-max))
        (while (and (re-search-backward magik-version-match nil t)
                    (> (point) magik-version-position))
          (unless (save-excursion
                    (beginning-of-line)
                    (search-forward magik-version-invalid-string (line-end-position) t))
            (push (list (match-string-no-properties 1)
                        (match-string-no-properties 2)
                        (match-string-no-properties 3))
                  list)))))
    list))

(defun magik-versions-update-menu ()
  "Update the dynamic Versions submenu."
  (interactive)
  (when (derived-mode-p 'magik-version-mode)
    (let ((versions (magik-versions-list))
          entries def)
      (while versions
        (setq def (car versions)
              versions (cdr versions)
              entries (nconc entries (list (vector def (list 'magik-version-select def) t)))))
      (easy-menu-change (list "Environment")
                        "Definitions"
                        (or entries (list "No Versions found"))))))

;;; Package initialisation
(modify-syntax-entry ?_  "w"  magik-version-mode-syntax-table)

(add-hook 'magik-session-start-process-pre-hook #'magik-version-header-string)

(progn
  ;; ------------------------ magik version mode ------------------------

  (define-key magik-version-mode-map " "    'magik-version-next)
  (define-key magik-version-mode-map "a"    'magik-version-gis-aliases)
  (define-key magik-version-mode-map "+"    'magik-version-file-add)
  (define-key magik-version-mode-map "o"    'magik-version-file-open)
  (define-key magik-version-mode-map "q"    'magik-version-quit)
  (define-key magik-version-mode-map "r"    'magik-version-run)
  (define-key magik-version-mode-map "\r"   'magik-version-select)
  (define-key magik-version-mode-map [mouse-2] 'magik-version-mouse-select)
  (define-key magik-version-mode-map [remap read-only-mode] 'magik-version-disable-read-only-mode))


(provide 'magik-version)
;;; magik-version.el ends here
