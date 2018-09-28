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

(eval-when-compile (require 'cl))

(require 'magik-utils)
;;(require 'magik-aliases)
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

(defcustom magik-version-program "gis_version"
  "*The program name for the gis_version environment program."
  :group 'magik-version
  :type  'file)

(defcustom magik-version-file (let ((file (concat (file-name-as-directory (getenv "HOME"))
						  "gis_version.txt")))
				(if (file-exists-p file)
				    file))
  "*A file containing locations of GIS versions.
This provides an alternative interface to a gis_version program.
"
  :group 'magik-version
  :type  '(choice (file)
		  (const nil)))

(defcustom magik-version-match "^[* ] \\(\\S-+\\)\\s-*\\(\\S-+\\)\\s-*\\(.*\\)"
  "*Regexp matching valid versions listed by `gis-version-program' or `gis-version-file'."
  :group 'magik-version
  :type  'regexp)

(defcustom magik-version-program-error "gis_version: "
  "*The error string that the gis version program returns when stream is invalid."
  :group 'magik-version
  :type  'string)

(defcustom magik-version-invalid-string "(invalid)"
  "*The string marking an invalid gis version entry."
  :group 'magik-version
  :type  'string)

(defcustom magik-version-select-hook nil
  "*Hook to run after a selection has been made."
  :group 'magik-version
  :type  'hook)

(add-hook 'magik-version-select-hook  'magik-aliases-update-menu)

(defcustom magik-version-help "Select a Smallworld Core Product Installation.\n\nThe product you select will define the environment for any new Smallworld\nsessions that `F2 z' runs.\n\nTo make the selection, move the cursor to the line you want and press RETURN.\n\nPress q to exit and do nothing.\n\n"
  "Help text displayed at top of gis_version buffer."
  :group 'magik-version
  :type  'string)

(defcustom magik-version-help-file-add "To add a new installation, press +.\n"
  "Help text for Adding to `gis-version-file' displayed after `gis-version-help'."
  :group 'magik-version
  :type  'string)

(defcustom magik-version-font-lock-keywords
  (list
   '("^.*(invalid).*$" . font-lock-warning-face)
   '("^\\([*]\\s-+\\S-+\\)\\s-+\\(\\S-+\\)"
     (1 font-lock-constant-face)
     (2 font-lock-variable-name-face))
   '("^ \\s-+\\(\\S-+\\)\\s-+\\(\\S-+\\)"
     (1 font-lock-function-name-face)
     (2 font-lock-variable-name-face))
   '("^\\S-.*" . font-lock-doc-face)
   )
  "Default fontification of gis_version."
  :group 'magik-version
  :type 'sexp)

(defvar magik-version-file-format "  %-17s   %-23s   %s\n"
  "*Header format to use for a newly created gis version file.")

(defvar magik-version-file-header
  (concat (format magik-version-file-format "name" "version" "directory")
	  (format magik-version-file-format
		  "-----------------"
		  "------------------------"
		  "--------------------------------"))
  "*Header string to use for a newly created gis version file.")

(defvar magik-version-sw-path-list nil
  "Stores list of Smallworld directories added to PATH.")

(defvar magik-version-mode-map (make-sparse-keymap)
  "Keymap for selection of alternative GIS environments")

(defvar magik-version-menu nil
  "Keymap for the gis_version buffer menu bar")

(easy-menu-define magik-version-menu magik-version-mode-map
  "Menu for gis_version mode."
  `(,"Environment"
    [,"Select"                    magik-version-select      t]
    [,"Run"                       magik-version-run         t]
    [,"Next"                      magik-version-next        t]
    [,"Open aliases file"         magik-version-gis-aliases t]
    [,"Quit"                      magik-version-quit        t]
    "---"
    [,"Add new installation"      magik-version-file-add    magik-version-file]
    [,"Reset process environment" magik-version-reset-emacs-environment t]
    "---"
    [,"Customize"                 magik-version-customize   t]
    [,"Help"                      magik-version-help        t]))

(define-key magik-version-mode-map [f1]   'magik-version-help)
(define-key magik-version-mode-map " "    'magik-version-next)
(define-key magik-version-mode-map "a"    'magik-version-gis-aliases)
(define-key magik-version-mode-map "+"    'magik-version-file-add)
(define-key magik-version-mode-map "q"    'magik-version-quit)
(define-key magik-version-mode-map "r"    'magik-version-run)
(define-key magik-version-mode-map "\r"   'magik-version-select)
(define-key magik-version-mode-map [mouse-2] 'magik-version-mouse-select)

(defvar magik-version-mode-syntax-table nil
  "Syntax table in use in DEF-mode buffers.")

(defvar magik-version-current nil
  "Current gis_version stream.")
(make-variable-buffer-local 'magik-version-current)

(defvar magik-version-position nil
  "A position in the gis version buffer above which the user shouldn't click.")

(defun magik-version-help ()
  "Display help on how to use the magik-version Mode interface."
  (interactive)
  (sw-help-open sw-help-gis-version-id))

(defun magik-version-customize ()
  "Open Customization buffer for magik-version Mode."
  (interactive)
  (customize-group 'magik-version))

(defun magik-version (gis-version)
  "Return the output from running the gis version script (as opposed to the alias).
If it fails, return the symbol, 'failed."
  (save-excursion
    (set-buffer (generate-new-buffer "*gis-version temp buffer*"))
    (if magik-version-file
	(insert-file-contents magik-version-file)
      (call-process magik-version-program nil t nil gis-version))
    (goto-char (point-min))
    (prog1
	(if (search-forward magik-version-program-error nil t)
	    'failed
	  (buffer-string))
      (kill-buffer (current-buffer)))))

(defun magik-version-program-current ()
  "Run `gis-version-program' and determine the current version.
Return the current gis version that this Emacs inherited from its environment,
or nil if there isn't one, or 'no-gis-version-script if there is no gis version command."
  (save-excursion
    (set-buffer (generate-new-buffer "*gis-version temp buffer*"))
    (condition-case nil
	(progn
	  (call-process magik-version-program nil t nil)
	  (goto-char (point-min))
	  (prog1
	      (if (search-forward "\n* " nil t)
		  (buffer-substring
		   (point)
		   (progn (search-forward " ") (1- (point))))
		nil)
	    (kill-buffer (current-buffer))))
      (error
       (kill-buffer (current-buffer))
       'no-gis-version-script))))

(defun magik-version-run ()
  "Run GIS command in selected version."
  (interactive)
  (beginning-of-line)
  (let ((process-environment (copy-list process-environment))
	(exec-path (copy-list exec-path))
	stream version buffer)
    (setq stream (car (magik-version-select-internal))
	  buffer  (concat "*gis " stream "*"))
    (magik-shell buffer)
    (setq magik-version-current stream)))

(defun magik-version-gis-aliases ()
  "Open gis_aliases file of selected version.
Will prompt for layered product to use if selected version
has more than one aliases file available."
  ;;Does not cope with 'partial stream versions' where the directory components list 2 (or more directories)
  (interactive)
  (beginning-of-line)
  (let* ((process-environment (copy-list process-environment))
	 (exec-path (copy-list exec-path))
	 (version-list (magik-version-select-internal))
	 lp-alist
	 alias-file)
    (if (null (car version-list))
	(error "Invalid selection")
      (setq lp-alist (magik-aliases-layered-products-file
		      (magik-aliases-expand-file "$SMALLWORLD_GIS/config/LAYERED_PRODUCTS")))
      (cond ((null lp-alist) nil)
	    ((eq (length lp-alist) 1)
	     (setq alias-file (concat (cdar lp-alist) "/config/gis_aliases")))
	    (t
	     (let* ((lp   (completing-read "Select a Layered Product with gis_aliases file: " lp-alist nil t))
		    (path (cdr (assoc lp lp-alist))))
	       (if path
		   (setq alias-file (concat path "/config/gis_aliases"))))))
      (if alias-file
	  (progn
	    (find-file alias-file)
	    (setq magik-aliases-process-environment (copy-list process-environment)
		  magik-aliases-exec-path (copy-list exec-path)
		  magik-aliases-program-args (list "-p" (caddr version-list))
		  magik-version-current (car version-list))
	    ;;(set 'aliases-gis-version-current current-version)
	    )))))

(defun magik-version-next ()
  "Move point to next valid version listed."
  (interactive)
  (forward-line 1)
  (save-match-data
    (while (and (not (eobp))
		(or (looking-at (concat "^.*" magik-version-invalid-string))
		    (not (looking-at magik-version-match))))
      (forward-line 1))
    (if (not (eobp))
	(beginning-of-line)
      (goto-char magik-version-position)
      (re-search-forward magik-version-match nil t)
      (beginning-of-line))))

(defun magik-version-current ()
  "Return the current gis_version."
  (interactive)
  (if magik-version-file
      magik-version-current
    (magik-version-program-current)))

;;;###autoload
(defun magik-version-mode ()
  "Major Mode for gis_version."
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'magik-version-position)

  (use-local-map magik-version-mode-map)
  (easy-menu-add magik-version-menu)
  (set-syntax-table magik-version-mode-syntax-table)

  (setq major-mode 'magik-version-mode
	mode-name "Environment"
	buffer-undo-list t
	font-lock-defaults
	'(magik-version-font-lock-keywords
	  nil t
	  (("-" . "w"))))

  (run-hooks 'magik-version-hook))

(defun magik-version-smallworld-gis-p (path)
  "Return t if path points to a Smallworld installation."
  (file-directory-p (concat (file-name-directory path) "config")))

(defun magik-version-read-smallworld-gis-completion (string predicate flag)
  "Provide directory completion for finding Smallworld installations.
Repeated TAB and \\[minibuffer-completion-help] still provide
directory listing so users can navigate a directory structure looking
for a Smallworld installation. Only when
`gis-version-smallworld-gis-p' returns t for a given path will the
path be considered to be a real Smallworld installation directory
suitable for selection."
  (if (magik-version-smallworld-gis-p string)
      (cond ((eq flag #'lambda) t)
	    ((null flag)       t)
	    (t            string))
    (cl-flet ((read-smallworld-gis-predicate (d) (equal d (file-name-directory d))))
      (let ((root (file-name-directory string))
	    (completions (all-completions string
					  'read-file-name-internal
					  'read-smallworld-gis-predicate)))
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
		 (concat (or root "") completions))))))))

(defun magik-version-read-smallworld-gis ()
  "Prompt for a valid value for SMALLWORLD_GIS."
  (let ((path
	 (completing-read "Enter product directory for Core installation: "
			  'magik-version-read-smallworld-gis-completion
			  nil t nil nil (getenv "SMALLWORLD_GIS"))))
    (setq path (directory-file-name (file-name-directory path)))
    (subst-char-in-string ?/ ?\\ path t)
    path))

(defun magik-version-file-add (root name version)
  "Add a new entry to the file given by `gis-version-file'."
  (interactive
   (let* ((ok (or magik-version-file
		  (error "File interface is not being used")))
	  (root (magik-version-read-smallworld-gis))
	  (product-version-file (concat (file-name-as-directory root)
					"config/PRODUCT_VERSION"))
	  name version)
     (if (file-exists-p product-version-file)
	 (save-excursion
	   (set-buffer (get-buffer-create " *product_version*"))
	   (erase-buffer)
	   (insert-file-contents product-version-file)
	   (goto-char (point-min))
	   (setq version (current-word)
		 name    version)))
     (list root
	   (read-string "Enter name for this installation: " name)
	   (read-string "Enter version number of this installation: " version))))

  (or magik-version-file
      (error "File interface is not being used"))
  (save-excursion
    (set-buffer (find-file-noselect magik-version-file))
    (goto-char (point-max))
    (let ((inhibit-read-only t))
      (insert (format magik-version-file-format name version root))
      (save-buffer)))
  (if (eq major-mode 'magik-version-mode)
      (magik-version-selection)))

(defun magik-version-file-create (file root)
  "Create a gis version format file based upon the current environment.
Called if no magik-version program exists or `gis-version-file' is nil.
Will set `gis-version-file' to FILE."
  (interactive
   (if (y-or-n-p "Create File Interface for SW Environments? ")
       (let ((smallworld-gis (getenv "SMALLWORLD_GIS"))
	     (dir (file-name-as-directory (getenv "HOME")))
	     (file "gis_version.txt"))
	 (if smallworld-gis
	     nil
	   (setq smallworld-gis (magik-version-read-smallworld-gis)))
	 (list
	  (read-file-name "Enter New Environment List File: " dir file nil file)
	  smallworld-gis))
     (error "")))
  (setq magik-version-file file)

  (if (file-exists-p file)
      (find-file file)
    (find-file file)
    (insert magik-version-file-header)
    (let ((process-environment (copy-list process-environment)))
      (setenv "SMALLWORLD_GIS" root)
      (call-interactively 'magik-version-file-add))
    (save-buffer))
  nil)

(defun magik-version-selection ()
  "Display a list of possible gis products for the user to choose between."
  (interactive
   (cond (magik-version-file
	  ;; already supplied a file
	  nil)
	 ((eq magik-version-current 'no-gis-version-script)
	  (call-interactively 'magik-version-file-create))
	 (t nil)))
  (set-buffer (get-buffer-create "*gis version selection*"))
  (magik-version-mode)

  (message "Starting %s selection..." magik-version-program)

  (setq buffer-read-only nil)
  (erase-buffer)
  (insert magik-version-help)
  (if (and magik-version-file magik-version-help-file-add)
      (insert "\n" magik-version-help-file-add "\n"))

  (save-excursion
    (save-match-data
      (if (not magik-version-file)
	  (call-process magik-version-program nil t nil)
	(insert-file-contents magik-version-file)
	(goto-char (point-min))

	(if (search-forward "-------" nil t) (forward-line 1)) ;skip a header
	(while (re-search-forward magik-version-match nil t)
	  (beginning-of-line)
	  (forward-char 1)
	  (backward-delete-char 1)
	  (insert " ")
	  (cond ((string-match magik-version-invalid-string (match-string-no-properties 3))
		 nil)
		((file-exists-p (match-string-no-properties 3))
		 nil)
		(t
		 (goto-char (match-beginning 3))
		 (insert magik-version-invalid-string " ")))))))

  (if (stringp magik-version-current)
      (save-excursion
	(save-match-data
	  (if (re-search-forward (concat "^. " magik-version-current " ") nil t)
	      (progn
		(beginning-of-line)
		(delete-char 1)
		(insert "*"))))))

  (setq magik-version-position (point))
  (save-match-data
    (if (search-forward "-------" nil t) (setq magik-version-position (point)))) ;skip a header

  (message "Starting %s selection... done" magik-version-program)
  (setq buffer-read-only t)
  (set-buffer-modified-p nil)
  (switch-to-buffer (current-buffer)))

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

(defun magik-version-mouse-select (click)
  "Choose product using mouse event."
  (interactive "e")
  (mouse-set-point click)
  (beginning-of-line)
  (magik-version-select))

(defun magik-version-select ()
  "Store the gis product name in the global variable, `gis-version-current', so that
`F2 z' will set the correct product's environment before starting the gis.
Also now update the emacs `process-environment' correctly.
The frame and icon title strings will be modified according to
`gis-version-frame-title-format' and `gis-version-icon-title-format'."
  (interactive)
  (let ((stream (car (magik-version-select-internal))))
    (setq-default magik-version-current stream)
    (kill-buffer (current-buffer))
    (magik-version-display-title)
    (run-hooks 'magik-version-select-hook)
    (message "The current installation for this Emacs is now %s." stream)))

(defun magik-version-select-internal ()
  "Modify `process-environment' and `exec-path' for current version.
Return (STREAM VERSION SMALLWORLD_GIS)."
  (if (< (point) magik-version-position)
      (error "No Environment at this point"))
  (if (save-excursion
	(beginning-of-line)
	(search-forward magik-version-invalid-string (line-end-position) t))
      (error "You have selected an (invalid) Environment"))
  (let (stream
	version
	smallworld-gis)
    (beginning-of-line)
    (if (looking-at magik-version-match)
	(setq stream (match-string-no-properties 1)
	      version (match-string-no-properties 2)
	      smallworld-gis  (match-string-no-properties 3))
      (error "No Environment on this line"))
    (if (not (and magik-version-current
		  (string-equal stream magik-version-current)))
	(progn
	  (setq process-environment (copy-list magik-utils-original-process-environment)
		exec-path           (copy-list magik-utils-original-exec-path))

	  (magik-version-set-environment smallworld-gis
					 stream
					 version
					 (if (and magik-version-program
						  (not magik-version-file))
					     magik-version-program))))
    (list stream version smallworld-gis)))

(defun magik-version-set-environment (smallworld-gis stream version program)
  "Modify the process and exec-path environment given stream and smallworld-gis path."
  (setenv "SMALLWORLD_GIS" smallworld-gis)
  (setenv "SW_STREAM" stream)
  (setenv "SW_VERSION" version)

  (save-excursion
    (set-buffer (get-buffer-create " *temp gis stream select*"))
    (erase-buffer)
    ;; Use cmdproxy explicitly and not shell-file-name since Cygwin
    ;; may be being used and in this case 'set' sometimes reports shell values
    ;; to be VAR='c:/path/to/file' i.e it inserts ''s.
    (magik-version-call-process-windows "cmdproxy" nil t nil
					"/c"
					(concat "echo off && "
						(expand-file-name "config/environment.bat"  smallworld-gis)
						"&& set"))
    (goto-char (point-min))
    (magik-version-set-emacs-environment)))

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
For magik-version code to work the SMALLWORLD paths need to be prepended to PATH.
On UNIX PATH is modified to have SMALLWORLD appended (on Windows it is prepended).

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
	      ((and (> (setq cnt (count p new-list :test 'equal)) 1)
		    (not (eq cnt (count p orig-list :test 'equal))))
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
      (setq magik-version-sw-path-list (copy-list sw-list)
	    new (mapconcat 'directory-file-name
			   (append sw-list new-list)
			   path-separator))
      (subst-char-in-string ?/ ?\\ new))))

(defun magik-version-set-emacs-environment ()
  "Update `process-environment' and `exec-path' variables."
  (let* ((orig-shell (getenv "SHELL"))
	 (orig-path  (getenv "PATH")))
    (setq process-environment nil)
    (while (not (eobp))
      (if (looking-at ".*=")
	  (push (buffer-substring (line-beginning-position) (line-end-position)) process-environment))
      (forward-line 1))
    (setenv "SHELL" orig-shell)
    (setenv "PATH" (magik-version-prepend-sw-paths orig-path (getenv "PATH")))
    (setq exec-path (append (parse-colon-path (getenv "PATH"))
			    (parse-colon-path (getenv "EMACSPATH"))))))

(defun magik-version-reset-emacs-environment ()
  "Reset the Emacs environment back to how it was when Emacs started.
This affects all Environment variables `process-environment'
and the execution path `exec-path'."
  (interactive)
  (if (yes-or-no-p "Are you sure you want to reset the process environment? ")
      (progn
	(setq process-environment (copy-list magik-utils-original-process-environment)
	      exec-path           (copy-list magik-utils-original-exec-path))
	(setq-default magik-version-current nil)
	(magik-version-selection))))

(defun magik-version-header-string ()
  "Insert a string describing the gis_version status.
Used before running a GIS process."

  (let (gis-version-script)
    (if (stringp magik-version-current)
	(setq gis-version-script (gis-version magik-version-current)))
    (cond ((eq magik-version-current 'no-gis-version-script)
	   nil)
	  ((null magik-version-current)
	   (insert "\n** There is no currently selected gis product.\n** (Attempting to run anyway).\n\n"))
	  ((eq gis-version-script 'failed)
	   (insert (format "\n** Can't find the currently selected product, %s.\n** (Attempting to run anyway).\n" magik-version-current)))
	  (t
	   (insert (format "Gis Environment: %s\n" magik-version-current))))))

;;; Package initialisation
(if magik-version-mode-syntax-table
    nil
  (setq magik-version-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_  "w"  magik-version-mode-syntax-table))

(setq-default magik-version-current (magik-version-current))

(add-hook 'gis-start-process-pre-hook 'magik-version-header-string)

(provide 'magik-version)
;;; magik-version.el ends here
