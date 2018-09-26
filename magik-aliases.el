;;; magik-aliases.el --- mode for editing GIS aliases files.

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
  (require 'cl)
  (defvar msb-menu-cond)

  (require 'magik-utils))

(defgroup magik-aliases nil
  "Customise Magik aliases files group."
  :group 'magik
  :group 'tools)

(defcustom magik-aliases-user-file-list '("$HOME/gis_aliases")
  "A list of a User's personal gis_aliases files."
  :group 'magik-aliases
  :type  'file)

(defcustom magik-aliases-common-file-list nil
  "*List of common gis_aliases files.
This list is expected to be setup by the Emacs maintainer,
a user can setup their personal gis_aliases file list using
`aliases-user-file-list'. Both these lists are concatenated to
form the top section of the SW->Alias Files submenu."
  :group 'magik-aliases
  :type  '(repeat file))

(defcustom magik-aliases-mode-hook nil
  "*Hook to run after ALIASES mode is set."
  :group 'magik-aliases
  :type  'hook)

(defcustom magik-aliases-program "runalias.exe"
  "*Program to process an alias file."
  :group 'magik-aliases
  :type  'string)

(defcustom magik-aliases-program-path '("../bin/x86" "../../product/bin/x86")
  "*Path to `aliases-program'.
Setting this sets the default value. When opening a gis_aliases file,
the buffer local value of this variable will be set to the directory
containing the `aliases-program' if it is in a relative path to the file."
  :group 'magik-aliases
  :type  '(repeat directory))

(defcustom magik-aliases-program-args nil
  "*Arguments to pass to `aliases-program'."
  :group 'magik-aliases
  :type  '(repeat string))

(defcustom magik-aliases-switch-to-buffer t
  "*User control for switching to the process buffer of a selected alias.
If this is t then the buffer is displayed."
  :group 'magik-aliases
  :type  'boolean)

(defcustom magik-aliases-switch-to-buffer-regexp nil
  "*User control for switching to the process buffer of a selected alias.
If the alias name matches the given regular expression the buffer
is displayed."
  :group 'magik-aliases
  :type  '(choice regexp (const nil)))

(defcustom magik-aliases-switch-to-buffer-hooks nil
  "*User control for switching to the process buffer of a selected alias.
Each function in the hook is passed the name of the alias.
If any function returns t, then the buffer is displayed."
  :group 'magik-aliases
  :type  'hook)

(defvar magik-aliases-mode-map (make-sparse-keymap)
  "Keymap for GIS aliases files")

(define-key magik-aliases-mode-map [f1]   'magik-aliases-help)
(define-key magik-aliases-mode-map (kbd "<S-return>") 'magik-aliases-run-program)

(defvar magik-aliases-menu nil
  "Menu for Aliases mode.")

(easy-menu-define magik-aliases-menu magik-aliases-mode-map
  "Menu for aliases mode."
  `(,"Aliases"
    [,"Run current definition"        magik-aliases-run-program t]
    "----"
    (,"Definitions")
    "---"
    [,"Customize"                     magik-aliases-customize   t]
    [,"Help"                          magik-aliases-help        t]))

(defvar magik-aliases-mode-syntax-table nil
  "Syntax table in use in Aliases-mode buffers.")

(defvar magik-aliases-definition-regexp "^\\([^#]\\S-+\\):\\s-*$"
  "Regexp matching an alias definition")

;; Imenu configuration
(defvar magik-aliases-imenu-generic-expression
  (list
   (list nil magik-aliases-definition-regexp 1)
   )
  "Imenu generic expression for Aliases mode.  See `imenu-generic-expression'.")

;; Font-lock configuration
(defcustom magik-aliases-font-lock-keywords
  (list
   (cons magik-aliases-definition-regexp 'font-lock-function-name-face)
   '("^\\s-+\\([A-Z_]+\\)\\s-*:=" 1 font-lock-type-face)
   '("^\\s-+\\([A-Z_]+\\)\\s-*=" 1 font-lock-variable-name-face)
   '("^\\s-+\\(\\sw+\\)\\s-*=" 1 font-lock-builtin-face)
   '("\\s$\\sw+\\s$" . font-lock-constant-face)
   )
  "Default fontification of Aliases buffers."
  :group 'magik-aliases
  :type 'sexp)

(defvar magik-aliases-exec-path nil
  "Stored `exec-path' for executing GIS command.")

(defvar magik-aliases-process-environment nil
  "Stored `process-environment' for executing GIS command.")

(defun magik-aliases-help ()
  "Display help on how to use the Aliases Mode interface."
  (interactive)
  (sw-help-open sw-help-aliases-id))

(defun magik-aliases-customize ()
  "Open Customization buffer for Aliases Mode."
  (interactive)
  (customize-group 'magik-aliases))

;;;###autoload
(defun magik-aliases-mode ()
  "Major mode for editing Magik aliases files.

You can customise magik-aliases-mode with the magik-aliases-mode-hook."

  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'require-final-newline)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'magik-aliases-program)
  (make-local-variable 'magik-aliases-exec-path)
  (make-local-variable 'magik-aliases-process-environment)

  (use-local-map magik-aliases-mode-map)
  (easy-menu-add magik-aliases-menu)
  (set-syntax-table magik-aliases-mode-syntax-table)

  (setq major-mode 'magik-aliases-mode
	mode-name "Aliases"
	magik-aliases-program (magik-aliases-program-set magik-aliases-program)
	require-final-newline t
	comment-start "#"
	comment-end   ""
	imenu-generic-expression magik-aliases-imenu-generic-expression
	font-lock-defaults
	'(magik-aliases-font-lock-keywords
	  nil nil))

  (add-hook 'menu-bar-update-hook 'magik-aliases-update-menu)
  (add-hook 'kill-buffer-hook 'magik-aliases-kill-buffer nil t)
  ;;Avoid menu-bar-update-hook, since this is executed
  ;;many times and the magik-aliases-update-sw-menu function does
  ;;perform file existence checks. So by using a local kill-buffer-hook
  ;;it should cut down on the number of times this function is executed
  ;;whilst still retaining the accuracy of the SW->Alias Files submenu.
  ;;(add-hook menu-bar-update-hook-sym 'magik-aliases-update-sw-menu)

  (run-hooks 'magik-aliases-mode-hook))

(defun magik-aliases-kill-buffer ()
  "Function to run when an Aliases mode buffer is run."
  (if (eq major-mode 'magik-aliases-mode)
      (progn
	(setq major-mode 'fundamental-mode) ; prevent current buffer being listed.
	(magik-aliases-update-sw-menu))))

(defun magik-aliases-list ()
  "Return list of alias definitions."
  (let (list)
    (save-excursion
      (save-match-data
	(goto-char (point-max))
	(while (re-search-backward magik-aliases-definition-regexp nil t)
	  (push (match-string-no-properties 1) list))))
    list))

(defun magik-aliases-switch-to-buffer (alias)
  "Return t, to switch to the buffer that the GIS.exe process is running in.
Since some entries in the aliases file do not start a Smallworld Magik GIS
process we do not necessarily want to switch to the buffer running the
process all the time. These are the following methods by which we control
when the buffer is displayed:
  Hook: `aliases-switch-to-buffer-hooks'
       Each function in the hook is passed the name of the alias.
       If any function returns t, then the buffer is displayed.
  Regexp: `aliases-switch-to-buffer-regexp'
       If the alias name matches the given regular expression the buffer
       is displayed.
  Variable: `aliases-switch-to-buffer'
       If this is t then the buffer is displayed.
"
  (cond ((run-hook-with-args-until-success 'magik-aliases-switch-to-buffer-hooks alias)
	 t)
	((stringp magik-aliases-switch-to-buffer-regexp)
	 (save-match-data
	   (match-string magik-aliases-switch-to-buffer-regexp alias)))
	(t
	 magik-aliases-switch-to-buffer)))

(defun magik-aliases-program-set (&optional default)
  "Return the program to use to operate on a gis_aliases file."
  (let ((path magik-aliases-program-path)
	finished program)
    (while path
      (setq program (expand-file-name
		     (concat (file-name-as-directory (car path)) default))
	    path (cdr path))
      (if (file-executable-p program)
	  (setq path nil)
	(setq program nil)))
    (or program default)))

(defun magik-aliases-run-program (&optional alias file dir)
  "Run gis.exe on the aliases file.

With a prefix arg, ask user for current directory to use."
  (interactive (if (not (magik-aliases-at-alias-definition))
		   (list
		    (completing-read "Definition: "
				     (mapcar (function
					      (lambda (d) (cons d d)))
					     (magik-aliases-list))
				     nil t)
		    nil nil)))
  (cond (current-prefix-arg
	 (setq dir (file-name-as-directory
		    (expand-file-name
		     (read-file-name "Set current working directory: ")))))
	((null dir)
	 (setq dir default-directory)))

  (let ((program magik-aliases-program)
	(args    magik-aliases-program-args)
	(file    (or file (buffer-file-name)))
	(buf     "gis")
	(version (if (boundp 'magik-version-current)
		     (symbol-value 'magik-version-current)))
	(process-environment-aliases magik-aliases-process-environment)
	(exec-path-aliases magik-aliases-exec-path))
    (save-excursion
      (cond (alias nil)
	    ((re-search-backward magik-aliases-definition-regexp nil t)
	     (setq alias (match-string-no-properties 1)))
	    (t
	     (error "Cannot find any alias definitions")))
      (if (file-exists-p (concat (file-name-directory file) "environment.bat"))
	  (setq args (append args (list "-e" (concat (file-name-directory file) "environment.bat")) nil)))
      (setq args (append args (list "-a" file alias) nil)) ;; alias name MUST be last

      (if (stringp version)
	  (setq buf (concat buf " " version)))
      (if alias
	  (setq buf (concat buf " " alias)))
      (setq buf (generate-new-buffer (concat "*" buf "*")))
      (set-buffer buf)
      (magik-shell-mode)

      (insert "Command: " program " ")
      (mapc (function (lambda (s) (insert s " "))) args)
      (setq default-directory dir
	    args (append (list program) args)
	    magik-shell-exec-path (copy-list (or exec-path-aliases exec-path))
	    magik-shell-process-environment (copy-list (or process-environment-aliases process-environment))
	    magik-shell-current-command (mapconcat 'identity args " "))
      (if (stringp version)
	  (set 'magik-version-current version))

      (insert (format "\nCwd is: %s\n\n" default-directory))
      (magik-shell-start-process args))
    (if (magik-aliases-switch-to-buffer alias)
	(switch-to-buffer buf))))

(defun magik-aliases-at-alias-definition ()
  "Return definition, if point is in an alias definition."
  (save-excursion
    (beginning-of-line)
    (save-match-data
      (cond ((looking-at magik-aliases-definition-regexp)
	     (match-string-no-properties 1))
	    ((looking-at "\\(\\s-\\|\n\\)*\n\\sw+:\\s-*$")
	     ;;At point in between definitions
	     nil)
	    ((re-search-backward magik-aliases-definition-regexp nil t)
	     (match-string-no-properties 1))
	    (t nil)))))

(defun magik-aliases-expand-file (file)
  "Expand FILE path including environment variables.
Returns nil if FILE cannot be expanded."
  (condition-case nil
      (expand-file-name (substitute-in-file-name file))
    (error nil)))

(defun magik-aliases-layered-products-file (file)
  "Read contents of FILE with the format of LAYERED_PRODUCTS configuration file."
  (if (file-exists-p file)
      (save-excursion
	(set-buffer (get-buffer-create " *aliases LAYERED_PRODUCTS*"))
	(insert-file-contents file nil nil nil 'replace)

	;; Always ensure that a default sw_core: set to SMALLWORLD_GIS is present
	;; in case the value has been manually modified but we still wish to locate
	;; a gis_aliases file next to the LAYERED_PRODUCTS file.
	(goto-char (point-min))
	(insert "sw_core:\n	path		= %SMALLWORLD_GIS%\n")
	(magik-aliases-layered-products-alist))))

(defun magik-aliases-layered-products-alist ()
  "Return alist of contents for LAYERED_PRODUCTS file."
  (save-excursion
    (save-match-data
      (let (alist pt lp dir)
	(goto-char (point-min))
	(while (re-search-forward "^\\([^\r\n:]+\\):" nil t)
	  (setq lp (match-string-no-properties 1))
	  (if (re-search-forward "^\\s-*path\\s-*=\\s-*" nil t)
	      (progn
		(setq pt (point))
		(end-of-line)
		(skip-syntax-backward "-")
		(skip-chars-backward "/\\") ;avoid trailing directory character.
		(setq dir
		      (magik-aliases-expand-file
		       (buffer-substring-no-properties pt (point))))
		(if (file-exists-p (concat dir "/config/gis_aliases"))
		    (let ((lp-dir (cons lp dir)))
		      (or (member lp-dir alist) (push lp-dir alist))) ))))
	alist))))

(defun magik-aliases-update-menu ()
  "Update the dynamic Aliases submenu."
  (interactive)
  (if (eq major-mode 'magik-aliases-mode)
      (let ((aliases (magik-aliases-list))
	    entries def)
	(while aliases
	  (setq def (car aliases)
		aliases (cdr aliases))
	  (push (vector def (list 'magik-aliases-run-program def) t) entries)) ;; :key-sequence nil
	(easy-menu-change (list "Aliases")
			  "Definitions"
			  (or entries (list "No Aliases found"))))))

(defun magik-aliases-update-sw-menu ()
  "Update 'resources-menu-sw-alias-files' submenu in SW menu bar."
  (interactive)
  (let (default-files
	 lp-files
	 buffers
	 (rescan (list "---" (vector "*Rescan*" 'magik-aliases-update-sw-menu t))))
    (dolist (f (append magik-aliases-user-file-list magik-aliases-common-file-list ))
      (push `[,f
	      (progn
		(find-file (magik-aliases-expand-file ,f))
		(magik-aliases-mode))
	      (and ,f (magik-aliases-expand-file ,f))
	      ]
	    default-files))

    (when (getenv "SMALLWORLD_GIS")
      (dolist (lp (magik-aliases-layered-products-file
		   (magik-aliases-expand-file "$SMALLWORLD_GIS/../smallworld_registry/LAYERED_PRODUCTS")))
	(push `[,(format "%s: %s" (car lp) (cdr lp))
		(progn
		  (find-file ,(concat (cdr lp) "/config/gis_aliases"))
		  (magik-aliases-mode))
		,(cdr lp)
		]
	      lp-files))
      (push "---" lp-files))

    (loop for buf in (magik-utils-buffer-mode-list 'magik-aliases-mode)
	  do (push (vector (buffer-file-name (get-buffer buf))
			   (list 'switch-to-buffer buf)
			   t) buffers))
    (or (eq (length buffers) 0) (push "---" buffers))

    (easy-menu-change (list "Tools" "Magik")
		      "Alias Files"
		      (append default-files lp-files buffers rescan))))

;;; Package initialisation
(add-hook 'magik-aliases-mode-hook 'magik-aliases-update-sw-menu)

(if magik-aliases-mode-syntax-table
    ()
  (setq magik-aliases-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?_ "w" magik-aliases-mode-syntax-table)
  (modify-syntax-entry ?: "w" magik-aliases-mode-syntax-table)
  (modify-syntax-entry ?% "$" magik-aliases-mode-syntax-table); Windows Environment vars
  (modify-syntax-entry ?# "<" magik-aliases-mode-syntax-table)
  (modify-syntax-entry ?\n ">" magik-aliases-mode-syntax-table))

;;; Package registration
(or (assoc "aliases$" auto-mode-alist)
    (push '("aliases$" . magik-aliases-mode) auto-mode-alist))
(or (assoc "aliases.txt$" auto-mode-alist)
    (push '("aliases.txt$" . magik-aliases-mode) auto-mode-alist))

;;; speedbar configuration
(eval-after-load 'speedbar
  '(speedbar-add-supported-extension "aliases$"))

;;MSB configuration
(defun magik-aliases-msb-configuration ()
  "Adds Aliases files to msb menu, supposes that msb is already loaded."
  (let* ((l (length msb-menu-cond))
	 (last (nth (1- l) msb-menu-cond))
	 (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
	 (handle (1- (nth 1 last))))
    (setcdr precdr (list
		    (list
		     '(eq major-mode 'magik-aliases-mode)
		     handle
		     "Aliases Files (%d)")
		    last))))

(eval-after-load 'msb
  '(magik-aliases-msb-configuration))

(provide 'magik-aliases)
;;; magik-aliases.el ends here
