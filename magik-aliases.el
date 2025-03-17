;;; magik-aliases.el --- mode for editing GIS aliases files.   -*- lexical-binding: t; -*-

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
  (defvar msb-menu-cond)
  (require 'magik-utils))

(require 'easymenu)
(require 'compat)

(defgroup magik-aliases nil
  "Customise Magik aliases files group."
  :group 'magik
  :group 'tools)

(defcustom magik-aliases-user-file-list '("$HOME/gis_aliases")
  "A list of a User's personal gis_aliases files."
  :group 'magik-aliases
  :type  '(repeat file))

(defcustom magik-aliases-common-file-list nil
  "*List of common gis_aliases files.
This list is expected to be setup by the Emacs maintainer,
a user can setup their personal gis_aliases file list using
`magik-aliases-user-file-list'.  Both these lists are concatenated to
form the top section of the SW->Alias Files submenu."
  :group 'magik-aliases
  :type  '(repeat file))

(defcustom magik-aliases-program "runalias.exe"
  "*Program to process an alias file."
  :group 'magik-aliases
  :type  'string)

(defcustom magik-aliases-program-path '("../bin/x86" "../../product/bin/x86")
  "*Path to `magik-aliases-program'.
Setting this sets the default value.  When opening a gis_aliases file,
the buffer local value of this variable will be set to the directory
containing the `magik-aliases-program' if it is in a relative path to the file."
  :group 'magik-aliases
  :type  '(repeat directory))

(defcustom magik-aliases-program-args nil
  "*Arguments to pass to `aliases-program'."
  :group 'magik-aliases
  :type  '(repeat string))

(defcustom magik-aliases-layered-products-file "$SMALLWORLD_GIS/../smallworld_registry/LAYERED_PRODUCTS"
  "*The default location of the LAYERED_PRODUCTS file."
  :group 'magik
  :type 'string)

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

(defvar magik-aliases-definition-regexp "^\\([^#]\\S-+\\):\\s-*$"
  "Regexp matching an alias definition.")

;; Imenu configuration
(defvar magik-aliases-imenu-generic-expression
  (list
   (list nil magik-aliases-definition-regexp 1))
  "Imenu generic expression for Aliases mode.  See `imenu-generic-expression'.")

;; Font-lock configuration
(defcustom magik-aliases-font-lock-keywords
  (list
   (cons magik-aliases-definition-regexp 'font-lock-function-name-face)
   '("^\\s-+\\([A-Z_]+\\)\\s-*:=" 1 font-lock-type-face)
   '("^\\s-+\\([A-Z_0-9]+\\)\\s-*=" 1 font-lock-variable-name-face)
   '("^\\s-+\\(\\sw+\\)\\s-*=" 1 font-lock-builtin-face)
   '("\\s$\\sw+\\s$" . font-lock-constant-face))
  "Default fontification of Aliases buffers."
  :group 'magik-aliases
  :type 'sexp)

(defvar magik-aliases-exec-path nil
  "Stored variable `exec-path' for executing Magik session command.")

(defvar magik-aliases-process-environment nil
  "Stored variable `process-environment' for executing Magik session command.")

(defun magik-aliases-customize ()
  "Open Customization buffer for Aliases Mode."
  (interactive)
  (customize-group 'magik-aliases))

;;;###autoload
(define-derived-mode magik-aliases-mode nil "Aliases"
  "Major mode for editing Magik aliases files.

You can customise `magik-aliases-mode' with the `magik-aliases-mode-hook'.

\\{magik-aliases-mode-map}"
  :group 'magik
  :abbrev-table nil

  (compat-call setq-local
               require-final-newline t
               comment-start "#"
               comment-end ""
               show-trailing-whitespace nil
               magik-aliases-program (magik-aliases-program-set magik-aliases-program)
               imenu-generic-expression magik-aliases-imenu-generic-expression
               font-lock-defaults '(magik-aliases-font-lock-keywords nil nil))

  (add-hook 'menu-bar-update-hook 'magik-aliases-update-menu nil t)
  (add-hook 'kill-buffer-hook 'magik-aliases-kill-buffer nil t))

(defvar magik-aliases-menu nil
  "Menu for Aliases mode.")

(easy-menu-define magik-aliases-menu magik-aliases-mode-map
  "Menu for aliases mode."
  `(,"Aliases"
    [,"Run current definition"        magik-aliases-run-program t]
    [,"Next"                          magik-aliases-next        t]
    [,"Quit"                          magik-aliases-quit        t]
    "----"
    (,"Definitions")
    "---"
    [,"Customize"                     magik-aliases-customize   t]))

(defun magik-aliases-kill-buffer ()
  "Function to run when an Aliases mode buffer is run."
  (if (eq major-mode 'magik-aliases-mode)
      (progn
        (setq major-mode 'fundamental-mode) ; prevent current buffer being listed.
        (magik-aliases-update-sw-menu))))

(defun magik-aliases-n ()
  "If buffer is read-only goto next alias, else insert SPC."
  (interactive)
  (if buffer-read-only
      (magik-aliases-next)
    (magik-aliases-insert " ")))

(defun magik-aliases-down ()
  "If buffer is read-only goto next alias, else insert <down>."
  (interactive)
  (if buffer-read-only
      (magik-aliases-next)
    (forward-line)))

(defun magik-aliases-next ()
  "Move point to next valid alias listed."
  (interactive)
  (save-match-data)
  (if (re-search-forward magik-aliases-definition-regexp nil t)
      (forward-line)
    (goto-char (point-min))
    (when (re-search-forward magik-aliases-definition-regexp nil t)
      (forward-line))))

(defun magik-aliases-q ()
  "If buffer is read-only goto next alias, else insert q."
  (interactive)
  (if buffer-read-only
      (magik-aliases-quit)
    (magik-aliases-insert "q")))

(defun magik-aliases-quit ()
  "Quit, without selecting anything, aliases selection mode."
  (interactive)
  (kill-buffer (current-buffer)))

(defun magik-aliases-insert (arg)
  "Insert ARG at point."
  (insert arg))

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
  "Return t, to switch to the buffer that the Magik process is running in.
Since some entries in the aliases file do not start a Smallworld Magik
process we do not necessarily want to switch to the buffer running the
process all the time.  These are the following methods by which we control
when the buffer is displayed:
  Hook: `aliases-switch-to-buffer-hooks'
       Each function in the hook is passed the name of the ALIAS.
       If any function returns t, then the buffer is displayed.
  Regexp: `aliases-switch-to-buffer-regexp'
       If the ALIAS name matches the given regular expression the buffer
       is displayed.
  Variable: `aliases-switch-to-buffer'
       If this is t then the buffer is displayed."
  (cond ((run-hook-with-args-until-success 'magik-aliases-switch-to-buffer-hooks alias)
         t)
        ((stringp magik-aliases-switch-to-buffer-regexp)
         (save-match-data
           (match-string magik-aliases-switch-to-buffer-regexp alias)))
        (t
         magik-aliases-switch-to-buffer)))

(defun magik-aliases-program-set (&optional default)
  "Return the program to use to operate on a gis_aliases file.
Optionally a DEFAULT program can be set."
  (let ((path magik-aliases-program-path)
        program)
    (while path
      (setq program (expand-file-name
                     (concat (file-name-as-directory (car path)) default))
            path (cdr path))
      (if (file-executable-p program)
          (setq path nil)
        (setq program nil)))
    (unless program
      (setq program (expand-file-name
                     (concat (getenv "SMALLWORLD_GIS") "/config/"
                             (file-name-as-directory (car magik-aliases-program-path)) magik-aliases-program)))
      (unless (file-executable-p program)
        (setq program nil)))
    (or program default)))

(defun magik-aliases-run-program (&optional alias file dir)
  "Run `runalias' on the ALIAS FILE in DIR.

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
             (error "Can't find any alias definitions")))
      (if (file-exists-p (concat (file-name-directory file) "environment.bat"))
          (setq args (append args (list "-e" (concat (file-name-directory file) "environment.bat")) nil)))
      (setq args (append args (list "-a" file alias) nil)) ;; alias name MUST be last

      (if (stringp version)
          (setq buf (concat buf " " version)))
      (if alias
          (setq buf (concat buf " " alias)))
      (setq buf (generate-new-buffer (concat "*" buf "*")))
      (kill-buffer (current-buffer))
      (set-buffer buf)
      (magik-session-mode)

      (insert "Command: " program " ")
      (mapc (function (lambda (s) (insert s " "))) args)
      (setq default-directory dir
            args (append (list program) args))
      (compat-call setq-local
            magik-session-exec-path (cl-copy-list (or exec-path-aliases exec-path))
            magik-session-process-environment (cl-copy-list (or process-environment-aliases process-environment))
            magik-session-current-command (mapconcat 'identity args " "))
      (if (stringp version)
          (set 'magik-version-current version))

      (insert (format "\nCwd is: %s\n\n" default-directory))
      (magik-session-start-process args))
    (if (magik-aliases-switch-to-buffer alias)
        (display-buffer buf))))

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
Returns nil if FILE can't be expanded."
  (condition-case nil
      (expand-file-name (substitute-in-file-name (replace-regexp-in-string "\\%[^%]*\\%" (lambda (a) (concat "$" (substring a 1 -1))) file nil 'literal)))
    (error nil)))

(defun magik-aliases-layered-products-file (file)
  "Read contents of FILE with the format of LAYERED_PRODUCTS configuration file."
  (when (file-exists-p file)
    (with-current-buffer (get-buffer-create " *aliases LAYERED_PRODUCTS*")
      (insert-file-contents file nil nil nil 'replace)

      ;; Always ensure that a default sw_core: set to SMALLWORLD_GIS is present
      ;; in case the value has been manually modified but we still wish to locate
      ;; a gis_aliases file next to the LAYERED_PRODUCTS file.
      (goto-char (point-min))
      (insert "sw_core:\n path    = %SMALLWORLD_GIS%\n")
      (magik--aliases-layered-products-alist))))

(defun magik--aliases-layered-products-alist ()
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
                      (or (member lp-dir alist) (push lp-dir alist))))))
        alist)))))

(defun magik-aliases-layered-products-acp-path (file)
  "Read LAYERED_PRODUCTS configuration file.

  Read contents of FILE with the format of LAYERED_PRODUCTS configuration file
  and return paths to append to the variable `exec-path'."
  (when (file-exists-p file)
    (with-current-buffer (get-buffer-create " *aliases LAYERED_PRODUCTS*")
      (insert-file-contents file nil nil nil 'replace)

      ;; Always ensure that a default sw_core: set to SMALLWORLD_GIS is present
      ;; in case the value has been manually modified but we still wish to locate
      ;; a gis_aliases file next to the LAYERED_PRODUCTS file.
      (goto-char (point-min))
      (insert "sw_core:\n path    = %SMALLWORLD_GIS%\n")
      (magik--aliases-layered-products-acp-list))))

(defun magik--aliases-layered-products-acp-list ()
  "Return list of ACP paths."
  (save-excursion
    (save-match-data
      (let (paths pt dir etc-dir)
        (goto-char (point-min))
        (while (re-search-forward "^\\([^\r\n:]+\\):" nil t)
          (if (re-search-forward "^\\s-*path\\s-*=\\s-*" nil t)
              (progn
                (setq pt (point))
                (end-of-line)
                (skip-syntax-backward "-")
                (skip-chars-backward "/\\") ;avoid trailing directory character.
                (setq dir
                      (magik-aliases-expand-file
                       (buffer-substring-no-properties pt (point)))
                      etc-dir (concat dir (if (eq system-type 'windows-nt)
                                              "/etc/x86"
                                            "/etc/Linux.x86")))
                (if (file-directory-p etc-dir)
                    (push etc-dir paths)))))
        paths))))

(defun magik-aliases-update-menu ()
  "Update the dynamic Aliases submenu."
  (interactive)
  (if (eq major-mode 'magik-aliases-mode)
      (let ((aliases (magik-aliases-list))
            entries def)
        (while aliases
          (setq def (car aliases)
                aliases (cdr aliases)
                entries (nconc entries (list (vector def (list 'magik-aliases-run-program def) t)))))
        (easy-menu-change (list "Aliases")
                          "Definitions"
                          (or entries (list "No Aliases found"))))))

(defun magik-aliases-update-sw-menu ()
  "Update `Alias Files' submenu in SW menu bar."
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
                   (magik-aliases-expand-file magik-aliases-layered-products-file)))
        (push `[,(format "%s: %s" (car lp) (cdr lp))
                (progn
                  (find-file ,(concat (cdr lp) "/config/gis_aliases"))
                  (magik-aliases-mode))
                ,(cdr lp)
                ]
              lp-files))
      (push "---" lp-files))

    (cl-loop for buf in (magik-utils-buffer-mode-list 'magik-aliases-mode)
             do (push (vector (buffer-file-name (get-buffer buf))
                              (list 'display-buffer buf)
                              t) buffers))
    (or (eq (length buffers) 0) (push "---" buffers))

    (easy-menu-change (list "Tools" "Magik")
                      "Alias Files"
                      (append default-files lp-files buffers rescan))))

;;; Package initialisation
(add-hook 'magik-aliases-mode-hook 'magik-aliases-update-sw-menu)

(modify-syntax-entry ?_ "w" magik-aliases-mode-syntax-table)
(modify-syntax-entry ?: "w" magik-aliases-mode-syntax-table)
(modify-syntax-entry ?% "$" magik-aliases-mode-syntax-table); Windows Environment vars
(modify-syntax-entry ?# "<" magik-aliases-mode-syntax-table)
(modify-syntax-entry ?\n ">" magik-aliases-mode-syntax-table)

;;; Package registration
(or (assoc "aliases$" auto-mode-alist)
    (push '("aliases$" . magik-aliases-mode) auto-mode-alist))
(or (assoc "aliases.txt$" auto-mode-alist)
    (push '("aliases.txt$" . magik-aliases-mode) auto-mode-alist))

;;MSB configuration
(defun magik-aliases-msb-configuration ()
  "Add Aliases files to msb menu, supposes that msb is already loaded."
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

(with-eval-after-load 'msb
  (magik-aliases-msb-configuration))

(progn
  ;; ------------------------ magik aliases mode ------------------------

  (define-key magik-aliases-mode-map (kbd "<S-return>") 'magik-aliases-run-program)
  (define-key magik-aliases-mode-map " "                'magik-aliases-n)
  (define-key magik-aliases-mode-map (kbd "<down>")     'magik-aliases-down)
  (define-key magik-aliases-mode-map "q"                'magik-aliases-q))

(provide 'magik-aliases)
;;; magik-aliases.el ends here
