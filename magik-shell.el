;;; magik-shell.el --- mode for running a Smallworld Magik interactive process

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
;;; The filter for the magik shell process is in magik-shell-filter.el
;;
;; This is a new version of the gis-mode that uses a vector of marker
;; pairs rather than a list.  This allows us to move up and down the
;; array efficiently and also use things like binary search.
;;
;; Every hundred commands, a new bigger vector is created and the
;; invalid or degenerate previous commands are cleaned out - previous
;; commands are counted as degenerate if they point at a non-existent
;; buffer (cos the user killed the buffer and created a new one) or
;; have length zero (cos the user deleted the text the the markers
;; bounded).
;;
;; Unlike previous versions, the markers will mark the whole of the
;; text sent to the gis, including the dollar and the return.
;;
;; Note that all previous commands need to be kept, not just the last
;; 20 or so because the markers are the only way we can know what the
;; commands were - looking for prompts and dollars is too unreliable.
;;
;; Unlike shell-mode, we don't keep comint-last-input-start and
;; comint-last-input-end.  (??? I've changed last-input-* to
;; comint-last-input-* everywhere.  I hope this still works!)
;;
;; The previous commands are kept in a buffer local variable called
;; magik-shell-prev-cmds.
;;
;; Where possible, we try to allow more than one gis to be running.
;; This gets a bit tricky for things like transmit-method-to-magik
;; because they have to know where to send the magik to.  In order to
;; simplify this, we are getting rid of the variable,
;; magik-process-name, because it is a duplicate of magik-shell-buffer.  We
;; also don't ever refer to the process by its name but always by its
;; buffer - this should save any confusion with gis process naming.
;;
;; We don't rely on the form of the prompt any more.  We just rely on
;; it ending in a space.  The only place where we need to be sure is in
;; the filter.
;;
;; In this version of magik-shell-mode, we don't have any automatic
;; indentation of magik-code.  The tab is just for inserting tabs and
;; nothing else.
;;
;; During a sequence of M-p and M-n commands, the actual command
;; we're looking at is recorded in the buffer-local variable,
;; magik-shell-cmd-num.
;;
;; Unlike direct-gis-mode.el we keep the oldest command at the front.
;; This is fine because we can get to the end of a vector quickly.
;; We record how many commands are in our vector in the buffer-local
;; variable, magik-shell-no-of-cmds.  To get rid of annoying edge
;; effects in going up and down the vector, we keep a pair of markers
;; that bound an empty bit of text at the end of the vector.
;;
;; Arbitrary decision: if a command is recalled by grabbing it with
;; the RET key, the magik-shell-cmd-num is set to 0 (as if it had been
;; typed by hand) rather than the number of the command that was
;; recalled.

;;; Code:

(eval-when-compile
  (require 'comint)
  (defvar comint-last-input-start)
  (defvar comint-last-input-end)
  (defvar msb-menu-cond))

(require 'cl)
(require 'magik-mode)
(require 'magik-electric)
(require 'magik-indent)
(require 'magik-pragma)
(or (boundp 'ac-sources) (setq ac-sources nil))

(defcustom magik-shell-buffer nil
  "*The default Smallworld session.
Used for switching to the first Smallworld session."
  :group 'magik
  :type '(choice string (const nil)))

(defcustom magik-shell-buffer-default-name "*gis*"
  "*The default name of a Gis process buffer when creating new Smallworld sessions."
  :group 'magik
  :type 'string)

(defcustom magik-shell-prompt nil
  "String or Regular expression identifing the default Magik Prompt.
If global value is nil, a GIS session will attempt to discover the current
setting of the Magik Prompt by calling `magik-shell-prompt-get'."
  :group 'magik
  :type '(choice regexp (const nil)))

					; paulw - preset rather than allow discovery (which doesn't seem to work)
(setq magik-shell-prompt "Magik\\(\\|SF\\)> ")

(defcustom magik-shell-command-history-max-length 90
  "*The maximum length of the displayed `magik-shell-command' in the SW -> GIS Command History submenu.
`magik-shell-command' is a string of the form \"[DIRECTORY] COMMAND ARGS\"."
  :group 'magik
  :type  'integer)

(defcustom magik-shell-command-history-max-length-dir (floor (/ magik-shell-command-history-max-length 2))
  "*The maximum length of the displayed directory path in the SW -> GIS Command History submenu."
  :group 'magik
  :type  'integer)

(defcustom magik-shell-recall-cmd-move-to-end nil
  "*If t, move the cursor point to the end of the recalled command.
This behaviour is available for \\[recall-prev-gis-cmd] and \\[recall-next-gis-cmd] only.
The default is nil, which preserves the original behaviour to leave
the cursor point in the same position.

The similar commands, \\[recall-prev-matching-gis-cmd] and \\[recall-next-matching-gis-cmd]
that use command string matching are not affected by this setting."
  :group 'magik
  :type 'boolean)

(defcustom magik-shell-font-lock-prompt-face 'font-lock-type-face
  "*Font-lock Face to use when displaying the Magik Prompt."
  :group 'magik
  :type 'face)

(defcustom magik-shell-font-lock-error-face 'font-lock-warning-face
  "*Font-lock Face to use when displaying Error lines."
  :group 'magik
  :type 'face)

(defcustom magik-shell-font-lock-traceback-face 'font-lock-warning-face
  "*Font-lock Face to use when displaying Traceback lines."
  :group 'magik
  :type 'face)

(defcustom magik-shell-font-lock-keywords
  (append
   magik-font-lock-keywords-1
   magik-font-lock-keywords-2
   (list
    '("^\\*\\*\\*\\* Error:.*$"      0 magik-shell-font-lock-error-face t)
    '("^\\*\\*\\*\\* Warning:.*$"    0 font-lock-warning-face t)
    '("^---- traceback.* ----" . magik-shell-font-lock-traceback-face)
    '("^@.*$"                . font-lock-reference-face)
    ;;magik-shell-prompt entries are handled by magik-shell-filter-set-gis-prompt-action
    ))
  "Additional expressions to highlight in GIS mode."
  :type 'sexp
  :group 'magik)

(defcustom magik-shell-mode-hook nil
  "*Hook for customising GIS mode."
  :type 'hook
  :group 'magik)

(defcustom magik-shell-start-process-pre-hook nil
  "*Hook run before starting the process."
  :type 'hook
  :group 'magik)

(defcustom magik-shell-start-process-post-hook nil
  "*Hook run after starting the process."
  :type 'hook
  :group 'magik)

(defcustom magik-shell-auto-insert-dollar nil
  "Controls whether gis mode automatically inserts a $ after each valid magik statement."
  :group 'magik
  :type 'boolean)

(defcustom magik-shell-sentinel-hooks nil
  "*Hooks to run after the gis process has finished.
Each hook is passed the exit status of the gis process."
  :type 'hook
  :group 'magik)

(defcustom magik-shell-drag-n-drop-mode nil
  "Variable storing setting of \\[magik-shell-drag-n-drop-mode].

To make this mode operate on a per-buffer basis, simply make
this variable buffer-local by putting the following in your .emacs

  (defvar magik-shell-mode-hook nil)
  (defun magik-shell-drag-n-drop-mode-per-buffer ()
    (set (make-local-variable 'magik-shell-drag-n-drop-mode) magik-shell-drag-n-drop-mode))
  (add-hook 'magik-shell-mode-hook 'magik-shell-drag-n-drop-mode-per-buffer)
"

  ;;Use of integers is a standard way of forcing minor modes on and off.
  :type '(choice (const :tag "On" 1)
		 (const :tag "Off" -1))
  :group 'magik)

(defvar magik-shell-buffer-alist nil
  "Alist storing GIS buffer name and number used for prefix key switching.")

(defvar magik-shell-drag-n-drop-mode-line-string nil
  "Mode-line string to use for Drag 'n' Drop mode.")

(defvar magik-shell-filter-state nil
  "State variable for the filter function.")

(defvar magik-shell-mode-map (make-keymap)
  "Keymap for Magik shell command buffers")

(defvar magik-shell-menu nil
  "Keymap for the Magik shell buffer menu bar")

(easy-menu-define magik-shell-menu magik-shell-mode-map
  "Menu for Magik shell mode."
  `(,"Magik Shell"
    [,"Previous Command"                 magik-shell-recall-prev-cmd           t]
    [,"Next Command"                     magik-shell-recall-next-cmd           t]
    [,"Previous Matching Command"        magik-shell-recall-prev-matching-cmd  :active t :keys "f2 p"]
    [,"Next Matching Command"            magik-shell-recall-next-matching-cmd  :active t :keys "f2 n"]
    "----"
    [,"Fold"                             magik-shell-display-history           :active t :keys "f2 up"]
    [,"Unfold"                           magik-shell-undisplay-history         :active t :keys "f2 down"]
    "----"
    [,"Electric Template"                magik-electric-explicit-space         :active t :keys "f2 SPC"]
    [,"Symbol Complete"                  magik-symbol-complete                 :active t :keys "f4 f4"]
    [,"Deep Print"                       magik-deep-print                      :active t :keys "f2 x"]
    "----"
    [,"Previous Traceback"               magik-shell-traceback-up              :active t :keys "f4 up"]
    [,"Next Traceback"                   magik-shell-traceback-down            :active t :keys "f4 down"]
    [,"Print Traceback"                  magik-shell-traceback-print           :active t :keys "f4 P, f2 ="]
    [,"Save Traceback"                   magik-shell-traceback-save            :active t :keys "f4 S"]
    "----"
    [,"External Shell Process"           magik-shell-shell                     :active t :keys "f4 $"]
    [,"Kill Magik Process"               magik-shell-kill-process              :active (and magik-shell-process
											    (eq (process-status magik-shell-process) 'run))]
    (,"Magik Command History")
    "---"
    (,"Toggle..."
     [,"Magik Shell Filter"             magik-shell-toggle-filter              :active t :keys "f2 f"
      :style toggle :selected (let ((b (get-buffer-process
					(current-buffer))))
				(and b (process-filter b)))]
     [,"Drag and Drop"                  magik-shell-drag-n-drop-mode       :active t
      :style toggle :selected magik-shell-drag-n-drop-mode])
    [,"Customize"                       magik-shell-customize               t]))

(defvar magik-shell-mode-error-map (make-sparse-keymap)
  "Keymap for Jumping to error messages.")

(define-key magik-shell-mode-error-map [mouse-2]  'magik-shell-error-goto-mouse)
(define-key magik-shell-mode-error-map [C-return] 'magik-shell-error-goto)

(defvar magik-shell-process nil
  "The process object of the command running in the buffer.")

(defvar magik-shell-current-command nil
  "The current `magik-shell-command' in the current buffer.")

(defvar magik-shell-exec-path nil
  "Stored value of `exec-path' when the GIS process was started.")
(make-variable-buffer-local 'magik-shell-exec-path)

(defvar magik-shell-process-environment nil
  "Stored value of `process-environment' when the GIS process was started.")
(make-variable-buffer-local 'magik-shell-process-environment)

(defvar magik-shell-cb-buffer nil
  "The Class browser buffer associated with the GIS process.")

(defvar magik-shell-no-of-cmds nil
  "No. of commands we have sent to this buffer's gis including the
null one at the end, but excluding commands that have been spotted as
being degenerate.")

(defvar magik-shell-cmd-num nil
  "A number telling us what command is being recalled.  Important for
M-p and M-n commands.  The first command typed is number 0.  The
current command being typed is number (1- magik-shell-no-of-cmds).")

(defvar magik-shell-prev-cmds nil
  "A vector of pairs of markers, oldest commands first.  Every time
the vector fills up, we copy to a new vector and clean out naff
markers.")

(defvar magik-shell-history-length 20
  "The default number of commands to fold.")

(defvar magik-shell-command-syntax-table nil
  "Syntax table in use for parsing quotes in magik-shell-command.")

;; Create the syntax table
(if magik-shell-command-syntax-table
    ()
  (setq magik-shell-command-syntax-table (make-syntax-table))
  ;; Allow embedded environment variables in Windows %% and Unix $ or ${} formats
  (modify-syntax-entry ?$  "w"  magik-shell-command-syntax-table)
  (modify-syntax-entry ?\{ "w"  magik-shell-command-syntax-table)
  (modify-syntax-entry ?\} "w"  magik-shell-command-syntax-table)
  (modify-syntax-entry ?%  "w"  magik-shell-command-syntax-table)

  (modify-syntax-entry ?_  "w"  magik-shell-command-syntax-table) ;make _ a word character for environment variable sustitution

  (modify-syntax-entry ?\' "\"" magik-shell-command-syntax-table) ;count single quotes as a true quote
  (modify-syntax-entry ?\" "\"" magik-shell-command-syntax-table) ;count double quotes as a true quote
  (modify-syntax-entry ?\\ "\\" magik-shell-command-syntax-table) ;allow a \ as an escape character
  (modify-syntax-entry ?.  "w"  magik-shell-command-syntax-table) ;(for filenames)

  ;; Special characters for Windows filenames
  (modify-syntax-entry ?:  "w"  magik-shell-command-syntax-table)
  (modify-syntax-entry ?~  "w"  magik-shell-command-syntax-table) ;(mainly for NT 8.3 filenames)
  )

(defconst magik-shell-command-default "[%HOME%] %SMALLWORLD_GIS%/bin/x86/runalias.exe swaf_mega"
  "The default value for magik-shell-command.
It illustrates how Environment variables can be embedded in the command.
Also it neatly shows the three ways of referencing Environment variables,
via the Windows and Unix forms: %%, $ and ${}. All of which are
expanded irrespective of the current Operating System.")

;;Although still settable by the user via M-x set-variable,
;;it is preferred that magik-shell-comand-history be used instead.
(defvar magik-shell-command magik-shell-command-default
  "*The command used to invoke the gis.  It is offered as the default
string for next time.")

(defcustom magik-shell-command-history nil
  "*List of commands run by a GIS buffer."
  :group 'magik
  :type  '(choice (const nil)
		  (repeat string)))
(put 'magik-shell-command-history 'permanent-local t)

(defcustom magik-shell-kill-process-function 'sw-delete-process-safely
  "*The function used to terminate a Magik PROCESS in the GIS buffer.

`kill-process'   terminates the process but the process may tidy itself up
		 before exiting and so Emacs will not display the terminated
		 process message in the buffer until that is complete.

`delete-process' terminates the process and Emacs immediately displays the
		 process terminated message.

`quit-process'   Sends SIGQUIT signal if the OS implements it.
		 Not implemented on Windows OSes."
  :group 'magik
  :type  'function)

(defun magik-shell-help ()
  "Display help on how to use the Gis Mode interface."
  (interactive)
  (sw-help-open sw-help-gis-id))

(defun magik-shell-customize ()
  "Open Customization buffer for Gis Mode."
  (interactive)
  (customize-group 'gis))

(defun magik-shell-prompt-update-font-lock ()
  "Update the Font-lock variable `magik-shell-font-lock-keywords' with current `magik-shell-prompt' setting."
  (let ((entry (list (concat "^" magik-shell-prompt) 0 magik-shell-font-lock-prompt-face t)))
    (if (member entry magik-shell-font-lock-keywords)
	nil ;; Already entered
      (setq magik-shell-font-lock-keywords (append magik-shell-font-lock-keywords (list entry)))
      (if (fboundp 'font-lock-set-defaults)
	  (progn  ;; Emacs 20 and later font-lock mode post-process its variables
	    (set 'font-lock-set-defaults nil)
	    (funcall 'font-lock-set-defaults))))))

(defun magik-shell-prompt-get (&optional force-query-p)
  "If `magik-shell-prompt' is nil, get the GIS session's command line prompt.
If interactive and a prefix arg is used then GIS session will be
queried irrespective of default value of `magik-shell-prompt'"
  (interactive "P")
  (if (and (null force-query-p)
	   (stringp (default-value 'magik-shell-prompt))) ;user has overridden setting
      (progn
	(setq magik-shell-prompt (or magik-shell-prompt ;user may have set a local value for it
				     (default-value 'magik-shell-prompt)))
	(magik-shell-prompt-update-font-lock))
    (process-send-string
     magik-shell-process
     "_block
	!terminal!.put(%x.from_value(1))
	!terminal!.put(%P)
	_if magik_rep.responds_to?(:prompt_generator)
	_then !terminal!.write(magik_rep.prompt_generator.invoke(\"MagikSF> \"))
	_else !terminal!.write(\"Magik\\(SF\\|2\\)> \")
	_endif
	!terminal!.put(%x.from_value(5))
	!terminal!.put(%space)
    _endblock\n$\n")))
(add-hook 'magik-shell-start-process-post-hook 'magik-shell-prompt-get)

(defun magik-shell-shell ()
  "Start a command shell with the same environment as the current GIS process."
  (interactive)
  (require 'shell)
  (let ((process-environment (copy-list magik-shell-process-environment))
	(exec-path (copy-list magik-shell-exec-path))
	(buffer (concat "*shell*" (buffer-name)))
	(version (and (boundp 'magik-shell-version-current) (symbol-value 'magik-shell-version-current))))
    (make-comint-in-buffer "magik-shell-shell"
			   buffer
			   "cmd" nil "/k"
			   (concat (getenv "SMALLWORLD_GIS") "\\config\\environment.bat"))
    (save-excursion
      (set-buffer buffer)
      (if (stringp version) (set 'magik-shell-version-current version)))
    (switch-to-buffer-other-window buffer)))

(defun magik-shell-parse-gis-command (command)
  "Parse the magik-shell-command string taking care of any quoting
and return a list of all the components of the command."

  ;;Copy the string into a temp buffer.
  ;;Use the Emacs sexp code and an appropriate syntax-table 'magik-shell-command-syntax-table'
  ;;to cope with quotes and possible escaped quotes.
  ;;forward-sexp therefore guarantees preservation of white within quoted regions.
  ;;However, I do some extra work to try and remove the surrounding quotes from the returned result
  (let ((temp-buf (get-buffer-create " *magik-shell-command parser*"))
	(command-list))
    (save-excursion
      (save-match-data
	(set-buffer temp-buf)
	(erase-buffer)
	(set-syntax-table magik-shell-command-syntax-table)
	(insert command)

					;Remove excess trailing whitespace to avoid spurious extra empty arguments being passed
	(goto-char (point-max))
	(delete-horizontal-space)

	(goto-char (point-min))
	(condition-case var
	    (setq command-list
		  (loop
		   with start-char ;point containing valid word character - not whitespace or a quote
		   with substr ;substring containing command-line argument
		   do (progn
			(setq start-char
			      (save-excursion
				(skip-chars-forward " \t") ;skip intervening white space
				(and (looking-at "[\"\']") (forward-char 1)) ;strip begin-quote
				(point)))

			(forward-sexp)
			(setq substr (buffer-substring start-char (point)))
			(if (string-match "[\"\']$" substr) ;strip end-quote if any
			    (setq substr (substring substr 0 (match-beginning 0))))
					;Now look for embeded environment variables
			(setq substr (substitute-in-file-name substr)))
		   collect substr
		   until (eobp)))
	  (scan-error
	   (error "%s or quotes" (cadr var)))))
      (kill-buffer temp-buf)
      command-list)))

(defun magik-shell-buffer-alist-remove ()
  "Remove current buffer from `magik-shell-buffer-alist'."
  (let ((c (rassoc (buffer-name) magik-shell-buffer-alist)))
    (if c
	(progn
	  (setcdr c nil)
	  (car c)))))

(defun magik-shell-buffer-alist-prefix-function (arg mode predicate)
  "Function to process prefix keys when used with \\[gis]."
  (let ((buf (cdr (assq arg magik-shell-buffer-alist))))
    (if (and buf
	     (save-excursion
	       (set-buffer buf)
	       (magik-utils-buffer-mode-list-predicate-p predicate)))
	t
      (error "No GIS buffer"))
    buf))

(defun magik-shell-command-display (command)
  "Return shortened Gis command suitable for display."
  (if (stringp command) ; defensive programming. Should be a string but need to avoid errors
      (let              ; because this function is called in a menu-update-hook
	  ((command-len (- (min (length command) magik-shell-command-history-max-length)))
	   (label ""))
	(save-match-data
	  (if (string-match "^\\[[^\]]*\\]" command)
	      (setq label
		    (concat (magik-utils-file-name-display (match-string 0 command)
							   magik-shell-command-history-max-length-dir)
			    "..."))))
	(concat label (substring command (+ command-len (length label)))))))

(defun magik-shell-update-sw-menu ()
  "Update GIS process submenu in SW menu bar."
  (let* ((magik-shell-alist (sort (copy-alist magik-shell-buffer-alist)
				  #'(lambda (a b) (< (car a) (car b)))))
	 magik-shell-list)
    (dolist (c magik-shell-alist)
      (let ((i   (car c))
	    (buf (cdr c)))
	(if buf
	    (setq magik-shell-list
		  (append magik-shell-list
			  (list (vector buf
					(list 'switch-to-buffer buf)
					':active t
					':keys (format "M-%d f2 z" i))))))))
    ;;GIS buffers ordered according to when they were started.
    ;;killed session numbers are reused.
    (easy-menu-change (list "Tools" "Magik")
		      "Magik Shell Processes"
		      (or magik-shell-list (list "No Processes")))))

(defun magik-shell-update-gis-menu ()
  "Update the GIS menu bar."
  (if (eq major-mode 'magik-shell-mode)
      (let (command-list)
	(save-match-data
	  ;;Delete duplicates from magik-shell-command-history local and global values
	  ;;Note: delete-duplicates does not appear to work on localised variables.
	  (setq magik-shell-command-history
		(remove-duplicates magik-shell-command-history :test 'equal))
	  (setq-default magik-shell-command-history
			(remove-duplicates (default-value 'magik-shell-command-history)
					   :test 'equal))

	  (dolist (command magik-shell-command-history)
	    (push (apply
		   'vector
		   (magik-shell-command-display command)
		   (list 'gis (buffer-name) (purecopy command))
		   ':active
		   '(not (get-buffer-process (buffer-name)))
		   ;; ':key-sequence nil
		   (list ':help (purecopy command)))
		  command-list)))

	(if (get-buffer-process (buffer-name))
	    (setq command-list
		  (append command-list
			  (list "---"
				(apply 'vector (magik-shell-command-display magik-shell-current-command)
				       'ignore ':active nil (list ':key-sequence nil
								  ':help (purecopy magik-shell-current-command)))
				(apply 'vector "Start New Magik Shell" 'magik-shell-new-buffer
				       ':active t
				       ':keys '("C-u f2 z"))))))

	(easy-menu-change (list "Magik Shell")
			  "Magik Shell Command History"
			  (or command-list (list "No History"))))))

(defun magik-shell-update-sw-shell-menu ()
  "Update GIS shell submenu in SW menu bar."
  (let ((shell-bufs (magik-utils-buffer-mode-list 'shell-mode
						  (function (lambda () (getenv "SMALLWORLD_GIS")))))
	shell-list)
    (loop for buf in shell-bufs
	  do (push (vector buf (list 'switch-to-buffer buf) t) shell-list))
    (easy-menu-change (list "Tools" "Magik")
		      "External Shell Processes"
		      (or shell-list (list "No Processes")))))


(defun magik-shell-mode ()
  "Major mode for run a gis as a direct sub-process.

The default name for a buffer running a gis is \"*gis*\".  The name of
the current gis buffer is kept in the user-option, `magik-shell-buffer'.

There are many ways of recalling previous commands (see the on-line
help on F1).

Commands are sent to the gis with the F8 key or the return key.

Entry to this mode calls the value of magik-shell-mode-hook."

  (interactive)
  (let
      ((tmp-no-of-gis-cmds            magik-shell-no-of-cmds)
       (tmp-gis-cmd-num               magik-shell-cmd-num)
       (tmp-prev-gis-cmds             magik-shell-prev-cmds))
    (kill-all-local-variables)
    (make-local-variable 'selective-display)
    (make-local-variable 'comint-last-input-start)
    (make-local-variable 'comint-last-input-end)
    (make-local-variable 'font-lock-defaults)

    (make-local-variable 'magik-shell-command-history)
    (make-local-variable 'magik-shell-current-command)
    (make-local-variable 'magik-shell-no-of-cmds)
    (make-local-variable 'magik-shell-cmd-num)
    (make-local-variable 'magik-shell-prev-cmds)
    (make-local-variable 'magik-shell-filter-state)
    (make-local-variable 'magik-shell-process)
    (make-local-variable 'magik-shell-prompt)
    (make-local-variable 'magik-shell-exec-path)
    (make-local-variable 'magik-shell-process-environment)
    (make-local-variable 'magik-shell-cb-buffer)
    (make-local-variable 'magik-shell-drag-n-drop-mode-line-string)
    (make-local-variable 'magik-transmit-debug-mode-line-string)
    (make-local-variable 'ac-sources)

					;(make-local-hook 'kill-buffer-hook) ;add-hook uses local option

    (use-local-map magik-shell-mode-map)
    (easy-menu-add magik-shell-menu)
    (set-syntax-table magik-mode-syntax-table)

    (if (null tmp-no-of-gis-cmds)
	(progn
	  (setq magik-shell-no-of-cmds 1)
	  (setq magik-shell-cmd-num 0)
	  (setq magik-shell-prev-cmds (make-vector 100 nil))
	  ;; the null marker-pair is a pair of references to the same marker.
	  ;; This is so that they will always move together and therefore be null.
	  (aset magik-shell-prev-cmds 0 (let ((m (point-min-marker))) (cons m m))))
      (setq magik-shell-no-of-cmds            tmp-no-of-gis-cmds)
      (setq magik-shell-cmd-num               tmp-gis-cmd-num)
      (setq magik-shell-prev-cmds             tmp-prev-gis-cmds))

    (setq major-mode 'magik-shell-mode
	  mode-name "Magik Shell"
	  selective-display t
	  local-abbrev-table magik-mode-abbrev-table
	  comint-last-input-start (make-marker)
	  comint-last-input-end   (make-marker)
	  magik-shell-command-history (or magik-shell-command-history (default-value 'magik-shell-command-history))
	  magik-shell-filter-state "\C-a"
	  magik-shell-cb-buffer    (concat "*cb*" (buffer-name))
	  magik-shell-drag-n-drop-mode-line-string " DnD"
	  magik-transmit-debug-mode-line-string " #DEBUG"
	  font-lock-defaults '(magik-shell-font-lock-keywords
			       nil t
			       ((?_ . "w")))
	  ac-sources (append '(
			       magik-ac-class-method-source
			       magik-ac-dynamic-source
			       magik-ac-global-source
			       magik-ac-object-source
			       magik-ac-raise-condition-source
			       )
			     ac-sources))

    ;; Update magik-shell-buffer to current buffer name if magik-shell-buffer's buffer
    ;; does not exist.  this effectively stores the first most likely
    ;; default value in magik-shell-buffer even if an aliases file GIS session
    ;; was started first.
    (if (and magik-shell-buffer (get-buffer magik-shell-buffer))
	nil
      (setq-default magik-shell-buffer (buffer-name)))

    (if (rassoc (buffer-name) magik-shell-buffer-alist)
	nil
      ;; Update magik-shell-buffer-alist
      (let ((n 1))
	(while (cdr (assq n magik-shell-buffer-alist))
	  (setq n (1+ n)))
	(if (assq n magik-shell-buffer-alist)
	    (setcdr (assq n magik-shell-buffer-alist) (buffer-name))
	  (add-to-list 'magik-shell-buffer-alist (cons n (buffer-name))))))

    ;; *gis* buffer always inherits the current global environment
    (if (equal (buffer-name) "*gis*")
	(setq magik-shell-exec-path (copy-list exec-path)
	      magik-shell-process-environment (copy-list process-environment))
      (setq magik-shell-exec-path (copy-list (or magik-shell-exec-path exec-path))
	    magik-shell-process-environment (copy-list (or magik-shell-process-environment
							   process-environment))))

    (setq mode-line-process '(": %s"))

    (abbrev-mode 1)
    (save-excursion
      (set-buffer (get-buffer-create (concat " *filter*" (buffer-name))))
      (erase-buffer))

    (add-hook 'menu-bar-update-hook 'magik-shell-update-gis-menu)
    (add-hook 'menu-bar-update-hook 'magik-shell-update-sw-menu)
    (add-hook 'menu-bar-update-hook 'magik-shell-update-sw-shell-menu)
    (add-hook 'kill-buffer-hook 'magik-shell-buffer-alist-remove nil t) ;local hook
    (run-hooks 'magik-shell-mode-hook)))

(defun magik-shell-sentinel (proc msg)
  "Sentinel function, runs when the magik process exits."
  (let ((magik-shell-exit-status (process-exit-status proc))
	(buf (process-buffer proc)))
    (save-excursion
      (set-buffer buf)
      ;; ensure process end message is at end of buffer.
      (goto-char (point-max))
      (cond ((eq (process-status proc) 'exit)
	     (insert "\n\n" (format "Process %s %s"
				    (process-name proc)
				    msg)
		     "\n")
	     (message "Magik process %s exited: %s" buf msg))
	    ((eq (process-status proc) 'signal)
	     (insert "\n\n" (format "Process %s %s"
				    (process-name proc)
				    msg)
		     "\n")
	     (message "Magik process %s signalled: %s" buf msg)))

      (message "Magik process %s process %s has terminated with exit code: %s"
	       buf (process-name proc) (number-to-string magik-shell-exit-status))

      ;;Allow messages to appear in *Messages* buffer
      (sit-for 0.01)
      (run-hook-with-args 'magik-shell-sentinel-hooks magik-shell-exit-status))))

(defun magik-shell-start-process (args)
  "Run a Gis process in the current buffer.
Adds `magik-shell-current-command' to `magik-shell-command-history' if not already there."
  (let ((exec-path (copy-list magik-shell-exec-path))
	(process-environment (copy-list magik-shell-process-environment)))
    (run-hooks 'magik-shell-start-process-pre-hook)
    (or (member magik-shell-current-command magik-shell-command-history)
	(add-to-list 'magik-shell-command-history magik-shell-current-command))
    (setq magik-shell-process
	  (apply 'start-process "magik-shell-process" (current-buffer) (car args) (cdr args)))
    (set-process-sentinel magik-shell-process 'magik-shell-sentinel)
    (set-marker (process-mark magik-shell-process) (point-max))
    (set-process-filter magik-shell-process 'magik-shell-filter)

    ;;MF New bit for connecting to the method finder:
    ;;MF We nuke the current cb first and reconnect later.
    (when (and magik-cb-dynamic (get-buffer magik-shell-cb-buffer))
      (let ((magik-cb-process (get-buffer-process magik-shell-cb-buffer)))
	(if magik-cb-process (delete-process magik-cb-process)))
      (process-send-string magik-shell-process "_if method_finder _isnt _unset\n_then\n  method_finder.lazy_start?\n  method_finder.send_socket_to_emacs()\n_endif\n$\n"))
    (sit-for 0.01)
    (run-hooks 'magik-shell-start-process-post-hook)))

;; Put up here coz of load order problems.
;; The logic of the `F2 s' is still not quite right anyway.

;;;###autoload
(defun magik-shell (&optional buffer command)
  "Run a Gis process in a buffer in `magik-shell-mode'.

The command is typically \"sw_magik_win32\" or \"sw_magik_motif\", but
can be any interactive program such as \"csh\".

The program that is offered as a default is stored in the variable,
`magik-shell-command', which you can customise.  e.g.

\(setq magik-shell-command
  \"[$HOME] sw_magik_win32 -Mextdir %TEMP% -image $SMALLWORLD_GIS/images/gis.msf\"
\)
The command automatically expands environment variables using
Windows %% and Unix $ and ${} nomenclature.

You can setup a list of standard commands by setting the
default value of `magik-shell-command-history'.

Prefix argument controls:
With a numeric prefix arg, switch to the Gis process of that number
where the number indicates the order it was started. The
SW->Gis Processes indicates which numbers are in use. If a Gis process
buffer is killed, its number is reused when a new Gis process is started.

With a non-numeric prefix arg, ask user for buffer name to use for
GIS.  This will default to a unique currently unused name based upon
the current value of `magik-shell-buffer-default-name'.

If there is already a Gis process running in a visible window or
frame, just switch to that buffer, or prompt if more than one.  If
there is not, prompt for a command to run, and then run it."

  (interactive)
  (if command (setq magik-shell-command command))
  (let (dir
	cmd
	args
	;;read-string's history arg does not work with buffer-local variables
	;;history also always has something see Package Registration at end
	(command-history magik-shell-command-history)
	alias-beg
	alias-expansion
	(alias-buffer "*temp gis alias buffer*")
	(keepgoing t)
	(magik-shell-start-process-pre-hook magik-shell-start-process-pre-hook)
	(buffer (magik-utils-get-buffer-mode (cond (buffer buffer)
						   ((eq major-mode 'magik-shell-mode) (buffer-name))
						   (t nil))
					     'magik-shell-mode
					     "Enter Magik process buffer:"
					     (or magik-shell-buffer magik-shell-buffer-default-name)
					     'magik-shell-buffer-alist-prefix-function
					     (generate-new-buffer-name magik-shell-buffer-default-name)))
	(rev-1920-regexp " +\\[rev\\(19\\|20\\)\\] +")
	(alias-subst-regexp "\\\\!\\(\\\\\\)?\\*"))
    (if (and (get-buffer-process buffer)
	     (eq (process-status (get-buffer-process buffer)) 'run))
	(progn
	  (pop-to-buffer buffer)
	  (goto-char (point-max)))

      ;; Else start a fresh gis:
      ;; We keep going round expanding aliases until there is no alias expansion.
      ;; Each time round the user can edit the expanded alias.
      ;; We also silently remove any strings of the form [rev20] or [rev19].

      (save-excursion
	(set-buffer (get-buffer-create alias-buffer))
	(erase-buffer)
	(if (and (equal (getenv "SHELL") "/bin/csh")
		 (file-readable-p "~/.alias"))
	    (insert-file-contents "~/.alias"))

	(while keepgoing
	  (setq keepgoing nil)
	  (setq magik-shell-command (sub magik-shell-command rev-1920-regexp " "))
	  (or (eq (string-match "\\[" magik-shell-command) 0)
	      (setq magik-shell-command (concat "[" default-directory "] " magik-shell-command)))
	  (or command
	      (setq magik-shell-command
		    (read-string "Magik command: "
				 (car command-history)
				 'command-history)))
	  (if (string-match rev-1920-regexp magik-shell-command)
	      (progn
		(setq keepgoing t)
		(setq magik-shell-command (sub magik-shell-command rev-1920-regexp " "))))
	  (or (eq (string-match "\\[" magik-shell-command) 0)
	      (setq magik-shell-command (concat "[" default-directory "] " magik-shell-command)))
	  (string-match "\\[\\([^\]]*\\)\\] *\\([^ ]*\\) *\\(.*\\)" magik-shell-command)
	  (setq dir  (substring magik-shell-command (match-beginning 1) (match-end 1)))
	  (setq cmd  (substring magik-shell-command (match-beginning 2) (match-end 2)))
	  (setq args (substring magik-shell-command (match-beginning 3) (match-end 3)))

	  (goto-char (point-min))
	  (if (re-search-forward (concat "^alias[ \t]+" (regexp-quote cmd) "[ \t]+") nil t)
	      (progn
		(setq keepgoing t)
		(setq alias-beg (match-end 0))
		(goto-char alias-beg)
		(if (looking-at "['\"]")
		    (progn
		      (incf alias-beg)
		      (end-of-line)
		      (re-search-backward "['\"]"))
		  (end-of-line))
		(setq alias-expansion (buffer-substring alias-beg (point)))
		(or (string-match alias-subst-regexp alias-expansion)
		    (setq alias-expansion (concat alias-expansion " \\!*")))
		(setq alias-expansion (sub alias-expansion alias-subst-regexp args))
		(setq magik-shell-command (concat "[" dir "] " alias-expansion)))))

	(kill-buffer alias-buffer))

      (pop-to-buffer (get-buffer-create buffer))
      (magik-shell-mode)
      (goto-char (point-max))
      (insert "\n" (current-time-string) "\n")
      (setq default-directory (expand-file-name
			       (file-name-as-directory
				(substitute-in-file-name dir)))
	    magik-shell-current-command (copy-sequence magik-shell-command)
	    magik-shell-command-history (cons magik-shell-current-command
					      (delete magik-shell-current-command magik-shell-command-history)))
      (setq-default magik-shell-command-history (cons magik-shell-current-command
						      (delete magik-shell-current-command magik-shell-command-history)))
      (or (file-directory-p default-directory)
	  (error "Directory does not exist: %s" default-directory))
      (add-hook 'magik-shell-start-process-pre-hook
		(function (lambda () (insert magik-shell-command ?\n ?\n)))
		t)
      (magik-shell-start-process (magik-shell-parse-gis-command (concat cmd " " args))))))

(defun magik-shell-new-buffer ()
  "Start a new GIS session."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'gis)))

(defun magik-shell-kill-process ()
  "Kill the current gis process.
Uses `magik-shell-kill-process-function' function to kill the process given in `magik-shell-process'."
  (interactive)
  (if (and magik-shell-process
	   (eq (process-status magik-shell-process) 'run)
	   (y-or-n-p "Kill the Magik process? "))
      (let ((status (process-status magik-shell-process)))
	(funcall magik-shell-kill-process-function magik-shell-process)
	(sit-for 0.1)
	(if (eq status (process-status magik-shell-process))
	    (insert "\nMagik is still busy and will exit at an appropriate point. Please be patient... \n")))))

(defun magik-shell-query-interrupt-shell-subjob ()
  "Ask and then comint-interrupt-subjob."
  (interactive)
  (if (y-or-n-p "Kill the Magik process? ")
      (comint-kill-subjob)))

(defun magik-shell-query-quit-shell-subjob ()
  "Ask and then comint-quit-subjob."
  (interactive)
  (if (y-or-n-p "Kill the Magik process? ")
      (comint-quit-subjob)))

(defun magik-shell-query-stop-shell-subjob ()
  "Ask and then comint-stop-subjob."
  (interactive)
  (if (y-or-n-p "Suspend the Magik process? ")
      (comint-stop-subjob)))

(defun magik-shell-query-shell-send-eof ()
  "Ask and then comint-send-eof."
  (interactive)
  (if (y-or-n-p "Send EOF to the Magik process? ")
      (comint-send-eof)))

;; R E C A L L I N G   C O M M A N D S
;; ___________________________________
;;
;;; Each gis command is recorded by vec-gis-mode.el so that the
;;; the user can recall and edit previous commands.  This file
;;; also adds dollars and implements the history-folding feature.

(defun magik-shell-copy-cmd (n offset)
  "Copy command number N to the bottom of the buffer (replacing
any current command) and locate the cursor to an offset OFFSET."
  (delete-region (process-mark (get-buffer-process (current-buffer))) (point-max))
  (goto-char (point-max))
  (let*
      ((pair (aref magik-shell-prev-cmds n))
       (str (subst-char-in-string  ?\r ?\n (buffer-substring (car pair) (cdr pair))))
       (len (length str)))
    (insert str)
    (forward-char (- (max 0 (min len offset)) len))
    (if (pos-visible-in-window-p)
	(while
	    (not (pos-visible-in-window-p (point-max)))
	  (scroll-up 1)))))

(defun magik-shell-send-region (beg end)
  "Record in `magik-shell-prev-cmds' the region BEG to END and send to the gis.
Also update `magik-shell-cmd-num'.  Also append the string to \" *history**gis*\"."
  (save-excursion
    (let ((str (buffer-substring beg end)))
      (set-buffer (get-buffer-create (concat " *history*" (buffer-name))))
      (magik-mode)
      (let ((orig-point (point)))
	(goto-char (point-max))
	(insert str "\n")
	(goto-char orig-point))))
  (let ((n magik-shell-no-of-cmds))
    (if (= n (length magik-shell-prev-cmds))
	(magik-shell--make-new-cmds-vec))
    (setq n magik-shell-no-of-cmds)   ;; aaargh! I had forgotten this line and had a horrible intermittent bug.
    ;; NB: we are keeping a null marker at the end and this must be moved along.
    (aset magik-shell-prev-cmds n (aref magik-shell-prev-cmds (1- n)))
    (aset magik-shell-prev-cmds (1- n) (cons (copy-marker beg) (copy-marker end)))
    (setq magik-shell-cmd-num magik-shell-no-of-cmds)
    (incf magik-shell-no-of-cmds)

    (set-marker comint-last-input-start beg)
    (set-marker comint-last-input-end   end)
    (set-marker (process-mark (get-buffer-process (current-buffer))) end)
    (goto-char (point-max))
    (save-excursion
      (while
	  (> (- end beg) 240)
	(goto-char (+ beg 240))
	(if (search-backward "\n" beg t)
	    (process-send-region (get-buffer-process (current-buffer)) beg (1+ (point)))
	  (error "Sending long lines will probably crash the gis buffer. Use load_file instead."))
	(setq beg (1+ (point)))))
    (process-send-region (get-buffer-process (current-buffer)) beg end)))

(defun magik-shell--make-new-cmds-vec ()
  "Create a new bigger vector for `magik-shell-prev-cmds' and copy the
non-degenerate commands into it."
  (message "Resizing the command history vector...")
  (let*
      ((len (length magik-shell-prev-cmds))
       (v (make-vector (+ len 100) nil))
       (i 0)
       (v_i 0))
    (while
	(< i len)
      (let
	  ((x (aref magik-shell-prev-cmds i)))
	(if (and (marker-buffer (car x))
		 (marker-buffer (cdr x))
		 (> (cdr x) (car x)))
	    (progn
	      (aset v v_i x)
	      (incf v_i))))
      (incf i))
    (let
	((m (copy-marker (point-min))))
      (aset v v_i (cons m m)))
    (setq magik-shell-no-of-cmds (1+ v_i))
    (setq magik-shell-prev-cmds v)
    (message "Re-sizing the command history vector... Done. (%s commands)." (number-to-string v_i))))

(defun magik-shell-beginning-of-line (&optional n)
  "Move point to beginning of Nth line or just after prompt.
If command is repeated then place point at beginning of prompt."
  (interactive "p")
  (beginning-of-line n)
  ;;Only move to end of prompt if last-command was this one
  ;;AND a prefix key has not be used (n=1).
  (and (not (and (eq last-command 'magik-shell-beginning-of-line) (eq n 1)))
       (looking-at magik-shell-prompt)
       (goto-char (match-end 0))))

					; paulw - mods to make pre/post SW5 work in a single emacs
					; see also swkeys.el for key definition

(defun magik-shell-toggle-dollar ()
  "Toggle auto-insertion of $ terminator"
  (interactive )
  (setq magik-shell-auto-insert-dollar (not magik-shell-auto-insert-dollar))
  (if magik-shell-auto-insert-dollar
      (message "Insert dollar now enabled")
    (message "Insert dollar now disabled")))

(defun magik-shell-newline (arg)
  "If in a prev. cmd., recall.
If within curr. cmd., insert a newline.
If at end of curr. cmd. and cmd. is complete, send to gis.
If at end of curr. cmd. and cmd. is not complete, insert a newline.
Else (not in any cmd.) recall line."
  (interactive "*P")
  (let
      ((n (magik-shell--get-curr-cmd-num))
       (p (process-mark (get-buffer-process (current-buffer)))))
    (cond
     (n  ; in a prev. cmd.
      (magik-shell-copy-cmd n
			    (- (point)
			       (car (aref magik-shell-prev-cmds n)))))

     ((>= (point) p)
      (if abbrev-mode (save-excursion (expand-abbrev)))
      (cond
       ((looking-at "[ \t\n]*\\'")  ; at end of curr. cmd.
	(newline arg)
	(cond
	 ((save-excursion
	    (and (progn
		   (skip-chars-backward " \t\n")
		   (eq (preceding-char) ?$))
		 (> (point) p)))
	  (skip-chars-backward " \t\n")
	  (forward-char)
	  (delete-region (point) (point-max))
	  (magik-shell-send-region (marker-position p) (point)))
	 ((magik-shell--complete-magik-p p (point))
					;          (insert "$\n") ;; paulw - remove additional <CR> which messes with pling variables
	  (if magik-shell-auto-insert-dollar (insert "$\n"))
	  (delete-region (point) (point-max))
	  (magik-shell-send-region (marker-position p) (point)))))
       ((looking-at "[ \t\n]*\\$[ \t\n]*\\'")
	(if (magik-shell--complete-magik-p p (point))
	    (progn
	      (search-forward "$")
	      (delete-region (point) (point-max))
	      (insert "\n")
	      (magik-shell-send-region (marker-position p) (point)))
	  (newline arg)))
       (t
	(newline arg))))

     (t  ; not in any cmd.
      (delete-region (process-mark (get-buffer-process (current-buffer))) (point-max))
      (let
	  ((str (buffer-substring (line-beginning-position) (line-end-position)))
	   (n (- (line-end-position) (point))))
	(goto-char (point-max))
	(insert str)
	(backward-char n))))))

(defun magik-shell--complete-magik-p (beg end)
  "Return t if the region from BEG to END is a syntactically complete piece of
Magik.  Also write a message saying why the magik is not complete."
  (save-excursion
    (goto-char beg)
    (let
	(stack  ; ...of pending brackets and keywords (strings).
	 last-tok)
      (while
	  (progn
	    (let
		((toks (magik-tokenise-region-no-eol-nor-point-min (point) (min (line-end-position) end))))
	      (if toks (setq last-tok (car (last toks))))
	      (dolist (tok toks)
		(cond
		 ((or (and (equal (car stack) "_for")    (equal (car tok) "_over"))
		      (and (equal (car stack) "_over")   (equal (car tok) "_loop"))
		      (and (equal (car stack) "_pragma") (equal (car tok) "_method"))
		      (and (equal (car stack) "_pragma") (equal (car tok) "_proc")))
		  (pop stack)
		  (push (car tok) stack))
		 ((member (car tok) '("_for" "_over" "_pragma"))
		  (push (car tok) stack))
		 ((assoc (car tok) magik-begins-and-ends)
		  (push (car tok) stack))
		 ((assoc (car tok) magik-ends-and-begins)
		  (cond
		   ((null stack)
		    (error "Found '%s' with no corresponding '%s'"
			   (car tok)
			   (cdr (assoc (car tok) magik-ends-and-begins))))
		   ((equal (cdr (assoc (car tok) magik-ends-and-begins)) (car stack))
		    (pop stack))
		   (t
		    (error "Found '%s' when expecting '%s'"
			   (car tok)
			   (cdr (assoc (car stack) (append magik-begins-and-ends
							   '(("_for" . "_over")
							     ("_over" . "_loop")
							     ("_pragma" . "_proc or _method"))))))))))))
	    (/= (line-end-position) (point-max)))
	(forward-line))
      (cond
       (stack
	(message "Not sent (waiting for '%s')."
		 (cdr (assoc (car stack) (append magik-begins-and-ends
						 '(("_for" . "_over")
						   ("_over" . "_loop")
						   ("_pragma" . "_proc or _method"))))))
	nil)
       ((assoc (car last-tok) magik-operator-precedences)
	(message "Not sent (there is a pending operator '%s')." (car last-tok))
	nil)
       (t
	t)))))

(defun magik-shell--get-curr-cmd-num ()
  "Return the num of the command that point is in, or nil if not in a command.
Being in the prompt before the command counts too.  We do this by binary search."
  (magik-shell--get-curr-cmd-num-2 0 (1- magik-shell-no-of-cmds)))

(defun magik-shell--get-curr-cmd-num-2 (min max)
  "Return the num of the command that point is in, or nil if it isn't in the half-open
range [MIN, MAX)."
  (if (> max min)
      (let*
	  ((mid (/ (+ min max) 2))
	   (pair (aref magik-shell-prev-cmds mid))
	   (p (point)))
	(cond
	 ((or (null (marker-buffer (car pair))))
					;(= (car pair) (cdr pair)))
	  (magik-shell--get-curr-cmd-num-2 (1+ mid) max))
	 ((save-excursion
	    (goto-char (car pair))
	    (beginning-of-line)
	    (and (>= p (point))
		 (< p (cdr pair))))
	  mid)
	 ((>= p (cdr pair))
	  (magik-shell--get-curr-cmd-num-2 (1+ mid) max))
	 ((< p (car pair))
	  (magik-shell--get-curr-cmd-num-2 min mid))
	 (t
	  (error "Sorry... Confused command recall"))))))

(defun magik-shell-electric-magik-space (arg)
  "copy blocks to the bottom of the gis buffer first and then do an electric space."
  (interactive "*p")
  (magik-shell--prepare-for-edit-cmd)
  (magik-electric-space arg))

(defun magik-shell-insert-char (arg)
  "Take a copy of a command before inserting the char."
  (interactive "*p")
  (magik-shell--prepare-for-edit-cmd)
  (self-insert-command arg))

(defun magik-shell-delete-char (arg)
  "Take a copy of a command before deleting the char."
  (interactive "*p")
  (magik-shell--prepare-for-edit-cmd)
  (delete-char arg))

(defun magik-shell-kill-word (arg)
  "Take a copy of a command before killing the word."
  (interactive "*p")
  (magik-shell--prepare-for-edit-cmd)
  (kill-word arg))

(defun magik-shell-backward-kill-word (arg)
  "Take a copy of a command before killing the word."
  (interactive "*p")
  (magik-shell--prepare-for-edit-cmd)
  (backward-kill-word arg))

(defun magik-shell-backward-delete-char (arg)
  "Take a copy of a command before deleting the char."
  (interactive "*p")
  (magik-shell--prepare-for-edit-cmd)
  (delete-backward-char arg))

(defun magik-shell-kill-line (arg)
  "Take a copy of a command before killing the line."
  (interactive "*P")
  (magik-shell--prepare-for-edit-cmd)
  (kill-line))

(defun magik-shell-kill-region (beg end)
  "Ask if they really want to kill the region, before killing it."
  (interactive "*r")
  (if (y-or-n-p "Cutting and pasting big regions can confuse the gis-mode markers. Kill anyway? ")
      (kill-region beg end)))

(defun magik-shell--prepare-for-edit-cmd ()
  "If we're in a previous command, replace any current command with
this one."
  (let
      ((n (magik-shell--get-curr-cmd-num)))
    (if n
	(magik-shell-copy-cmd n
			      (- (point)
				 (car (aref magik-shell-prev-cmds n)))))))

(defun magik-shell-send-command-at-point ()
  "Send the command at point, copying to the end of the buffer if necessary and
don't add extra dollars."
  (interactive "*")
  (or (get-buffer-process (current-buffer))
      (error "There is no process running in this buffer"))
  (let
      ((n (magik-shell--get-curr-cmd-num))
       (p (process-mark (get-buffer-process (current-buffer)))))
    (cond
     (n
      (magik-shell-copy-cmd n 0)
      (magik-shell-send-region (marker-position p) (point-max)))
     ((>= (point)
	  (save-excursion
	    (goto-char p)
	    (beginning-of-line)
	    (point)))
      (goto-char (point-max))
      (or (eq (preceding-char) ?\n)
	  (insert ?\n))
      (magik-shell-send-region (marker-position p) (point-max)))
     (t
      (error "Not a command")))))

(defun magik-shell--matching-cmd-p (n str)
  "Return t if magik-shell-prev-cmds[N] is a non-degenerate command matching STR or off the end (i.e.
n<0 or n>=magik-shell-no-of-cmds)."
  (or (< n 0)
      (>= n magik-shell-no-of-cmds)
      (let
	  ((pair (aref magik-shell-prev-cmds n))
	   (len (length str)))
	(and (marker-buffer (car pair))
	     (> (- (cdr pair) (car pair)) len)
	     (equal (buffer-substring (car pair) (+ (car pair) len)) str)))))

(defun magik-shell-recall (str step end-of-command-p)
  "Recall a command starting with STR in the direction STEP.
If END-OF-COMMAND-P is t then cursor is placed at and of the recalled command.
An internal function that deals with 4 cases."
  (let ((n magik-shell-cmd-num)
	mark )
    (while
	(progn
	  (incf n step)
	  (not (magik-shell--matching-cmd-p n str))))
    (if (= n -1)
	(if (equal str "")
	    (error "No previous command")
	  (error "No previous command matching '%s'" str)))
    (setq mark (process-mark (get-buffer-process (current-buffer))))
    (if (= n magik-shell-no-of-cmds)
	(decf n))
    (magik-shell-copy-cmd n
			  (if (equal str "")
			      (- (point) mark)
			    (length str)))
    (setq magik-shell-cmd-num n)
    (if end-of-command-p
	(progn
	  (goto-char (point-max))
	  ;; skip back past \n$\n and whitespace
	  (skip-chars-backward " \t\n$" mark)
	  ))))

(defun magik-shell-recall-prev-cmd ()
  "Recall the earlier gis commands
Cursor point is placed at end of command.
Compare with \\[recall-prev-matching-gis-cmd] placing cursor
immediately at the start of a command"
  (interactive "*")
  (magik-shell-recall "" -1 magik-shell-recall-cmd-move-to-end))

(defun magik-shell-recall-next-cmd ()
  "Recall the later gis commands
Cursor point is placed at end of command.
Compare with \\[recall-next-matching-gis-cmd] placing cursor
immediately at the start of a command"
  (interactive "*")
  (magik-shell-recall "" 1 magik-shell-recall-cmd-move-to-end))

(defun magik-shell-recall-prev-matching-cmd ()
  "Recall the earlier and earlier gis commands that match the part of
the command before the cursor."
  (interactive "*")
  (magik-shell-recall (buffer-substring
		       (process-mark (get-buffer-process (current-buffer)))
		       (point))
		      -1
		      nil))

(defun magik-shell-recall-next-matching-cmd ()
  "Recall the earlier and earlier gis commands that match the part of
the command before the cursor."
  (interactive "*")
  (magik-shell-recall (buffer-substring
		       (process-mark (get-buffer-process (current-buffer)))
		       (point))
		      1
		      nil))

(defun magik-shell-display-history (arg)
  "Fold (hide) away the parts of the gis buffer in between the last ARG commands.
If ARG is null, use a default of `magik-shell-history-length'."
  (interactive "*P")
  (setq arg (if (null arg) magik-shell-history-length (prefix-numeric-value arg)))
  (let
      ((b (current-buffer)))
    (or (eq major-mode 'magik-shell-mode)
	(set-buffer magik-shell-buffer))
    (setq selective-display t)
    (let
	((p (point))
	 (i (max 0 (min (- magik-shell-no-of-cmds arg 1) (1- magik-shell-no-of-cmds))))
	 j)
      ;; look for the start point of the folding; this may involve skipping
      ;; over bad markers.
      (while
	  (and (< i magik-shell-no-of-cmds)
	       (not (marker-buffer (car (aref magik-shell-prev-cmds i)))))
	(incf i))
      (if (= i (1- magik-shell-no-of-cmds))
	  (error "No commands to fold"))
      ;; we now have the index of the first command to fold.
      (message "Folding the last %s commands..." (number-to-string arg))
      (goto-char (car (aref magik-shell-prev-cmds i)))
      (while (search-forward "\n" nil t)
	(replace-match "\r"))
      (setq j i)
      (while
	  (< j (1- magik-shell-no-of-cmds))
	(goto-char (car (aref magik-shell-prev-cmds j)))
	(if (re-search-backward "[\r\n]" nil t)
	    (progn
	      (insert ?\n)
	      (delete-char 1)))
	(incf j))
      (goto-char (point-max))
      (if (search-backward "\r" nil t)
	  (progn
	    (insert ?\n)
	    (delete-char 1)))
      (message "Folding the last %s commands...Done" (number-to-string arg))
      (goto-char p)
      (set-buffer b))))

(defun magik-shell-undisplay-history (arg)
  "Unfold the last ARG commands.  If ARG is null, use a default of `magik-shell-history-length'."
  (interactive "*P")
  (setq arg (if (null arg) magik-shell-history-length (prefix-numeric-value arg)))
  (let
      ((b (current-buffer)))
    (or (eq major-mode 'magik-shell-mode)
	(set-buffer magik-shell-buffer))
    (setq selective-display t)
    (let
	((p (point))
	 (i (max 0 (min (- magik-shell-no-of-cmds arg 1) (1- magik-shell-no-of-cmds)))))
      ;; look for the start point of the folding; this may involve skipping
      ;; over bad markers.
      (while
	  (and (< i magik-shell-no-of-cmds)
	       (not (marker-buffer (car (aref magik-shell-prev-cmds i)))))
	(incf i))
      (if (= i (1- magik-shell-no-of-cmds))
	  (error "No commands to unfold"))
      ;; we now have the index of the first command to unfold.
      (message "Unfolding the last %s commands..." (number-to-string arg))
      (goto-char (car (aref magik-shell-prev-cmds i)))
      (while (search-forward "\r" nil t)
	(replace-match "\n"))
      (message "Unfolding the last %s commands...Done" (number-to-string arg))
      (goto-char p)
      (set-buffer b))))

(defun magik-shell-goto-process-mark ()
  "(goto-char (process-mark (get-buffer-process (current-buffer))))"
  (interactive)
  (goto-char (process-mark (get-buffer-process (current-buffer)))))

(defun magik-shell-set-process-mark-to-eob ()
  "(set-marker (process-mark (get-buffer-process (current-buffer))) (point-max))"
  (interactive)
  (set-marker (process-mark (get-buffer-process (current-buffer))) (point-max)))
;;;
;;;  T R A C E B A C K
;;;

;; support for `gis-traceback-print()'
(defun magik-shell-print-region-and-fold (start end switches)
  "like `print-region-1()' but with long lines folded first."
  (let ((name (concat (buffer-name) " Emacs buffer"))
	(width tab-width))
    (save-excursion
      (message "Printing...")
      (let ((oldbuf (current-buffer)))
	(set-buffer (get-buffer-create " *spool temp*"))
	(widen) (erase-buffer)
	(insert-buffer-substring oldbuf start end)
	(setq tab-width width)
	(untabify (point-min) (point-max))
	(goto-char (point-min))
	(while
	    (not (eobp))
	  (if (> (- (line-end-position) (point)) 72)
	      (progn
		(forward-char 72)
		(insert ?\n))
	    (forward-line)))
	(setq start (point-min) end (point-max)))
      (apply 'call-process-region
	     (nconc (list start end lpr-command
			  nil nil nil)
		    (nconc (and (eq system-type 'berkeley-unix)
				(list "-J" name "-T" name))
			   switches)))
      (message "Printing... Done"))))

(defun magik-shell-error-narrow-region ()
  "Narrow region between the current Magik prompts."
  (narrow-to-region (save-excursion (re-search-backward magik-shell-prompt))
		    (save-excursion
		      (or (re-search-forward magik-shell-prompt nil t) (goto-char (point-max)))
		      (beginning-of-line)
		      (point))))

(defun magik-shell-error-line-col (line)
  "Return (LINE . COLUMN) cons for location of error."
  (let ((col 0))
    (save-excursion
      (save-match-data
	(search-backward "--- line")
	(end-of-line)
	(forward-word -1)
	(setq line (+ line (string-to-number (current-word))))
	(if (re-search-forward "^\\s-*\\^" nil t)
	    (setq col (1- (length (match-string 0)))))))
    (cons line col)))

(defun magik-shell-error-goto ()
  "Goto file that contains the Magik error."
  (interactive)
  (let ((case-fold-search nil) ;case-sensitive searching required for "Loading"
	(line-adjust 0)
	(pos 0)
	file line-col buf)
    (save-match-data
      (save-restriction
	(magik-shell-error-narrow-region)
	(save-excursion
	  (beginning-of-line)
	  (if (looking-at (concat "^\\*\\*\\*\\*.*" "on line" " \\([0-9]+\\)$"))
	      (progn
		(setq line-col (magik-shell-error-line-col (string-to-number (match-string-no-properties 1)))
		      file (and (save-excursion (re-search-backward "Loading \\(.*\\)" nil t))
				(match-string-no-properties 1)))
		(if (file-exists-p file)
		    (setq buf (or (find-buffer-visiting file) (find-file-noselect file)))
		  (if (re-search-backward "^\\*\\*\\*\\* Emacs: buffer=\\(.*\\) file=\\(.*\\) position=\\([0-9]+\\)" nil t)
		      (setq buf  (match-string-no-properties 1)
			    file (match-string-no-properties 2)
			    pos  (string-to-number (match-string-no-properties 3))
			    line-adjust -4))))))))
    (or file
	(error "No Error on this line to go to"))
    (pop-to-buffer buf)
    (goto-char pos)

    ;;Subtract line-adjust lines because we add lines
    ;;to the transmitted buffer and Magik counts lines from 0.
    (forward-line (+ (car line-col) line-adjust))

    (move-to-column (cdr line-col))))

(defun magik-shell-error-goto-mouse (click)
  "Goto error at mouse point."
  (interactive "e")
  (mouse-set-point click)
  (magik-shell-error-goto))

(defun magik-shell-traceback-print ()
  "Send the text from the most recent \"**** Error\" to the end of
the buffer to the printer.  Query first."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (search-backward "\n**** Error" nil t)
	(if (y-or-n-p (concat (format "Print the last traceback (%s lines)?"
				      (number-to-string (count-lines (point) (point-max)))) " "))
	    (magik-shell-print-region-and-fold (point) (point-max) nil))
      (error "Couldn't find a line starting with '**** Error' - nothing printed" ))))

(defun magik-shell-traceback-save ()
  "Save in \"~/traceback.txt\" all the text onwards from the most recent \"**** Error\"."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (search-backward "\n**** Error" nil t)
	(progn
	  (write-region (point) (point-max) "~/traceback.txt")
	  (message "Saved the traceback in '~/traceback.txt'."))
      (error "Couldn't find a line starting with '**** Error' - nothing saved"))))

(defun magik-shell-traceback-up ()
  "Move up buffer to next traceback."
  (interactive)
  (save-match-data
    (re-search-backward "---- traceback: "))
  (forward-line -1))

(defun magik-shell-traceback-down ()
  "Move down buffer to next traceback."
  (interactive)
  (forward-line 1)
  (forward-line 1)
  (save-match-data
    (re-search-forward "---- traceback: "))
  (forward-line -1))

;;; Drag 'n' Drop
;;
;; When a file is dragged and dropped and the current buffer is
;; as GIS mode buffer, the file is loaded into the GIS session.

(defun magik-shell-drag-n-drop-mode (&optional arg)
  "Toggle Drag 'n' drop GIS loading functionality."
  (interactive "P")
  (setq magik-shell-drag-n-drop-mode
	(if (null arg)
	    (not magik-shell-drag-n-drop-mode)
	  (> (prefix-numeric-value arg) 0)))
  (add-hook 'find-file-hooks 'magik-shell-drag-n-drop-load)
  (if magik-shell-drag-n-drop-mode
      (message "Magik 'Drag and Drop' file mode is on")
    (message "Magik 'Drag and Drop' file mode is off"))
  (force-mode-line-update))

(defun magik-shell-drag-n-drop-load ()
  "Load a drag and dropped file into the Gis session.
If the previous buffer was a GIS session buffer and the previous event was
a drag & drop event then we load the dropped file into the GIS session.

The file must be in a Major mode that defines the function:
  MODE-gis-drag-n-drop-load
where MODE is the name of the major mode with the '-mode' postfix."
  (let (fn gis)
    ;;hopefully the tests are done in the cheapest, most efficient order
    ;;but gis-drag-n-drop-mode is checked last in case user has set
    ;;up a per-buffer Drag 'n' drop mode
    (if (and (listp last-input-event)
	     (eq (car last-input-event) 'drag-n-drop)
	     (setq fn (intern (concat (substring (symbol-name major-mode) 0 -5)
				      "-gis-drag-n-drop-load")))
	     (fboundp fn)
	     (windowp (caadr last-input-event))
	     (setq gis (window-buffer (caadr last-input-event)))
	     (save-excursion
	       (set-buffer gis)
	       (and magik-shell-drag-n-drop-mode
		    (eq major-mode 'magik-shell-mode))))
	(funcall fn gis (buffer-file-name)))))

;;;Package registration

;;Ensure Default magik-shell-command are placed at head of magik-shell-command-history
(mapcar (function
	 (lambda (c)
	   (and c
		(not (member c magik-shell-command-history))
		(push c magik-shell-command-history))))
	(list magik-shell-command-default magik-shell-command))


;;; package setup via setting of variable before load.
(and magik-shell-drag-n-drop-mode (magik-shell-drag-n-drop-mode magik-shell-drag-n-drop-mode))

(or (assq 'magik-shell-drag-n-drop-mode minor-mode-alist)
    (push '(magik-shell-drag-n-drop-mode magik-shell-drag-n-drop-mode-line-string) minor-mode-alist))
;;(or (assoc 'magik-shell-drag-n-drop-mode minor-mode-alist)
;;    (push (list 'magik-shell-drag-n-drop-mode " DnD") minor-mode-alist))

;;MSB configuration
(defun magik-shell-msb-configuration ()
  "Adds GIS buffers to MSB menu, supposes that MSB is already loaded."
  (let* ((l (length msb-menu-cond))
	 (last (nth (1- l) msb-menu-cond))
	 (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
	 (handle (1- (nth 1 last))))
    (setcdr precdr (list
		    (list
		     '(eq major-mode 'magik-shell-mode)
		     handle
		     "GIS (%d)")
		    last))))

(eval-after-load 'msb
  '(magik-shell-msb-configuration))

(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'magik-shell-mode))

(progn
  ;; ---------------------- magik shell mode -------------------------

  (loop for i from ?  to ?~ do
	(define-key magik-shell-mode-map (char-to-string i) 'magik-shell-insert-char))

  (define-key magik-shell-mode-map [f1]        'magik-shell-help)
  (define-key magik-shell-mode-map "\ep"       'magik-shell-recall-prev-cmd)
  (define-key magik-shell-mode-map "\en"       'magik-shell-recall-next-cmd)
  (define-key magik-shell-mode-map "\r"        'magik-shell-newline)
  (define-key magik-shell-mode-map " "         'magik-shell-electric-magik-space)
  (define-key magik-shell-mode-map "\C-?"      'magik-shell-backward-delete-char)
  (define-key magik-shell-mode-map "\C-a"      'magik-shell-beginning-of-line)
  (define-key magik-shell-mode-map "\C-d"      'magik-shell-delete-char)
  (define-key magik-shell-mode-map "\ed"       'magik-shell-kill-word)
  (define-key magik-shell-mode-map "\e\C-?"    'magik-shell-backward-kill-word)
  (define-key magik-shell-mode-map "\C-k"      'magik-shell-kill-line)
  (define-key magik-shell-mode-map "\C-w"      'magik-shell-kill-region)
  (define-key magik-shell-mode-map [f8]        'magik-shell-send-command-at-point)
  (define-key magik-shell-mode-map "\C-c\C-c"  'magik-shell-kill-process)
  (define-key magik-shell-mode-map "\C-c\C-\\" 'query-quit-shell-subjob)
  (define-key magik-shell-mode-map "\C-c\C-z"  'query-stop-shell-subjob)
  (define-key magik-shell-mode-map "\C-c\C-d"  'query-shell-send-eof)

  (define-key magik-shell-mode-map (kbd "<f2> <up>")   'display-gis-history)
  (define-key magik-shell-mode-map (kbd "<f2> \C-p")   'display-gis-history)
  (define-key magik-shell-mode-map (kbd "<f2> <down>") 'undisplay-gis-history)
  (define-key magik-shell-mode-map (kbd "<f2> \C-n")   'undisplay-gis-history)
  (define-key magik-shell-mode-map (kbd "<f2> =")      'magik-shell-traceback-print)
  (define-key magik-shell-mode-map (kbd "<f2> f")      'toggle-gis-filter)
  (define-key magik-shell-mode-map (kbd "<f2> p")      'magik-shell-recall-prev-matching-cmd)
  (define-key magik-shell-mode-map (kbd "<f2> n")      'magik-shell-recall-next-matching-cmd)

  (define-key magik-shell-mode-map (kbd "<f4> <f4>")   'magik-symbol-complete)
  (define-key magik-shell-mode-map (kbd "<f4> <up>")   'magik-shell-traceback-up)
  (define-key magik-shell-mode-map (kbd "<f4> <down>") 'magik-shell-traceback-down)
  (define-key magik-shell-mode-map (kbd "<f4> $")      'magik-shell-shell)
  (define-key magik-shell-mode-map (kbd "<f4> g")      'magik-shell-error-goto)
  (define-key magik-shell-mode-map (kbd "<f4> m")      'magik-copy-method-to-buffer)
  (define-key magik-shell-mode-map (kbd "<f4> r")      'magik-copy-region-to-buffer)
  (define-key magik-shell-mode-map (kbd "<f4> s")      'magik-add-debug-statement)
  (define-key magik-shell-mode-map (kbd "<f4> w")      'magik-set-work-buffer)
  (define-key magik-shell-mode-map (kbd "<f4> P")      'magik-shell-traceback-print)
  (define-key magik-shell-mode-map (kbd "<f4> S")      'magik-shell-traceback-save))

(provide 'magik-shell)
;;; magik-shell.el ends here
