;;; magik-session.el --- mode for running a Smallworld Magik interactive process

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
;;; The filter for the magik shell process is in magik-session-filter.el
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
;; magik-session-prev-cmds.
;;
;; Where possible, we try to allow more than one Magik process to be running.
;; This gets a bit tricky for things like transmit-method-to-magik
;; because they have to know where to send the magik to.  In order to
;; simplify this, we are getting rid of the variable,
;; magik-process-name, because it is a duplicate of magik-session-buffer.  We
;; also don't ever refer to the process by its name but always by its
;; buffer - this should save any confusion with Magik process naming.
;;
;; We don't rely on the form of the prompt any more.  We just rely on
;; it ending in a space.  The only place where we need to be sure is in
;; the filter.
;;
;; In this version of magik-session-mode, we don't have any automatic
;; indentation of magik-code.  The tab is just for inserting tabs and
;; nothing else.
;;
;; During a sequence of M-p and M-n commands, the actual command
;; we're looking at is recorded in the buffer-local variable,
;; magik-session-cmd-num.
;;
;; Unlike direct-gis-mode.el we keep the oldest command at the front.
;; This is fine because we can get to the end of a vector quickly.
;; We record how many commands are in our vector in the buffer-local
;; variable, magik-session-no-of-cmds.  To get rid of annoying edge
;; effects in going up and down the vector, we keep a pair of markers
;; that bound an empty bit of text at the end of the vector.
;;
;; Arbitrary decision: if a command is recalled by grabbing it with
;; the RET key, the magik-session-cmd-num is set to 0 (as if it had been
;; typed by hand) rather than the number of the command that was
;; recalled.

;;; Code:

(eval-when-compile
  (require 'comint)
  (defvar comint-last-input-start)
  (defvar comint-last-input-end)
  (defvar msb-menu-cond))

(require 'yasnippet)
(require 'magik-mode)
(require 'magik-indent)
(require 'magik-pragma)
(or (boundp 'ac-sources) (setq ac-sources nil))

(defcustom magik-session-buffer nil
  "*The default Smallworld session.
Used for switching to the first Smallworld session."
  :group 'magik
  :type '(choice string (const nil)))

(defcustom magik-session-buffer-default-name "*gis*"
  "*The default name of a Magik Session buffer when creating new Magik sessions."
  :group 'magik
  :type 'string)

(defcustom magik-session-prompt nil
  "String or Regular expression identifying the default Magik Prompt.
If global value is nil, a Magik session will attempt to discover the current
setting of the Magik Prompt by calling `magik-session-prompt-get'."
  :group 'magik
  :type '(choice regexp (const nil)))

;; paulw - preset rather than allow discovery (which doesn't seem to work)
(setq magik-session-prompt "Magik\\(\\|SF\\)> ")

(defcustom magik-session-command-history-max-length 90
  "*The maximum length of the displayed `magik-session-command' in the submenu.
This applies to the Magik Session -> Magik Session Command History submenu.
`magik-session-command' is a string of the form \"[DIRECTORY] COMMAND ARGS\"."
  :group 'magik
  :type  'integer)

(defcustom magik-session-command-history-max-length-dir (floor (/ magik-session-command-history-max-length 2))
  "*The maximum length of the displayed directory path in the submenu.
This applies to the Magik Session -> Magik Session Command History submenu."
  :group 'magik
  :type  'integer)

(defcustom magik-session-recall-cmd-move-to-end nil
  "*If t, move the cursor point to the end of the recalled command.
This behaviour is available for \\[magik-session-recall-prev-cmd] and \\[magik-session-recall-next-cmd] only.
The default is nil, which preserves the original behaviour to leave
the cursor point in the same position.

The similar commands, \\[magik-session-recall-prev-matching-cmd] and \\[magik-session-recall-next-matching-cmd]
that use command string matching are not affected by this setting."
  :group 'magik
  :type 'boolean)

(defcustom magik-session-font-lock-prompt-face 'font-lock-type-face
  "*Face name used to display the Magik Prompt."
  :group 'magik
  :type 'face)

(defcustom magik-session-font-lock-error-face 'font-lock-warning-face
  "*Face name used to display Error lines."
  :group 'magik
  :type 'face)

(defcustom magik-session-font-lock-traceback-face 'font-lock-warning-face
  "*Face name used to display Traceback lines."
  :group 'magik
  :type 'face)

(defcustom magik-session-font-lock-keywords
  (append
   magik-font-lock-keywords-1
   magik-font-lock-keywords-2
   (list
    '("^\\*\\*\\*\\* Error:.*$"      0 magik-session-font-lock-error-face t)
    '("^\\*\\*\\*\\* Warning:.*$"    0 font-lock-warning-face t)
    '("^---- traceback.* ----" . magik-session-font-lock-traceback-face)
    '("^@.*$"                . font-lock-reference-face)))
  "Additional expressions to highlight in Magik mode."
  :type 'sexp
  :group 'magik)

(defcustom magik-session-start-process-pre-hook nil
  "*Hook run before starting the process."
  :type 'hook
  :group 'magik)

(defcustom magik-session-start-process-post-hook nil
  "*Hook run after starting the process."
  :type 'hook
  :group 'magik)

(defcustom magik-session-auto-insert-dollar nil
  "If t, automatically insert a $ after each valid Magik statement."
  :group 'magik
  :type 'boolean)

(defcustom magik-session-sentinel-hooks nil
  "*Hooks to run after the Magik process has finished.
Each hook is passed the exit status of the Magik process."
  :type 'hook
  :group 'magik)

(defcustom magik-session-drag-n-drop-mode nil
  "Variable storing setting of \\[magik-session-drag-n-drop-mode]."
  ;;Use of integers is a standard way of forcing minor modes on and off.
  :type '(choice (const :tag "On" 1)
                 (const :tag "Off" -1))
  :group 'magik)

(defvar magik-session-buffer-alist nil
  "Alist storing Magik session buffer name and number.
Used for prefix key switching.")

(defvar magik-session-drag-n-drop-mode-line-string nil
  "Mode-line string to use for Drag and Drop mode.")

(defvar magik-session-filter-state nil
  "State variable for the filter function.")

(defvar magik-session-process nil
  "The process object of the command running in the buffer.")

(defvar magik-session-current-command nil
  "The current `magik-session-command' in the current buffer.")

(defvar-local magik-session-exec-path nil
  "Stored value of variable `exec-path'.
It holds the value from when the Magik session process was started.")

(defvar-local magik-session-process-environment nil
  "Stored value of variable `process-environment'.
It holds the value from when the Magik session process was started.")

(defvar magik-session-cb-buffer nil
  "The Class browser buffer associated with the Magik session process.")

(defvar magik-session-no-of-cmds nil
  "Number of commands we have sent to this buffer's gis.
Including the null one at the end, but excluding commands that have been spotted
as being degenerate.")
(put 'magik-session-no-of-cmds 'permanent-local t)

(defvar magik-session-cmd-num nil
  "A number telling us what command is being recalled.
Important for \\[magik-session-recall-prev-cmd] and \\[magik-session-recall-next-cmd].
The first command typed is number 0.
The current command being typed is number (1- magik-session-no-of-cmds).")
(put 'magik-session-cmd-num 'permanent-local t)

(defvar magik-session-prev-cmds nil
  "A vector of pairs of markers, oldest commands first.
Every time the vector fills up, we copy to a new vector and
clean out naff markers.")
(put 'magik-session-prev-cmds 'permanent-local t)

(defvar magik-session-history-length 20
  "The default number of commands to fold.")

(defvar magik-session-command-syntax-table nil
  "Syntax table in use for parsing quotes in magik-session-command.")

;; Create the syntax table
(if magik-session-command-syntax-table
    ()
  (setq magik-session-command-syntax-table (make-syntax-table))
  ;; Allow embedded environment variables in Windows %% and Unix $ or ${} formats
  (modify-syntax-entry ?$  "w"  magik-session-command-syntax-table)
  (modify-syntax-entry ?\{ "w"  magik-session-command-syntax-table)
  (modify-syntax-entry ?\} "w"  magik-session-command-syntax-table)
  (modify-syntax-entry ?%  "w"  magik-session-command-syntax-table)

  (modify-syntax-entry ?_  "w"  magik-session-command-syntax-table) ;make _ a word character for environment variable substitution

  (modify-syntax-entry ?\' "\"" magik-session-command-syntax-table) ;count single quotes as a true quote
  (modify-syntax-entry ?\" "\"" magik-session-command-syntax-table) ;count double quotes as a true quote
  (modify-syntax-entry ?\\ "\\" magik-session-command-syntax-table) ;allow a \ as an escape character
  (modify-syntax-entry ?.  "w"  magik-session-command-syntax-table) ;(for filenames)

  ;; Special characters for Windows filenames
  (modify-syntax-entry ?:  "w"  magik-session-command-syntax-table)
  (modify-syntax-entry ?~  "w"  magik-session-command-syntax-table) ;(mainly for NT 8.3 filenames)
  )

(defconst magik-session-command-default "[%HOME%] %SMALLWORLD_GIS%/bin/x86/runalias.exe swaf_mega"
  "The default value for magik-session-command.
It illustrates how Environment variables can be embedded in the command.
Also it neatly shows the three ways of referencing Environment variables,
via the Windows and Unix forms: %%, $ and ${}.  All of which are
expanded irrespective of the current Operating System.")

;;Although still settable by the user via M-x set-variable,
;;it is preferred that magik-session-comand-history be used instead.
(defvar magik-session-command magik-session-command-default
  "*The command used to invoke the gis.
It is offered as the default string for next time.")

(defcustom magik-session-command-history nil
  "*List of commands run by a Magik buffer."
  :group 'magik
  :type  '(choice (const nil)
                  (repeat string)))
(put 'magik-session-command-history 'permanent-local t)

(defun magik-session-customize ()
  "Open Customization buffer for Magik Session Mode."
  (interactive)
  (customize-group 'gis))

(defun magik-session-prompt-update-font-lock ()
  "Update the Font-lock variable `magik-session-font-lock-keywords'.
Uses current `magik-session-prompt' setting as value."
  (let ((entry (list (concat "^" magik-session-prompt) 0 magik-session-font-lock-prompt-face t)))
    (if (member entry magik-session-font-lock-keywords)
        nil ;; Already entered
      (setq magik-session-font-lock-keywords (append magik-session-font-lock-keywords (list entry)))
      (if (fboundp 'font-lock-set-defaults)
          (progn  ;; Emacs 20 and later font-lock mode post-process its variables
            (set 'font-lock-set-defaults nil)
            (funcall 'font-lock-set-defaults))))))

(defun magik-session-prompt-get (&optional force-query-p)
  "If `magik-session-prompt' is nil, get the Magik session's command line prompt.
If interactive and a prefix arg is used then Magik session will be
queried irrespective of default value of `magik-session-prompt'"
  (interactive "P")
  (if (and (null force-query-p)
           (stringp (default-value 'magik-session-prompt))) ;user has overridden setting
      (progn
        (compat-call setq-local magik-session-prompt (or magik-session-prompt ;user may have set a local value for it
                                                         (default-value 'magik-session-prompt)))
        (magik-session-prompt-update-font-lock))
    (process-send-string
     magik-session-process
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
(add-hook 'magik-session-start-process-post-hook 'magik-session-prompt-get)

(defun magik-session-shell ()
  "Start a command shell with the same environment as the current Magik process."
  (interactive)
  (require 'shell)
  (let ((process-environment (cl-copy-list magik-session-process-environment))
        (exec-path (cl-copy-list magik-session-exec-path))
        (buffer (concat "*shell*" (buffer-name)))
        (version (and (boundp 'magik-session-version-current) (symbol-value 'magik-session-version-current))))
    (make-comint-in-buffer "magik-session-shell"
                           buffer
                           (executable-find "cmd") nil "/k"
                           (concat (getenv "SMALLWORLD_GIS") "\\config\\environment.bat"))
    (with-current-buffer buffer
      (if (stringp version) (set 'magik-session-version-current version)))
    (display-buffer buffer)))

(defun magik-session-parse-gis-command (command)
  "Parse the magik-session-command string taking care of any quoting.
Return a list of all the components of the COMMAND."

  ;;Copy the string into a temp buffer.
  ;;Use the Emacs sexp code and an appropriate syntax-table 'magik-session-command-syntax-table'
  ;;to cope with quotes and possible escaped quotes.
  ;;forward-sexp therefore guarantees preservation of white within quoted regions.
  ;;However, I do some extra work to try and remove the surrounding quotes from the returned result
  (let ((temp-buf (get-buffer-create " *magik-session-command parser*"))
        (command-list))
    (save-excursion
      (save-match-data
        (set-buffer temp-buf)
        (erase-buffer)
        (set-syntax-table magik-session-command-syntax-table)
        (insert command)

                                        ;Remove excess trailing whitespace to avoid spurious extra empty arguments being passed
        (goto-char (point-max))
        (delete-horizontal-space)

        (goto-char (point-min))
        (condition-case var
            (setq command-list
                  (cl-loop
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
                                        ;Now look for embedded environment variables
                        (setq substr (substitute-in-file-name substr)))
                   collect substr
                   until (eobp)))
          (scan-error
           (error "%s or quotes" (cadr var)))))
      (kill-buffer temp-buf)
      command-list)))

(defun magik-session-buffer-alist-remove ()
  "Remove current buffer from `magik-session-buffer-alist'."
  (let ((c (rassoc (buffer-name) magik-session-buffer-alist)))
    (if c
        (progn
          (setcdr c nil)
          (car c)))))

(defun magik-session-buffer-alist-prefix-function (arg mode predicate)
  "Function to process prefix keys when used with \\[magik-session]."
  (let ((buf (cdr (assq arg magik-session-buffer-alist))))
    (if (and buf
             (with-current-buffer buf
               (magik-utils-buffer-mode-list-predicate-p predicate)))
        t
      (error "No Magik session buffer"))
    buf))

(defun magik-session-command-display (command)
  "Return shortened Magik session COMMAND suitable for display."
  (if (stringp command) ; defensive programming. Should be a string but need to avoid errors
      (let              ; because this function is called in a menu-update-hook
          ((command-len (- (min (length command) magik-session-command-history-max-length)))
           (label ""))
        (save-match-data
          (if (string-match "^\\[[^\]]*\\]" command)
              (setq label
                    (concat (magik-utils-file-name-display (match-string 0 command)
                                                           magik-session-command-history-max-length-dir)
                            "..."))))
        (concat label (substring command (+ command-len (length label)))))))

(defun magik-session-update-tools-magik-gis-menu ()
  "Update Magik Session processes submenu in Tools -> Magik pulldown menu."
  (let* ((magik-session-alist (sort (copy-alist magik-session-buffer-alist)
                                    #'(lambda (a b) (< (car a) (car b)))))
         magik-session-list)
    (dolist (c magik-session-alist)
      (let ((i   (car c))
            (buf (cdr c)))
        (if buf
            (setq magik-session-list
                  (append magik-session-list
                          (list (vector buf
                                        (list 'display-buffer buf)
                                        ':active t
                                        ':keys (format "M-%d f2 z" i))))))))
    ;;Magik session buffers ordered according to when they were started.
    ;;killed session numbers are reused.
    (easy-menu-change (list "Tools" "Magik")
                      "Magik Session Processes"
                      (or magik-session-list (list "No Processes")))))

(defun magik-session-update-magik-session-menu ()
  "Update the Magik Session Command history in the Magik Session pulldown menu."
  (when (derived-mode-p 'magik-session-mode)
    (let (command-list)
      (save-match-data
        ;;Delete duplicates from magik-session-command-history local and global values
        ;;Note: delete-duplicates does not appear to work on localised variables.
        (compat-call setq-local magik-session-command-history (cl-remove-duplicates magik-session-command-history :test 'equal))
        (setq-default magik-session-command-history
                      (cl-remove-duplicates (default-value 'magik-session-command-history)
                                            :test 'equal))

        (dolist (command magik-session-command-history)
          (push (apply
                 'vector
                 (magik-session-command-display command)
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
                              (apply 'vector (magik-session-command-display magik-session-current-command)
                                     'ignore ':active nil (list ':key-sequence nil
                                                                ':help (purecopy magik-session-current-command)))
                              (apply 'vector "Start New Magik Session" 'magik-session-new-buffer
                                     ':active t
                                     ':keys '("C-u f2 z"))))))

      (easy-menu-change (list "Magik Session")
                        "Magik Session Command History"
                        (or command-list (list "No History"))))))

(defun magik-session-update-tools-magik-shell-menu ()
  "Update External Shell Processes submenu in Tools -> Magik pulldown menu."
  (let ((shell-bufs (magik-utils-buffer-mode-list 'shell-mode
                                                  (function (lambda () (getenv "SMALLWORLD_GIS")))))
        shell-list)
    (cl-loop for buf in shell-bufs
             do (push (vector buf (list 'display-buffer buf) t) shell-list))
    (easy-menu-change (list "Tools" "Magik")
                      "External Shell Processes"
                      (or shell-list (list "No Processes")))))

(define-derived-mode magik-session-mode nil "Magik Session"
  "Major mode to run a Magik session as a direct subprocess.
The default name for a buffer running a session is \"*gis*\". The name of
the current session buffer is stored in the user option `magik-session-buffer`.
There are many ways to recall previous commands (see the online
help with \\[help-command]).
Commands are sent to the session with the \\[magik-session-newline] or
\\[magik-session-send-command-at-point].
Entry to this mode runs `magik-session-mode-hook`.
\\{magik-session-mode-map}"
  :group 'magik
  :syntax-table magik-base-mode-syntax-table

  (compat-call setq-local
               selective-display t
               comint-last-input-start (make-marker)
               comint-last-input-end (make-marker)
               magik-session-command-history (or magik-session-command-history
                                                 (default-value 'magik-session-command-history))
               magik-session-filter-state "\C-a"
               magik-session-cb-buffer (concat "*cb*" (buffer-name))
               magik-session-cmd-num magik-session-cmd-num
               magik-session-drag-n-drop-mode-line-string " DnD"
               magik-session-no-of-cmds magik-session-no-of-cmds
               magik-session-prev-cmds magik-session-prev-cmds
               magik-transmit-debug-mode-line-string " #DEBUG"
               show-trailing-whitespace nil
               font-lock-defaults '(magik-session-font-lock-keywords nil t ((?_ . "w")))
               ac-sources (append '(magik-ac-class-method-source
                                    magik-ac-dynamic-source
                                    magik-ac-global-source
                                    magik-ac-object-source
                                    magik-ac-raise-condition-source)
                                  ac-sources)
               magik-session-exec-path (cl-copy-list (or magik-session-exec-path exec-path))
               magik-session-process-environment (cl-copy-list (or magik-session-process-environment process-environment))
               mode-line-process '(": %s")
               local-abbrev-table magik-base-mode-abbrev-table)

  (unless magik-session-no-of-cmds
    (compat-call setq-local
                 magik-session-no-of-cmds 1
                 magik-session-cmd-num 0
                 magik-session-prev-cmds (make-vector 100 nil))
    (aset magik-session-prev-cmds 0 (let ((m (point-min-marker))) (cons m m))))

  (unless (and magik-session-buffer (get-buffer magik-session-buffer))
    (setq-default magik-session-buffer (buffer-name)))

  (unless (rassoc (buffer-name) magik-session-buffer-alist)
    (let ((n 1))
      (while (cdr (assq n magik-session-buffer-alist))
        (setq n (1+ n)))
      (if (assq n magik-session-buffer-alist)
          (setcdr (assq n magik-session-buffer-alist) (buffer-name))
        (add-to-list 'magik-session-buffer-alist (cons n (buffer-name))))))

  ;; Special handling for *gis* buffer
  (if (equal (buffer-name) "*gis*")
      (compat-call setq-local
                   magik-session-exec-path (cl-copy-list exec-path)
                   magik-session-process-environment (cl-copy-list process-environment)))

  (abbrev-mode 1)

  (with-current-buffer (get-buffer-create (concat " *filter*" (buffer-name)))
    (erase-buffer))

  (add-hook 'before-change-functions 'magik-session--prepare-for-edit-cmd nil t)
  (add-hook 'menu-bar-update-hook 'magik-session-update-magik-session-menu nil t)
  (add-hook 'menu-bar-update-hook 'magik-session-update-tools-magik-gis-menu nil t)
  (add-hook 'menu-bar-update-hook 'magik-session-update-tools-magik-shell-menu nil t)
  (add-hook 'kill-buffer-hook 'magik-session-buffer-alist-remove nil t))

(defvar magik-session-menu nil
  "Keymap for the Magik session buffer menu bar.")

(easy-menu-define magik-session-menu magik-session-mode-map
  "Menu for Magik session mode."
  `(,"Magik Session"
    [,"Previous Command"                 magik-session-recall-prev-cmd           t]
    [,"Next Command"                     magik-session-recall-next-cmd           t]
    [,"Previous Matching Command"        magik-session-recall-prev-matching-cmd  t]
    [,"Next Matching Command"            magik-session-recall-next-matching-cmd  t]
    "----"
    [,"Fold"                             magik-session-display-history   :active t :keys "<f2> <up>,   <f2> C-p"]
    [,"Unfold"                           magik-session-undisplay-history :active t :keys "<f2> <down>,   <f2> C-n"]
    "----"
    [,"Electric Template"                magik-explicit-electric-space           t]
    [,"Symbol Complete"                  magik-symbol-complete                   t]
    ;; [,"Deep Print"                       magik-deep-print                      :active t :keys "<f2> x"]
    "----"
    [,"Previous Traceback"               magik-session-traceback-up              t]
    [,"Next Traceback"                   magik-session-traceback-down            t]
    [,"Print Traceback"                  magik-session-traceback-print   :active t :keys "<f4> P,   <f2> ="]
    [,"Save Traceback"                   magik-session-traceback-save            t]
    "----"
    [,"External Shell Process"           magik-session-shell                     t]
    [,"Kill Magik Process"               magik-session-kill-process              (and magik-session-process
                                                                                      (eq (process-status magik-session-process) 'run))]
    (,"Magik Session Command History")
    "---"
    (,"Toggle..."
     [,"Magik Session Filter"             magik-session-filter-toggle-filter     :active t
      :style toggle :selected (let ((b (get-buffer-process
                                        (current-buffer))))
                                (and b (process-filter b)))]
     [,"Drag and Drop"                  magik-session-drag-n-drop-mode       :active t
      :style toggle :selected magik-session-drag-n-drop-mode])
    [,"Customize"                       magik-session-customize               t]))

(defun magik-session-sentinel (proc msg)
  "Sentinel function, runs when the magik process exits."
  (let ((magik-session-exit-status (process-exit-status proc))
        (buf (process-buffer proc)))
    (with-current-buffer buf
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
               buf (process-name proc) (number-to-string magik-session-exit-status))

      ;;Allow messages to appear in *Messages* buffer
      (sit-for 0.01)
      (run-hook-with-args 'magik-session-sentinel-hooks magik-session-exit-status))))

(defun magik-session-start-process (args)
  "Run a Magik process in the current buffer.
Adds `magik-session-current-command' to `magik-session-command-history'
if not already there."
  (run-hooks 'magik-session-start-process-pre-hook)
  (or (member magik-session-current-command magik-session-command-history)
      (add-to-list 'magik-session-command-history magik-session-current-command))
  (compat-call setq-local magik-session-process (apply 'start-process "magik-session-process" (current-buffer) (car args) (cdr args)))
  (set-process-sentinel magik-session-process 'magik-session-sentinel)
  (set-marker (process-mark magik-session-process) (point-max))
  (set-process-filter magik-session-process 'magik-session-filter)

  ;;MF New bit for connecting to the method finder:
  ;;MF We nuke the current cb first and reconnect later.
  (when (and magik-cb-dynamic (get-buffer magik-session-cb-buffer))
    (let ((magik-cb-process (get-buffer-process magik-session-cb-buffer)))
      (if magik-cb-process (delete-process magik-cb-process)))
    (process-send-string magik-session-process "_if method_finder _isnt _unset\n_then\n  method_finder.lazy_start?\n  method_finder.send_socket_to_emacs()\n_endif\n$\n"))
  (sit-for 0.01)
  (run-hooks 'magik-session-start-process-post-hook))

;; Put up here coz of load order problems.
;; The logic of the `F2 s' is still not quite right anyway.

;;;###autoload
(defun magik-session (&optional buffer command)
  "Run a Magik process in a buffer in `magik-session-mode'.

The command is typically \"runalias\" or \"gis\", but
can be any interactive program such as \"csh\".

The program that is offered as a default is stored in the variable,
`magik-session-command', which you can customise.  e.g.

\(setopt magik-session-command
\"[$HOME] runalias swaf_mega\"
\)
The command automatically expands environment variables using
Windows %% and Unix $ and ${} nomenclature.

You can setup a list of standard commands by setting the
default value of `magik-session-command-history'.

Prefix argument controls:
With a numeric prefix arg, switch to the Magik process of that number
where the number indicates the order it was started. The
SW->Magik Processes indicates which numbers are in use. If a Magik process
buffer is killed, its number is reused when a new Magik process is started.

With a non-numeric prefix arg, ask user for buffer name to use for
the process.  This will default to a unique currently unused name based upon
the current value of `magik-session-buffer-default-name'.

If there is already a Magik process running in a visible window or
frame, just switch to that buffer, or prompt if more than one.  If
there is not, prompt for a command to run, and then run it."

  (interactive)
  (if command (setq magik-session-command command))
  (let (dir
        cmd
        args
        ;;read-string's history arg does not work with buffer-local variables
        ;;history also always has something see Package Registration at end
        (command-history magik-session-command-history)
        alias-beg
        alias-expansion
        (alias-buffer "*temp gis alias buffer*")
        (keepgoing t)
        (magik-session-start-process-pre-hook magik-session-start-process-pre-hook)
        (buffer (magik-utils-get-buffer-mode (cond (buffer buffer)
                                                   ((derived-mode-p 'magik-session-mode) (buffer-name))
                                                   (t nil))
                                             'magik-session-mode
                                             "Enter Magik Session buffer:"
                                             (or magik-session-buffer magik-session-buffer-default-name)
                                             'magik-session-buffer-alist-prefix-function
                                             (generate-new-buffer-name magik-session-buffer-default-name)))
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

      (with-current-buffer (get-buffer-create alias-buffer)

        (erase-buffer)
        (when (and (string-equal shell-file-name "/bin/csh")
                   (file-readable-p "~/.alias"))
          (insert-file-contents "~/.alias"))

        (while keepgoing
          (setq keepgoing nil)
          (setq magik-session-command (sub magik-session-command rev-1920-regexp " "))
          (or (eq (string-match "\\[" magik-session-command) 0)
              (setq magik-session-command (concat "[" default-directory "] " magik-session-command)))
          (or command
              (setq magik-session-command
                    (read-string "Magik command: "
                                 (car command-history)
                                 'command-history)))
          (if (string-match rev-1920-regexp magik-session-command)
              (progn
                (setq keepgoing t)
                (setq magik-session-command (sub magik-session-command rev-1920-regexp " "))))
          (or (eq (string-match "\\[" magik-session-command) 0)
              (setq magik-session-command (concat "[" default-directory "] " magik-session-command)))
          (string-match "\\[\\([^\]]*\\)\\] *\\([^ ]*\\) *\\(.*\\)" magik-session-command)
          (setq dir  (substring magik-session-command (match-beginning 1) (match-end 1))
                cmd  (substring magik-session-command (match-beginning 2) (match-end 2))
                args (substring magik-session-command (match-beginning 3) (match-end 3)))

          (goto-char (point-min))
          (if (re-search-forward (concat "^alias[ \t]+" (regexp-quote cmd) "[ \t]+") nil t)
              (progn
                (setq keepgoing t)
                (setq alias-beg (match-end 0))
                (goto-char alias-beg)
                (if (looking-at "['\"]")
                    (progn
                      (cl-incf alias-beg)
                      (end-of-line)
                      (re-search-backward "['\"]"))
                  (end-of-line))
                (setq alias-expansion (buffer-substring alias-beg (point)))
                (or (string-match alias-subst-regexp alias-expansion)
                    (setq alias-expansion (concat alias-expansion " \\!*")))
                (setq alias-expansion (sub alias-expansion alias-subst-regexp args))
                (setq magik-session-command (concat "[" dir "] " alias-expansion)))))

        (kill-buffer alias-buffer))

      (pop-to-buffer (get-buffer-create buffer))
      (unless (derived-mode-p 'magik-session-mode)
        (magik-session-mode))
      (goto-char (point-max))
      (insert "\n" (current-time-string) "\n")
      (setq default-directory (expand-file-name
                               (file-name-as-directory
                                (substitute-in-file-name dir))))
      (compat-call setq-local
                   magik-session-current-command (copy-sequence magik-session-command)
                   magik-session-command-history (cons magik-session-current-command
                                                       (delete magik-session-current-command magik-session-command-history)))
      (setq-default magik-session-command-history (cons magik-session-current-command
                                                        (delete magik-session-current-command magik-session-command-history)))
      (or (file-directory-p default-directory)
          (error "Directory does not exist: %s" default-directory))
      (add-hook 'magik-session-start-process-pre-hook
                (function (lambda () (insert magik-session-command ?\n ?\n)))
                t)
      (magik-session-start-process (magik-session-parse-gis-command (concat cmd " " args))))))

(defun magik-session-new-buffer ()
  "Start a new Magik session."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'gis)))

(defun magik-session-kill-process ()
  "Kill the current Magik process."
  (interactive)
  (if (and magik-session-process
           (eq (process-status magik-session-process) 'run)
           (y-or-n-p "Kill the Magik process? "))
      (let ((status (process-status magik-session-process)))
        (kill-process magik-session-process)
        (sit-for 0.1)
        (if (eq status (process-status magik-session-process))
            (insert "\nMagik is still busy and will exit at an appropriate point. Please be patient... \n")))))

(defun magik-session-query-interrupt-shell-subjob ()
  "Ask and then `comint-interrupt-subjob'."
  (interactive)
  (if (y-or-n-p "Kill the Magik process? ")
      (comint-kill-subjob)))

(defun magik-session-query-quit-shell-subjob ()
  "Ask and then `comint-quit-subjob'."
  (interactive)
  (if (y-or-n-p "Kill the Magik process? ")
      (comint-quit-subjob)))

(defun magik-session-query-stop-shell-subjob ()
  "Ask and then `comint-stop-subjob'."
  (interactive)
  (if (y-or-n-p "Suspend the Magik process? ")
      (comint-stop-subjob)))

(defun magik-session-query-shell-send-eof ()
  "Ask and then `comint-send-eof'."
  (interactive)
  (if (y-or-n-p "Send EOF to the Magik process? ")
      (comint-send-eof)))

;; R E C A L L I N G   C O M M A N D S
;; ___________________________________
;;
;;; Each gis command is recorded by magik-session-prev-cmds so that the
;;; the user can recall and edit previous commands.  This file
;;; also adds dollars and implements the history-folding feature.

(defun magik-session-copy-cmd (n offset)
  "Copy command number N to the bottom of the buffer replacing any current command.
Locate the cursor to an offset OFFSET."
  (delete-region (process-mark (get-buffer-process (current-buffer))) (point-max))
  (goto-char (point-max))
  (let*
      ((pair (aref magik-session-prev-cmds n))
       (str (subst-char-in-string  ?\r ?\n (buffer-substring (car pair) (cdr pair))))
       (len (length str)))
    (insert str)
    (forward-char (- (max 0 (min len offset)) len))
    (if (pos-visible-in-window-p)
        (while
            (not (pos-visible-in-window-p (point-max)))
          (scroll-up 1)))))

(defun magik-session-send-region (beg end)
  "Record in `magik-session-prev-cmds' the region BEG to END and send to the gis.
Also update `magik-session-cmd-num'.
Also append the string to \" *history**gis*\"."
  (save-excursion
    (let ((str (buffer-substring beg end)))
      (set-buffer (get-buffer-create (concat " *history*" (buffer-name))))
      (magik-mode)
      (let ((orig-point (point)))
        (goto-char (point-max))
        (insert str "\n")
        (goto-char orig-point))))
  (let ((n magik-session-no-of-cmds))
    (if (= n (length magik-session-prev-cmds))
        (magik-session--make-new-cmds-vec))
    (setq n magik-session-no-of-cmds)   ;; aaargh! I had forgotten this line and had a horrible intermittent bug.
    ;; NB: we are keeping a null marker at the end and this must be moved along.
    (aset magik-session-prev-cmds n (aref magik-session-prev-cmds (1- n)))
    (aset magik-session-prev-cmds (1- n) (cons (copy-marker beg) (copy-marker end)))
    (compat-call setq-local magik-session-cmd-num magik-session-no-of-cmds)
    (cl-incf magik-session-no-of-cmds)

    (set-marker comint-last-input-start beg)
    (set-marker comint-last-input-end   end)
    (set-marker (process-mark (get-buffer-process (current-buffer))) end)
    (goto-char (point-max))
    (process-send-region (get-buffer-process (current-buffer)) beg end)))

(defun magik-session--make-new-cmds-vec ()
  "Create a new bigger vector for `magik-session-prev-cmds'.
Copies the non-degenerate commands into it."
  (let* ((len (length magik-session-prev-cmds))
         (v (make-vector (+ len 100) nil))
         (i 0)
         (v_i 0))
    (while (< i len)
      (let ((x (aref magik-session-prev-cmds i)))
        (when (and (marker-buffer (car x))
                   (marker-buffer (cdr x))
                   (> (cdr x) (car x)))
          (aset v v_i x)
          (cl-incf v_i)))
      (cl-incf i))
    (let ((m (copy-marker (point-min))))
      (aset v v_i (cons m m)))
    (compat-call setq-local
                 magik-session-no-of-cmds (1+ v_i)
                 magik-session-prev-cmds v)))

(defun magik-session-beginning-of-line (&optional n)
  "Move point to beginning of Nth line or just after prompt.
If command is repeated then place point at beginning of prompt."
  (interactive "p")
  (beginning-of-line n)
  ;;Only move to end of prompt if last-command was this one
  ;;AND a prefix key has not be used (n=1).
  (and (not (and (eq last-command 'magik-session-beginning-of-line) (eq n 1)))
       (looking-at magik-session-prompt)
       (goto-char (match-end 0))))

                                        ; paulw - mods to make pre/post SW5 work in a single emacs
                                        ; see also swkeys.el for key definition

(defun magik-session-toggle-dollar ()
  "Toggle auto-insertion of $ terminator."
  (interactive)
  (setq magik-session-auto-insert-dollar (not magik-session-auto-insert-dollar))
  (message "Insert dollar now %s" (if (symbol-value magik-session-auto-insert-dollar) "enabled" "disabled")))

(defun magik-session-newline (arg)
  "If in a previous cmd, recall.
If within current cmd, insert a newline.
If at end of current cmd and cmd is complete, send to gis.
If at end of current cmd and cmd is not complete, insert a newline.
Else (not in any cmd) recall line."
  (interactive "*P")
  (let
      ((n (magik-session--get-curr-cmd-num))
       (p (process-mark (get-buffer-process (current-buffer)))))
    (cond
     (n  ; in a prev. cmd.
      (magik-session-copy-cmd n
                              (- (point)
                                 (car (aref magik-session-prev-cmds n)))))

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
          (magik-session-send-region (marker-position p) (point)))
         ((magik-session--complete-magik-p p (point))
                                        ;          (insert "$\n") ;; paulw - remove additional <CR> which messes with pling variables
          (if magik-session-auto-insert-dollar (insert "$\n"))
          (delete-region (point) (point-max))
          (magik-session-send-region (marker-position p) (point)))))
       ((looking-at "[ \t\n]*\\$[ \t\n]*\\'")
        (if (magik-session--complete-magik-p p (point))
            (progn
              (search-forward "$")
              (delete-region (point) (point-max))
              (insert "\n")
              (magik-session-send-region (marker-position p) (point)))
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

(defun magik-session--complete-magik-p (beg end)
  "Return t if the region from BEG to END is a syntactically piece of Magik.
Also write a message saying why the magik is not complete."
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

(defun magik-session--get-curr-cmd-num ()
  "Return the num of the command that point is in, or nil if not in a command.
Being in the prompt before the command counts too.  We do this by binary search."
  (magik-session--get-curr-cmd-num-2 0 (1- magik-session-no-of-cmds)))

(defun magik-session--get-curr-cmd-num-2 (min max)
  "Return the num of the command that point is in.
Return nil if it isn't in the half-open range [MIN, MAX)."
  (if (> max min)
      (let*
          ((mid (/ (+ min max) 2))
           (pair (aref magik-session-prev-cmds mid))
           (p (point)))
        (cond
         ((or (null (marker-buffer (car pair))))
                                        ;(= (car pair) (cdr pair)))
          (magik-session--get-curr-cmd-num-2 (1+ mid) max))
         ((save-excursion
            (goto-char (car pair))
            (beginning-of-line)
            (and (>= p (point))
                 (< p (cdr pair))))
          mid)
         ((>= p (cdr pair))
          (magik-session--get-curr-cmd-num-2 (1+ mid) max))
         ((< p (car pair))
          (magik-session--get-curr-cmd-num-2 min mid))
         (t
          (error "Sorry... Confused command recall"))))))

(defun magik-session--prepare-for-edit-cmd (_beg _end)
  "If we're in a previous command, replace any current command with this one."
  (let ((n (magik-session--get-curr-cmd-num)))
    (when n
      (magik-session-copy-cmd n
                              (- (point)
                                 (car (aref magik-session-prev-cmds n)))))))

(defun magik-session-send-command-at-point ()
  "Send the command at point.
Copying to the end of the buffer if necessary and don't add extra dollars."
  (interactive "*")
  (or (get-buffer-process (current-buffer))
      (error "There is no process running in this buffer"))
  (let
      ((n (magik-session--get-curr-cmd-num))
       (p (process-mark (get-buffer-process (current-buffer)))))
    (cond
     (n
      (magik-session-copy-cmd n 0)
      (magik-session-send-region (marker-position p) (point-max)))
     ((>= (point)
          (save-excursion
            (goto-char p)
            (beginning-of-line)
            (point)))
      (goto-char (point-max))
      (or (eq (preceding-char) ?\n)
          (insert ?\n))
      (magik-session-send-region (marker-position p) (point-max)))
     (t
      (error "Not a command")))))

(defun magik-session--matching-cmd-p (n str)
  "Return t if magik-session-prev-cmds[N] is a command matching STR or off the end.
E.g. n<0 or n>=magik-session-no-of-cmds."
  (or (< n 0)
      (>= n magik-session-no-of-cmds)
      (let
          ((pair (aref magik-session-prev-cmds n))
           (len (length str)))
        (and (marker-buffer (car pair))
             (> (- (cdr pair) (car pair)) len)
             (equal (buffer-substring (car pair) (+ (car pair) len)) str)))))

(defun magik-session-recall (str step end-of-command-p)
  "Recall a command starting with STR in the direction STEP.
If END-OF-COMMAND-P is t then cursor is placed at and of the recalled command.
An internal function that deals with 4 cases."
  (or (get-buffer-process (current-buffer))
      (error "There is no process running in this buffer"))
  (let ((n magik-session-cmd-num)
        mark)
    (while
        (progn
          (cl-incf n step)
          (not (magik-session--matching-cmd-p n str))))
    (when (= n -1)
      (if (equal str "")
          (user-error "No previous command")
        (user-error "No previous command matching '%s'" str)))
    (when (= n magik-session-no-of-cmds)
      (cl-decf n)
      (if (equal str "")
          (user-error "No next command")
        (user-error "No next command matching '%s'" str)))
    (setq mark (process-mark (get-buffer-process (current-buffer))))
    (magik-session-copy-cmd n
                            (if (equal str "")
                                (- (point) mark)
                              (length str)))
    (compat-call setq-local magik-session-cmd-num n)
    (when end-of-command-p
      (goto-char (point-max))
      ;; skip back past \n$\n and whitespace
      (skip-chars-backward " \t\n$" mark))))

(defun magik-session-recall-prev-cmd ()
  "Recall the earlier Magik session commands.

The variable \\[magik-session-recall-cmd-move-to-end] decides
whether cursor point is placed at end of command.
Compare with \\[magik-session-recall-prev-matching-cmd]"
  (interactive "*")
  (magik-session-recall "" -1 magik-session-recall-cmd-move-to-end))

(defun magik-session-recall-next-cmd ()
  "Recall the later Magik session commands.

The variable \\[magik-session-recall-cmd-move-to-end] decides
whether cursor point is placed at end of command.
Compare with \\[magik-session-recall-next-matching-cmd]"
  (interactive "*")
  (magik-session-recall "" 1 magik-session-recall-cmd-move-to-end))

(defun magik-session-recall-prev-matching-cmd ()
  "Recall prev Magik session commands matching part of the command before cursor."
  (interactive "*")
  (magik-session-recall (buffer-substring
                         (process-mark (get-buffer-process (current-buffer)))
                         (point))
                        -1
                        nil))

(defun magik-session-recall-next-matching-cmd ()
  "Recall next Magik session commands matching part of the command before cursor."
  (interactive "*")
  (magik-session-recall (buffer-substring
                         (process-mark (get-buffer-process (current-buffer)))
                         (point))
                        1
                        nil))

(defun magik-session-display-history (arg)
  "Fold (hide) away the parts of the Magik buffer in between the last ARG commands.
If ARG is null, use a default of `magik-session-history-length'."
  (interactive "*P")
  (setq arg (if (null arg) magik-session-history-length (prefix-numeric-value arg)))
  (let
      ((b (current-buffer)))
    (or (derived-mode-p 'magik-session-mode)
        (set-buffer magik-session-buffer))
    (compat-call setq-local selective-display t)
    (let
        ((p (point))
         (i (max 0 (min (- magik-session-no-of-cmds arg 1) (1- magik-session-no-of-cmds))))
         j)
      ;; look for the start point of the folding; this may involve skipping
      ;; over bad markers.
      (while
          (and (< i magik-session-no-of-cmds)
               (not (marker-buffer (car (aref magik-session-prev-cmds i)))))
        (cl-incf i))
      (if (= i (1- magik-session-no-of-cmds))
          (error "No commands to fold"))
      ;; we now have the index of the first command to fold.
      (message "Folding the last %s commands..." (number-to-string arg))
      (goto-char (car (aref magik-session-prev-cmds i)))
      (while (search-forward "\n" nil t)
        (replace-match "\r"))
      (setq j i)
      (while
          (< j (1- magik-session-no-of-cmds))
        (goto-char (car (aref magik-session-prev-cmds j)))
        (if (re-search-backward "[\r\n]" nil t)
            (progn
              (insert ?\n)
              (delete-char 1)))
        (cl-incf j))
      (goto-char (point-max))
      (if (search-backward "\r" nil t)
          (progn
            (insert ?\n)
            (delete-char 1)))
      (message "Folding the last %s commands...Done" (number-to-string arg))
      (goto-char p)
      (set-buffer b))))

(defun magik-session-undisplay-history (arg)
  "Unfold the last ARG commands.
If ARG is null, use a default of `magik-session-history-length'."
  (interactive "*P")
  (setq arg (if (null arg) magik-session-history-length (prefix-numeric-value arg)))
  (let
      ((b (current-buffer)))
    (or (derived-mode-p 'magik-session-mode)
        (set-buffer magik-session-buffer))
    (compat-call setq-local selective-display t)
    (let
        ((p (point))
         (i (max 0 (min (- magik-session-no-of-cmds arg 1) (1- magik-session-no-of-cmds)))))
      ;; look for the start point of the folding; this may involve skipping
      ;; over bad markers.
      (while
          (and (< i magik-session-no-of-cmds)
               (not (marker-buffer (car (aref magik-session-prev-cmds i)))))
        (cl-incf i))
      (if (= i (1- magik-session-no-of-cmds))
          (error "No commands to unfold"))
      ;; we now have the index of the first command to unfold.
      (message "Unfolding the last %s commands..." (number-to-string arg))
      (goto-char (car (aref magik-session-prev-cmds i)))
      (while (search-forward "\r" nil t)
        (replace-match "\n"))
      (message "Unfolding the last %s commands...Done" (number-to-string arg))
      (goto-char p)
      (set-buffer b))))

(defun magik-session-goto-process-mark ()
  "(goto-char (process-mark (get-buffer-process (current-buffer))))."
  (interactive)
  (goto-char (process-mark (get-buffer-process (current-buffer)))))

(defun magik-session-set-process-mark-to-eob ()
  "(set-marker (process-mark (get-buffer-process (current-buffer))) (point-max))."
  (interactive)
  (set-marker (process-mark (get-buffer-process (current-buffer))) (point-max)))
;;;
;;;  T R A C E B A C K
;;;

;; support for `magik-session-traceback-print'
(defun magik-session-print-region-and-fold (start end switches)
  "Like `print-region-1' but with long lines folded first."
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

(defun magik-session-error-narrow-region ()
  "Narrow region between the current Magik prompt."
  (narrow-to-region (save-excursion (re-search-backward magik-session-prompt))
                    (save-excursion
                      (or (re-search-forward magik-session-prompt nil t) (goto-char (point-max)))
                      (beginning-of-line)
                      (point))))

(defun magik-session-error-line-col (line)
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

(defun magik-session-error-goto ()
  "Goto file that contain the Magik error."
  (interactive)
  (let ((case-fold-search nil) ;case-sensitive searching required for "Loading"
        (line-adjust 0)
        (pos 0)
        file line-col buf)
    (save-match-data
      (save-restriction
        (magik-session-error-narrow-region)
        (save-excursion
          (beginning-of-line)
          (if (looking-at (concat "^\\*\\*\\*\\*.*" "on line" " \\([0-9]+\\)$"))
              (progn
                (setq line-col (magik-session-error-line-col (string-to-number (match-string-no-properties 1)))
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

(defun magik-session-error-goto-mouse (click)
  "Goto error at mouse point CLICK."
  (interactive "e")
  (mouse-set-point click)
  (magik-session-error-goto))

(defun magik-session-traceback-print ()
  "Send the text from the most recent error to the end of the buffer to the print.
Query first for \"**** Error\"."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (search-backward "\n**** Error" nil t)
        (if (y-or-n-p (concat (format "Print the last traceback (%s lines)?"
                                      (number-to-string (count-lines (point) (point-max)))) " "))
            (magik-session-print-region-and-fold (point) (point-max) nil))
      (error "Couldn't find a line starting with '**** Error' - nothing printed" ))))

(defun magik-session-traceback-save ()
  "Save in \"~/traceback.txt\" all the text onwards from the most recent error.
An error is is searched using \"**** Error\"."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (search-backward "\n**** Error" nil t)
        (progn
          (write-region (point) (point-max) "~/traceback.txt")
          (message "Saved the traceback in '~/traceback.txt'."))
      (error "Couldn't find a line starting with '**** Error' - nothing saved"))))

(defun magik-session-traceback-up ()
  "Move up buffer to the previous traceback."
  (interactive)
  (if (re-search-backward "---- traceback: " nil t)
      (forward-line -1)
    (user-error "No previous traceback found")))

(defun magik-session-traceback-down ()
  "Move down buffer to the next traceback."
  (interactive)
  (forward-line 2)
  (if (re-search-forward "---- traceback: " nil t)
      (forward-line -1)
    (forward-line -2)
    (user-error "No next traceback found")))

;;; Drag 'n' Drop
;;
;; When a file is dragged and dropped and the current buffer is
;; as Magik mode buffer, the file is loaded into the Magik session.

(defun magik-session-drag-n-drop-mode (&optional arg)
  "Toggle Drag and drop Magik loading functionality."
  (interactive "P")
  (setq magik-session-drag-n-drop-mode
        (if (null arg)
            (not magik-session-drag-n-drop-mode)
          (> (prefix-numeric-value arg) 0)))
  (add-hook 'find-file-hooks 'magik-session-drag-n-drop-load)
  (if magik-session-drag-n-drop-mode
      (message "Magik 'Drag and Drop' file mode is on")
    (message "Magik 'Drag and Drop' file mode is off"))
  (force-mode-line-update))

(defun magik-session-drag-n-drop-load ()
  "Load a drag and dropped file into the Magik Session.
If the previous buffer was a Magik session buffer and the previous event was
a drag & drop event then we load the dropped file into the Magik session.

The file must be in a Major mode that defines the function:
  MODE-drag-n-drop-load
where MODE is the name of the major mode with the '-mode' postfix."
  (let (fn gis)
    ;;hopefully the tests are done in the cheapest, most efficient order
    ;;but gis-drag-n-drop-mode is checked last in case user has set
    ;;up a per-buffer Drag 'n' drop mode
    (if (and (listp last-input-event)
             (eq (car last-input-event) 'drag-n-drop)
             (setq fn (intern (concat (substring (symbol-name major-mode) 0 -5)
                                      "-drag-n-drop-load")))
             (fboundp fn)
             (windowp (caadr last-input-event))
             (setq gis (window-buffer (caadr last-input-event)))
             (with-current-buffer gis

               (and magik-session-drag-n-drop-mode
                    (derived-mode-p 'magik-session-mode))))
        (funcall fn gis (buffer-file-name)))))

(defun magik-session-disable-save ()
  "Like `save-buffer', but does nothing in magik-session-mode."
  (interactive)
  (message "Can't save Magik Session buffer."))

;;;Package registration

;;Ensure Default magik-session-command are placed at head of magik-session-command-history
(mapc (function
       (lambda (c)
         (and c
              (not (member c magik-session-command-history))
              (push c magik-session-command-history))))
      (list magik-session-command-default magik-session-command))

;;; package setup via setting of variable before load.
(and magik-session-drag-n-drop-mode (magik-session-drag-n-drop-mode magik-session-drag-n-drop-mode))

(or (assq 'magik-session-drag-n-drop-mode minor-mode-alist)
    (push '(magik-session-drag-n-drop-mode magik-session-drag-n-drop-mode-line-string) minor-mode-alist))
;;(or (assoc 'magik-session-drag-n-drop-mode minor-mode-alist)
;;    (push (list 'magik-session-drag-n-drop-mode " DnD") minor-mode-alist))

;;MSB configuration
(defun magik-session-msb-configuration ()
  "Add Magik buffers to MSB menu, supposes that MSB is already loaded."
  (let* ((l (length msb-menu-cond))
         (last (nth (1- l) msb-menu-cond))
         (precdr (nthcdr (- l 2) msb-menu-cond)) ; cdr of this is last
         (handle (1- (nth 1 last))))
    (setcdr precdr (list
                    (list
                     '(derived-mode-p 'magik-session-mode)
                     handle
                     "Magik (%d)")
                    last))))

(with-eval-after-load 'msb
  (magik-session-msb-configuration))

(with-eval-after-load 'auto-complete
  (add-to-list 'ac-modes 'magik-session-mode))

(defvar magik-session-mode-error-map (make-sparse-keymap)
  "Keymap for Jumping to error messages.")

(progn
  ;; ------------------------ magik session mode ------------------------

  (define-key magik-session-mode-error-map [mouse-2]  'magik-session-error-goto-mouse)
  (define-key magik-session-mode-error-map [C-return] 'magik-session-error-goto)

  (define-key magik-session-mode-map "\ep"       'magik-session-recall-prev-cmd)
  (define-key magik-session-mode-map "\en"       'magik-session-recall-next-cmd)
  (define-key magik-session-mode-map "\r"        'magik-session-newline)
  (define-key magik-session-mode-map " "         'magik-yas-maybe-expand)
  (define-key magik-session-mode-map "\C-a"      'magik-session-beginning-of-line)
  (define-key magik-session-mode-map [f8]        'magik-session-send-command-at-point)
  (define-key magik-session-mode-map "\C-c\C-c"  'magik-session-kill-process)
  (define-key magik-session-mode-map "\C-c\C-\\" 'magik-session-query-quit-shell-subjob)
  (define-key magik-session-mode-map "\C-c\C-z"  'magik-session-query-stop-shell-subjob)
  (define-key magik-session-mode-map "\C-c\C-d"  'magik-session-query-shell-send-eof)

  (define-key magik-session-mode-map (kbd "<f2> <up>")   'magik-session-display-history)
  (define-key magik-session-mode-map (kbd "<f2> \C-p")   'magik-session-display-history)
  (define-key magik-session-mode-map (kbd "<f2> <down>") 'magik-session-undisplay-history)
  (define-key magik-session-mode-map (kbd "<f2> \C-n")   'magik-session-undisplay-history)
  (define-key magik-session-mode-map (kbd "<f2> =")      'magik-session-traceback-print)
  (define-key magik-session-mode-map (kbd "<f2> f")      'magik-session-filter-toggle-filter)
  (define-key magik-session-mode-map (kbd "<f2> p")      'magik-session-recall-prev-matching-cmd)
  (define-key magik-session-mode-map (kbd "<f2> n")      'magik-session-recall-next-matching-cmd)

  (define-key magik-session-mode-map (kbd "<f4> <f4>")   'magik-symbol-complete)
  (define-key magik-session-mode-map (kbd "<f4> <up>")   'magik-session-traceback-up)
  (define-key magik-session-mode-map (kbd "<f4> <down>") 'magik-session-traceback-down)
  (define-key magik-session-mode-map (kbd "<f4> $")      'magik-session-shell)
  (define-key magik-session-mode-map (kbd "<f4> g")      'magik-session-error-goto)
  (define-key magik-session-mode-map (kbd "<f4> m")      'magik-copy-method-to-buffer)
  (define-key magik-session-mode-map (kbd "<f4> r")      'magik-copy-region-to-buffer)
  (define-key magik-session-mode-map (kbd "<f4> s")      'magik-add-debug-statement)
  (define-key magik-session-mode-map (kbd "<f4> w")      'magik-set-work-buffer-name)
  (define-key magik-session-mode-map (kbd "<f4> P")      'magik-session-traceback-print)
  (define-key magik-session-mode-map (kbd "<f4> S")      'magik-session-traceback-save)

  (define-key magik-session-mode-map [remap save-buffer] 'magik-session-disable-save))

(provide 'magik-session)
;;; magik-session.el ends here
