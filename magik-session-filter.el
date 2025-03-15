;;; magik-session-filter.el --- deal with the output from the magik process.

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

;; The filter receives all the text coming back from magik.  Under
;; normal conditions, it just puts the text in the buffer.  However,
;; all text between a C-a character (ascii 1) and a C-e character are
;; intercepted and displayed in special ways according to the
;; character immediately following the C-a character.
;;
;; The filter rotates between 3 states, which we indicate by the
;; string it is searching for in order to come out of that state.
;;
;; It stays in `normal' state until it sees a C-a character.  Then it
;; sits in `special' state until it sees a C-e character.  Then it is
;; just waiting (in `prompt' state) for the space character, before it
;; goes back into `normal' state again.
;;
;; Excess characters after the particular character it is looking for
;; are dealt with by calling ourselves recursively with the remainder
;; of the string.

;;; Code:

(eval-when-compile
  (defvar comint-last-input-end) ;;avoid compiler warning
  (require 'comint))

(require 'magik-mode)
(require 'magik-session)

(defvar magik-session-filter-state "\C-a"
  "Either \"\\C-a\", \"\\C-e\, \"\\C-f\" or \" \".")

(defcustom magik-session-filter-action-alist nil
  "An alist that matches the different filter actions with the action character.
The alist is a cons cell of the form (CHARACTER . FUNCTION).
FUNCTION takes one argument, the string after the action character."
  :group 'magik
  :type '(repeat (cons (character :tag "Filter character")
                       (function :tag "Action function"))))

(defun magik-session-filter-get-state (buf)
  "Displays the state of the Magik session filter for BUF."
  (with-current-buffer buf
    magik-session-filter-state))

(defun magik-session-filter (proc str)
  "Deal with strings coming back from Magik."
  (save-match-data
    (let*
        ((buf (process-buffer proc))
         (n (string-match (magik-session-filter-get-state buf) str))
         (filter-buf (concat " *filter*" (buffer-name buf))))
      (cond ((equal (magik-session-filter-get-state buf) "\C-e")
             (with-current-buffer (get-buffer-create filter-buf)
               (insert (if n (substring str 0 n) str))
               (message "Filtering Magik output...(%s chars)" (number-to-string (point-max))))
             (if n
                 (condition-case err
                     (magik-session-filter-action
                      proc
                      (with-current-buffer (get-buffer-create filter-buf)
                        (prog1
                            (buffer-string)
                          (erase-buffer))))
                   (error
                    (with-current-buffer buf
                      (setq n nil
                            str ""
                            magik-session-filter-state "\C-a")
                      (message "Error: %s" (error-message-string err)))))))
            ((equal (magik-session-filter-get-state buf) "\C-a")
             (with-current-buffer buf (magik-session-filter-insert buf proc n str)))
            (t
             nil))
      ;; else if in " " or "\C-f" state then do nothing.
      (if n
          (progn
            (with-current-buffer buf
              (setq magik-session-filter-state
                    (cdr (assoc magik-session-filter-state
                                '(("\C-a" . "\C-e")
                                  ("\C-e" . " ")
                                  ("\C-f" . "\C-a")
                                  (" " . "\C-a"))))))
            (magik-session-filter proc (substring str (1+ n))))))))

(defun magik-session-filter-insert (buf proc n str)
  "Insert into BUF at the `process-mark' of PROC, N chars from STR.
If N is nil insert the whole of STR.  We insert before all markers except the
 `comint-last-input-end' and the last command from magik-session-prev-cmds."
  (save-excursion
    (goto-char (process-mark proc))
    ;; we make sure that the end-marker for the last command typed by the user
    ;; (if there is one, else just the null initial command)
    ;; is not moved along by the `insert-before-markers' function.
    ;; Also the marker `comint-last-input-end' mustn't shift.
    ;;
    ;; BAD!!!  Evil bug: we mustn't assume that `buf' has a window!
    ;; BAD!!!            This was only put in so that the initial gis
    ;; BAD!!!            startup messages would show.  This is now fixed
    ;; BAD!!!            by simply inserting a welcome string in the buffer.
    ;; BAD!!! Also, we must make sure that any window start markers remain pinned to the
    ;; BAD!!! start of the line!
    (let*
        ((b (car (aref magik-session-prev-cmds (max 0 (- magik-session-no-of-cmds 2)))))
         (e (cdr (aref magik-session-prev-cmds (max 0 (- magik-session-no-of-cmds 2)))))
         (b-pos (marker-position b))
         (e-pos (marker-position e))
         (comint-last-input-end-pos (marker-position comint-last-input-end))
         (pt (point))
         ;; BAD!!! (w (get-buffer-window buf))
         )
      (insert-before-markers (if n (substring str 0 n) str))
      (set-marker b b-pos)  ;shouldn't really be necessary
      (set-marker e e-pos)
      (set-marker comint-last-input-end comint-last-input-end-pos)
      ;; BAD!!! (goto-char (window-start w))
      ;; BAD!!! (beginning-of-line)
      ;; BAD!!! (set-window-start w (point) t)
      (save-restriction
        (save-match-data
          (narrow-to-region pt (point))
          (goto-char (point-min))
          (while (re-search-forward (concat "^\\*\\*\\*\\*.*" "on line" " \\([0-9]+\\)") nil t)
            (add-text-properties (match-beginning 0) (match-end 0)
                                 (list 'mouse-face 'highlight
                                       'help-echo "mouse-2: Goto error in file"
                                       'local-map magik-session-mode-error-map))))))))

(defun magik-session-filter-toggle-filter (&optional buffer)
  "Toggle the filter on the Magik session BUFFER.

With a prefix arg, ask user for Magik session buffer to use."
  (interactive)
  (setq buffer (magik-utils-get-buffer-mode buffer
                                            'magik-session-mode
                                            "Enter Magik Session buffer: "
                                            magik-session-buffer
                                            'magik-session-buffer-alist-prefix-function))
  (barf-if-no-gis buffer)

  (let ((process (get-buffer-process buffer)))
    (if (process-filter process)
        (progn
          (set-process-filter process nil)
          (message "Cancelled the filter in '%s'." buffer))
      (with-current-buffer (get-buffer-create (concat " *filter*" buffer))
        (erase-buffer)
        (set-buffer buffer)
        (setq magik-session-filter-state "\C-a")
        (set-process-filter process 'magik-session-filter))
      (message "Set the filter in '%s'." buffer))))

(defun magik-session-filter-action (proc str)
  "Deal with STR back from Magik according to `magik-session-filter-action-alist'."
  (let* ((s (substring str 0 1))
         (minibuffer-window (minibuffer-window))
         (minibuffer-active-p (and minibuffer-window
                                   (minibuffer-window-active-p minibuffer-window)))
         fn)

    ;;Protect against filter running with minibuffer window active.
    (if minibuffer-active-p
        (set-buffer (process-buffer proc)))

    (setq str (substring str 1)
          fn (cdr (assoc s magik-session-filter-action-alist)))
    (if fn
        (progn
          (funcall fn proc str)
          (and minibuffer-active-p (select-window minibuffer-window)))
      (and minibuffer-active-p (select-window minibuffer-window))
      (error "No filter defined for character %s" s))))

(defun magik-session-filter-register-action (string function &optional replace)
  "Registers the Magik session filter STRING with execution of FUNCTION.
STRING is normally a single character.
If STRING has already been registered then an error is raised unless
REPLACE is t. If REPLACE is t then the FUNCTION will replace the old
action's function setting."
  (let ((action (assoc string magik-session-filter-action-alist)))
    (cond ((not action)
           (add-to-list 'magik-session-filter-action-alist (cons string function)))
          (replace
           (setcdr action function))
          (load-file-name ;avoid message when reloading library
           nil)
          (t
           (message "Filter action for '%s' is already set" string)))))

;;; generic magik-session-filter code ends here.

;;; Set up filter action functions for the Magik session process
(defun magik-session-filter-action-completion (proc str)
  "Magik sessions Filter Action interface for a Magik symbol completion.
According to STR returned from Magik."
  (let ((ans (read str))
        (curr-word-len (length (magik-utils-curr-word))))
    (cond
     ((eq (length ans) 0)
      (message "Can't find completion for %s." (magik-utils-curr-word)))
     ((eq (length ans) 1)
      (if (eq (length (car ans)) curr-word-len)
          (message "Sole completion.")
        (insert (substring (car ans) curr-word-len))
        (if (<= curr-word-len 2)
            (message "Finding completions... Done"))))
     (t
      (let*
          ((longest-common-prefix (car ans))
           (len (length longest-common-prefix))
           (strings (cdr ans))
           i)
        (while
            strings
          (setq i 0)
          (while
              (and (< i len)
                   (< i (length (car strings)))
                   (eq (aref (car strings) i) (aref longest-common-prefix i)))
            (cl-incf i))
          (setq len i)
          (pop strings))
        (if (> len curr-word-len)
            (insert (substring longest-common-prefix curr-word-len len))
          (with-output-to-temp-buffer "*magik completions*"
            (display-completion-list ans))))
      (if (<= curr-word-len 2)
          (message "Finding completions...Done."))))))


(defun magik-session-filter-action-deep-print (proc str)
  "Magik session Filter Action interface for a deep print action.
According to the STR returned from Magik."
  (let ((buffer (concat "*deep print*" (buffer-name (process-buffer proc)))))
    (pop-to-buffer buffer) ;;Buffer should always exist since the only User interface is via deep-print.
    (if (eq (string-to-char str) ?\n)     ; then this is a sub-structure
        (progn
          (forward-line)
          (setq str (substring str 1)))
      ;; else this is an initial structure.
      (erase-buffer))
    (insert str)))

(defun magik-session-filter-action-find-file (proc str)
  "(Deprecated) Magik session Filter Action interface for `find-file'.
Find a file and goto a particular line number
STR is of the form 42:/bla/bla/foo.magik or
:/bla/bla/foo.magik which means don't jump to any
particular line number."
  (save-match-data
    (string-match ":" str)
    (let
        ((beg (match-beginning 0)))
      (find-file-other-window (substring str (match-end 0)))
      (or (zerop beg)
          (goto-line (string-to-number str))))))

(defun magik-session-filter-action-file-open (proc str)
  "Magik session Filter Action interface for opening files in Emacs.
STR consists of newline separated KEY=VALUE pairs.
Recognised KEYs are:
  file        Name of file to open
  function    Elisp function to open file with (default \\[find-file])
  line        Line to jump to.
  column      Column to place the cursor on.
Search based keys:
  method      Method to search for in file.
  class       Class  to search for in file.
  search      String to search for in file.

PROC must return the buffer of the file.

method and class searching combine to a specific search
search is a generic search string.  If given with method and class
the string will be searched for after the method and class search has completed.

line and column can be used together.
The behaviour is undefined if any search key and line or column are used."
  (save-match-data
    (let ((alist '((function . "find-file")))
          val start-pt)
      ;;process KEY=VALUE pairs
      (dolist (i (split-string str "\n+"))
        (when (string-match "^\\([^=]+\\)=\\(.*\\)" i)
          (setq alist (cons (cons (intern (match-string 1 i)) (match-string 2 i))
                            alist))))

      ;;Open file
      (set-buffer (funcall (intern (cdr (assq 'function alist))) (cdr (assq 'file alist))))

      ;;act on keys and values.
      (if (setq val (assq 'method alist))
          (progn
            (widen)
            (goto-char (point-min))
            (magik-goto-class-method (cdr val) (cdr (assq 'class alist)))
            (setq start-pt (point))))
      (if (setq val (assq 'search alist))
          (progn
            (widen)
            (goto-char (or start-pt (point-min))) ;;continue search from class.method?
            (if (search-forward (cdr val) nil t)
                (goto-char (match-beginning 0)))))
      (if (setq val (assq 'line alist))   (goto-line (string-to-number (cdr val))))
      (if (setq val (assq 'column alist)) (move-to-column (string-to-number (cdr val)))))))

(defun magik-session-filter-action-cb-mf (proc socketname)
  "Magik has started a method_finder PROC and tell Emacs what the SOCKETNAME is."
  (setq magik-cb--mf-socket-synchronised socketname))

(defun magik-session-filter-action-cb-goto-method (proc str)
  "Magik session Filter Action interface for cb-goto-method."
  (magik-cb-goto-method str nil))

(defun magik-session-filter-action-magik-session-prompt-set (proc str)
  "Magik session Filter Action for setting `magik-session-prompt' variable."
  (with-current-buffer (buffer-name (process-buffer proc))
    (setq magik-session-prompt str)
    (magik-session-prompt-update-font-lock)))

(defun magik-session-filter-action-eval-elisp (proc str)
  "Magik session Filter Action that let Magik send Elisp code for Emacs evaluation."
  (with-current-buffer (get-buffer-create
                        (generate-new-buffer-name ; Be ultra careful to avoid multiple sessions eval'ing Elisp code!!!
                         (concat "*Magik Elisp eval*" (buffer-name (process-buffer proc)))))
    (erase-buffer)
    (insert str)
    (condition-case err
        (eval-region (point-min) (point-max))
      (error ;convert errors to messages
       (message (error-message-string err))))
    (kill-buffer (current-buffer))))

(magik-session-filter-register-action "\n" 'magik-session-filter-action-completion)
(magik-session-filter-register-action "p"  'magik-session-filter-action-deep-print)
(magik-session-filter-register-action "f"  'magik-session-filter-action-find-file)

(magik-session-filter-register-action "s"  'magik-session-filter-action-cb-mf)
(magik-session-filter-register-action "j"  'magik-session-filter-action-cb-goto-method)

(magik-session-filter-register-action "P"  'magik-session-filter-action-magik-session-prompt-set)
(magik-session-filter-register-action "E"  'magik-session-filter-action-eval-elisp)
(magik-session-filter-register-action "F"  'magik-session-filter-action-file-open)

(provide 'magik-session-filter)
;;; magik-session-filter.el ends here
