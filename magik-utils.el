;;; magik-utils.el --- programming utils for the Magik lisp.

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
  (require 'sort))
(require 'cl-lib)
(require 'seq)

(defcustom magik-utils-by-default-prompt-buffer-p nil
  "Defines if prompting for an gis buffer is used by default.
This relates to the function `magik-utils-get-buffer-mode'.
Any non-nil value means that the user will be prompted with
the possible gis buffer options.  A value of nil makes it
use the DEFAULT value that had been passed in."
  :type 'boolean
  :group 'magik)

(defun barf-if-no-gis (&optional buffer process)
  "Return process object of Magik PROCESS.
Signal an error if no Magik process is running.
Use BUFFER as `magik-session-buffer'."
  (setq buffer  (or buffer magik-session-buffer)
        process (or process (get-buffer-process buffer)))
  (or process
      (error "There is no Magik Session running in buffer '%s'" buffer)))

(defun gsub (str from to)
  "Return a STR with any matches for the regexp, FROM, replaced by TO."
  (save-match-data
    (prog1
        (if (string-match from str)
            (concat (substring str 0 (match-beginning 0))
                    to
                    (gsub (substring str (match-end 0)) from to))
          str))))

(defun sub (str from to)
  "Return a STR with the first match for the regexp, FROM, replaced by TO."
  (save-match-data
    (prog1
        (if (string-match from str)
            (concat (substring str 0 (match-beginning 0))
                    to
                    (substring str (match-end 0)))
          str))))

(defun global-replace-regexp (regexp to-string)
  "Replace REGEXP with TO-STRING globally."
  (save-match-data
    (goto-char (point-min))
    (while
        (re-search-forward regexp nil t)
      (replace-match to-string nil nil))))

(defun magik-utils-locate-all-dominating-file (path file-name)
  "Find all ancestor paths of PATH containing FILE-NAME.
Uses `locate-dominating-file` repeatedly.
Returns a list of paths, or nil if none are found."
  (let ((paths '())
        (directory path))
    (while (setq directory (locate-dominating-file directory file-name))
      (push (expand-file-name file-name directory) paths)
      (setq directory (file-name-directory (directory-file-name directory))))
    (nreverse paths)))

(defun magik-utils-curr-word ()
  "Return the word (or part-word) before point as a string."
  (save-excursion
    (buffer-substring
     (point)
     (progn
       (skip-chars-backward "_!?a-zA-Z0-9")
       (point)))))

;; copied from emacs 18 because the emacs 19 find-tag-tag seems to be different.
(defun magik-utils-find-tag-tag (string)
  "Find tag for STRING."
  (let* ((default (magik-utils-find-tag-default))
         (spec (read-string
                (if default
                    (format "%s (default %s) " string default)
                  string))))
    (list (if (equal spec "")
              default
            spec))))

;;also copied
(defun magik-utils-find-tag-default ()
  "Find tag with default behaviour."
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (re-search-backward "\\sw\\|\\s_" nil t)
        (progn
          (forward-char 1)
          (buffer-substring (point)
                            (progn
                              (forward-sexp -1)
                              (while (looking-at "\\s'")
                                (forward-char 1))
                              (point))))
      nil)))

(defun magik-utils-substitute-in-string (string)
  "Return STRING with environment variable references replaced."
  (let ((substr string)
        start)
    (while (or (string-match "\$\\(\\sw+\\)" substr start)
               (string-match "\${\\(\\sw+\\)}" substr start)
               (string-match "%\\(\\sw+\\)%" substr start))
      (let ((env-name (substring substr (match-beginning 1) (match-end 1))))
        (setq start (match-end 0)) ;increment start position irrespective of a match
        (and (getenv env-name)
             (setq substr (replace-match (getenv env-name) t t substr 0)))))
    substr))

(defun which-file (filename &optional err path)
  "Return the full path when the given FILENAME name is in the PATH.
If PATH is not given then `load-path' is used.
nil is returned if no FILENAME found in PATH.

If ERR string is given, output as an error.  %s will be replaced with FILENAME."
  (let ((path (or path load-path))
        file)
    (while (and path
                (not (file-exists-p
                      (setq file (file-name-concat (file-name-as-directory (car path)) filename)))))
      (setq path (cdr path)))
    (cond (path file)
          (err  (error err filename))
          (t    nil))))

(defun magik-utils-file-name-display (file maxlen &optional sep)
  "Return shortened FILE name suitable for display.
Retaining head and tail portions of path."
  (let ((sep (or sep "..."))
        (dirsep "\\")
        components head tail c)
    (if (< (length file) maxlen)
        file
      (setq components (reverse (split-string file "[\\/]+")))
      ;;collect last three parts of path
      (push (pop components) tail)
      (push (pop components) tail)
      (push (pop components) tail)
      (setq maxlen (- maxlen (apply '+ (mapcar 'length tail)) (length tail) (length sep))
            components (reverse components))
      ;;now collect as many parts of the top of the path that we can.
      (while (and (setq c (car components)) (< (apply '+ (mapcar 'length head))
                                               (- maxlen (length head) (length c))))
        (push c head)
        (setq components (cdr components)))

      (mapconcat 'identity (append (reverse head) (list sep) tail) dirsep))))

(defun magik-utils-buffer-mode-list-predicate-p (predicate)
  "Return t if PREDICATE function or variable is true or PREDICATE is nil."
  (cond ((null predicate) t) ;no predicate given
        ((functionp predicate) (funcall predicate))
        ((boundp predicate) (symbol-value predicate))
        (t t)))

(defun magik-utils-buffer-visible-list (mode &optional predicate)
  "Return list (BUFFER . THIS-FRAME) for given Major mode MODE.
MODE may also be a list of modes.
Optional PREDICATE is either a function or a variable which must not return nil."
  (save-excursion
    (cl-loop for b in (buffer-list)
             do (set-buffer b)
             if (get-buffer-window b 'visible)
             if (member major-mode (if (listp mode) mode (list mode)))
             if (magik-utils-buffer-mode-list-predicate-p predicate)
             collect (cons (buffer-name)
                           (windowp (get-buffer-window b nil))))))

(defun magik-utils-buffer-mode-list (mode &optional predicate)
  "Return list of buffers with the given Major mode MODE.
MODE may also be a list of modes.
Optional PREDICATE is either a function or a variable which must not return nil."
  (save-excursion
    (cl-loop for b in (buffer-list)
             do (set-buffer b)
             if (member major-mode (if (listp mode) mode (list mode)))
             if (magik-utils-buffer-mode-list-predicate-p predicate)
             collect (buffer-name))))

(defun magik-utils-buffer-mode-list-sorted (mode &optional predicate sort-fn)
  "Return standardised sorted list of buffers with the given Major mode MODE.
Optional PREDICATE is either a function or a variable which must not return nil.
Optional SORT-FN overrides the default sort function, `string-lessp'.

This function is provided for the standardised sorting of Magik session buffers.
Since the introduction of having multiple Magik sessions with the key being
the Magik session buffer name, it is very useful to have a standardised sort of
Magik session buffers."
  (sort (magik-utils-buffer-mode-list mode predicate)
        (or sort-fn 'string-lessp)))

(defun magik-utils-get-buffer-mode (buffer mode prompt default &optional prefix-fn initial predicate)
  "Generalised function to return a suitable major MODE buffer to use.

Used for determining a suitable BUFFER using the following interface:
1. If Prefix arg is given and is integer,
   then use the buffer returned from the PREFIX-FN.
   The buffer list is filtered according to PREDICATE if given.
2. If Prefix arg is given and is not an integer,
   then PROMPT user with completing list of known buffers
   (optionally provide an INITIAL value).
   The buffer list is filtered according to PREDICATE if given.
3. If BUFFER is given use that.
4. Use the buffer displayed in the current frame,
   only PROMPT if more than one buffer in current frame is displayed
   and only list those in the current frame.
5. Use the buffer displayed in the some other frame,
   only PROMPT if more than one buffer in the other frames are displayed
   and only list those that are displayed in the other frames.
6. Use DEFAULT value, or PROMPT if `magik-utils-by-default-prompt-buffer-p'
   is not nil."
  (let* ((prefix-fn (or prefix-fn
                        #'(lambda (arg mode predicate)
                            (nth (1- arg)
                                 (reverse (magik-utils-buffer-mode-list-sorted mode predicate))))))
         (predicate (or predicate
                        #'(lambda ()
                            "This assumes buffer is set by `magik-utils-buffer-mode-list'"
                            (get-buffer-process (current-buffer)))))
         (prompt (concat prompt " "))
         (visible-buffs (magik-utils-buffer-visible-list mode predicate))
         (prompt-when-multiple-options
          #'(lambda (buffers)
              (and buffers
                   (setq buffer
                         (if (length= buffers 1) (car buffers)
                           (completing-read prompt buffers nil t initial)))
                   (not (equal buffer ""))
                   buffer))))
    (cond ((integerp current-prefix-arg) (funcall prefix-fn current-prefix-arg mode predicate))
          (current-prefix-arg (funcall prompt-when-multiple-options (magik-utils-buffer-mode-list mode predicate)))
          (buffer buffer)
          ((funcall prompt-when-multiple-options (seq-reduce #'(lambda (buffers buff)
                                                                 (if (cdr buff) (cons (car buff) buffers)))
                                                             visible-buffs nil))
           buffer)
          ((funcall prompt-when-multiple-options (mapcar 'car visible-buffs))
           (select-frame-set-input-focus
            (window-frame (get-buffer-window buffer 'visible)))
           buffer)
          (magik-utils-by-default-prompt-buffer-p (funcall prompt-when-multiple-options
                                                           (magik-utils-buffer-mode-list mode predicate)))
          (t default))))

(provide 'magik-utils)
;;; magik-utils.el ends here
