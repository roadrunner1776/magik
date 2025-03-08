;;; magik-cb-ac.el --- Magik Classbrowser Autocomplete Support  -*- lexical-binding: t; -*-

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

(require 'auto-complete)

;; A U T O - C O M P L E T E
;; _________________________

(defun magik-cb-ac-filter (p s)
  "Process data coming back from the CB auto-complete buffer."
  (with-current-buffer (process-buffer p)
    (unwind-protect
        (let ((buffer-read-only nil)
              (coding-system-for-read magik-cb-coding-system)
              fn)
          (setq magik-cb-filter-str (concat magik-cb-filter-str s))
          (save-match-data
            (setq fn (cond ((string-match "\C-e" magik-cb-filter-str)
                            'magik-cb-ac-candidate-methods)
                           ((string-match "\C-c" magik-cb-filter-str)
                            'magik-cb-ac-candidate-classes)
                           (t
                            nil))))
          (setq magik-cb-filter-str ""
                magik-cb--ac-candidates (if fn
                                            (progn
                                              (insert-file-contents (magik-cb-temp-file-name p) nil nil nil t)
                                              (funcall fn)))))
      (setq magik-cb-filter-str ""
            magik-cb--ac-candidates (if (eq magik-cb--ac-candidates 'unset) nil magik-cb--ac-candidates)))))

(defun magik-cb-ac-start-process ()
  "Start a Class Browser process for auto-complete-mode.
Stores process object in `magik-cb-ac-process'."

                                        ; TODO get-gis-buffer
  (setq magik-cb-ac-process (magik-cb-get-process-create "*cb-ac*" 'magik-cb-ac-filter "*gis*" nil)))

(defun magik-cb-ac-candidate-methods ()
  "Return candidate methods matching `ac-prefix' from Method finder output."
  ;;TODO combine method definition with its signature.
  (let ((method (car ac-prefix))
        (class (cdr ac-prefix))
        (ac-limit ac-limit))
    (setq method
          (if (zerop (length method))
              "\\sw"
            (regexp-quote method)))
    (let ((i 0)
          (regexp (concat "^\\(" method "\\S-*\\)" magik-cb-in-keyword "\\(\\S-+\\)\\s-+\\(.*\\)\n\\(.*\n\\)\n\\(\\( +##.*\n\\)*\\)")) ; capture item and comments
          candidate
          classify
          args
          documentation
          candidates)
      (goto-char (point-min))
      (save-match-data
        (while (and (or (null ac-limit) (< i ac-limit))
                    (re-search-forward regexp nil t))
          (setq candidate (match-string-no-properties 1)
                class     (match-string-no-properties 2)
                classify  (match-string-no-properties 3)
                args      (magik-cb-method-args (match-beginning 4))
                documentation (match-string-no-properties 5))
          (put-text-property 0 (length candidate)
                             'document
                             (magik-cb-method-docstring class candidate args classify documentation)
                             candidate)
          (if (member candidate candidates)
              nil ; already present
            (setq candidates (append (list candidate) candidates)
                  i (1+ i)))))
      (nreverse candidates))))

(defun magik-cb-method-args (pt)
  "Return method arguments from Class Browser at point PT."
  (save-excursion
    (goto-char pt)
    (save-match-data
      (let ((case-fold-search nil)
            optional
            args
            gather
            opt
            name)
        (if (looking-at "$")
            nil ; No arguments
          (forward-char 1) ; space
          (while (not (looking-at "$"))
            (setq pt (point))
            (cond ((looking-at "\\(OPT \\)?GATH \\(.*\\)")
                   (setq gather (buffer-substring-no-properties (match-beginning 2) (match-end 2)))
                   (goto-char (match-end 0)))
                  ((looking-at "OPT ")
                   (setq opt t)
                   (goto-char (match-end 0)))
                  ((> (skip-syntax-forward "w_") 0) ; found argument (may contain _ which may be classed as symbols.)
                   (setq name (list (buffer-substring-no-properties pt (point))))
                   (if opt
                       (setq optional (append optional name))
                     (setq args (append args name)))
                   (if (eq (following-char) ? )
                       (forward-char 1)))
                  (t ;catch all error
                   (message "Found unrecognised character at %d in %s" (point) (current-buffer))
                   (goto-char (end-of-line))))))
        (list args optional gather)))))

;;TODO extract out Magik method signature from callsification and documentation processing.
(defun magik-cb-method-docstring (class candidate args classify documentation)
  "Return method documentation string."
  (let* ((required (elt args 0))
         (optional (elt args 1))
         (gather (elt args 2))
         (method-signature (magik-method-name-type candidate))
         (method (car method-signature))
         (signature (cdr method-signature))
         (signature-p (> (length signature) 0))
         assignment)

    ;;Standardise classification string
    (cond ((zerop (length classify))
           ;;do nothing
           nil)
          ((equal (substring classify 0 1) "A")
           (setq classify (concat "Advanced" (substring classify 1))))
          ((equal (substring classify 0 1) "B")
           (setq classify (concat "Basic" (substring classify 1))))
          (t
           ;;do nothing
           nil))

    ;; Handle << assignment like signatures - take first required argument
    (if (and signature-p (equal (substring signature -1) "<"))
        (setq assignment (car required)
              required (cdr required)))
    (if documentation
        (while (string-match "^ +## " documentation)
          (setq documentation (replace-match "" nil nil documentation))))
    (if gather
        ;; prefix rest args with _gather and convert to a list.
        (setq gather (list (concat "_gather " gather))))
    (if optional
        ;; prefix first optional arg with _optional
        (setcar optional (concat "_optional " (car optional))))
    ;; TODO handle arrays [], []<< etc.
    (concat
     (cond ((equal class "<condition>")
            (concat "raise(:" method (if required ",\n      ")
                    (mapconcat (lambda (r) (concat ":" r ", <value>")) required ",\n      ")
                    ")\n"
                    "  " classify
                    "\n"))
           ((equal class "<global>")
            ;; Globals are either procedures with arguments or dynamics.
            (let* ((args-string (mapconcat 'identity (append required optional gather) ", "))
                   (argsp (not (equal args-string ""))))
              (concat method
                      (if argsp "(")
                      args-string
                      (if argsp ")")
                      "\n"
                      "  " classify
                      "\n")))
           ((not signature-p)
            (concat method
                    "\n"
                    "  " classify
                    "\n"))
           ((equal (substring signature 0 1) "(")
            (let ((args-string (mapconcat 'identity (append required optional gather) ", ")))
              (concat method "("
                      args-string
                      (substring signature 1) ;; appends rest of signature. ), )<< and )^<<
                      assignment ;; allows for ()<< and ()^<< too.
                      "\n"
                      "  " classify
                      "\n")))
           (assignment ;; handle << and ^<<
            (concat method
                    signature
                    assignment
                    "\n"
                    "  " classify
                    "\n"))
           (t "UNKNOWN??\n\n"))
     documentation)))

(defun magik-cb-ac-candidate-classes ()
  "Return candidate classes from Method finder output."
                                        ;TODO handle package definitions?
  (let ((i 0)
        (regexp (concat "\\(\\S-+:\\)\\(\\S-+\\)")) ; capture class name and its package
        candidate
        candidates)
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward regexp nil t)
        (setq candidate (match-string-no-properties 2))
        (save-match-data
          (if (member candidate candidates)
              nil ; already present
            (setq candidates (append (list candidate) candidates)
                  i (1+ i)))))
      (nreverse candidates))))

(defun magik-cb-ac-method-candidates ()
  "Return list of methods for a class matching AC-PREFIX for auto-complete mode.
AC-PREFIX is of the form \"CLASS\".\"METHOD_NAME_PREFIX\""
  (let ((magik-cb--ac-candidates 'unset) ; use 'unset symbol since nil is also a valid return value.
        (ac-prefix ac-prefix)
        (ac-limit (or ac-limit 1000000))
        class method character)
    (save-match-data
      (cond ((null magik-cb-ac-process)
             (setq magik-cb--ac-candidates nil))
            ((not (string-match "\\(\\S-+\\)\\.\\(.*\\)" ac-prefix))
             (setq magik-cb--ac-candidates nil))
            (t
             (setq  class (match-string-no-properties 1 ac-prefix)
                    method (match-string-no-properties 2 ac-prefix)
                    character (if (equal method "") method (substring method 0 1))
                    ac-prefix (cons method class))
             (process-send-string magik-cb-ac-process
                                  (concat "method_name ^" character "\n"
                                          "unadd class \nadd class " class "\n"
                                          "method_cut_off " (number-to-string ac-limit) "\n"
                                          "override_flags\nshow_classes\nshow_args\nshow_comments\nprint_curr_methods\n"))
             (while (and (eq magik-cb--ac-candidates 'unset)
                         (magik-cb-is-running nil magik-cb-ac-process))
               (sleep-for 0.1))
             (setq magik-cb--ac-candidates (append (list (concat " " class "." character)) magik-cb--ac-candidates)))))
    magik-cb--ac-candidates))

(defun magik-cb-ac-class-candidates ()
  "Return list of classes matching AC-PREFIX for auto-complete mode."
  (let ((magik-cb--ac-candidates 'unset)) ; use 'unset symbol since nil is also a valid return value.
    (cond ((null magik-cb-ac-process)
           (setq magik-cb--ac-candidates nil))
          (t
           (process-send-string magik-cb-ac-process
                                (concat "dont_override_flags\npr_family " ac-prefix "\n"))
           (while (and (eq magik-cb--ac-candidates 'unset)
                       (magik-cb-is-running nil magik-cb-ac-process))
             (sleep-for 0.1))))
    magik-cb--ac-candidates))

(provide 'magik-cb-ac)
;;; magik-cb-ac.el ends here
