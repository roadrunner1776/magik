(require 'magik-mode)
(require 'magik-session)
(require 'server)

(defgroup magik-mods-group nil
  "Group for customization"
  :prefix "magik-mods-")

(defface magik-mods-traceback-face
  '((t :inherit link
       ;; :foreground "#999900"
       ))
  "Face for traceback lines"
  :group 'magik-mods-group )

(defface magik-mods-apropos-method-face
  '((t :inherit link
       :foreground "#50fa7b"
       ))
  "Face for apropos method lines"
  :group 'magik-mods-group )

(defvar magik-mods-keywords '((".+\..+ \(.+\:[0-9]+\)" . 'magik-mods-traceback-face)
			      ("\\(slot\\|iter\\|method\\|class\\) .+ in .+" . 'magik-mods-apropos-method-face))
  "Keywords for magik-mods-minor-mode highlighting.")
;;(font-lock-add-keywords nil '(("\\(iter\\|method\\|class\\) .+ in .+" . 'magik-mods-apropos-method-face)))
(defun magik-mods-session-newline (arg)
  (interactive "*P")
  ;;if its a link do a cb jump instead of newline
  (cond ((eq (get-text-property (point) 'face) 'magik-mods-traceback-face)
	 (magik-mods-traceback-cb-jump))
	((eq (get-text-property (point) 'face) 'magik-mods-apropos-method-face)
	 (magik-mods-print-method-cb-jump))
	((magik-session-newline arg)))
  )

(defun magik-mods-mouse-set-point (arg)
  (interactive "*P")
  ;;if its a link do a cb jump instead of mouse-set-point
  (cond ((eq (get-text-property (point) 'face) 'magik-mods-traceback-face)
	 (magik-mods-traceback-cb-jump))
	((eq (get-text-property (point) 'face) 'magik-mods-apropos-method-face)
	 (magik-mods-print-method-cb-jump))
	((mouse-set-point arg)))
  )

(defun magik-mods-set-cb-process-var ()
  (unless magik-cb-process
    (setq magik-cb-process (magik-cb-process (get-buffer (concat "*cb*" (buffer-name (current-buffer))))))
    (magik-cb-process))
  )

(defun magik-mods-traceback-cb-jump ()
  (interactive)
  (magik-mods-set-cb-process-var)
  (let (class_name
	method_name
	method_and_class
	start_of_file_point
	magik-proc)
    (save-excursion
      (end-of-line)
      (search-backward-regexp "\(.+\:[0-9]+\)")
      (setq start_of_file_point (point))
      (beginning-of-line)
      (setq method_and_class (car
			      (split-string
			       (buffer-substring-no-properties (point) start_of_file_point) " " t)))
      (setq method_and_class (split-string method_and_class "\\."))
      (setq method_name (cadr method_and_class))
      (setq class_name (car method_and_class))
      )
    (setq magik-proc (magik-transmit-string (concat "method_finder.emacs_jump_to_method_source(\"" method_name "\",\"" class_name "\")" "\n")
					    (save-excursion 
					      (beginning-of-line) 
					      (magik-package-line)) 
					    (lambda (f) (magik-function "load_file" f 'unset (or (buffer-file-name) 'unset))) 
					    (lambda (f) (magik-function "system.unlink" f 'false 'true)) 
					    ))
    )
  )

(defun magik-mods-print-method-cb-jump ()
  (interactive)
  (magik-mods-set-cb-process-var)
  (let (class_name
	method_name
	method_and_class
	start_of_line
	magik-proc)
    (save-excursion
      (beginning-of-line)
      (setq start_of_line (point))
      (end-of-line)
      (setq method_and_class (split-string
			      (buffer-substring-no-properties start_of_line (point))
			      " in " t
			      "\\(slot\\|iter method\\|method\\|class \\(constant\\|variable\\)\\| \\)"))
      (setq class_name (cadr method_and_class))
      (setq method_name (split-string (car method_and_class) "\\((\\|\\(\\^<<\\|<<\\)\\)" t " "))
      (if (length> method_name 1)
	  (if (string-match-p ")" (cadr method_name))
	      (setq method_name (concat (car method_name) "()"))
	    (setq method_name (concat (car method_name) "<<"))
	    )
	(setq method_name (car method_name))
	)
      )
    (setq magik-proc (magik-transmit-string (concat "method_finder.emacs_jump_to_method_source(\"" method_name "\",\"" class_name "\")" "\n")
					    (save-excursion 
					      (beginning-of-line) 
					      (magik-package-line)) 
					    (lambda (f) (magik-function "load_file" f 'unset (or (buffer-file-name) 'unset))) 
					    (lambda (f) (magik-function "system.unlink" f 'false 'true)) 
					    ))
    )
  )

;;this magik code loading needs to be more dynamic in the future maybe inline magik code in elisp 
(defvar magik-mods-magik-code-loaded? nil
  "Shows if the magik-mods-code is loaded in current magik session")
(defvar magik-mods-magik-code-location "./magik-mods.magik")

(define-minor-mode magik-mods-mode
  "Experimental mods for magik mode"
  :lighter " magik mods"
  :version "28.1"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "<RET>") 'magik-mods-session-newline)
	    (define-key map (kbd "<mouse-1>") 'magik-mods-mouse-set-point)
	    map)
  :group 'magik-mods-group
  (when (bound-and-true-p magik-mods-mode)
    (setq magik-session-font-lock-keywords (append magik-session-font-lock-keywords magik-mods-keywords))
    ;; (or (eq (server-running-p) t)
    ;; 	(server-start)))
    )
  (when (not (bound-and-true-p magik-mods-mode))
    (font-lock-remove-keywords nil magik-mods-keywords)
    (font-lock-fontify-buffer))
  )

(defun magik-mods-load-magik-code ()
  (when (eq magik-mods-magik-code-loaded? nil)
    (setq magik-proc (magik-transmit-string (concat "_block\n"
						    "load_file(\"" magik-mods-magik-code-location "\"" ")\n"
						    "_endblock \n")
					    (save-excursion 
					      (beginning-of-line) 
					      (magik-package-line)) 
					    (lambda (f) (magik-function "load_file" f 'unset (or (buffer-file-name) 'unset))) 
					    (lambda (f) (magik-function "system.unlink" f 'false 'true)) 
					    ))
    (setq magik-mods-magik-code-loaded? t)
    )
  )

;;autoload
(add-hook 'magik-session-mode-hook 'magik-mods-mode)

(add-hook 'magik-cb-mode-hook 'magik-mods-load-magik-code)

(provide 'magik-mods-mode)
