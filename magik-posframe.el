(defvar magik-posframe-buffer "magik-posframe"
  "Posframe buffer")

(defvar magik-posframe-source-file ""
  "")

(defun magik-posframe-show(buffer-text source-file)
  "Show posframe for magik definition"
  (interactive)
  (with-current-buffer (get-buffer-create magik-posframe-buffer)
    (erase-buffer)
    (insert buffer-text);"_method object.test\n\twrite(\"test posframe method\")\n_endmethod\n$")
    (magik-mode))
  (setq magik-posframe-source-file source-file)
  (when (posframe-workable-p)
    (posframe-show magik-posframe-buffer
		   :border-width 1
		   :border-color "lightgreen"
                   :position (point)))
  )

;;;(advice-add 'keyboard-quit :around #'magik-posframe-hide)
;;; (advice-add 'magik-mods-session-newline :around #'magik-posframe-goto-file)

(defun magik-posframe-hide (fn &rest args)
  "hide posframe"
  (when magik-mods-mode
    (posframe-hide magik-posframe-buffer))
  (apply fn args)
  )

(defun magik-posframe-goto-file ();(fn &rest args)
  (interactive)
  (when magik-mods-mode
    (let ((filename (split-string magik-posframe-source-file "::")))
      (goto-line (string-to-number (cadr filename))
		 (find-file (car filename)))
    (posframe-hide-all))
    )
  ;; (apply fn args)
  )

(defun magik-posframe-show-print-method ()
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
    (setq magik-proc (magik-transmit-string (concat "method_finder.posframe_popup(\"" method_name "\",\"" class_name "\")" "\n")
					    (save-excursion 
					      (beginning-of-line) 
					      (magik-package-line)) 
					    (lambda (f) (magik-function "load_file" f 'unset (or (buffer-file-name) 'unset))) 
					    (lambda (f) (magik-function "system.unlink" f 'false 'true)) 
					    ))
    )
  )
