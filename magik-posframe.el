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



