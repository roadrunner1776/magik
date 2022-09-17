;;; magik-session-extras.el --- Optional additions to the magik-session-mode

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

;; commentary goes here
;; enable these functionality via (require 'magik-session-extras)
;; use-package method

;;; Code:

(require 'magik-mode)
(require 'magik-session)

(defface magik-session-traceback-call-stack-face
  '((t :inherit link))
  "*Font-lock Face to highlight the call stack lines when displaying a Traceback."
  :group 'magik)

(defface magik-session-method-definition-face
  '((t :inherit font-lock-function-name-face
       :underline t))
  "*Font-lock Face to highlight the method lines outputted by apropos() and print_local_methods()."
  :group 'magik)

(defvar magik-session-extras-font-lock-keywords
  '(("^.+\.[^0-9]+ \(.+\:[0-9]+\)" . 'magik-session-traceback-call-stack-face)
	("^\\(slot\\|iter\\|method\\|class\\) .+ in .+" . 'magik-session-extras-method-definition-face))
  "Additional Font-lock Keywords for magik-session-mode.")

(defun magik-session-extras ()
  (when (bound-and-true-p magik-session-extras)
    (setq magik-session-font-lock-keywords (append magik-session-font-lock-keywords
												   magik-session-extras-font-lock-keywords))
	;;advice-ad extras-newline method to keyboard-enter
    )
  )

(defun magik-session-extras-newline (arg)
  (interactive "*P")
  ;;if its a link do a cb jump instead of newline
  (cond ((eq (get-text-property (point) 'face) 'magik-session-traceback-call-stack-face)
		 (magik-session-extras-cb-method-jump-traceback))
		((eq (get-text-property (point) 'face) 'magik-session-extras-method-definition-face)
		 (magik-session-extras-cb-method-jump-method))
	((magik-session-newline arg)))
  )

(defun magik-session-extras-set-cb-process-var ()
  ;; testen of dit nou echt nodig is...
  (unless magik-cb-process
    (setq magik-cb-process (magik-cb-process (get-buffer (concat "*cb*" (buffer-name (current-buffer))))))
    (magik-cb-process))
  )

(defun magik-session-extras-go-to-method-definition (method-name exemplar-name)
  (magik-transmit-string (concat "method_finder.emacs_jump_to_method_source(\"" method-name "\",\"" exemplar-name "\")" "\n")
					     (save-excursion 
					       (beginning-of-line) 
					       (magik-package-line)) 
					     (lambda (f) (magik-function "load_file" f 'unset (or (buffer-file-name) 'unset))) 
					     (lambda (f) (magik-function "system.unlink" f 'false 'true)) 
					     )
  )

(defun magik-session-extras-cb-method-jump-traceback ()
  (interactive)
  (magik-session-extras-set-cb-process-var)
  (let (exemplar-name
		method-name
		method-and-exemplar
		start-of-file-point)
    (save-excursion
      (end-of-line)
      (search-backward-regexp "\(.+\:[0-9]+\)")
      (setq start-of-file-point (point))
      (beginning-of-line)
      (setq method-and-exemplar (car
								 (split-string
								  (buffer-substring-no-properties (point) start-of-file-point) " " t)))
      (setq method-and-exemplar (split-string method-and-exemplar "\\."))
      (setq method-name (cadr method-and-exemplar))
      (setq exemplar-name (car method-and-exemplar))
      )
	(magik-session-extras-go-to-method-definition (method-name exemplar-name))
    )
  )

(defun magik-session-extras-cb-method-jump-method ()
  (interactive)
  (magik-session-extras-set-cb-process-var)
  (let (exemplar-name
		method-name
		method-and-exemplar
		start-of-line)
    (save-excursion
      (beginning-of-line)
      (setq start-of-line (point))
      (end-of-line)
      (setq method-and-exemplar (split-string
								 (buffer-subastring-no-properties start_of_line (point))
								 " in " t
								 "\\(slot\\|iter method\\|method\\|class \\(constant\\|variable\\)\\| \\)"))
      (setq exemplar-name (cadr method-and-exemplar))
      (setq method-name (split-string (car method-and-exemplar) "\\((\\|\\(\\^<<\\|<<\\)\\)" t " "))
      (if (length> method-name 1)
		  (if (string-match-p ")" (cadr method-name))
			  (setq method-name (concat (car method-name) "()"))
			(setq method-name (concat (car method-name) "<<"))
			)
		(setq method-name (car method-name))
		)
      )
	(magik-session-extras-go-to-method-definition (method-name exemplar-name))
    )
  )

(defvar-local magik-session-extras-magik-code-loaded? nil
  "Shows if the magik-session-extras-code is loaded in the current magik session")

(defun magik-session-extras-ensure-magik-code-loaded ()
  (when (eq magik-session-extras-magik-code-loaded? nil)
	(magik-session-extras-load-magik-code)
    (setq magik-session-extras-magik-code-loaded? t)
    )
  )

;; Autoload

(add-hook 'magik-session-mode-hook 'magik-session-extras-ensure-magik-code-loaded)

;; Inline magik code

(defun magik-session-extras-load-magik-code ()
  (magik-transmit-string
   "_package sw
    $
    
    _method method_finder.get_method_info_extra(method_name, class_name)
    	_local method_info << property_list.new()
    	_for package_name, package_data _over sw:package.all_packages.fast_keys_and_elements()
    	_loop
    		_local exemplar << package_data.get_value(class_name.as_symbol())
    		_if exemplar.responds_to?(method_name.as_symbol(), _true)
    		_then
    			method_info[:method] << method_name
    			real_pkg_name << exemplar.exemplar_global.package.name
    			method_info[:class] << write_string(real_pkg_name, %:, class_name)
    			_leave
    		_endif
    	_endloop
    	_return method_info
    _endmethod
    $
    
    _method method_finder.emacs_jump_to_method_source(method_name, class_name)
    	_local method_info << _self.get_method_info_extra(method_name, class_name)
    	!terminal!.write(\"E\",write_string(\"(magik-cb-send-string\", %space, %\", \"pr_source_file \", method_info[:method], %space,
    															  method_info[:class], \"\n\", %\", %)),\"\", %newline)
    _endmethod
    $"
   (save-excursion 
	 (beginning-of-line) 
	 (magik-package-line)) 
   (lambda (f) (magik-function "load_file" f 'unset (or (buffer-file-name) 'unset))) 
   (lambda (f) (magik-function "system.unlink" f 'false 'true)) 
   )
  )

(provide 'magik-session-extras)
;;; magik-session-extras.el ends here
