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

;; Adds additional functionality to magik-session-mode.
;; It currently adds the functionality to jump immediately from a traceback call stack line to the method definition on the prompt.
;; It will highlight the call stack lines and the output lines from methods such as .apropos and print_local_methods().
;; It uses the magik-cb-process in the background, so the magik-cb-process has to be started at least once.
;;
;; Usage:
;; via use-package: (use-package magik-session-extras :after magik-mode :hook magik-session-mode)
;; without use-package: (require 'magik-session-extras) (add-hook 'magik-session-mode-hook 'magik-session-extras)

;;; Code:

(require 'magik-mode)
(require 'magik-session)

(defface magik-session-traceback-call-stack-face
  '((t :inherit link))
  "Font Lock mode face used to highlight the call stack lines in a Traceback."
  :group 'magik-session-faces)

(defface magik-session-method-definition-face
  '((t :inherit magik-method-face
       :underline t))
  "Font Lock mode face used to highlight specific method lines.
Right now apropos() and print_local_methods()."
  :group 'magik-session-faces)

(defvar magik-session-extras-font-lock-keywords
  `((,(rx bol (one-or-more not-newline) "." (one-or-more (not numeric))
          (one-or-more whitespace) (syntax open-parenthesis)
          (one-or-more not-newline) ":" (one-or-more (or numeric not-newline))
          (syntax close-parenthesis) eol) . 'magik-session-traceback-call-stack-face)
	(,(rx bol (or "slot" "iter" "method" "class" "CORRUPT") " "
		  (one-or-more anychar) " in " (one-or-more not-newline) eol) . 'magik-session-method-definition-face))
  "Additional Font-lock Keywords for `magik-session-mode'.")

(defun magik-session-extras--activate ()
  "Activate the additional functionality for the `magik-session-mode'."
  (setq magik-session-font-lock-keywords (append magik-session-font-lock-keywords
                                                 magik-session-extras-font-lock-keywords))
  (advice-add 'magik-session-newline :around #'magik-session-extras-newline))

(defun magik-session-extras--deactivate ()
  "Deactivates the additional functionality for the `magik-session-mode'."
  (let ((font-lock-keywords magik-session-font-lock-keywords))
    (dolist (keyword magik-session-extras-font-lock-keywords font-lock-keywords)
      (setq font-lock-keywords (remove keyword font-lock-keywords)))
    (setq magik-session-font-lock-keywords font-lock-keywords)
    (advice-remove 'magik-session-newline #'magik-session-extras-newline)))

(define-minor-mode magik-session-extras
  "Optional additions to the `magik-session-mode'."
  :version "28.1"
  (when (bound-and-true-p magik-session-extras)
    (magik-session-extras--activate))
  (when (not (bound-and-true-p magik-session-extras))
    (magik-session-extras--deactivate)))

(defun magik-session-extras-newline (fn &rest args)
  "Jump to method definition based on the face of the text.
Or as fallback the normal newline behaviour with FN and ARGS."
  (cond ((eq (get-text-property (point) 'face) 'magik-session-traceback-call-stack-face)
         (magik-session-extras-cb-method-jump-traceback))
        ((eq (get-text-property (point) 'face) 'magik-session-method-definition-face)
         (magik-session-extras-cb-method-jump-method))
        ((apply fn args))))

(defun magik-session-extras-set-cb-process-var ()
  "Set the `magik-cb-process' variable.
So that the callback to `magik-cb-send-string' will work."
  (unless magik-cb-process
    (setq magik-cb-process (magik-cb-process (get-buffer (concat "*cb*" (buffer-name (current-buffer))))))
    (magik-cb-process)))

(defun magik-session-extras-go-to-method-definition (method-name exemplar-name)
  "Goto METHOD-NAME for EXEMPLAR-NAME."
  (magik-transmit-string
   (concat "method_finder.emacs_go_to_method_definition(\"" method-name "\",\"" exemplar-name "\")" "\n")
   (save-excursion
     (beginning-of-line)
     (magik-package-line))
   (lambda (f) (magik-function "load_file" f 'unset (or (buffer-file-name) 'unset)))
   (lambda (f) (magik-function "system.unlink" f 'false 'true))))

(defun magik-session-extras-cb-method-jump-traceback ()
  "Jump to the method definition.
Based on the `magik-session-traceback-call-stack-face'.
Using the `magik-cb-process' in the background."
  (interactive)
  (magik-session-extras-ensure-magik-code-loaded)
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
      (setq method-and-exemplar (split-string method-and-exemplar "\\.")
            method-name (cadr method-and-exemplar)
            exemplar-name (car method-and-exemplar)))
    (magik-session-extras-go-to-method-definition method-name exemplar-name)))

(defun magik-session-extras-cb-method-jump-method ()
  "Jump to the method definition.
Based on the `magik-session-method-definition-face'.
Using the `magik-cb-process' in the background."
  (interactive)
  (magik-session-extras-ensure-magik-code-loaded)
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
                                 (buffer-substring-no-properties start-of-line (point))
                                 " in " t
                                 "\\(slot\\|iter method\\|method \\|class \\(constant\\|variable\\)\\| \\)")
            exemplar-name (cadr method-and-exemplar)
            method-name (split-string (car method-and-exemplar) "\\((\\|\\(\\^<<\\|<<\\)\\)" t " "))
      (if (length> method-name 1)
          (if (string-match-p ")" (cadr method-name))
              (setq method-name (concat (car method-name) "()"))
            (setq method-name (concat (car method-name) "<<")))
        (setq method-name (car method-name))))
    (magik-session-extras-go-to-method-definition method-name exemplar-name)))

(defvar-local magik-session-extras-magik-code-loaded? nil
  "If t, `magik-session-extras-load-magik-code' is loaded in the Magik session.")

(defun magik-session-extras-ensure-magik-code-loaded ()
  "Load the `magik-session-extras-load-magik-code' in the current Magik session."
  (when (eq magik-session-extras-magik-code-loaded? nil)
    (magik-session-extras-load-magik-code)
    (setq magik-session-extras-magik-code-loaded? t)))

;; Inline magik code

(defun magik-session-extras-load-magik-code ()
  "Load the required magik code in to the `magik-session'.
To interact with the `magik-cb-mode'."
  (magik-transmit-string
   "_package sw
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

    _method method_finder.emacs_go_to_method_definition(method_name, class_name)
      _local method_info << _self.get_method_info_extra(method_name, class_name)
        !terminal!.write(\"E\", write_string(\"(magik-cb-send-string\", %space, %\", \"pr_source_file \", method_info[:method],
                         %space, method_info[:class], %newline, %\", %) ), \"\", %newline)
    _endmethod"
   (save-excursion
     (beginning-of-line)
     (magik-package-line))
   (lambda (f) (magik-function "load_file" f 'unset (or (buffer-file-name) 'unset)))
   (lambda (f) (magik-function "system.unlink" f 'false 'true))))

(provide 'magik-session-extras)
;;; magik-session-extras.el ends here
