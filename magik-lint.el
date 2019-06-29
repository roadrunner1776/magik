;;; magik-lint.el --- Flycheck support for Magik

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

(require 'flycheck)

(defcustom magik-lint-jar-file (expand-file-name (concat user-emacs-directory "magik-lint/magik-lint-0.1.0.jar"))
  "Location of the magik-lint jar file."
  :group 'magik
  :type  '(choice (file)
		  (const nil)))

(unless (and (eq system-type 'windows-nt)
	     (flycheck-default-executable-find "java"))
  (add-to-list 'exec-path (expand-file-name (substitute-in-file-name "$JAVA_HOME/bin/")) 'append))

(flycheck-define-checker magik-magiklint
  "A Magik syntax checker and validator using the magik-lint utility."
  :command ("java"
	    "-jar" (eval (expand-file-name magik-lint-jar-file))
	    "--msg-template" "\"${path}:${line}:${column}: (${category}) ${msg}\""
	    source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": (Major) " (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": (Minor) " (message) line-end))
  :modes (magik-mode))

(add-to-list 'flycheck-checkers 'magik-magiklint 'append)

(provide 'magik-lint)
;;; magik-lint.el ends here
