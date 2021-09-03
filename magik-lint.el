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

(defcustom magik-lint-jar-file (expand-file-name (concat user-emacs-directory "magik-lint/magik-lint-0.4.0.jar"))
  "Location of the magik-lint jar file."
  :group 'magik
  :type  '(choice (file)
                  (const nil)))

(flycheck-def-config-file-var flycheck-magik-lintrc magik-lint-java ".magiklint")

(flycheck-def-args-var flycheck-magik-lint-args magik-lint-java)

(flycheck-def-args-var flycheck-magik-lint-java-args magik-lint-java)

(flycheck-define-checker magik-lint-java
  "A Magik syntax checker and validator using the magik-lint utility.

See URL `https://github.com/StevenLooman/sonar-magik/tree/master/magik-lint'."
  :command ("java"
            (eval flycheck-magik-lint-java-args)
            "-jar" (eval (expand-file-name magik-lint-jar-file))
            (eval flycheck-magik-lint-args)
            (config-file "--rcfile" flycheck-magik-lintrc)
            "--max-infractions" (eval (number-to-string flycheck-checker-error-threshold))
            "--msg-template" "\"${path}:${line}:${column}: (${category}) ${msg}\""
            "--column-offset" "+1"
            source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": (Critical) " (message) line-end)
   (error line-start (file-name) ":" line ":" column ": (Major) " (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": (Minor) " (message) line-end))
  :modes (magik-mode))

(unless (and (eq system-type 'windows-nt)
             (funcall flycheck-executable-find "java"))
  (setq flycheck-magik-lint-java-executable (executable-find (substitute-in-file-name "$JAVA_HOME/bin/java"))))

(add-to-list 'flycheck-checkers 'magik-lint-java 'append)

(provide 'magik-lint)
;;; magik-lint.el ends here
