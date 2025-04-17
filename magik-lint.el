;;; magik-lint.el --- Flycheck support for Magik     -*- lexical-binding: t; -*-

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

(defun magik-lint--latest-version ()
  "Return latest version of the magik linter."
  (ignore-errors
    (with-current-buffer (url-retrieve-synchronously "https://api.github.com/repos/StevenLooman/magik-tools/releases/latest" 'silent 'inhibit-cookies)
      (goto-char (point-min))
      (re-search-forward "^$")
      (delete-region (point) (point-min))
      (let ((response (json-read)))
        (cdr (assoc 'tag_name response))))))

(defcustom magik-lint-jar-file-version
  (or (magik-lint--latest-version) "0.10.1")
  "Version of magik-lint to use."
  :group 'magik
  :type 'string)

(defcustom magik-lint-jar-file "magik-lint/magik-lint-%s.jar"
  "Location of the magik-lint jar file."
  :group 'magik
  :type  '(choice (file)
                  (const nil)))

(defun magik-lint--jar-file ()
  "Expanded magik lint jar file name."
  (expand-file-name (if magik-lint-jar-file-version
                        (format magik-lint-jar-file magik-lint-jar-file-version)
                      magik-lint-jar-file)
                    user-emacs-directory))

(flycheck-def-config-file-var flycheck-magik-lintrc magik-lint-java ".magiklint")

(flycheck-def-args-var flycheck-magik-lint-args magik-lint-java)

(flycheck-def-args-var flycheck-magik-lint-java-args magik-lint-java)

(flycheck-define-checker magik-lint-java
  "A Magik syntax checker and validator using the magik-lint utility.

See URL `https://github.com/StevenLooman/sonar-magik/tree/master/magik-lint'."
  :command ("java"
            (eval flycheck-magik-lint-java-args)
            "-jar" (eval (magik-lint--jar-file))
            (eval flycheck-magik-lint-args)
            (config-file "--rcfile" flycheck-magik-lintrc)
            "--max-infractions" (eval (number-to-string flycheck-checker-error-threshold))
            "--msg-template" "\"${path}:${line}:${column}: (${category}) ${msg} (${symbol})\""
            "--column-offset" "+1"
            source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ": (Critical) " (message) line-end)
   (error line-start (file-name) ":" line ":" column ": (Major) " (message) line-end)
   (warning line-start (file-name) ":" line ":" column ": (Minor) " (message) line-end))
  :modes (magik-mode magik-ts-mode))

(when (and (eq system-type 'windows-nt)
           (not (funcall flycheck-executable-find "java")))
  (setq flycheck-magik-lint-java-executable (or (funcall flycheck-executable-find (expand-file-name "bin/java" (getenv "JAVA_HOME"))) "java")))

(if (file-exists-p (magik-lint--jar-file))
    (add-to-list 'flycheck-checkers 'magik-lint-java 'append)
  (warn "magik-lint executable not found: %s; please download from https://github.com/StevenLooman/magik-tools/releases/latest" (magik-lint--jar-file)))

(provide 'magik-lint)
;;; magik-lint.el ends here
