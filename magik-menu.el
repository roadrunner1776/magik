;;; magik-menu.el --- set the Magik menus.

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

(require 'magik-aliases)
(require 'magik-version)
(require 'magik-mode)
(require 'magik-shell)
(require 'magik-shell-filter)
(require 'magik-cb)

(require 'easymenu)

(defconst magik-menu-main-menu
  `(,"Magik"
    (,"Alias Files")
    "---"
    [,"Select Environment"            magik-version-selection
     :active t
     :keys "f2 s"]
    [,"Run/Goto session"              magik-shell
     :active t
     :keys "f2 z"]
    [,"Start new session"             magik-shell-new-buffer
     :active t
     :keys "C-u f2 z"]
    (,"Magik processes")
    "---"
    (,"Class Browser")
    (,"Class Browser Processes")
    "---"
    (,"External Shell Processes")
    [,"List Processes"    list-processes t]
    "---"
    [,"Customize" sw-customize
     :active t] ;; :key-sequence nil
    (,"Help")))

(defconst magik-menu-cb-menu
  `(,"Class Browser"
    [,"Run/Goto Class Browser"       magik-cb
     :active t
     :keys "f3 f3"]
    [,"Start New Class Browser"      magik-cb-new-buffer
     :active t
     :keys "C-u f3 f3"]
    [,"Paste Method in CB"           magik-cb-paste-method
     :active t
     :keys "f3 m"]
    [,"Paste Class in CB"            magik-cb-paste-class
     :active t
     :keys "f3 c"]
    [,"Clear Method and Class in CB" magik-cb-and-clear
     :active t
     :keys "f3 /"]
    "---"
    [,"Jump to Source"               magik-cb-jump-to-source
     :active t
     :keys "f3 j"]
    "---"
    [,"Customize"                    magik-cb-customize
     :active t] ;; :key-sequence nil
    [,"Help"                         magik-cb-help
     :active t
     :keys "f3 ?"]))

;;;###autoload
(defun magik-menu-set-menus ()
  "Setup main Magik menus."
  (easy-menu-change (list "Tools")
		    "Magik"
		    (cdr magik-menu-main-menu)
		    "Search Files (Grep)...")
  (easy-menu-change (list "Tools" "Magik")
		    "Class Browser"
		    (cdr magik-menu-cb-menu))

  ;; Due to a minor bug in easy-menu-change, have to set the "No Process" etc.
  ;; strings separately
  (easy-menu-change (list "Tools" "Magik")
		    "Magik Processes"
		    (list "No Processes"))
  (easy-menu-change (list "Tools" "Magik")
		    "Class Browser Processes"
		    (list "No Processes"))
  (easy-menu-change (list "Tools" "Magik")
		    "Shell Processes"
		    (list "No Processes"))

  (easy-menu-change (list "Tools") "--" nil "Search Files (Grep)...")

  (and (fboundp 'magik-aliases-update-sw-menu)
       (magik-aliases-update-sw-menu)))

(provide 'magik-menu)
;;; magik-menu.el ends here
