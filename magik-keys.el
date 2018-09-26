;;; magik-keys.el --- bind all the Magik keys, menus and mouse actions.

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

(eval-when-compile
  (require 'cl)
  (require 'magik-utils)
  (require 'magik-mode)
  (require 'magik-shell)
  (require 'magik-cb))

(require 'magik-menu)

(defun magik-customize ()
  "Open Customization buffer for Smallword Development."
  (interactive)
  (customize-group 'magik))

;;;###autoload
(defun magik-global-bindings ()
  "Setup the default Smallworld key bindings."

  ;; ---------------------- top-level globals --------------------------

  (global-set-key [f6]    'magik-copy-method)
  (global-set-key [f7]    'magik-transmit-method)
  (global-set-key [f8]    'magik-transmit-region)
  (global-set-key [f9]    'magik-mark-method)

  ;; ------------------- F2 globals ----------------------

  (global-unset-key (kbd "<f2>"))

  (global-set-key (kbd "<f2> <f7>")   'magik-transmit-method)
  (global-set-key (kbd "<f2> <f8>")   'magik-transmit-region)
  (global-set-key (kbd "<f2> RET")     'magik-transmit-thing)
  (global-set-key (kbd "<f2> #")      'magik-comment-region)
  (global-set-key (kbd "<f2> ESC #")    'magik-uncomment-region)
  (global-set-key (kbd "<f2> b")      'magik-transmit-buffer)
  (global-set-key (kbd "<f2> h")      'magik-heading)
  (global-set-key (kbd "<f2> m")      'magik-transmit-method)
  (global-set-key (kbd "<f2> r")      'magik-transmit-region)
  (global-set-key (kbd "<f2> q")      'fill-magik-public-comment)
  (global-set-key (kbd "<f2> t")      'magik-trace-curr-statement)

  (global-set-key (kbd "<f2> SPC")    'explicit-electric-magik-space)
  (global-set-key (kbd "<f2> x")      'deep-print)

  (global-set-key (kbd "<f2> <f1>")   'sw-help-keys)
  (global-set-key (kbd "<f2> [")      'toggle-debug)
  (global-set-key (kbd "<f2> TAB")    'hippie-expand)
  (global-set-key (kbd "<f2> e")      'electric-magik-mode)
  (global-set-key (kbd "<f2> k")      'sw-reload-dotemacs)
  (global-set-key (kbd "<f2> s")      'magik-version-selection)
  (global-set-key (kbd "<f2> z")      'magik-shell)

  ;; ------------------- F3 globals ----------------------

  (global-unset-key (kbd "<f3>"))

  (global-set-key (kbd "<f3> <f3>")  'magik-cb)
  (global-set-key (kbd "<f3> c")     'magik-cb-paste-class)
  (global-set-key (kbd "<f3> j")     'magik-cb-jump-to-source)
  (global-set-key (kbd "<f3> m")     'magik-cb-paste-method)
  (global-set-key (kbd "<f3> /")     'magik-cb-and-clear)
  (global-set-key (kbd "<f3> ?")     'magik-cb-help)
  (global-set-key (kbd "<f3> E")     'magik-cb-execute-method-finder))

(provide 'magik-keys)
;;; magik-keys.el ends here
