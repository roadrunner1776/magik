;;; magik-treesit.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Robin Putters

;; Author: Robin Putters <krn-robin@github>
;; Keywords: languages

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

(require 'magik-mode)
(require 'treesit)

(defvar magik--treesit-settings
  (treesit-font-lock-rules
   :language 'magik
   :feature 'pragma
   '((pragma) @magik-pragma-face)

   :language 'magik
   :feature 'comment
   '((comment) @font-lock-comment-face
     (documentation) @magik-doc-face)

   :language 'magik
   :feature 'type
   '((method
      exemplarname: (identifier) @font-lock-type-face
      name: (identifier) @magik-method-face)
     (call
      message: (identifier) @magik-method-face)
     (slot_accessor) @magik-slot-face
     (iterator (identifier) @font-lock-variable-name-face)
     [(variable) (dynamic_variable) (global_variable)] @font-lock-variable-name-face)

   :language 'magik
   :feature 'error
   '((ERROR) @font-lock-warning-face)

   :language 'magik
   :override t
   :feature 'string
   '((string_literal) @font-lock-string-face)

   :language 'magik
   :feature 'constant
   '([(number) (character_literal)] @font-lock-constant-face
     [(symbol)] @magik-symbol-face)

   :language 'magik
   :feature 'keyword
   `([(false) (true) (maybe) (unset)] @font-lock-constant-face ;;FIXME "_constant"

     ["_and" "_andif" "_div" "_is" "_isnt" "_cf" "_mod" "_not" "_or" "_orif" "_xor" "_xorif"] @magik-keyword-operators-face

     [(self) (super)] @font-lock-type-face ;;FIXME "_clone"

     ["_abstract" "_private"  "_method" "_endmethod"] @magik-method-face ;;FIXME "_primitive"

     ["_proc" "_endproc"] @magik-procedure-face

     ["_block" "_endblock" "_catch" "_throw" "_endcatch"
      "_if" "_then" "_elif" "_else" "_endif"
      "_protect" "_locking" "_protection" "_endprotect" ;;FIXME "_lock" "_endlock"
      "_try" "_endtry" "_when" "_handling" "_with"  ;;FIXME  "_using"
       "_package" "_default"] @magik-keyword-statements-face ;;FIXME "_pragma" "_thisthread"

      ["_iter" "_continue" "_for" "_loop" "_endloop" "_loopbody" "_over" "_leave"] @magik-keyword-loop-face ;;FIXME: "_finally"  "_while"

      ["_gather" "_scatter" "_optional" "_return" ">>"] @magik-keywords-arguments-face ;;FIXME "_allresults"

      ["_dynamic" "_global" "_import" "_local" ] @magik-keyword-variable-face))) ;;FIXME "_class" "_recursive"

;;;###autoload
(define-derived-mode magik-ts-mode magik-base-mode "Magik"
  "Major mode for editing Magik code, using tree-sitter library."
  :group 'magik
  :abbrev-table nil
  :syntax-table nil

  ;; Tree-sitter setup.
  (treesit-parser-create 'magik)

  (setq-local treesit-font-lock-settings magik--treesit-settings
	      treesit-font-lock-feature-list '((comment pragma)
					       (type constant keyword string)
					       ()
					       ()
					       (error)))
  (treesit-major-mode-setup))

(with-eval-after-load 'treesit-auto
  (add-to-list 'treesit-auto-recipe-list
		(make-treesit-auto-recipe
		 :lang 'magik
		 :ts-mode 'magik-ts-mode
		 :remap 'magik-mode
		 :url "https://github.com/krn-robin/tree-sitter-magik"
		 :revision "main"
		 :source-dir "src")))

(provide 'magik-treesit)
;;; magik-treesit.el ends here
