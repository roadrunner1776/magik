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
     (try (identifier) @font-lock-variable-name-face)
     (label) @font-lock-variable-name-face
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
   `([(false) (true) (maybe) (unset)] @font-lock-constant-face
     ["_constant"] @font-lock-constant-face

     ["_and" "_andif" "_div" "_is" "_isnt" "_cf" "_mod" "_not" "_or" "_orif" "_xor" "_xorif"] @magik-keyword-operators-face

     [(self) (super) (clone)] @font-lock-type-face

     ["_abstract" "_private"  "_method" "_endmethod" "_primitive"] @magik-method-face

     ["_proc" "_endproc"] @magik-procedure-face

     ["_block" "_endblock" "_catch" "_throw" "_endcatch"
      "_if" "_then" "_elif" "_else" "_endif"
      "_protect" "_locking" "_protection" "_endprotect" "_lock" "_endlock"
      "_try" "_endtry" "_when" "_handling" "_with"
      "_package" "_default" (thisthread)] @magik-keyword-statements-face

     ["_iter" "_continue" "_for" "_loop" "_endloop" "_loopbody" "_over" "_leave" "_finally" "_while"] @magik-keyword-loop-face

     ["_gather" "_scatter" "_allresults" "_optional" "_return" ">>"] @magik-keyword-arguments-face

     ["_dynamic" "_global" "_import" "_local" "_class"] @magik-keyword-variable-face)))

(defvar magik-ts-mode--indent-rules
  `((magik
     ((node-is "}") parent 0)
     ((node-is ")") parent 0)
     ((node-is "_protection") parent 0)
     ((node-is "_then") parent 0)
     ((node-is "_endblock") parent 0)
     ((node-is "_endif") parent 0)
     ((node-is "_loop") parent 0)
     ((node-is "_endloop") parent 0)
     ((node-is "_endmethod") parent 0)
     ((node-is "_endprotect") parent 0)
     ((node-is "_endlock") parent 0)
     ((node-is "_endproc") parent 0)
     ((node-is "_endtry") parent 0)
     ((node-is "method") parent 0)
     ((node-is "elif") parent 0)
     ((node-is "else") parent 0)
     ((node-is "when") parent 0)

     ((parent-is "block") parent magik-indent-level)
     ((parent-is "if") parent magik-indent-level)
     ((parent-is "elif") parent magik-indent-level)
     ((parent-is "else") parent magik-indent-level)
     ((parent-is "iterator") parent magik-indent-level)
     ((parent-is "loop") parent magik-indent-level)
     ((parent-is "finally") parent magik-indent-level)
     ((parent-is "while") parent magik-indent-level)
     ((parent-is "method") parent magik-indent-level)
     ((parent-is "protect") parent magik-indent-level)
     ((parent-is "lock") parent magik-indent-level)
     ((parent-is "proc") parent magik-indent-level)
     ((parent-is "try") parent magik-indent-level)

     ((parent-is "assignment") parent magik-indent-level)
     ((parent-is "logical_operator") parent magik-indent-level)
     ((parent-is "relational_operator") parent magik-indent-level)
     ((parent-is "arithmetic_operator") parent magik-indent-level)
     ((parent-is "unary_operator") parent magik-indent-level)

     ((parent-is "documentation") first-sibling 0)
     ((parent-is "invoke") (nth-sibling 2) 0)

     ((parent-is "call") parent-bol magik-indent-level)
     ((parent-is "vector") parent-bol magik-indent-level)

     (no-node parent-bol 0)
     (catch-all prev-sibling 0))))

;;;###autoload
(define-derived-mode magik-ts-mode magik-base-mode "Magik"
  "Major mode for editing Magik code, using tree-sitter library."
  :group 'magik
  :abbrev-table nil
  :syntax-table nil

  ;; Tree-sitter setup.
  (treesit-parser-create 'magik)

  (setq-local treesit--indent-verbose t
	      treesit-simple-indent-rules magik-ts-mode--indent-rules
	      treesit-font-lock-settings magik--treesit-settings
	      treesit-font-lock-feature-list '((comment pragma)
					       (type constant keyword string)
					       ()
					       ()
					       (error)))
  (treesit-major-mode-setup))

;;;###autoload
(when (and (or (featurep 'treesit)
	       (require 'treesit nil 'noerror))
	   (fboundp 'treesit-ready-p))
  (add-to-list 'treesit-language-source-alist '(magik "https://github.com/krn-robin/tree-sitter-magik"))
  (when (treesit-ready-p 'magik)
    (add-to-list 'major-mode-remap-alist '(magik-mode . magik-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.magik\\'" . magik-ts-mode))))

(provide 'magik-treesit)
;;; magik-treesit.el ends here
