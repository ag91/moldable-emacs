;;; me-elisp-api.el --- Utilities to extract Elisp structure via tree sitter parser -*- lexical-binding: t; -*-


;; Copyright (C) 2026 Andrea

;; Author: Andrea <andrea-dev@hotmail.com>
;; URL: https://github.com/ag91/moldable-emacs
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;; Utilities to extract Elisp structure via tree sitter parser

;;; Code:
(require 'dash)
(require 's)
(require 'me-utils)
(require 'me-tree)

(defun me-elisp-description (tree)
  "Extract description from TREE.

>> (me-elisp-description
  '((:type comment
     :text \";;; test.el --- test description  -*- lexical-binding: t -*-\n\"
     :begin 1
     :end 62
     :buffer \"test.el\"
     :buffer-file \"/tmp/test.el\"
     :mode emacs-lisp-mode
     :level 0)))
=> \"test.el --- test description\""
  (--> tree
       (--find (and (equal 'comment (plist-get it :type))
                    (s-starts-with-p
                     ";;; "
                     (plist-get it :text)))
               it)
       (plist-get it :text)
       (s-split ";;;" it t)
       car
       (s-split "-\\*-" it)
       car
       s-trim))

(defun me-elisp-defcustoms (tree)
  "Extract defcustoms from TREE.

>> (me-elisp-defcustoms
  '((:type something-else)
    (:type list
     :text \"(defcustom test 1 \\\"HI\\\")\"
     :begin 321 :end 354
     :buffer \"test.el\"
     :buffer-file \"/tmp/test.el\"
     :mode emacs-lisp-mode
     :level 0)))
=> ((:type list
     :text \"(defcustom test 1 \\\"HI\\\")\"
     :begin 321 :end 354
     :buffer \"test.el\"
     :buffer-file \"/tmp/test.el\"
     :mode emacs-lisp-mode
     :level 0))"
  (--filter
   (and (equal 'list (plist-get it :type))
        (s-starts-with-p "(defcustom " (plist-get it :text)))
   tree))

(defun me-elisp-functions (tree)
  "Extract functions from TREE.

>> (me-elisp-functions '(
    (:type something-else)
    (:type function_definition
    :text \"(defun test--private ()\\n  \\\"test\\\"\\n  1)\"
    :begin 472
    :end 512
    :buffer \"test.el\"
    :buffer-file \"/tmp/test.el\"
    :mode emacs-lisp-mode
    :level 0)
    (:type function_definition
    :text \"(defun test-public ()\\n  \\\"test\\\"\\n  1)\"
    :begin 432
    :end 470
    :buffer \"test.el\"
    :buffer-file \"/tmp/test.el\"
    :mode emacs-lisp-mode
    :level 0)))
=> ((:type function_definition
    :text \"(defun test--private ()\\n  \\\"test\\\"\\n  1)\"
    :begin 472
    :end 512
    :buffer \"test.el\"
    :buffer-file \"/tmp/test.el\"
    :mode emacs-lisp-mode
    :level 0)
    (:type function_definition
    :text \"(defun test-public ()\\n  \\\"test\\\"\\n  1)\"
    :begin 432
    :end 470
    :buffer \"test.el\"
    :buffer-file \"/tmp/test.el\"
    :mode emacs-lisp-mode
    :level 0))"
  (--filter
   (equal 'function_definition (plist-get it :type))
   tree))

(defun me-elisp-macros (tree)
  "Extract macros from TREE.

>> (me-elisp-macros '(
    (:type something-else)
    (:type list
    :text \"(defmacro test--private ()\\n  \\\"test\\\"\\n  1)\"
    :begin 472
    :end 512
    :buffer \"test.el\"
    :buffer-file \"/tmp/test.el\"
    :mode emacs-lisp-mode
    :level 0)
    (:type list
    :text \"(defmacro test-public ()\\n  \\\"test\\\"\\n  1)\"
    :begin 432
    :end 470
    :buffer \"test.el\"
    :buffer-file \"/tmp/test.el\"
    :mode emacs-lisp-mode
    :level 0)))
=> ((:type list
    :text \"(defmacro test--private ()\\n  \\\"test\\\"\\n  1)\"
    :begin 472
    :end 512
    :buffer \"test.el\"
    :buffer-file \"/tmp/test.el\"
    :mode emacs-lisp-mode
    :level 0)
    (:type list
    :text \"(defmacro test-public ()\\n  \\\"test\\\"\\n  1)\"
    :begin 432
    :end 470
    :buffer \"test.el\"
    :buffer-file \"/tmp/test.el\"
    :mode emacs-lisp-mode
    :level 0))"
  (--filter
   (and
    (equal 'list (plist-get it :type))
    (s-starts-with-p "(defmacro" (plist-get it :text)))
   tree))

(defun me-elisp-public-symbols (tree)
  "Extract only public symbols from TREE.
>> (me-elisp-public-symbols
    '((:text \"(defun my--private ())\")
      (:text \"(defun my-public ())\")))
=> ((:text \"(defun my-public ())\"))"
  (--remove
   (--> it
        (plist-get it :text)
        (s-split "\n" it)
        car
        (s-contains-p "--" it))
   tree))

(defun me-elisp-extract-api (tree)
  "Given a treesitter TREE, extract a plist with the human readable API."
  (list
   :description (with-demoted-errors "%S" (me-elisp-description tree))
   :defcustoms (with-demoted-errors "%S" (me-elisp-defcustoms tree))
   :macros (with-demoted-errors "%S" (me-elisp-public-symbols (me-elisp-macros tree)))
   :functions (with-demoted-errors "%S" (me-elisp-public-symbols (me-elisp-functions tree)))))

(provide 'me-elisp-api)
;;; me-elisp-api.el ends here
