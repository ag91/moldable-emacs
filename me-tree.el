;;; moldable-emacs.el --- Moldable Development Extension -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea

;; Author: Andrea <andrea-dev@hotmail.com>
;; Version: 20211115-snapshot
;; URL: https://github.com/ag91/moldable-emacs
;; Package-Requires: ((emacs "26.1") (dash "2.19.1") (s "1.12.0") (async "1.9.4"))
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
;;; Tree parsing utilities for moldable-emacs.

;;; Code:
(require 'dash)
(require 's)
(require 'me-utils)
(require 'treesit)

(defcustom me-use-treesitter t
  "Use https://github.com/emacs-tree-sitter to produce concrete syntax trees, if nil we will use built-in treesit.el."
  :type 'boolean
  :group 'moldable-emacs)

(defun me-to-parse-tree (&optional node)
  "Return list of all abstract syntax tree nodes one step away from leaf nodes.
Optionally start from NODE."
  (if me-use-treesitter
      (me-mold-treesitter-to-parse-tree node)
    (me-mold-treesit-to-parse-tree node)))

(defun me-tree-node-at-point (point)
  (if me-use-treesitter
      (me-to-parse-tree (tree-sitter-node-at-point :named point))
    (treesit-node-at point nil 'named)))

(defun me-extension-to-major-mode (extension)
  "Find `major-mode' for EXTENSION.

>> (me-extension-to-major-mode \".el\")
=> emacs-lisp-mode"
  (cdr (--find (s-match (car it) extension) auto-mode-alist)))

(defun me-major-mode-to-tree-sitter-grammar (major-mode)
  "Find emacs-tree-sitter grammar for MAJOR-MODE."
  (if me-use-treesitter
      (alist-get major-mode tree-sitter-major-mode-language-alist)
    (treesit-language-at (point))))

(defun me-extension-to-tree-sitter-grammar (extension)
  "Find emacs-tree-sitter grammar for EXTENSION."
  (--> extension
       me-extension-to-major-mode
       me-major-mode-to-tree-sitter-grammar))

(defun me--treesitter-filepath-to-flattened-tree (file &optional contents)
  (when-let ((grammar (me-extension-to-tree-sitter-grammar (file-name-extension file t))))
    (with-temp-buffer
      (if contents (insert contents) (insert-file-contents-literally file))
      (let ((buffer-file-name file)
            (tree-sitter-language (tree-sitter-require grammar))
            (tree-sitter-parser (tsc-make-parser)))
        (tsc-set-language tree-sitter-parser tree-sitter-language)
        (--> (tsc--without-restriction
              (tsc-parse-chunks tree-sitter-parser #'tsc--buffer-input nil)) ; TODO this seems to break for non unicode files
             tsc-root-node
             me-to-parse-tree)))))

(defun me--treesit-filepath-to-flattened-tree (file &optional contents)
  (when-let ((grammar (me-extension-to-tree-sitter-grammar (file-name-extension file t))))
    (--> (or contents (with-temp-buffer (insert-file-contents-literally file) (buffer-string)))
         (treesit-parse-string it grammar))
    (treesit-parse-string it grammar)
    me-to-parse-tree-new))

(defun me-filepath-to-flattened-tree (file &optional contents)
  "Return the flattened tree for FILE.
Optionally use CONTENTS string instead of file contents."
  (if me-use-treesitter (me--treesitter-filepath-to-flattened-tree file contents)
    (me--treesit-filepath-to-flattened-tree file contents)))

(defun me-mold-treesit-to-parse-tree (&optional node)
  "Return list of all abstract syntax tree nodes one step away from leaf nodes.
Optionally start from NODE. Note this keeps text properties in
the :text property of a node."
  (let ((root (or
               node
               (with-demoted-errors "me-mold-treesit-to-parse-tree: %S"
                 (treesit-parse-string ;; in treesit we parse the file only if using a lang-ts-mode
                  (buffer-string)
                  (me-major-mode-to-tree-sitter-grammar major-mode)))))
        (make-node (lambda (n level)
                     (list
                      :type (intern (treesit-node-type n))
                      :text (substring-no-properties (treesit-node-text n))
                      :begin (treesit-node-start n)
                      :end (treesit-node-end n)
                      :buffer (buffer-name)
                      :buffer-file (when buffer-file-name
                                     (s-replace (getenv "HOME") "~"
                                                buffer-file-name))
                      :mode major-mode
                      :level level))))
    (when root
      (cl-labels
          ((fn (node level)
             (mapcar (lambda (n)
                       (setq acc (cons
                                  (funcall make-node n level)
                                  acc))
                       (fn n (1+ level)))
                     (treesit-node-children node))))
        (setq-local acc nil)
        (fn root 0)
        (cons
         (funcall make-node root 0)
         (reverse acc))))))

(defun me-mold-treesitter-to-parse-tree (&optional node)
  "Return list of all abstract syntax tree nodes one step away from leaf nodes.
Optionally start from NODE."
  (let ((root (or
               node
               (ignore-errors (tsc-root-node tree-sitter-tree)))))
    (when root
      (cl-labels
          ((fn (node level)
             (tsc-mapc-children
              (lambda (n)
                (setq acc (cons
                           (list
                            :type (tsc-node-type n)
                            :text (tsc-node-text n)
                            :begin (tsc-node-start-position n)
                            :end (tsc-node-end-position n)
                            :buffer (buffer-name)
                            :buffer-file (when buffer-file-name
                                           (s-replace (getenv "HOME") "~"
                                                      buffer-file-name))
                            :mode major-mode
                            :level level)
                           acc))
                (fn n (1+ level)))
              node)))
        (setq-local acc nil)
        (fn root 0)
        (cons (list
               :type (tsc-node-type root)
               :text (tsc-node-text root)
               :begin (tsc-node-start-position root)
               :end (tsc-node-end-position root)
               :buffer (buffer-name)
               :buffer-file (when buffer-file-name
                              (s-replace (getenv "HOME") "~"
                                         buffer-file-name))
               :mode major-mode
               :level 0)
              (reverse acc))))))

(defun me-insert-treesitter-follow-overlay (nodes &optional transformer)
  "Add overlayed entries for NODES types using `emacs-tree-sitter'.
You can extract the data you want to show
with TRANSFORMER, which is a function taking a node and returning
a string (node -> string)."
  (cursor-sensor-mode 1)
  (--each
      nodes
    (let ((type (plist-get it :type))
          (beg (point)))
      (insert                           ; this insert the type of the node with overlay inline!
       (or (when transformer (funcall transformer it))
           (format "%s\n" type)))
      (let ((old-buffer (plist-get it :buffer))
            (ov (make-overlay beg (- (point) 1)))) ;; after `insert' point =/= beg, point goes after insertion
        (overlay-put
         ov
         'cursor-sensor-functions
         (list `(lambda (affected-window old-position entered-or-left)
                  (cond
                   ((eq entered-or-left 'entered)
                    (overlay-put ,ov 'face 'tree-sitter-query-match)
                    (let ((tree-sitter-query--target-buffer ,old-buffer))
                      (tree-sitter-query--eval-query (format "((%s) @%s)" ,(symbol-name type) ,(symbol-name type)))))
                   ((eq entered-or-left 'left)
                    (let ((tree-sitter-query--target-buffer ,old-buffer))
                      (overlay-put ,ov 'face nil)
                      (tree-sitter-query--clean-target-buffer)))))))))))

(provide 'me-tree)
;;; me-tree.el ends here
