;;; me-notes.el --- Support for taking notes via moldable emacs -*- lexical-binding: t; -*-


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
;;; Support for taking notes via moldable emacs

;;; Code:
(require 'dash)
(require 's)
(require 'me-utils)
(require 'me-org)

(defcustom me-note-file-store "~/workspace/agenda/moldableNotes.el"
  "Store for notes."
  :group 'moldable-emacs)

(defvar me-notes nil "Prototype of notes.")

(defun me-load-all-notes ()
  "Load all notes unless cached."
  (if me-notes
      me-notes
    (setq me-notes
          (ignore-errors
            (with-temp-buffer
              (insert-file-contents-literally me-note-file-store)
              (goto-char (point-min))
              (eval `',(list-at-point)))))))
(defun me-store-note (note)
  "Persist NOTE."
  (add-to-list 'me-notes note)
  (async-start
   `(lambda ()
      (write-region ,(pp-to-string (me-load-all-notes)) nil ,me-note-file-store)))
  note)
(defun me-tag-note-p (note)
  "If NOTE is a tag."
  (me-get-in note '(:then :tags)))
(defun me-load-notes ()
  "Load only textual notes unless cached."
  (-remove 'me-tag-note-p (me-load-all-notes)))
(defun me-ask-for-details-according-to-context (note)
  "Ask for NOTE details."
  (let ((text (read-string "Note:")))
    (plist-put note :then `(:string ,text :state note))))
(defun me-filter-notes-by-buffer (buffername)
  "Filter notes by BUFFERNAME."
  (--filter
   (ignore-errors (equal buffername (plist-get (plist-get (plist-get it :given) :node) :buffer)))
   me-notes))
(defun me-filter-notes-by-project ()
  "Gather notes by project."
  (--filter
   (ignore-errors (s-starts-with-p (projectile-root-bottom-up default-directory) (expand-file-name (me-get-in it '(:given :node :buffer-file)))))
   me-notes))
(defun me-filter-notes-by-mode (mode)
  "Filter notes by MODE."
  (--filter
   (ignore-errors (equal mode (plist-get (plist-get (plist-get it :given) :node) :mode)))
   me-notes))
(defun me-note-to-org-heading (note)
  "Turn a NOTE in a `org-mode' heading."
  (let* ((given (plist-get (plist-get note :given) :node))
         (then (plist-get note :then))
         (id (plist-get given :key))
         (title (me-make-elisp-file-link
                 (concat (s-trim (s-replace-all  '(("\"" . "") ("\n" . " ")) (s-truncate 60 (plist-get given :text)))) " ")
                 (format
                  "(progn (find-file-other-window \"%s\") (goto-char %s))"
                  (plist-get given :buffer-file)
                  (plist-get given :begin))
                 "elisp"))
         (content (plist-get then :string)))
    (format
     "* %s%s\n:PROPERTIES:\n:ID:       %s\n:END:\n%s\n"
     (let ((state (me-get-in note '(:then :state))))
       (if (and state (not (eq 'note state)))
           (format "%s " (upcase (symbol-name state)))
         ""))
     title
     id
     content)))

(defun me-ask-for-todo-details-according-to-context (note)
  "Ask for NOTE details."
  (let ((text (read-string "Note:")))
    (plist-put note :then `(:string ,text :state todo))))

(provide 'me-notes)
;;; me-notes.el ends here
