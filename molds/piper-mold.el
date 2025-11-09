;;; piper-mold.el --- Piper operations as moldable-emacs molds  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Integration of emacs-piper with moldable-emacs
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; This file provides piper-prefixed molds that integrate emacs-piper
;; operations into the moldable-emacs workflow.  Each piper operation
;; appears as a separate mold in the me-mold completion menu.
;;
;; These molds work on either structured data (if `self' is a string)
;; or on buffer contents, enabling seamless integration with other molds.

;;; Code:

(require 'piper)
(require 's)
(require 'dash)

;;; Helper Functions

(defun piper-mold--get-input ()
  "Get input from either `self' (if string) or buffer contents."
  (if (and (boundp 'self) (stringp self))
      self
    (buffer-substring-no-properties (point-min) (point-max))))

(defun piper-mold--set-output (buffername output)
  "Insert OUTPUT into BUFFERNAME and set `self' to OUTPUT."
  (with-current-buffer buffername
    (erase-buffer)
    (insert output)
    (setq-local self output)))

(defun piper-mold--with-temp-buffer-from-input (body-fn)
  "Execute BODY-FN in a temp buffer containing the input text.
Returns the buffer contents after execution."
  (let ((input (piper-mold--get-input)))
    (with-temp-buffer
      (insert input)
      (funcall body-fn)
      (buffer-substring-no-properties (point-min) (point-max)))))

;;; Mold Registrations

(me-register-mold
 :key "piper-Pipe"
 :given (:fn t)
 :then (:fn
        (let* ((cmd (read-string "Pipe: | "))
               (input (piper-mold--get-input))
               (output (shell-command-to-string
                        (format "printf '%%s' %s | %s"
                                (shell-quote-argument input)
                                cmd))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-Shell"
 :given (:fn t)
 :then (:fn
        (let* ((cmd (read-string "Shell command: "))
               (input (piper-mold--get-input))
               (output (with-temp-buffer
                         (insert input)
                         (shell-command-on-region (point-min) (point-max) cmd nil t)
                         (buffer-substring-no-properties (point-min) (point-max)))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-KeepLines"
 :given (:fn t)
 :then (:fn
        (let* ((pattern (read-string "Keep lines matching: "))
               (output (piper-mold--with-temp-buffer-from-input
                        (lambda ()
                          (goto-char (point-min))
                          (keep-lines pattern)))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-DeleteLines"
 :given (:fn t)
 :then (:fn
        (let* ((pattern (read-string "Delete lines matching: "))
               (output (piper-mold--with-temp-buffer-from-input
                        (lambda ()
                          (goto-char (point-min))
                          (flush-lines pattern)))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-Sort"
 :given (:fn t)
 :then (:fn
        (let ((output (piper-mold--with-temp-buffer-from-input
                       (lambda ()
                         (sort-lines nil (point-min) (point-max))))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-ReverseSort"
 :given (:fn t)
 :then (:fn
        (let ((output (piper-mold--with-temp-buffer-from-input
                       (lambda ()
                         (sort-lines t (point-min) (point-max))))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-Unique"
 :given (:fn t)
 :then (:fn
        (let ((output (piper-mold--with-temp-buffer-from-input
                       (lambda ()
                         (delete-duplicate-lines (point-min) (point-max))))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-Join"
 :given (:fn t)
 :then (:fn
        (let* ((delimiter (read-string "Delimiter: "))
               (output (piper-mold--with-temp-buffer-from-input
                        (lambda ()
                          (goto-char (point-min))
                          (while (search-forward "\n" nil t)
                            (replace-match delimiter))))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-Replace"
 :given (:fn t)
 :then (:fn
        (let* ((from (read-string "Replace: "))
               (to (read-string "With: "))
               (output (piper-mold--with-temp-buffer-from-input
                        (lambda ()
                          (goto-char (point-min))
                          (while (search-forward from nil t)
                            (replace-match to))))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-ReplaceRegexp"
 :given (:fn t)
 :then (:fn
        (let* ((regexp (read-string "Replace regexp: "))
               (to (read-string "With: "))
               (output (piper-mold--with-temp-buffer-from-input
                        (lambda ()
                          (goto-char (point-min))
                          (while (re-search-forward regexp nil t)
                            (replace-match to))))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-KeepColumns"
 :given (:fn t)
 :then (:fn
        (let* ((delimiter (read-string "Column delimiter: "))
               (fields (read-string "Column number(s) to keep: "))
               (field-list (mapcar #'string-to-number
                                   (split-string fields "[, ]+" t)))
               (output (piper-mold--with-temp-buffer-from-input
                        (lambda ()
                          (goto-char (point-min))
                          (while (not (eobp))
                            (let* ((line (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position)))
                                   (columns (split-string line delimiter))
                                   (kept-columns (--map (nth (1- it) columns)
                                                        field-list))
                                   (new-line (string-join kept-columns delimiter)))
                              (delete-region (line-beginning-position)
                                             (line-end-position))
                              (insert new-line)
                              (forward-line 1)))))))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-Xargs"
 :given (:fn t)
 :then (:fn
        (let* ((cmd (read-string "Command: "))
               (input (piper-mold--get-input))
               (lines (split-string input "\n" t "[[:space:]]+"))
               (args (string-join lines " "))
               (full-cmd (format "%s %s" cmd args))
               (output (shell-command-to-string full-cmd)))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-ForEachLine"
 :given (:fn t)
 :then (:fn
        (let* ((cmd (read-string "Command (use {} for line): "))
               (input (piper-mold--get-input))
               (lines (split-string input "\n" t "[[:space:]]+"))
               (outputs (--map
                         (let ((line-cmd (replace-regexp-in-string
                                          "{}" (shell-quote-argument it)
                                          cmd t t)))
                           (shell-command-to-string line-cmd))
                         lines))
               (output (string-join outputs "")))
          (piper-mold--set-output buffername output))))

(me-register-mold
 :key "piper-ToClipboard"
 :given (:fn t)
 :then (:fn
        (let ((input (piper-mold--get-input)))
          (kill-new input)
          (message "Copied to clipboard")
          (with-current-buffer buffername
            (erase-buffer)
            (insert input)
            (setq-local self input)))))

(provide 'piper-mold)
;;; piper-mold.el ends here
