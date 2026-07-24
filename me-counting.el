;;; me-counting.el --- Utilities to count things -*- lexical-binding: t; -*-


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
;;; Utilities to count things

;;; Code:
(require 'dash)
(require 's)

;; taken from: https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer
(defvar me-punctuation-marks '(","
                               "."
                               "'"
                               "&"
                               "\"")
  "List of Punctuation Marks that you want to count.")

(defun me-count-raw-word-list (raw-word-list)
  "Produce a dictionary of RAW-WORD-LIST with the number of occurrences for each word."
  (--> raw-word-list
       (--reduce-from
        (progn
          (cl-incf (cdr (or (assoc it acc)
                            (car (push (cons it 0) acc)))))
          acc)
        nil
        it)
       (sort it (lambda (a b) (string< (car a) (car b))))))

(defun me-word-stats (string)
  "Return word (as a token between spaces) frequency in STRING."
  (let* ((words (split-string
                 (downcase string)
                 (format "[ %s\f\t\n\r\v]+"
                         (mapconcat #'identity me-punctuation-marks ""))
                 t))
         (punctuation-marks (--filter
                             (member it me-punctuation-marks)
                             (split-string string "" t)))
         (raw-word-list (append punctuation-marks words))
         (word-list (me-count-raw-word-list raw-word-list)))
    (sort word-list (lambda (a b) (> (cdr a) (cdr b))))))


(defun me-get-reading-time (text)
  "Calculate reading time of TEXT in minutes according to https://www.coengoedegebure.com/add-reading-time-to-articles/."
  (with-temp-buffer
    (insert text)
    (/ (count-words (point-min) (point-max)) 228)))

(defun me-get-book-pages (text)
  "Calculate number of book pages TEXT would fill according to https://kindlepreneur.com/words-per-page/."
  (with-temp-buffer
    (insert text)
    (/ (count-words (point-min) (point-max)) 280)))

(defun me-calc-numeric-p (text)
  "Check if TEXT is a numeric arithmetic expression `calc' can work with."
  (let ((calc-eval-error 't)) (ignore-errors (calc-eval text 'num))))

(defun me-arithmetic-component-p (it)
  "Is IT an arithmetic component?"
  (or
   (string= it (number-to-string (string-to-number it)))
   (string= "-" it)
   (string= "+" it)
   (string= "/" it)
   (string= "*" it)
   (string= "%" it)
   (string= "^" it)
   (string= "(" it)
   (string= ")" it)
   (string= "." it)))

(defun me-arithmetic-expression-member-p (it)
  "Check if there is an arithmetic member in IT."
  (or (me-arithmetic-component-p it)
      ;; in case we have something like "1+1"
      (-all?
       #'me-arithmetic-component-p
       (s-split "" it 't))))

(defun me-arithmetic-at-point () ;; TODO needs refactoring!
  "Find an arithmetic expression on the current line.
NIL if not there."
  (--> (or
        (when (region-active-p)
          (list
           (buffer-substring-no-properties
            (car (car (region-bounds)))
            (cdr (car (region-bounds))))
           "")) ;; this is for common format (list string-before-point string-after-point)
        (list
         (buffer-substring-no-properties
          (save-excursion (beginning-of-line) (point))
          (point))
         (buffer-substring-no-properties
          (point)
          (save-excursion (end-of-line) (point)))))
       (list
        ;; take only arithmetic words from point to beginning of line
        (--> it
             (nth 0 it)
             (s-split " " it 't)
             (reverse it)
             (-take-while #'me-arithmetic-expression-member-p it)
             (reverse it)
             (s-join " " it))
        ;; take only arithmetic words from point to end of line
        (--> it
             (nth 1 it)
             (s-split " " it 't)
             (-take-while #'me-arithmetic-expression-member-p it)
             (s-join " " it)))
       ;; join the two parts
       (concat (nth 0 it) (nth 1 it))
       s-trim
       (unless (string-blank-p it) it)))

(provide 'me-counting)
;;; me-counting.el ends here
