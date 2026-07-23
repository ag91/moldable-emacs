;;; me-mold-data.el --- Utils for mold companion data -*- lexical-binding: t; -*-


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
;;; Utils for mold companion data

;;; Code:
(require 'dash)
(require 's)
(require 'me-utils)

(defvar me-temporary-mold-data nil "Holder of mold data before it is assigned to local variable `mold-data'.")

(defun me-setup-self-mold-data ()
  "Setup `me-temporary-mold-data' for setting up `mold-data' in mold buffer."
  (setq me-temporary-mold-data
        (list
         :old-self (ignore-errors self)
         :old-buffer (buffer-name)
         :old-file (buffer-file-name)
         :old-point (point)
         :old-mode major-mode
         :old-date (ignore-errors (plist-get mold-data :date))
         :old-mold (ignore-errors (plist-get mold-data :mold)))))
(add-hook 'me-mold-before-hook #'me-setup-self-mold-data)

(defun me-set-self-mold-data ()
  "Set `mold-data'."
  (setq-local mold-data
              (append
               (list
                :mold me-last-used-mold
                :self (ignore-errors self)
                :date (format-time-string "%FT%T%z"))
               me-temporary-mold-data)))

(defun me-get-marked-dired-files ()
  "Get marked `dired' files."
  (goto-char (point-min))
  (dired-get-marked-files))

(defun me-get-all-dired-files ()
  "Get all `dired' files."
  (mark-whole-buffer)
  (call-interactively #'dired-mark)
  (let ((files (dired-get-marked-files)))
    (call-interactively #'dired-unmark-all-files)
    files))

(defun me-set-dired-self-for-playground ()
  "Set Playground `self' to dired list of files."
  (when
      (and
       (s-starts-with-p "Playground" me-last-used-mold)
       (ignore-errors mold-data)
       (eq (plist-get mold-data :old-mode) 'dired-mode))
    (setq-local self
                (with-current-buffer (plist-get mold-data :old-buffer)
                  (or (me-get-marked-dired-files)
                      (me-get-all-dired-files))))))
(add-hook 'me-mold-after-hook #'me-set-dired-self-for-playground) ;; the order is important: keep before me-set-self-mold-data

(provide 'me-mold-data)
;;; me-mold-data.el ends here
