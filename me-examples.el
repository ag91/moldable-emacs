;;; me-examples.el --- Molds examples utils -*- lexical-binding: t; -*-


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
;;; Molds examples utils

;;; Code:
(require 'dash)
(require 's)
(require 'me-utils)
(require 'me-tree)

(defcustom me-example-resource-dir
  (concat (file-name-directory load-file-name) "resources/")
  "Directory containing resources for examples (like media files)."
  :group 'moldable-emacs
  :type 'string)

(defun me-record-given-of-example ()
  "Reset and store in `me-last-example' the given of a mold example."
  (let* ((type (if (buffer-file-name) 'file 'buffer))
         (point (point))
         (name (or (buffer-file-name) (buffer-name)))
         (mode major-mode)
         (current-mold-data (ignore-errors (copy-tree mold-data)))
         (contents (if (eq mode 'image-mode)
                       (let ((filename (concat
                                        me-example-resource-dir
                                        (file-name-nondirectory name))))
                         (write-region
                          (point-min)
                          (point-max)
                          filename)
                         filename)
                     (buffer-substring-no-properties (point-min) (point-max)))))
    (setq me-last-example
          `(:given
            (
             :type ,type
             :name ,name
             :mode ,mode
             :contents ,contents
             :point ,point
             ,@(when current-mold-data
                 (list :mold-data current-mold-data)))))))
(add-hook 'me-mold-before-hook #'me-record-given-of-example)

(defun me-record-then-of-example ()
  "Reset and store in `me-last-example' the then of a mold example."
  (let* ((type (if (buffer-file-name) 'file 'buffer))
         (name (or (buffer-file-name) (buffer-name)))
         (mode major-mode)
         (contents (if (eq mode 'image-mode)
                       (let ((filename (concat
                                        me-example-resource-dir
                                        (file-name-nondirectory name))))
                         (write-region
                          (point-min)
                          (point-max)
                          filename)
                         filename)
                     (buffer-substring-no-properties (point-min) (point-max)))))
    (plist-put
     me-last-example
     :then
     `(
       :type ,type
       :name ,name
       :mode ,mode
       :contents ,contents))))
(add-hook 'me-mold-after-hook #'me-record-then-of-example)

(defun me-check-then-clause (then)
  "Run THEN clause and return list with success and issues.
This is a function used to test mold examples."
  (let* ((contents (list
                    (plist-get then :contents)
                    (buffer-substring-no-properties (point-min) (point-max))))
         (modes
          (list (plist-get then :mode)
                major-mode))
         (to-test (list contents modes)))
    (list
     :success (--reduce (and acc it) (--map (apply #'equal it) to-test))
     :issues (--map (-zip '(:expected :actual) it) (--remove (apply #'equal it) to-test)))))

(defun me-check-example (example run-fn)
  "Run RUN-FN in the EXAMPLE."
  (append
   (list :example (plist-get example :name))
   (eval `(let ((buf ,(current-buffer))
                (pos ,(point))
                (beg (plist-get ',example :given))
                (end (plist-get ',example :then)))
            (me--given beg
              (let ((result (me-check-then-clause end)))
                (kill-buffer)
                (switch-to-buffer buf)
                (goto-char pos)
                result))))))

(defun me-check-mold-examples (mold)
  "Check that MOLD's examples are working, returning test reports for each of them."
  (--map
   (progn
     (unless (plist-get it :name)
       (warn (concat "Missing name for example of " (plist-get mold :key))))
     (me-check-example it (lambda () (me-mold-run-then mold))))
   (plist-get mold :examples)))
(defun me-test-example (example run-fn)
  "Test RUN-FN in the EXAMPLE."
  (let ((result (plist-get (me-check-example example run-fn) :success)))
    (if result
        result
      (message "Issues: %s" (list example (me-check-example example run-fn))))))

(defun me-mold-add-last-example ()
  "Add `me-last-example' to last mold."
  (interactive)
  (when me-last-used-mold
    (find-file (plist-get (me-find-mold me-last-used-mold) :origin))
    (goto-char (point-min))
    (search-forward (format ":key \"%s\"" me-last-used-mold))
    (let* ((result (me-check-example me-last-example (me-get-in (me-find-mold me-last-used-mold) '(:then :fn))))
           (pass (plist-get result :success))
           (issues (plist-get result :issues)))
      (unless pass
        (warn "The example you are trying to add does not work because the following did not match:\n%s" issues)))
    (kill-new (pp-to-string me-last-example))
    (message "You have the example of the last run of this mold in the kill ring: use it!")
    ;; TODO make this smarter
    ))

(defun me-insert-last-example ()
  "Insert `me-last-example' at point."
  (interactive)
  (if me-last-example
      (insert (pp-to-string me-last-example))
    (message "Sorry, no example available in `me-last-example'!")))

(provide 'me-examples)
;;; me-examples.el ends here
