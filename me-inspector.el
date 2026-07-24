;;; me-inspector.el --- Mold data inspector -*- lexical-binding: t; -*-


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
;;; Mold data inspector

;;; Code:
(require 'dash)
(require 's)

(defcustom me-show-inspector t "Show inspector to see what is the data in self and mold-data for the running mold.")

(defvar me-mold-start-buffer nil "Buffer on which you run `me-mold'.")
(defvar me-last-window-configuration nil "Stores last window configuration from when you firstly invoked me-mold.")

(defun me-set-me-mold-start-buffer ()
  (setq me-mold-start-buffer (buffer-name)))
(add-hook 'me-mold-before-hook #'me-set-me-mold-start-buffer)

(defun me-start-inspector (mold-buffer)
  "Start inspector for MOLD-BUFFER. This show mold state."
  (when me-show-inspector
    (let ((final-window (selected-window)))
      (select-window (split-window-below))
      (switch-to-buffer (get-buffer-create "*moldable-emacs-inspector*"))
      (erase-buffer)
      (emacs-lisp-mode)
      (me-print-to-buffer (list
                           :note "hs-minor-mode enabled for code folding."
                           :self
                           (with-current-buffer mold-buffer (ignore-errors self))
                           :mold-data
                           (with-current-buffer mold-buffer (ignore-errors mold-data))))
      (hs-minor-mode 1)
      (call-interactively #'hs-hide-level)
      (select-window final-window))))

(defun me-show-buffer-and-mold ()
  "Show only start buffer (on the left) and mold (on the right).
This stores the original screen configuration in the `m' register."
  (let ((old-buffer me-mold-start-buffer)
        (mold-buffer (current-buffer)))
    (window-configuration-to-register "m") ; store starting configuration - this overrides it every time
    (delete-other-windows)
    (switch-to-buffer old-buffer)
    (switch-to-buffer-other-window mold-buffer)
    (me-start-inspector mold-buffer)))
(add-hook 'me-mold-after-hook #'me-show-buffer-and-mold 100)

(defun me-store-window-configuration ()
  "Store current window configuration when not in a mold."
  (unless (ignore-errors (or self mold-data))
    (setq me-last-window-configuration (current-window-configuration))))
(add-hook 'me-mold-before-hook 'me-store-window-configuration)

(defun me-restore-starting-window-configuration ()
  "Restore window configuration saved before running `me-mold' for the first time."
  (interactive)
  (if me-last-window-configuration (set-window-configuration me-last-window-configuration)
    (error "No window configuration stored in `me-last-window-configuration'!")))



(provide 'me-inspector)
;;; me-inspector.el ends here
