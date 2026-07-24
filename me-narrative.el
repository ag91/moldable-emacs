;;; me-narrative.el --- Support for story telling via moldable emacs -*- lexical-binding: t; -*-


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
;;; Support for story telling via moldable emacs

;;; Code:
(require 'dash)
(require 's)
(require 'me-utils)
(require 'me-org)

(defcustom me-diary-file
  (expand-file-name "diary.org" user-emacs-directory)
  "Default file for saving moldable-emacs narratives as diary entries."
  :group 'moldable-emacs
  :type 'file)

(defun me--format-narrative (composed-key steps)
  "Format STEPS as an Org narrative string for COMPOSED-KEY.
STEPS is a list of plists with :key, :docs, :output, and optionally :link
and :form (for EvalSexp, the input form that was evaluated)."
  (concat
   (format "* %s\n\n" composed-key)
   (s-join "\n\n"
           (--map (concat
                   (format "** %s\n%s\n\n%s"
                           (or (plist-get it :link) (plist-get it :key))
                           (or (plist-get it :docs) "")
                           (if (plist-get it :link)
                               (format "Buffer: %s" (plist-get it :link))
                             ""))
                   (when (plist-get it :form)
                     (format "\n\nEvaluated form:\n#+begin_src elisp\n%s\n#+end_src"
                             (plist-get it :form)))
                   (format "\n\n#+begin_example\n%s\n#+end_example"
                           (or (plist-get it :output) "")))
                  steps))))

(defun me--composed-key (keys)
  "Make a readable composed key from a list of mold KEYS."
  (s-join " -> " keys))

(defun me-trace (name data &optional source)
  "Record a trace step with NAME, DATA, and optional SOURCE.
SOURCE is a plist with :file, :begin, :end for code location.
When `me-trace' is nil, this is a no-op."
  (when me-trace
    (let ((step (list :name name
                      :data data
                      :source source
                      :ts (current-time))))
      (plist-put me-trace :steps
                 (append (plist-get me-trace :steps) (list step))))
    data))

(defun me-replay-story (source mold-keys &optional step-data)
  "Replay a story by opening SOURCE and applying each mold in MOLD-KEYS.
SOURCE is a file path or buffer name.  MOLD-KEYS is a list of mold key strings.
STEP-DATA is an optional list of plists, one per mold key, containing
extra data for replay (e.g. :code for Playground, :sexp for EvalSexp).
The first element of STEP-DATA is the source step data, which may
contain :output to recreate the buffer contents if the file is missing."
  (interactive
   (list (read-file-name "Source file: ")
         (read-string "Mold keys (space-separated): ")))
  (let ((keys (if (stringp mold-keys)
                  (s-split " " (s-trim mold-keys) t)
                mold-keys))
        (source-data (car step-data)))
    (cond
     ((and source (file-exists-p source))
      (find-file source))
     ((and source-data (plist-get source-data :output))
      (let ((buf (get-buffer-create (or source "replay-source"))))
        (with-current-buffer buf
          (erase-buffer)
          (insert (plist-get source-data :output)))
        (switch-to-buffer buf)))
     (source
      (find-file source)))
    (--each-indexed keys
      (let* ((key it)
             (data (nth (1+ it-index) step-data)))
        (cond
         ((string= key "Playground")
          (let ((me-playground-self (plist-get data :self)))
            (me-mold "Playground"))
          (when-let ((code (plist-get data :code))
                     (buf (--find (s-starts-with-p "*moldable-emacs-Playground" it)
                                  (mapcar #'buffer-name (buffer-list)))))
            (with-current-buffer buf
              (erase-buffer)
              (insert code)
              (goto-char (point-min))
              (search-forward "(" nil t))))
         ((string= key "EvalSexp")
          (let ((me-evalsexp-form (plist-get data :sexp)))
            (me-mold "EvalSexp")))
         (t
          (me-mold key)))))))

(defun me--parse-narrative-content (content)
  "Parse the Narrative src block CONTENT into step plists.
Each step has :key, :output, and optionally :form (for EvalSexp)."
  (let* ((steps nil)
         (lines (s-lines content))
         (current-key nil)
         (current-output nil)
         (current-form nil)
         (in-example nil)
         (in-form nil))
    (--each lines
      (cond
       ((s-match "^,\\*\\* " it)
        (when current-key
          (push (list :key current-key
                      :output (s-trim (s-join "\n" (reverse current-output)))
                      :form current-form)
                steps))
        (setq current-key (me-org-replace-link-by-link-description (s-trim (s-replace-regexp "^,\\*\\* " "" it))))
        (setq current-output nil)
        (setq current-form nil))
       ((s-contains-p "#+begin_src elisp" it)
        (setq in-form t))
       ((s-contains-p "#+end_src" it)
        (setq in-form nil))
       (in-form
        (setq current-form (if current-form
                               (concat current-form "\n" it)
                             it)))
       ((s-contains-p "#+begin_example" it)
        (setq in-example t))
       ((s-contains-p "#+end_example" it)
        (setq in-example nil))
       (in-example
        (push it current-output))))
    (when current-key
      (push (list :key current-key
                  :output (s-trim (s-join "\n" (reverse current-output)))
                  :form current-form)
            steps))
    (reverse steps)))

(defun me-replay-story-from-diary ()
  "Replay the story stored in the current diary entry.
Parses the Narrative subheading's src block for all replay data:
source buffer name, mold keys, step outputs, and EvalSexp forms."
  (interactive)
  (save-excursion
    ;; Find the Narrative subheading and parse its content
    (org-next-visible-heading 1)
    (let ((narrative-content nil))
      (when (string= (nth 4 (org-heading-components)) "Narrative")
        (setq narrative-content
              (buffer-substring-no-properties
               (org-entry-beginning-position)
               (org-entry-end-position))))
      (unless narrative-content
        (error "No Narrative subheading found"))
      ;; Extract the src block content
      (let* ((src-start (s-index-of "#+begin_src org\n" narrative-content))
             (src-end (s-index-of "\n#+end_src" narrative-content))
             (src-content (when (and src-start src-end)
                            (substring narrative-content
                                       (+ src-start (length "#+begin_src org\n"))
                                       src-end)))
             (steps (me--parse-narrative-content src-content))
             (source-step (car steps))
             (source-output (plist-get source-step :output))
             (source-name (plist-get source-step :key))
             (mold-steps (cdr steps))
             (mold-keys (--map (plist-get it :key) mold-steps)))
        ;; Recreate source buffer
        (let ((source-file (when (s-present-p source-name)
                             (expand-file-name source-name))))
          (cond
           ((and source-file (file-exists-p source-file))
            (find-file source-file))
           (source-output
            (let ((buf (get-buffer-create (or source-name "replay-source"))))
              (with-current-buffer buf
                (erase-buffer)
                (insert source-output))
              (switch-to-buffer buf)))))
        ;; Run each mold
        (--each-indexed mold-keys
          (let* ((key it)
                 (data (nth it-index mold-steps)))
            (cond
             ((string= key "Playground")
              (me-mold "Playground")
              (when-let ((code (plist-get data :output))
                         (buf (--find (s-starts-with-p "*moldable-emacs-Playground" it)
                                      (mapcar #'buffer-name (buffer-list)))))
                (with-current-buffer buf
                  (erase-buffer)
                  (insert code)
                  (goto-char (point-min))
                  (search-forward "(" nil t))))
             ((string= key "EvalSexp")
              (let ((me-evalsexp-form (plist-get data :form)))
                (me-mold "EvalSexp")))
             (t
              (me-mold key)))))))))

(defun me-save-narrative-to-diary (entry-title subtree-path)
  "Save the current narrative buffer to `me-diary-file'.
ENTRY-TITLE is the heading for the diary entry.
SUBTREE-PATH is the org heading path under which to insert (e.g. \"2026-07\")."
  (interactive
   (list (read-string "Entry title: "
                      (when (and (buffer-local-value 'self (current-buffer))
                                 (plist-get (buffer-local-value 'self (current-buffer)) :steps))
                        (me--composed-key
                         (--map (plist-get it :key)
                                (plist-get (buffer-local-value 'self (current-buffer)) :steps)))))
         (read-string "Subtree path (leave empty for top level): ")))
  (let* ((narrative-content (buffer-substring-no-properties (point-min) (point-max)))
         (replay-link "[[elisp:(me-replay-story-from-diary)][Replay]]"))
    (unless (file-exists-p me-diary-file)
      (with-temp-file me-diary-file
        (insert "#+TITLE: Moldable Emacs Diary\n\n")))
    (with-current-buffer (find-file-noselect me-diary-file)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (when (s-present-p subtree-path)
        (let ((headings (s-split "/" subtree-path t)))
          (--each headings
            (unless (org-find-visit-headline it)
              (insert (format "* %s\n" it))
              (org-do-demote)))
          (org-find-visit-headline (car (last headings)))))
      (let ((entry-level (if (s-present-p subtree-path) 2 1)))
        (insert (format "%s* %s\n" (make-string (1- entry-level) ?*) entry-title))
        (insert (format "%s* Replay\n" (make-string entry-level ?*)))
        (insert replay-link)
        (insert "\n")
        (insert (format "%s* Narrative\n" (make-string entry-level ?*)))
        (insert "#+begin_src org\n")
        (insert (org-escape-code-in-string narrative-content))
        (insert "\n#+end_src\n"))
      (save-buffer)
      (message "Saved to %s" me-diary-file))))

(provide 'me-narrative)
;;; me-narrative.el ends here
