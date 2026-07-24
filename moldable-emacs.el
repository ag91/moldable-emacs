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

;; This is an extension of Emacs aiming to enable Moldable
;; Development.  Or better still, aiming to make you a better story
;; teller when you deal with code.

;;; Code:

(require 'dash)
(require 's)
(require 'async)
(require 'thunk)
(require 'cl-lib)
(require 'me-utils)
(require 'me-tree)
(require 'me-org)
(require 'me-mold-data)
(require 'me-examples)
(require 'me-inspector)
(require 'me-notes)
(require 'me-analysis)

(defgroup moldable-emacs nil
  "Customize group for Moldable-Emacs."
  :group 'convenience
  :prefix "me-")

(defcustom me-i-know-what-i-am-doing
  nil
  "Set this to t if don't need to see tutorials."
  :group 'moldable-emacs)

(defcustom me-files-with-molds
  (--map
   (concat
    (file-name-directory load-file-name) ; https://stackoverflow.com/questions/26991001/elisp-get-path-to-file-relative-to-script
    it)
   (list
    "molds/core.el"
    "molds/contrib.el"
    "molds/playgrounds.el"))
  "Files containing molds."
  :group 'moldable-emacs)

(defcustom me-molds-debug-on
  nil
  "Toggle for debugging information."
  :group 'moldable-emacs)

(defun me-setup-molds ()
  "Load molds from `me-files-with-molds'."
  (-each me-files-with-molds #'load-file))

























(defvar me-available-molds nil "Available molds.")

(defvar me-mold-history nil "List of molds produced.")

(defvar me-mold-before-hook nil "Hooks to run before running a mold.")

(defvar me-mold-after-hook nil "Hooks to run after running a mold.")

(defvar me-mold-before-mold-runs-hook nil "Hooks to run before the chosen mold runs.")


(defun me-interpret-given (mold)
  "Interpret MOLD :given clause into a sexp to run."
  (me-get-in mold '(:given :fn)))

(defun me-async-all (producers callback)
  "Call each function in PRODUCERS with a callback, in parallel.
When all producers have called their callback, call CALLBACK with
a list of their results in order.

>> (me-async-all (list (lambda (cb) (funcall cb 1)) (lambda (cb) (funcall cb 2))) (lambda (r) (setq test/r r)))
=> :assert (equal test/r (list 1 2))"
  (let ((results (make-vector (length producers) nil)))
    (--each-indexed producers
      (funcall
       it
       (lambda (result)
         (aset results it-index result)
         (when (equal (length results) (length producers))
           (funcall callback (append results nil))
           ))))))

(defun me-interpret-then (mold)
  "Interpret MOLD :then clause into a sexp to run."
  (let ((then (plist-get mold :then)))
    (cond
     ;; when :async is defined, we expect a callback (eg :async
     ;; (lambda (cb) <async code>)) and we will show a placeholder to
     ;; not block Emacs
     ((ignore-errors (car (plist-get then :async)))
      (let ((bindings (plist-get then :async)))
        `(progn
           (switch-to-buffer
            (get-buffer-create buffername))
           (with-current-buffer buffername
             (erase-buffer)
             (insert (format "Loading %s contents..." ,(plist-get mold :key))))
           (me-async-all
            (list ,@(mapcar #'cadr bindings))
            (lambda (results)
              (let ,(--map-indexed
                     (list (car it) `(nth ,it-index results))
                     bindings
                     )
                (with-current-buffer buffername (erase-buffer))
                ,(plist-get then :fn)
                ))))))
     ((-contains-p then :fn)
      `(progn
         (get-buffer-create buffername)
         ,(plist-get then :fn)
         (ignore-errors
           (switch-to-buffer-other-window
            (get-buffer buffername))))))))

(defun me-mold-buffername (mold)
  "Get the resulting buffer name of MOLD.
When MOLD has a :when clause, skip the timestamp so auto-refresh can reuse the same buffer."
  (concat "*moldable-emacs-" (or (plist-get mold :buffername) (plist-get mold :key))
          (unless (plist-get mold :when) (concat "-" (format-time-string "%Y%m%d%H%M%S")))
          "*"))

(defmacro me-with-mold-let (mold &rest clause) ;; TODO this must evaluate only once any time is called AND needs to make evaluation of bindings lazy?
  (declare (indent defun))
  "Wrap BODY in a let with :let and :buffername of MOLD, plus add the body for CLAUSE."
  (let ((m (-clone mold))) ;; for some strange reason, it seems that a mold with (:let ((1 ..) (2 ..) (3 ..))) ends up with (:let ((1 ..))) if I use thunk-let* on the original mold, so I clone it
    `(funcall
      (lambda (m clause)
        (eval
         `(progn
            (let ((buffername ,(me-mold-buffername m)))
              (,(if (ignore-errors (eq (car clause) :then))
                    'let*
                  'thunk-let*)
               (,@(plist-get m :let))
               (pcase ',clause
                 ('(:given) ,(me-interpret-given m))
                 ('(:then) ,(me-interpret-then m))
                 (_ ,@clause)))))
         't))
      ,m
      ',clause)))
(put 'me-with-mold-let 'lisp-indent-function 1)

;; (me-print-to-buffer (let ((mold (me-find-mold "PlistToJson")))
;;                       (me-with-mold-let mold
;;                                         :then))
;;                     (get-buffer-create "bla"))


(defun me-mold-run-given (mold)
  "Run MOLD :given."
  (unless (me-get-in mold '(:given :fn)) (error "For now all molds need to declare :given with :fn"))
  (me-with-mold-let (-clone mold)
    :given))

(defvar me-usable-mold-stats nil)
(defun me-mold-specificity (mold)
  "An attempt to quantify how specific a MOLD is in this context.

We score to the specificity of the predicates in the :given (like
checking for a major mode has more weight than checking for a dependency
on the system because you must have a specific kind of buffer open,
while the dependency is always on the system.)

>> (me-mold-specificity '(:given (:fn (and (eq major-mode 'csv-mode) (me-require 'bla)))))
=> 7"
  ;; (s-count-matches "(" (format "%s" (let* ((given (plist-get mold :given)))
  ;;                                     (if (ignore-errors (equal 'me-mold-run-given (car (nth 1 given))))
  ;;                                         (plist-get (eval (nth 1 (nth 1 given))) :given)
  ;;                                       given))))
  (let* ((given-fn (me-get-in mold '(:given :fn)))
         (given-fn-str (format "%s" given-fn)))
    (cond
     ((null given-fn) 0)
     ((eq given-fn t) 0)
     ((equal given-fn ''t) 0)
     ((and (listp given-fn)
           (memq (car given-fn) '(and or)))
      (+
       ;; how many statements in the :given
       (length (cdr given-fn))
       ;; +1 for weak requirements
       (s-count-matches (rx (or
                             "(or"
                             "me-require"
                             "executable-find"))
                        given-fn-str)
       ;; +2 for strong requirements
       (* 2 (s-count-matches (rx (or
                                  "(and"
                                  "major-mode"
                                  "(buffer-name)"
                                  ))
                             given-fn-str))))
     (t 1))
    )
  )


(defun me-usable-molds (&optional molds buffer)
  "Return the usable molds among the `me-available-molds'.
Optionally you can pass your own candidate MOLDS.
Optionally you can pass a BUFFER to use instead of the `current-buffer'."
  (let ((_ (setq me-usable-mold-stats nil))
        (molds (or molds me-available-molds))
        (buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (--> molds
           (--filter
            (let* ((beginning (current-time))
                   (result (save-excursion
                             (condition-case err
                                 (me-mold-run-given it)
                               (error (message "me-usable-molds: error in :given of %s:\n   %s" (plist-get it :key) err))))) ; TODO composite molds
                   (ending (current-time))
                   (_ (when me-molds-debug-on
                        (let ((key (plist-get it :key))
                              (expended-time (time-to-seconds
                                              (time-subtract
                                               ending
                                               beginning))))
                          (add-to-list 'me-usable-mold-stats (list :mold key
                                                                   :time
                                                                   expended-time))
                          (when (>= expended-time 1)
                            (warn
                             (button-buttonize
                              (format "%s took over 1 sec: %s" key expended-time)
                              `(lambda (x)
                                 (me-goto-mold-source ,key)))))))))
              result) ;; TODO run this in parallel when time goes over 100ms)
            it)
           ;; sort by specificity of molds: TODO using n of parentheses in :then as a shortcut
           (--sort (> (me-mold-specificity it)
                      (me-mold-specificity other))
                   it)))))

(defun me-usable-p (mold-key)
  "Check if MOLD-KEY mold is usable."
  (= (length
      (-non-nil
       (me-usable-molds
        (list (me-find-mold mold-key)))))
     1))

(defun me-mold-run-then (mold)
  "Run MOLD :then."
  (unless (me-get-in mold '(:then :fn)) (error "For now all molds need to declare :then with :fn"))
  (me-with-mold-let mold :then))

(defvar me-mold-whens nil "All :when clauses of molds to check periodically.")

(defvar me-mold-completion-history nil "This holds completion history to order molds by usage.")

(defun me-mold (&optional mold-key view-fn)
  "Propose a list of available molds for the current context.
Use MOLD-KEY as chosen mold when it is provided and usable.
Use VIEW-FN to show result buffer when provided."
  (interactive)
  (run-hooks 'me-mold-before-hook)
  (let* ((beginning (current-time))
         (molds (me-usable-molds))
         (keys (--map (plist-get it :key) molds))
         (ending (current-time))
         (_ (when me-molds-debug-on
              (message "Finding molds took %s seconds in total." (time-to-seconds
                                                                  (time-subtract
                                                                   ending
                                                                   beginning))))))
    (--> keys
         (or (when (-contains-p keys mold-key) mold-key)
             (completing-read
              "Pick the mold you need:"
              it
              nil
              t
              nil
              'me-mold-completion-history))
         (-find
          (lambda (x)
            (string=
             (plist-get x :key)
             it))
          molds)
         (funcall
          (lambda (mold)
            (--each
                me-mold-before-mold-runs-hook
              (funcall it mold))
            mold)
          it)
         me-mold-run-then)              ; TODO how can I use VIEW-FN ?
    (run-hooks 'me-mold-after-hook)))

(defun me-add-when-to-periodic-check (mold)
  "Add MOLD :when clause to `me-mold-whens'."
  (-when-let* ((w (plist-get mold :when))
               (mold-b (me-mold-buffername mold))
               (current-b (buffer-name)))
    (setq me-mold-whens (-distinct
                         (cons
                          (list
                           :when w
                           :mold-buffer mold-b
                           :mold-key (plist-get mold :key)
                           :current-buffer current-b)
                          me-mold-whens)))))

(add-hook 'me-mold-before-mold-runs-hook 'me-add-when-to-periodic-check)

(defun me-get-visible-buffers ()
  "Return buffer names that are visible now."
  (let (result)          ; taken from helm-buffers-get-visible-buffers
    (walk-windows
     (lambda (x)
       (push (buffer-name (window-buffer x)) result))
     nil 'visible)
    result))

(defun me-run-whens ()
  "Run molds :then clauses for `me-mold-whens' clauses that are satisfied."
  (--each me-mold-whens
    (save-excursion
      (when (and
             ;; both original buffer are visible: it means I am looking at them and I want automatic updates
             (-contains? (me-get-visible-buffers) (plist-get it :mold-buffer))
             (-contains? (me-get-visible-buffers) (plist-get it :current-buffer))
             ;; the when clause is satisfied
             (eval (me-get-in it '(:when :fn))))
        ;; save current window config
        (let ((window-config (current-window-configuration)))
          ;; go to :current-buffer
          (switch-to-buffer (plist-get it :current-buffer))
          ;; run the :then clause of :mold-key mold
          (message "Running then in buffer %s" (buffer-name) )
          (me-mold-run-then (me-find-mold (plist-get it :mold-key)))
          ;; restore old window config
          (set-window-configuration window-config))))))

(defcustom me-no-when-updates nil
  "When non-nil, it prevents automatic refresh of molds.
When a :when clause is defined on the mold and the relevant buffers are visible,
`moldable-emacs' tries to refresh the mold according to the `:when' clause trigger logic.")

(unless me-no-when-updates (run-with-idle-timer 0.8 t 'me-run-whens))

(defun me-mold-compose-molds (mold1 mold2)
  "Compose MOLD1 and MOLD2 in a new mold."
  `(
    :key ,(format
           "CompositionOf%sAnd%s"
           (plist-get mold1 :key)
           (plist-get mold2 :key))
    :given (:fn (me-mold-run-given ',mold1)) ;; we need me-mold-run-given because we need to propagate the :let bindings
    :then (:fn
           (progn (me-mold-run-then ',mold1)
                  (me-mold-run-then ',mold2)
                  ;; (delete-window (get-buffer-window (plist-get ',mold1 :buffername)))
                  (switch-to-buffer buffername)
                  (kill-buffer-and-window)
                  (rename-buffer buffername)
                  (switch-to-buffer buffername)))))

(defun me-mold-compose (m1 m2 &optional props)
  "Compose M1 and M2 in a single mold.
Add PROPS (e.g.,  `(:docs \"...\" :examples nil)') to it."
  (let ((mold1 (if (stringp m1) (me-find-mold m1) m1))
        (mold2 (if (stringp m2) (me-find-mold m2) m2)))
    (if (and mold1 mold2)
        (let ((result (me-mold-compose-molds mold1 mold2)))
          (--each props
            (plist-put result (nth 0 it) (nth 1 it)))
          result)
      (error (format "Could not find molds, check out: %s." (list m1 m2))))))

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

(defvar me-trace nil
  "Current trace being recorded, or nil if not tracing.
A trace is a plist with :steps, a list of step plists.
Each step has :name, :data, :source, and :ts (timestamp).
Inspired by the Chrome Trace Event Format (trace-event-format).")

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

(defmacro me-with-tracing (&rest body)
  "Evaluate BODY with tracing enabled.
Each `me-trace' call inside BODY records a step.
Returns the trace plist as `self' in the result buffer."
  `(let ((me-trace (list :steps nil)))
     ,@body
     me-trace))

(defcustom me-diary-file
  (expand-file-name "diary.org" user-emacs-directory)
  "Default file for saving moldable-emacs narratives as diary entries."
  :group 'moldable-emacs
  :type 'file)

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



(defcustom me-playground-molds-file
  (concat (file-name-directory load-file-name) "molds/playgrounds.el")
  "File where molds extracted from the Playground are inserted.
This file should be listed in `me-files-with-molds' so extracted
molds become available after `me-setup-molds'."
  :group 'moldable-emacs
  :type 'string)

(defun me--playground-buffer-p ()
  "Return non-nil if the current buffer is a Playground mold buffer."
  (ignore-errors
    (and mold-data
         (stringp (plist-get mold-data :mold))
         (s-starts-with-p "Playground" (plist-get mold-data :mold)))))

(defun me--infer-given-from-mold-data ()
  "Infer a `:given' plist for a mold extracted from the Playground.
Uses the current buffer's `mold-data' to suggest a predicate that
matches the context the Playground was invoked from."
  (let* ((old-mold (plist-get mold-data :old-mold))
         (old-mode (plist-get mold-data :old-mode))
         (old-self (plist-get mold-data :old-self)))
    (cond
     ((and old-mold old-self (listp old-self))
      '(:fn (ignore-errors (and self (listp self)))))
     ((and old-mold old-self (me-plistp old-self))
      '(:fn (ignore-errors (and self (listp self) (me-plistp self)))))
     (old-mold
      '(:fn (ignore-errors self)))
     (old-mode
      `(:fn (eq major-mode ',old-mode)))
     (t
      '(:fn t)))))

(defun me--playground-user-code ()
  "Extract the user's Elisp code from the current Playground buffer.
Strips the leading tip comments and blank lines inserted by the
Playground mold."
  (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
    (--> contents
         (s-split "\n" it)
         (--drop-while (or (s-blank-p it) (s-starts-with-p ";;" it)) it)
         (s-join "\n" it)
         (s-trim it))))

(defun me-extract-mold-from-playground ()
  "Extract the current Playground buffer's code as a reusable mold.
Reads the Elisp code in the Playground buffer, infers a `:given'
predicate from the current `mold-data', prompts for a `:key' and
`:docs', and inserts a `me-register-mold' form into
`me-playground-molds-file'."
  (interactive)
  (unless (me--playground-buffer-p)
    (user-error "Not in a Playground buffer"))
  (let* ((user-code (me--playground-user-code))
         (given (me--infer-given-from-mold-data))
         (key (read-string "Mold key: "))
         (docs (read-string "Docs: ")))
    (when (s-blank-p key)
      (user-error "Mold key cannot be empty"))
    (let ((mold-form
           (format "(me-register-mold\n :key %S\n :given %S\n :then (:fn\n        (let* ((result\n               (progn\n                 %s)))\n          (with-current-buffer buffername\n            (erase-buffer)\n            (emacs-lisp-mode)\n            (me-print-to-buffer result)\n            (setq-local self result))))\n :docs %S\n :examples nil)\n\n"
                   key
                   given
                   user-code
                   docs)))
      (unless (file-exists-p me-playground-molds-file)
        (with-temp-file me-playground-molds-file
          (insert ";;; playgrounds.el --- Molds extracted from the Playground -*- lexical-binding: t; -*-\n\n")
          (insert "(require 'moldable-emacs)\n\n")
          (insert ";;; Code:\n\n")
          (insert ";;; playgrounds.el ends here\n")))
      (with-current-buffer (find-file-noselect me-playground-molds-file)
        (goto-char (point-min))
        (if (search-forward ";;; playgrounds.el ends here" nil t)
            (progn
              (goto-char (match-beginning 0))
              (insert mold-form))
          (goto-char (point-max))
          (insert mold-form))
        (save-buffer)
        (display-buffer (current-buffer)))
      (load-file me-playground-molds-file)
      (message "Extracted mold %s and loaded it" key))))



(add-hook 'me-mold-after-hook #'me-set-self-mold-data -100)

(defvar me-last-example nil "Last automatically generated example for mold.
This should simplify the testing and documentation of molds.")

(defcustom me-example-resource-dir
  (concat (file-name-directory load-file-name) "resources/")
  "Directory containing resources for examples (like media files)."
  :group 'moldable-emacs
  :type 'string)





(add-hook 'me-mold-before-hook #'me-record-given-of-example)

(add-hook 'me-mold-after-hook #'me-record-then-of-example)


(defun me-warn-on-run-if-no-example (mold)
  "Emit warning if MOLD has no examples."
  (unless (or (not me-molds-debug-on) (plist-get mold :examples))
    (warn
     (button-buttonize
      (format "Mold %s has no examples! Would you mind to add one?\nYou can use TODO now to add the last usage as an example.\n" (plist-get mold :key))
      `(lambda (x)
         (me-goto-mold-source ,(plist-get mold :key)))))))

(defun me-warn-on-run-if-no-docs (mold)
  "Emit warning if MOLD has no examples."
  (unless (or (not me-molds-debug-on) (plist-get mold :docs))
    (warn
     (button-buttonize
      (format "Mold %s has no docs! Would you mind to add a line to tell what it is for?\n" (plist-get mold :key))
      `(lambda (x)
         (me-goto-mold-source ,(plist-get mold :key)))))))

(add-hook 'me-mold-before-mold-runs-hook #'me-warn-on-run-if-no-example)
(add-hook 'me-mold-before-mold-runs-hook #'me-warn-on-run-if-no-docs)

(defmacro me--given (given &rest body)
  "Setup according to GIVEN and run BODY.
GIVEN is a plist with :type, :name, :mode, :contents, :point,
and optionally :mold-data (a plist to set as buffer-local
`mold-data' for testing molds that depend on it)."
  `(let* ((given (eval ',given))
          (type (plist-get  given :type))
          (name (plist-get given :name))
          (mode (plist-get given :mode))
          (point (plist-get given :point))
          (mold-data-to-set (plist-get given :mold-data))
          (body ',body)
          (contents (if (eq mode 'image-mode)
                        (with-temp-buffer
                          (insert-file-contents-literally (plist-get given :contents))
                          (buffer-substring-no-properties (point-min) (point-max)))
                      (plist-get given :contents))))
     (eval (if (equal type 'buffer)
               `(with-temp-buffer
                  (rename-buffer ,name "-new")
                  (insert ,contents)
                  (,(if mode mode 'fundamental-mode))
                  (if ,point (goto-char ,point) (goto-char (point-min)))
                  (when ,mold-data-to-set
                    (setq-local mold-data ,mold-data-to-set))
                  ,@body)
             `(with-temp-file ,name
                (let ((buffer-file-name ,name ))
                  (rename-buffer (file-name-nondirectory ,name) "-new")
                  (insert ,contents)
                  (,(if mode mode 'fundamental-mode))
                  (if ,point (goto-char ,point) (goto-char (point-min)))
                  (when ,mold-data-to-set
                    (setq-local mold-data ,mold-data-to-set))
                  ,@body))))))
(put 'me--given 'lisp-indent-function 1)









(defun me-test-mold-examples (mold)
  "Check that all MOLD's examples are working."
  (--reduce
   (and it acc)
   (--map
    (me-test-example it (lambda () (me-mold-run-then mold)))
    (plist-get mold :examples))))

;; (me-test-mold-examples (me-find-mold "Playground"))

(defun me-example-to-docstring (example)
  "Make a string for EXAMPLE."
  (let* ((start (plist-get example :given))
         (start-buffer-or-file (plist-get start :type))
         (start-name (plist-get start :name))
         (start-contents (plist-get start :contents))
         (end (plist-get example :then))
         (end-buffer-or-file (plist-get end :type))
         (end-name (plist-get end :name))
         (end-contents (plist-get end :contents)))
    (format
     "\n\nGiven the \"%s\" %s with the following contents:\n\n----------\n\n%s\n\n----------\n\nThe mold returns the \"%s\" %s with the following contents:\n\n----------\n\n%s\n\n----------"
     start-name
     start-buffer-or-file
     start-contents
     end-name
     end-buffer-or-file
     end-contents)))



(defun me-mold-doc (mold-key)
  "Produce structured doc for a mold identified by MOLD-KEY."
  (--> mold-key
       me-find-mold
       (list
        :title
        (format "Documentation about %s mold" (plist-get it :key))
        :documentation
        (concat (plist-get it :docs)
                (let ((examples (plist-get it :examples)))
                  (when (> (length examples) 0)
                    (me-example-to-docstring (car examples))))))))

(defun me-mold-docs ()
  "Propose a list of available views for the current context."
  (interactive)
  (let* ((molds (me-usable-molds))
         (keys (--map (plist-get it :key) molds)))
    (--> keys
         (completing-read
          "Pick the view you need:"
          it)
         me-mold-doc
         (progn                         ;; TODO this is a bit poor. Maybe use an Org Mode file?
           (switch-to-buffer (get-buffer-create (plist-get it :title)))
           (erase-buffer)
           (insert (plist-get it :documentation))))))

(defun me-show-example (example run-fn)
  "Run RUN-FN in the EXAMPLE."
  (let* ((name (plist-get example :name))
         (start (plist-get example :given))
         (end (plist-get example :then)))
    (me--given start
               (funcall run-fn)
               (me-then-assert end))))


(defun me-demo-example (example)
  "Demo EXAMPLE in a dedicated frame."
  (let* ((name (plist-get example :name))
         (given (plist-get example :given))
         (given-name (plist-get given :name))
         (given-mode (plist-get given :mode))
         (given-contents (if (eq given-mode 'image-mode)
                             (with-temp-buffer
                               (insert-file-contents-literally (plist-get given :contents))
                               (buffer-substring-no-properties (point-min) (point-max)))
                           (plist-get given :contents)))
         (then (plist-get example :then))
         (then-name (plist-get then :name))
         (then-mode (plist-get then :mode))
         (then-contents (if (eq then-mode 'image-mode)
                            (with-temp-buffer
                              (insert-file-contents-literally (plist-get then :contents))
                              (buffer-substring-no-properties (point-min) (point-max)))
                          (plist-get then :contents)))
         (frame (make-frame `((name . ,name) (width . 100) (height . 70) (fullscreen . nil)))))
    (x-focus-frame frame)
    (select-frame frame)
    (split-window-horizontally)
    (switch-to-buffer given-name)
    (erase-buffer)
    (insert given-contents)
    (funcall given-mode)
    (other-window 1)
    (switch-to-buffer then-name)
    (erase-buffer)
    (insert then-contents)
    (funcall then-mode)))

;; (me-demo-example '(:name "some example" :given (:type buffer :name "somebuffer" :contents "some contents") :then (:type file :name "/tmp/somefile.txt" :contents "some new contents")))

(defun me-mold-demo (mold)
  "Demo MOLD using its examples."
  (if-let ((mold mold)
           (example (nth 0 (plist-get mold :examples))))
      (me-demo-example example)
    (error "No example available for this mold to demo")))

(defun me-mold-demo-by-key (key)
  "Demo mold after find it using its KEY."
  (me-mold-demo (me-find-mold key)))

(defun me-open-node-at-point (node)
  "Follow node at point."
  (interactive
   (list (list-at-point)))
  (let* ((buffer (plist-get node :buffer))
         (file (plist-get node :buffer-file)))
    (if (and node buffer (plist-get node :begin))
        (if (-contains-p (--map (format "%s" it) (buffer-list)) buffer)
            (progn
              (switch-to-buffer-other-window buffer)
              (goto-char (plist-get node :begin)))
          (when file (find-file file))
          (goto-char (plist-get node :begin)))
      (error "Cannot follow node %s!" node))))

(defun me-find-mold (key)
  "Find mold for KEY."
  (--find (equal key (plist-get it :key)) me-available-molds))

(defcustom me-enable-history 't
  "Keeps history for current session, if defined."
  :group 'moldable-emacs)
(defvar me-current-history-index 0 "Keeps track of where you are in history.")

(defun me-save-buffer-in-history ()
  "Enable keeping history for current session."
  (unless (equal (plist-get (-last-item me-mold-history) :buffername)
                 (buffer-name))
    (setq me-mold-history
          (append
           (-take me-current-history-index me-mold-history)
           (list (list :buffername (buffer-name) :date (format-time-string "%FT%T%z")))))
    (setq me-current-history-index (length me-mold-history))))

(when me-enable-history (progn
                          (add-hook 'me-mold-before-hook #'me-save-buffer-in-history)
                          (add-hook 'me-mold-after-hook #'me-save-buffer-in-history)))

(defun me-go-back ()
  "Go back to previous mold."
  (interactive)
  (ignore-errors
    (--> me-mold-history
      (nth
       (- me-current-history-index 1)
       it)
      (plist-get it :buffername)
      switch-to-buffer)
    (setq me-current-history-index (- me-current-history-index 1))
    (message "Back to %s" (buffer-name))))

(defun me-go-forward ()
  "Go back to next mold."
  (interactive)
  (let ((current-index (--find-index (string= (plist-get it :buffername) (buffer-name)) me-mold-history)))
    (ignore-errors
      (--> me-mold-history
        (nth
         (+ current-index 1)
         it)
        (plist-get it :buffername)
        switch-to-buffer)
      (setq me-current-history-index (+ current-index 1))
      (message "Forward to %s" (buffer-name)))))

(defun me-add-to-available-molds (mold)
  "Add MOLD to `me-available-molds' and so usable by `me-mold'."
  (let ((-compare-fn (lambda (x y) (equal (plist-get x :key) (plist-get y :key))))
        (mold (append mold (list :origin (me-find-origin-file-of-mold (plist-get mold :key))))))
    (setq me-available-molds
          (-distinct (add-to-list 'me-available-molds mold)))))

(defvar me-before-register-mold-hook nil "Hooks to run before a mold is registered.")

(defun me-find-origin-file-of-mold (key)
  "Find the file that defines the mold identified by KEY."
  (--find
   (with-current-buffer (find-file-noselect it)
     (save-excursion
       (goto-char (point-min))
       (ignore-errors (search-forward (concat "\"" key "\"")))))
   me-files-with-molds))


(defmacro me-register-mold (&rest mold)
  "Register MOLD."
  `(progn
     (--each me-before-register-mold-hook (funcall it ',mold))
     (me-add-to-available-molds ',mold)))
(put 'me-register-mold 'lisp-indent-function 1)

(defun me-find-relative-test-report (filepath)
  "Find Clojure test report for FILEPATH." ;; TODO refactor a bit for supporting Clojure with https://github.com/ruedigergad/test2junit
  (let* ((_report-directory (concat (locate-dominating-file (file-name-directory filepath) "target") "target/test-reports"))
         (report-directory
          (if (string= "clj" (file-name-extension  filepath))
              (concat _report-directory "/xml")
            _report-directory))
         (_filename (file-name-base filepath))
         (filename
          (if (string= "clj" (file-name-extension  filepath))
              (s-replace "_test" "-test" _filename)
            _filename)))
    (--> report-directory
         directory-files
         (--find
          (s-ends-with-p (concat filename ".xml") it)
          it)
         (concat report-directory "/" it))))

(defun me-make-elisp-file-link (description target &optional link-type)
  "Make Org file link with DESCRIPTION and TARGET.
Optionally pass the LINK-TYPE instead of file.

>> (me-make-elisp-file-link \"description\" \"/tmp/test.el::10\")
=> \"[[file:/tmp/test.el::10][description]]\"

>> (me-make-elisp-file-link \"description\" \"(goto-char 10)\" \"elisp\")
=> \"[[elisp:(goto-char 10)][description]]\""
  (format "[[%s:%s][%s]]" (or link-type "file") target description))

(defun me-make-elisp-navigation-link (name target)
  "Make an Elisp Org link that navigates to a position of NAME in TARGET.

TARGET can be a buffer, file or tree node.

; invalidated the test because I didn't store the file
> (me-make-elisp-navigation-link \"defmacro\" \"/tmp/test.el\")
> \"[[elisp:(progn (find-file-other-window \\\"/tmp/test.el\\\") (goto-char 441))][defmacro]]\"

> (me-make-elisp-navigation-link \"defmacro\" \"test.el\")
> \"[[elisp:(progn (switch-to-buffer-other-window \\\"test.el\\\") (goto-char 441))][defmacro]]\"

>> (me-make-elisp-navigation-link \"defmacro\"
  '(:type symbol
    :text \"defmacro\"
    :begin 433
    :end 441
    :buffer \"test.el\"
    :mode emacs-lisp-mode
    :level 1))
=> \"[[elisp:(progn (switch-to-buffer-other-window \\\"test.el\\\") (goto-char 433))][defmacro]]\"

>> (me-make-elisp-navigation-link \"defmacro\"
  '(:type symbol
    :text \"defmacro\"
    :begin 433
    :end 441
    :buffer \"test.el\"
    :buffer-file \"/tmp/test.el\"
    :mode emacs-lisp-mode
    :level 1))
=> \"[[elisp:(progn (find-file-other-window \\\"/tmp/test.el\\\") (goto-char 433))][defmacro]]\""
  (let* ((filep (or (plist-get target :buffer-file) (ignore-errors (file-exists-p target))))
         (pos-file (if filep
                       (or
                        (and (plist-get target :begin) (list (plist-get target :begin) (plist-get target :buffer-file)))
                        (with-temp-buffer
                          (insert-file-contents-literally target)
                          (goto-char (point-min))
                          (list (or (search-forward name nil 'noerror) 1) target)))
                     (or
                      (and (plist-get target :begin) (list (plist-get target :begin) (plist-get target :buffer)))
                      (save-excursion
                        (with-current-buffer target
                          (goto-char (point-min))
                          (list (or (search-forward name nil 'noerror) 1) target)))))))
    (me-make-elisp-file-link
     (s-replace "\n" "" name)
     (format
      "(progn (%s \"%s\") (goto-char %s))"
      (if filep "find-file-other-window" "switch-to-buffer-other-window")
      (nth 1 pos-file)
      (nth 0 pos-file))
     "elisp")))

(defun me-make-elisp-buffer-navigation-link (name buffer-name)
  "Make an Elisp Org link that navigates to a position of NAME in BUFFER-NAME."
  (let* ((pos (with-current-buffer buffer-name
                (goto-char (point-min))
                (or (search-forward (if (s-contains-p "\"" name) (prin1-to-string name) name) nil 'noerror) 1))))
    (me-make-elisp-file-link
     name
     (format
      "(progn (switch-to-buffer-other-window \"%s\") (goto-char %s))"
      buffer-name
      pos)
     "elisp")))

(defun me-color-string (str color)
  "Color STR with COLOR."
  (propertize
   str
   'display
   (propertize
    str
    'face
    (list :background color))))

;; https://hungyi.net/posts/org-mode-subtree-contents/
(defun me-org-copy-subtree-contents (&optional buffer position)
  "Get the content text of the subtree at point and add it to the `kill-ring'.
Excludes the heading and any child subtrees.
Optionally select BUFFER and POSITION."
  (with-current-buffer (or buffer (current-buffer))
    (when position (goto-char position))
    (if (org-before-first-heading-p)
        (message "Not in or on an org heading")
      (save-excursion
        ;; If inside heading contents, move the point back to the heading
        ;; otherwise `org-agenda-get-some-entry-text' won't work.
        (unless (org-on-heading-p) (org-previous-visible-heading 1))
        (let ((contents (substring-no-properties
                         (org-agenda-get-some-entry-text
                          (point-marker)
                          most-positive-fixnum))))
          contents)))))

(defun me-org-to-flatten-tree (buffername)
  "Convert Org BUFFERNAME to a list of plists."
  (--map (append
          (list :type 'org)
          (plist-put (cadr it) :title nil)
          `(:buffer ,(buffer-name))
          `(:buffer-file ,(buffer-file-name))
          `(:text ,(me-org-copy-subtree-contents (plist-get it :begin))))
         (org-ql-query :select 'element :from (list buffername))))

(defun me-register-mold-by-key (key mold)
  "Register composition MOLD with KEY."
  (me-add-to-available-molds (plist-put mold :key key)))

(defvar me-last-used-mold nil "Keep the `:key' of last used mold.")

(defun me-set-last-mold (mold)
  "Set last used MOLD."
  (setq me-last-used-mold (plist-get mold :key)))

(add-hook 'me-mold-before-mold-runs-hook #'me-set-last-mold)





(defun me-mold-insert-name ()
  "Insert a mold name at point."
  (interactive)
  (--> me-available-molds
       (--map (plist-get it :key) it)
       (completing-read
        "Insert at point the following mold name:"
        it)
       insert))


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

(defun me-calc-numeric-p (text)
  "Check if TEXT is a numeric arithmetic expression `calc' can work with."
  (let ((calc-eval-error 't)) (ignore-errors (calc-eval text 'num)))
  )

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

(defcustom me-note-file-store "~/workspace/agenda/moldableNotes.el"
  "Store for notes."
  :group 'moldable-emacs)

(defvar me-notes nil "Prototype of notes.")











(defun me-ask-for-todo-details-according-to-context (note)
  "Ask for NOTE details."
  (let ((text (read-string "Note:")))
    (plist-put note :then `(:string ,text :state todo))))

;; https://stackoverflow.com/questions/21486934/file-specific-key-binding-in-emacs












;; some functionality to edit nodes!!












(defun me-transit-node-buffer (node target-buffer &optional point)
  "Create a transition changing buffer's NODE to TARGET-BUFFER."
  (list
   :before node
   :after (plist-put
           (plist-put
            (plist-put
             (-copy node)
             :buffer
             target-buffer)
            :begin
            (or point
                ;; default to the last position in target buffer since `me-add-node' adds using :begin
                (with-current-buffer target-buffer (point-max))))
           :text
           ;; the definitions don't get the final newline, we add one ahead
           (concat "\n" (plist-get node :text)))))

(defun me-transit-node-buffers (nodes target-buffer &optional point)
  "Create transitions moving NODES to TARGET-BUFFER."
  (--map (me-transit-node-buffer it target-buffer point) nodes))






(defmacro me-with-url-contents (url &rest body)
  "Retrieve URL contents and run BODY in buffer."
  `(with-current-buffer (url-retrieve-synchronously ,url)
     (goto-char url-http-end-of-headers)
     (delete-region (point-min) (point))
     ,@body))
(put 'me-with-url-contents 'lisp-indent-function 1)

(defun me-get-json-from-url (url)
  "Retrieve json from URL as a plist."
  (me-with-url-contents url
                        (save-excursion
                          (let ((json-object-type 'plist)
                                (json-array-type 'list))
                            (goto-char (point-min))
                            (json-read)))))
























;; organize screens better


(defun me-goto-mold-source (mold)
  "Go to source code of MOLD."
  (interactive
   (list
    (completing-read
     "Pick the mold you need:"
     (--map (plist-get it :key) me-available-molds))))
  (--> mold
       (-find
        (lambda (x)
          (string=
           (plist-get x :key)
           it))
        me-available-molds)
       (plist-get it :origin)
       (find-file it))
  (goto-char (point-min))
  (search-forward mold))


;; begin similar nodes



;; end similar nodes

;; begin elisp API
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
   :description (with-demoted-errors (me-elisp-description tree))
   :defcustoms (with-demoted-errors (me-elisp-defcustoms tree))
   :macros (with-demoted-errors (me-elisp-public-symbols (me-elisp-macros tree)))
   :functions (with-demoted-errors (me-elisp-public-symbols (me-elisp-functions tree)))))
;; end elisp API

(provide 'moldable-emacs)
;;; moldable-emacs.el ends here
