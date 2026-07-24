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
(require 'me-elisp-api)
(require 'me-narrative)
(require 'me-counting)

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
                             given-fn-str))
       ;; +5 if we have a matching major mode requirement
       (* 5 (s-count-matches (eval `(rx (or
                                         ,(format "(equal major-mode '%s)" major-mode)
                                         ,(format "(eq major-mode '%s)" major-mode)
                                         )))
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
                             (buttonize
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

(defvar me-trace nil
  "Current trace being recorded, or nil if not tracing.
A trace is a plist with :steps, a list of step plists.
Each step has :name, :data, :source, and :ts (timestamp).
Inspired by the Chrome Trace Event Format (trace-event-format).")

(defmacro me-with-tracing (&rest body)
  "Evaluate BODY with tracing enabled.
Each `me-trace' call inside BODY records a step.
Returns the trace plist as `self' in the result buffer."
  `(let ((me-trace (list :steps nil)))
     ,@body
     me-trace))


(add-hook 'me-mold-after-hook #'me-set-self-mold-data -100)

(defvar me-last-example nil "Last automatically generated example for mold.
This should simplify the testing and documentation of molds.")

(defun me-warn-on-run-if-no-example (mold)
  "Emit warning if MOLD has no examples."
  (unless (or (not me-molds-debug-on) (plist-get mold :examples))
    (warn
     (buttonize
      (format "Mold %s has no examples! Would you mind to add one?\nYou can use TODO now to add the last usage as an example.\n" (plist-get mold :key))
      `(lambda (x)
         (me-goto-mold-source ,(plist-get mold :key)))))))

(defun me-warn-on-run-if-no-docs (mold)
  "Emit warning if MOLD has no examples."
  (unless (or (not me-molds-debug-on) (plist-get mold :docs))
    (warn
     (buttonize
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

;; begin easy extract of playgrounds into molds
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
;; end

(provide 'moldable-emacs)
;;; moldable-emacs.el ends here
