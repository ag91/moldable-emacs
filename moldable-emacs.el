;;; moldable-emacs.el --- Moldable Development Extension -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Andrea

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
    "molds/contrib.el"))
  "Files containing molds."
  :group 'moldable-emacs)

(defcustom me-molds-debug-on
  nil
  "Toggle for debugging information."
  :group 'moldable-emacs)

(defun me-setup-molds ()
  "Load molds from `me-files-with-molds'."
  (-each me-files-with-molds #'load-file))

(defun me-get-in (plist keys)
  "Navigate PLIST's KEYS in sequence.
For example, (me-get-in '(:a (:b (:c 1))) '(:a :b :c)) yields 1."
  (ignore-errors
    (--reduce-from
     (if (numberp it)
         (nth it acc)
       (plist-get acc it))
     plist
     keys)))

;; (me-get-in '(:a (1 2 (:b result))) '(:a 2 :b))

(defun me-alist-get-in (alist keys)
  "Navigate an ALIST via KEYS.
Numbers in SYMBOLS are considered indeces of sequences."
  (ignore-errors
    (--reduce-from
     (if (numberp it)
         (nth it acc)
       (alist-get it acc))
     alist
     keys)))

;; (me-alist-get-in '((a . (1 2 3 ((c . result))))) '(a 3 c))

(defmacro me-with-file (file &rest body)
  "Open FILE, execute BODY close FILE if it was not already open."
  `(let ((old-buffer (current-buffer))
         (kill-buffer-p (not (get-file-buffer ,file))))
     (unwind-protect
         (progn
           (find-file ,file)
           ,@body)
       (progn
         (when kill-buffer-p (kill-buffer))
         (switch-to-buffer old-buffer)))))
(put 'me-with-file 'lisp-indent-function 1)

(defun me-async-map--finish (futures post-fn too-late-p poll-time)
  "Run FUTURES and apply POST-FN on their results.
Use TOO-LATE-P and POLL-TIME to stop."
  (if (not (-some #'null (mapcar #'async-ready futures)))
      (let ((results (--map
                      (let ((buf (process-buffer it)))
                        (with-current-buffer buf
                          (async-handle-result
                           #'identity
                           async-callback-value
                           (current-buffer))))
                      futures)))
        (funcall post-fn results))
    (if (funcall too-late-p)
        'interrupted
      (run-with-timer
       poll-time
       nil
       #'me-async-map--finish
       futures
       post-fn
       too-late-p
       poll-time))))

(defun me-async-map (fn els &optional post-fn poll-time timeout) ;; TODO maybe I can just use this https://github.com/chuntaro/emacs-promise
  "Run FN async on elements ELS.
Optionally define a POST-FN to run on the results of apply FN on ELS.
Optionally define a POLL-TIME to look for results and a TIMEOUT to fail."
  (let* ((start (current-time))
         (futures (mapcar
                   (lambda (el)
                     (async-start `(lambda ()
                                     (setq load-path ',load-path)
                                     (funcall ,fn ,el))))
                   els))
         (too-late-p
          `(lambda () (>= (time-to-seconds (time-since ',start)) (or ,timeout 300)))))
    (me-async-map--finish
     futures
     (or post-fn (lambda (results)
                   (message (format "me-async-map finished with the following results:\n%s" results))
                   'completed))
     too-late-p
     (or poll-time 1))))

;; (me-async-map
;;  (lambda (x) (make-directory x 't))
;;  (list "/tmp/bla" "/tmp/blo" "/tmp/blu")
;;  (lambda (_) (message "%s" (directory-files "/tmp"))))

(defun me-pmap (fn els &optional poll-time timeout) ;; TODO maybe I can just use this https://github.com/chuntaro/emacs-promise
  "Run FN in parallel on elements ELS.
Optionally define a POST-FN to run on the results of apply FN on ELS.
Optionally define a POLL-TIME to look for results and a TIMEOUT to fail."
  (let* ((start (current-time))
         (futures (mapcar
                   (lambda (el)
                     (async-start `(lambda ()
                                     (setq load-path ',load-path)
                                     (funcall ,fn (if ,(seqp el) ',el ,el)))))
                   els))
         (too-late-p
          `(lambda () (>= (time-to-seconds (time-since ',start)) (or ,timeout 300)))))
    (while (some #'null (mapcar #'async-ready futures))
      (when (funcall too-late-p) (error "Me-pmap has waited too long: timed out"))
      (sleep-for (or poll-time 0.2)))
    (--map
     (let ((buf (process-buffer it)))
       (with-current-buffer buf
         (async-handle-result
          #'identity
          async-callback-value
          (current-buffer))))
     futures)))


(defun me-print-to-buffer (object &optional buffer)
  "Print OBJECT in BUFFER without truncation."
  (let ((print-length nil)
        (eval-expression-print-length nil))
    (pp-display-expression object (or buffer (current-buffer)))))

(defun me-make-org-table (headlines objects)
  "Make an Org Table with OBJECTS formats and HEADLINES."
  (concat
   (concat "| " (s-join " | " (-map #'car headlines))  " |\n")
   (concat "|-" (format (s-repeat (- (length headlines) 1) "-+-")) "-|\n")
   (s-join
    "\n"
    (--map (concat
            "| "
            (s-join
             " | "
             (-map
              (lambda (headline)
                (let ((result (funcall (plist-get (cdr headline) :extractor) it))
                      (handler (plist-get (cdr headline) :handler)))
                  (if handler
                      (funcall handler result)
                    result)))
              headlines))
            " |")
           objects))))

(defun me-insert-string-table (table-string)
  "Insert TABLE-STRING in buffer.
Make sure table is also indented."
  (insert table-string)
  (save-excursion
    (search-backward "|" nil nil 2) ;; count 2 to avoid an extra (empty) row at the bottom
    (org-cycle))
  (setq-local org-confirm-elisp-link-function nil))

(defun me-insert-org-table (headlines objects)
  "Produce org table of OBJECTS formatting with HEADLINES."
  (me-insert-string-table (me-make-org-table headlines objects)))

(defun me-alist-to-lists-of-plist (alist)
  "Convert ALIST to a `plist'."
  (let ((keys (ignore-errors
                (and (= (length (car alist)) (length (-filter #'stringp (car alist))))
                     (--map (intern (concat ":" it)) (car alist))))))
    (if keys
        (--map (-flatten (-zip-lists keys it)) (cdr alist))
      alist)))


(defun me-org-table-to-plist (table-string)
  "Make TABLE-STRING a plist.

>> (me-org-table-to-plist \"| a | b |
|---+---|
| x | y |
| w | z |
\")
=> (:a (\"x\" \"w\") :b (\"y\" \"z\"))"
  (with-temp-buffer
    (save-excursion (insert table-string))
    (org-table-transpose-table-at-point)
    (let ((table (org-table-to-lisp))
          result)
      (dolist (r table result)
        (when (listp r) (setq result (plist-put result (intern (concat ":" (s-replace "\"" "" (car r)))) (cdr r))))))))

(defun me-org-table-to-flat-plist (table-string)
  "Convert Org mode table TABLE-STRING to a list of plists."
  (let* ((plist (me-org-table-to-plist table-string))
         (keys (-filter 'symbolp plist)))
    (--> keys
         (--map (-map (lambda (x) (list it (substring-no-properties x))) (plist-get plist it)) it)
         (apply '-zip it)
         (-map '-flatten it))))

(defun me-flat-org-table-to-string (flat-org-table)
  "Make a string out of FLAT-ORG-TABLE.

>> (me-flat-org-table-to-string '((:a 1 :b 2) (:a 3 :b 4)))
=> \"| a | b |
|--+--|
| 1 | 2 |
| 3 | 4 |\""
  (me-make-org-table
   (--map
    (list (substring (symbol-name it) 1) . (:extractor `(lambda (x) (format "%s" (plist-get x ,it)))))
    (-filter #'keywordp (car flat-org-table)))
   flat-org-table))
(defalias 'me-plist-table-to-org-table 'me-flat-org-table-to-string)

(defun me-insert-flat-org-table (flat-org-table)
  "Insert FLAT-ORG-TABLE in current buffer."
  (me-insert-string-table (me-flat-org-table-to-string flat-org-table)))

(defun me-org-tabletolisp-to-plist (org-table-to-lisp)
  "Create a plist of ORG-TABLE-TO-LISP obtained by `org-table-to-lisp' fn."
  (--> org-table-to-lisp
       (orgtbl-to-orgtbl it nil)
       (me-org-table-to-flat-plist it)))

(defun me-first-org-table (&optional buffer)
  "Find first org table.  Optionally in BUFFER."
  (ignore-errors
    (with-current-buffer (or buffer (current-buffer)) ;; TODO remove org links in table!
      (save-excursion
        (re-search-forward org-table-line-regexp nil t)
        (me-org-tabletolisp-to-plist (org-table-to-lisp))))))

(defun me-all-flat-org-tables (&optional buffer)
  "Find first org table.  Optionally in BUFFER."
  (ignore-errors
    (with-current-buffer (or buffer (current-buffer)) ;; TODO remove org links in table!
      (save-excursion
        (let (result)
          (while (and
                  (re-search-forward org-table-line-regexp nil t)
                  (goto-char (- (org-table-end) 1)))
            (setq result
                  (cons (me-org-tabletolisp-to-plist (org-table-to-lisp))
                        result)))
          result)))))

(defun me-types (tree)
  "List types in current syntax TREE.

>> (me-types '((:type a) (:type b)))
=> (a b)"
  (--> tree
       (--map (plist-get it :type) it)
       -distinct))

(defun me-by-type (type tree)
  "Filter TREE entries by TYPE.

>> (me-by-type 'a '((:type a :text \"hi\") (:type b)))
=> ((:type a :text \"hi\"))"
  (when (symbolp type)
    (--filter (eq (plist-get it :type) type) tree)))

(defun me-by-types (types tree)
  "Filter TREE entries by any of the TYPES."
  (--filter (-contains? types (plist-get it :type)) tree))

(defun me-count-by-key (key list)
  "Group LIST by KEY and count groups.

>> (me-count-by-key :a '((:a \"x\") (:a \"x\") (:a \"y\")))
=> ((:a \"x\" :count 2) (:a \"y\" :count 1))"
  (--> list
       (--group-by (plist-get it key) it)
       (--map (list key (car it) :count (length (cdr it))) it)
       (--sort (> (plist-get it :count) (plist-get other :count)) it)))

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

(defun me-extension-to-major-mode (extension)
  "Find `major-mode' for EXTENSION.
For example: \".scala\" => scala-mode."
  (cdr (--find (s-match (car it) extension) auto-mode-alist)))

(defun me-major-mode-to-tree-sitter-grammar (major-mode)
  "Find emacs-tree-sitter grammar for MAJOR-MODE."
  (alist-get major-mode tree-sitter-major-mode-language-alist))

(defun me-extension-to-tree-sitter-grammar (extension)
  "Find emacs-tree-sitter grammar for EXTENSION."
  (--> extension
       me-extension-to-major-mode
       me-major-mode-to-tree-sitter-grammar))

(defun me-filepath-to-flattened-tree (file &optional contents)
  "Return the flattened tree for FILE.
Optionally use CONTENTS string instead of file contents."
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
             me-mold-treesitter-to-parse-tree)))))

(defun me-nodes-with-duplication (self)
  "Find nodes that are duplicated for SELF."
  (-remove
   'null
   (--map
    (-flatten                           ; do not need enclosing list
     (let ((-compare-fn (lambda (a b) (string= (plist-get a :text) (plist-get b :text)))) ;; this is for making -distinct apply on the :text property
           (nodes-for-single-type (cdr it)))
       (--reduce-from  ; find duplicate
        (-remove-first ; by removing only the first matching node text
         (lambda (x) (string= (plist-get x :text) (plist-get it :text)))
         acc)
        nodes-for-single-type
        (-distinct nodes-for-single-type))))
    (--filter
     (symbolp (car it)) ; taking only the nodes that tree-sitter recognize with a syntax identifier
     (--group-by (plist-get it :type) self)))))

(defvar me-available-molds nil "Available molds.")

(defvar me-mold-history nil "List of molds produced.")

(defvar me-mold-before-hook nil "Hooks to run before running a mold.")

(defvar me-mold-after-hook nil "Hooks to run after running a mold.")

(defvar me-mold-before-mold-runs-hook nil "Hooks to run before the chosen mold runs.")


(defun me-interpret-given (mold)
  "Interpret MOLD :given clause into a sexp to run."
  (me-get-in mold '(:given :fn)))

(defun me-interpret-then (mold)
  "Interpret MOLD :then clause into a sexp to run."
  (let ((then (plist-get mold :then)))
    (cond
     ((ignore-errors (car (plist-get then :async)))
      `(let ((_ (async-let ,(plist-get then :async)
                  (progn
                    ,(plist-get then :fn)
                    (ignore-errors
                      (switch-to-buffer-other-window
                       (get-buffer buffername)))))))
         (get-buffer-create buffername)
         (with-current-buffer buffername
           (erase-buffer)
           (insert (format "Loading %s contents..." ,(plist-get mold :key))))))
     ((ignore-errors (car (plist-get then :no-async)))
      `(let* ,(plist-get then :no-async)
         (progn
           (get-buffer-create buffername)
           ,(plist-get then :fn)
           (ignore-errors
             (switch-to-buffer-other-window
              (get-buffer buffername))))))
     ((-contains-p then :fn)
      `(progn
         (get-buffer-create buffername)
         ,(plist-get then :fn)
         (ignore-errors
           (switch-to-buffer-other-window
            (get-buffer buffername))))))))

(defun me-mold-buffername (mold)
  "Get the resulting buffer name of MOLD."
  (concat "*moldable-emacs-" (or (plist-get mold :buffername) (plist-get mold :key)) "*"))

(defmacro me-with-mold-let (mold &rest clause) ;; TODO this must evaluate only once any time is called AND needs to make evaluation of bindings lazy?
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

This is a naive implementation because we just count how many parentheses are in the :given of the mold: if there is a lot of nesting molds come on top.

Ideally we want to give a score to the specificity of the
predicates in the :given (like checking for a major mode has more
weight than checking for a dependency on the system because you
must have a specific kind of buffer open, while the dependency is
always on the system.) "
  (s-count-matches "(" (format "%s" (let* ((given (plist-get mold :given)))
                                      (if (ignore-errors (equal 'me-mold-run-given (car (nth 1 given))))
                                          (plist-get (eval (nth 1 (nth 1 given))) :given)
                                        given)))))

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
              it))
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
         :old-mold me-last-used-mold)))

(add-hook 'me-mold-before-hook #'me-setup-self-mold-data)

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

(defun me-set-self-mold-data ()
  "Set `mold-data'."
  (setq-local mold-data
              (append
               (list
                :self (ignore-errors self)
                :date (format-time-string "%FT%T%z"))
               me-temporary-mold-data)))

(add-hook 'me-mold-after-hook #'me-set-self-mold-data -100)

(defvar me-last-example nil "Last automatically generated example for mold.
This should simplify the testing and documentation of molds.")

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
             :point ,point)))))

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

(defmacro me-given (given &rest body)
  "Setup according to GIVEN and run BODY."
  `(let* ((given (eval ',given))
          (type (plist-get  given :type))
          (name (plist-get given :name))
          (mode (plist-get given :mode))
          (point (plist-get given :point))
          (body ',body)
          (contents (if (eq mode 'image-mode)
                        (with-temp-buffer
                          (insert-file-contents-literally (plist-get given :contents))
                          (buffer-substring-no-properties (point-min) (point-max)))
                      (plist-get given :contents))))
     (eval (if (equal type 'buffer)
               `(with-temp-buffer
                  (rename-buffer ,name "-new") ;; TODO it would be better to keep the old buffer alive: now if I am testing Playground, it kills an existing Playground buffer too. It is fine for now because I plan to use this only for testing purposes.
                  (insert ,contents)
                  (,(if mode mode 'fundamental-mode))
                  (if ,point (goto-char ,point) (goto-char (point-min)))
                  ,@body)
             `(with-temp-file ,name
                (let ((buffer-file-name ,name ))
                  (rename-buffer (file-name-nondirectory ,name) "-new")
                  (insert ,contents) ;; TODO this does not work for images: it seems there are coding system issues
                  (,(if mode mode 'fundamental-mode))
                  (if ,point (goto-char ,point) (goto-char (point-min)))
                  ,@body))))))
(put 'me-given 'lisp-indent-function 1)

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
            (me-given beg
                      (funcall ',run-fn)
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
    (me-given start
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
                          (list (search-forward name) target)))
                     (or
                      (and (plist-get target :begin) (list (plist-get target :begin) (plist-get target :buffer)))
                      (save-excursion
                        (with-current-buffer target
                          (goto-char (point-min))
                          (list (search-forward name) target)))))))
    (me-make-elisp-file-link
     name
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
                (search-forward (if (s-contains-p "\"" name) (prin1-to-string name) name)))))
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
  (ignore-errors (plist-put mold-data :mold (plist-get mold :key))) ;; TODO remove me-set-last-mold and just set mold-data with `me-mold-before-mold-runs-hook'
  (setq me-last-used-mold (plist-get mold :key)))

(add-hook 'me-mold-before-mold-runs-hook #'me-set-last-mold)

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
        (warn (format
               "The example you are trying to add does not work because the following did not match:\n%s"
               issues))))
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

(defun me-require (dependency)
  "Try to require DEPENDENCY, and just give nil if not found."
  (require dependency nil t))

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

(defun me-ask-for-todo-details-according-to-context (note)
  "Ask for NOTE details."
  (let ((text (read-string "Note:")))
    (plist-put note :then `(:string ,text :state todo))))

;; https://stackoverflow.com/questions/21486934/file-specific-key-binding-in-emacs
(defun me-override-keybiding-in-buffer (key command)
  "Override KEY with COMMAND in buffer."
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))

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


(defun me-usable-molds-requiring-deps ()
  "Find molds that require dependencies to run."
  (me-usable-molds-requiring-deps-in me-available-molds))

(defun me-usable-molds-requiring-deps-in (molds-alist)
  "Find molds in MOLDS-ALIST that require dependencies to run."
  (--remove
   (let ((mold it)
         (given-cond (me-get-in it '(:given :fn))))
     (ignore-errors
       (and
        (> (length given-cond) 1)
        (eq (car given-cond) 'and)
        (me-with-mold-let mold
                          (funcall
                           (lambda ()
                             (eval (cons 'and (--remove
                                               (or
                                                (and
                                                 (seqp it)
                                                 (-contains? it 'executable-find))
                                                (and
                                                 (seqp it)
                                                 (-contains? it 'me-require)))
                                               (cdr (me-get-in mold '(:given :fn))))))))))))
   molds-alist))

(defun me-find-missing-dependencies-for-mold (mold)
  "List unmet dependencies by MOLD."
  (let* ((flatten-given (-flatten (me-get-in mold '(:given :fn)))) ;; TODO this will break if I add other keywords than :fn
         (executables (--> flatten-given
                           (--find-indices (eq it 'executable-find) it)
                           (--map (list (nth it flatten-given) (nth (+ 1 it) flatten-given)) it)
                           (--remove (eval it) it)))
         (requires (--> flatten-given
                        (--find-indices (eq it 'me-require) it)
                        (--map (list (nth it flatten-given) `(quote ,(nth (+ 2 it) flatten-given))) it)
                        (--remove (eval it) it))))
    (list
     :key (plist-get mold :key)
     :missing-dependencies
     (append requires executables))))

(defun me-find-missing-dependencies-for-molds (molds)
  "List unmet dependencies by MOLDS."
  (-map
   #'me-find-missing-dependencies-for-mold
   molds))


;; some functionality to edit nodes!!
(defun me-remove-node (node)
  "Remove NODE from :buffer or :buffer-file using :begin and :end as anchors."
  (let ((begin (plist-get node :begin))
        (end (plist-get node :end))
        (buffer (plist-get node :buffer))
        (file (plist-get node :buffer-file)))
    (with-current-buffer buffer
      (delete-region begin end))
    ;; (if file
    ;;     (me-with-file file (delete-region begin end)))
    ;; (when (and buffer (get-buffer buffer))
    ;;   (with-current-buffer buffer
    ;;     (delete-region begin end)))
    ))

(defun me-add-node (node)
  "Add NODE to :buffer or :buffer-file using its :begin position as an anchor."
  (let ((begin (plist-get node :begin))
        (text (plist-get node :text))
        (buffer (plist-get node :buffer))
        (file (plist-get node :buffer-file)))
    (with-current-buffer buffer
      (goto-char begin)
      (insert text))
    ;; (if file
    ;;     (me-with-file
    ;;      (goto-char begin)
    ;;      (insert text))
    ;;   (when (and buffer (get-buffer buffer))
    ;;     (with-current-buffer buffer
    ;;       (goto-char begin)
    ;;       (insert text))))
    ))

(defun me-change-node (transition)
  "Run a TRANSITION to change a node.  This must contain a :before and an :after node."
  (let ((before (plist-get transition :before))
        (after (plist-get transition :after)))
    (me-remove-node before)
    (me-add-node after)))

(defun me-change-nodes (transitions)
  "Change nodes according to TRANSITIONS.
These contain a :before node and an :after node."
  (-each (reverse transitions)
    #'me-change-node))

(defun me-transit-node-text (node fn)
  "Create a transition changing text of NODE via FN.
FN is a function taking the text of NODE and generating new text."
  (list
   :before node
   :after (plist-put
           (-copy node)
           :text
           (funcall fn (plist-get node :text)))))

(defun me-transit-node-texts (nodes fn)
  "Create transitions changing texts of NODES via FN."
  (--map (me-transit-node-text it fn) nodes))

(defun me-node-children (node nodes)
  "Get children of NODE in NODES."
  (let ((begin (plist-get node :begin))
        (end (plist-get node :end)))
    (--filter
     (and (> (plist-get it :begin) begin)
          (<  (plist-get it :end) end))
     nodes)))

(defun me-children-number (node nodes)
  "Get children number of NODE in NODES."
  (--> (me-get-children node nodes)
       length))

(defun me-hash-to-plist (hash-table)
  ;; from http://ergoemacs.org/emacs/elisp_hash_table.html (this is a recursive version)
  "Produce a plist from the HASH-TABLE (recursively)."
  (let (result)
    (maphash
     (lambda (k v)
       (push (list (if (stringp k) (intern k) k)
                   (if (hash-table-p v) (xah-hash-to-list v) v))
             result))
     hash-table)
    result))

(defun me-plist-focus (plist keys)
  "Focus only on KEYS of PLIST.
For example, (me-plist-focus '(:a a :b b :c c) '(:a :c)) => '(:a a :c c)."
  (-flatten (--map (list it (plist-get plist it)) keys)))

;; (me-plist-focus '(:a a :b b :c c) '(:a :c))

(defun me-focus-on-consistent-keys (list-of-plist)
  "Focus on common keys of LIST-OF-PLIST.
For example ((:a 1 :b 1 :c 1) (:a 2 :c 2)) becomes ((:a 1 :c 1) (:a 2 :c 2)).
This is useful for plotting."
  (let ((keys (-reduce '-intersection (--map (-filter 'symbolp it) list-of-plist))))
    (--map (me-plist-focus it keys) list-of-plist)))

(defun me-get-region ()
  "Get the active region's string."
  (when (region-active-p)
    (buffer-substring-no-properties
     (caar (region-bounds))
     (cdar (region-bounds)))))

;; https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
(defun me-org-replace-link-by-link-description ()
  "Remove the link part of an `org-mode' link at point and keep only the description."
  (interactive)
  (let ((elem (org-element-context)))
    (if (eq (car elem) 'link)
        (let* ((content-begin (org-element-property :contents-begin elem))
               (content-end  (org-element-property :contents-end elem))
               (link-begin (org-element-property :begin elem))
               (link-end (org-element-property :end elem)))
          (if (and content-begin content-end)
              (let ((content (buffer-substring-no-properties content-begin content-end)))
                (delete-region link-begin link-end)
                (insert content)))))))

(defun me-replace-org-links-with-descriptions (&optional text)
  "Remove org links in place unless TEXT is passed."
  (if text
      (with-temp-buffer
        (org-mode)
        (insert text)
        (goto-char (point-min))
        (me-replace-org-links-with-descriptions)
        (buffer-substring-no-properties (point-min) (point-max)))
    (while (eq (org-next-link) 't)
      (me-org-replace-link-by-link-description))))

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

(defun me-clj-var-p (node)
  "Check if flattened tree NODE is a Clojure function."
  (and
   (eq 'list_lit (plist-get node :type))
   (s-starts-with-p "(def " (plist-get node :text))))

(defun me-clj-fn-p (node)
  "Check if flattened tree NODE is a Clojure function."
  (and
   (eq 'list_lit (plist-get node :type))
   (or (s-starts-with-p "(defn " (plist-get node :text))
       ;;  in case I have (def x (fn [] ...))
       (s-starts-with-p "(fn " (plist-get node :text)))))

(defun me-clj-atom-p (node)
  "Check if flattened tree NODE is a Clojure atom."
  (and
   (eq 'list_lit (plist-get node :type))
   (s-starts-with-p "(def " (plist-get node :text))
   (s-contains-p "atom" (plist-get node :text))))

(defun me-clj-require-p (node)
  "Check if flattened tree NODE is a Clojure :require."
  (and
   (eq 'list_lit (plist-get node :type))
   (s-starts-with-p "(:require" (plist-get node :text))))

(defun me-clj-datomic-query-p (node)
  "Check if there is a Datomic query in flattened tree's NODE."
  (and
   (eq 'quoting_lit (plist-get node :type))
   (s-contains-p ":where" (plist-get node :text))))

(defun me-project-to-nodes (dir &optional file-extension)
  "Produce nodes for project DIR.
Optionally filter for files with FILE-EXTENSION."
  (--> (projectile-project-files dir)
       (if file-extension
           (--filter
            (equal file-extension
                   (file-name-extension it))
            it)
         it)
       (--map
        (let ((filename (let ((default-directory dir)) (expand-file-name it))))
          (or
           (ignore-errors (me-filepath-to-flattened-tree filename)) ; sometimes there is an encoding issue with this that I can fix me-mold-treesitter-to-parse-tree
           (ignore-errors (with-file filename
                                     (me-mold-treesitter-to-parse-tree)))))
        it)))

(defun me-project-to-flattened-nodes (dir &optional file-extension)
  "Create a list of all the syntax elements nodes of files in DIR filtering by FILE-EXTENSION (e.g, 'clj')."
  (-flatten-n 1 (me-project-to-nodes dir file-extension)))

(defun me-node-complexity-stats (node)
  "Use code-compass `calculate-complexity-stats' to get complexity stats of NODE."
  (if (me-require 'code-compass)
      (code-compass-calculate-complexity-stats (plist-get node :text))
    (error "install code-compass for this from https://github.com/ag91/code-compass")))

(defun me-node-complexity (node)
  "Use code-compass `calculate-complexity-stats' to get complexity of NODE."
  (alist-get 'total (me-node-complexity-stats node)))

(defun me-project-function-nodes-by-complexity (dir &optional extension)
  "Gather (possible) function nodes for project DIR. Optionally filter nodes by EXTENSION."
  (--> (me-project-to-flattened-nodes dir extension) ;; TODO cache this
       (me-by-types
        (-keep
         (lambda (type)
           (and (s-contains-p "function" (or (ignore-errors (symbol-name type)) ""))
                type))
         (me-types it))
        it)
       (--sort
        (> (me-node-complexity it)
           (me-node-complexity other))
        it)))

(defun me-clj-project-to-nodes-categories (dir &optional file-extension) ; TODO this works for Clojure now, I need to bind the predicates according to the extension/grammar instead. If 'python `me-node-fn-p' should behave differently than me-clj-fn-p
  "Produce categories of nodes for project DIR.
Optionally filter for files with FILE-EXTENSION."
  (-->  (me-project-to-nodes dir file-extension)

        (list
         :fns
         (-non-nil (--map (-filter 'me-clj-fn-p it) it))
         :datomic-queries
         (-non-nil (--map (-filter 'me-clj-datomic-query-p it) it))
         :vars
         (-non-nil (--map (-filter 'me-clj-var-p it) it))
         :atoms
         (-non-nil (--map (-filter 'me-clj-atom-p it) it))
         :requires
         (-non-nil (--map (-filter 'me-clj-require-p it) it)))))

(defun me-format-iso8601-time (time)
  "Format TIME to ISO8601.
-- taken from http://xahlee.info/emacs/emacs/elisp_datetime.html."
  (concat
   (format-time-string "%Y-%m-%dT%T" time)
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z" time))))

(defun me-keys (plist)
  "Return keys of PLIST."
  (--filter (and (symbolp it) (s-starts-with-p ":" (symbol-name it))) plist))

(defun me-select-keys (plist keys)
  "Select subset of KEYS from PLIST.
For '(:a 1 :b 2 :c 3) and '(:a :c) this yields '(:a 1 :c 3)."
  (--reduce-from (append acc (list it (plist-get plist it))) nil keys))
;; (me-select-keys '(:a 1 :b 2 :c 3) '(:a :c))

(defun me-merge (join-when-you-can? &rest plists)
  "Merge keys of PLISTS when possible.
If JOIN-WHEN-YOU-CAN? is true, if keys contain lists,
 we append their results instead of replacing."
  (--reduce
   (-reduce-from
    (lambda (acc1 key)
      (let ((a (plist-get acc key))
            (b (plist-get it key)))
        (if (and join-when-you-can? (listp a) (listp b))
            (append acc1 (list key (-union a b)))
          (append acc1 (list key b)))))
    nil
    (-union (me-keys it) (me-keys acc)))
   plists))
;(me-merge t '(:a ("1") :b "2") '(:a ("3") :b "3"))


;; organize screens better

(defvar me-mold-start-buffer nil "Buffer on which you run `me-mold'.")
(defun me-set-me-mold-start-buffer () (setq me-mold-start-buffer (buffer-name)))

(add-hook 'me-mold-before-hook #'me-set-me-mold-start-buffer)

(defcustom me-show-inspector t "Show inspector to see what is the data in self and mold-data for the running mold.")

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

(defun me-goto-mold-source (mold)
  "Go to source code of MOLD."
  (interactive
   (list nil))
  (let* ((molds me-available-molds)
         (keys (--map (plist-get it :key) molds))
         (picked-mold (or mold
                          (completing-read
                           "Pick the mold you need:"
                           keys))))
    (--> picked-mold
         (-find
          (lambda (x)
            (string=
             (plist-get x :key)
             it))
          molds)
         (plist-get it :origin)
         (find-file it))
    (goto-char (point-min))
    (search-forward picked-mold)))

(defun me-org-roam-backlinks-contents (node &optional depth)
  "Collect NODE backlink contents.
Optionally stop at DEPTH, by default 0.
The format of the contents is (:node .. :node-contents .. :sub-nodes (list (:node .. :contents)) :sub-sub-nodes (list <recursive type>))."
  (let* ((depth (or depth 0))
         (backlinks (org-roam-backlinks-get node))
         (contents
          (--map
           (let ((backlink-node (org-roam-backlink-source-node it)))
             (list
              :node backlink-node
              :contents
              (with-temp-buffer
                (insert-file-contents-literally (org-roam-node-file backlink-node))
                (buffer-string))))
           backlinks)))
    (list :node node
          :node-contents (with-temp-buffer
                           (insert-file-contents-literally (org-roam-node-file node))
                           (buffer-string))
          :sub-nodes-contents contents
          :sub-sub-nodes
          (when (> depth 0)
            (--map (me-org-roam-backlinks-contents (plist-get it :node) (- depth 1)) contents)))))

(defun me-org-roam-format-backlinks-contents (contents &optional depth)
  "Format CONTENTS to an Org tree.
Optionally provide DEPTH to define the number of additions asterisks to prepend to heading."
  (let* ((depth (or depth 0)))
    (s-concat
     (s-repeat depth "*")
     (plist-get contents :node-contents)
     "\n\n"
     (--> (plist-get contents :sub-nodes-contents)
          (--map (concat (s-repeat (+ 1 depth) "*") (plist-get it :contents)) it)
          (s-join "\n\n" it))
     "\n\n"
     (s-join
      "\n\n"
      (--map (me-org-roam-format-backlinks-contents it (+ 1 depth))
             (plist-get contents :sub-sub-nodes))))))

;; syntax highlighting
(defun me-highlight-node (node)
  "Highlight NODE in its buffer."
  (with-current-buffer (get-buffer-create (plist-get node :buffer)) ; TODO handle :buffer-file
    (let* ((node-start (plist-get node :begin))
           (node-end (plist-get node :end))
           (overlay (make-overlay node-start node-end))
           (capture-name (or (ignore-errors (symbol-name (plist-get node :type)))
                             (plist-get node :type))))
      ;; Ensure the overlay is deleted when it becomes empty.
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'face '(:background "Green"))
      ;; Use the capture's name as the mouseover tooltip.
      (unless (string= capture-name "")
        (overlay-put overlay 'help-echo capture-name)))))

(defun me-highlight-nodes (nodes)
  "Highlight NODES in their buffer."
  (-each nodes 'me-highlight-node))


(defun me-insert-follow-overlay (node-to-overlay nodes)
  "Link NODE-TO-OVERLAY and NODES with an overlay executing when cursor touches the area of NODE-TO-OVERLAY."
  (cursor-sensor-mode 1)
  (let ((old-buffer (plist-get (car nodes) :buffer))
        (ov (make-overlay
             (plist-get node-to-overlay :begin)
             (plist-get node-to-overlay :end))))
    (overlay-put
     ov
     'cursor-sensor-functions
     (list `(lambda (affected-window old-position entered-or-left)
              (cond
               ((eq entered-or-left 'entered)
                (progn (overlay-put ,ov 'face '(:background "Green"))
                       (-each ',nodes 'me-highlight-node)))
               ((eq entered-or-left 'left)
                (progn (overlay-put ,ov 'face nil)
                       (with-current-buffer ,old-buffer
                         (remove-overlays))))))))))

(defun me-syntax-description (type language)
  "Get description for node of TYPE and LANGUAGE."
  (or
   ;;  TODO I should generalize this to add descriptions on demand (in particular if I am going to define my own types)
   (plist-get
    (--find (equal (plist-get it :label) (or
                                          (ignore-errors (symbol-name type))
                                          type))
            nil ;; me-natural-syntax-tree-labels - TODO not shared yet
            )
    :description)
   (format "[[elisp:(browse-web \"%s %s\")][Search for description]]" language type)))

(defun me-csv-buffer-to-plist ()
  "Turn CSV buffer in a list of plists."
  (let* ((separator (--> (thing-at-point 'line t)
                         (list (list "," (length (s-split "," it)))
                               (list ";" (length (s-split ";" it)))
                               (list "\t" (length (s-split "\t" it))))
                         (--max-by (> (nth 1 it) (nth 1 other)) it)
                         car))
         (keys (--> (thing-at-point 'line t)
                    (s-split separator it) ;; TODO splitting doesn't take in account of "bla , lol",123 entries
                    (--map (intern (concat ":" (s-replace "\"" "" (s-trim it)))) it)))
         (plist nil)
         (_ (while (ignore-errors (not (next-logical-line)))
              (--> (thing-at-point 'line t)
                   (s-split separator it) ;; TODO splitting doesn't take in account of "bla , lol",123 entries
                   (-map #'s-trim it)
                   (-zip-lists keys it)
                   -flatten
                   (setq plist (cons it plist))))))
    plist))

(defun me-plist-to-csv-string (plist)
  "Make PLIST into a CSV string."
  (let ((keys (me-keys (car plist))))
    (concat
     ;; header
     (s-join "," (--map (s-drop 1 (symbol-name it)) keys))
     ;; entries
     "\n"
     (--> plist
          (--map
           (s-join "," (--map (format (if (and (stringp it) (s-contains-p "," it)) "\"%s\"" "%s") it) (-remove 'symbolp (me-select-keys it keys))))
           it)
          (s-join "\n" it)))))

(defun me-heatmap (plists-list intervals)
  "Insert a heatmap as an org table, given a PLISTS-LIST and INTERVALS.
Example:
  (me-heatmap '((:a 1 :b 2 :c 3)
                (:a 2 :b 8 :c 10))
              '(3 5 8))"
  (me-insert-org-table
   (--map
    (cons (symbol-name it)
          `(:extractor
            (lambda (obj) (plist-get obj ,it))
            :handler
            (lambda (number)
              (let* ((color
                      (cond ((>= number (nth 0 intervals)) "red")
                            ((>= number (nth 1 intervals)) "orange")
                            ('otherwise "green"))))
                (me-color-string (number-to-string number) color)))))
    (me-keys (car plists-list)))
   plists-list))

;; begin - restore window configuration
(defvar me-last-window-configuration nil "Stores last window configuration from when you firstly invoked me-mold.")

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
;; end - restore window configuration

(defun me-stats (number-list)
  "Calculate some basic stats on NUMBER-LIST."
  (let ((mean (/ (-sum number-list) (length number-list))))
    (list :mean mean
          :median (nth (/ (+ (length number-list) 1) 2) (--sort (> it other) number-list))
          :min (-min number-list)
          :max (-max number-list)
          :standard-deviation (sqrt (/ (-sum (--map (expt (- it mean) 2) number-list)) (length number-list)))
          ;; TODO percentiles
          )))

;; begin urls collection
(defun me-re-seq (regexp string)
  "Get a list of all REGEXP matches in a STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun me-re-url-seq (string)
  "Get a list of all urls in STRING."
  (let ((urlreg "https?://\\(www\\)?\\(?:[./#\+-]?\\w*\\)+"))
    (me-re-seq urlreg string)))

(defun me-urls-in-clipboard ()
  "Get a list of all urls in the kill ring head."
  (let (text)
    (with-temp-buffer
      (clipboard-yank)
      (setq text (buffer-string)))
    (reverse (me-re-url-seq text))))

(defun me-urls-in-region ()
  "Get a list of all urls in region."
  (reverse (me-re-url-seq (when (region-active-p)
                            (buffer-substring-no-properties
                             (caar (region-bounds))
                             (cdar (region-bounds)))))))

;; end urls collection

;; begin utilities org ql - org transclusion
(defun me-org-ql-to-org-transclusion (org-ql-headlines)
  "Transform ORG-QL-HEADLINES into something manageable by `org-transclusion'.

>> (me-org-ql-to-org-transclusion '((headline (:ID \"some-id\" :raw-value \"some heading\"))))
=> (\"#+transclude: [[id:some-id][some heading]]

\")
"
  (--map (format "#+transclude: [[id:%s][%s]]\n\n"
                 (org-element-property :ID it)
                 (org-element-property :raw-value it))
         org-ql-headlines))

(defun me-org-transclude-in-buffer (org-transclusion-headings &optional buffer switch? hook)
  "Set up BUFFER using `org-transclusion' on ORG-TRANSCLUSION-HEADINGS.

When SWITCH? it switches to BUFFER.
HOOK is a (lambda () ...) to run some side effects.

NOTE: this does nothing if you don't have org-transclusion installed."
  (and
   (me-require 'org-transclusion)
   (let ((buffer (or buffer (get-buffer-create "*moldable emacs org transclusion*"))))
     (with-current-buffer buffer
       (org-mode)
       (org-transclusion-remove-all)
       (erase-buffer)
       (insert (s-join "\n\n" org-transclusion-headings))
       (org-transclusion-add-all)
       (goto-char (point-min))
       (when hook (funcall hook)))
     (when switch? (switch-to-buffer-other-window buffer)))))

(defun me-org-roam-backlink-to-org-transclusion (backlink)
  "Format `org-roam' BACKLINK to `org-transclusion' format.

Note: nil if org-roam is not installed."
  (and (me-require 'org-roam)
       (format
        "#+transclude: [[id:%s][%s]]\n\n"
        (org-roam-node-id (org-roam-backlink-source-node backlink))
        (org-roam-node-title (org-roam-backlink-source-node backlink)))))
;; end utilities org ql - org transclusion

;; begin similar nodes
(defun me-child-p (node possible-parent)
  "Check if NODE is a child of POSSIBLE-PARENT."
  (let ((node-begin (plist-get node :begin))
        (node-end (plist-get node :end))
        (possible-parent-begin (plist-get possible-parent :begin))
        (possible-parent-end (plist-get possible-parent :end)))
    (and
     (> node-begin possible-parent-begin)
     (< node-end possible-parent-end))))

(defun me-find-similar-nodes (node tree)
  "Given a NODE and a TREE, finds the elements with the same type and that have most in common (adds a :similarity score as well)."
  (--> tree
       ;; only same type
       (me-by-type (plist-get node :type) it)
       ;; remove node from similar nodes
       (-difference it (list node))
       ;; remove parents of node OR parents of similar nodes (we want the smallest similar nodes otherwise the wrapping of a parent wouldn't bring anything to the similarity score)
       (-remove
        (lambda (possible-parent) (or
                                   (me-child-p node possible-parent)
                                   (--any (me-child-p it possible-parent) it)))
        it)
       ;; calculate similarity score
       (--map
        (and
         (append it
                 (list
                  :similarity-score
                  (length (-intersection (--map (me-plist-focus it '(:type :text :buffer)) (me-node-children node tree))
                                         (--map (me-plist-focus it '(:type :text :buffer)) (me-node-children it tree)))))))
        it)
       ;; sort by it
       (--sort (> (plist-get it :similarity-score) (plist-get other :similarity-score)) it)))
;; end similar nodes

(provide 'moldable-emacs)
;;; moldable-emacs.el ends here
