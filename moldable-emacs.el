(require 'dash)
(require 's)
(require 'async)
(require 'thunk)

(defcustom me-i-know-what-i-am-doing
  nil
  "Set this to t if don't need to see tutorials.")

(defcustom me-files-with-molds
  (--map
   (concat
    (file-name-directory load-file-name) ; https://stackoverflow.com/questions/26991001/elisp-get-path-to-file-relative-to-script
    it)
   (list
    "molds/core.el"
    "molds/contrib.el"))
  "Files containing molds.")

(defcustom me-molds-debug-on
  nil
  "Toggle for debugging information.")

(defun me-setup-molds ()
  "Load molds from `me-files-with-molds'."
  (-each me-files-with-molds #'load-file))

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

(defun me-async-map--finish (futures post-fn too-late-p poll-time)
  (if (not (some #'null (mapcar #'async-ready futures)))
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
  "Run FN async on elements ELS. Optionally define a POST-FN to run on the results of apply FN on ELS. Optionally define a POLL-TIME to look for results and a TIMEOUT to fail."
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
  "Run FN in parallel on elements ELS. Optionally define a POST-FN to run on the results of apply FN on ELS. Optionally define a POLL-TIME to look for results and a TIMEOUT to fail."
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
      (when (funcall too-late-p) (error "me-pmap has waited too long: timed out"))
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
   (concat "| " (s-join " | " (-map #'car headlines))  " | \n")
   (concat "|-" (format (s-repeat (- (length headlines) 1) "-+-")) "-| \n")
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
  "Insert TABLE-STRING in buffer. Make sure table is well indented."
  (insert table-string)
  (save-excursion
    (search-backward "|" nil nil 2) ;; count 2 to avoid an extra (empty) row at the bottom
    (org-cycle))
  (setq-local org-confirm-elisp-link-function nil))

(defun me-insert-org-table (headlines objects)
  "Produce org table of OBJECTS formatting with HEADLINES."
  (me-insert-string-table (me-make-org-table headlines objects)))

(defun me-alist-to-plist (alist)
  "Convert ALIST to a `plist'."
  (let ((keys (ignore-errors
                (and (= (length (car alist)) (length (-filter #'stringp (car alist))))
                     (--map (intern (concat ":" it)) (car alist))))))
    (if keys
        (--map (-flatten (-zip-lists keys it)) (cdr alist))
      alist)))

(defun me-org-table-to-plist (table-string)
  "Make TABLE-STRING a plist."
  (with-temp-buffer
    (save-excursion (insert table-string))
    (org-table-transpose-table-at-point)
    (let ((table (org-table-to-lisp))
          result)
      (dolist (r table result)
        (setq result (plist-put result (intern (concat ":" (s-replace "\"" "" (car r)))) (cdr r)))))))

(defun me-org-table-to-flat-plist (table-string)
  (let* ((plist (me-org-table-to-plist table-string))
         (keys (-filter 'symbolp plist)))
    (--> keys
         (--map (-map (lambda (x) (list it (substring-no-properties x))) (plist-get plist it)) it)
         (apply '-zip it)
         (-map '-flatten it))))

(defun me-flat-org-table-to-string (flat-org-table)
  (me-make-org-table
   (--map
    (list (substring (symbol-name it) 1) . (:extractor `(lambda (x) (format "%s" (plist-get x ,it)))))
    (-filter #'keywordp (car flat-org-table)))
   flat-org-table))

(defun me-insert-flat-org-table (flat-org-table)
  (me-insert-string-table (me-flat-org-table-to-string flat-org-table)))

(defun me-org-tabletolisp-to-plist (org-table-to-lisp)
  "Create a plist of ORG-TABLE-TO-LISP obtained by `org-table-to-lisp' fn."
  (--> org-table-to-lisp
       (orgtbl-to-orgtbl it nil)
       (me-org-table-to-flat-plist it)))

(defun me-first-org-table (&optional buffer)
  "Find first org table. Optionally in BUFFER."
  (ignore-errors
    (with-current-buffer (or buffer (current-buffer)) ;; TODO remove org links in table!
      (save-excursion
        (re-search-forward org-table-line-regexp nil t)
        (me-org-tabletolisp-to-plist (org-table-to-lisp))))))

(defun me-all-flat-org-tables (&optional buffer)
  "Find first org table. Optionally in BUFFER."
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


(defun me-by-type (type tree)
  "Filter TREE entries by TYPE."
  (when (symbolp type)
    (--filter (eq (plist-get it :type) type) tree)))

;; (with-temp-buffer
;;   (save-excursion (insert "| a | b |
;;     |---+---|
;;     | x | y |
;;     | w | z |
;;     "))
;;   (call-interactively 'my/org-table-to-plist))

(defun me-mold-treesitter-to-parse-tree (&optional node)
  "Return list of all abstract syntax tree nodes one step away from leaf nodes. Optionally start from NODE."
  (let ((root (or
               node
               (ignore-errors (tsc-root-node tree-sitter-tree)))))
    (when root
      (let* ((fn
              (lambda (node)
                (tsc-mapc-children
                 (lambda (n)
                   (setq acc (cons
                              (list
                               :type (tsc-node-type n)
                               :text (tsc-node-text n)
                               :begin (tsc-node-start-position n)
                               :end (tsc-node-end-position n)
                               :buffer (buffer-name)
                               :buffer-file (s-replace (getenv "HOME") "~" (buffer-file-name)))
                              acc))
                   (funcall fn n))
                 node))))
        (setq-local acc nil)
        (funcall fn root)
        (reverse acc)))))

(defun me-extension-to-major-mode (extension)
  "Find major-mode for EXTENSION. E.g., \".scala\" => scala-mode"
  (cdr (--find (s-match (car it) extension) auto-mode-alist)))

(defun me-major-mode-to-tree-sitter-grammar (major-mode)
  "Find emacs-tree-sitter grammar for MAJOR-MODE."
  (alist-get major-mode tree-sitter-major-mode-language-alist))

(defun me-extension-to-tree-sitter-grammar (extension)
  "Find emacs-tree-sitter grammar for EXTENSION."
  (--> extension
       me-extension-to-major-mode
       me-major-mode-to-tree-sitter-grammar))


(defun me-filepath-to-flattened-tree (file)
  "Return the flattened tree for FILE."
  (when-let ((grammar (me-extension-to-tree-sitter-grammar (file-name-extension file t))))
    (with-temp-buffer
      (insert-file-contents-literally file)
      (let ((buffer-file-name file)
            (tree-sitter-language (tree-sitter-require grammar))
            (tree-sitter-parser (tsc-make-parser)))
        (tsc-set-language tree-sitter-parser tree-sitter-language)
        (--> (tsc--without-restriction
               (tsc-parse-chunks tree-sitter-parser #'tsc--buffer-input nil))
             tsc-root-node
             me-mold-treesitter-to-parse-tree)))))

(defun nodes-with-duplication (self)
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

(defmacro me-with-mold-let (mold &rest clause) ;; TODO this must evaluate only once any time is called AND needs to make evaluation of bindings lazy?
  "Wrap BODY in a let with :let and :buffername of MOLD, plus add the body for CLAUSE."
  `(funcall
    (lambda (m clause)
      (eval
       `(progn
          (let ((buffername (concat "*moldable-emacs-" (or ,(plist-get m :buffername) ,(plist-get m :key)) "*")))
            (,(if (ignore-errors (eq (car clause) :then)) 'let* 'thunk-let*) (,@(plist-get m :let))
             (pcase ',clause
               ('(:given) ,(me-interpret-given m))
               ('(:then) ,(me-interpret-then m))
               (_ ,@clause)))))
       't))
    ,mold
    ',clause))

;; (me-print-to-buffer (let ((mold (me-find-mold "PlistToJson")))
;;                       (me-with-mold-let mold
;;                                         :then))
;;                     (get-buffer-create "bla"))


(defun me-mold-run-given (mold)
  "Run MOLD :given."
  (me-with-mold-let mold
                    :given))

(defvar me-usable-mold-stats nil)


(defun me-usable-molds (&optional molds buffer)
  "Return the usable molds among the `me-available-molds' for the `current-buffer'. Optionally you can pass a list of MOLDS and a BUFFER to filter the usable ones."
  (let ((_ (setq me-usable-mold-stats nil))
        (molds (or molds me-available-molds))
        (buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (--filter
       (let* ((beginning (current-time))
              (result (save-excursion
                        (condition-case err
                            (me-mold-run-given it)
                          (error (message "me-usable-molds: error in :given of %s:\n   %s" (plist-get it :key) err))))) ; TODO composite molds
              (ending (current-time))
              (_ (when me-molds-debug-on
                   (add-to-list 'me-usable-mold-stats (list :mold (plist-get it :key)
                                                            :time
                                                            (time-to-seconds
                                                             (time-subtract
                                                              ending
                                                              beginning)))))))
         result) ;; TODO run this in parallel when time goes over 100ms (time goes already over for org-table condition when there are many org tables in the same file - I got over 2 seconds wait for 5 tables mostly empty!!!)
       molds))))

(defun me-mold-run-then (mold)
  "Run MOLD :then."
  (me-with-mold-let mold :then))

(defun me-mold ()
  "Propose a list of available molds for the current context."
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
         (completing-read
          "Pick the mold you need:"
          it)
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
         me-mold-run-then)
    (run-hooks 'me-mold-after-hook)))

(defun me-mold-compose-molds (mold1 mold2)
  "Compose MOLD1 and MOLD2 in a new mold."
  `(
    :key ,(format
           "CompositionOf%sAnd%s"
           (plist-get mold1 :key)
           (plist-get mold2 :key))
    :given (:fn (me-mold-run-given ',mold1))
    :then (:fn
           (progn (me-mold-run-then ',mold1)
                  (me-mold-run-then ',mold2)
                  ;; (delete-window (get-buffer-window (plist-get ',mold1 :buffername)))
                  (switch-to-buffer buffername)
                  (kill-buffer-and-window)))))

(defun me-mold-compose (m1 m2 &optional props)
  "Compose M1 and M2 in a single mold. Add PROPS (e.g.,  `(:docs \"...\" :examples nil)') to it."
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

(defun me-set-dired-self-for-playground ()
  "Set Playground `self' to dired list of files."
  (when (and
         (s-starts-with-p "Playground" me-last-used-mold)
         (ignore-errors mold-data)
         (eq (plist-get mold-data :old-mode) 'dired-mode))
    (setq-local self (with-current-buffer (plist-get mold-data :old-buffer)
                       (or (progn (goto-char (point-min)) (dired-get-marked-files))
                           (progn (mark-whole-buffer)
                                  (call-interactively #'dired-mark)
                                  (let ((files (dired-get-marked-files)))
                                    (call-interactively #'dired-unmark-all-files)
                                    files)))))))
(add-hook 'me-mold-after-hook #'me-set-dired-self-for-playground) ;; the order is important: keep before me-set-self-mold-data

(defun me-set-self-mold-data ()
  "Set `mold-data'."
  (setq-local mold-data
              (append
               (list
                :self (ignore-errors self)
                :date (format-time-string "%FT%T%z"))
               me-temporary-mold-data)))

(add-hook 'me-mold-after-hook #'me-set-self-mold-data)

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
             :contents ,contents)))))

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
    (warn "Mold %s has no examples! Would you mind to add one?\nYou can use TODO now to add the last usage as an example.\n"
          (plist-get mold :key))))

(defun me-warn-on-run-if-no-docs (mold)
  "Emit warning if MOLD has no examples."
  (unless (or (not me-molds-debug-on) (plist-get mold :docs))
    (warn "Mold %s has no docs! Would you mind to add a line to tell what it is for?\n"
          (plist-get mold :key))))

(add-hook 'me-mold-before-mold-runs-hook #'me-warn-on-run-if-no-example)
(add-hook 'me-mold-before-mold-runs-hook #'me-warn-on-run-if-no-docs)

(defmacro me-given (given &rest body)
  "Setup according to GIVEN and run BODY."
  `(let* ((given (eval ',given))
          (type (plist-get  given :type))
          (name (plist-get given :name))
          (mode (plist-get given :mode))
          (body ',body)
          (contents (if (eq mode 'image-mode)
                        (with-temp-buffer
                          (insert-file-contents-literally (plist-get given :contents))
                          (buffer-substring-no-properties (point-min) (point-max)))
                      (plist-get given :contents))))
     (eval (if (equal type 'buffer)
               `(with-temp-buffer
                  (rename-buffer ,name) ;; TODO this does not work if there is a homonym buffer open!
                  (insert ,contents)
                  (,(if mode mode 'fundamental-mode))
                  ,@body)
             `(with-temp-file ,name
                (insert ,contents) ;; TODO this does not work for images: it seems there are coding system issues
                (,(if mode mode 'fundamental-mode))
                ,@body)))))



(defun me-check-then-clause (then)
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
  (let* ((beg (plist-get example :given))
         (end (plist-get example :then)))
    (me-given beg
              (goto-char (point-min))
              (funcall run-fn)
              (me-check-then-clause end)
              (kill-buffer))))

(defun me-test-example (example run-fn)
  "Test RUN-FN in the EXAMPLE."
  (let ((result (plist-get (me-check-example example run-fn) :success)))
    (if result
        result
      (message "Issues: %s" (plist-get (me-check-example example run-fn) :issues)))))

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
  (if-let ((mold mold)
           (example (nth 0 (plist-get mold :examples))))
      (me-demo-example example)
    (error "No example available for this mold to demo.")))

(defun me-mold-demo-by-key (key)
  (me-mold-demo (me-find-mold key)))

(defun me-open-at-point ()
  "Follow node at point."
  (interactive)
  (let* ((node (list-at-point))
         (buffer (plist-get node :buffer))
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

(defcustom me-enable-history 't "Keeps history for current session, if defined.")
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

(defun me-find-relative-test-report (filepath)
  ;; TODO refactor a bit for supporting Clojure with https://github.com/ruedigergad/test2junit
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
  (format "[[%s:%s][%s]]" (or link-type "file") target description))

(defun me-make-elisp-navigation-link (name file-name)
  "Make an Elisp Org link that navigates to a position of NAME in FILE-NAME."
  (let* ((pos-file (with-temp-buffer
                     (insert-file-contents-literally file-name)
                     (goto-char (point-min))
                     (list (search-forward (format "%s" name)) file-name))))
    (me-make-elisp-file-link
     name
     (format
      "(progn (find-file-other-window \"%s\") (goto-char %s))"
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

(defun me-buffer-changed-while-the-mold-is-on-p (buffername)
  "Return `t' if buffer changed from last call."
  ;; TODO store hash of `buffername' in a unique variable and test against if it the variable is defined
  nil)

;; https://hungyi.net/posts/org-mode-subtree-contents/
(defun me-org-copy-subtree-contents (&optional buffer position)
  "Get the content text of the subtree at point and add it to the `kill-ring'.
Excludes the heading and any child subtrees."
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
       (incf (cdr (or (assoc it acc)
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
      (insert
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
  "Tell if string IT contains an arithmetic member."
  (or (me-arithmetic-component-p it)
      ;; in case we have something like "1+1"
      (-all?
       #'me-arithmetic-component-p
       (s-split "" it 't))))

(defun me-arithmetic-at-point () ;; TODO needs refactoring!
  "Find an arithmetic expression on the current line. NIL if not there."
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

(defcustom me-note-file-store "~/workspace/agenda/moldableNotes.el" "Store for notes.")

(defvar me-notes nil "Prototype of notes.")

(defun me-store-note (note) ;; TODO implement persistence
  (add-to-list 'me-notes note)
  (async-start
   `(lambda ()
      (write-region ,(pp-to-string (me-load-notes)) nil ,me-note-file-store)))
  me-notes note)

(defun me-load-notes () ;; TODO implement persistence
  (if me-notes
      me-notes
    (setq me-notes
          (ignore-errors
            (with-temp-buffer
              (insert-file-contents-literally me-note-file-store)
              (goto-char (point-min))
              (eval `',(list-at-point)))))))

(defun me-ask-for-details-according-to-context (note)
  "Ask for NOTE details."
  (let ((text (read-string "Note:")))
    (plist-put note :then `(:string ,text))))

(defun me-override-keybiding-in-buffer (key command)
  ;; https://stackoverflow.com/questions/21486934/file-specific-key-binding-in-emacs
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))

(defun me-filter-notes-by-buffer (buffername)
  (--filter
   (ignore-errors (equal buffername (plist-get (plist-get (plist-get it :given) :node) :buffer)))
   me-notes))

(defun me-filter-notes-by-project ()
  "Gather notes by project."
  (let ((files (-map #'file-name-nondirectory (when (projectile-project-root) (projectile-current-project-files)))))
    (--filter
     (ignore-errors (-contains-p files (file-name-nondirectory (plist-get (plist-get (plist-get it :given) :node) :buffer))))
     me-notes)))

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
     "* %s\n:PROPERTIES:\n:ID:       %s\n:END:\n%s\n"
     title
     id
     content)))


(defun me-usable-molds-requiring-deps ()
  "Find molds that require dependencies to run."
  (--remove
   (let ((mold it)
         (given-cond (me-get-in it '(:given :fn))))
     (ignore-errors
       (and
        (> (length given-cond) 1)
        (eq (car given-cond) 'and)
        (me-with-mold-let mold
                          (eval (cons 'and (--remove
                                            (or
                                             (and
                                              (seqp it)
                                              (-contains? it 'executable-find))
                                             (and
                                              (seqp it)
                                              (-contains? it 'me-require)))
                                            (cdr given-cond))))))))
   me-available-molds))

(defun me-find-missing-dependencies-for-mold (mold)
  "List unmet dependencies by MOLD."
  (let ((given-cond (me-get-in mold '(:given :fn)))) ;; TODO this will break if I add other keywords than :fn
    (list
     :key (plist-get mold :key)
     :missing-dependencies
     (and
      (ignore-errors (> (length given-cond) 1))
      (eq (car given-cond) 'and)
      (--> (cdr given-cond)
           (--filter
            (or
             (and
              (seqp it)
              (-contains? it 'executable-find)
              (me-with-mold-let mold (not (eval it))))
             (and
              (seqp it)
              (-contains? it 'me-require)
              (me-with-mold-let mold (not (eval it))))
             )
            it))))))

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
    (if file
        (me-with-file file (delete-region begin end)))
    (when (and buffer (get-buffer buffer))
      (with-current-buffer buffer
        (delete-region begin end)))))

(defun me-add-node (node)
  "Add NODE to :buffer or :buffer-file using its :begin position as an anchor."
  (let ((begin (plist-get node :begin))
        (text (plist-get node :text))
        (buffer (plist-get node :buffer))
        (file (plist-get node :buffer-file)))
    (if file
        (me-with-file
         (goto-char begin)
         (insert text))
      (when (and buffer (get-buffer buffer))
        (with-current-buffer buffer
          (goto-char begin)
          (insert text))))))

(defun me-change-node (transition)
  "Run a TRANSITION to change a node. This must contain a :before and an :after node."
  (let ((before (plist-get transition :before))
        (after (plist-get transition :after)))
    (me-remove-node before)
    (me-add-node after)))

(defun me-change-nodes (transitions)
  "Change nodes according to TRANSITIONS. These contain a :before node and an :after node."
  (-each (reverse transitions)
    #'me-change-node))

(defun me-transitate-node-text (node fn)
  "Create a transition changing text of NODE via FN. FN is a function taking the text of NODE and generating new text."
  (list
   :before node
   :after (plist-put
           (copy-list node)
           :text
           (funcall fn (plist-get node :text)))))

(defun me-transitate-node-texts (nodes fn)
  "Create transitions changing texts of NODES via FN."
  (--map (me-transitate-node-text it fn) nodes))


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

(defun me-get-in (plist keys)
  "Navigate PLIST's KEYS in sequence.

For example, (me-get-in '(:a (:b (:c 1))) '(:a :b :c)) yields 1."
  (ignore-errors
    (--reduce-from
     (plist-get acc it)
     plist
     keys)))

(defun me-plist-focus (plist keys)
  "Focus only on KEYS of PLIST. For example, (me-plist-focus '(:a a :b b :c c) '(:a :c)) => '(:a a :c c)."
  (-flatten (--map (list it (plist-get plist it)) keys)))

;; (me-plist-focus '(:a a :b b :c c) '(:a :c))

(defun me-focus-on-consistent-keys (list-of-plist)
  "Focus on common keys of LIST-OF-PLIST. For example ((:a 1 :b 1 :c 1) (:a 2 :c 2)) becomes ((:a 1 :c 1) (:a 2 :c 2)). This is useful for plotting."
  (let ((keys (-reduce '-intersection (--map (-filter 'symbolp it) list-of-plist)))) ;; TODO
    (--map (me-plist-focus it keys) list-of-plist)))

(defun me-get-region ()
  "Get the active region's string."
  (when (region-active-p)
    (buffer-substring-no-properties
     (caar (region-bounds))
     (cdar (region-bounds)))))

;; https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
(defun me-org-replace-link-by-link-description ()
  "Remove the link part of an org-mode link at point and keep only the description"
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

(defun me-get-json-from-url (url)
  "Retrieve json from URL as a plist."
  (me-with-url-contents url
                        (save-excursion
                          (let ((json-object-type 'plist)
                                (json-array-type 'list))
                            (goto-char (point-min))
                            (json-read)))))

(provide 'moldable-emacs)
