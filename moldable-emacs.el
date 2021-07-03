(require 'dash)
(require 's)
(require 'async)

(defcustom me/files-with-molds
  (--map
   (concat
    (file-name-directory load-file-name) ; https://stackoverflow.com/questions/26991001/elisp-get-path-to-file-relative-to-script
    it)
   (list
    "molds/core.el"
    "molds/contrib.el"))
  "Files containing molds.")

(defun me/setup-molds ()
  "Load molds from `me/files-with-molds'."
  (-each me/files-with-molds #'load-file))

(defmacro with-file (file &rest body)
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

(defun async-map--finish (futures post-fn too-late-p poll-time)
  (if (not (some #'null (mapcar #'async-ready futures)))
      (funcall post-fn)
    (if (funcall too-late-p)
        'interrupted
      (run-with-timer
       poll-time
       nil
       #'async-map--finish
       futures
       post-fn
       too-late-p
       poll-time))))

(defun async-map (fn els &optional post-fn poll-time timeout) ;; TODO maybe I can just use this https://github.com/chuntaro/emacs-promise
  (let* ((start (current-time))
         (futures (mapcar
                   (lambda (el)
                     (async-start `(lambda () (funcall ,fn ,el))))
                   els))
         (too-late-p
          `(lambda () (>= (time-to-seconds (time-since ',start)) (or ,timeout 300)))))
    (async-map--finish
     futures
     (or post-fn (lambda ()
                   (message "async-map: completed!")
                   'completed))
     too-late-p
     (or poll-time 1))))

;; (async-map
;;  (lambda (x) (make-directory x 't))
;;  (list "/tmp/bla" "/tmp/blo" "/tmp/blu")
;;  (lambda () (message "%s" (directory-files "/tmp"))))

(defun me/make-org-table (headlines objects)
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


(defun me/insert-string-table (table-string)
  (insert table-string)
  (save-excursion
    (goto-char (point-min))
    (search-forward "|")
    (org-cycle))
  (setq-local org-confirm-elisp-link-function nil))

(defun me/insert-org-table (headlines objects)
  "Produce org table of OBJECTS formatting with HEADLINES."
  (me/insert-string-table (me/make-org-table headlines objects)))

(defun me/org-table-to-plist (table-string)  ;; TODO from https://www.reddit.com/r/emacs/comments/lo6n9y/converting_table_to_list/ useful for a table to tree mold

  (with-temp-buffer
    (save-excursion (insert table-string))
    (org-table-transpose-table-at-point)
    (let ((table (org-table-to-lisp))
          result)
      (dolist (r table result)
        (setq result (plist-put result (intern (concat ":" (car r))) (cdr r)))))))

(defun me/org-table-to-flat-plist (table-string)
  (let* ((plist (me/org-table-to-plist table-string))
         (keys (-filter 'symbolp plist)))
    (--> keys
      (--map (-map (lambda (x) (list it x)) (plist-get plist it)) it)
      (apply '-zip it)
      (-map '-flatten it))))

(defun me/flat-org-table-to-string (flat-org-table)
  (me/make-org-table
   (--map
    (list (substring (symbol-name it) 1) . (:extractor `(lambda (x) (plist-get x ,it))))
    (-filter #'keywordp (car flat-org-table)))
   flat-org-table))

(defun me/insert-flat-org-table (flat-org-table)
  (me/insert-string-table (me/flat-org-table-to-string flat-org-table)))

(defun me/first-org-table (&optional buffer)
  "Find first org table. Optionally in FILE."
  (ignore-errors
    (with-current-buffer (or buffer (current-buffer)) ;; TODO remove org links in table!
      (save-excursion
        (re-search-forward org-table-line-regexp nil t)
        (--> (org-table-to-lisp)
          (orgtbl-to-orgtbl it nil)
          (me/org-table-to-flat-plist it))))))

(defun me/all-flat-org-tables (&optional buffer)
  "Find first org table. Optionally in FILE."
  (ignore-errors
    (with-current-buffer (or buffer (current-buffer)) ;; TODO remove org links in table!
      (save-excursion
        (let (result)
          (while (and
                  (re-search-forward org-table-line-regexp nil t)
                  (goto-char (- (org-table-end) 1)))
            (setq result
                  (cons (--> (org-table-to-lisp)
                          (orgtbl-to-orgtbl it nil)
                          (me/org-table-to-flat-plist it))
                        result)))
          result)))))



(defun me/by-type (type tree)
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

(defun me/append-time (buffername )
  (concat buffername (format-time-string "%Y-%m-%d-%H:%M:%S") ;; TODO this creates problems for testing!
          ))


(defun me/mold-treesitter-to-parse-tree (&optional node)
  "Return list of all abstract syntax tree nodes one step away from leaf nodes."
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
                               :buffer-file (buffer-file-name))
                              acc))
                   (funcall fn n))
                 node))))
        (setq-local acc nil)
        (funcall fn root)
        (reverse acc)))))

(defun nodes-with-duplication (self)
  (-remove
   'null
   (--map
    (-flatten ; do not need enclosing list
     (let ((-compare-fn (lambda (a b) (string= (plist-get a :text) (plist-get b :text)))) ;; this is for making -distinct apply on the :text property
           (nodes-for-single-type (cdr it)))
       (--reduce-from ; find duplicate
        (-remove-first ; by removing only the first matching node text
         (lambda (x) (string= (plist-get x :text) (plist-get it :text)))
         acc)
        nodes-for-single-type
        (-distinct nodes-for-single-type))))
    (--filter
     (symbolp (car it)) ; taking only the nodes that tree-sitter recognize with a syntax identifier
     (--group-by (plist-get it :type) self)))))

(defvar me/available-molds nil "Available molds.")

(defvar me/mold-history nil "List of molds produced.")

(defvar me/mold-before-hook nil "Hooks to run before running a mold.")

(defvar me/mold-after-hook nil "Hooks to run after running a mold.")

(defvar me/mold-before-mold-runs-hook nil "Hooks to run before the chosen mold runs.")

(defun me/usable-mold (&optional molds buffer)
  "Returns the usable molds among the `me/available-molds' for the `current-buffer'. Optionally you can pass a list of MOLDS and a BUFFER to filter the usable ones."
  (let ((molds (or molds me/available-molds))
        (buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (--filter
       (funcall (plist-get it :given)) ;; TODO run this in parallel when time goes over 100ms (time goes already over for org-table condition when there are many org tables in the same file - I got over 2 seconds wait for 5 tables mostly empty!!!)
       molds))))

(defun me/mold ()
  "Propose a list of available molds for the current context."
  (interactive)
  (run-hooks 'me/mold-before-hook)
  (let* ((beginning (current-time))
         (molds (me/usable-mold))
         (keys (--map (plist-get it :key) molds))
         (ending (current-time))
         (_ (message "Finding molds took %s seconds in total." (time-to-seconds
                                                                (time-subtract
                                                                 ending
                                                                 beginning)))))
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
             me/mold-before-mold-runs-hook
           (funcall it mold))
         mold)
       it)
      (plist-get it :then)
      funcall
      switch-to-buffer-other-window)
    (run-hooks 'me/mold-after-hook)))

(defvar me/last-example nil "Last automatically generated example
for mold. This should simplify the testing and documentation of molds.")

(defun me/record-given-of-example ()
  "Reset and store in `me/last-example' the given of a mold example."
  (let* ((type (if (buffer-file-name) 'file 'buffer))
         (name (or (buffer-file-name) (buffer-name)))
         (mode major-mode)
         (contents (buffer-substring-no-properties (point-min) (point-max))))
    (setq me/last-example nil)
    (setq me/last-example
          `(:given
            (
             :type ,type
             :name ,name
             :mode ,mode
             :contents ,contents)))))

(defun me/record-then-of-example ()
  "Reset and store in `me/last-example' the then of a mold example."
  (let* ((type (if (buffer-file-name) 'file 'buffer))
         (name (or (buffer-file-name) (buffer-name)))
         (mode major-mode)
         (contents (buffer-substring-no-properties (point-min) (point-max))))
    (plist-put
     me/last-example
     :then
     `(
       :type ,type
       :name ,name
       :mode ,mode
       :contents ,contents))))

(add-hook 'me/mold-before-hook #'me/record-given-of-example)

(add-hook 'me/mold-after-hook #'me/record-then-of-example)


(defun me/warn-on-run-if-no-example (mold)
  "Emit warning if MOLD has no examples."
  (unless (plist-get mold :examples)
    (warn "Mold %s has no examples! Would you mind to add one?\nYou can use TODO now to add the last usage as an example.\n"
          (plist-get mold :key))))

(defun me/warn-on-run-if-no-docs (mold)
  "Emit warning if MOLD has no examples."
  (unless (plist-get mold :docs)
    (warn "Mold %s has no docs! Would you mind to add a line to tell what it is for?\n"
          (plist-get mold :key))))

(add-hook 'me/mold-before-mold-runs-hook #'me/warn-on-run-if-no-example)
(add-hook 'me/mold-before-mold-runs-hook #'me/warn-on-run-if-no-docs)

(defmacro me/given (given &rest body)
  "Setup according to GIVEN and run BODY."
  (let* ((given (eval given))
         (type (plist-get  given :type))
         (name (plist-get given :name))
         (contents (plist-get given :contents))
         (mode (plist-get given :mode)))
    (if (equal type 'buffer)
        `(with-temp-buffer
           (rename-buffer ,name)
           (insert ,contents)
           (,(if mode mode 'fundamental-mode))
           ,@body)
      `(with-temp-file ,name
         (insert ,contents)
         (,(if mode mode 'fundamental-mode))
         ,@body))))

(ert-deftest me/given_valid-with-buffer ()
  (should
   (equal
    (me/given '(:type buffer :name "some.txt" :contents "bla" :mode emacs-lisp-mode) (format "%s %s" (buffer-name) major-mode))
    "some.txt emacs-lisp-mode")))

(defun me/check-then-clause (then)
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

(defun me/check-example (example run-fn)
  "Run RUN-FN in the EXAMPLE."
  (let* ((start (plist-get example :given))
         (end (plist-get example :then)))
    (me/given start
              (goto-char (point-min))
              (with-current-buffer (funcall run-fn)
                (me/check-then-clause end)))))

(defun me/test-example (example run-fn)
  "Test RUN-FN in the EXAMPLE."
  (plist-get (me/check-example example run-fn) :success))


(defun me/test-mold-examples (mold)
  "Check that all MOLD's examples are working."
  (--reduce
   (and it acc)
   (--map
    (me/test-example it (lambda () (set-buffer (funcall (plist-get mold :then)))))
    (plist-get mold :examples))))

;; (me/test-mold-examples (me/find-mold "Playground"))

(defun me/example-to-docstring (example)
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
     "Given the \"%s\" %s with the following contents:\n\n----------\n\n%s\n\n----------\n\nThe mold returns the \"%s\" %s with the following contents:\n\n----------\n\n%s\n\n----------"
     start-name
     start-buffer-or-file
     start-contents
     end-name
     end-buffer-or-file
     end-contents
     )))

(ert-deftest me/example-to-docstring_produce-doc-string ()
  (should
   (string=
    (me/example-to-docstring '(:given (:type buffer :name "somebuffer" :contents "some contents") :then (:type file :name "/tmp/somefile.txt" :contents "some new contents")))

    "Given the \"somebuffer\" buffer with the following contents:

----------

some contents

----------

The mold returns the \"/tmp/somefile.txt\" file with the following contents:

----------

some new contents

----------"

    )))

(defun me/mold-docs ()
  "Propose a list of available views for the current context."
  (interactive)
  (let* ((molds (me/usable-mold))
         (keys (--map (plist-get it :key) molds)))
    (--> keys
      (completing-read
       "Pick the view you need:"
       it)
      (-find
       (lambda (x)
         (string=
          (plist-get x :key)
          it))
       molds)
      (list
       :title
       (format "Documentation about %s mold" (plist-get it :key))
       :documentation
       (concat (plist-get it :docs)
               (let ((examples (plist-get it :examples)))
                 (when (> (length examples) 0)
                   (me/example-to-docstring (car examples))))))
      (progn
        (switch-to-buffer (get-buffer-create (plist-get it :title)))
        (erase-buffer)
        (insert (plist-get it :documentation))))))

(defun me/show-example (example run-fn)
  "Run RUN-FN in the EXAMPLE."
  (let* ((name (plist-get example :name))
         (start (plist-get example :given))
         (end (plist-get example :then)))
    (me/given start
              (funcall run-fn)
              (me/then-assert end))))


(defun me/demo-example (example)
  "Demo EXAMPLE in a dedicated frame."
  (let* ((name (plist-get example :name))
         (given (plist-get example :given))
         (given-name (plist-get given :name))
         (given-contents (plist-get given :contents))
         (then (plist-get example :then))
         (then-name (plist-get then :name))
         (then-contents (plist-get then :contents))
         (frame (make-frame `((name . ,name) (width . 100) (height . 70) (fullscreen . nil)))))
    (x-focus-frame frame)
    (select-frame frame)
    (split-window-horizontally)
    (switch-to-buffer given-name)
    (erase-buffer)
    (insert given-contents)
    (other-window 1)
    (switch-to-buffer then-name)
    (erase-buffer)
    (insert then-contents)
    ;;(delete-frame frame) ; probably I do not need to delete it, but just to make it smaller? Indeed, I can delete it manually
    )
  ;;(delete-other-frames)
  )

;; (me/demo-example '(:name "some example" :given (:type buffer :name "somebuffer" :contents "some contents") :then (:type file :name "/tmp/somefile.txt" :contents "some new contents")))

(defun me/mold-demo (mold)
  (if-let ((mold mold)
           (example (nth 0 (plist-get mold :examples))))
      (me/demo-example example)
    (error "No example available for this mold to demo.")))

(defun me/mold-demo-by-key (key)
  (me/mold-demo (me/find-mold key)))

(defcustom me/enable-history 't "Keeps history for current session, if defined.")

(defun me/save-buffer-in-history ()
  "Enable keeping history for current session."
  (setq me/mold-history
        (concatenate
         'list me/mold-history
         (list (buffer-name)))))

(when me/enable-history (add-hook 'me/mold-before-hook #'me/save-buffer-in-history))

(defun me/open-at-point ()
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

(defun me/find-mold (key)
  "Find mold for KEY."
  (--find (equal key (plist-get it :key)) me/available-molds))

(defun me/go-back ()
  "Go back to previous mold."
  (interactive)
  (let ((current-index (--find-index (string= it (buffer-name)) me/mold-history)))
    (ignore-errors
      (switch-to-buffer
       (nth
        (- current-index 1)
        me/mold-history)))))

(defun me/go-forward ()
  "Go back to next mold." ;; TODO this is naive: can easily be wrong
  (interactive)
  (let ((current-index (--find-index (string= it (buffer-name)) me/mold-history)))
    (ignore-errors
      (switch-to-buffer
       (nth
        (+ current-index 1)
        me/mold-history)))))

(defun me/add-to-available-molds (mold)
  (let ((-compare-fn (lambda (x y) (equal (plist-get x :key) (plist-get y :key))))
        (mold (concatenate 'list mold (list :origin (me/find-origin-file-of-mold (plist-get mold :key))))))
    (setq me/available-molds
          (-distinct (add-to-list 'me/available-molds mold)))))

(defvar me/before-register-mold-hook nil "Hooks to run before a mold is registered.")

(defun me/find-origin-file-of-mold (key)
  (--find
   (with-current-buffer (find-file-noselect it)
     (save-excursion
       (goto-char (point-min))
       (ignore-errors (search-forward (concat "\"" key "\"")))))
   me/files-with-molds))

(defmacro me/register-mold (&rest mold) ;; TODO I should validate molds somehow, not just assign them! Also use hashmap?
  (--each me/before-register-mold-hook (funcall it mold))
  `(me/add-to-available-molds ',mold))

(ert-deftest me/register-mold_new-mold () ;; TODO use this as a documentation mold example??
  (let ((me/available-molds nil))
    (me/register-mold
     :key "bla"
     :description "bla"
     :given (lambda () 't)
     :then (lambda () 't))
    (should
     (string=
      (plist-get (car me/available-molds) :key)
      "bla"))))

(defun me/find-relative-test-report (filepath)
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

(defun me/make-elisp-file-link (description target &optional link-type)
  (format "[[%s:%s][%s]]" (or link-type "file") target description))

(defun me/make-elisp-navigation-link (name file-name)
  "Make an Elisp Org link that navigates to a position of NAME in BUFFERNAME."
  (let* ((pos-file (with-temp-buffer
                     (insert-file-contents-literally file-name)
                     (goto-char (point-min))
                     (list (search-forward (format "%s" name)) file-name))))
    (me/make-elisp-file-link
     name
     (format
      "(progn (find-file-other-window \"%s\") (goto-char %s))"
      (nth 1 pos-file)
      (nth 0 pos-file))
     "elisp")))

(defun me/color-string (str color)
  (propertize
   str
   'display
   (propertize
    str
    'face
    (list :background color))))

(defun me/buffer-changed-while-the-mold-is-on-p (buffername)
  "Return `t' if buffer changed from last call."
  ;; TODO store hash of `buffername' in a unique variable and test against if it the variable is defined
  nil)

;; https://hungyi.net/posts/org-mode-subtree-contents/
(defun me/org-copy-subtree-contents (&optional buffer position)
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

(defun me/org-to-flatten-tree (buffername)
  (--map (concatenate
          'list
          (list :type 'org)
          (plist-put (cadr it) :title nil)
          `(:buffer ,(buffer-name))
          `(:buffer-file ,(buffer-file-name))
          `(:text ,(me/org-copy-subtree-contents (plist-get it :begin))))
         (org-ql-query :select 'element :from (list buffername))))

(defun me/register-mold-by-key (key mold)
  "Register composition MOLD with "
  (me/add-to-available-molds (plist-put mold :key key)))

(defun me/mold-compose-molds (mold1 mold2)
  `(
    :key ,(format
           "CompositionOf%sAnd%s"
           (plist-get mold1 :key)
           (plist-get mold2 :key))
    :given ,(plist-get mold1 :given) ;; TODO I can do better than this: I want to join the :given of the composed molds. This will be useful to list dependencies.
    :then (lambda ()
            (when (funcall ,(plist-get mold1 :given))
              (switch-to-buffer (funcall ,(plist-get mold1 :then)))
              (when (funcall ,(plist-get mold2 :given))
                (switch-to-buffer
                 (funcall ,(plist-get mold2 :then))))))))

(defun me/mold-compose (m1 m2)
  (let ((mold1 (if (stringp m1) (me/find-mold m1) m1))
        (mold2 (if (stringp m2) (me/find-mold m2) m2)))
    (if (and mold1 mold2)
        (me/mold-compose-molds mold1 mold2)
      (error (format "Could not find molds, check out: %s." `((,m1 . ,mold1) (,m2 . ,mold2)))))))


(defvar me/last-used-mold nil "Keep the `:key' of last used mold.")

(defun me/set-last-mold (mold)
  (setq me/last-used-mold (plist-get mold :key)))

(add-hook 'me/mold-before-mold-runs-hook #'me/set-last-mold)

(defun me/mold-add-last-example ()
  "Add `me/last-example' to last mold."
  (interactive)
  (when me/last-used-mold
    (find-file (plist-get (me/find-mold me/last-used-mold) :origin))
    (goto-char (point-min))
    (search-forward (format ":key \"%s\"" me/last-used-mold))
    (let* ((result (me/check-example me/last-example (plist-get (me/find-mold me/last-used-mold) :then)))
           (pass (plist-get result :success))
           (issues (plist-get result :issues)))
      (unless pass
        (warn (format
               "The example you are trying to add does not work because the following did not match:\n%s"
               issues))))
    (kill-new (pp-to-string me/last-example))
    (message "You have the example of the last run of this mold in the kill ring: use it!")
    ;; TODO make this smarter
    ))

(defun me/require (dependency)
  "Try to require DEPENDENCY, and just give nil if not found."
  (ignore-errors (require dependency)))

(defun me/mold-insert-name ()
  "Insert a mold name at point."
  (interactive)
  (--> me/available-molds
    (--map (plist-get it :key) it)
    (completing-read
     "Insert at point the following mold name:"
     it)
    insert))


;; taken from: https://emacs.stackexchange.com/questions/13514/how-to-obtain-the-statistic-of-the-the-frequency-of-words-in-a-buffer
(defvar me/punctuation-marks '(","
                               "."
                               "'"
                               "&"
                               "\"")
  "List of Punctuation Marks that you want to count.")

(defun me/count-raw-word-list (raw-word-list)
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

(defun me/word-stats (string)
  "Return word (as a token between spaces) frequency in STRING."
  (let* ((words (split-string
                 (downcase string)
                 (format "[ %s\f\t\n\r\v]+"
                         (mapconcat #'identity me/punctuation-marks ""))
                 t))
         (punctuation-marks (--filter
                             (member it me/punctuation-marks)
                             (split-string string "" t)))
         (raw-word-list (append punctuation-marks words))
         (word-list (me/count-raw-word-list raw-word-list)))
    (sort word-list (lambda (a b) (> (cdr a) (cdr b))))))


(defun me/get-reading-time (text)
  "Calculate reading time of TEXT in minutes according to https://www.coengoedegebure.com/add-reading-time-to-articles/."
  (with-temp-buffer
    (insert text)
    (/ (count-words (point-min) (point-max)) 228)))

(defun me/get-book-pages (text)
  "Calculate number of book pages TEXT would fill according to https://kindlepreneur.com/words-per-page/."
  (with-temp-buffer
    (insert text)
    (/ (count-words (point-min) (point-max)) 280)))

(defun me/insert-treesitter-follow-overlay (nodes &optional transformer)
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

(defun me/calc-numeric-p (text)
  "Check if TEXT is a numeric arithmetic expression `calc' can work with."
  (let ((calc-eval-error 't)) (ignore-errors (calc-eval text 'num)))
  )

(defun me/arithmetic-component-p (it)
  "Is IT an arithmetic component?"
  (or
   (string= it (number-to-string (string-to-number it)))
   (string= "-" it)
   (string= "+" it)
   (string= "/" it)
   (string= "*" it)
   (string= "%" it)
   (string= "^" it)))

(defun me/arithmetic-expression-member-p (it)
  "Tell if string IT contains an arithmetic member."
  (or (me/arithmetic-component-p it)
      ;; in case we have something like "1+1"
      (-all?
       #'me/arithmetic-component-p
       (s-split "" it 't))))

(defun me/arithmetic-at-point () ;; TODO needs refactoring!
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
       (-take-while #'me/arithmetic-expression-member-p it)
       (reverse it)
       (s-join " " it))
     ;; take only arithmetic words from point to end of line
     (--> it
       (nth 1 it)
       (s-split " " it 't)
       (-take-while #'me/arithmetic-expression-member-p it)
       (s-join " " it)))
    ;; join the two parts
    (concat (nth 0 it) (nth 1 it))
    s-trim
    (unless (string-blank-p it) it)))

(provide 'moldable-emacs)
