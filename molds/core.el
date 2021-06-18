(me/register-mold
 :key "Playground"
 :given (lambda () 't)
 :then (lambda ()
         (let ((tree (or
                      (ignore-errors
                        (me/mold-treesitter-to-parse-tree))
                      (ignore-errors
                        (save-excursion
                          (goto-char (point-min))
                          (eval `',(read (current-buffer)))))))
               (buffer (get-buffer-create (me/append-time "m/tree-playground-from"))))
           (with-current-buffer buffer
             (emacs-lisp-mode)
             (erase-buffer)
             (setq-local self tree)
             buffer)
           buffer))
 ;; TODO experimental for auto-completion: how can I make molds easy to autocomplete?
 :actions (me/by-type identity)
 :docs "You can write any Elisp here. 
Then you can evaluate with `EvalSexp'. 
This mold saves structured data of the previous buffer 
in the local variable `self'."
 :examples ((
             :name "Empty file"
             :given (:type file :name "/tmp/test.txt" :mode text-mode :contents "")
             :then (:type buffer :name (me/append-time "m/tree-playground-from") :mode emacs-lisp-mode :contents ""))))

(me/register-mold
 :key "CodeAsTree"
 :given (lambda () (seq-contains-p minor-mode-list 'tree-sitter-mode))
 :then (lambda ()
         (let* ((buffer (get-buffer-create "m/tree"))
                (tree (me/mold-treesitter-to-parse-tree)))
           (with-current-buffer buffer
             (erase-buffer)
             (prin1 tree buffer) ;; TODO I need to do keep the position, or allow editing in place, no?
             (pp-buffer)
             (emacs-lisp-mode)
             buffer))))

(me/register-mold
 :key "NodeAtPointToTree"
 :given (lambda () (ignore-errors (tree-sitter-node-at-point)))
 :then (lambda ()
         (let* ((buffer (get-buffer-create "m/tree"))
                (tree (me/mold-treesitter-to-parse-tree (tree-sitter-node-at-point))))
           (with-current-buffer buffer
             (erase-buffer)
             (emacs-lisp-mode)
             (prin1 tree buffer)
             (pp-buffer)
             (setq self tree)
             (current-buffer)))))

;; TODO maybe mold treeWithJsonToPlist? (--map (json-parse-string (plist-get it :text) :object-type 'plist) self)
;; TODO and maybe plist to Org Table? See "GebE2ECumulativeErrors" for that

(me/register-mold
 :key "XMLToTree"
 :given (lambda () (or
                    (eq major-mode 'html-mode)
                    (eq major-mode 'nxml-mode)))
 :when (lambda () (me/buffer-changed-while-the-mold-is-on-p (buffer-name)))
 :then (lambda ()
         (let* ((buffer (get-buffer-create (concat "m/treeXML" (buffer-name))))
                (tree (libxml-parse-html-region (point-min) (point-max))))
           (with-current-buffer buffer
             (emacs-lisp-mode)
             (erase-buffer)
             (pp-display-expression tree buffer)
             (goto-char (point-min))
             buffer))))

;; TODO maybe add parent as well? There is not this information in the org-ql node.
(me/register-mold
 :key "OrgAsTree"
 :given (lambda () (and (eq major-mode 'org-mode) (require 'org-ql)))
 :then (lambda ()
         (let* ((buffer (get-buffer-create "m/tree"))
                (tree (me/org-to-flatten-tree (current-buffer))))
           
           (with-current-buffer buffer
             (emacs-lisp-mode)
             (erase-buffer)
             (pp-display-expression tree buffer)
             (setq-local self tree)
             buffer))))

(me/register-mold
 :key "TreeAsGraph"
 :given (lambda () (and nil (eq major-mode 'emacs-lisp-mode) (equal (buffer-name) "m/tree")))
 :then (lambda ()
       (pair-tree (save-excursion (goto-char (point-min)) (read (current-buffer)))))) ;; TODO fail I need a better candidate to visualize trees...

(me/register-mold
 :key "SentencesAsTree"
 :given (lambda () (and (eq major-mode 'text-mode)))
 :then (lambda ()
       (let* ((buffer (get-buffer-create "SentencesTree"))
              (sentences (s-split (sentence-end) (buffer-substring-no-properties (point-min) (point-max)) 't)))
         (with-current-buffer buffer
           (erase-buffer)
           (prin1 (mapcar 'list sentences) buffer) ;; TODO I need to do keep the position, or allow editing in place, no?
           (pp-buffer)
           buffer))))

(me/register-mold
 :key "ElispAsTree"
 :given (lambda () (eq major-mode 'emacs-lisp-mode))
 :then (lambda ()
       (let ((sexps)
             (buffer (get-buffer-create "m/tree")))
         (ignore-errors
           (save-excursion
             (goto-char (point-min))
             (while (setq sexp (read (current-buffer)))
               (push sexp sexps)))
           )
         (with-current-buffer buffer
           (erase-buffer)
           (prin1 (reverse sexps) buffer)
           (emacs-lisp-mode)
           (pp-buffer)
           buffer))))

(me/register-mold
 :key "TreeOfDuplicates"
 :given (lambda () (and (s-starts-with-p "m/tree" (buffer-name)) (eq major-mode 'emacs-lisp-mode)))
 :then (lambda ()
         (let* ((self (ignore-errors
                        (save-excursion
                          (goto-char (point-min))
                          (eval `',(read (current-buffer))))))
                (duplicated-tree (nodes-with-duplication self))
                (buffer (get-buffer-create (me/append-time "m/tree-from" ))))
           (with-current-buffer buffer
             (erase-buffer)
             (emacs-lisp-mode)
             (setq-local self duplicated-tree)
             (pp-display-expression duplicated-tree buffer)
             buffer)
           buffer)))

(me/register-mold
 :key "EvalSexp"
 :given (lambda () (eq major-mode 'emacs-lisp-mode))
 :then (lambda ()
         (let ((_ (remove-overlays))
               (_ (overlay-put
                   (make-overlay
                    (car (thing-at-point-bounds-of-list-at-point))
                    (cdr (thing-at-point-bounds-of-list-at-point)))
                   'face
                   'bold))
               (tree (list-at-point))
               (result (ignore-errors (eval (list-at-point))))
               (buffer (get-buffer-create (me/append-time "m/tree-eval-from"))))
           (with-current-buffer buffer
             (emacs-lisp-mode)
             (erase-buffer)
             (pp-display-expression (or result tree) buffer)
             (setq-local self tree)
             buffer)
           buffer))
 :docs "You can evaluate a lisp expression after point and produce a new buffer with the result."
 :examples ((
             :name "Simple addition"
             :given (
                     :type file
                     :name "/home/andrea/someElispBuffer"
                     :mode emacs-lisp-mode
                     :contents "(+ 1 2)")
             :then (
                    :type buffer
                    :name (me/append-time "m/tree-eval-from")
                    :mode emacs-lisp-mode
                    :contents "3"))))

(me/register-mold
 :key "GotoNodeBuffer"
 :given (lambda () (and (s-starts-with-p "m/tree" (buffer-name)) (eq major-mode 'emacs-lisp-mode) (-contains-p (list-at-point) :buffer)))
 :then (lambda ()
       (let* ((old-buffer (current-buffer))
              (result (list-at-point))
              (buffer (plist-get result :buffer)))
         (switch-to-buffer buffer)
         (message "%s" (plist-get result :begin))
         (goto-char (plist-get result :begin))
         (switch-to-buffer old-buffer)
         buffer)))

(me/register-mold
 :key "TreeToOrgTodos"
 :given (lambda () (and (s-starts-with-p "m/tree" (buffer-name)) (eq major-mode 'emacs-lisp-mode)))
 :then (lambda ()
       (let* ((tree (ignore-errors
                      (save-excursion
                        (goto-char (point-min))
                        (eval `',(read (current-buffer))))))
              (buffer (get-buffer-create "TodosOnTheFly"))
              (text (concat
                     "* Todo list [/]\n"
                     (s-join
                      "\n\n"
                      (--map
                       (format
                        "- [ ] [[elisp:%s][%s]]"
                        (format
                         "(progn (find-file-other-window \"%s\") (goto-char %s))"
                         (plist-get it :buffer-file)
                         (plist-get it :begin))
                        (s-truncate 100 (plist-get it :text)))
                       tree)))))
         (with-current-buffer buffer
           (erase-buffer)
           (org-mode)
           (setq-local org-confirm-elisp-link-function nil)
           (insert text))
         buffer)))

(me/register-mold
 :key "Stats"
 :given (lambda () 't)
 :then (lambda () ;; TODO deliver this in org-mode buffer because later I can interpret that in a tree and run new molds on it!
       (let* ((old-buffer (buffer-name))
              (buffer (get-buffer-create "Statistics"))
              (self (me/mold-treesitter-to-parse-tree))
              (lines (count-lines-page))
              (words (call-interactively 'count-words))
              (book-pages (/ (count-words (point-min) (point-max)) 280)) ;; https://kindlepreneur.com/words-per-page/
              (reading-time (/ (count-words (point-min) (point-max)) 228)) ; https://www.coengoedegebure.com/add-reading-time-to-articles/
              (word-analysis (--filter (> (length (car it)) 2) (c/word-stats (buffer-substring-no-properties (point-min) (point-max))))) ;; TODO fix when I merge code-compass
              (word-analysis-stats (-concat (-take 3 word-analysis) (reverse (-take 3 (reverse word-analysis)))))
              (funs (when self (length (--filter (eq (plist-get it :type) 'function_definition) self))))
              (ifs (when self (length (--filter (or (eq (plist-get it :type) 'if_expression) (eq (plist-get it :type) 'if_statement)) self))))
              (classes (when self (length (--filter  (eq (plist-get it :type) 'class_definition) self))))
              (comments (when self (length (--filter (eq (plist-get it :type) 'comment) self)))))
         (with-current-buffer buffer
           (read-only-mode -1)
           (erase-buffer)
           (org-mode)
           (insert "* Generic Stats\n\n")
           (insert (format "- Reading time: %s minutes \n" reading-time))
           (insert (format "- %s\n" lines))
           (insert (format "- %s\n" words))
           (insert (format "- Average book pages for this text: %s\n\n" book-pages))
           (insert (format "- Buffer size in KiloBytes: %s\n\n" (buffer-size)))
           (insert "- Up to three most and least used words:\n\n") ;; TODO maybe add an org link that can rerun the complete analysis keeping track of the previous buffer by creating a link [[(elisp: c/analyswords old-buffer; navigate to new buffer)][click here for all the analysis]] OR I could just implement the linking of mold buffers for at least last buffer!! 
           (--each word-analysis-stats
             (insert (format "  %s | %s\n" (substring (concat (number-to-string (cdr it)) (s-repeat 5 " ")) 0 3) (car it))))
           (insert "\n")
           (when funs
             (insert "* Programming Stats\n\n")  
             (insert "\n")
             (insert "-- Code Stats --\n\n")
             (insert (format "#Functions: %s \n" funs))
             (insert (format "#If-else: %s \n" ifs))
             (insert (format "#Classes: %s \n" classes))
             (insert (format "#Comments: %s \n" comments)))
           (insert "\n")
           (when self
             (message "debug3")
             
             (insert "* Duplication Stats\n\n")
             (insert "-- Code Duplication By Token Type --\n\n")
             (let* ((duplicates-by-type (--group-by (plist-get it :type) (nodes-with-duplication self)))
                    (texts-by-type
                     (--map
                      (cons (car it) (-map (lambda (x) (plist-get x :text)) (cdr it)))
                      (--group-by (plist-get it :type) self))))
               (require 'tree-sitter-query)
               (cursor-sensor-mode)
               (--each
                   duplicates-by-type
                 (let ((type (car it))
                       (texts (--map (ignore-errors (plist-get it :text)) it))
                       (beg (point)))
                   (insert
                    (format
                     "%s: %s/%s\n"
                     type
                     (length (--filter (-contains-p texts it) (-find (lambda (x) (eq (car x) type)) texts-by-type)))
                     (length (cdr (-find (lambda (x) (eq (car x) type)) texts-by-type)))
                     ))
                   (let ((ov (make-overlay beg (- (point) 1))))
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
                                   (tree-sitter-query--clean-target-buffer))))))))))))
           (read-only-mode)
           buffer))))

(me/register-mold
 :key "JsonAsTree"
 :given (lambda () (eq major-mode 'json-mode)) ;; TODO or region contains json
 :then (lambda ()
       (let ((json
              (save-excursion
                (let ((json-object-type 'alist)
                      (json-array-type 'list))
                  (goto-char (point-min))
                  (json-read))))
             (buffer (get-buffer-create "m/tree")))
         (with-current-buffer buffer
           (erase-buffer)
           (prin1 json buffer)
           (emacs-lisp-mode)
           (pp-buffer)
           buffer))))

(me/register-mold
 :key "AsParseTree"
 :given (lambda () (or (eq major-mode 'json-mode)
                         (eq major-mode 'yaml-mode)
                         (eq major-mode 'javascript-mode)
                         (eq major-mode 'xml-mode)
                         (eq major-mode 'scala-mode)
                         (eq major-mode 'tilde-mode)
                         (eq major-mode 'typescript-mode))) ;; TODO or region contains json
 :then (lambda ()
       (tree-sitter-debug-mode)
       tree-sitter-debug--tree-buffer))

(me/register-mold
 :key "FirstOrgTable"
 :given (lambda () (eq major-mode 'org-mode))
 :then (lambda ()
         (let ((table (me/first-org-table))
               (buffer (get-buffer-create (me/append-time "m/first-org-table"))))
           (with-current-buffer buffer
             (read-only-mode -1)
             (erase-buffer)
             (org-mode)
             (me/insert-flat-org-table table)
             (goto-char (point-min))
             (setq-local self table)
             (read-only-mode)
             buffer)))
 :examples ((
             :name "Sample table"
             :given (
                     :type file
                     :name "/tmp/test.org"
                     :mode org-mode
                     :contents "* Some Heading


| a | b |
|---+---|
| 1 | 2 |
| 2 | 3 |
")
             :then (
                    :type buffer
                    :name (me/append-time "m/first-org-table")
                    :mode org-mode
                    :contents "| a | b |
|---+---|
| 1 | 2 |
| 2 | 3 |"))))


(me/register-mold
 :key "OrgTableToCSV"
 :given (lambda () (and (eq major-mode 'org-mode) (s-starts-with-p "m/first-org-table" (buffer-name))))
 :then (lambda ()
       (let ((table (org-table-to-lisp))
             (buffer (get-buffer-create (me/append-time "m/csv-from-org-table"))))
         (with-current-buffer buffer
           (read-only-mode -1)
           (erase-buffer)
           (csv-mode)
           (insert (orgtbl-to-csv table nil))
           (goto-char (point-min))
           (while (eq (org-next-link) 't)
             (ag/org-replace-link-by-link-description))
           (setq-local self table)
           (read-only-mode)
           buffer))))

(me/register-mold
 :key "CSVToOrgTable"
 :given (lambda () (eq major-mode 'csv-mode))
 :then (lambda ()
         (let ((buffer (get-buffer-create (me/append-time "m/org-table-from-csv")))
               (table (buffer-string)))
           (with-current-buffer buffer
             (read-only-mode -1)
             (erase-buffer)
             (org-mode)
             (insert table)
             (mark-whole-buffer)
             (call-interactively #'org-table-create-or-convert-from-region)
             (goto-char (point-min))
             (setq-local self (org-table-to-lisp))
             (read-only-mode)
             buffer))))


(defcustom me/note-file-store "~/workspace/agenda/moldableNotes.el" "Store for notes.")

(defvar me/notes nil "Prototype of notes.")

(defun me/store-note (note) ;; TODO implement persistence
  (add-to-list 'me/notes note)
  (async-start
   `(lambda ()
      (write-region ,(pp-to-string (me/load-notes)) nil ,me/note-file-store)))
  me/notes note)

(defun me/load-notes () ;; TODO implement persistence
  (if me/notes
      me/notes
    (setq me/notes
          (ignore-errors
            (with-temp-buffer
              (insert-file-contents-literally me/note-file-store)
              (goto-char (point-min))
              (eval `',(list-at-point)))))))

(defun me/ask-for-details-according-to-context (note)
  (let ((text (read-string "Note:"))) ;; TODO I want to ask also the color this should highlight!
    (plist-put note :then `(:string ,text))))

(defun me/override-keybiding-in-buffer (key command)
  ;; https://stackoverflow.com/questions/21486934/file-specific-key-binding-in-emacs
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))

(me/register-mold
 :key "Annotate"
 :given (lambda () 't)
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (boundaries (or (region-bounds) `((,(beginning-of-line) . ,(end-of-line))))) ;; TODO the fallback is not ideal, because a file line can change pretty easily
                (default-note
                  `(
                    :given (:node
                            (
                             :key ,(format-time-string "%Y-%m-%d+%H:%M:%S")
                             :type position
                             :text ,(buffer-substring-no-properties (caar boundaries) (cdar boundaries))
                             :begin ,(caar boundaries)
                             :end ,(cdar boundaries)
                             :buffer ,buffername
                             :buffer-file ,(buffer-file-name)
                             :mode ,major-mode))
                    :when nil
                    :then nil))
                (note (me/ask-for-details-according-to-context default-note))
                (buffer (get-buffer-create "Annotate buffer")))
           (with-current-buffer buffer
             (erase-buffer)
             (emacs-lisp-mode)
             (prin1 note buffer)
             (pp-buffer)
             (me/override-keybiding-in-buffer
              (kbd "C-x C-s")
              '(lambda ()
                 (interactive)
                 (let ((note
                        (ignore-errors
                          (save-excursion
                            (goto-char (point-min))
                            (eval `',(list-at-point))))))
                   (setq-local self note)
                   (me/store-note note)
                   (message "Note stored!"))))
             (setq-local self note))
           buffer)))

(defun me/filter-notes-by-buffer (buffername)
  (--filter
   (ignore-errors (equal buffername (plist-get (plist-get (plist-get it :given) :node) :buffer)))
   me/notes))

(me/register-mold
 :key "ShowNotesByBuffer"
 :given (lambda () (and (me/filter-notes-by-buffer (buffer-name))))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (buffer (get-buffer-create (format "Notes for %s" buffername)))
                (notes (me/filter-notes-by-buffer buffername)))
           (with-current-buffer buffer
             (erase-buffer)
             (emacs-lisp-mode)
             (prin1 notes buffer)
             (setq self notes)
             (pp-buffer))
           buffer)))

(defun me/filter-notes-by-mode (mode)
  (--filter
   (ignore-errors (equal mode (plist-get (plist-get (plist-get it :given) :node) :mode)))
   me/notes))

(me/register-mold
 :key "ShowNotesByMode"
 :given (lambda () (and (me/filter-notes-by-mode major-mode)))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (buffer (get-buffer-create (format "Notes for %s" buffername)))
                (notes (me/filter-notes-by-buffer buffername)))
           (with-current-buffer buffer
             (erase-buffer)
             (emacs-lisp-mode)
             (prin1 notes buffer)
             (setq self notes)
             (pp-buffer))
           buffer)))

(defun me/note-to-org-heading (note)
  "Turn a NOTE in a `org-mode' heading."
  (let* ((given (plist-get (plist-get note :given) :node))
         (then (plist-get note :then))
         (id (plist-get given :key))
         (title (me/make-elisp-file-link
                 (s-truncate 60 (plist-get given :text))
                 (plist-get given :buffer-file)))
         (content (plist-get then :string)))
    (format
     "* %s\n:PROPERTIES:\n:ID:       %s\n:END:\n%s\n"
     title
     id
     content)))

(me/register-mold
 :key "NotesToOrg"
 :given (lambda ()
          (and
           (s-starts-with-p "Notes" (buffer-name))
           (me/load-notes)))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (buffer (get-buffer-create (format "Notes for %s as Org" buffername)))
                (notes
                 (or
                  (when 't self) ;; TODO check that `self' contains notes
                  (me/load-notes)))) 
           (with-current-buffer buffer
             (erase-buffer)
             (org-mode)
             (setq-local org-confirm-elisp-link-function nil)
             (insert (--reduce-from
                      (concat acc (me/note-to-org-heading it) "\n")
                      ""
                      notes))
             (setq self notes)
             (me/override-keybiding-in-buffer
              (kbd "C-x C-s")
              '(lambda ()
                 (interactive)
                 (--each
                     (me/org-to-flatten-tree (current-buffer))
                   (plist-put
                    (plist-get
                     (-find (lambda (el) (equal (plist-get el :key) (plist-get it :id))) self)
                     :then)
                    :string
                    (plist-get it :text)))
                 (--each self (me/store-note it))
                 (message "Notes stored!"))))
           buffer)))

(me/register-mold
 :key "NoteToOrg"
 :given (lambda ()
          (and
           (s-starts-with-p "Annotate buffer" (buffer-name))
           ))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (buffer (get-buffer-create "Org Annotate buffer"))
                (note self)) 
           (with-current-buffer buffer
             (erase-buffer)
             (org-mode)
             (setq-local org-confirm-elisp-link-function nil)
             (insert (me/note-to-org-heading note))
             (setq self note)
             (me/override-keybiding-in-buffer
              (kbd "C-x C-s")
              '(lambda ()
                 (interactive)
                 (--each
                     (me/org-to-flatten-tree (current-buffer))
                   (plist-put
                    (plist-get
                     (-find (lambda (el) (equal (plist-get el :key) (plist-get it :id))) self)
                     :then)
                    :string
                    (plist-get it :text)))
                 (--each self (me/store-note it))
                 (message "Notes stored!"))))
           buffer)))

(me/register-mold-by-key "AnnotateWithOrg"
                         (me/mold-compose "Annotate" "NoteToOrg"))

(me/register-mold
 :key "ShowAllNotes"
 :given (lambda () (me/load-notes))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (buffer (get-buffer-create (format "Notes available" buffername)))
                (notes (me/load-notes)))
           (with-current-buffer buffer
             (erase-buffer)
             (emacs-lisp-mode)
             (prin1 notes buffer)
             (setq self notes)
             (pp-buffer))
           buffer)))

(me/register-mold
 :key "WhatMoldsCanIUse"
 :given (lambda () t)
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (buffer (get-buffer-create (format "What Molds Can I Use For %s ?" buffername)))
                (molds (me/usable-mold)))
           (with-current-buffer buffer
             (erase-buffer)
             (org-mode)
             (me/insert-org-table
              `(("Mold" .
                 (:extractor
                  (lambda (obj) (plist-get obj :key))
                  :handler
                  (lambda (s) (me/make-elisp-navigation-link s (symbol-file 'me/register-mold)))
                  ))
                ("Demo" .
                 (:extractor
                  (lambda (obj) (plist-get obj :key))
                  :handler
                  (lambda (s) 
                    (me/make-elisp-file-link  "Start!" (format "(me/mold-demo (me/find-mold \"%s\"))" s) "elisp"))))
                ("Documentation" .
                 (:extractor
                  (lambda (obj) (or (plist-get obj :docs) "Not available."))
                  :handler
                  (lambda (s) (car (s-split "\n" s))))))
              molds)
             (setq-local molds))
           buffer))
 :docs "You can see examples and demos of the molds you can use from here."
 :examples nil)

(me/register-mold
 :key "NodeAtPointToPlayground"
 :given (lambda () (and (thing-at-point 'sexp) (equal major-mode 'emacs-lisp-mode)))
 :then (lambda ()
         (let* ((buffer (get-buffer-create "Node at point"))
                                        ;(tree (me/mold-treesitter-to-parse-tree (tree-sitter-node-at-point)))
                (node (thing-at-point 'sexp 't))
                (node-pos (list
                           :buffer (buffer-name)
                           :file (buffer-file-name)
                           :start (car (bounds-of-thing-at-point 'sexp))
                           :end (cdr (bounds-of-thing-at-point 'sexp)))))
           (with-current-buffer buffer
             (erase-buffer)
             (emacs-lisp-mode)
             (insert node)
             (pp-buffer)
             (setq-local self-pos node-pos)
             (setq-local self node)
             buffer))))
