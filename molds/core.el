(defvar me-playground-self nil "With this you can inject self in the Playground mold.")

(me-register-mold
 :key "Playground"
 :given (:fn 't)
 :then (:fn
        (let ((tree (or ;; TODO I need to revisit this: has the code tree always precedence?
                     me-playground-self
                     (ignore-errors
                       (me-mold-treesitter-to-parse-tree))
                     (ignore-errors (me-org-to-flatten-tree (current-buffer)))
                     (ignore-errors
                       (save-excursion
                         (goto-char (point-min))
                         (eval `',(read (current-buffer))))))))
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (auto-save-mode)
            (erase-buffer)
            (insert ";; Tips:\n;;    Use `self' to access the mold context.\n;;    You can access the previous mold context through `mold-data'.\n\n")
            (goto-char (point-max))
            (setq-local self tree))))
 ;; TODO experimental for auto-completion: how can I make molds easy to autocomplete?
 :actions (me-by-type identity)
 :docs "You can write any Elisp here.
Then you can evaluate with `EvalSexp'.
This mold saves structured data of the previous buffer
in the local variable `self'."
 :examples ((
             :name "Empty file"
             :given (:type file :name "/tmp/test.txt" :mode text-mode :contents "")
             :then (:type buffer :name "Playground" :mode emacs-lisp-mode :contents ";; Tips:
;;    Use `self' to access the mold context.
;;    You can access the previous mold context through `mold-data'.

"))))

(me-register-mold
 :key "Query"
 :given (:fn 't)
 :then (:fn
        (let ((self (ignore-errors
                      (save-excursion
                        (goto-char (point-min))
                        (eval `',(read (current-buffer))))))
              (sexps (call-interactively 'eval-expression)))
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (erase-buffer)
            (setq-local self sexps)
            (me-print-to-buffer sexps))))
 :docs "Evaluate Elisp sexp showing the result in a new Elisp buffer.
Useful to run Elisp on the fly without a Playground."
 :examples nil                          ; TODO need to extend me-given for this to insert text in the minibuffer
 )

(me-register-mold
 :key "WhatMoldsCanIUse?"
 :given (:fn t)
 :then (:fn
        (let* ((molds (me-usable-molds))
               (missing-deps-molds
                (--filter
                 (plist-get it :missing-dependencies)
                 (me-find-missing-dependencies-for-molds (me-usable-molds-requiring-deps)))))
          (with-current-buffer buffername
            (erase-buffer)
            (org-mode)
            (insert "* Molds you can use now.\n\n")
            (me-insert-org-table
             `(("Mold" .
                (:extractor
                 (lambda (obj) (ignore-errors (me-make-elisp-navigation-link (plist-get obj :key) (plist-get obj :origin))))))
               ("Demo" .
                (:extractor
                 (lambda (obj) (plist-get obj :key))
                 :handler
                 (lambda (s)
                   (if (plist-get (me-find-mold s) :examples)
                       (me-make-elisp-file-link  "Start!" (format "(me-mold-demo (me-find-mold \"%s\"))" s) "elisp")
                     "Not available."))))
               ("Documentation" .
                (:extractor
                 (lambda (obj) (or (plist-get obj :docs) "Not available."))
                 :handler
                 (lambda (s) (car (s-split "\n" s))))))
             molds)
            (when missing-deps-molds
              (insert "\n\n\n** Molds you could use by installing some extra dependencies.\n\n")
              (me-insert-org-table
               `(("Mold" .
                  (:extractor
                   (lambda (obj) (me-make-elisp-navigation-link (plist-get obj :key) (plist-get (me-find-mold (plist-get obj :key)) :origin)))))
                 ("Demo" .
                  (:extractor
                   (lambda (obj) (plist-get obj :key))
                   :handler
                   (lambda (s)
                     (me-make-elisp-file-link  "Start!" (format "(me-mold-demo (me-find-mold \"%s\"))" s) "elisp"))))
                 ("Documentation" .
                  (:extractor
                   (lambda (obj) (or (plist-get (me-find-mold (plist-get obj :key)) :docs) "Not available."))
                   :handler
                   (lambda (s) (car (s-split "\n" s)))))
                 ("Required Dependencies" .
                  (:extractor
                   (lambda (obj) (--> (plist-get obj :missing-dependencies)
                                      (--map
                                       (concat
                                        (when (equal (nth 0 it) 'me-require) "emacs ")
                                        (pp-to-string (nth 1 it)))
                                       it)
                                      (s-join ", " it)))
                   :handler
                   (lambda (s) (me-make-elisp-file-link s (format "//duckduckgo.com/?q=%s" s) "https")))))
               missing-deps-molds))
            (setq-local self molds))))
 :docs "You can see examples and demos of the molds you can use."
 :examples nil)

(me-register-mold
 :key "CodeAsTree"
 :given (:fn (and
              (me-require 'tree-sitter)
              (bound-and-true-p tree-sitter-mode)))
 :then (:fn
        (let* ((tree (me-mold-treesitter-to-parse-tree)))
          (with-current-buffer buffername
            (erase-buffer)
            (emacs-lisp-mode)
            (me-print-to-buffer tree)
            (setq-local self tree))))
 :docs "You get a flattened tree of all parsed elements.
You can transform this to extract information with the Playground mold."
 :examples ((
             :name "JSON to code flattened tree"
             :given
             (:type file :name "/tmp/test.json" :mode json-mode :contents "{\n  \"a\": 1,\n  \"b\": [1,2]\n}\n")
             :then
             (:type buffer :name "CodeAsTree" :mode emacs-lisp-mode :contents "((:type object :text \"{\\n  \\\"a\\\": 1,\\n  \\\"b\\\": [1,2]\\n}\" :begin 1 :end 27 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"{\" :text \"{\" :begin 1 :end 2 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type pair :text \"\\\"a\\\": 1\" :begin 5 :end 11 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type string :text \"\\\"a\\\"\" :begin 5 :end 8 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 5 :end 6 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type string_content :text \"a\" :begin 6 :end 7 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 7 :end 8 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \":\" :text \":\" :begin 8 :end 9 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type number :text \"1\" :begin 10 :end 11 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \",\" :text \",\" :begin 11 :end 12 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type pair :text \"\\\"b\\\": [1,2]\" :begin 15 :end 25 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type string :text \"\\\"b\\\"\" :begin 15 :end 18 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 15 :end 16 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type string_content :text \"b\" :begin 16 :end 17 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 17 :end 18 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \":\" :text \":\" :begin 18 :end 19 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type array :text \"[1,2]\" :begin 20 :end 25 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"[\" :text \"[\" :begin 20 :end 21 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type number :text \"1\" :begin 21 :end 22 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \",\" :text \",\" :begin 22 :end 23 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type number :text \"2\" :begin 23 :end 24 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"]\" :text \"]\" :begin 24 :end 25 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"}\" :text \"}\" :begin 26 :end 27 :buffer \"test.json\" :buffer-file \"/tmp/test.json\"))\n"))
            ))

(me-register-mold
 :key "NodeAtPointToTree"
 :given (:fn (and
              (me-require 'tree-sitter)
              (ignore-errors (tree-sitter-node-at-point))))
 :then (:fn
        (let* ((tree (me-mold-treesitter-to-parse-tree (tree-sitter-node-at-point))))
          (with-current-buffer buffername
            (erase-buffer)
            (emacs-lisp-mode)
            (me-print-to-buffer tree)
            (setq-local self tree)
            (current-buffer))))
 :docs "You can obtain the code tree for the node at point.
This is a more focused view than `CodeToTree.'"
 :examples ((
             :name "Pointer just after \"a\": "
             :given
             (:type file :name "/tmp/test.json" :mode json-mode :contents "{\n  \"a\": 1,\n  \"b\": [1,2]\n}\n" :point 9)
             :then
             (:type buffer :name "*moldable-emacs-NodeAtPointToTree*" :mode emacs-lisp-mode :contents "((:type string :text \"\\\"a\\\"\" :begin 5 :end 8 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 5 :end 6 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type string_content :text \"a\" :begin 6 :end 7 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 7 :end 8 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \":\" :text \":\" :begin 8 :end 9 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type number :text \"1\" :begin 10 :end 11 :buffer \"test.json\" :buffer-file \"/tmp/test.json\"))\n"))))

(me-register-mold
 :key "ElispListToOrgTable"
 :let ((l (list-at-point)))
 :given (:fn (ignore-errors
               (and
                (>= (length l) 2)
                (listp (car l))
                (or ;; cons list or alist
                 (consp (car l))
                 (--all? (= (length it) (length (car l))) l))
                (or
                 (-all? #'stringp (list (caar l) (cdar l)))
                 (-all? #'stringp (car l))
                 (and (stringp (caar l)) (stringp (cdar l)))
                 (--all? (equal (-filter #'symbolp (car l)) (-filter #'symbolp it)) l)))))
 :then (:fn
        (let* ((list (if (ignore-errors (length (car l)))
                         (me-alist-to-lists-of-plist l)
                       (me-alist-to-lists-of-plist (-map #'-cons-to-list l)))))
          (with-current-buffer buffername
            (org-mode)
            (erase-buffer)
            (me-insert-flat-org-table list)
            (setq-local self list))))
 :docs "You can produce an Org Table of the plist, list or alist _starting_ at point."
 :examples ((
             :name "Alist to Org table"
             :given
             (:type file :name "/tmp/my.el" :mode emacs-lisp-mode :contents "((\"Index\" \"Value\")\n (1 3)\n (2 9)\n (3  27))" :point 1)
             :then
             (:type buffer :name "Org Table for list starting for (:Index 1 :Value 3)" :mode org-mode :contents "| Index | Value |\n|-------+-------|\n|     1 |     3 |\n|     2 |     9 |\n|     3 |    27 |\n"))
            (
             :name "Cons list to Org table"
             :given
             (:type file :name "/tmp/my.el" :mode emacs-lisp-mode :contents "((\"Index\" . \"Value\")\n (1 . 3)\n (2 . 9)\n (3 . 27))" :point 1)
             :then
             (:type buffer :name "Org Table for list starting for (:Index 1 :Value 3)" :mode org-mode :contents "| Index | Value |\n|-------+-------|\n|     1 |     3 |\n|     2 |     9 |\n|     3 |    27 |\n"))
            (
             :name "Property list to Org table"
             :given
             (:type file :name "/tmp/my.el" :mode emacs-lisp-mode :contents "((:index 1 :value 3)\n (:index 2 :value 9)\n (:index 3 :value 27))" :point 1)
             :then
             (:type buffer :name "Org Table for list starting for (:index 1 :value 3)" :mode org-mode :contents "| index | value |\n|-------+-------|\n|     1 |     3 |\n|     2 |     9 |\n|     3 |    27 |\n"))))


(me-register-mold
 :key "CSVtoPlist"
 :given (:fn (and (me-require 'csv-mode) (eq major-mode 'csv-mode)))
 :then (:fn
        (save-excursion
          (goto-char (point-min))
          (let* ((separator (--> (thing-at-point 'line t)
                                 (list (list "," (length (s-split "," it)))
                                       (list ";" (length (s-split ";" it)))
                                       (list "\t" (length (s-split "\t" it))))
                                 (--max-by (> (nth 1 it) (nth 1 other)) it)
                                 car))
                 (keys (--> (thing-at-point 'line t)
                            (s-split separator it)
                            (--map (intern (concat ":" (s-replace "\"" "" (s-trim it)))) it)))
                 (plist nil)
                 (_ (while (ignore-errors (not (next-logical-line)))
                      (--> (thing-at-point 'line t)
                           (s-split separator it)
                           (-map #'s-trim it)
                           (-zip-lists keys it)
                           -flatten
                           (setq plist (cons it plist))))))
            (with-current-buffer buffername
              (emacs-lisp-mode)
              (erase-buffer)
              (me-print-to-buffer plist)
              (setq-local self plist)))))
 :docs "You can extract a plist from a CSV buffer."
 :examples ((
             :name "Simple CSV"
             :given
             (:type buffer :name "OrgTableToCSV" :mode csv-mode :contents "a,b,c\n1,2,3\n2,3,4")
             :then
             (:type buffer :name "CSVtoPlist" :mode emacs-lisp-mode :contents "((:a \"2\" :b \"3\" :c \"4\")\n (:a \"1\" :b \"2\" :c \"3\"))\n"))))

(me-register-mold
 :key "OrgTableToElispPList"
 :let ((list (me-first-org-table)))
 :given (:fn (and
              (eq major-mode 'org-mode)
              list))
 :then (:fn
        (with-current-buffer buffername
          (emacs-lisp-mode)
          (erase-buffer)
          (me-print-to-buffer list)
          (setq-local self list)))
 :docs "You can transform an Org Table to a plist."
 :examples ((
             :name "Org table to plist"
             :given
             (:type file :name "/tmp/my.org" :mode org-mode :contents "| bla   | some |\n|-------+------|\n| \"bla\" |    1 |\n| \"blo\" |    2 |\n\n")
             :then
             (:type buffer :name "Org Table for list starting for (:bla \"bla\" :some 1)" :mode emacs-lisp-mode :contents "((:bla \"\\\"bla\\\"\" :some \"1\")\n (:bla \"\\\"blo\\\"\" :some \"2\"))\n"))
            ))

(me-register-mold
 :key "XMLToTree"
 :given (:fn (or
              (eq major-mode 'html-mode)
              (eq major-mode 'nxml-mode)))
 :when (:fn (me-buffer-changed-while-the-mold-is-on-p (buffer-name)))
 :then (:fn
        (let* ((tree (libxml-parse-html-region (point-min) (point-max))))
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (erase-buffer)
            (me-print-to-buffer tree)
            (goto-char (point-min))))))

;;; CONTINUE...
;; TODO maybe add parent as well? There is not this information in the org-ql node.
(me-register-mold
 :key "OrgAsTree"
 :given (:fn (and
              (eq major-mode 'org-mode)
              (me-require 'org-ql)))
 :then (:fn
        (let* ((tree (me-org-to-flatten-tree (current-buffer))))
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (erase-buffer)
            (me-print-to-buffer tree)
            (setq-local self tree)))))

(me-register-mold
 :key "SentencesAsTree"
 :given (:fn (and (eq major-mode 'text-mode)))
 :then (:fn
        (let* ((sentences
                (s-split
                 (sentence-end)
                 (buffer-substring-no-properties (point-min) (point-max))
                 't)))
          (with-current-buffer buffername
            (erase-buffer)
            (me-print-to-buffer (mapcar 'list sentences))))) ;; TODO I need to do keep the position, or allow editing in place, no?
 :docs "Create a list of sentences for a text buffer."
 :examples ((
             :name "3 sentences"
             :given
             (:type file :name "/tmp/test.txt" :mode text-mode :contents "Some sentence. Some other sentence.\n\nSome more." :point 1)
             :then
             (:type buffer :name "*moldable-emacs-SentencesAsTree*" :mode emacs-lisp-mode :contents "((\"Some sentence\")\n (\"Some other sentence\")\n (\"Some more\"))\n"))))

(me-register-mold
 :key "ElispAsTree"
 :given (:fn (eq major-mode 'emacs-lisp-mode))
 :then (:fn
        (let ((sexps))
          (ignore-errors
            (save-excursion
              (goto-char (point-min))
              (while (setq sexp (read (current-buffer)))
                (push sexp sexps)))
            )
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (erase-buffer)
            (me-print-to-buffer (reverse sexps))
            (setq-local self sexps)))))

(me-register-mold
 :key "TreeOfDuplicates"
 :docs "Find the duplicate nodes in your Elisp buffer."
 :given (:fn (and (eq major-mode 'emacs-lisp-mode)))
 :then (:fn
        (let* ((self (ignore-errors
                       (save-excursion
                         (goto-char (point-min))
                         (eval `',(read (current-buffer))))))
               (duplicated-tree (me-nodes-with-duplication self))
               (buffer (get-buffer-create "m/tree" )))
          (with-current-buffer buffername
            (erase-buffer)
            (emacs-lisp-mode)
            (setq-local self duplicated-tree)
            (me-print-to-buffer duplicated-tree))))
 :examples ((
             :name "C file to flattened tree"
             :given (:type buffer :name "CodeAsTree" :mode emacs-lisp-mode :contents "((:type declaration :text \"include a;\" :begin 1 :end 11 :buffer \"my.cc\" :buffer-file \"/tmp/my.cc\")
 (:type type_identifier :text \"include\" :begin 1 :end 8 :buffer \"my.cc\" :buffer-file \"/tmp/my.cc\")
 (:type identifier :text \"a\" :begin 9 :end 10 :buffer \"my.cc\" :buffer-file \"/tmp/my.cc\")
 (:type \";\" :text \";\" :begin 10 :end 11 :buffer \"my.cc\" :buffer-file \"/tmp/my.cc\")
 (:type declaration :text \"include b;\" :begin 12 :end 22 :buffer \"my.cc\" :buffer-file \"/tmp/my.cc\")
 (:type type_identifier :text \"include\" :begin 12 :end 19 :buffer \"my.cc\" :buffer-file \"/tmp/my.cc\")
 (:type identifier :text \"b\" :begin 20 :end 21 :buffer \"my.cc\" :buffer-file \"/tmp/my.cc\")
 (:type \";\" :text \";\" :begin 21 :end 22 :buffer \"my.cc\" :buffer-file \"/tmp/my.cc\"))
") :then (:type buffer :name "TreeOfDuplicates" :mode emacs-lisp-mode :contents "((:type type_identifier :text \"include\" :begin 12 :end 19 :buffer \"my.cc\" :buffer-file \"/tmp/my.cc\"))
"))))

(me-register-mold
 :key "EvalSexp"
 :given (:fn (eq major-mode 'emacs-lisp-mode))
 :then (:fn
        (let* ((orig-point (point))
               (tree (progn (unless (list-at-point)
                              (progn (goto-char (point-min)) (search-forward "(" nil t)))
                            (or (ignore-errors (eval (list-at-point))) (list-at-point))))
               (_ (remove-overlays))
               (_ (overlay-put
                   (make-overlay
                    (car (thing-at-point-bounds-of-list-at-point))
                    (cdr (thing-at-point-bounds-of-list-at-point)))
                   'face
                   'bold))
               (_ (goto-char orig-point)))
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (erase-buffer)
            (me-print-to-buffer tree)
            (setq-local self tree))))
 :docs "You can evaluate an Elisp sexp after point and show the result.")

;; (me-register-mold
;;  :key "GotoNodeBuffer"
;;  :let ((l (list-at-point))
;;        (buffername (plist-get l :buffer)))
;;  :given (:fn (and
;;               (eq major-mode 'emacs-lisp-mode)
;;               (-contains-p l :buffer)))
;;  :buffername buffername
;;  :then (:fn
;;         (let* ((old-buffer (current-buffer)))
;;           (switch-to-buffer buffername)
;;           (goto-char (plist-get l :begin))
;;           (switch-to-buffer old-buffer))))

(me-register-mold
 :key "TreeToOrgTodos"
 :given (:fn (and
              (eq major-mode 'emacs-lisp-mode)))
 :then (:fn
        (let* ((tree (ignore-errors
                       (save-excursion
                         (goto-char (point-min))
                         (eval `',(read (current-buffer))))))
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
          (with-current-buffer buffername
            (erase-buffer)
            (org-mode)
            (setq-local org-confirm-elisp-link-function nil)
            (insert text))))
 :docs "Transform a flatten tree of nodes into a Org mode list."
 :examples ((
             :name "C file with duplicates"
             :given (:type buffer :name "TreeOfDuplicates" :mode emacs-lisp-mode :contents "((:type type_identifier :text \"include\" :begin 12 :end 19 :buffer \"my.cc\" :buffer-file \"/tmp/my.cc\"))
") :then (:type buffer :name "TreeToOrgTodos" :mode org-mode :contents "* Todo list [/]
- [ ] [[elisp:(progn (find-file-other-window \"/tmp/my.cc\") (goto-char 12))][include]]"))))

(me-register-mold
 :key "Stats"
 :docs "View some generic buffer stats like reading time and most frequent words.
It specializes for source code."
 :given (:fn 't)
 :then (:fn ;; TODO deliver this in org-mode buffer because later I can interpret that in a tree and run new molds on it!
        (let* ((old-buffer (buffer-name))
               (buffer (get-buffer-create "Statistics"))
               (buffersize (buffer-size))
               (self (me-mold-treesitter-to-parse-tree))
               (contents (buffer-substring-no-properties (point-min) (point-max)))
               (lines (count-lines-page))
               (words (call-interactively 'count-words))
               (book-pages (me-get-book-pages contents))
               (reading-time (me-get-reading-time contents))
               (word-analysis (--filter (> (length (car it)) 2) (me-word-stats contents)))
               (word-analysis-stats (-concat (-take 3 word-analysis) (reverse (-take 3 (reverse word-analysis)))))
               (funs (when self (length (me-by-type 'function_definition self))))
               (methods (when self (length (me-by-type 'method_declaration self))))
               (ifs (when self (length (--filter (or (eq (plist-get it :type) 'if_expression) (eq (plist-get it :type) 'if_statement)) self))))
               (classes (when self (length (--filter (or (eq (plist-get it :type) 'class_definition) (eq (plist-get it :type) 'class_declaration)) self))))
               (comments (when self (length (me-by-type 'comment self)))))
          (with-current-buffer buffername
            (erase-buffer)
            (org-mode)
            (insert "* Generic Stats\n\n")
            (insert (format "- Reading time: %s minutes \n" reading-time))
            (insert (format "- %s\n" lines))
            (insert (format "- %s\n" words))
            (insert (format "- Average book pages for this text: %s\n\n" book-pages))
            (insert (format "- Buffer size in KiloBytes: %s\n\n" buffersize))
            (insert "- Up to three most and least used words:\n\n") ;; TODO maybe add an org link that can rerun the complete analysis keeping track of the previous buffer by creating a link [[(elisp: c/analyswords old-buffer; navigate to new buffer)][click here for all the analysis]] OR I could just implement the linking of mold buffers for at least last buffer!!
            (--each word-analysis-stats
              (insert (format "  %s | %s\n" (substring (concat (number-to-string (cdr it)) (s-repeat 5 " ")) 0 3) (car it))))
            (insert "\n")
            (when funs
              (insert "* Programming Stats\n\n")
              (insert "\n")
              (insert "-- Code Stats --\n\n")
              (insert (format "#Functions: %s \n" funs))
              (insert (format "#Methods: %s \n" methods))
              (insert (format "#If-else: %s \n" ifs))
              (insert (format "#Classes: %s \n" classes))
              (insert (format "#Comments: %s \n" comments)))
            (insert "\n")
            (when self
              (insert "* Duplication Stats\n\n")
              (insert "-- Code Duplication By Token Type --\n\n")
              (let* ((nodes-with-duplication (me-nodes-with-duplication self))
                     (texts-by-type
                      (--map
                       (cons (car it) (-map (lambda (x) (plist-get x :text)) (cdr it)))
                       (--group-by (plist-get it :type) self))))
                (me-require 'tree-sitter-query)
                (me-insert-treesitter-follow-overlay
                 nodes-with-duplication
                 (lambda (node)
                   (let ((type (plist-get node :type))
                         (texts (--map
                                 (ignore-errors (plist-get it :text))
                                 (me-by-type type nodes-with-duplication))))
                     (format
                      "%s: %s/%s\n"
                      type
                      (length (--filter
                               (-contains-p texts it)
                               (--find (eq (car it) type) texts-by-type)))
                      (length (cdr (-find (lambda (x) (eq (car x) type)) texts-by-type)))))))
                )))))
 :docs "You can extract information from the original buffer without reading it."
 :examples ((
             :name "Basic stats"
             :given
             (:type file :name "/tmp/test.txt" :mode text-mode :contents "This is a little test file. Test!\n")
             :then
             (:type buffer :name "Statistics" :mode org-mode :contents "* Generic Stats\n\n- Reading time: 0 minutes \n- Page has 1 line (0 + 1)\n- Buffer has 1 line, 7 words, and 34 characters.\n- Average book pages for this text: 0\n\n- Buffer size in KiloBytes: 34\n\n- Up to three most and least used words:\n\n  1   | file\n  1   | little\n  1   | test\n  1   | test\n  1   | test!\n  1   | this\n\n\n"))))

(me-register-mold
 :key "JsonAsTree"
 :given (:fn (eq major-mode 'json-mode)) ;; TODO or region contains json
 :then (:fn
        (let ((json
               (save-excursion
                 (let ((json-object-type 'alist)
                       (json-array-type 'list))
                   (goto-char (point-min))
                   (json-read)))))
          (with-current-buffer buffername
            (erase-buffer)
            (me-print-to-buffer json)
            (emacs-lisp-mode)
            (setq-local self json)))))

(me-register-mold
 :key "JsonAsPlist"
 :given (:fn (eq major-mode 'json-mode)) ;; TODO or region contains json
 :then (:fn
        (let ((json
               (save-excursion
                 (let ((json-object-type 'plist)
                       (json-array-type 'list))
                   (goto-char (point-min))
                   (json-read)))))
          (with-current-buffer buffername
            (erase-buffer)
            (emacs-lisp-mode)
            (me-print-to-buffer json)
            (setq-local self json)))))

(me-register-mold
 :key "PlistToJson"
 :let ((plist (thing-at-point 'sexp t)))
 :given (:fn (and
              (eq major-mode 'emacs-lisp-mode)
              plist
              (or
               (ignore-errors (json-plist-p (read (thing-at-point 'sexp t))))
               (ignore-errors (json-plist-p (car (read (thing-at-point 'sexp t))))))))
 :then (:fn
        (let ((plist (read plist)))
          (with-current-buffer buffername
            (erase-buffer)
            (json-mode)
            (if (json-plist-p plist)
                (insert (json-encode-plist plist))
              (insert (json-encode-array plist)))
            (setq-local self plist)
            (json-pretty-print-buffer)))))

(me-register-mold
 :key "FirstOrgTable"
 :let ((table (me-first-org-table)))
 :given (:fn
         (eq major-mode 'org-mode)
         table)
 :then (:fn
        (with-current-buffer buffername
          (erase-buffer)
          (org-mode)
          (me-insert-flat-org-table table)
          (goto-char (point-min))
          (setq-local self table)))
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
                    :name "m/first-org-table"
                    :mode org-mode
                    :contents "| a | b |
|---+---|
| 1 | 2 |
| 2 | 3 |"))))


(me-register-mold
 :key "OrgTableToCSV"
 :given (:fn (and
              (eq major-mode 'org-mode)
              (me-require 'csv-mode)
              (s-contains-p "org" (buffer-name) t)
              (s-contains-p "table" (buffer-name) t)))
 :then (:fn
        (let ((table (org-table-to-lisp)))
          (with-current-buffer buffername
            (erase-buffer)
            (csv-mode)
            (insert (orgtbl-to-csv table nil))
            (goto-char (point-min))
            (me-replace-org-links-with-descriptions)
            (setq-local self table))))
 :docs "You can make a CSV out of an Org Table."
 :examples ((
             :name "Simple table to CSV"
             :given
             (:type buffer :name "m/first-org-table2021-07-04-18:20:55" :mode org-mode :contents "| bla   | some |\n|-------+------|\n| \"bla\" |    1 |\n| \"blo\" |    2 |\n")
             :then
             (:type buffer :name "m/csv-from-org-table2021-07-04-18:33:27" :mode csv-mode :contents "bla,some\n\"\"\"bla\"\"\",1\n\"\"\"blo\"\"\",2"))
            ))

(me-register-mold-by-key "ListToCSV"
                         (me-mold-compose
                          "ElispListToOrgTable"
                          "OrgTableToCSV"
                          '((:docs "You can produce a CSV from a Plist.")
                            (:examples ((
                                         :name "Simple plist to csv."
                                         :given
                                         (:type file :name "/tmp/test.el" :mode emacs-lisp-mode :contents "((:a 1 :b 2)\n (:a 2 :b 3)\n (:a 4 :b 9))" :point 1)
                                         :then
                                         (:type buffer :name "*moldable-emacs-OrgTableToCSV*" :mode csv-mode :contents "a,b\n1,2\n2,3\n4,9")))))))

(me-register-mold
 :key "CSVToOrgTable"
 :given (:fn (eq major-mode 'csv-mode))
 :then (:fn
        (let ((table (buffer-string)))
          (with-current-buffer buffername
            (erase-buffer)
            (org-mode)
            (insert table)
            (mark-whole-buffer)
            (call-interactively #'org-table-create-or-convert-from-region)
            (goto-char (point-min))
            (setq-local self (me-org-tabletolisp-to-plist (org-table-to-lisp))))))
 :docs "Transform a CSV buffer in an Org table.")

(me-register-mold
 :key "Annotate"
 :given (:fn 't)
 :then (:fn
        (let* ((buffer (buffer-name))
               (boundaries (or (region-bounds) (bounds-of-thing-at-point 'sexp) `((,(beginning-of-line) . ,(end-of-line)))))
               (default-note
                 `(
                   :given (:node
                           (
                            :key ,(format-time-string "%Y-%m-%d+%H:%M:%S")
                            :type position
                            :text ,(buffer-substring-no-properties (caar boundaries) (cdar boundaries))
                            :begin ,(caar boundaries)
                            :end ,(cdar boundaries)
                            :buffer ,buffer
                            :buffer-file ,(ignore-errors (s-replace (getenv "HOME") "~" (buffer-file-name)))
                            :mode ,major-mode
                            :git-hash ,(when-let* ((hash? (shell-command-to-string "git rev-parse --short HEAD"))
                                                   (hash (when (< (length hash?) 15) hash?))) ;; TODO this is a hack because more chars may mean an error
                                         (s-trim hash))))
                   :when nil
                   :then nil))
               (note (me-ask-for-details-according-to-context default-note)))
          (with-current-buffer buffername
            (erase-buffer)
            (emacs-lisp-mode)
            (insert ";; Remember to save the note with C-x C-s!\n\n\n")
            (me-print-to-buffer note)
            (me-override-keybiding-in-buffer
             (kbd "C-x C-s")
             '(lambda ()
                (interactive)
                (let ((note
                       (ignore-errors
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward "(")
                           (eval `',(list-at-point))))))
                  (setq-local self note)
                  (me-store-note note)
                  (message "Note stored!"))))
            (setq-local self note))))
 :docs "Take a note with moldable-emacs.")

(me-register-mold
 :key "Add Todo"
 :given (:fn 't)
 :then (:fn
        (let* ((buffer (buffer-name))
               (boundaries (or (region-bounds) (bounds-of-thing-at-point 'sexp) `((,(beginning-of-line) . ,(end-of-line)))))
               (default-note
                 `(
                   :given (:node
                           (
                            :key ,(format-time-string "%Y-%m-%d+%H:%M:%S")
                            :type position
                            :text ,(buffer-substring-no-properties (caar boundaries) (cdar boundaries))
                            :begin ,(caar boundaries)
                            :end ,(cdar boundaries)
                            :buffer ,buffer
                            :buffer-file ,(ignore-errors (s-replace (getenv "HOME") "~" (buffer-file-name)))
                            :mode ,major-mode
                            :git-hash ,(when-let* ((hash? (shell-command-to-string "git rev-parse --short HEAD"))
                                                   (hash (when (< (length hash?) 15) hash?))) ;; TODO this is a hack because more chars may mean an error
                                         (s-trim hash))))
                   :when nil
                   :then nil))
               (note (me-ask-for-todo-details-according-to-context default-note)))
          (with-current-buffer buffername
            (erase-buffer)
            (emacs-lisp-mode)
            (insert ";; Remember to save the note with C-x C-s!\n\n\n")
            (me-print-to-buffer note)
            (me-override-keybiding-in-buffer
             (kbd "C-x C-s")
             '(lambda ()
                (interactive)
                (let ((note
                       (ignore-errors
                         (save-excursion
                           (goto-char (point-min))
                           (search-forward "(")
                           (eval `',(list-at-point))))))
                  (setq-local self note)
                  (me-store-note note)
                  (message "Note stored!"))))
            (setq-local self note))))
 :docs "Take a note with moldable-emacs.")

(me-register-mold
 :key "ShowNotesByBuffer"
 :let ((notes (me-filter-notes-by-buffer (buffer-name))))
 :given (:fn (and notes))
 :then (:fn
        (with-current-buffer buffername
          (erase-buffer)
          (emacs-lisp-mode)
          (me-print-to-buffer notes)
          (setq-local self notes))))

(me-register-mold
 :key "ShowNotesByProject"
 :let ((notes (me-filter-notes-by-project)))
 :given (:fn (and (me-require 'projectile) (ignore-errors (vc-root-dir)) notes))
 :then (:fn
        (with-current-buffer buffername
          (erase-buffer)
          (emacs-lisp-mode)
          (me-print-to-buffer notes)
          (setq-local self notes))))


(me-register-mold
 :key "ShowNotesByMode"
 :let ((notes (me-filter-notes-by-mode major-mode)))
 :given (:fn notes)
 :then (:fn
        (with-current-buffer buffername
          (erase-buffer)
          (emacs-lisp-mode)
          (me-print-to-buffer notes)
          (setq-local self notes))))

(me-register-mold
 :key "NotesToOrg"
 :let ((notes (or (ignore-errors self) (me-load-notes))))
 :given (:fn
         notes)
 :then (:fn
        (with-current-buffer buffername
          (erase-buffer)
          (org-mode)
          (setq-local org-confirm-elisp-link-function nil)
          (insert (--reduce-from
                   (concat acc (me-note-to-org-heading it) "\n")
                   ""
                   notes))
          (setq-local self notes)
          (me-override-keybiding-in-buffer
           (kbd "C-x C-s")
           '(lambda ()
              (interactive)
              (--each
                  (me-org-to-flatten-tree (current-buffer))
                (let* ((old-note (-find (lambda (el) (equal (plist-get el :key) (plist-get it :id))) self))
                       (text (plist-get it :text))
                       (new-note (plist-put old-note :then (list :string text))))
                  (me-store-note new-note)))
              (message "Notes stored!"))))))

(me-register-mold
 :key "NoteToOrg"
 :given (:fn
         (and
          (s-contains-p "Annotate" (buffer-name))))
 :then (:fn
        (let ((note self))
          (with-current-buffer buffername
            (erase-buffer)
            (org-mode)
            (setq-local org-confirm-elisp-link-function nil)
            (insert "# Remember to save the note with C-x C-s!\n\n\n")
            (insert (me-note-to-org-heading note))
            (setq-local self note)
            (me-override-keybiding-in-buffer
             (kbd "C-x C-s")
             '(lambda ()
                (interactive)
                (let* ((org-node (nth 0 (me-org-to-flatten-tree (current-buffer))))
                       (text (plist-get org-node :text))
                       (new-note (plist-put self :then (list :string text))))
                  (me-store-note new-note))
                (message "Notes stored!")))))))

(me-register-mold
 :key "ShowAllNotes"
 :let ((notes (me-load-notes)))
 :given (:fn notes)
 :then (:fn
        (with-current-buffer buffername
          (erase-buffer)
          (emacs-lisp-mode)
          (me-print-to-buffer notes)
          (setq-local self notes)))
 :docs "Show all notes stored as an Elisp list.")

(me-register-mold-by-key "AnnotateWithOrg"
                         (me-mold-compose "Annotate" "NoteToOrg"
                                          '((:docs "Take a note and edit it in the Org format."))))

(me-register-mold-by-key "ShowNotesByProjectInOrg"
                         (me-mold-compose "ShowNotesByProject" "NotesToOrg"
                                          '((:docs "Show only notes relevant to the current project in the Org format."))))

(me-register-mold-by-key "ShowNotesByBufferInOrg"
                         (me-mold-compose "ShowNotesByBuffer" "NotesToOrg" '((:docs "Show only notes relevant to the current buffer in the Org format."))))

(me-register-mold-by-key "ShowNotesByModeInOrg"
                         (me-mold-compose "ShowNotesByMode" "NotesToOrg" '((:docs "Show only notes relevant to the mode in the Org format."))))

(me-register-mold-by-key "ShowAllNotesInOrg"
                         (me-mold-compose "ShowAllNotes" "NotesToOrg"
                                          '((:docs "Show all the notes in the Org format."))))

(me-register-mold
 :key "NodeAtPointToPlayground"
 :let ((node (list-at-point)))
 :given (:fn (and
              (equal major-mode 'emacs-lisp-mode)
              node))
 :then (:fn
        (let* ((node-pos (list
                          :buffer (buffer-name)
                          :file (buffer-file-name)
                          :start (car (bounds-of-thing-at-point 'sexp))
                          :end (cdr (bounds-of-thing-at-point 'sexp)))))
          (with-current-buffer buffername
            (erase-buffer)
            (emacs-lisp-mode)
            (me-print-to-buffer node)
            (setq-local self-pos node-pos)
            (setq-local self node))))
 :docs "You can move the node under point to a Playground mold."
 :examples ((
             :name "Simple list"
             :given
             (:type file :name "/tmp/test.el" :mode emacs-lisp-mode :contents "(list 1 2 3)")
             :then
             (:type buffer :name "Node at point" :mode emacs-lisp-mode :contents "(list 1 2 3)\n"))))

(me-register-mold
 :key "Evaluate Arithmetic Expression"
 :let ((expression (me-arithmetic-at-point))) ;; TODO this is naive: does not support neither square root! Actually it does: 4^1/2. Still it breaks for things like ". 1 + 2" because the expression starts with a dot...
 :given (:fn expression)
 :then (:fn
        (let* ((result (calc-eval expression))
               (tree (list :given expression :then result))
               (colored-result (me-color-string result "green")))
          (with-current-buffer buffername
            (erase-buffer)
            (insert (format "%s = %s" expression colored-result))
            (setq-local self tree))))
 :docs "You can produce the result of arithmetic expressions."
 :examples ((
             :name "Simple arithmetic expression"
             :given
             (:type file :name "/tmp/my.txt" :mode text-mode :contents "bla bla 1 + 1 / 2 bla bla\n" :point 9)
             :then
             (:type buffer :name "Evaluate 1 + 1 / 2" :mode fundamental-mode :contents "1 + 1 / 2 = 1.5"))))


(me-register-mold
 :key "Mold History"
 :given (:fn (and me-mold-history me-current-history-index))
 :then (:fn
        (let* ((history me-mold-history)
               (buffer (get-buffer-create "mold-history")))
          (with-current-buffer buffername
            (read-only-mode -1)
            (org-mode)
            (erase-buffer)
            (me-insert-org-table
             `(("History Item" .
                (
                 :extractor
                 (lambda (obj) (plist-get obj :buffername))
                 :handler
                 (lambda (obj)
                   (me-make-elisp-file-link
                    obj
                    (format "(switch-to-buffer \"%s\")" obj)
                    "elisp"))
                 ))
               ("Time" .
                (
                 :extractor
                 (lambda (obj) (plist-get obj :date)))))
             me-mold-history)
            (setq-local self history))))
 :docs "You can see the current history of the molds you used."
 :examples nil)


(me-register-mold
 :key "Inspect molds running time"
 :given (:fn (and me-molds-debug-on me-usable-mold-stats))
 :then (:fn
        (let* ((stats (--sort (> (plist-get it :time) (plist-get other :time)) me-usable-mold-stats)))
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (erase-buffer)
            (me-print-to-buffer stats)
            (setq-local self stats))))
 :docs "You can see how long did the mold take to evaluate the given clause."
 :examples nil)


(me-register-mold
 :key "Show Tutorials"
 :given (:fn (not me-i-know-what-i-am-doing))
 :then (:fn
        (let ((tutorials (--> (symbol-file 'me-mold)
                              (file-name-directory it)
                              (concat it "/tutorials")
                              (directory-files it t)
                              (--filter (equal (file-name-extension it) "org") it)))
              (blogs (me-pmap
                      `(lambda (url)
                         (with-current-buffer (url-retrieve-synchronously (concat "https://raw.githubusercontent.com/ag91/ag91.github.io/source/blog/" url))
                           (goto-char url-http-end-of-headers)
                           (delete-region (point-min) (point))
                           (let ((title (progn (goto-char (point-min))
                                               (next-line) ; for some reason I am a line too early
                                               (string-trim (nth 1 (split-string (thing-at-point 'line t) "TITLE:"))))))
                             (delete-region (point-min) (- (search-forward "\n* " nil t) 2))
                             (list :title title :contents (buffer-substring-no-properties (point-min) (point-max))))))
                      (list
                       "MoldableEmacsVision.org"
                       "MoldableDired.org"
                       ;; TODO add others
                       )))
              )
          (with-current-buffer buffername
            (org-mode)
            (erase-buffer)
            (insert "#+TITLE: Moldable Emacs Tutorials\n\n")
            (--each tutorials
              (insert-file-contents-literally it)
              (goto-char (point-max)))
            (when blogs (insert "\n\n* Blogs\n\n"))
            (--each blogs
              (insert (concat "** " (plist-get it :title) "\n\n"))
              (insert (s-replace "* " "*** " (plist-get it :contents)))
              (insert "\n\n")
              (goto-char (point-max)))
            (goto-char (point-min))
            (call-interactively #'outline-hide-sublevels)
            (setq-local self tutorials))))
 :docs "You can consult `moldable-emacs' tutorials."
 :examples nil)

(me-register-mold
 :key "To Date"
 :let ((number (thing-at-point 'number t)))
 :given (:fn (and number))
 :then (:fn
        (let* ((tree number))
          (with-current-buffer buffername
            (org-mode)
            (erase-buffer)
            (insert (format "- Seconds: %s is %s\n" number (me-format-iso8601-time (seconds-to-time number))))
            (insert (format "- MilliSeconds: %s is %s\n" (/ number 1000) (me-format-iso8601-time (seconds-to-time (/ number 1000)))))
            (insert "\n If you want to play around:\n")
            (insert "
#+begin_src elisp :var date=\"2016-12-01T23:35:06-05:00\"
(time-to-seconds (parse-iso8601-time-string date)) ; multiply by 1000 if you need millis
#+end_src

#+begin_src elisp :var time=1480653306
(me-format-iso8601-time time)
#+end_src
")
            (setq-local self tree))))
 :docs "You can convert an epoch number to date."
 :examples nil)
