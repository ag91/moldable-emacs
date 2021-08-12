(me/register-mold
 :key "Playground"
 :given (lambda () 't)
 :then (lambda ()
         (let ((tree (or ;; TODO I need to revisit this: has the code tree always precedence?
                      (ignore-errors
                        (me/mold-treesitter-to-parse-tree))
                      (ignore-errors (me/org-to-flatten-tree (current-buffer)))
                      (ignore-errors
                        (save-excursion
                          (goto-char (point-min))
                          (eval `',(read (current-buffer)))))))
               (buffer (get-buffer-create "m/tree-playground")))
           (with-current-buffer buffer
             (emacs-lisp-mode)
             (erase-buffer)
             (insert ";; Tips:\n;;    Use `self' to access the mold context.\n;;    You can access the previous mold context through `mold-data'.\n\n")
             (goto-char (point-max))
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
             :then (:type buffer :name "m/tree-playground-from" :mode emacs-lisp-mode :contents ""))))

(me/register-mold
 :key "Query"
 :given (lambda () 't)
 :then (lambda ()
         (let ((self (ignore-errors
                       (save-excursion
                         (goto-char (point-min))
                         (eval `',(read (current-buffer))))))
               (sexps (call-interactively 'eval-expression))
               (buffer (get-buffer-create "m/tree")))
           (with-current-buffer buffer
             (erase-buffer)
             (setq-local self sexps)
             (pp-display-expression sexps buffer)
             (emacs-lisp-mode)
             buffer)
           buffer)))

(me/register-mold
 :key "WhatMoldsCanIUse?"
 :given (lambda () t)
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (buffer (get-buffer-create (format "What Molds Can I Use For %s ?" buffername)))
                (molds (me/usable-mold))
                (missing-deps-molds
                 (--filter
                  (plist-get it :missing-dependencies)
                  (me/find-missing-dependencies-for-molds (me/usable-molds-requiring-deps)))))
           (with-current-buffer buffer
             (erase-buffer)
             (org-mode)
             (insert "* Molds you can use now.\n\n")
             (me/insert-org-table
              `(("Mold" .
                 (:extractor
                  (lambda (obj) (me/make-elisp-navigation-link (plist-get obj :key) (plist-get obj :origin)))))
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
             (when missing-deps-molds
               (insert "\n\n\n** Molds you could use by installing some extra dependencies.\n\n")
               (me/insert-org-table
                `(("Mold" .
                   (:extractor
                    (lambda (obj) (me/make-elisp-navigation-link (plist-get obj :key) (plist-get (me/find-mold (plist-get obj :key)) :origin)))))
                  ("Demo" .
                   (:extractor
                    (lambda (obj) (plist-get obj :key))
                    :handler
                    (lambda (s)
                      (me/make-elisp-file-link  "Start!" (format "(me/mold-demo (me/find-mold \"%s\"))" s) "elisp"))))
                  ("Documentation" .
                   (:extractor
                    (lambda (obj) (or (plist-get (me/find-mold (plist-get obj :key)) :docs) "Not available."))
                    :handler
                    (lambda (s) (car (s-split "\n" s)))))
                  ("Required Dependencies" .
                   (:extractor
                    (lambda (obj) (--> (plist-get obj :missing-dependencies)
                                    (--map
                                     (concat
                                      (when (equal (nth 0 it) 'me/require) "emacs ")
                                      (pp-to-string (nth 1 it)))
                                     it)
                                    (s-join ", " it)))
                    :handler
                    (lambda (s) (me/make-elisp-file-link s (format "//duckduckgo.com/?q=%s" s) "https")))))
                missing-deps-molds))
             (setq-local self molds))
           buffer))
 :docs "You can see examples and demos of the molds you can use."
 :examples nil)

(me/register-mold
 :key "CodeAsTree"
 :given (lambda () (and
                    (me/require 'tree-sitter)
                    (seq-contains-p minor-mode-list 'tree-sitter-mode)))
 :then (lambda ()
         (let* ((buffer (get-buffer-create "m/tree"))
                (tree (me/mold-treesitter-to-parse-tree)))
           (with-current-buffer buffer
             (erase-buffer)
             (prin1 tree buffer) ;; TODO I need to do keep the position, or allow editing in place, no?
             (pp-buffer)
             (emacs-lisp-mode)
             buffer)))
 :docs "You get a flattened tree of all parsed elements. You can transform this to extract information with the Playground mold."
 :examples ((
             :name "JSON to code flattened tree"
             :given
             (:type file :name "/tmp/test.json" :mode json-mode :contents "{\n  \"a\": 1,\n  \"b\": [1,2]\n}\n")
             :then
             (:type buffer :name "m/tree" :mode emacs-lisp-mode :contents "((:type object :text \"{\n  \\\"a\\\": 1,\n  \\\"b\\\": [1,2]\n}\" :begin 1 :end 27 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"{\" :text \"{\" :begin 1 :end 2 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type pair :text \"\\\"a\\\": 1\" :begin 5 :end 11 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type string :text \"\\\"a\\\"\" :begin 5 :end 8 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 5 :end 6 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type string_content :text \"a\" :begin 6 :end 7 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 7 :end 8 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \":\" :text \":\" :begin 8 :end 9 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type number :text \"1\" :begin 10 :end 11 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \",\" :text \",\" :begin 11 :end 12 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type pair :text \"\\\"b\\\": [1,2]\" :begin 15 :end 25 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type string :text \"\\\"b\\\"\" :begin 15 :end 18 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 15 :end 16 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type string_content :text \"b\" :begin 16 :end 17 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 17 :end 18 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \":\" :text \":\" :begin 18 :end 19 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type array :text \"[1,2]\" :begin 20 :end 25 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"[\" :text \"[\" :begin 20 :end 21 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type number :text \"1\" :begin 21 :end 22 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \",\" :text \",\" :begin 22 :end 23 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type number :text \"2\" :begin 23 :end 24 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"]\" :text \"]\" :begin 24 :end 25 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"}\" :text \"}\" :begin 26 :end 27 :buffer \"test.json\" :buffer-file \"/tmp/test.json\"))\n"))
            ))

(me/register-mold
 :key "NodeAtPointToTree"
 :given (lambda () (and
                    (me/require 'tree-sitter)
                    (ignore-errors (tree-sitter-node-at-point))))
 :then (lambda ()
         (let* ((buffer (get-buffer-create "m/tree"))
                (tree (me/mold-treesitter-to-parse-tree (tree-sitter-node-at-point))))
           (with-current-buffer buffer
             (erase-buffer)
             (emacs-lisp-mode)
             (prin1 tree buffer)
             (pp-buffer)
             (setq self tree)
             (current-buffer))))
 :docs "You can obtain the code flattened tree only for the node at point. This is a more focused view than `CodeToTree.'"
 :examples ((
             :name "Pointer just after \"a\": "
             :given
             (:type file :name "/tmp/test.json" :mode json-mode :contents "{\n  \"a\": 1,\n  \"b\": [1,2]\n}\n")
             :then
             (:type buffer :name "m/tree" :mode emacs-lisp-mode :contents "((:type string :text \"\\\"a\\\"\" :begin 5 :end 8 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 5 :end 6 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type string_content :text \"a\" :begin 6 :end 7 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \"\\\"\" :text \"\\\"\" :begin 7 :end 8 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type \":\" :text \":\" :begin 8 :end 9 :buffer \"test.json\" :buffer-file \"/tmp/test.json\")\n (:type number :text \"1\" :begin 10 :end 11 :buffer \"test.json\" :buffer-file \"/tmp/test.json\"))\n"))
            ))

;; TODO maybe mold treeWithJsonToPlist? (--map (json-parse-string (plist-get it :text) :object-type 'plist) self)
;; TODO and maybe plist to Org Table? See "GebE2ECumulativeErrors" for that


(me/register-mold
 :key "ElispListToOrgTable"
 :given (lambda () (let ((l (list-at-point)))
                     (ignore-errors
                       (and
                        (> (length l) 2)
                        (listp (car l))
                        (or ;; cons list or alist
                         (consp (car l))
                         (--all? (= (length it) (length (car l))) l))
                        (or
                         (-all? #'stringp (list (caar l) (cdar l)))
                         (-all? #'stringp (car l))
                         (and (stringp (caar l)) (stringp (cdar l)))
                         (--all? (equal (-filter #'symbolp (car l)) (-filter #'symbolp it)) l))))))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (l (list-at-point))
                (list (if (ignore-errors (length (car l)))
                          (me/alist-to-plist l)
                        (me/alist-to-plist (-map #'-cons-to-list l))))
                (buffer (get-buffer-create (format "Org Table for list starting for %s" (car list)))))
           (with-current-buffer buffer
             (org-mode)
             (erase-buffer)
             (me/insert-flat-org-table list)
             (setq-local self list))
           buffer))
 :docs "You can produce an Org Table of the plist, list or alist _starting_ at point."
 :examples ((
             :name "Alist to Org table"
             :given
             (:type file :name "/tmp/my.el" :mode emacs-lisp-mode :contents "((\"Index\" \"Value\")\n (1 3)\n (2 9)\n (3  27))")
             :then
             (:type buffer :name "Org Table for list starting for (:Index 1 :Value 3)" :mode org-mode :contents "| Index | Value |\n|-------+-------|\n|     1 |     3 |\n|     2 |     9 |\n|     3 |    27 |\n|       |       |\n"))
            (
             :name "Cons list to Org table"
             :given
             (:type file :name "/tmp/my.el" :mode emacs-lisp-mode :contents "((\"Index\" . \"Value\")\n (1 . 3)\n (2 . 9)\n (3 . 27))")
             :then
             (:type buffer :name "Org Table for list starting for (:Index 1 :Value 3)" :mode org-mode :contents "| Index | Value |\n|-------+-------|\n|     1 |     3 |\n|     2 |     9 |\n|     3 |    27 |\n|       |       |\n"))
            (
             :name "Property list to Org table"
             :given
             (:type file :name "/tmp/my.el" :mode emacs-lisp-mode :contents "((:index 1 :value 3)\n (:index 2 :value 9)\n (:index 3 :value 27))")
             :then
             (:type buffer :name "Org Table for list starting for (:index 1 :value 3)" :mode org-mode :contents "| index | value |\n|-------+-------|\n|     1 |     3 |\n|     2 |     9 |\n|     3 |    27 |\n|       |       |\n"))

            ))

(me/register-mold
 :key "OrgTableToElispPList"
 :given (lambda () (and
                    (eq major-mode 'org-mode)
                    (me/first-org-table)))
 :then (lambda ()
         (let* ((list (me/first-org-table))
                (buffer (get-buffer-create (format "Org Table for list starting for %s" (car list)))))
           (with-current-buffer buffer
             (emacs-lisp-mode)
             (erase-buffer)
             (insert (pp list))
             (setq-local self list))
           buffer))
 :docs "You can transform an Org Table to a plist."
 :examples ((
             :name "Org table to plist"
             :given
             (:type file :name "/tmp/my.org" :mode org-mode :contents "| bla   | some |\n|-------+------|\n| \"bla\" |    1 |\n| \"blo\" |    2 |\n\n")
             :then
             (:type buffer :name "Org Table for list starting for (:bla \"bla\" :some 1)" :mode emacs-lisp-mode :contents "((:bla \"\\\"bla\\\"\" :some \"1\")\n (:bla \"\\\"blo\\\"\" :some \"2\"))\n"))
            ))

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
 :given (lambda () (and
                    (eq major-mode 'org-mode)
                    (me/require 'org-ql)))
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
 :key "SentencesAsTree"
 :given (lambda () (and (eq major-mode 'text-mode)))
 :then (lambda ()
         (let* ((buffer (get-buffer-create "SentencesTree"))
                (sentences
                 (s-split
                  (sentence-end)
                  (buffer-substring-no-properties (point-min) (point-max))
                  't)))
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
                (buffer (get-buffer-create "m/tree" )))
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
         (let* ((_ (remove-overlays))
               (_ (overlay-put
                   (make-overlay
                    (car (thing-at-point-bounds-of-list-at-point))
                    (cdr (thing-at-point-bounds-of-list-at-point)))
                   'face
                   'bold))
               (tree (list-at-point))
               (result (ignore-errors (eval (list-at-point))))
               (buffer (get-buffer-create (s-truncate 30 (format "m/tree-eval of %s" tree)))))
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
                    :name "m/tree-eval-from"
                    :mode emacs-lisp-mode
                    :contents "3"))))

(me/register-mold
 :key "GotoNodeBuffer"
 :given (lambda () (and
                    (s-starts-with-p "m/tree" (buffer-name))
                    (eq major-mode 'emacs-lisp-mode)
                    (-contains-p (list-at-point) :buffer)))
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
 :given (lambda () (and
                    (s-starts-with-p "m/tree" (buffer-name))
                    (eq major-mode 'emacs-lisp-mode)))
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
                (buffersize (buffer-size))
                (self (me/mold-treesitter-to-parse-tree))
                (contents (buffer-substring-no-properties (point-min) (point-max)))
                (lines (count-lines-page))
                (words (call-interactively 'count-words))
                (book-pages (me/get-book-pages contents))
                (reading-time (me/get-reading-time contents))
                (word-analysis (--filter (> (length (car it)) 2) (me/word-stats contents)))
                (word-analysis-stats (-concat (-take 3 word-analysis) (reverse (-take 3 (reverse word-analysis)))))
                (funs (when self (length (me/by-type 'function_definition self))))
                (methods (when self (length (me/by-type 'method_declaration self))))
                (ifs (when self (length (--filter (or (eq (plist-get it :type) 'if_expression) (eq (plist-get it :type) 'if_statement)) self))))
                (classes (when self (length (--filter (or (eq (plist-get it :type) 'class_definition) (eq (plist-get it :type) 'class_declaration)) self))))
                (comments (when self (length (me/by-type 'comment self)))))
           (with-current-buffer buffer
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
               (let* ((nodes-with-duplication (nodes-with-duplication self))
                      (texts-by-type
                       (--map
                        (cons (car it) (-map (lambda (x) (plist-get x :text)) (cdr it)))
                        (--group-by (plist-get it :type) self))))
                 (me/require 'tree-sitter-query)
                 (me/insert-treesitter-follow-overlay
                  nodes-with-duplication
                  (lambda (node)
                    (let ((type (plist-get node :type))
                          (texts (--map
                                  (ignore-errors (plist-get it :text))
                                  (me/by-type type nodes-with-duplication))))
                      (format
                       "%s: %s/%s\n"
                       type
                       (length (--filter
                                (-contains-p texts it)
                                (--find (eq (car it) type) texts-by-type)))
                       (length (cdr (-find (lambda (x) (eq (car x) type)) texts-by-type)))))))
                 )))
           buffer))
 :docs "You can extract information from the original buffer without reading it."
 :examples ((
             :name "Basic stats"
             :given
             (:type file :name "/tmp/test.txt" :mode text-mode :contents "This is a little test file. Test!\n")
             :then
             (:type buffer :name "Statistics" :mode org-mode :contents "* Generic Stats\n\n- Reading time: 0 minutes \n- Page has 1 line (0 + 1)\n- Buffer has 1 line, 7 words, and 34 characters.\n- Average book pages for this text: 0\n\n- Buffer size in KiloBytes: 158\n\n- Up to three most and least used words:\n\n  1   | file\n  1   | little\n  1   | test\n  1   | test\n  1   | test!\n  1   | this\n\n\n"))
            ))

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
 :given (lambda () (and
                    (me/require 'tree-sitter)
                    (seq-contains-p minor-mode-list 'tree-sitter-mode)))
 :then (lambda ()
         (tree-sitter-debug-mode)
         tree-sitter-debug--tree-buffer))

(me/register-mold
 :key "FirstOrgTable"
 :given (lambda ()
          (eq major-mode 'org-mode)
          (me/first-org-table))
 :then (lambda ()
         (let ((table (me/first-org-table))
               (buffer (get-buffer-create "m/first-org-table")))
           (with-current-buffer buffer
             (erase-buffer)
             (org-mode)
             (me/insert-flat-org-table table)
             (goto-char (point-min))
             (setq-local self table)
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
                    :name "m/first-org-table"
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
               (buffer (get-buffer-create "m/csv-from-org-table")))
           (with-current-buffer buffer
             (erase-buffer)
             (csv-mode)
             (insert (orgtbl-to-csv table nil))
             (goto-char (point-min))
             (while (eq (org-next-link) 't)
               (ag/org-replace-link-by-link-description))
             (setq-local self table)
             buffer)))
 :docs "You can make a CSV out of an Org Table."
 :examples ((
             :name "Simple table to CSV"
             :given
             (:type buffer :name "m/first-org-table2021-07-04-18:20:55" :mode org-mode :contents "| bla   | some |\n|-------+------|\n| \"bla\" |    1 |\n| \"blo\" |    2 |\n")
             :then
             (:type buffer :name "m/csv-from-org-table2021-07-04-18:33:27" :mode csv-mode :contents "bla,some\n\"\"\"bla\"\"\",1\n\"\"\"blo\"\"\",2"))
            ))

(me/register-mold
 :key "CSVToOrgTable"
 :given (lambda () (eq major-mode 'csv-mode))
 :then (lambda ()
         (let ((buffer (get-buffer-create "m/org-table-from-csv"))
               (table (buffer-string)))
           (with-current-buffer buffer
             (erase-buffer)
             (org-mode)
             (insert table)
             (mark-whole-buffer)
             (call-interactively #'org-table-create-or-convert-from-region)
             (goto-char (point-min))
             (setq-local self (org-table-to-lisp))
             buffer))))

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
                             :buffer-file ,(s-replace (getenv "HOME") "~" (buffer-file-name))
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


(me/register-mold
 :key "ShowNotesByProject"
 :given (lambda () (and (me/require 'projectile) (me/filter-notes-by-project)))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (buffer (get-buffer-create (format "Notes for %s" (projectile-project-root))))
                (notes (me/filter-notes-by-project)))
           (with-current-buffer buffer
             (erase-buffer)
             (emacs-lisp-mode)
             (prin1 notes buffer)
             (setq self notes)
             (pp-buffer))
           buffer)))


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

;; (me/register-mold-by-key "AnnotateWithOrg"
;;                          (me/mold-compose "Annotate" "NoteToOrg"))

(me/register-mold-by-key "ShowNotesByProjectInOrg"
                         (me/mold-compose "ShowNotesByProject" "NotesToOrg"))

(me/register-mold-by-key "ShowNotesByBufferInOrg"
                         (me/mold-compose "ShowNotesByBuffer" "NotesToOrg"))

(me/register-mold-by-key "ShowNotesByModeInOrg"
                          (me/mold-compose "ShowNotesByMode" "NotesToOrg"))

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
 :key "NodeAtPointToPlayground"
 :given (lambda () (and
                    (thing-at-point 'sexp)
                    (equal major-mode 'emacs-lisp-mode)))
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
             buffer)))
 :docs "You can move the node under point to a Playground mold."
 :examples ((
             :name "Simple list"
             :given
             (:type file :name "/tmp/test.el" :mode emacs-lisp-mode :contents "(list 1 2 3)")
             :then
             (:type buffer :name "Node at point" :mode emacs-lisp-mode :contents "(list 1 2 3)\n"))
            ))

(me/register-mold
 :key "Evaluate Arithmetic Expression"
 :given (lambda () (me/arithmetic-at-point)) ;; TODO this is naive: does not support neither square root! Actually it does: 4^1/2. Still it breaks for things like ". 1 + 2" because the expression starts with a dot...
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (expression (me/arithmetic-at-point))
                (result (calc-eval expression))
                (tree (list :given expression :then result))
                (buffer (get-buffer-create (format "Evaluate %s" expression)))
                (colored-result (me/color-string result "green")))
           (with-current-buffer buffer
             (erase-buffer)
             (insert (format "%s = %s" expression colored-result))
             (setq-local self tree))
           buffer))
 :docs "You can produce the result of arithmetic expressions."
 :examples ((
             :name "Simple arithmetic expression"
             :given
             (:type file :name "/tmp/my.txt" :mode text-mode :contents "bla bla 1 + 1 / 2 bla bla\n")
             :then
             (:type buffer :name "Evaluate 1 + 1 / 2" :mode fundamental-mode :contents "1 + 1 / 2 = 1.5"))))


(me/register-mold
 :key "Mold History"
 :given (lambda () (and me/mold-history me/current-history-index))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (history me/mold-history)
                (buffer (get-buffer-create "mold-history")))
           (with-current-buffer buffer
             (read-only-mode -1)
             (org-mode)
             (erase-buffer)
             (me/insert-org-table
              `(("History Item" .
                 (
                  :extractor
                  (lambda (obj) (plist-get obj :buffername))
                  :handler
                  (lambda (obj)
                    (me/make-elisp-file-link
                     obj
                     (format "(switch-to-buffer \"%s\")" obj)
                     "elisp"))
                  ))
                ("Time" .
                 (
                  :extractor
                  (lambda (obj) (plist-get obj :date)))))
              me/mold-history)
             (setq-local self history))
           buffer))
 :docs "You can see the current history of the molds you used."
 :examples nil)
