(defun me-highlight-unit-test-time (time-as-string)
  "Highlight TIME-AS-STRING according to unit tests."
  (and (ignore-errors (string-to-number time-as-string))
       (let* ((time (string-to-number time-as-string))
              (color (cond
                      ((>= time (/ 10.0 1000)) "red")
                      ((>= time (/ 2.0 1000)) "orange")
                      ('otherwise "green"))))
         (me-color-string time-as-string color))))

(me-register-mold
 :key "TestRunningStats"
 :let ((report (ignore-errors
                 (me-find-relative-test-report (buffer-file-name)))))
 :given (:fn (and
              (me-require 'esxml)
              report))
 :then (:fn
        (let* ((file (buffer-file-name))
               (testcases (esxml-query-all "testcase" report)))
          (with-current-buffer buffername
            (erase-buffer)
            (org-mode)
            (insert (format "* %s Statistics\n\n" buffername))
            (me-insert-org-table
             `(("Test Case" .
                (:extractor
                 (lambda (obj)
                   (message "%s" obj)
                   (--> obj
                        cdr
                        car
                        (alist-get 'name it)))
                 :handler
                 (lambda (s)
                   (me-make-elisp-navigation-link
                    (s-trim (-last-item (s-split "should" s t)))
                    ,file))))
               ("Time (s)" .
                (:extractor
                 (lambda (obj) (--> obj
                                    cdr
                                    car
                                    (alist-get 'time it)))
                 :handler
                 (lambda (s) (me-highlight-unit-test-time s)))))
             testcases)
            (setq-local self report))))
 :docs "Show performance stats of tests that produce a XML report in JUnit style.

For Clojure support, you need to use https://github.com/ruedigergad/test2junit and have the
following in your lein project.clj

`:test2junit-output-dir \"target/test-reports\"
  :profiles {:dev {:plugins [[test2junit \"1.4.2\"]]}}`")

(me-register-mold
 :key "CSVToBarChart"
 :given (:fn
         (and (executable-find "graph")
              (me-require 'csv-mode)
              (eq major-mode 'csv-mode)))
 :then (:fn
        (let ((table nil) ;; TODO maybe I can use org-table-convert-region to produce a org table, also maybe another mold
              (contents (buffer-substring-no-properties (point-min) (point-max))))
          (with-current-buffer buffername
            (erase-buffer)
            (with-temp-file "/tmp/somefile.csv"
              (insert contents))
            (setq-local self table)
            (async-shell-command
             (format
              "graph --bar --figsize %sx%s --xtick-angle 90 %s"
              (display-pixel-width)
              (display-pixel-height)
              "/tmp/somefile.csv"))
            (insert "Placeholder buffer..."))))
 :docs "Make a bar chart out of a csv buffer.")

(me-register-mold
 :key "CSVToLineChart"
 :given (:fn (and (executable-find "graph")
                  (me-require 'csv-mode)
                  (eq major-mode 'csv-mode)))
 :then (:fn
        (let ((table nil) ;; TODO maybe I can use org-table-convert-region to produce a org table, also maybe another mold
              (contents (buffer-substring-no-properties (point-min) (point-max))))
          (with-current-buffer buffername
            (erase-buffer)
            (with-temp-file "/tmp/somefile.csv"
              (insert contents))
            (setq-local self table)
            (async-shell-command
             (format
              "graph --figsize %sx%s --xtick-angle 90 %s"
              (display-pixel-width)
              (display-pixel-height)
              "/tmp/somefile.csv"))
            (insert "Placeholder buffer..."))))
 :docs "Make a line chart out of a csv buffer.")

(me-register-mold-by-key
 "FirstOrgTableToLineChart"
 (me-mold-compose
  (me-mold-compose "FirstOrgTable"  "OrgTableToCSV")
  "CSVToLineChart"
  '((:docs "Make a line chart out of an Org table."))))

(me-register-mold-by-key
 "FirstOrgTableToBarChart"
 (me-mold-compose
  (me-mold-compose "FirstOrgTable"  "OrgTableToCSV")
  "CSVToBarChart"
  '((:docs "Make a bar chart out of an Org table."))))

(me-register-mold-by-key
 "PlistToBarChart"
 (me-mold-compose
  "ElispListToOrgTable"
  "FirstOrgTableToBarChart"
  '((:docs "Make a bar chart out of a plist"))))

(me-register-mold-by-key
 "PlistToLineChart"
 (me-mold-compose
  "ElispListToOrgTable"
  "FirstOrgTableToLineChart"
  '((:docs "Make a line chart out of a plist"))))

(defun me-find-children (node tree)
  "Find children of NODE in TREE."
  (--filter
   (progn
     (and
      (> (plist-get it :begin) (plist-get node :begin) )
      (< (plist-get it :end) (plist-get node :end))))
   tree))

(defun me-find-child-with-type (type node tree)
  "Find child of NODE with TYPE belonging to TREE."
  (--> (me-find-children node tree)
       (--find
        (equal (plist-get it :type) type)
        it)))

(defun me-clojure-function-macro-nodes (clojure-flattened-tree)
  "Filter only functions and macros from CLOJURE-FLATTENED-TREE."
  (--> clojure-flattened-tree
       (me-by-type 'list_lit it)
       (--filter (or
                  (s-starts-with-p "(defmacro" (plist-get it :text))
                  (s-starts-with-p "(defn" (plist-get it :text)))
                 it)))

(defun me-functions-complexity (tree complexity-fn)
  "Calculate complexity of functions in TREE by using COMPLEXITY-FN."
  (--> tree
       (if-let ((clj-fns (me-clojure-function-macro-nodes it)))
           clj-fns
         (me-by-type 'function_definition it))
       (--map
        (let ((text (plist-get it :text)))
          (list
           :identifier (plist-get
                        (or (me-find-child-with-type 'identifier it tree)
                            (me-find-child-with-type ; clojure hack: find the name of the fn, but avoid to pick the defn/defmacro sym_lit
                             'sym_lit it
                             (--remove (and (or (equal "defn" (plist-get it :text))
                                                (equal "defmacro" (plist-get it :text)))
                                            (eq 'sym_lit (plist-get it :type)))
                                       tree)))
                        :text)
           :complexity (funcall complexity-fn text) ;; TODO this is a dependency on code-compass!
           :node it))
        it)))

(defun me-highlight-function-complexity (complexity)
  "Highlight with color COMPLEXITY. This specifies ad-hoc thresholds."
  (let* ((str (format "%s" complexity))
         (color (cond
                 ((>= complexity 12) "red") ;; TODO numbers at random!
                 ((>= complexity 6) "orange")
                 ('otherwise "green"))))
    (me-color-string str color)))

(defun me-highlight-function-length (len)
  (let* ((str (format "%s" len))
         (color (cond
                 ((>= len 20) "red") ;; TODO numbers at random!
                 ((>= len 8) "orange")
                 ('otherwise "green"))))
    (me-color-string str color)))

(me-register-mold
 :key "FunctionsComplexity"
 :let ((complexities
        (ignore-errors (me-functions-complexity
                        (me-mold-treesitter-to-parse-tree)
                        #'c/calculate-complexity-stats))))
 :given (:fn (and
              (me-require 'code-compass)
              (me-require 'tree-sitter)
              (not (eq major-mode 'json-mode))
              (not (eq major-mode 'csv-mode))
              (not (eq major-mode 'yaml-mode))
              complexities))
 :then (:fn
        (with-current-buffer buffername
          (erase-buffer)
          (org-mode)
          (setq-local self complexities)
          (me-insert-org-table
           `(("Function" .
              (:extractor
               (lambda (obj) obj)
               :handler
               (lambda (obj) (me-make-elisp-navigation-link
                              (plist-get obj :identifier)
                              (plist-get (plist-get obj :node) :buffer-file)))))
             ("Complexity" .
              (:extractor
               (lambda (obj) (alist-get 'total (plist-get obj :complexity)))
               :handler
               (lambda (s) (me-highlight-function-complexity s))))
             ("Length" .
              (:extractor
               (lambda (obj) (alist-get 'n-lines (plist-get obj :complexity)))
               :handler
               (lambda (s) (me-highlight-function-length s))))
             )
           complexities)
          ))
 :docs "Show a table showing code complexity for the functions in the buffer.")

;; TODO make mold that let you open a note, this should add a warning if the note is outdated (i.e., the position cannot be found anymore)
(defun me-structure-to-dot-string (structure)
  (format
   "%s=%s;"
   (plist-get structure :key)
   (plist-get structure :option)))

(defun me-node-to-dot-string (node) ;; TODO probably be flexible: remove :key and use all the other entries as they are after making :xx into 'xx and wrapping values into `=""'?
  (format
   "%s [label=\"%s\" shape=\"%s\" style=\"filled\" fillcolor=\"%s\"]"
   (plist-get node :key)
   (or (plist-get node :label) (plist-get node :key))
   (or (plist-get node :shape) "")
   (or (plist-get node :color) "")))

(defun me-edge-to-dot-string (edge)
  (format
   "%s -> %s [taillabel=\"%s\"]"
   (plist-get edge :from)
   (plist-get edge :to)
   (or (plist-get edge :label) "")
   ;; TODO add shape!
   ))

(defun me-diagram-to-dot-string (diagram)
  (concat
   "digraph {\n"
   (string-join (mapcar #'me-structure-to-dot-string (plist-get diagram :structure)) "\n")
   "\n"
   (string-join (mapcar #'me-node-to-dot-string (plist-get diagram :nodes)) "\n")
   "\n"
   (string-join (mapcar #'me-edge-to-dot-string (plist-get diagram :edges)) "\n")

   "\n}\n"))

(defun me-dot-string-to-picture (dot-diagram)
  (if-let* ((dot (executable-find "dot"))
            (dotfilename (make-temp-file "dot" nil ".dot"))
            (filename (make-temp-file "image" nil ".svg"))
            (_ (or (write-region dot-diagram nil dotfilename) "don't stop!"))
            (output (shell-command-to-string (format "%s -Tsvg -o%s %s" dot filename dotfilename))))
      filename
    (message "Something went wrong in generating a dot file: %s." (list dot dotfilename filename output))))

;; (let ((diagram '(
;;                  :structure ((:key rankdir :option LR))
;;                  :nodes ((:key "a" :shape circle :color red)
;;                          (:key "b" :shape circle :color red))
;;                  :edges ((:from "a" :to "b" :label "someLabel" :shape 'dotted)
;;                          (:from "b" :to "a" :label "someOtherLabel")))))
;;   (find-file (me-dot-string-to-picture (me-diagram-to-dot-string diagram))))


(me-register-mold ;; https://orgmode.org/worg/org-tutorials/org-dot-diagrams.html
 :key "OrgTablesToDot"
 :let ((tables (me-all-flat-org-tables)))
 :given (:fn (and
              (eq major-mode 'org-mode)
              (>= (length tables) 1)
              (<= (length tables) 3)
              't ;; TODO check for tables
              ;; TODO check for buffer name since probably I can avoid to make these by hand..
              ))
 :then (:fn
        (let* ((diagram (list
                         :structure (--find (-contains-p (car it) :option) tables)
                         :nodes (--find (-contains-p (car it) :key) tables)
                         :edges (--find (-contains-p (car it) :to) tables))))
          (with-current-buffer buffername
            (erase-buffer)
            (insert (me-diagram-to-dot-string diagram))
            (setq-local self tables))))
 :docs "Convert Org tables to Graphviz dot format.")

(me-register-mold ;; https://orgmode.org/worg/org-tutorials/org-dot-diagrams.html
 :key "DotToPicture"
 :given (:fn (and
              (executable-find "dot")
              (eq major-mode 'fundamental-mode)
              (s-contains-p "Dot" (buffer-name))))
 :then (:fn
        (let* ((file (me-dot-string-to-picture (buffer-substring-no-properties (point-min) (point-max)))))
          (with-current-buffer buffername
            (fundamental-mode)
            (erase-buffer)
            (insert-file-contents-literally file)
            (image-mode)
            (setq-local self file))))
 :docs "Convert Graphviz dot buffer to an graph image.")

(me-register-mold-by-key
 "OrgTablesToDotPicture"
 (me-mold-compose "OrgTablesToDot" "DotToPicture"
                  '((
                     :when
                     (:fn
                      (progn
                        (ignore-errors (unless (s-contains-p "| key" (thing-at-point 'line)) (goto-char (search-backward "| key"))))
                        (not
                         ;; TODO this is fragile if I move the cursor in the Org Table buffer..
                         ;; I could use :old-point in the mold-data of the "OrgTablesToDot" mold to go to the right place?
                         ;; or I could just have a function that jumps at the beginning of the table for this?
                         (equal (ignore-errors me-org-tables-to-dot) ;first time this is undefined
                                (setq-local me-org-tables-to-dot (me-all-flat-org-tables))))))
                     ;; add :when clause saying that if the table at point has changed, return true
                     :docs "Make a graph image out of Org tables representing a graph."))))

(me-register-mold
 :key "Image To Text"
 :docs "Extracts text from the image using `imageclip'."
 :let ((file-name (buffer-file-name))
       (buf-name (buffer-name)))
 :given (:fn (and
              (eq major-mode 'image-mode)
              (executable-find "imgclip")))
 :then
 (
  :async ((_ (shell-command-to-string
              (format "imgclip -p '%s' --lang eng"
                      (or file-name
                          ;; otherwise store the open image in /tmp for imgclip to work on a file
                          (let ((path (concat "/tmp/" buf-name)))
                            (write-region (point-min) (point-max) path)
                            path))))))
  :fn (let* ((img (list :img (or (buffer-file-name) (buffer-name)))))
        (with-current-buffer buffername
          (erase-buffer)
          (clipboard-yank)
          (setq-local self img)
          (plist-put self :text (buffer-substring-no-properties (point-min) (point-max))))))
 :examples nil)

(me-register-mold
 :key "List Files To Edit After This"
 :let ((bufferfile (buffer-file-name)))
 :given (:fn (and
              bufferfile
              (me-require 'code-compass)
              (me-require 'vc)
              (vc-root-dir)))
 :then (:fn
        (with-current-buffer buffername
          (read-only-mode -1)
          (emacs-lisp-mode)
          (erase-buffer)
          (insert "Loading coupled files...")
          )
        (c/get-coupled-files-alist
         (vc-root-dir)
         `(lambda (files)
            (with-current-buffer ,buffername
              (erase-buffer)
              (me-print-to-buffer
               (c/get-matching-coupled-files files ,bufferfile)
               ,buffername)
              (setq-local self files)))))
 :docs "You can list the files coupled to the file you are visiting."
 :examples nil)

(me-register-mold
 :key "Files To Edit As Org Todos"
 :let ((old-file (plist-get (ignore-errors mold-data) :old-file)))
 :given (:fn
         (and
          (me-require 'code-compass)
          (s-contains-p "Files To Edit After " (buffer-name))
          self
          old-file))
 :then (:fn
        (let* ((current-buffer (current-buffer))
               (tree (-map 'car self))
               (buffer (let ((buffer (c/show-todo-buffer tree old-file)))
                         (switch-to-buffer current-buffer)
                         buffer)))))    ;; TODO likely broken in refactoring!
 :docs "You can make a TODO list of files to edit next."
 :examples nil)

(me-register-mold
 :key "Clojure examples for function at point"
 :let ((matching-test (ignore-errors
                        (projectile-find-matching-test (buffer-file-name))) ))
 :given (:fn (and
              (me-require 'projectile)
              (eq major-mode 'clojure-mode)
              (equal (nth 0 (list-at-point)) 'defn)
              matching-test))
 :then (:fn
        (let* ((test-file
                (concat (projectile-project-root) matching-test))
               (test-file-tree (me-filepath-to-flattened-tree test-file))
               (funct (list-at-point))
               (function-name
                (--> funct
                     (when (and it (> (length it) 3) (equal (nth 0 it) 'defn)) (nth 1 it))
                     (symbol-name it)))
               (examples-nodes
                (--> (me-by-type 'list_lit test-file-tree)
                     (--filter (and
                                (s-starts-with-p "(is(=" (s-replace " " "" (plist-get it :text)))
                                (s-contains-p function-name (plist-get it :text)))
                               it))))
          (with-current-buffer buffername
            (org-mode)
            (erase-buffer)
            (insert (concat "* Examples for " function-name "\n\n"))
            (--each examples-nodes
              (insert (format
                       "
- [[elisp:(progn (find-file \"%s\") (goto-char %s))][Between test file char %s and %s]]

  #+begin_src clojure
%s
  #+end_src
"
                       (plist-get it :buffer-file)
                       (plist-get it :begin)
                       (plist-get it :begin)
                       (plist-get it :end)
                       (plist-get it :text))))
            (setq-local self funct)
            (setq-local org-confirm-elisp-link-function nil))))
 :docs "You can list examples of usage of the Clojure function at point."
 :examples nil)


(me-register-mold
 :key "EdnToElisp"
 :let ((edn (or
             (when (region-active-p)
               (ignore-errors (parseedn-read-str (buffer-substring-no-properties (caar (region-bounds)) (cdar (region-bounds))))))
             (ignore-errors (parseedn-read)))))
 :given (:fn (and
              (or (eq major-mode 'clojure-mode)
                  (eq major-mode 'clojurec-mode)
                  (eq major-mode 'clojurescript-mode))
              (me-require 'parseedn)
              edn))
 :then (:fn
        (let* ((plist (me-hash-to-plist edn)))
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (erase-buffer)
            (me-print-to-buffer plist)
            (setq-local self plist))))
 :docs "You can parse EDN format as an Elisp object."
 :examples nil)

(me-register-mold
 :key "Playground Clojure"
 :given (:fn (and (me-require 'clojure-mode)))
 :then (:fn
        (let* ((region (me-get-region))
               (tree (ignore-errors self)))
          (with-current-buffer buffername
            (clojure-mode)
            (erase-buffer)
            (insert ";; Tips:\n;;    Use `self' to access the mold context.\n;;    You can access the previous mold context through `mold-data'.\n\n")
            (when region (insert region))
            (me-override-keybiding-in-buffer
             (kbd "C-x B")
             '(lambda ()
                (interactive)
                (if (executable-find "bb") (async-shell-command "bb nrepl-server 1667") (error "Install babanshka first!"))
                (cider-connect-clj (list :host "localhost" :port 1667))
                ))
            (ignore-errors
              (when (cider-connected-p) (cider-nrepl-sync-request:eval (format "(def self '%s)" (let ((x self)) (with-temp-buffer (me-print-to-buffer x)
                                                                                                                                  (buffer-string)))))))
            (setq-local self tree))))
 :docs "You can play around with code in Clojure."
 :examples ((
             :name "Empty Playground"
             :given
             (:type buffer :name "example.txt" :mode text-mode :contents "")
             :then
             (:type buffer :name "*moldable-emacs-Playground Clojure*" :mode clojure-mode :contents ";; Tips:\n;;    Use `self' to access the mold context.\n;;    You can access the previous mold context through `mold-data'.\n\n"))))

(me-register-mold
 :key "Eval With Clojure"
 :given (:fn (and (or (eq major-mode 'clojure-mode)
                      (eq major-mode 'clojurec-mode)
                      (eq major-mode 'clojurescript-mode))
                  (me-require 'clojure-mode)
                  (me-require 'cider)
                  (cider-connected-p)))
 :then (:fn
        (let* ((tree (ignore-errors (--> (cider-last-sexp)
                                         (cider-nrepl-sync-request:eval it ;; (ignore-errors (cider-current-ns))
                                                                        )
                                         (nrepl-dict-get it "value")))))
          (with-current-buffer buffername
            (clojure-mode)
            (erase-buffer)
            (insert tree)
            (pp-buffer)
            (setq-local self tree))))
 :docs "You can evaluate the *last* Clojure expression.")


(defvar me-lighthouse-url-to-audit nil "This is a variable to set for setting the Audit mold. Useful for looping.")

(me-register-mold
 :key "Audit with Lighthouse"
 :let ((url (or me-lighthouse-url-to-audit (thing-at-point-url-at-point))))
 :given (:fn (and
              url
              (me-require 'json)
              (executable-find "lighthouse") ;; npm i -g lighthouse
              (or (executable-find "chromium")
                  (executable-find "chrome")
                  (file-exists-p "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome"))))
 :then (
        :async ((_ (shell-command (format "lighthouse %s --quiet --chrome-flags=--headless --output json --output-path /tmp/audit.json" url)))
                (_ (while (not (file-exists-p "/tmp/audit.json")) (sleep-for 1.5))))
        :fn (let* ((plist (with-temp-buffer
                            (insert-file-contents-literally "/tmp/audit.json")
                            (goto-char (point-min))
                            (json-read))))
              (with-current-buffer buffername
                (emacs-lisp-mode)
                (erase-buffer)
                (me-print-to-buffer plist)
                (setq-local self plist)
                (delete-file "/tmp/audit.json"))))
 :docs "You can audit a url with Lighthouse."
 :examples nil)


(me-register-mold
 :key "List To Dot" ;; TODO this could be merged with the OrgTablesToDot
 :let ((text (or
              (me-get-region)
              (thing-at-point 'paragraph))))
 :given (:fn (and text))
 :then (:fn
        (let* ((diagram (--> text
                             me-replace-org-links-with-descriptions
                             (s-replace-all
                              '(("\"" . "")
                                ("\n" . " "))
                              it)
                             (s-split ".) " it t)
                             (--map (--> it
                                         (s-split " " it t)
                                         (-take 5 it)
                                         (s-join " " it))
                                    it)
                             (--map (list :key (concat "node" (substring (sha1 it) 0 10)) :label it) it)
                             (list
                              :structure '((:key rankdir :option TD))
                              :nodes it
                              :edges (--map
                                      (list :from (plist-get (nth 0 it) :key)
                                            :to (plist-get (nth 1 it) :key)
                                            :label "")
                                      (-partition-in-steps 2 1 it))))))
          (with-current-buffer buffername
            (erase-buffer)
            (insert (me-diagram-to-dot-string diagram))
            (setq-local self diagram))))
 :docs "You can transform a bullet list like '1) some 2) any' into a dot graph."
 :examples ((
             :name "Simple List"
             :given
             (:type file :name "/tmp/example.txt" :mode text-mode :contents "1) something\n2) something else" :point 1)
             :then
             (:type buffer :name "*moldable-emacs-List To Dot*" :mode fundamental-mode :contents "digraph {\nrankdir=TD;\nnode1af17e7372 [label=\"something\" shape=\"\" style=\"filled\" fillcolor=\"\"]\nnode637828c03a [label=\"something else\" shape=\"\" style=\"filled\" fillcolor=\"\"]\nnode1af17e7372 -> node637828c03a [taillabel=\"\"]\n}\n"))))

(me-register-mold-by-key
 "List To Picture"
 (me-mold-compose "List To Dot"  "DotToPicture"))


(me-register-mold
 :key "Playground JS with Nyxt"
 :given (:fn (and
              (me-require 'emacs-with-nyxt)
              (emacs-with-nyxt-connected-p)
              (emacs-with-nyxt-send-sexps '(find-mode (current-buffer) 'web-mode))))
 :then (:fn
        (let* ((tree (ignore-errors self)))
          (with-current-buffer buffername
            (js-mode)
            (erase-buffer)
            (setq-local self tree))))
 :docs "You can write some JS to evaluate with Eval JS with Nyxt."
 :examples nil)

(me-register-mold
 :key "Eval JS with Nyxt"
 :given (:fn (and
              (eq major-mode 'js-mode)
              (me-require 'emacs-with-nyxt)
              (emacs-with-nyxt-connected-p)
              (emacs-with-nyxt-send-sexps '(find-mode (current-buffer) 'web-mode))))
 :then (:fn
        (let* ((result (--> (buffer-substring-no-properties (point-min) (point-max))
                            (emacs-with-nyxt-send-sexps `(ffi-buffer-evaluate-javascript (current-buffer) ,it))
                            (string-reverse it)
                            (s-split " ," it)
                            (-drop 1 it)
                            (s-join " ," it)
                            (string-reverse it)
                            (read it))))
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (erase-buffer)
            (me-print-to-buffer result)
            (setq-local self result))))
 :docs "You can evaluate some JS in the current buffer of Nyxt."
 :examples nil)

(me-register-mold
 :key "Playground Parenscript with Nyxt"
 :given (:fn (and
              (ignore-errors (emacs-with-nyxt-connected-p))
              (me-require 'emacs-with-nyxt)
              (emacs-with-nyxt-send-sexps '(find-mode (current-buffer) 'web-mode))))
 :then (:fn
        (let* ((tree (ignore-errors self)))
          (with-current-buffer buffername
            (lisp-mode)
            (if (eq cl-ide 'sly)
                (sly-mode)
              (slime-mode))
            (erase-buffer)
            (insert "(ps:ps\n\n;;write your Parenscript here\n\n)")
            (setq-local self tree))))
 :docs "You can write some Parenscript to evaluate with Eval Common Lisp with Nyxt."
 :examples nil)

(me-register-mold
 :key "Eval Parenscript with Nyxt"
 :let ((tree (progn (unless (list-at-point)
                      (progn (goto-char (point-min)) (search-forward "(" nil t)))
                    (or (ignore-errors (eval (list-at-point))) (list-at-point)))))
 :given (:fn (and
              (eq major-mode 'lisp-mode)
              (me-require 'emacs-with-nyxt)
              (emacs-with-nyxt-connected-p)
              (emacs-with-nyxt-send-sexps '(find-mode (current-buffer) 'web-mode))
              tree))
 :then (:fn
        (let* ((result (--> (emacs-with-nyxt-send-sexps `(ffi-buffer-evaluate-javascript (current-buffer) ,tree))
                            (string-reverse it)
                            (s-split " ," it)
                            (-drop 1 it)
                            (s-join " ," it)
                            (string-reverse it)
                            (read it))))
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (erase-buffer)
            (me-print-to-buffer result)
            (setq-local self result))))
 :docs "You can evaluate some Parenscript in the current buffer of Nyxt."
 :examples nil)

(me-register-mold
 :key "AllMolds"
 :given (:fn t)
 :then (:fn
        (let* ((molds me-available-molds)
               (missing-deps-molds
                (--filter
                 (plist-get it :missing-dependencies)
                 (me-find-missing-dependencies-for-molds (me-usable-molds-requiring-deps)))))
          (with-current-buffer buffername
            (erase-buffer)
            (org-mode)
            (insert "* All Registered Molds.\n\n")
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
 :key "Backlinks as Org"
 :given (:fn (and
              (me-require 'org-roam)
              (org-roam-node-p (org-roam-node-at-point))))
 :then (:fn
        (let* ((backlinks (org-roam-backlinks-get (org-roam-node-at-point))))
          (with-current-buffer buffername
            (org-mode)
            (erase-buffer)
            (--each backlinks
              (insert-file-contents-literally (org-roam-node-file (org-roam-backlink-source-node it)))
              ;; TODO add link to original file somehow?
              ;; (insert (format "[%s]" (org-roam-node-file (org-roam-backlink-source-node it))))
              )
            (setq-local self backlinks))))
 :docs "You can check backlinks for current org-roam node."
 :examples nil)

(me-register-mold
 :key "Backlinks as Org With transclusion"
 :given (:fn (and
              (me-require 'org-roam)
              (me-require 'org-transclusion)
              (org-roam-node-p (org-roam-node-at-point))))
 :then (:fn
        (let* ((backlinks (org-roam-backlinks-get (org-roam-node-at-point))))
          (with-current-buffer buffername
            (org-mode)
            (org-transclusion-remove-all)
            (erase-buffer)
            (--each backlinks
              (insert (format "#+transclude: [[id:%s][%s]]\n\n" (org-roam-node-id (org-roam-backlink-source-node it)) (org-roam-node-title (org-roam-backlink-source-node it))))
              )
            (org-transclusion-add-all)
            (goto-char (point-min))
            (setq-local self backlinks))))
 :docs "You can check backlinks for current org-roam node."
 :examples nil)

(defcustom me-backlinks-depth 2 "Define how deep you want to search for backlinks for \"Deep Backlinks as Org\" mold.")

(me-register-mold
 :key "Deep Backlinks as Org"
 :given (:fn (and
              (me-require 'org-roam)
              (org-roam-node-p (org-roam-node-at-point))))
 :then (:fn
        (let* ((backlinks-contents (me-org-roam-backlinks-contents (org-roam-node-at-point) me-backlinks-depth)))
          (with-current-buffer buffername
            (org-mode)
            (erase-buffer)
            (insert (me-org-roam-format-backlinks-contents backlinks-contents))
            (setq-local self backlinks-contents))))
 :docs "You can format nested backlinks as a single Org mode buffer."
 :examples nil)


(defcustom me-languages
  (list :from "en" :to '("fr" "de" "it" "no" "pt" "es" "sv"))
  "Languages to translate to.")

(me-register-mold
 :key "To multiple languages"
 :let ((sentence (or (me-get-region)
                     (sentence-at-point t))))
 :given (:fn (and
              me-languages
              sentence
              (me-require 'google-translate)))
 :then (:fn
        (let* ((translations
                (-map
                 (lambda (language)
                   (google-translate-translate (plist-get me-languages :from) language sentence 'kill-ring)
                   (list :language language :translation (car kill-ring)))
                 (plist-get me-languages :to)))
               (tree (--map (append (list :sentence sentence) it) translations)))
          (with-current-buffer buffername
            (org-mode)
            (erase-buffer)
            (insert (format "* %s\n\n" sentence))
            (--each translations
              (insert (format "** %s #%s\n\n"
                              (plist-get it :language)
                              (plist-get it :translation) )))
            (setq-local self tree))))
 :docs "You can translate text to multiple languages."
 :examples '((:name "Hello there translation"
                    :given
                    (:type file :name "/tmp/test.txt" :mode text-mode :contents "Hello there!" :point 13)
                    :then
                    (:type buffer :name "*moldable-emacs-To multiple languages*" :mode org-mode :contents "* Hello there!\n\n** fr #Bonjour!\n\n** de #Hallo!\n\n** it #Ciao!\n\n** no #Hei der!\n\n** pt #Olá!\n\n** es #¡Hola!\n\n** sv #Hej där!\n\n"))))

(defun me-get-org-roam-notes-with-tags (tags)
  "Get org roam notes matching TAGS."
  (-flatten
   (--keep
    (ignore-errors (--> it
                        org-roam-node-from-title-or-alias
                        org-roam-backlinks-get
                        (-map 'org-roam-backlink-source-node it)))
    tags)))

(me-register-mold
 :key "TagsToOrgRoamNotes"
 :given (:fn (and (me-require 'org-roam)))
 :then (:fn
        (let* ((tags (completing-read-multiple
                      "Choose tags (or make new ones):"
                      (--> (org-roam--get-titles)
                           (--filter (= 1 (length (and (s-lowercase-p it) (<= (length (s-split "-" it)) 1) (s-split " " it)))) it)
                           )))
               (notes (ignore-errors
                        (--reduce-from
                         (-intersection (me-get-org-roam-notes-with-tags (list it)) acc)
                         (me-get-org-roam-notes-with-tags (list (car tags))) ; if nil, -intersect would always return nil
                         (cdr tags))))
               (tags-and-notes (list :tags tags :notes notes)))
          (with-current-buffer buffername
            (org-mode)
            (org-transclusion-remove-all)
            (erase-buffer)
            (--each notes
              (insert (format "#+transclude: [[id:%s][%s]]\n\n" (org-roam-node-id it) (org-roam-node-title it))))
            (org-transclusion-add-all)
            (goto-char (point-min))
            (setq-local self tags-and-notes))))
 :docs "You can query the Org Roam notes that contain ALL the selected tags."
 :examples nil)

(me-register-mold
 :key "LearnSyntax"
 :given (:fn (ignore-errors (and (plist-get (car self) :begin)
                                 (plist-get (car self) :end))))
 :then (:fn
        (let* ((language (--> (plist-get (car self) :mode)
                              symbol-name
                              (s-replace "-mode" "" it)))
               (grouped-nodes (--sort (< (length (cdr it))
                                         (length (cdr other)))
                                      (--group-by (plist-get it :type) self))))
          (with-current-buffer buffername
            (org-mode)
            (erase-buffer)
            (insert (format "* Syntax Types for %s\n\n" language))
            (--each-indexed grouped-nodes
              (let* ((type (or (ignore-errors (symbol-name (nth 0 it)))
                               (car it)))
                     (description (me-syntax-description type language))
                     (kill (format
                            "  [[elisp:(add-to-list 'kill-ring (s-join \"\\n\" (-distinct (--map (plist-get it :text) (cdr (nth %s self))))))][Kill matching text]]"
                            it-index))
                     (p (point))
                     (nodes (cdr it)))
                (insert (format "- %s :: %s found\n\n  %s\n%s\n\n" type (length (cdr it)) description kill))
                (me-insert-follow-overlay (list :begin (+ 2 p) ; to keep track of the "- " bit
                                                :end (- (point) 1)) ; `insert' adds the pointer on the space after
                                          nodes)))
            (setq-local self grouped-nodes))))
 :docs "You can learn about the syntax of a language."
 :examples nil)

(me-register-mold-by-key "LearnSyntaxOfCode"
                         (me-mold-compose "CodeAsTree" "LearnSyntax"
                                          '((:docs "Explain syntax of code."))))



(me-register-mold
 :key "FlycheckErrorsAsTree"
 :given (:fn (and
              (me-require 'flycheck)
              (flycheck-has-current-errors-p)))
 :then (:fn
        (let* ((errors (--> (flycheck-overlay-errors-in (point-min)
                                                        (point-max))
                            (--map (let* ((buffer (buffer-name (flycheck-error-buffer it)))
                                          (begin (flycheck-error-pos it))
                                          (end (with-current-buffer buffer
                                                 (next-single-char-property-change begin 'flycheck-error)))
                                          (text (buffer-substring-no-properties begin end)))
                                     (list :type (flycheck-error-checker it)
                                           :text text
                                           :error (flycheck-error-message it)
                                           :begin begin
                                           :end end
                                           :buffer buffer
                                           :filename (flycheck-error-filename it)
                                           ))
                                   it))))
          (with-current-buffer buffername
            (emacs-lisp-mode)
            (erase-buffer)
            (me-print-to-buffer errors)
            (setq-local self errors))))
 :docs "You can see Flycheck errors as a flattened tree."
 :examples ((
             :name "Missing quotes in clj map"
             :given
             (:type buffer :name "*moldable-emacs-Playground Clojure*" :mode clojure-mode :contents ";; Tips:\n;;    Use `self' to access the mold context.\n;;    You can access the previous mold context through `mold-data'.\n\n{:key needs-quotes}" :point 143)
             :then
             (:type buffer :name "*moldable-emacs-FlycheckErrorsAsTree*" :mode emacs-lisp-mode :contents "((:type clj-kondo-clj :text \"needs-quotes\" :error \"Unresolved symbol: needs-quotes\" :begin 130 :end 142 :buffer \"*moldable-emacs-Playground Clojure*\" :filename nil))\n"))))
