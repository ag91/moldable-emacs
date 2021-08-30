(defun me/highlight-unit-test-time (time-as-string)
  "Highlight TIME-AS-STRING according to unit tests."
  (and (ignore-errors (string-to-number time-as-string))
       (let* ((time (string-to-number time-as-string))
              (color (cond
                      ((>= time (/ 10.0 1000)) "red")
                      ((>= time (/ 2.0 1000)) "orange")
                      ('otherwise "green"))))
         (me/color-string time-as-string color))))

(me/register-mold
 :key "TestRunningStats"
 :given (lambda () (and
                    (me/require 'esxml)
                    (ignore-errors
                      (me/find-relative-test-report (buffer-file-name)))))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (file (buffer-file-name))
                (self (with-file
                       (me/find-relative-test-report (buffer-file-name))
                       (libxml-parse-html-region (point-min) (point-max))))
                (testcases (esxml-query-all "testcase" self))
                (buffer (get-buffer-create (concat "Test Stats For" buffername))))
           (with-current-buffer buffer
             (erase-buffer)
             (org-mode)
             (insert (format "* %s Statistics\n\n" buffername))
             (me/insert-org-table
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
                    (me/make-elisp-navigation-link
                     (s-trim (-last-item (s-split "should" s t)))
                     ,file))))
                ("Time (s)" .
                 (:extractor
                  (lambda (obj) (--> obj
                                  cdr
                                  car
                                  (alist-get 'time it)))
                  :handler
                  (lambda (s) (me/highlight-unit-test-time s)))))
              testcases)
             (setq-local self self)
             buffer)))
 :docs "Show performance stats of tests that produce a XML report in JUnit style.

For Clojure support, you need to use https://github.com/ruedigergad/test2junit and have the
following in your lein project.clj

`:test2junit-output-dir "target/test-reports"
  :profiles {:dev {:plugins [[test2junit "1.4.2"]]}}`")


(me/register-mold
 :key "CSVToBarChart"
 :given (lambda ()
          (and (executable-find "graph")
               (eq major-mode 'csv-mode)))
 :then (lambda ()
         (let ((table nil) ;; TODO maybe I can use org-table-convert-region to produce a org table, also maybe another mold
               (contents (buffer-substring-no-properties (point-min) (point-max))))
           (with-temp-file "/tmp/somefile.csv"
             (insert contents))
           (setq-local self table)
           (async-shell-command
            (format
             "graph --bar --figsize %sx%s --xtick-angle 90 %s"
             (display-pixel-width)
             (display-pixel-height)
             "/tmp/somefile.csv"))
           (current-buffer))))

(me/register-mold
 :key "CSVToLineChart"
 :given (lambda () (and (executable-find "graph")
                        (eq major-mode 'csv-mode)))
 :then (lambda ()
         (let ((table nil) ;; TODO maybe I can use org-table-convert-region to produce a org table, also maybe another mold
               (contents (buffer-substring-no-properties (point-min) (point-max))))
           (with-temp-file "/tmp/somefile.csv"
             (insert contents))
           (setq-local self table)
           (async-shell-command
            (format
             "graph --figsize %sx%s --xtick-angle 90 %s"
             (display-pixel-width)
             (display-pixel-height)
             "/tmp/somefile.csv"))
           (current-buffer))))

(me/register-mold-by-key
 "FirstOrgTableToLineChart"
 (me/mold-compose
  (me/mold-compose "FirstOrgTable"  "OrgTableToCSV")
  "CSVToLineChart"))

(me/register-mold-by-key
 "FirstOrgTableToBarChart"
 (me/mold-compose
  (me/mold-compose "FirstOrgTable"  "OrgTableToCSV")
  "CSVToBarChart"))

(defun me/find-children (node tree)
  (--filter
   (progn
     (and
      (> (plist-get it :begin) (plist-get node :begin) )
      (< (plist-get it :end) (plist-get node :end))))
   tree))

(defun me/find-child-with-type (type node tree)
  (--> (me/find-children node tree)
    (--find
     (equal (plist-get it :type) type)
     it)))

(defun me/functions-complexity (tree complexity-fn)
  (--> tree
    (me/by-type 'function_definition it)
    (--map
     (let ((text (plist-get it :text)))
       (list
        :identifier (plist-get (me/find-child-with-type 'identifier it tree) :text)
        :complexity (funcall complexity-fn text) ;; TODO this is a dependency on code-compass!
        :node it))
     it)))

(defun me/highlight-function-complexity (complexity)
  (let* ((str (format "%s" complexity))
         (color (cond
                 ((>= complexity 12) "red") ;; TODO numbers at random!
                 ((>= complexity 6) "orange")
                 ('otherwise "green"))))
    (me/color-string str color)))

(defun me/highlight-function-length (len)
  (let* ((str (format "%s" len))
         (color (cond
                 ((>= len 20) "red") ;; TODO numbers at random!
                 ((>= len 8) "orange")
                 ('otherwise "green"))))
    (me/color-string str color)))

(me/register-mold
 :key "FunctionsComplexity"
 :given (lambda () (and
                    (me/require 'code-compass)
                    (me/require 'tree-sitter)
                    (ignore-errors (me/by-type 'function_definition (me/mold-treesitter-to-parse-tree)))
                    ))
 :then (lambda ()
         (let* ((tree (me/mold-treesitter-to-parse-tree))
                (buffer (get-buffer-create (format "Functions Complexity of %s" (buffer-name))))
                (complexities (me/functions-complexity tree #'c/calculate-complexity-stats)))
           (with-current-buffer buffer
             (erase-buffer)
             (org-mode)
             (setq-local self tree)
             (me/insert-org-table
              `(("Function" .
                 (:extractor
                  (lambda (obj) obj)
                  :handler
                  (lambda (obj) (me/make-elisp-navigation-link
                                 (plist-get obj :identifier)
                                 (plist-get (plist-get obj :node) :buffer-file)))))
                ("Complexity" .
                 (:extractor
                  (lambda (obj) (alist-get 'total (plist-get obj :complexity)))
                  :handler
                  (lambda (s) (me/highlight-function-complexity s))))
                ("Length" .
                 (:extractor
                  (lambda (obj) (alist-get 'n-lines (plist-get obj :complexity)))
                  :handler
                  (lambda (s) (me/highlight-function-length s))))
                )
              complexities)
             )
           buffer)))

;; TODO make mold that let you open a note, this should add a warning if the note is outdated (i.e., the position cannot be found anymore)
(defun me/structure-to-dot-string (structure)
  (format
   "%s=%s;"
   (plist-get structure :key)
   (plist-get structure :option)))

(defun me/node-to-dot-string (node) ;; TODO probably be flexible: remove :key and use all the other entries as they are after making :xx into 'xx and wrapping values into `=""'?
  (format
   "%s [label=\"%s\" shape=\"%s\" style=\"filled\" fillcolor=\"%s\"]"
   (plist-get node :key)
   (or (plist-get node :label) (plist-get node :key))
   (or (plist-get node :shape) "")
   (or (plist-get node :color) "")))

(defun me/edge-to-dot-string (edge)
  (format
   "%s -> %s [taillabel=\"%s\"]"
   (plist-get edge :from)
   (plist-get edge :to)
   (or (plist-get edge :label) "")
   ;; TODO add shape!
   ))

(defun me/diagram-to-dot-string (diagram)
  (concat
   "digraph {\n"
   (string-join (mapcar #'me/structure-to-dot-string (plist-get diagram :structure)) "\n")
   "\n"
   (string-join (mapcar #'me/node-to-dot-string (plist-get diagram :nodes)) "\n")
   "\n"
   (string-join (mapcar #'me/edge-to-dot-string (plist-get diagram :edges)) "\n")

   "\n}\n"))

(defun me/dot-string-to-picture (dot-diagram)
  (if-let* ((dot (executable-find "dot"))
            (dotfilename (make-temp-file "dot" nil ".dot"))
            (filename (make-temp-file "image" nil ".png"))
            (_ (or (write-region dot-diagram nil dotfilename) "don't stop!"))
            (output (shell-command-to-string (format "%s -Tpng -o%s %s" dot filename dotfilename))))
      filename
    (message "Something went wrong in generating a dot file: %s." (list dot dotfilename filename output))))

;; (let ((diagram '(
;;                  :structure ((:key rankdir :option LR))
;;                  :nodes ((:key "a" :shape circle :color red)
;;                          (:key "b" :shape circle :color red))
;;                  :edges ((:from "a" :to "b" :label "someLabel" :shape 'dotted)
;;                          (:from "b" :to "a" :label "someOtherLabel")))))
;;   (find-file (me/dot-string-to-picture (me/diagram-to-dot-string diagram))))


(me/register-mold ;; https://orgmode.org/worg/org-tutorials/org-dot-diagrams.html
 :key "OrgTablesToDot"
 :given (lambda () (and
                    (eq major-mode 'org-mode)
                    (>= (length (me/all-flat-org-tables)) 1)
                    (<= (length (me/all-flat-org-tables)) 3)
                    't ;; TODO check for tables
                    ;; TODO check for buffer name since probably I can avoid to make these by hand..
                    ))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (tables (me/all-flat-org-tables))
                (diagram (list
                          :structure (--find (-contains-p (car it) :option) tables)
                          :nodes (--find (-contains-p (car it) :key) tables)
                          :edges (--find (-contains-p (car it) :to) tables)))
                (buffer (get-buffer-create "table-as-dot")))
           (with-current-buffer buffer
             (erase-buffer)
             (insert (me/diagram-to-dot-string diagram))
             (setq-local self tables))
           buffer)))

(me/register-mold ;; https://orgmode.org/worg/org-tutorials/org-dot-diagrams.html
 :key "DotToPicture"
 :given (lambda () (and
                    (executable-find "dot")
                    (eq major-mode 'fundamental-mode)
                    (s-starts-with-p "dot-" (buffer-name))))
 :then (lambda ()
         (let* ((buffername (buffer-name)))
           ;; TODO the following seems broken
           (find-file-other-window (me/dot-string-to-picture (buffer-substring-no-properties (point-min) (point-max))))
           (buffer-name))))

(me/register-mold-by-key
 "OrgTablesToDotPicture"
 (me/mold-compose "OrgTablesToDot" "DotToPicture"))


(me/register-mold
 :key "Image To Text"
 :docs "Extracts text from the image using `imageclip'."
 :given (lambda () (and
                    (eq major-mode 'image-mode)
                    (executable-find "imgclip")))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (img (list :img (or (buffer-file-name) buffername)))
                (buffer (get-buffer-create (format "Text from %s" buffername)))
                (_ (async-map
                    `(lambda (s)
                       (shell-command-to-string
                        (format "imgclip -p '%s' --lang eng" s)))
                    (list (or (buffer-file-name)
                              (let ((path (concat "/tmp/" buffername)))
                                (write-region (point-min) (point-max) path)
                                path)))
                    `(lambda (_)
                       (with-current-buffer ,buffer
                         (erase-buffer)
                         (clipboard-yank)
                         (plist-put self :text (buffer-substring-no-properties (point-min) (point-max))))))))
           (with-current-buffer buffer
             (erase-buffer)
             (setq-local self img)
             (insert "Loading text from image..."))
           buffer))
 :examples ((:name "Initial Loading"
                   :given
                   (:type file :name "/tmp/my.jpg" :mode image-mode :contents "/home/andrea/.emacs.d/lisp/moldable-emacs/resources/my.jpg")
                   :then
                   (:type buffer :name "Text from my.jpg" :mode fundamental-mode :contents "Loading text from image..."))
            ))


(me/register-mold
 :key "List Files To Edit After This"
 :given (lambda () (and
                    (buffer-file-name)
                    (me/require 'code-compass)
                    (me/require 'vc)
                    (vc-root-dir)))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (bufferfile (buffer-file-name))
                (buffer (get-buffer-create (concat "Files To Edit After " buffername))))
           (with-current-buffer buffer
             (read-only-mode -1)
             (emacs-lisp-mode)
             (erase-buffer)
             (insert "Loading coupled files...")
             )
           (c/get-coupled-files-alist
            (vc-root-dir)
            `(lambda (files)
               (with-current-buffer ,buffer
                 (erase-buffer)
                 (prin1
                  (c/get-matching-coupled-files files ,bufferfile)
                  ,buffer)
                 (pp-buffer)
                 (setq-local self files))
               ))
           buffer))
 :docs "You can list the files coupled to the file you are visiting."
 :examples nil)

(me/register-mold
 :key "Files To Edit As Org Todos"
 :given (lambda ()
          (and
           (me/require 'code-compass)
           (s-starts-with-p "Files To Edit After " (buffer-name))
           self
           (plist-get mold-data :old-file)))
 :then (lambda ()
         (let* ((current-buffer (current-buffer))
                (tree (-map 'car self))
                (buffer (let ((buffer (c/show-todo-buffer tree (plist-get mold-data :old-file))))
                          (switch-to-buffer current-buffer)
                          buffer)))
           buffer))
 :docs "You can make a TODO list of files to edit next."
 :examples nil)

(me/register-mold
 :key "Clojure examples for function at point"
 :given (lambda () (and
                    (me/require 'projectile)
                    (eq major-mode 'clojure-mode)
                    (equal (nth 0 (list-at-point)) 'defn)
                    (ignore-errors
                      (projectile-find-matching-test (buffer-file-name)))))
 :then (lambda ()
         (let* ((test-file
                 (concat (projectile-project-root)
                         (projectile-find-matching-test (buffer-file-name))))
                (test-file-tree (me/mold-treesitter-file test-file))
                (funct (list-at-point))
                (function-name
                 (--> funct
                      (when (and it (> (length it) 3) (equal (nth 0 it) 'defn)) (nth 1 it))
                      (symbol-name it)))
                (examples-nodes
                 (--> (me/by-type 'list_lit test-file-tree)
                      (--filter (and
                                 (s-starts-with-p "(is(=" (s-replace " " "" (plist-get it :text)))
                                 (s-contains-p function-name (plist-get it :text)))
                                it)))
                (buffer (get-buffer-create (concat "Examples for " function-name))))
           (with-current-buffer buffer
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
             (setq-local org-confirm-elisp-link-function nil))
           buffer))
 :docs "You can list examples of usage of the Clojure function at point."
 :examples nil)


(me/register-mold
 :key "EdnToElisp"
 :given (lambda () (and
                    (me/require 'parseedn)
                    (or
                     (when (region-active-p)
                       (ignore-errors (parseedn-read-str (buffer-substring-no-properties (caar (region-bounds)) (cdar (region-bounds))))))
                     (ignore-errors (parseedn-read)))))
 :then (lambda ()
         (let* ((buffername (buffer-name))
                (edn (or (when (region-active-p)
                           (parseedn-read-str (buffer-substring-no-properties (caar (region-bounds)) (cdar (region-bounds)))))
                         (parseedn-read)))
                (plist (me/hash-to-plist edn))
                (buffer (get-buffer-create "EdnToElisp")))
           (with-current-buffer buffer
             (emacs-lisp-mode)
             (erase-buffer)
             (prin1 plist buffer)
             (setq-local self plist))
           buffer))
 :docs "You can parse EDN format as an Elisp object."
 :examples nil)
