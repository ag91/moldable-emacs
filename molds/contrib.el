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
                    (require 'esxml)
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
 :given (lambda () (and (executable-find "graph") (eq major-mode 'csv-mode)))
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
 :given (lambda () (and (executable-find "graph") (eq major-mode 'csv-mode)))
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
 :given (lambda () (and (me/by-type 'function_definition (me/mold-treesitter-to-parse-tree)) (require 'code-compass)))
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
                (buffer (get-buffer-create (me/append-time "dot-"))))
           (with-current-buffer buffer
             (erase-buffer)
             (insert (me/diagram-to-dot-string diagram))
             (setq-local self tables))
           buffer)))

(me/register-mold ;; https://orgmode.org/worg/org-tutorials/org-dot-diagrams.html
 :key "DotToPicture"
 :given (lambda () (and
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
