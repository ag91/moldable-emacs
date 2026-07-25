;;; me-analysis.el --- Code analysis utilities -*- lexical-binding: t; -*-


;; Copyright (C) 2026 Andrea

;; Author: Andrea <andrea-dev@hotmail.com>
;; URL: https://github.com/ag91/moldable-emacs
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
;;; Code analysis utilities

;;; Code:
(require 'dash)
(require 's)
(require 'me-utils)
(require 'me-tree)


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
  "Filter TREE entries by any of the TYPES.

>> (me-by-types '(a b) '((:type a :text \"hi\") (:type b)))
=> ((:type a :text \"hi\") (:type b))"
  (--filter (-contains? types (plist-get it :type)) tree))

(defun me-by-node-text (pred tree)
  "Filter TREE entries by a PRED on node text.

>> (me-by-node-text (lambda (it) (equal \"hi\" it)) '((:type a :text \"hi\") (:type b)))
=> ((:type a :text \"hi\"))"
  (--filter (funcall pred (plist-get it :text)) tree))

(defun me-count-by-key (key list)
  "Group LIST by KEY and count groups.

>> (me-count-by-key :a '((:a \"x\") (:a \"x\") (:a \"y\")))
=> ((:a \"x\" :count 2) (:a \"y\" :count 1))"
  (--> list
       (--group-by (plist-get it key) it)
       (--map (list key (car it) :count (length (cdr it))) it)
       (--sort (> (plist-get it :count) (plist-get other :count)) it)))

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

(defun me-remove-node (node)
  "Remove NODE from :buffer or :buffer-file using :begin and :end as anchors."
  (let ((begin (plist-get node :begin))
        (end (plist-get node :end))
        (buffer (plist-get node :buffer))
        (file (plist-get node :buffer-file)))
    (with-current-buffer buffer
      (delete-region begin end))))

(defun me-add-node (node)
  "Add NODE to :buffer or :buffer-file using its :begin position as an anchor."
  (let ((begin (plist-get node :begin))
        (text (plist-get node :text))
        (buffer (plist-get node :buffer)))
    (with-current-buffer buffer
      (goto-char begin)
      (insert text))))

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

(defun me-transit-node-buffer (node target-buffer &optional point)
  "Create a transition changing buffer's NODE to TARGET-BUFFER."
  (list
   :before node
   :after (plist-put
           (plist-put
            (plist-put
             (-copy node)
             :buffer
             target-buffer)
            :begin
            (or point
                ;; default to the last position in target buffer since `me-add-node' adds using :begin
                (with-current-buffer target-buffer (point-max))))
           :text
           ;; the definitions don't get the final newline, we add one ahead
           (concat "\n" (plist-get node :text)))))

(defun me-transit-node-buffers (nodes target-buffer &optional point)
  "Create transitions moving NODES to TARGET-BUFFER."
  (--map (me-transit-node-buffer it target-buffer point) nodes))

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
           (ignore-errors (me-filepath-to-flattened-tree filename)) ; sometimes there is an encoding issue with this that I can fix me-to-parse-tree
           (ignore-errors (me-with-file filename
                            ;; to store the :buffer in the nodes we need the buffer open, otherwise me-transit-* wouldn't work, it relies on buffers
                            (me-to-parse-tree)))))
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
(provide 'me-analysis)
;;; me-analysis.el ends here
