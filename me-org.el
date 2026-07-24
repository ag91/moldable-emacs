;;; me-org.el --- Utilties for org  -*- lexical-binding: t; -*-

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
;;; Utilties for org-mode.

;;; Code:
(require 'dash)
(require 's)
(require 'me-utils)
(require 'org)

(defmacro me-with-org-parent-heading (&rest body)
  "Execute BODY with narrowing on the upmost parent heading if it exists."
  `(save-excursion
     (while (org-up-heading-safe))
     (with-demoted-errors (org-narrow-to-subtree))
     (condition-case err
         (let ((r__ (progn ,@body)))
           (widen)
           r__)
       (error
        (widen)
        (error err)))
     ))

(defun me-make-org-table (headlines objects)
  "Make an Org Table with OBJECTS formats and HEADLINES.

>> (me-make-org-table '((\"a\" . (:extractor identity)) (\"b\" . (:extractor identity :handler (lambda (x) (concat x \"hi\"))))) '(\"1\" \"2\"))
=> \"| a | b |
|--+--|
| 1 | 1hi |
| 2 | 2hi |\""
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

(defun me-format-org-table ()
  "Format table."
  (save-excursion
    (ignore-errors
      (search-backward "|" nil nil 2)
      ;; go line above to not add an empty row
      (beginning-of-line -1))
    (org-cycle)))

(defun me-insert-string-table (table-string)
  "Insert TABLE-STRING in buffer.
Make sure table is also indented."
  (insert table-string)
  (me-format-org-table)
  (setq-local org-confirm-elisp-link-function nil))

(defun me-insert-org-table (headlines objects)
  "Produce org table of OBJECTS formatting with HEADLINES."
  (me-insert-string-table (me-make-org-table headlines objects)))

(defun me-org-table-as-alist-to-plist (alist)
  "Convert ALIST to a `plist'.
>> (me-org-table-as-alist-to-plist '((\"a\" \"b\" \"c\") (\"1\" \"2\" \"3\")))
=> ((:a \"1\" :b \"2\" :c \"3\"))"
  (let ((keys (ignore-errors
                (and (= (length (car alist)) (length (-filter #'stringp (car alist))))
                     (--map (intern (concat ":" it)) (car alist))))))
    (if keys
        (--map (-flatten (-zip-lists keys it)) (cdr alist))
      alist)))

(defun me-plist-org-table-to-table-with-headings (&optional org-table-lisp)
  "Transform a table obtained from a plist (ORG-TABLE-LISP),
so with keyword entries, into a org table with headings.

>> (me-plist-org-table-to-table-with-headings '((\":a\" \"9\" \":b\" \"8\")))
=> \"|a|b|
|9|8|\""
  (let ((lisp-table (or org-table-lisp (org-table-to-lisp))))
    (--> lisp-table
         (--map
          (--> (-map 'substring-no-properties it)
               (--remove (s-starts-with-p ":" it) it))
          it)
         (cons (--keep (and (s-starts-with-p ":" it) (s-replace ":" "" (substring-no-properties it))) (car lisp-table)) it)
         (--map (format "|%s|" (s-join "|" it)) it)
         (s-join "\n" it))))

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
    (when (equal major-mode 'org-mode)
      (with-current-buffer (or buffer (current-buffer)) ;; TODO remove org links in table!
        (me-with-org-parent-heading
         (re-search-forward org-table-line-regexp nil t)
         (me-org-tabletolisp-to-plist (org-table-to-lisp)))))))

(defun me-all-flat-org-tables (&optional buffer whole-buffer)
  "Find org tables within current headline or in whole buffer if no headline found.
Optionally in input BUFFER. Search in WHOLE-BUFFER, if t."
  (ignore-errors
    (with-current-buffer (or buffer (current-buffer)) ;; TODO remove org links in table!
      (me-with-org-parent-heading
       (when whole-buffer
         (goto-char 0)
         (widen))
       (let (result)
         (while (and
                 (re-search-forward org-table-line-regexp nil t)
                 (goto-char (- (org-table-end) 1)))
           (setq result
                 (cons (me-org-tabletolisp-to-plist (org-table-to-lisp))
                       result)))
         result)))))

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
  ;; https://emacs.stackexchange.com/questions/10707/in-org-mode-how-to-remove-a-link
  (if text
      (with-temp-buffer
        (org-mode)
        (insert text)
        (goto-char (point-min))
        (me-replace-org-links-with-descriptions)
        (buffer-substring-no-properties (point-min) (point-max)))
    (while (eq (org-next-link) 't)
      (me-org-replace-link-by-link-description))))

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

(defun me-org-ql-to-org-transclusion (org-ql-headlines)
  "Transform ORG-QL-HEADLINES into something manageable by `org-transclusion'.

>> (me-org-ql-to-org-transclusion '((headline (:ID \"some-id\" :raw-value \"some heading\"))))
=> (\"#+transclude: [[id:some-id][some heading]]

\")"
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
        "#+TRANSCLUDE: [[id:%s][%s]]\n\n"
        (org-roam-node-id (org-roam-backlink-source-node backlink))
        (org-roam-node-title (org-roam-backlink-source-node backlink)))))


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
                          (list (or (search-forward name nil 'noerror) 1) target)))
                     (or
                      (and (plist-get target :begin) (list (plist-get target :begin) (plist-get target :buffer)))
                      (save-excursion
                        (with-current-buffer target
                          (goto-char (point-min))
                          (list (or (search-forward name nil 'noerror) 1) target)))))))
    (me-make-elisp-file-link
     (s-replace "\n" "" name)
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
                (or (search-forward (if (s-contains-p "\"" name) (prin1-to-string name) name) nil 'noerror) 1))))
    (me-make-elisp-file-link
     name
     (format
      "(progn (switch-to-buffer-other-window \"%s\") (goto-char %s))"
      buffer-name
      pos)
     "elisp")))

(provide 'me-org)
;;; me-org.el ends here
