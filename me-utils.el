;;; moldable-emacs.el --- Moldable Development Extension -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Andrea

;; Author: Andrea <andrea-dev@hotmail.com>
;; Version: 20211115-snapshot
;; URL: https://github.com/ag91/moldable-emacs
;; Package-Requires: ((emacs "26.1") (dash "2.19.1") (s "1.12.0") (async "1.9.4"))
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
;;; Utilities for moldable-emacs.

;;; Code:
(require 'dash)
(require 's)

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
(put 'me-with-file 'lisp-indent-function 1)

(defun me-async-map--finish (futures post-fn too-late-p poll-time)
  "Run FUTURES and apply POST-FN on their results.
Use TOO-LATE-P and POLL-TIME to stop."
  (if (not (-some #'null (mapcar #'async-ready futures)))
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
  "Run FN async on elements ELS.
Optionally define a POST-FN to run on the results of apply FN on ELS.
Optionally define a POLL-TIME to look for results and a TIMEOUT to fail."
  (let* ((start (current-time))
         (futures (mapcar
                   (lambda (el)
                     (async-start (lambda ()
                                    (setq load-path load-path)
                                    (funcall fn el))))
                   els))
         (too-late-p
          (lambda () (>= (time-to-seconds (time-since start)) (or timeout 300)))))
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
  "Run FN in parallel on elements ELS.
Optionally define a POST-FN to run on the results of apply FN on ELS.
Optionally define a POLL-TIME to look for results and a TIMEOUT to fail."
  (let* ((start (current-time))
         (futures (mapcar
                   (lambda (el)
                     (async-start `(lambda ()
                                     (setq load-path ',load-path)
                                     (funcall ,fn (if ,(seqp el) ',el ,el)))))
                   els))
         (too-late-p
          `(lambda () (>= (time-to-seconds (time-since ',start)) (or ,timeout 300)))))
    (while (-some #'null (mapcar #'async-ready futures))
      (when (funcall too-late-p) (error "Me-pmap has waited too long: timed out"))
      (sleep-for (or poll-time 0.2)))
    (--map
     (let ((buf (process-buffer it)))
       (with-current-buffer buf
         (async-handle-result
          #'identity
          async-callback-value
          (current-buffer))))
     futures)))

;; experimental: only for high latency (not high CPU) network like operations
(defun me-pmap-threaded (fn els &optional num-threads)
  "Parallel map FN over ELS using Emacs Lisp threads.
FN is a function of one argument. ELS is a list.
NUM-THREADS defaults to min(length ELS, number of CPU cores or 8)."
  (let* ((n (length els))
         (num-threads (or num-threads
                          (min n (max 1 (or (ignore-errors (string-to-number (getenv "NPROC"))) 8)))))
         (idx 0)
         (idx-mutex (make-mutex))
         (results (make-vector n nil))
         (threads '()))
    ;; spawn workers
    (dotimes (_ num-threads)
      (push
       (make-thread
        (lambda ()
          (while t
            ;; get next index
            (let ((i n))
              (mutex-lock idx-mutex)
              (unwind-protect
                  (progn
                    (if (>= idx n)
                        (setq i nil)
                      (setq i idx)
                      (setq idx (1+ idx))))
                (mutex-unlock idx-mutex))
              (if (null i)
                  (cl-return-from nil) ;; no more work, exit thread
                (let* ((el (nth i els))
                       (res (condition-case err
                                (funcall fn el)
                              (error (list :error (format "%S" err))))))
                  (aset results i res)))
              (setq i n)))))
       threads))
    ;; join threads
    (dolist (th threads)
      (thread-join th))
    ;; return results as list
    (let (out)
      (dotimes (i n)
        (push (aref results i) out))
      (nreverse out))))


(defun me-format-iso8601-time (time)
  "Format TIME to ISO8601.
-- taken from http://xahlee.info/emacs/emacs/elisp_datetime.html."
  (concat
   (format-time-string "%Y-%m-%dT%T" time)
   ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
    (format-time-string "%z" time))))

(defun me-print-to-buffer (object &optional buffer)
  "Print OBJECT in BUFFER without truncation."
  (let ((print-length nil)
        (eval-expression-print-length nil))
    (pp-display-expression object (or buffer (current-buffer)))))


;; plist begin
(defun me-plistp (list)
  "Tell if LIST is a property list.
>> (me-plistp '(:a 1 :b 2))
=> t

>> (me-plistp '(:a 1 :b))
=> nil

>> (me-plistp '(1))
=> nil"
  (let ((evenp (lambda (x) (= 0 (mod x 2))))
        (all-keys t))
    (and
     (listp list)
     (funcall evenp (length list))
     (= (/ (length list) 2)
        (length
         (--keep
          (and
           (funcall evenp it-index)
           (setq
            all-keys
            (and
             all-keys
             (symbolp it)
             (s-starts-with-p
              ":"
              (symbol-name it))))
           it)
          list))
        )
     all-keys)))

(defun me-alist-to-plist (alist)
  "Convert ALIST to a `plist'.
>> (me-alist-to-plist '((a . 1) (b . 2)))
=> (:a 1 :b 2)

>> (me-alist-to-plist '((:a . 1) (:b . 2)))
=> (:a 1 :b 2)

>> (me-alist-to-plist '((\"a\" . 1) (\"b\" . 2)))
=> (:a 1 :b 2)"
  (when (-every? #'consp alist)
    (-flatten
     (--map
      (list
       (intern
        (s-replace
         "\""
         ""
         (let ((key (prin1-to-string (car it))))

           (if (s-starts-with-p ":" key)
               key
             (concat ":" key)))))
       (cdr it))
      alist))))

(defun me-hash-to-plist (hash-table)
  ;; from http://ergoemacs.org/emacs/elisp_hash_table.html (this is a recursive version)
  "Produce a plist from the HASH-TABLE (recursively).

>> (me-hash-to-plist #s(hash-table size 30 data (key1 val1 key2 300)))
=> (:key1 val1 :key2 300)"
  (let (result)
    (maphash
     (lambda (k v)
       (push (list (intern (format ":%s" k))
                   (if (hash-table-p v) (me-hash-to-plist v) v))
             result))
     hash-table)
    (-flatten-n 1 (reverse result))))


(defun me-get-in (plist keys)
  "Navigate PLIST's KEYS in sequence.
For example, (me-get-in '(:a (:b (:c 1))) '(:a :b :c)) yields 1.

>> (me-get-in '(:a (:b 1)) '(:a :b))
=> 1

>> (me-get-in '(:a (:b 1)) '(1 :b))
=> 1

>> (me-get-in '((a . ((b . 1)))) '(a b))
=> 1

>> (me-get-in '(:a (1 2 3)) '(:a))
=> (1 2 3)
"
  (let ((access
         (lambda (key list)
           (if (me-plistp plist)
               (plist-get list key)
             (alist-get key list)))))
    (--reduce-from
     (if (numberp it)
         (nth it acc)
       (funcall access it acc))
     plist
     keys)))

(defun me-plist-focus (plist keys)
  "Focus only on KEYS of PLIST.
For example, (me-plist-focus '(:a a :b b :c c) '(:a :c)) => '(:a a :c c)."
  (-flatten (--map (list it (plist-get plist it)) keys)))
(defalias 'me-select-keys 'me-plist-focus)

(defun me-focus-on-consistent-keys (list-of-plist)
  "Focus on common keys of LIST-OF-PLIST.
For example ((:a 1 :b 1 :c 1) (:a 2 :c 2)) becomes ((:a 1 :c 1) (:a 2 :c 2)).
This is useful for plotting."
  (let ((keys (-reduce '-intersection (--map (-filter 'symbolp it) list-of-plist))))
    (--map (me-plist-focus it keys) list-of-plist)))

(defun me-keys (map)
  "Return keys of PLIST and ALIST.

>> (me-keys '((a . 1) (b . 2)))
=> (a b)

>> (me-keys '(:a 1 :b 2))
=> (:a :b)

>> (let* ((h (make-hash-table)) (_ (puthash :a 1 h)) (_ (puthash :b 2 h))) (me-keys h))
=> (:b :a)"
  (cond
   ((hash-table-p map) (hash-table-keys map))
   ((me-plistp map) (--filter (and (symbolp it) (s-starts-with-p ":" (symbol-name it))) map))
   (t (-map 'car map))))

(defun me-merge (join-when-you-can? &rest plists)
  "Merge keys of PLISTS when possible.
If JOIN-WHEN-YOU-CAN? is true, if keys contain lists,
 we append their results instead of replacing.

>> (me-merge t '(:a (\"1\") :b \"2\") '(:a (\"3\") :b \"3\"))
=> (:a (\"1\" \"3\") :b \"3\")"
  (--reduce
   (-reduce-from
    (lambda (acc1 key)
      (let ((a (plist-get acc key))
            (b (plist-get it key)))
        (if (and join-when-you-can? (listp a) (listp b))
            (append acc1 (list key (-union a b)))
          (append acc1 (list key b)))))
    nil
    (-union (me-keys it) (me-keys acc)))
   plists))

(defun me-plist-to-csv-string (plist)
  "Make PLIST into a CSV string."
  (let ((keys (me-keys (car plist))))
    (concat
     ;; header
     (s-join "," (--map (s-chop-left 1 (symbol-name it)) keys))
     ;; entries
     "\n"
     (--> plist
          (--map
           (s-join "," (--map (format (if (and (stringp it) (s-contains-p "," it)) "\"%s\"" "%s") it) (-remove 'symbolp (me-select-keys it keys))))
           it)
          (s-join "\n" it)))))

(defun me-heatmap (plists-list intervals)
  "Insert a heatmap as an org table, given a PLISTS-LIST and INTERVALS.
Example:
  (me-heatmap '((:a 1 :b 2 :c 3)
                (:a 2 :b 8 :c 10))
              '(3 5 8))"
  (me-insert-org-table
   (--map
    (cons (symbol-name it)
          `(:extractor
            (lambda (obj) (plist-get obj ,it))
            :handler
            (lambda (number)
              (let* ((color
                      (cond ((>= number (nth 0 intervals)) "red")
                            ((>= number (nth 1 intervals)) "orange")
                            ('otherwise "green"))))
                (me-color-string (number-to-string number) color)))))
    (me-keys (car plists-list)))
   plists-list))
;; plist end

(defun me-get-region ()
  "Get the active region's string."
  (when (region-active-p)
    (substring-no-properties (funcall region-extract-function))))

(defun me-stats (number-list)
  "Calculate some basic stats on NUMBER-LIST."
  (let ((mean (/ (-sum number-list) (length number-list))))
    (list :mean mean
          :median (nth (/ (+ (length number-list) 1) 2) (--sort (> it other) number-list))
          :min (-min number-list)
          :max (-max number-list)
          :standard-deviation (sqrt (/ (-sum (--map (expt (- it mean) 2) number-list)) (length number-list)))
          ;; TODO percentiles
          )))

;; begin urls collection
(defun me-re-seq (regexp string)
  "Get a list of all REGEXP matches in a STRING."
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      matches)))

(defun me-re-url-seq (string)
  "Get a list of all urls in STRING."
  (let ((urlreg "https?://\\(www\\)?\\(?:[./#\+-]?\\w*\\)+"))
    (me-re-seq urlreg string)))

(defun me-urls-in-clipboard ()
  "Get a list of all urls in the kill ring head."
  (let (text)
    (with-temp-buffer
      (clipboard-yank)
      (setq text (buffer-string)))
    (reverse (me-re-url-seq text))))

(defun me-urls-in-region ()
  "Get a list of all urls in region."
  (reverse (me-re-url-seq (when (region-active-p)
                            (buffer-substring-no-properties
                             (caar (region-bounds))
                             (cdar (region-bounds)))))))
;; end urls collection

;; syntax highlighting

(defun me-highlight-node (node)
  "Highlight NODE in its buffer."
  (with-current-buffer (get-buffer-create (plist-get node :buffer)) ; TODO handle :buffer-file
    (let* ((node-start (plist-get node :begin))
           (node-end (plist-get node :end))
           (overlay (make-overlay node-start node-end))
           (capture-name (or (ignore-errors (symbol-name (plist-get node :type)))
                             (plist-get node :type))))
      ;; Ensure the overlay is deleted when it becomes empty.
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'face '(:background "Green"))
      ;; Use the capture's name as the mouseover tooltip.
      (unless (string= capture-name "")
        (overlay-put overlay 'help-echo capture-name)))))

(defun me-highlight-nodes (nodes)
  "Highlight NODES in their buffer."
  (-each nodes 'me-highlight-node))


(defun me-insert-follow-overlay (node-to-overlay nodes)
  "Link NODE-TO-OVERLAY and NODES with an overlay executing when cursor touches the area of NODE-TO-OVERLAY."
  (cursor-sensor-mode 1)
  (let ((old-buffer (plist-get (car nodes) :buffer))
        (ov (make-overlay
             (plist-get node-to-overlay :begin)
             (plist-get node-to-overlay :end))))
    (overlay-put
     ov
     'cursor-sensor-functions
     (list `(lambda (affected-window old-position entered-or-left)
              (cond
               ((eq entered-or-left 'entered)
                (progn (overlay-put ,ov 'face '(:background "Green"))
                       (-each ',nodes 'me-highlight-node)))
               ((eq entered-or-left 'left)
                (progn (overlay-put ,ov 'face nil)
                       (with-current-buffer ,old-buffer
                         (remove-overlays))))))))))

(defun me-syntax-description (type language)
  "Get description for node of TYPE and LANGUAGE."
  (or
   ;;  TODO I should generalize this to add descriptions on demand (in particular if I am going to define my own types)
   (plist-get
    (--find (equal (plist-get it :label) (or
                                          (ignore-errors (symbol-name type))
                                          type))
            nil ;; me-natural-syntax-tree-labels - TODO not shared yet
            )
    :description)
   (format "[[elisp:(browse-web \"%s %s\")][Search for description]]" language type)))

;; keybindings
(defun me-override-keybiding-in-buffer (key command)
  "Override KEY with COMMAND in buffer."
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))

;; deps

(defun me-usable-molds-requiring-deps ()
  "Find molds that require dependencies to run."
  (me-usable-molds-requiring-deps-in me-available-molds))

(defun me-usable-molds-requiring-deps-in (molds-alist)
  "Find molds in MOLDS-ALIST that require dependencies to run."
  (--remove
   (let ((mold it)
         (given-cond (me-get-in it '(:given :fn))))
     (ignore-errors
       (and
        (> (length given-cond) 1)
        (eq (car given-cond) 'and)
        (me-with-mold-let mold
          (funcall
           (lambda ()
             (eval (cons 'and (--remove
                               (or
                                (and
                                 (seqp it)
                                 (-contains? it 'executable-find))
                                (and
                                 (seqp it)
                                 (-contains? it 'me-require)))
                               (cdr (me-get-in mold '(:given :fn))))))))))))
   molds-alist))

(defun me-find-missing-dependencies-for-mold (mold)
  "List unmet dependencies by MOLD."
  (let* ((flatten-given (-flatten (me-get-in mold '(:given :fn)))) ;; TODO this will break if I add other keywords than :fn
         (executables (--> flatten-given
                           (--find-indices (eq it 'executable-find) it)
                           (--map (list (nth it flatten-given) (nth (+ 1 it) flatten-given)) it)
                           (--remove (eval it) it)))
         (requires (--> flatten-given
                        (--find-indices (eq it 'me-require) it)
                        (--map (list (nth it flatten-given) `(quote ,(nth (+ 2 it) flatten-given))) it)
                        (--remove (eval it) it))))
    (list
     :key (plist-get mold :key)
     :missing-dependencies
     (append requires executables))))

(defun me-find-missing-dependencies-for-molds (molds)
  "List unmet dependencies by MOLDS."
  (-map
   #'me-find-missing-dependencies-for-mold
   molds))

(provide 'me-utils)
;;; me-utils.el ends here
