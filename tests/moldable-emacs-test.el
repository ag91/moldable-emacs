(require 'ert)
(require 'moldable-emacs)


(ert-deftest me-alist-to-lists-of-plist_convert-alist-to-plist ()
  (should
   (equal (me-alist-to-lists-of-plist '(("A" "b") (1 2) (3 4))) '((:A 1 :b 2) (:A 3 :b 4)))))

;; (ert-deftest me-alist-to-lists-of-plist_convert-alist-to-plist+1 ()
;;   (should
;;    (equal (me-alist-to-lists-of-plist '(("A" . "b") (1 . 2) (3 . 4))) '((:A 1 :b 2) (:A 3 :b 4)))))

(ert-deftest me-given_valid-with-buffer ()
  (should
   (equal
    (me-given '(:type buffer :name "some.txt" :contents "bla" :mode emacs-lisp-mode) (format "%s %s" (buffer-name) major-mode))
    "some.txt emacs-lisp-mode")))

(ert-deftest me-given_valid-with-buffer-param ()
  (should
   (equal
    (let ((x '(:type buffer :name "some.txt" :contents "bla" :mode emacs-lisp-mode))) (me-given x (format "%s %s" (buffer-name) major-mode)))
    "some.txt emacs-lisp-mode")))

(ert-deftest me-example-to-docstring_produce-doc-string ()
  (should
   (string=
    (me-example-to-docstring '(:given (:type buffer :name "somebuffer" :contents "some contents") :then (:type file :name "/tmp/somefile.txt" :contents "some new contents")))

    "

Given the \"somebuffer\" buffer with the following contents:

----------

some contents

----------

The mold returns the \"/tmp/somefile.txt\" file with the following contents:

----------

some new contents

----------"

    )))

(ert-deftest me-register-mold_new-mold () ;; TODO use this as a documentation mold example??
  (let ((me-available-molds nil))
    (me-register-mold
     :key "bla"
     :description "bla"
     :given (lambda () 't)
     :then (lambda () 't))
    (should
     (string=
      (plist-get (car me-available-molds) :key)
      "bla"))))

(ert-deftest me-focus-on-consistent-keys_return-only-shared-key-values ()
  (should
   (equal (me-focus-on-consistent-keys '((:a 1 :b 1 :c 1) (:a 2 :c 2))) '((:a 1 :c 1) (:a 2 :c 2)))))

(ert-deftest me-mold-treesitter-to-parse-tree_buffers ()
  ;; buffer is visiting a file
  (let ((tf (make-temp-file "moldable-emacs" )))
    (with-temp-file tf
      (insert "int i=0;"))

    (unwind-protect
        (with-temp-buffer
          (insert-file-contents tf t)
          (java-mode)
          (tree-sitter-mode)
          (let ((bn (buffer-name))
                (bfn (buffer-file-name)))
            (should
             (equal (me-mold-treesitter-to-parse-tree)
                    `((:type local_variable_declaration :text "int i=0;" :begin 1 :end 9 :buffer ,bn :buffer-file ,bfn)
                      (:type integral_type :text "int" :begin 1 :end 4 :buffer ,bn :buffer-file ,bfn)
                      (:type "int" :text "int" :begin 1 :end 4 :buffer ,bn :buffer-file ,bfn)
                      (:type variable_declarator :text "i=0" :begin 5 :end 8 :buffer ,bn :buffer-file ,bfn)
                      (:type identifier :text "i" :begin 5 :end 6 :buffer ,bn :buffer-file ,bfn)
                      (:type "=" :text "=" :begin 6 :end 7 :buffer ,bn :buffer-file ,bfn)
                      (:type decimal_integer_literal :text "0" :begin 7 :end 8 :buffer ,bn :buffer-file ,bfn)
                      (:type ";" :text ";" :begin 8 :end 9 :buffer ,bn :buffer-file ,bfn))))))
      (delete-file tf)))

  ;; buffer is not visiting a file
  (with-temp-buffer
    (insert "int i=0;")
    (java-mode)
    (tree-sitter-mode)
    (let ((bn (buffer-name)))
      (should
       (equal (me-mold-treesitter-to-parse-tree)
              `((:type local_variable_declaration :text "int i=0;" :begin 1 :end 9 :buffer ,bn :buffer-file nil)
                (:type integral_type :text "int" :begin 1 :end 4 :buffer ,bn :buffer-file nil)
                (:type "int" :text "int" :begin 1 :end 4 :buffer ,bn :buffer-file nil)
                (:type variable_declarator :text "i=0" :begin 5 :end 8 :buffer ,bn :buffer-file nil)
                (:type identifier :text "i" :begin 5 :end 6 :buffer ,bn :buffer-file nil)
                (:type "=" :text "=" :begin 6 :end 7 :buffer ,bn :buffer-file nil)
                (:type decimal_integer_literal :text "0" :begin 7 :end 8 :buffer ,bn :buffer-file nil)
                (:type ";" :text ";" :begin 8 :end 9 :buffer ,bn :buffer-file nil))))))
  )

(ert-deftest me-get-in_not-found-is-nil ()
  (should
   (eq (me-get-in '() '(:a :b)) nil)))

(ert-deftest me-get-in_finds-val ()
  (should
   (eq (me-get-in '(:a (:b something)) '(:a :b)) 'something)))


(ert-deftest me-make-org-table_make-simple-table ()
  (should
   (string= (me-make-org-table
             `(("A" .
                (:extractor (lambda (e) (number-to-string (plist-get e :a)))))
               ("B" .
                (
                 :extractor (lambda (e) (number-to-string (plist-get e :b)))
                 :handler (lambda (s)
                            (concat "hello " s "!")))))
             '((:a 1 :b 2)
               (:a 2 :b 3)))
            "| A | B | 
|--+--| 
| 1 | hello 2! |
| 2 | hello 3! |" )))


(ert-deftest me-find-missing-dependencies-for-mold_finds-nothing ()
  (should
   (equal (me-find-missing-dependencies-for-mold '(:key "test" :given (:fn (and t))))
          '(:key "test" :missing-dependencies nil))))


(ert-deftest me-find-missing-dependencies-for-mold_finds-nothing-for-existing-dependencies ()
  (should
   (equal (me-find-missing-dependencies-for-mold '(:key "test" :given (:fn (and (me-require 'org) (executable-find "sh")))))
          '(:key "test" :missing-dependencies nil))))

(ert-deftest me-find-missing-dependencies-for-mold_finds-requires-and-executables ()
  (should
   (equal (me-find-missing-dependencies-for-mold '(:key "test" :given (:fn (and (me-require 'some-package) (executable-find "some-command")))))
          '(:key "test" :missing-dependencies ((me-require 'some-package) (executable-find "some-command"))))))

(ert-deftest me-find-missing-dependencies-for-mold_finds-nested-requires-and-executables ()
  (should
   (equal (me-find-missing-dependencies-for-mold '(:key "test" :given (:fn (or (or (me-require 'some-package) t) (and t (executable-find "some-command"))))))
          '(:key "test" :missing-dependencies ((me-require 'some-package) (executable-find "some-command"))))))

(ert-deftest me-interpret-then_expand-then ()
  (should
   (equal (me-interpret-then '(:then (:fn 'some-body)))
          '(progn (get-buffer-create buffername) 'some-body (ignore-errors (switch-to-buffer-other-window (get-buffer buffername)))))))

(ert-deftest me-interpret-then_expand-async ()
  (should
   (equal (let ((load-path '("some-load-path")))
            (me-interpret-then '(:key "Test" :then (:async ((bind1 'slow-binding)  (bind2 'slow-binding)) :fn 'some-body))))

          '(let
               ((_
                 (async-let
                     ((bind1 'slow-binding)
                      (bind2 'slow-binding))
                   (progn 'some-body
                          (ignore-errors
                            (switch-to-buffer-other-window
                             (get-buffer buffername)))))))
             (get-buffer-create buffername)
             (with-current-buffer buffername
               (erase-buffer)
               (insert
                (format "Loading %s contents..." "Test")))))))

(ert-deftest me-with-mold-let-evals ()
  (let ((mold '(
                :key "MissingExecutable"
                :given (:fn (and
                             ;; (progn (message "me-usable-molds-requiring-deps-1:major-mode=%s"
                             ;;                 major-mode) t)
                             ;; (executable-find "executable-does-not-exist")
                             (executable-find "sh")
                             (eq major-mode 'fundamental-mode)))
                :then (:fn
                       (switch-to-buffer buffername)
                       (kill-buffer-and-window))
                :docs "Test failing :given")))
    (should
     (equal (me-with-mold-let mold :given) t))
    (should
     (equal (buffer-name (me-with-mold-let mold :then)) "*moldable-emacs-MissingExecutable*"))
    (should
     (equal (me-with-mold-let mold (funcall (lambda () (+ 2 4)))) 6))
    ))

(ert-deftest me-usable-molds-requiring-deps-1 ()
  (let ((mold '(
                :key "NoMissingExecutable"
                :given (:fn (and
                             (executable-find "sh")
                             (eq major-mode 'fundamental-mode)))
                :then (:fn
                       (switch-to-buffer buffername)
                       (kill-buffer-and-window))
                :docs "Test failing :given")))
      (should
       (equal (me-usable-molds-requiring-deps-in (list mold)) nil))
      ))

(ert-deftest me-usable-molds-requiring-deps-2 ()
  (let ((mold '(
                :key "MissingExecutable"
                :given (:fn (and
                             (executable-find "sh-does-not-exist")
                             (eq major-mode 'fundamental-mode)))
                :then (:fn
                       (switch-to-buffer buffername)
                       (kill-buffer-and-window))
                :docs "Test failing :given")))
    (should
     ;; AZ: I would expect it to return (list molds). What am I missing?
     (equal (me-usable-molds-requiring-deps-in (list mold)) nil))
    ))


(ert-deftest me-keys_return-keys-of-plist ()
  (should
   (equal (me-keys '(:a 1 :b 2 :c 3)) '(:a :b :c))))


(ert-deftest me-org-table-to-plist_get-plist ()
  (should
   (equal
    (me-org-table-to-plist "| a | b |\n|1|2|\n|3|4|")
    '(:a ("1" "3") :b ("2" "4")) )))

(ert-deftest me-org-table-to-flat-plist_get-plists ()
  (should
   (equal
    (me-org-table-to-flat-plist "| a | b |\n|1|2|\n|3|4|")
    '((:a "1" :b "2") (:a "3" :b "4")))))

(ert-deftest me-flat-org-table-to-string_get-string ()
  (should
   (string= (me-flat-org-table-to-string '((:a "1" :b "2") (:a "3" :b "4")))
            "| a | b | 
|--+--| 
| 1 | 2 |
| 3 | 4 |" )))
