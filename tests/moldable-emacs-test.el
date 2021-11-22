(require 'ert)
(require 'moldable-emacs)


(ert-deftest me-alist-to-plist_convert-alist-to-plist ()
  (should
   (equal (me-alist-to-plist '(("A" "b") (1 2) (3 4))) '((:A 1 :b 2) (:A 3 :b 4)))))

;; (ert-deftest me-alist-to-plist_convert-alist-to-plist+1 ()
;;   (should
;;    (equal (me-alist-to-plist '(("A" . "b") (1 . 2) (3 . 4))) '((:A 1 :b 2) (:A 3 :b 4)))))

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
