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
