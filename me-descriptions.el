;;; me-descriptions.el --- Deterministic descriptions of code constructs -*- lexical-binding: t; -*-

;; This file provides human-readable descriptions of tree-sitter node
;; types. Each description function takes a parse-tree node (a plist
;; with :type, :text, :begin, :end, :mode, etc.) and returns a
;; human-readable string. Descriptions are deterministic — they encode
;; human knowledge about what constructs mean, not AI inference.

(defvar me-descriptions
  '((function_definition . me-describe-function)
    (function_declaration . me-describe-function)
    (class_definition . me-describe-class)
    (class_declaration . me-describe-class)
    (import_statement . me-describe-import)
    (import_from_statement . me-describe-import)
    (call_expression . me-describe-call)
    (if_statement . me-describe-if)
    (for_statement . me-describe-for)
    (for_each_statement . me-describe-for)
    (while_statement . me-describe-while)
    (return_statement . me-describe-return)
    (variable_declaration . me-describe-variable)
    (variable_declarator . me-describe-variable)
    (assignment . me-describe-assignment)
    (method_definition . me-describe-method)
    (method_declaration . me-describe-method)
    (defmethod . me-describe-method)
    (interface_declaration . me-describe-interface)
    (enum_declaration . me-describe-enum)
    (try_statement . me-describe-try)
    (switch_statement . me-describe-switch)
    (comment . me-describe-comment))
  "Alist mapping tree-sitter node types to description functions.
Each function takes a parse-tree node plist and returns a string.")

(defun me-describe (node)
  "Get a human-readable description for NODE.
NODE is a plist from `me-to-parse-tree' with :type, :text, etc.
Returns nil if no description is available."
  (let* ((type (plist-get node :type))
         (fn (alist-get type me-descriptions)))
    (when fn
      (funcall fn node))))

(defun me--node-text (node)
  "Get the text of NODE, trimmed."
  (s-trim (or (plist-get node :text) "")))

(defun me--extract-name (node)
  "Extract a name from NODE's text.
For function/class definitions, the name is typically the first
identifier after the keyword."
  (let ((text (me--node-text node)))
    (->> text
         (s-split "[ \t\n]+")
         (-drop 1)
         (-first (lambda (s) (s-present-p s)))
         (s-replace "(" ""))))

(defun me--count-params (node)
  "Count parameters in NODE's text.
Looks for parentheses and counts comma-separated items."
  (let ((text (me--node-text node)))
    (if (string-match "(" text)
        (let ((params (substring text (match-end 0))))
          (if (string-match ")" params)
              (let ((param-str (substring params 0 (match-beginning 0))))
                (if (s-blank-p (s-trim param-str))
                    0
                  (length (s-split "," param-str t))))
            0))
      0)))

(defun me-describe-function (node)
  "Describe a function definition NODE."
  (let ((name (or (me--extract-name node) "anonymous"))
        (params (me--count-params node)))
    (format "Function %s takes %d parameter%s and returns a value"
            name params (if (= params 1) "" "s"))))

(defun me-describe-method (node)
  "Describe a method definition NODE."
  (let ((name (or (me--extract-name node) "anonymous"))
        (params (me--count-params node)))
    (format "Method %s takes %d parameter%s and is called on an object"
            name params (if (= params 1) "" "s"))))

(defun me-describe-class (node)
  "Describe a class definition NODE."
  (let ((name (or (me--extract-name node) "anonymous")))
    (format "Class %s defines a new type with its own methods and attributes"
            name)))

(defun me-describe-interface (node)
  "Describe an interface definition NODE."
  (let ((name (or (me--extract-name node) "anonymous")))
    (format "Interface %s defines a contract that implementing classes must fulfill"
            name)))

(defun me-describe-enum (node)
  "Describe an enum definition NODE."
  (let ((name (or (me--extract-name node) "anonymous")))
    (format "Enum %s defines a fixed set of named constants"
            name)))

(defun me-describe-import (node)
  "Describe an import statement NODE."
  (let ((text (me--node-text node)))
    (format "Import statement bringing external code into scope: %s"
            (s-trim text))))

(defun me-describe-call (node)
  "Describe a function call NODE."
  (let ((text (me--node-text node)))
    (if (string-match "^\\([^( \t\n]+\\)" text)
        (format "Call to function %s" (match-string 1 text))
      "Function call")))

(defun me-describe-if (node)
  "Describe an if statement NODE."
  "Conditional branch: executes a block only if a condition is true")

(defun me-describe-for (node)
  "Describe a for/for-each statement NODE."
  "Loop: iterates over a collection, executing a block for each item")

(defun me-describe-while (node)
  "Describe a while statement NODE."
  "Loop: repeats a block as long as a condition remains true")

(defun me-describe-return (node)
  "Describe a return statement NODE."
  "Return statement: sends a value back to the caller")

(defun me-describe-variable (node)
  "Describe a variable declaration NODE."
  (let ((text (me--node-text node)))
    (format "Variable declaration: %s" (s-trim text))))

(defun me-describe-assignment (node)
  "Describe an assignment NODE."
  (let ((text (me--node-text node)))
    (if (string-match "^\\([^ \t\n=]+\\)" text)
        (format "Assignment: sets %s to a value" (s-trim (match-string 1 text)))
      "Assignment")))

(defun me-describe-try (node)
  "Describe a try statement NODE."
  "Error handling: attempts an operation and catches exceptions if they occur")

(defun me-describe-switch (node)
  "Describe a switch statement NODE."
  "Multi-way branch: selects one of several code paths based on a value")

(defun me-describe-comment (node)
  "Describe a comment NODE."
  "Comment: human-readable note in the code that is not executed")

(provide 'me-descriptions)
;;; me-descriptions.el ends here
