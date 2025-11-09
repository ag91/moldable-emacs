;;; moldable-emacs-transient.el --- Transient menus for moldable-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Moldable Emacs Transient Integration
;; Keywords: tools, convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides transient menu interfaces for moldable-emacs,
;; improving ergonomics by grouping related mold operations into
;; hierarchical menus similar to Magit.
;;
;; The menu uses a two-level hierarchy:
;; 1. Main menu shows categories
;; 2. Category submenus show specific mold operations
;;
;; Operations stay in the menu by default for easy chaining.
;; Only applicable molds (based on :given predicates) are shown.

;;; Code:

(require 'transient)
(require 'moldable-emacs)

;;; Configuration

(defcustom me-transient-menu-key "C-c m"
  "Key binding for invoking the moldable-emacs operations menu."
  :type 'string
  :group 'moldable-emacs)

;;; Helper Functions

(defun me--mold-usable-p (mold-key)
  "Check if mold with MOLD-KEY is currently usable.
Uses the mold's :given predicate."
  (when-let ((mold (me-find-mold mold-key)))
    (condition-case nil
        (me-interpret-given mold)
      (error nil))))

(defun me--has-any-molds-usable (mold-keys)
  "Check if any mold in MOLD-KEYS list is currently usable."
  (--any (me--mold-usable-p it) mold-keys))

(defmacro me--define-mold-suffix (suffix-name mold-key &optional transient-state docstring)
  "Define a transient suffix command for a mold.
SUFFIX-NAME is the command name (symbol).
MOLD-KEY is the mold's :key value (string).
TRANSIENT-STATE determines if menu stays open (default: t).
DOCSTRING is optional documentation."
  `(transient-define-suffix ,suffix-name ()
     ,(or docstring (format "Run %s mold." mold-key))
     :transient ,(if (eq transient-state nil) nil t)
     :if (lambda () (me--mold-usable-p ,mold-key))
     (interactive)
     (me-mold ,mold-key)))

;;; Piper Operations Suffixes

(me--define-mold-suffix me--mold-piper-keep-lines "piper-KeepLines" t)
(me--define-mold-suffix me--mold-piper-delete-lines "piper-DeleteLines" t)
(me--define-mold-suffix me--mold-piper-replace "piper-Replace" t)
(me--define-mold-suffix me--mold-piper-replace-regexp "piper-ReplaceRegexp" t)
(me--define-mold-suffix me--mold-piper-sort "piper-Sort" t)
(me--define-mold-suffix me--mold-piper-reverse-sort "piper-ReverseSort" t)
(me--define-mold-suffix me--mold-piper-unique "piper-Unique" t)
(me--define-mold-suffix me--mold-piper-join "piper-Join" t)
(me--define-mold-suffix me--mold-piper-keep-columns "piper-KeepColumns" t)
(me--define-mold-suffix me--mold-piper-pipe "piper-Pipe" t)
(me--define-mold-suffix me--mold-piper-shell "piper-Shell" t)
(me--define-mold-suffix me--mold-piper-xargs "piper-Xargs" t)
(me--define-mold-suffix me--mold-piper-for-each-line "piper-ForEachLine" t)
(me--define-mold-suffix me--mold-piper-to-clipboard "piper-ToClipboard" nil)

;;; Data Transform Suffixes

(me--define-mold-suffix me--mold-code-as-tree "CodeAsTree" t)
(me--define-mold-suffix me--mold-node-at-point-to-tree "NodeAtPointToTree" t)
(me--define-mold-suffix me--mold-org-as-tree "OrgAsTree" t)
(me--define-mold-suffix me--mold-json-as-tree "JsonAsTree" t)
(me--define-mold-suffix me--mold-elisp-as-tree "ElispAsTree" t)
(me--define-mold-suffix me--mold-xml-to-tree "XMLToTree" t)

(me--define-mold-suffix me--mold-elisp-list-to-org-table "ElispListToOrgTable" t)
(me--define-mold-suffix me--mold-org-table-to-csv "OrgTableToCSV" t)
(me--define-mold-suffix me--mold-csv-to-org-table "CSVToOrgTable" t)
(me--define-mold-suffix me--mold-first-org-table "FirstOrgTable" t)
(me--define-mold-suffix me--mold-csv-to-plist "CSVtoPlist" t)

(me--define-mold-suffix me--mold-json-as-plist "JsonAsPlist" t)
(me--define-mold-suffix me--mold-plist-to-json "PlistToJson" t)
(me--define-mold-suffix me--mold-yaml-to-json "YamlToJson" t)

;;; Code Analysis Suffixes

(me--define-mold-suffix me--mold-functions-complexity "FunctionsComplexity" t)
(me--define-mold-suffix me--mold-stats "Stats" t)
(me--define-mold-suffix me--mold-learn-syntax "LearnSyntax" t)
(me--define-mold-suffix me--mold-flycheck-errors-tree "FlycheckErrorsAsTree" t)

;;; Visualization Suffixes

(me--define-mold-suffix me--mold-csv-to-bar-chart "CSVToBarChart" t)
(me--define-mold-suffix me--mold-csv-to-line-chart "CSVToLineChart" t)
(me--define-mold-suffix me--mold-dot-to-picture "DotToPicture" t)

;;; Navigation Suffixes

(me--define-mold-suffix me--mold-similar-to-node "SimilarToNodeAtPoint" t)
(me--define-mold-suffix me--mold-same-prefix-nodes "SamePrefixToNodeAtPoint" t)
(me--define-mold-suffix me--mold-backlinks-org "Backlinks as Org" t)

;;; Playground Suffixes

(me--define-mold-suffix me--mold-playground "Playground" t)
(me--define-mold-suffix me--mold-eval-sexp "EvalSexp" nil)
(me--define-mold-suffix me--mold-query "Query" t)

;;; Meta Suffixes

(me--define-mold-suffix me--mold-what-molds "WhatMoldsCanIUse?" nil)
(me--define-mold-suffix me--mold-all-molds "AllMolds" nil)
(me--define-mold-suffix me--mold-history "Mold History" nil)

;;; Piper Submenu

(transient-define-prefix me-mold-piper-menu ()
  "Piper text and shell operations."
  :transient-suffix 't
  :transient-non-suffix 't

  ["Filter & Transform Lines"
   ("k" "Keep matching lines"      me--mold-piper-keep-lines)
   ("d" "Delete matching lines"    me--mold-piper-delete-lines)
   ("r" "Replace text"             me--mold-piper-replace)
   ("R" "Replace regexp"           me--mold-piper-replace-regexp)]

  ["Sort & Organize"
   ("s" "Sort lines"               me--mold-piper-sort)
   ("S" "Reverse sort"             me--mold-piper-reverse-sort)
   ("u" "Unique lines"             me--mold-piper-unique)
   ("j" "Join lines"               me--mold-piper-join)]

  ["Columns & Shell"
   ("c" "Keep columns"             me--mold-piper-keep-columns)
   ("p" "Pipe to shell"            me--mold-piper-pipe)
   ("!" "Shell command"            me--mold-piper-shell)
   ("x" "Xargs"                    me--mold-piper-xargs)
   ("e" "For each line"            me--mold-piper-for-each-line)]

  ["Actions"
   [("y" "Copy to clipboard"        me--mold-piper-to-clipboard)]
   [("q" "Back to main menu"        me-mold-operations-menu)
    ("Q" "Quit"                     transient-quit-one)]])

;;; Data Transform Submenu

(transient-define-prefix me-mold-data-menu ()
  "Data transformation operations."
  :transient-suffix 't
  :transient-non-suffix 't

  ["Trees"
   :if (lambda () (me--has-any-molds-usable '("CodeAsTree" "NodeAtPointToTree" "OrgAsTree" "JsonAsTree" "ElispAsTree" "XMLToTree")))
   ("c" "Code as tree"             me--mold-code-as-tree)
   ("n" "Node at point to tree"    me--mold-node-at-point-to-tree)
   ("o" "Org as tree"              me--mold-org-as-tree)
   ("j" "JSON as tree"             me--mold-json-as-tree)
   ("e" "Elisp as tree"            me--mold-elisp-as-tree)
   ("x" "XML to tree"              me--mold-xml-to-tree)]

  ["Tables"
   :if (lambda () (me--has-any-molds-usable '("ElispListToOrgTable" "OrgTableToCSV" "CSVToOrgTable" "FirstOrgTable" "CSVtoPlist")))
   ("T" "Elisp list to org table"  me--mold-elisp-list-to-org-table)
   ("O" "Org table to CSV"         me--mold-org-table-to-csv)
   ("C" "CSV to org table"         me--mold-csv-to-org-table)
   ("f" "First org table"          me--mold-first-org-table)
   ("p" "CSV to plist"             me--mold-csv-to-plist)]

  ["Format Conversions"
   :if (lambda () (me--has-any-molds-usable '("JsonAsPlist" "PlistToJson" "YamlToJson")))
   ("J" "JSON <-> Plist"           me--mold-json-as-plist)
   ("P" "Plist to JSON"            me--mold-plist-to-json)
   ("Y" "YAML to JSON"             me--mold-yaml-to-json)]

  ["Navigation"
   [("q" "Back to main menu"        me-mold-operations-menu)]
   [("Q" "Quit"                     transient-quit-one)]])

;;; Code Analysis Submenu

(transient-define-prefix me-mold-code-menu ()
  "Code analysis operations."
  :transient-suffix 't
  :transient-non-suffix 't

  ["Analysis"
   :if (lambda () (me--has-any-molds-usable '("FunctionsComplexity" "Stats" "LearnSyntax" "FlycheckErrorsAsTree")))
   ("f" "Functions complexity"     me--mold-functions-complexity)
   ("s" "Statistics"               me--mold-stats)
   ("l" "Learn syntax"             me--mold-learn-syntax)
   ("e" "Flycheck errors tree"     me--mold-flycheck-errors-tree)]

  ["Navigation"
   [("q" "Back to main menu"        me-mold-operations-menu)]
   [("Q" "Quit"                     transient-quit-one)]])

;;; Visualization Submenu

(transient-define-prefix me-mold-viz-menu ()
  "Visualization operations."
  :transient-suffix 't
  :transient-non-suffix 't

  ["Charts & Graphs"
   :if (lambda () (me--has-any-molds-usable '("CSVToBarChart" "CSVToLineChart" "DotToPicture")))
   ("b" "CSV to bar chart"         me--mold-csv-to-bar-chart)
   ("l" "CSV to line chart"        me--mold-csv-to-line-chart)
   ("d" "Dot to picture"           me--mold-dot-to-picture)]

  ["Navigation"
   [("q" "Back to main menu"        me-mold-operations-menu)]
   [("Q" "Quit"                     transient-quit-one)]])

;;; Navigation Submenu

(transient-define-prefix me-mold-nav-menu ()
  "Navigation and search operations."
  :transient-suffix 't
  :transient-non-suffix 't

  ["Find & Navigate"
   :if (lambda () (me--has-any-molds-usable '("SimilarToNodeAtPoint" "SamePrefixToNodeAtPoint" "Backlinks as Org")))
   ("s" "Similar nodes"            me--mold-similar-to-node)
   ("p" "Same prefix nodes"        me--mold-same-prefix-nodes)
   ("b" "Backlinks as org"         me--mold-backlinks-org)]

  ["Navigation"
   [("q" "Back to main menu"        me-mold-operations-menu)]
   [("Q" "Quit"                     transient-quit-one)]])

;;; Playground Submenu

(transient-define-prefix me-mold-playground-menu ()
  "Playground and evaluation operations."
  :transient-suffix 't
  :transient-non-suffix 't

  ["Evaluate & Experiment"
   :if (lambda () (me--has-any-molds-usable '("Playground" "EvalSexp" "Query")))
   ("p" "Playground"               me--mold-playground)
   ("e" "Eval sexp"                me--mold-eval-sexp)
   ("q" "Query"                    me--mold-query)]

  ["Navigation"
   [("b" "Back to main menu"        me-mold-operations-menu)]
   [("Q" "Quit"                     transient-quit-one)]])

;;; Main Menu

(transient-define-prefix me-mold-operations-menu ()
  "Main menu for moldable-emacs mold operations.
Navigate to category submenus or access meta operations."
  :transient-suffix 't
  :transient-non-suffix 't

  ["Mold Categories"
   [("p" "Piper (text/shell)"
     me-mold-piper-menu
     :if (lambda () (me--has-any-molds-usable '("piper-KeepLines" "piper-Pipe" "piper-Sort"))))
    ("d" "Data transforms"
     me-mold-data-menu
     :if (lambda () (me--has-any-molds-usable '("CodeAsTree" "JsonAsPlist" "CSVToOrgTable"))))
    ("c" "Code analysis"
     me-mold-code-menu
     :if (lambda () (me--has-any-molds-usable '("FunctionsComplexity" "Stats"))))]
   [("v" "Visualizations"
     me-mold-viz-menu
     :if (lambda () (me--has-any-molds-usable '("CSVToBarChart" "DotToPicture"))))
    ("n" "Navigate/Search"
     me-mold-nav-menu
     :if (lambda () (me--has-any-molds-usable '("SimilarToNodeAtPoint" "Backlinks as Org"))))
    ("P" "Playground/Eval"
     me-mold-playground-menu
     :if (lambda () (me--has-any-molds-usable '("Playground" "EvalSexp"))))]]

  ["Meta & Help"
   [("?" "What molds can I use?"    me--mold-what-molds)
    ("a" "All molds (complete)"     me-mold :transient nil)]
   [("h" "Mold history"             me--mold-history)
    ("m" "Show all molds list"      me--mold-all-molds)]]

  ["Actions"
   [("q" "Quit"                     transient-quit-one)]
   [("<escape>" "Quit"              transient-quit-one)]])

;;; Setup

;;;###autoload
(defun me-setup-transient-bindings ()
  "Setup key bindings for moldable-emacs transient menus."
  (when me-transient-menu-key
    (global-set-key (kbd me-transient-menu-key) #'me-mold-operations-menu)))

(provide 'moldable-emacs-transient)
;;; moldable-emacs-transient.el ends here
