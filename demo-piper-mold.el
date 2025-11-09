;;; demo-piper-mold.el --- Interactive demo of piper-mold integration -*- lexical-binding: t; -*-

;;; Commentary:

;; This file provides an interactive demonstration of piper-mold.el
;; Run each section step-by-step to see the integration in action.
;;
;; Usage:
;;   1. Open this file in Emacs
;;   2. Ensure moldable-emacs is loaded: (require 'moldable-emacs) (me-setup-molds)
;;   3. Place cursor after each form and press C-x C-e to evaluate
;;   4. Watch the magic happen!

;;; Code:

;; ============================================================================
;; SETUP: Ensure everything is loaded
;; ============================================================================

(require 'moldable-emacs)
(me-setup-molds)
(require 'piper)

;; Verify piper-mold.el is loaded
(message "Files with molds: %s" me-files-with-molds)
;; => Should include "molds/piper-mold.el"

;; Check how many piper-* molds are registered
(length (--filter (s-starts-with-p "piper-" (plist-get it :key))
                  me-available-molds))
;; => Should return 14 or more

;; List all piper molds
(--map (plist-get it :key)
       (--filter (s-starts-with-p "piper-" (plist-get it :key))
                 me-available-molds))
;; => ("piper-Pipe" "piper-Shell" "piper-KeepLines" ...)

;; ============================================================================
;; DEMO 1: Extract function definitions from piper-mold.el
;; ============================================================================

;; Step 1: Open piper-mold.el in a buffer
;; You can do this manually or evaluate:
(find-file "~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el")

;; Step 2: Now run interactively:
;;   M-x me-mold RET piper-KeepLines RET
;;   When prompted: defun
;;
;; You should see only lines containing "defun"

;; To see what this would produce programmatically:
(with-temp-buffer
  (insert-file-contents "~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el")
  (goto-char (point-min))
  (keep-lines "defun")
  (buffer-string))
;; Evaluate above to see the result!

;; ============================================================================
;; DEMO 2: Extract mold names and create a list
;; ============================================================================

;; Extract all :key "piper-*" lines, then get just the names
(with-temp-buffer
  (insert-file-contents "~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el")
  ;; Step 1: Keep only lines with :key "piper-
  (goto-char (point-min))
  (keep-lines ":key \"piper-")
  ;; Step 2: Extract just the mold names using shell
  (shell-command-on-region
   (point-min) (point-max)
   "sed 's/.*:key \"//;s/\".*//'"
   nil t)
  (buffer-string))
;; This simulates: piper-KeepLines → piper-Pipe workflow!

;; ============================================================================
;; DEMO 3: Count lines of code per function
;; ============================================================================

;; Interactive workflow simulation:
;; 1. Extract function names
;; 2. Use ForEachLine to count lines in each function
;; 3. Sort by count

;; Extract helper function names
(with-temp-buffer
  (insert-file-contents "~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el")
  (goto-char (point-min))
  (keep-lines "^(defun piper-mold--")
  (shell-command-on-region
   (point-min) (point-max)
   "sed 's/^(defun //;s/ .*//'"
   nil t)
  (buffer-string))
;; => Should show: piper-mold--get-input, piper-mold--set-output, ...

;; ============================================================================
;; DEMO 4: Create documentation from mold registrations
;; ============================================================================

;; Extract all me-register-mold blocks and create a summary
(with-temp-buffer
  (insert-file-contents "~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el")
  ;; Keep only :key lines
  (goto-char (point-min))
  (keep-lines " :key ")
  ;; Clean up to get markdown list
  (shell-command-on-region
   (point-min) (point-max)
   "sed 's/.*:key \"\\(.*\\)\"/- `\\1`/'"
   nil t)
  (buffer-string))
;; Copy result and paste into documentation!

;; ============================================================================
;; DEMO 5: Test a piper mold directly (programmatic)
;; ============================================================================

;; Simulate what piper-Sort mold does:
(let* ((test-input "zebra\napple\nbanana\ncherry")
       (buffername (get-buffer-create "*piper-test*")))
  (with-current-buffer buffername
    (erase-buffer)
    (insert test-input)
    ;; Simulate piper-Sort
    (sort-lines nil (point-min) (point-max))
    (setq-local self (buffer-substring-no-properties (point-min) (point-max))))
  (switch-to-buffer buffername)
  (message "Check *piper-test* buffer - it should be sorted!"))

;; ============================================================================
;; DEMO 6: Chain multiple operations programmatically
;; ============================================================================

;; Simulate: KeepLines → Pipe → Sort workflow
(let ((input "line 1: apple
line 2: banana
line 3: cherry
line 4: date
line 5: elderberry"))

  ;; Step 1: Keep only lines with vowels
  (setq input
        (with-temp-buffer
          (insert input)
          (goto-char (point-min))
          (keep-lines "[aeiou]")
          (buffer-string)))

  ;; Step 2: Extract just the fruit name (column 3)
  (setq input
        (with-temp-buffer
          (insert input)
          (shell-command-on-region
           (point-min) (point-max)
           "awk '{print $3}'"
           nil t)
          (buffer-string)))

  ;; Step 3: Sort
  (with-temp-buffer
    (insert input)
    (sort-lines nil (point-min) (point-max))
    (buffer-string)))
;; => "apple\nbanana\ncherry\ndate\nelderberry"

;; ============================================================================
;; INTERACTIVE INSTRUCTIONS
;; ============================================================================

;; Now try this yourself interactively:
;;
;; 1. Open piper-mold.el: C-x C-f ~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el
;;
;; 2. Run: M-x me-mold
;;    - Type "piper-" to see all piper molds
;;    - Choose "piper-KeepLines"
;;    - Enter pattern: "me-register-mold"
;;
;; 3. In the result buffer, run: M-x me-mold
;;    - Choose "piper-KeepColumns"
;;    - Delimiter: " "
;;    - Columns: 2
;;
;; 4. Run: M-x me-mold
;;    - Choose "piper-Sort"
;;
;; 5. Run: M-x me-mold
;;    - Choose "piper-ToClipboard"
;;
;; You just created a pipeline: Extract molds → Get names → Sort → Copy!

;; ============================================================================
;; VERIFICATION
;; ============================================================================

;; Verify all expected molds exist:
(let ((expected-molds '("piper-Pipe" "piper-Shell" "piper-KeepLines"
                        "piper-DeleteLines" "piper-Sort" "piper-ReverseSort"
                        "piper-Unique" "piper-Join" "piper-Replace"
                        "piper-ReplaceRegexp" "piper-KeepColumns"
                        "piper-Xargs" "piper-ForEachLine" "piper-ToClipboard"))
      (registered-molds (--map (plist-get it :key)
                               (--filter (s-starts-with-p "piper-"
                                                          (plist-get it :key))
                                        me-available-molds))))
  (--map (cons it (if (member it registered-molds) "✓ Found" "✗ Missing"))
         expected-molds))
;; Each should show "✓ Found"

(message "Demo loaded! Read the comments and try the interactive instructions.")

(provide 'demo-piper-mold)
;;; demo-piper-mold.el ends here
