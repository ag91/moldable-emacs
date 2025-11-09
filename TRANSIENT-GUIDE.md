# Moldable-Emacs Transient Menu Guide

## Overview

The transient menu system provides a hierarchical, discoverable interface for moldable-emacs operations, inspired by Magit. Instead of completing mold names repeatedly, you navigate through categorized menus with single-key shortcuts.

## Installation

The transient integration is automatically loaded when you require moldable-emacs:

```elisp
(require 'moldable-emacs)
(me-setup-molds)
```

The transient menu will be bound to `C-c m` by default.

### Configuration

```elisp
;; Customize the key binding (before loading)
(setq me-transient-menu-key "C-c m")

;; Or set to nil to disable automatic binding
(setq me-transient-menu-key nil)

;; Then manually bind later:
(global-set-key (kbd "C-c m") #'me-mold-operations-menu)
```

## Usage

### Opening the Menu

Press `C-c m` (or your configured key) to open the main operations menu.

### Main Menu Structure

```
┌─────────────────────────────────────┐
│ Mold Categories                     │
├─────────────────────────────────────┤
│ p  Piper (text/shell)               │
│ d  Data transforms                  │
│ c  Code analysis                    │
│ v  Visualizations                   │
│ n  Navigate/Search                  │
│ P  Playground/Eval                  │
├─────────────────────────────────────┤
│ Meta & Help                         │
├─────────────────────────────────────┤
│ ?  What molds can I use?            │
│ a  All molds (complete)             │
│ h  Mold history                     │
│ m  Show all molds list              │
├─────────────────────────────────────┤
│ Actions                             │
├─────────────────────────────────────┤
│ q  Quit                             │
└─────────────────────────────────────┘
```

### Navigation

1. **From main menu**: Press a category letter (e.g., `p` for Piper)
2. **In submenu**: Press operation letters to execute molds
3. **Return to main**: Press `q` in submenu
4. **Exit entirely**: Press `Q` or `<escape>`

### Behavior

- **Operations stay in menu by default** - Chain multiple operations easily
- **Only applicable molds shown** - Based on `:given` predicates
- **Categories hide when empty** - If no molds are available in a category
- **Manual invocation only** - Menu doesn't pop up automatically

## Category Breakdown

### Piper Operations (`p`)

Text and shell operations for transforming buffer content.

```
┌─────────────────────────────────────┐
│ Filter & Transform Lines            │
├─────────────────────────────────────┤
│ k  Keep matching lines              │
│ d  Delete matching lines            │
│ r  Replace text                     │
│ R  Replace regexp                   │
├─────────────────────────────────────┤
│ Sort & Organize                     │
├─────────────────────────────────────┤
│ s  Sort lines                       │
│ S  Reverse sort                     │
│ u  Unique lines                     │
│ j  Join lines                       │
├─────────────────────────────────────┤
│ Columns & Shell                     │
├─────────────────────────────────────┤
│ c  Keep columns                     │
│ p  Pipe to shell                    │
│ !  Shell command                    │
│ x  Xargs                            │
│ e  For each line                    │
├─────────────────────────────────────┤
│ Actions                             │
├─────────────────────────────────────┤
│ y  Copy to clipboard (exits)        │
│ q  Back to main menu                │
│ Q  Quit                             │
└─────────────────────────────────────┘
```

**Key point**: All operations stay in menu except "Copy to clipboard" which exits.

### Data Transforms (`d`)

Convert between data formats and representations.

**Subgroups:**
- **Trees**: Code/Org/JSON/Elisp/XML to tree structures
- **Tables**: Org table, CSV, plist conversions
- **Format Conversions**: JSON, YAML, plist transformations

### Code Analysis (`c`)

Analyze code structure and quality.

**Operations:**
- Functions complexity analysis
- Code statistics
- Syntax learning
- Flycheck error visualization

### Visualizations (`v`)

Generate charts and graphs from data.

**Operations:**
- CSV to bar chart
- CSV to line chart
- Dot to picture

### Navigate/Search (`n`)

Find and explore code relationships.

**Operations:**
- Find similar nodes
- Find same-prefix nodes
- Show backlinks as org

### Playground/Eval (`P`)

Interactive experimentation environments.

**Operations:**
- Open playground
- Eval sexp
- Query interface

## Example Workflows

### Workflow 1: Clean and Sort Log File

1. Open log file
2. `C-c m` → `p` (Piper menu)
3. `k` → Enter pattern: `ERROR`
4. `c` → Delimiter: ` ` → Columns: `1,2,3`
5. `s` (Sort)
6. `u` (Unique)
7. `y` (Copy to clipboard)

**Result**: Unique, sorted error entries copied to clipboard, all without leaving the menu!

### Workflow 2: Analyze Code Complexity

1. Open source file
2. `C-c m` → `c` (Code analysis)
3. `f` (Functions complexity)
4. `C-c m` → `v` (Visualization)
5. `b` (Bar chart)

**Result**: Visual complexity report

### Workflow 3: Transform Data Format

1. Open JSON file
2. `C-c m` → `d` (Data transforms)
3. `J` (JSON to plist)
4. `C-c m` → `P` (Playground)
5. `p` (Playground for editing)

**Result**: JSON converted to plist in playground buffer

## Tips & Tricks

### 1. Fast Piper Chaining

For text transformation workflows, stay in the piper menu:
```
C-c m p → k → r → s → u → y
```
Five operations, six keypresses!

### 2. Fallback to Completion

If you can't find a mold in the menu:
- Press `a` from main menu for full `completing-read` interface
- Or press `?` to see what molds are available

### 3. Check Availability

If a category/operation doesn't appear, the `:given` predicate returned false:
- Check if buffer is in correct mode
- Verify required data structure exists
- Use `?` to see requirements

### 4. Keyboard Maestro

Remember common sequences:
- **Text cleanup**: `C-c m p k s u y`
- **Quick analysis**: `C-c m c f`
- **Explore code**: `C-c m n s`

### 5. Return to Main Menu

From any submenu, `q` returns to main menu (capital `Q` quits entirely).

## Comparison: Before and After

### Before (Traditional Workflow)

```
M-x me-mold RET
Type: "piper-KeepLines" RET
Enter pattern: "ERROR" RET

M-x me-mold RET
Type: "piper-Sort" RET

M-x me-mold RET
Type: "piper-Unique" RET

M-x me-mold RET
Type: "piper-ToClipboard" RET
```

**Total**: 4 `M-x me-mold` invocations, typing mold names each time.

### After (Transient Workflow)

```
C-c m
p (Piper menu)
k (Keep lines) → Enter pattern: "ERROR"
s (Sort)
u (Unique)
y (Copy to clipboard)
```

**Total**: 1 menu invocation, 5 single-key presses.

## Technical Details

### Dynamic Availability

Molds are filtered using their `:given` predicates:

```elisp
(defun me--mold-usable-p (mold-key)
  "Check if mold is currently usable."
  (when-let ((mold (me-find-mold mold-key)))
    (me-interpret-given mold)))
```

If a mold's `:given` returns `nil`, it won't appear in the menu.

### Adding New Molds to Menus

To add a new mold to the transient interface:

```elisp
;; 1. Define a suffix command
(me--define-mold-suffix me--mold-my-operation "MyMoldKey" t)

;; 2. Add to appropriate submenu
;; Edit moldable-emacs-transient.el and add to a menu:
(transient-define-prefix me-mold-data-menu ()
  ...
  ["Trees"
   ...
   ("m" "My operation" me--mold-my-operation)]
  ...)
```

### Customization: Category Predicates

Categories are shown/hidden based on predicate functions:

```elisp
(defun me--has-any-molds-usable (mold-keys)
  "Check if any mold in MOLD-KEYS is usable."
  (--any (me--mold-usable-p it) mold-keys))

;; Used in menu definitions:
["Trees"
 :if (lambda () (me--has-any-molds-usable '("CodeAsTree" "JsonAsTree")))]
```

## Troubleshooting

### Issue: Menu doesn't appear

**Check:**
```elisp
;; Is transient loaded?
(featurep 'moldable-emacs-transient)  ; Should return t

;; Is key bound?
(where-is-internal 'me-mold-operations-menu)  ; Should show C-c m

;; Manually invoke:
M-x me-mold-operations-menu RET
```

### Issue: Category/operation missing

**Diagnosis:**
```elisp
;; Check if mold exists:
(me-find-mold "piper-Sort")  ; Should return mold plist

;; Check if usable:
(me--mold-usable-p "piper-Sort")  ; Should return t

;; Check :given predicate:
(plist-get (me-find-mold "piper-Sort") :given)
```

### Issue: Operations don't chain

**Cause**: Mold might be defined with `:transient nil`

**Check in moldable-emacs-transient.el:**
```elisp
;; Most operations should have :transient t
(me--define-mold-suffix me--mold-piper-sort "piper-Sort" t)
                                                         ^^^
```

## Advanced: Integration with Other Tools

### Combine with org-capture

```elisp
(defun my-mold-to-capture ()
  "Run mold and capture result."
  (interactive)
  (me-mold-operations-menu)
  ;; After mold chain, capture the buffer
  (org-capture))
```

### Combine with project.el

```elisp
(defun my-project-mold ()
  "Run mold on project root."
  (interactive)
  (let ((default-directory (project-root (project-current t))))
    (me-mold-operations-menu)))
```

## Keyboard Cheat Sheet

```
Main Menu:
  p - Piper        d - Data         c - Code
  v - Viz          n - Navigate     P - Playground
  ? - Help         a - All molds    q - Quit

Piper Menu:
  k - Keep         d - Delete       r - Replace
  s - Sort         u - Unique       j - Join
  p - Pipe         ! - Shell        y - Clipboard

Universal:
  q - Back to main     Q - Quit entirely
  <escape> - Quit
```

## FAQ

**Q: Can I still use `M-x me-mold`?**
A: Yes! The traditional interface still works. Press `a` in the main menu for completion.

**Q: Why don't all 100+ molds appear?**
A: Only applicable molds (where `:given` returns true) are shown. This keeps menus clean and relevant.

**Q: Can I customize key bindings?**
A: Yes, edit the transient definitions in `moldable-emacs-transient.el`. Each submenu is independently configurable.

**Q: How do I disable the transient menu?**
A: Set `me-transient-menu-key` to `nil` before loading moldable-emacs.

**Q: Can I make operations exit the menu?**
A: Yes, pass `nil` as the third argument to `me--define-mold-suffix`:
```elisp
(me--define-mold-suffix me--mold-my-op "MyMold" nil)
```

## See Also

- **QUICKSTART-piper-mold.md** - Piper operations tutorial
- **TUTORIAL-piper-mold.md** - Detailed piper integration guide
- **moldable-emacs documentation** - Core mold system
- **Magit manual** - Inspiration for transient interface

---

Happy molding with improved ergonomics! 🎉
