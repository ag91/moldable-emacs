# Moldable-Emacs Transient Quick Reference

## Setup

```elisp
(require 'moldable-emacs)
(me-setup-molds)
;; Transient menu auto-binds to C-c m
```

## Main Menu (`C-c m`)

| Key | Category | Description |
|-----|----------|-------------|
| `p` | Piper | Text/shell operations |
| `d` | Data | Format transformations |
| `c` | Code | Analysis tools |
| `v` | Viz | Charts/graphs |
| `n` | Navigate | Search/explore |
| `P` | Playground | Interactive eval |
| `?` | Help | What molds available |
| `a` | All | Full mold completion |
| `h` | History | Mold history |
| `q` | Quit | Exit menu |

## Piper Submenu

### Filter & Transform
| Key | Operation | Exits? |
|-----|-----------|--------|
| `k` | Keep matching lines | No |
| `d` | Delete matching lines | No |
| `r` | Replace text | No |
| `R` | Replace regexp | No |

### Sort & Organize
| Key | Operation | Exits? |
|-----|-----------|--------|
| `s` | Sort lines | No |
| `S` | Reverse sort | No |
| `u` | Unique lines | No |
| `j` | Join lines | No |

### Columns & Shell
| Key | Operation | Exits? |
|-----|-----------|--------|
| `c` | Keep columns | No |
| `p` | Pipe to shell | No |
| `!` | Shell command | No |
| `x` | Xargs | No |
| `e` | For each line | No |

### Actions
| Key | Operation | Exits? |
|-----|-----------|--------|
| `y` | Copy to clipboard | **Yes** |
| `q` | Back to main | - |
| `Q` | Quit entirely | - |

## Data Transforms Submenu

### Trees
| Key | Operation |
|-----|-----------|
| `c` | Code as tree |
| `n` | Node at point to tree |
| `o` | Org as tree |
| `j` | JSON as tree |
| `e` | Elisp as tree |
| `x` | XML to tree |

### Tables
| Key | Operation |
|-----|-----------|
| `T` | Elisp list to org table |
| `O` | Org table to CSV |
| `C` | CSV to org table |
| `f` | First org table |
| `p` | CSV to plist |

### Format Conversions
| Key | Operation |
|-----|-----------|
| `J` | JSON <-> Plist |
| `P` | Plist to JSON |
| `Y` | YAML to JSON |

## Code Analysis Submenu

| Key | Operation |
|-----|-----------|
| `f` | Functions complexity |
| `s` | Statistics |
| `l` | Learn syntax |
| `e` | Flycheck errors tree |

## Visualization Submenu

| Key | Operation |
|-----|-----------|
| `b` | CSV to bar chart |
| `l` | CSV to line chart |
| `d` | Dot to picture |

## Navigation Submenu

| Key | Operation |
|-----|-----------|
| `s` | Similar nodes |
| `p` | Same prefix nodes |
| `b` | Backlinks as org |

## Playground Submenu

| Key | Operation |
|-----|-----------|
| `p` | Playground |
| `e` | Eval sexp |
| `q` | Query |

## Common Workflows

### Clean Log File
```
C-c m → p → k (pattern) → s → u → y
```

### Analyze Code
```
C-c m → c → f
```

### Transform JSON
```
C-c m → d → J
```

### Create Chart from CSV
```
C-c m → v → b
```

## Tips

- **Stay in menu**: Most operations keep menu open for chaining
- **Hidden operations**: Only applicable molds show up (based on `:given`)
- **Return to main**: Press `q` in submenu
- **Exit entirely**: Press `Q` or `<escape>`
- **Full completion**: Press `a` from main menu

## Keyboard Flow Example

```
Open file.log
C-c m         ← Open menu
p             ← Piper submenu
k             ← Keep lines
ERROR         ← Pattern
[Result shown with menu still open]
c             ← Keep columns
,             ← Delimiter
1,2           ← Columns
[Result shown]
s             ← Sort
[Result shown]
y             ← Copy & exit
C-y           ← Paste elsewhere
```

**6 keypresses** for a 4-operation pipeline!

## Customization

```elisp
;; Change key binding
(setq me-transient-menu-key "C-c C-m")

;; Disable auto-binding
(setq me-transient-menu-key nil)
(global-set-key (kbd "M-m") #'me-mold-operations-menu)
```

## Troubleshooting

```elisp
;; Check if loaded
(featurep 'moldable-emacs-transient)  ;; Should be t

;; Manually invoke
M-x me-mold-operations-menu

;; Check mold availability
M-x me--mold-usable-p RET "piper-Sort"
```
