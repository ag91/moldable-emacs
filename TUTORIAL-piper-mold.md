# Hands-On Tutorial: Demonstrating piper-mold.el

This tutorial demonstrates the integration of piper operations into moldable-emacs by "molding" the `piper-mold.el` file itself.

## Prerequisites

1. Ensure moldable-emacs is loaded:
   ```elisp
   (require 'moldable-emacs)
   (me-setup-molds)  ; Load all molds including piper-mold.el
   ```

2. Verify piper is available:
   ```elisp
   (require 'piper)
   ```

## Tutorial Steps

### Step 1: Open piper-mold.el

```
M-x find-file RET ~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el RET
```

You should see the file with ~220 lines of Elisp code containing our piper mold definitions.

### Step 2: Extract Function Names (piper-KeepLines)

Now let's use our first piper mold to extract only the lines that define functions.

```
M-x me-mold RET piper-KeepLines RET
```

When prompted: `Keep lines matching: defun`

**Expected Result:** A new buffer showing only lines containing "defun":
```elisp
(defun piper-mold--get-input ()
(defun piper-mold--set-output (buffername output)
(defun piper-mold--with-temp-buffer-from-input (body-fn)
```

**What happened:** The mold filtered the entire file to show only function definitions.

### Step 3: Extract Just Function Signatures (piper-Pipe)

From the previous result, let's extract just the function signatures using a shell command.

```
M-x me-mold RET piper-Pipe RET
```

When prompted: `Pipe: | sed 's/^.*defun //' | sed 's/ .*//'`

**Expected Result:**
```
piper-mold--get-input
piper-mold--set-output
piper-mold--with-temp-buffer-from-input
```

**What happened:** We piped the filtered lines through sed to extract just the function names.

### Step 4: Sort the Function Names (piper-Sort)

Let's sort these function names alphabetically.

```
M-x me-mold RET piper-Sort RET
```

**Expected Result:** Same list but sorted alphabetically.

**What happened:** The `piper-Sort` mold sorted all lines in the buffer.

### Step 5: Start Over - Extract All Mold Keys

Let's go back to the original file and extract something different. Open piper-mold.el again.

```
M-x find-file RET ~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el RET
M-x me-mold RET piper-KeepLines RET
```

When prompted: `Keep lines matching: :key "piper-`

**Expected Result:**
```elisp
 :key "piper-Pipe"
 :key "piper-Shell"
 :key "piper-KeepLines"
 :key "piper-DeleteLines"
 :key "piper-Sort"
 :key "piper-ReverseSort"
 :key "piper-Unique"
 :key "piper-Join"
 :key "piper-Replace"
 :key "piper-ReplaceRegexp"
 :key "piper-KeepColumns"
 :key "piper-Xargs"
 :key "piper-ForEachLine"
 :key "piper-ToClipboard"
```

### Step 6: Extract Just the Mold Names (piper-KeepColumns)

Now let's extract just the mold names using column operations.

```
M-x me-mold RET piper-KeepColumns RET
```

When prompted:
- `Column delimiter: "`
- `Column number(s) to keep: 2`

**Expected Result:**
```
piper-Pipe
piper-Shell
piper-KeepLines
piper-DeleteLines
piper-Sort
piper-ReverseSort
piper-Unique
piper-Join
piper-Replace
piper-ReplaceRegexp
piper-KeepColumns
piper-Xargs
piper-ForEachLine
piper-ToClipboard
```

**What happened:** We split each line by `"` and kept the 2nd column (the mold name).

### Step 7: Create a Checklist (piper-Replace)

Let's turn this into a markdown checklist.

```
M-x me-mold RET piper-Replace RET
```

When prompted:
- `Replace: piper-`
- `With: - [ ] piper-`

**Expected Result:**
```
- [ ] piper-Pipe
- [ ] piper-Shell
- [ ] piper-KeepLines
- [ ] piper-DeleteLines
- [ ] piper-Sort
- [ ] piper-ReverseSort
- [ ] piper-Unique
- [ ] piper-Join
- [ ] piper-Replace
- [ ] piper-ReplaceRegexp
- [ ] piper-KeepColumns
- [ ] piper-Xargs
- [ ] piper-ForEachLine
- [ ] piper-ToClipboard
```

### Step 8: Copy to Clipboard (piper-ToClipboard)

Finally, copy this checklist to use elsewhere.

```
M-x me-mold RET piper-ToClipboard RET
```

**Expected Result:** Message "Copied to clipboard" and the checklist is now in your kill ring.

Now you can paste it anywhere with `C-y`!

## Advanced: Using piper-ForEachLine

Let's do something more advanced. Open piper-mold.el again and extract all mold keys (Step 5), then get documentation for each.

After extracting mold names (through Step 6), run:

```
M-x me-mold RET piper-ForEachLine RET
```

When prompted: `Command (use {} for line): echo "Testing {}" | wc -c`

**Expected Result:** Each line processed with its character count:
```
19
20
22
...
```

**What happened:** The command ran for each mold name, substituting `{}` with the name.

## Advanced: Combining with Non-Piper Molds

The beauty of this integration is that piper molds work seamlessly with other moldable-emacs molds!

After extracting mold names, you can:
1. Use a moldable-emacs visualization mold (if available)
2. Use `Playground` mold to edit the list interactively
3. Use any other mold that accepts text input

The `self` variable flows between molds, so:
```
File → piper-KeepLines → piper-KeepColumns → SomeMold → piper-Sort → Result
```

All work together seamlessly!

## Key Concepts Demonstrated

1. **Input Flexibility**: Molds work on buffer contents
2. **Chaining**: Each mold's output becomes the next mold's input
3. **Namespace**: All piper molds have `piper-` prefix
4. **Integration**: Appear in standard `me-mold` menu alongside other molds
5. **Bidirectional**: Can go from piper-molds to other molds and back

## Verification Checklist

- [ ] piper-molds appear in `M-x me-mold` completion
- [ ] piper-KeepLines filters lines correctly
- [ ] piper-Pipe executes shell commands
- [ ] piper-Sort sorts lines
- [ ] piper-KeepColumns extracts columns
- [ ] piper-Replace does text replacement
- [ ] piper-ToClipboard copies to kill ring
- [ ] Molds can be chained together
- [ ] Results can feed into other moldable-emacs molds

## Troubleshooting

If molds don't appear:
```elisp
;; Reload molds
(me-setup-molds)

;; Check if piper-mold.el is in the list
me-files-with-molds

;; Manually load if needed
(load-file "~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el")
```

If you get errors about `self` not bound:
- This is normal for the first mold in a chain
- The molds automatically fall back to buffer contents

## Next Steps

Try creating your own workflows:
- Extract and analyze TODO comments
- Process log files
- Transform CSV data
- Generate reports from code
- Create documentation from function signatures

Happy molding!
