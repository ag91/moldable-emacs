# Quick Start: Test piper-mold.el Integration

## 🚀 Fastest way to verify it works

### Step 1: Load Everything

Open Emacs and run:

```elisp
M-: (require 'moldable-emacs) RET
M-: (me-setup-molds) RET
M-: (require 'piper) RET
```

### Step 2: Verify Molds Are Registered

```elisp
M-: (length (--filter (s-starts-with-p "piper-" (plist-get it :key)) me-available-molds)) RET
```

**Expected output:** `14` (or more)

### Step 3: See All Piper Molds

```elisp
M-: (--map (plist-get it :key) (--filter (s-starts-with-p "piper-" (plist-get it :key)) me-available-molds)) RET
```

**Expected output:**
```elisp
("piper-Pipe" "piper-Shell" "piper-KeepLines" "piper-DeleteLines"
 "piper-Sort" "piper-ReverseSort" "piper-Unique" "piper-Join"
 "piper-Replace" "piper-ReplaceRegexp" "piper-KeepColumns"
 "piper-Xargs" "piper-ForEachLine" "piper-ToClipboard")
```

✅ **If you see this, the integration is working!**

---

## 🎯 Hands-On Demo: Mold the piper-mold.el File

### Demo: Extract all mold names and sort them

**Step 1:** Open piper-mold.el

```
C-x C-f ~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el RET
```

**Step 2:** Filter to mold registrations

```
M-x me-mold RET
```

Type `piper-Keep` and you'll see `piper-KeepLines` in the completion.

Select it and press RET.

When prompted: `:key "piper-`

**What you'll see:** A new buffer with only these lines:
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

**Step 3:** Extract just the names (use columns)

In the result buffer, run:

```
M-x me-mold RET piper-KeepColumns RET
```

Prompts:
- `Column delimiter:` enter `"`
- `Column number(s) to keep:` enter `2`

**What you'll see:**
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

**Step 4:** Sort alphabetically

```
M-x me-mold RET piper-Sort RET
```

**What you'll see:** Same list, now sorted!

```
piper-DeleteLines
piper-ForEachLine
piper-Join
piper-KeepColumns
piper-KeepLines
piper-Pipe
piper-Replace
piper-ReplaceRegexp
piper-ReverseSort
piper-Shell
piper-Sort
piper-ToClipboard
piper-Unique
piper-Xargs
```

**Step 5:** Copy to clipboard

```
M-x me-mold RET piper-ToClipboard RET
```

**What you'll see:** Message "Copied to clipboard"

Now press `C-y` anywhere to paste!

---

## 🎓 What Just Happened?

You created a **mold pipeline**:

```
piper-mold.el (220 lines)
    ↓ piper-KeepLines
14 lines with :key
    ↓ piper-KeepColumns
14 mold names
    ↓ piper-Sort
Sorted names
    ↓ piper-ToClipboard
Ready to paste!
```

Each mold:
1. Received input (from buffer or previous mold's `self`)
2. Transformed it
3. Stored result in new buffer + `self`
4. Made it available to next mold

---

## 🔥 Try More Examples

### Example 1: Extract all function names

```
Open: piper-mold.el
Run: M-x me-mold RET piper-KeepLines RET
Input: ^(defun
Run: M-x me-mold RET piper-Pipe RET
Input: | sed 's/^(defun //;s/ .*//'
```

Result: List of all function names!

### Example 2: Count comment lines

```
Open: piper-mold.el
Run: M-x me-mold RET piper-KeepLines RET
Input: ^;;
Run: M-x me-mold RET piper-Pipe RET
Input: | wc -l
```

Result: Number of comment lines!

### Example 3: Create a word frequency list

```
Open: piper-mold.el
Run: M-x me-mold RET piper-Pipe RET
Input: | tr ' ' '\n' | sort | uniq -c | sort -rn | head -20
```

Result: Top 20 most common words!

---

## 🐛 Troubleshooting

### Problem: "No molds available"

**Solution:**
```elisp
M-: (me-setup-molds) RET
```

### Problem: "piper-* molds don't appear"

**Check if file is in the list:**
```elisp
M-: me-files-with-molds RET
```

Should include: `"...molds/piper-mold.el"`

**Manually load if needed:**
```elisp
M-: (load-file "~/.config/emacs/.local/straight/repos/moldable-emacs/molds/piper-mold.el") RET
```

### Problem: "Symbol's value as variable is void: self"

This is **normal** for the first mold in a chain. The molds automatically fall back to buffer contents when `self` isn't set.

---

## 📚 More Resources

- **Full Tutorial:** See `TUTORIAL-piper-mold.md` for detailed walkthrough
- **Demo Script:** Run `demo-piper-mold.el` for interactive examples
- **moldable-emacs Docs:** Learn about the mold system
- **emacs-piper Docs:** Learn about piper operations

---

## ✨ Key Takeaways

1. ✅ **14 piper operations** are now available as molds
2. ✅ All have `piper-` prefix for easy discovery
3. ✅ Work seamlessly with other moldable-emacs molds
4. ✅ Chain operations by running `me-mold` repeatedly
5. ✅ Each mold preserves both visual and structured output

**Happy molding!** 🎉
