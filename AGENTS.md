# AGENTS.md

## Project Overview

moldable-emacs is an Emacs package implementing Moldable Development for
Emacs users. The goal is to make you a better story teller when you deal
with code. Molds are micro-views that transform data into more
absorbable representations; they compose into bigger stories.

## Repository Structure

- `moldable-emacs.el` — the mold engine: registration, dispatch,
  composition, context-passing, examples-as-tests, history, utilities
- `molds/core.el` — always-available and common molds
- `molds/contrib.el` — molds requiring optional dependencies
  (code-compass, graph-cli, graphviz, org-roam, nyxt, etc.)
- `tests/moldable-emacs-test.el` — ERT tests
- `tutorials/` — Org-mode tutorials (HowToWriteAMold, etc.)
- `plans/` — implementation plans
- `resources/` — media files for examples
- `Eldev` — build/test configuration (uses `eldev`)
- `test.sh` — runs tests via `./eldev -p -dtT test`

## Build and Test Commands

```bash
# Run tests
./test.sh

# Or directly via eldev
./eldev -p -dtT test
```

Always run tests after making changes. The test suite includes
`molds-have-examples_they-should-pass` which checks that all molds with
examples pass their examples.

## Coding Conventions

### Libraries

- **Prefer `dash.el` over built-in seq/cl-lib for list operations.**
  Use `--map`, `--filter`, `--reduce`, `--find`, `--each`, `-when-let*`,
  `-contains-p`, `-distinct`, etc.
- **Prefer `s.el` over built-in string functions for string operations.**
  Use `s-join`, `s-split`, `s-contains-p`, `s-replace-all`, `s-starts-with-p`,
  `s-ends-with-p`, `s-blank-p`, etc.
- `cl-lib` is used sparingly (mainly `cl-defstruct` if needed); prefer dash.
- `thunk.el` is used for lazy evaluation (`:let` bindings via `thunk-let*`).

### Naming

- All functions and variables use the `me-` prefix.
- Macros use `me-` prefix too (e.g., `me-register-mold`, `me--given`).
- Internal/test helpers can use `me--` (double dash).
- Mold keys are capitalized strings without spaces (e.g., `"CodeAsTree"`,
  `"FunctionsComplexity"`, `"List To Dot"` — spaces are allowed in keys
  for readability).

### Mold Definition Format

Molds are registered with `me-register-mold` (or `me-register-mold-by-key`
for composed molds). A mold is a plist with these keys:

```elisp
(me-register-mold
 :key "MoldName"                        ; required, string
 :given (:fn <predicate>)               ; required, plist with :fn
 :then (:fn <body>)                     ; required, plist with :fn
                                        ; or (:async (...) :fn <body>)
 :let ((var1 <expr>)                    ; optional, lazy bindings
       (var2 <expr>))
 :when (:fn <predicate>)                ; optional, auto-refresh trigger
 :docs "Documentation string."          ; optional but expected
 :examples ((                           ; optional but expected
             :name "Example name"
             :given (:type file|buffer
                     :name "..."
                     :mode some-mode
                     :contents "..."
                     :point N)
             :then (:type file|buffer
                    :name "..."
                    :mode some-mode
                    :contents "...")))))
```

### Mold Conventions

- **Every mold should have `:docs`.** The engine warns when missing.
- **Every mold should have `:examples` when possible.** The engine warns
  when missing. Examples double as tests and documentation.
- The `:given :fn` should check `major-mode`, dependencies via
  `me-require`, and executables via `executable-find`.
- The `:then :fn` should set `self` (buffer-local) to the structured data
  of the result, so downstream molds can use it.
- Use `me-require` instead of `require` in `:given` — it returns nil
  instead of erroring when the package is missing.
- Composed molds use `me-mold-compose` and are registered with
  `me-register-mold-by-key`.

### Examples

Examples are plists with `:given` and `:then` snapshots. They are
checked by `me-check-mold-examples` and serve as both tests and
documentation (via `me-example-to-docstring`).

When adding a new mold, provide at least one example. If the mold's
output is non-deterministic or requires interactive input, note why
examples are not possible with `:examples nil`.

**Improving the examples harness is an ongoing priority.** The current
harness (`me--given`, `me-check-then-clause`) has limitations with
async molds, image output, and interactive input. When you encounter
these limitations, document them and consider extending the harness.

### Code Style

- Use `lexical-binding: t` in all files.
- Indent with spaces, 2 spaces per level (following Emacs Lisp conventions).
- Use `--` threading macros (`-->`, `--map`, `--filter`) for pipelines.
- Use `ignore-errors` around operations that may fail in `:given` or
  `:then` when failure means the mold is not applicable.
- Use `with-current-buffer buffername` in `:then` to write output.
- Set `self` buffer-locally with `setq-local`.
- Keep molds small and composable. A mold should do one transformation.
- Prefer composing existing molds over writing complex `:then` bodies.

### File Organization

- Core molds (no external dependencies) go in `molds/core.el`.
- Molds requiring optional packages go in `molds/contrib.el`.
- Utility functions used by molds go in `moldable-emacs.el`.
- New mold files can be added to `me-files-with-molds`.

### Testing

- Write ERT tests for new utility functions in
  `tests/moldable-emacs-test.el`.
- Provide `:examples` for new molds — these are automatically tested.
- Run `./test.sh` before committing.
- The test `molds-have-examples_they-should-pass` validates all mold
  examples.

### Git Conventions

- Do not commit unless explicitly asked.
- Write concise commit messages matching the repo style.
- Never commit secrets or credentials.

## Key Concepts

- **Mold**: a micro-view that transforms data into a new representation.
- **Composition**: chaining molds via `me-mold-compose`. The composed
  mold runs mold1, then mold2, kills mold1's buffer, renames mold2's.
- **`self`**: buffer-local variable holding the structured data of the
  current buffer. Set by each mold's `:then`.
- **`mold-data`**: buffer-local plist with context about the previous
  mold run (`:old-self`, `:old-buffer`, `:old-mold`, etc.). Enables
  contextual composition.
- **Specificity**: `me-mold-specificity` ranks usable molds by how
  specific their `:given` predicates are. More specific molds appear
  first in completion.
- **Examples as tests**: `:examples` on molds are checked by the test
  suite and serve as documentation via `me-example-to-docstring`.

## Vision

The ultimate goal is storytelling: turning complex things into easier
to understand via abstraction. Molds should simplify storytelling to
other people — explaining rationale, context, choices, and resulting
choices. This must be deterministic (not AI-based statistical inference).
See `plans/NextSteps.org` for the implementation plan toward this vision.
