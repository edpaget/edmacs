# Findings: can we test window geometry automatically?

Answer: **yes, and the harness is cheaper than feared** -- but the premise
behind the question was wrong, and correcting it is the most valuable
result here.

---

## 0. The headline correction

**Batch did not lie about `display-buffer-in-side-window`.**

The project believes a batch experiment reported that
`display-buffer-in-side-window` resizes a reused side window, that the live
daemon proved it does not, and that this was a batch-vs-GUI divergence. It
was not. Batch and a real NS GUI frame give **byte-identical answers** on
this question. The two experiments differed in the *state of the window they
were run against*, not in the environment.

The rule, from `window.el` (Emacs 31.1, `window--display-buffer` :7867-7869,
`display-buffer-record-window` :7027-7053):

> A reused window is resized to a `window-width` request **only if** its
> `quit-restore` window parameter is a cons whose **slot 1 is the symbol
> `window`**.

- A window that `display-buffer` created itself gets
  `quit-restore = (window window SELWIN BUF)`. A later same-buffer reuse
  rewrites slot 0 to `same`, giving `(same window ...)` -- slot 1 is still
  `window`, so **the resize fires**. This is what the batch experiment
  measured, and it is correct.
- A window with **no** `quit-restore` never resizes. `quit-restore` is not in
  `window-persistent-parameters`, so **every frameset/desktop-restored window
  is in this state**.
- A window that ever displayed a *different* buffer gets the displaced
  buffer's quadruple written into slot 1, giving `(same (BUF START PT H) ...)`.
  Slot 1 is no longer `window`, so it **never resizes again for the life of
  that window**.

In all three cases the request is dropped **silently** -- no error, no nil
return, the function still hands back the window.

### Evidence from the user's own live daemon (read-only probe, 2026-09-04)

Three frames, three different states, in one Emacs:

| frame | window-system | char-w | total | body | fringes | `quit-restore` slot 1 |
|---|---|---|---|---|---|---|
| `cloudcitydotgay` | ns | 9 | 32 | 31 | `(0 0)` | **the buffer quadruple** |
| `edmacs` | ns | 9 | 32 | 30 | `(8 8)` | `window` |
| `F1` (daemon boot frame) | tty | 1 | 26 | 25 | `(0 0)` | `window` |

`cloudcitydotgay`'s sidebar buffer had been recreated -- the window shows
`*sidebar: cloudcitydotgay*<2>` while its `quit-restore` still records
`*sidebar: cloudcitydotgay*`. That single buffer re-creation permanently
disabled collapse on that frame. Its fringes are also `(0 0)` while
`edmacs-sidebar-collapsed` is `nil`: a past collapse zeroed them and nothing
ever restored them.

**Lesson for the process:** the expensive mistake was not "we ran it in
batch". It was "we ran it against a freshly created window and generalised to
a window that had been alive for hours". A harness that only ever builds its
fixture from scratch reproduces exactly the state that hides this bug. Every
geometry test here therefore asserts against a **deliberately aged** window
state as well as a fresh one.

---

## 1. Can `--batch` fake GUI geometry?

**No for fringes and scroll bars. Yes, adequately, for everything else.**

Measured (reproduced under `script -q /dev/null` too):

```
emacs -Q --batch:  char-width=1  display-graphic-p=nil
  set (left-fringe . 8) (right-fringe . 8)  -> frame params ACCEPT the value
                                            -> window-fringes still (0 0 nil nil)
                                            -> window-body-width UNCHANGED
  set (left-fringe . 16)                    -> same: no effect
```

A batch frame stores `left-fringe`/`right-fringe` and ignores them. The gap
between `window-total-width` and `window-body-width` is **always exactly 1**
in batch, and is **2 to 5** on the GUI frames measured. `script -q /dev/null
emacs -Q --batch` behaves exactly like plain `--batch` here -- attaching a pty
gains a controlling terminal (so `make-frame` works) but not a window system.

The one thing batch *can* fake is "chrome wider than one column":
`set-window-margins` **is** honoured in batch and does shave
`window-body-width`. That is not fringes, but it exercises the same
arithmetic, and it is what the tier-1 surrogate test uses.

### Where the GUI genuinely diverges

`window-resize`'s IGNORE argument. Smallest total width reachable:

| environment | `IGNORE=nil` | `IGNORE=t` | `IGNORE='safe` |
|---|---|---|---|
| `--batch` | 10 | **2** | 2 |
| NS GUI, fringes 8/8, 17px scroll bar | 10 | **8** | 2 |
| NS GUI, fringes zeroed | 10 | **5** | 2 |

`IGNORE=t` drops `window-min-width` but still respects the *pixel* cost of
fringes and scroll bar, so it cannot reach a 4-column strip on a graphical
frame while it can in batch. **`'safe` is the only IGNORE value that behaves
the same everywhere.** This is a real batch false-positive generator, and
`edmacs-geometry-test-gui-resize-ignore-t-is-not-enough` pins it.

Also: `window-resize` past the minimum **signals** `(error "Cannot resize
window ...")`. It does not return nil. `edmacs-stack--resize-width` already
wraps it in `ignore-errors`, which is what makes the refusal silent.

### Chrome is not a constant

`window-total-width - window-body-width` measured: 1 (batch, tty), 2 (ns,
char-width 9, 8px fringes, no scroll bar), 5 (ns, char-width 7, 8px fringes,
17px scroll bar), 3 (same frame with fringes zeroed). It is a function of
`frame-char-width`, fringe pixels and scroll-bar pixels, and **no constant
compensation can be right**. `edmacs-sidebar-show`'s hard-coded `1+` was
correct only for the tty case it was measured on.

`(window-width . (body-columns . N))` sizes in *body pixels* and lands
exactly N body columns on both batch and GUI -- it is the right request form
-- but it is still subject to the `quit-restore` rule above, so it alone does
not fix anything.

---

## 2. The harness

Two tiers, both delivered.

### Tier 1 -- plain `--batch`, CI-able

```
emacs -Q --batch -l ert -l modules/git-common-dir.el \
      -l modules/window-geometry-live-test.el \
      -f ert-run-tests-batch-and-exit
```

Covers the `quit-restore` rule, the `window-min-width` refusal, and (via the
`set-window-margins` surrogate) the body-vs-total arithmetic. 5 tests, 3
skips (the GUI-only ones).

### Tier 2 -- real graphical frame

```
scripts/gui-ert.sh modules/window-geometry-live-test.el [SELECTOR] [-l extra.el ...]
```

`scripts/gui-ert.sh` + `scripts/gui-ert.el` start a **throwaway daemon** under
its own server name, make one off-screen `no-focus-on-map` NS frame, run ERT
inside that frame, print a report and kill the daemon. It never touches the
user's `server` daemon and never sets `--init-directory`, so it cannot
bootstrap a second straight tree -- **safe to run from a worktree**. Exits
non-zero on failure. 8 tests, 0 skips.

`script -q /dev/null emacs -Q -nw` was **not** needed: a pty gets you a real
tty frame, which has no fringes either, so it buys nothing over `--batch` for
this question. It remains useful only for the existing multi-frame tests.

### Falsifiable outcome, as demanded

`modules/window-geometry-live-test.el`, run against unmodified `main`:

| tier | result |
|---|---|
| batch | **3 FAILED**, 2 passed (characterisation), 3 skipped |
| GUI | **5 FAILED**, 3 passed |

After the fix in this branch's `modules/sidebar.el`:

| tier | result |
|---|---|
| batch | 5 passed, 0 failed, 3 skipped |
| GUI | **8 passed, 0 failed, 0 skipped** |

No regressions: `sidebar-test.el` 126 tests / 124 as expected / 0 unexpected /
2 skipped -- identical to the pre-fix baseline. `windows-test.el` 108/108, 0
unexpected. `sidebar.el` byte-compiles clean.

---

## 3. Bugs the harness caught

1. **A reused side window is not resized** -- `edmacs-sidebar-collapse` relied
   entirely on `display-buffer-in-side-window` to set the width, so collapse
   was a silent no-op on every desktop-restored frame and on every frame whose
   sidebar buffer had ever been recreated.
2. **`window-resize` needs `'safe`, not `t`** -- the collapsed strip wants 5
   columns and `window-min-width` is 10; `t` is enough in batch and not enough
   on a GUI frame.
3. **The strip's producers got a width the window did not have** -- the
   hard-coded `1+` chrome compensation is right only where chrome costs one
   column.
4. **(new, found live) `edmacs-sidebar-expand` never restores fringes.**
   `edmacs-sidebar-show` zeroes them on collapse and nothing undoes it, so an
   expanded sidebar keeps zero fringes forever. Confirmed on the user's own
   running daemon before it was written as a test.

### The fix on this branch

`edmacs-sidebar-show` now (a) requests `(body-columns . N)` for the collapsed
case instead of `(1+ N)` total columns, (b) sets fringes *before* the width is
settled and restores the frame default on expand, and (c) calls a new
`edmacs-sidebar--enforce-width`, which corrects the width against the window's
own measured `window-body-width` with `IGNORE='safe`. It iterates up to three
times because chrome is not a whole number of columns -- a single computed
delta lands one column out on a GUI frame.

This fix is offered as proof the tests are falsifiable, not as a landed
change; it should be reviewed on its own terms.

---

## 4. The ambient-read lint

`scripts/ambient-reads.el` -- a plain source walk, not a byte-compiler warning
(the compiler sees macro-expanded code, where `if-let`/`when-let` have already
rewritten the argument lists the check depends on).

```
emacs -Q --batch -l scripts/ambient-reads.el -f edmacs-ambient-reads-batch modules/*.el
emacs -Q --batch -l scripts/ambient-reads.el \
      --eval '(setq edmacs-ambient-reads-only-errors t)' \
      -f edmacs-ambient-reads-batch modules/*.el   # exits 1 on any ERROR
```

Flags reads of `(selected-frame)`, `(selected-window)`, `(current-buffer)` and
`default-directory`, at two severities:

- **ERROR** -- the enclosing function *has* a parameter of the right kind and
  reads the ambient value anyway, outside the `(or PARAM (selected-frame))`
  defaulting idiom. This is the bug shape: the caller's argument is accepted
  and then ignored.
- **WARN** -- no such parameter; ambient by construction, a refactor candidate.

`let`-bindings and `setq` writes are excluded (they are the *correct* way to
use `default-directory`). The defaulting idiom is matched up to three sexp
levels out. A `;; ambient-reads: ok` comment on the line or the line above
suppresses a finding.

Current tree, all 44 files under `modules/`: **80 findings -- 2 ERROR, 78
WARN.** The SPIKE's count of 57 is the raw grep total across the seven
production modules; this tool's job is to separate the ~50 benign
`(or frame (selected-frame))` defaultings from the ones that matter.

The one true-positive ERROR:

```
modules/sidebar.el:819: edmacs-sidebar--anchor-region-to-bottom reads (selected-window)
```

That function is handed WINDOW and then guards its `set-window-point` on
`(eq window (selected-window))`. On any frame that is not the selected one --
i.e. every non-focused frame during a `window-size-change-functions` redraw --
the guard is false, point is never pulled forward, and the bottom anchor
silently loses to redisplay. Same family as `e5f202f`. **Not fixed here** (out
of this spike's scope); it wants its own change and test.

The other ERROR (`claude-term-approval-parity-live-test.el:202`) is a false
positive: the read is inside a nested `lambda` that deliberately captures the
caller's dynamic binding, and the tool attributes reads to the enclosing
top-level definition. That limitation is documented in the file's Commentary
and is what the suppression comment exists for.

---

## 5. What I could not determine

- **Why `cloudcitydotgay`'s sidebar buffer was recreated as `<2>`.** The
  consequence is proven and now tested; the trigger is not. Somewhere a
  frame's `edmacs-sidebar-buffer` parameter is lost while the old buffer stays
  alive. Worth its own investigation -- it is the mechanism that turns a
  transient state into a permanent one.
- **Whether the remembered-width unit is right.**
  `edmacs-sidebar--remember-width` stashes `(1+ (window-width window))`.
  `window-width` returns the **body** width, so on a GUI frame with 2 columns
  of chrome a manual resize to total 40 is stashed as 36 and comes back as
  total 36. The docstring attributes the offset to
  `display-buffer-in-side-window`; the measurements here say it is the
  body-vs-total gap, i.e. the same confusion as the collapsed `1+`. Stashing
  `window-total-width` would make the round trip exact, but it changes
  behaviour on the expanded path and breaks two pinned tests, so it is left
  alone. **Latent bug, unfixed.**
- **Non-macOS.** Everything graphical here was measured on NS.
  `scripts/gui-ert.el` picks a window system for X11 as a fallback but that
  path is untested. The `quit-restore` rule is pure Lisp and platform-free;
  the chrome arithmetic is not.
- **Whether a `window-configuration-change-hook` re-entry can undo an enforced
  width.** Not probed. The tests here assert immediately after
  `edmacs-sidebar-collapse` returns.

---

## 6. Recommendation

1. **Adopt the two-tier harness.** Tier 1 in the normal batch suite run; tier
   2 (`scripts/gui-ert.sh`) as a pre-landing gate for any change that touches
   window sizing. It costs about two seconds.
2. **Never assert geometry against a freshly created window alone.** Age the
   fixture -- clear `quit-restore`, or show a second buffer through the window
   -- and assert again. That single habit would have caught the original bug
   in batch.
3. **Stop compensating for chrome with constants.** Ask for
   `(body-columns . N)`, then correct against the window's own measured
   `window-body-width`, with `IGNORE='safe`.
4. **Treat a silent `display-buffer` width request as advisory, never
   authoritative.** It is dropped without a word in two states that occur in
   ordinary use.
5. **Run the ambient-read lint in errors-only mode as a gate**, once the
   `sidebar.el:819` true positive is fixed. Its WARN inventory is a useful map
   for the three sibling state-refactor spikes: it names exactly which
   functions those refactors have to thread an argument through.
6. **The three sibling spikes are now judgeable.** A state refactor that
   breaks the sidebar's geometry will fail tier 1 in batch and tier 2 on a
   real frame, and the lint will name any function that starts reading ambient
   state instead of taking it.
