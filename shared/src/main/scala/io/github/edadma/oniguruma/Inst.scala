package io.github.edadma.oniguruma

/** Bytecode instruction set for the backtracking VM. The shape is the
  * classic Pike/Cox style — `Char` / `Split` / `Jmp` / `Match` — extended
  * with `Save` for capture endpoints and an explicit `Atomic*` pair so
  * possessive quantifiers and `(?>…)` groups can clear their backtracks
  * on commit. Stage 4 added [[Inst.LookaroundEnter]] / [[Inst.LookaroundExit]]
  * for lookaround assertions, [[Inst.Backref]] for `\1` / `\k<name>`
  * back-references, and [[Inst.AnyCharIncludingNewline]] /
  * [[Inst.CharIgnoreCase]] to carry the `(?s)` and `(?i)` runtime-flag
  * adjustments. Stage 5 added [[Inst.Call]] for subroutine calls
  * (`\g<n>`, `\g<name>`, `\g<0>`).
  *
  * Instructions are addressed by their index in [[Program.code]]. `Split`,
  * `Jmp`, and `LookaroundEnter` carry absolute targets — the compiler
  * emits placeholders and patches them once both arms are known. */
sealed trait Inst

object Inst:

  /** Match exactly one literal codepoint. Advance `sp` by `Character.charCount(cp)`. */
  final case class Char(cp: Int) extends Inst

  /** `.` — match any one codepoint EXCEPT `\n`. This is the default Onig
    * interpretation when `(?s)` / `DotAll` is not active; under that flag
    * the compiler emits [[AnyCharIncludingNewline]] instead. */
  case object AnyChar extends Inst

  /** `.` under `(?s)` / `DotAll` — match any one codepoint, including
    * `\n`. Compiler emits this in place of [[AnyChar]] when the flag is
    * active at the dot's emit site. */
  case object AnyCharIncludingNewline extends Inst

  /** Match `cp` case-insensitively. Stage 4 only folds ASCII letters
    * (`A-Z` ↔ `a-z`); a Unicode `CaseFolding.txt`-driven version waits on
    * Stage 5's UCD tables. The compiler emits this in place of [[Char]]
    * for literal characters under `(?i)` / `IgnoreCase`. */
  final case class CharIgnoreCase(cp: Int) extends Inst

  /** Match a char class. `set` is the positive set; if `negated` is true,
    * the codepoint must lie OUTSIDE `set`. Two flavors are kept rather
    * than always pre-complementing because complementing a Unicode set
    * is more expensive than a single boolean flip. */
  final case class CharClass(set: IntervalSet, negated: Boolean) extends Inst

  /** Zero-width anchor — see [[AnchorKind]]. */
  final case class Anchor(kind: AnchorKind) extends Inst

  /** Capture-endpoint write. Slot `2*n` is group `n`'s start, slot
    * `2*n+1` is its end. Slot 0/1 hold the overall match. The VM rolls
    * the slot array back on backtrack via per-choice snapshots. */
  final case class Save(slot: Int) extends Inst

  /** Unconditional jump to the given absolute pc. */
  final case class Jmp(target: Int) extends Inst

  /** Two-target fork. Continue at `prefer`; on backtrack, try `alt`.
    * Both arms are encoded so the compiler can emit a placeholder and
    * patch later when the second arm's address is known. */
  final case class Split(prefer: Int, alt: Int) extends Inst

  /** Open an atomic region. Records the current backtrack-stack depth so
    * that [[AtomicCommit]] can drop everything pushed inside the region.
    * Used for `(?>…)` and the wrapping that turns a greedy quantifier
    * possessive. */
  case object AtomicMark extends Inst

  /** Close the atomic region opened by the matching [[AtomicMark]]:
    * pop the saved depth and truncate the choice stack to that depth.
    * Anything that fails after this point cannot re-enter the atomic
    * body — which is exactly the "no backtracking past here" guarantee. */
  case object AtomicCommit extends Inst

  /** Successful match — VM exits and reports captures. */
  case object Match extends Inst

  /** Open a lookaround assertion. Runs the sub-program at `pc+1` up to
    * the matching [[LookaroundExit]] at `exitPc` in a fresh VM state
    * (own choice / atomic stacks; sp restored on completion). Captures
    * inside the lookaround are discarded — Stage 4 keeps capture
    * propagation out of scope.
    *
    * On the parent VM:
    *   - `forward = true`: run the sub-program at the parent's current
    *     `sp`, looking for any match. Lookbehind (`forward = false`)
    *     scans every offset from 0 to current `sp` and asks the
    *     sub-program to land at exactly that boundary.
    *   - `negative = false` (positive): proceed past `exitPc` iff the
    *     sub-program matched.
    *   - `negative = true` (negative): proceed past `exitPc` iff the
    *     sub-program did NOT match.
    *
    * Either way, the parent's `sp` is left where it was — lookarounds
    * are zero-width.
    *
    * @param exitPc absolute address of the paired [[LookaroundExit]] */
  final case class LookaroundEnter(
      forward: Boolean,
      negative: Boolean,
      exitPc: Int,
  ) extends Inst

  /** Successful end of a lookaround sub-program — the sub-VM treats this
    * exactly like [[Match]], reporting "this candidate worked" back to
    * the [[LookaroundEnter]] that spawned it. The parent VM never
    * executes this instruction directly: the sub-VM detects it as the
    * sub-program's success terminator. */
  case object LookaroundExit extends Inst

  /** Match the captured substring of a previously-matched group as a
    * literal sequence of codepoints. `slot` is the START slot index
    * (`2 * n`); the END slot is implicitly `slot + 1`. If the referenced
    * group never matched (start = -1), the back-reference fails — this
    * is the conventional flavor and what Onig does too. A captured-empty
    * group (start = end) succeeds as a zero-width match. */
  final case class Backref(slot: Int, ignoreCase: Boolean) extends Inst

  /** Subroutine call into the body of a previously-defined group:
    * `\g<n>`, `\g<name>`, or `\g<0>` (whole-pattern recursion).
    *
    * Lowering uses an "inline body + leaveAt sentinel" scheme rather
    * than an explicit Return instruction. The compiler records, for
    * every group, the pc just past its opening `Save` (the body's
    * entry) and the pc just past its closing `Save` (one past the
    * body's last instruction). A `\g<n>` call site emits
    * `Call(entryPc, leaveAt)`; the VM pushes a frame with
    * `returnTo = pc + 1` and jumps to `entryPc`. Before dispatching
    * every instruction, the VM checks "is `pc == top-of-IP-stack's
    * leaveAt`? If so, pop the frame and resume at `returnTo`." That
    * way the same inline body serves both normal flow (caller falls
    * through past the `Save(2*n+1)` exit) and subroutine flow
    * (callee's frame fires at the same boundary).
    *
    * For `\g<0>`, `entryPc` is 0 and `leaveAt` is the pc of the
    * program's terminal `Match` — the recursion never actually
    * executes `Match` because the leaveAt check fires first.
    *
    * Captures inside a subroutine call do NOT propagate back to the
    * caller. The frame snapshots the slot array on push and restores
    * it on pop — PCRE-`(?R)`-style semantics rather than Onig-classic
    * "subroutine call rewrites group n's captures". For the
    * TextMate corpus's typical `\g<0>` balanced-bracket idioms this
    * is what's wanted; it's also what produces stable outer captures
    * for nested-recursion patterns. */
  final case class Call(entryPc: Int, leaveAt: Int) extends Inst

end Inst
