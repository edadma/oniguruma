package io.github.edadma.oniguruma

import scala.collection.mutable

/** Backtracking executor for [[Program]]. One instance per match
  * attempt; the [[run]] method returns the slot array on success or
  * `None` on no-match.
  *
  * The VM keeps three pieces of mutable state:
  *   - `pc` / `sp` — current instruction pointer and UTF-16 offset
  *     into the input;
  *   - a slot array sized `2 * (captureCount + 1)`, init `-1`, with
  *     entries written by `Save`;
  *   - a `choiceStack` of [[VM.Choice]] entries. Each `Split` pushes
  *     one; each `backtrack()` pops one. Entries snapshot `sp` and the
  *     full slot array so capture state rolls back cleanly.
  *
  * Atomic regions add a parallel `atomicStack` of choice-stack-depth
  * marks: `AtomicMark` pushes the current depth, `AtomicCommit` pops
  * the saved depth and truncates the choice stack back to it (dropping
  * every backtrack point pushed inside the region). To keep the
  * atomic stack consistent across backtracking, each pushed `Choice`
  * also records the atomic-stack depth at push time and the pop
  * restores it.
  *
  * Stage 4 added lookaround sub-execution. The step-loop is factored
  * into [[runOne]], parameterised by the "success" instruction the
  * sub-program should be looking for (`Match` for the top-level call,
  * `LookaroundExit` for a lookaround body) and an optional final-`sp`
  * constraint (used by lookbehind: a candidate succeeds only if the
  * sub-program lands at exactly the parent's current `sp`). Nested
  * lookarounds work by making another [[runOne]] call from inside the
  * `LookaroundEnter` case; each call has its own fresh stacks.
  *
  * Stage 5 added subroutine calls (`\g<…>`). The VM threads an `ipStack`
  * of [[VM.IpFrame]] entries through the loop: a `Call` pushes a frame
  * (return pc + leaveAt sentinel + slot snapshot) and jumps to the
  * target group's body entry; a pre-dispatch check at every step
  * notices "pc has reached the top frame's leaveAt" and pops the frame,
  * restoring slots and resuming at the caller. Pushed Choice points
  * snapshot the IP stack alongside the slot array so backtracking out
  * of a partial subroutine match unwinds frames cleanly. A recursion
  * cap on the IP stack prevents `\g<0>`-style infinite recursion.
  *
  * Stage 6.D added empty-body progress tracking. Each [[runOne]] keeps
  * a `splitLastSp` array sized to the program length: when execution
  * arrives at a `Split` whose pc was last visited at exactly the
  * current `sp`, the loop body must have made no progress on the
  * previous iteration, so the VM backtracks immediately instead of
  * retaking the loop arm. That collapses `(a*)*` against `"a"` from
  * the old 10M-step `StepLimit` ceiling to O(input length) steps, and
  * — being a per-Split check — has zero overhead for patterns that
  * actually advance. The [[StepLimit]] ceiling remains as a defensive
  * backstop for any pathology the progress check misses. */
final class VM(program: Program):

  import Inst.*

  /** Defensive ceiling on bytecode steps per [[runOne]]. With Stage 6.D
    * progress tracking the empty-body pathology is no longer the path
    * here — but the limit stays in place to catch any future bug or
    * adversarial input that slips past the per-Split check. */
  private val StepLimit: Int = 10_000_000

  /** Maximum subroutine-call recursion depth. Keeps `\g<0>` against
    * adversarial input from blowing the JVM stack. The cap is generous
    * enough that legitimately-balanced TextMate inputs (deeply nested
    * brackets) finish well below the limit. */
  private val MaxRecursionDepth: Int = 1_000

  private val captureCount = program.captureCount
  private val slotN        = 2 * (captureCount + 1)

  /** Run the program against `input` starting at UTF-16 offset
    * `startPos`, with `prevMatchEnd` as the answer to `\G`. Returns
    * `Some(slots)` with the captures on success, `None` on no-match. */
  def run(input: String, startPos: Int, prevMatchEnd: Int): Option[Array[Int]] =
    runOne(input, prevMatchEnd, startPc = 0, startSp = startPos,
           successInst = Match, requireEndAt = None)
      .map(_._1)

  /** One execution of the step loop. The parent call asks for `Match`
    * as the success terminator and no `sp` constraint; lookaround
    * sub-runs ask for `LookaroundExit` and (for lookbehind) a fixed
    * `requireEndAt`.
    *
    * Returns `Some((finalSlots, finalSp))` on success or `None` on
    * no-match. The parent uses only the slots; lookaround discards
    * both — the sub-run's only signal is the option's polarity. */
  private def runOne(
      input: String,
      prevMatchEnd: Int,
      startPc: Int,
      startSp: Int,
      successInst: Inst,
      requireEndAt: Option[Int],
  ): Option[(Array[Int], Int)] =
    // Check the JVM interrupt flag at every entry. The in-loop poll
    // below only fires every 65k bytecode steps, which is fine for
    // ONE long match — but callers that drive the engine in a
    // many-small-calls pattern (TextMate tokenizers calling
    // `matchAt` at every input position, for example) typically
    // average well under 65k steps per call. Without this entry-
    // point check the interrupt flag stays set across many runOne
    // calls and never gets consumed. Polling here makes the
    // cancellation surface latency-bounded by the duration of one
    // call instead of one in-loop poll interval.
    if Thread.interrupted() then
      throw new InterruptedException("oniguruma VM interrupted")
    val slots        = Array.fill(slotN)(-1)
    val choiceStack  = mutable.ArrayBuffer.empty[VM.Choice]
    val atomicStack  = mutable.ArrayBuffer.empty[Int]
    val ipStack      = mutable.ArrayBuffer.empty[VM.IpFrame]
    val len          = input.length

    // Per-Split last-visit-sp memo. `splitLastSp(pc) == sp` at dispatch
    // means we've already executed this Split at this exact offset and
    // came back without consuming input — i.e. the loop body around
    // this Split is a zero-width body, the classic `(a*)*` pathology.
    // The check fires immediately at the second arrival (no Choice is
    // pushed twice on top of itself), so the loop terminates in O(n)
    // steps instead of hitting `StepLimit`.
    //
    // We do NOT snapshot this on Choice push or restore it on
    // backtrack. The map's invariant is just "latest sp at which we
    // executed this Split"; backtracking that lowers sp simply leaves
    // a stale higher value behind, which never falsely fires the check
    // (different sp → proceed). The next legitimate visit at a
    // different sp overwrites it.
    //
    // The "-2" sentinel marks "never visited" — distinct from any
    // valid `sp` value, which is always `>= 0`.
    val splitLastSp = Array.fill(program.code.length)(-2)

    var pc        = startPc
    var sp        = startSp
    var done      = false
    var matched   = false
    var steps     = 0

    inline def backtrack(): Unit =
      if choiceStack.isEmpty then
        done = true
      else
        val ch = choiceStack.remove(choiceStack.size - 1)
        pc = ch.pc
        sp = ch.sp
        System.arraycopy(ch.slots, 0, slots, 0, slotN)
        // Atomic stack must agree with the choice stack — drop any marks
        // that were pushed after this choice point.
        while atomicStack.size > ch.atomicDepth do
          atomicStack.remove(atomicStack.size - 1)
        // Same for the IP stack: re-establish the call-frame state that
        // was in effect when this choice point was pushed. Restoration
        // is "shrink to that depth, then overwrite" so the array matches
        // the snapshot exactly without churning entries that didn't
        // change.
        while ipStack.size > ch.ipFrames.length do
          ipStack.remove(ipStack.size - 1)
        var i = 0
        while i < ch.ipFrames.length do
          if i < ipStack.size then ipStack(i) = ch.ipFrames(i)
          else ipStack += ch.ipFrames(i)
          i += 1

    while !done && steps < StepLimit do
      steps += 1

      // Cooperative cancellation: poll the JVM interrupt flag every
      // 65k steps so a caller running the VM on a worker thread can
      // kill a runaway match by calling `Thread.interrupt()`. The
      // mask check is a single AND, so the per-step cost is one
      // branch-on-zero and a constant compare — invisible in
      // benchmarks but it converts a hung VM into a thrown
      // InterruptedException within microseconds of the interrupt.
      // Without this, the StepLimit (10M steps) is the only out, and
      // a pathological pattern can pin a CPU for many seconds before
      // it hits that ceiling. The exception propagates up through
      // `runOne` / `run` / `Regex.findFirstMatchIn` / TmScanner and
      // is the caller's signal to abandon the match.
      //
      // On platforms where Thread.interrupted() is a no-op stub
      // (Scala.js, Scala Native), the check returns false and the VM
      // runs as before — those backends don't have the multi-thread
      // hung-call problem.
      if (steps & 0xFFFF) == 0 && Thread.interrupted() then
        throw new InterruptedException("oniguruma VM interrupted")

      // Subroutine-call return: if the top IP frame's `leaveAt` matches
      // the current pc, pop it before dispatching. Restores the slot
      // array (PCRE-`(?R)`-style: subroutine calls don't propagate
      // captures back) and resumes at the caller's `returnTo`. A while
      // loop handles the rare case where two frames close at the same
      // pc — successive returns happen in one step.
      while ipStack.nonEmpty && pc == ipStack.last.leaveAt do
        val frame = ipStack.remove(ipStack.size - 1)
        System.arraycopy(frame.slotsSnapshot, 0, slots, 0, slotN)
        pc = frame.returnTo

      val inst = program.code(pc)

      // The success terminator can be `Match` (top-level) or
      // `LookaroundExit` (sub-run). Treat whichever was requested as
      // the same success exit; the other one acts as a backtrack.
      if inst eq successInst then
        if requireEndAt.forall(_ == sp) then
          matched = true
          done = true
        else backtrack()
      else
        inst match
          case Char(cp) =>
            if sp < len then
              val c = Character.codePointAt(input, sp)
              if c == cp then
                sp += Character.charCount(c)
                pc += 1
              else backtrack()
            else backtrack()

          case CharIgnoreCase(cp) =>
            // ASCII-only fold for Stage 4. The compiler only emits this
            // for ASCII letters, so a simple `±0x20` check is enough.
            if sp < len then
              val c = Character.codePointAt(input, sp)
              if c == cp || (isAsciiLetter(cp) && (c ^ 0x20) == cp) then
                sp += Character.charCount(c)
                pc += 1
              else backtrack()
            else backtrack()

          case AnyChar =>
            if sp < len then
              val c = Character.codePointAt(input, sp)
              if c != '\n' then
                sp += Character.charCount(c)
                pc += 1
              else backtrack()
            else backtrack()

          case AnyCharIncludingNewline =>
            // `(?s)` / DotAll variant — the only difference is no
            // newline rejection.
            if sp < len then
              val c = Character.codePointAt(input, sp)
              sp += Character.charCount(c)
              pc += 1
            else backtrack()

          case CharClass(set, negated) =>
            if sp < len then
              val c    = Character.codePointAt(input, sp)
              val mem  = set.contains(c)
              if mem != negated then
                sp += Character.charCount(c)
                pc += 1
              else backtrack()
            else backtrack()

          case Inst.Anchor(kind) =>
            if checkAnchor(kind, input, sp, prevMatchEnd) then pc += 1
            else backtrack()

          case Save(slot) =>
            slots(slot) = sp
            pc += 1

          case Jmp(target) =>
            pc = target

          case Split(prefer, alt) =>
            // Stage 6.D progress check: if we've executed this Split at
            // exactly this sp before, the loop body must have looped back
            // without consuming input — fail this Split (the pushed
            // Choice from the FIRST visit is still on the stack and will
            // carry the matcher into the skip arm via backtracking).
            if splitLastSp(pc) == sp then
              backtrack()
            else
              splitLastSp(pc) = sp
              choiceStack += VM.Choice(alt, sp, slots.clone(), atomicStack.size, ipStack.toArray)
              pc = prefer

          case AtomicMark =>
            atomicStack += choiceStack.size
            pc += 1

          case AtomicCommit =>
            val depth = atomicStack.remove(atomicStack.size - 1)
            while choiceStack.size > depth do
              choiceStack.remove(choiceStack.size - 1)
            pc += 1

          case Backref(slot, ignoreCase) =>
            // Slot points at the START offset of the captured group;
            // slot+1 is the end. An unmatched group fails the backref.
            // A captured-empty group (start == end) succeeds with no
            // input consumed.
            //
            // A slot index BEYOND the slot-array length means the
            // referenced group number is larger than `captureCount`
            // for this pattern — `(\3)|...` and similar idioms from
            // the TextMate grammar corpus. Oniguruma accepts these
            // at parse time and behaves as "always fail" at runtime;
            // we mirror that by treating out-of-range as uncaptured.
            if slot + 1 >= slots.length then
              backtrack()
            else
              val gStart = slots(slot)
              val gEnd   = slots(slot + 1)
              if gStart < 0 || gEnd < 0 then
                backtrack()
              else if gStart == gEnd then
                pc += 1
              else if matchSubstring(input, sp, gStart, gEnd, ignoreCase) then
                sp += (gEnd - gStart)
                pc += 1
              else
                backtrack()

          case Call(entryPc, leaveAt) =>
            // Subroutine call — push a frame and jump into the target
            // group's body. The pre-dispatch leaveAt check at the top
            // of the loop handles the return when the body ends. A
            // depth cap protects against `\g<0>`-style infinite
            // recursion on adversarial input.
            if ipStack.size >= MaxRecursionDepth then
              backtrack()
            else
              ipStack += VM.IpFrame(returnTo = pc + 1, leaveAt = leaveAt, slotsSnapshot = slots.clone())
              pc = entryPc

          case LookaroundEnter(forward, negative, exitPc) =>
            // Spawn a sub-run that looks for `LookaroundExit` as its
            // success terminator. The parent's `sp` stays put either
            // way — lookarounds are zero-width.
            //
            // Stage 6.B: on success of a POSITIVE lookaround, merge
            // the sub-run's captures into the parent's slots. Group
            // slots the sub-run actually wrote (slot value `>= 0`)
            // overwrite the parent's; slots the sub-run never touched
            // leave the parent's value alone. Negative lookarounds
            // never propagate — they succeed on the sub-run's failure,
            // so there's no captured state to take.
            val subSlots =
              if forward then
                runOne(input, prevMatchEnd,
                       startPc = pc + 1, startSp = sp,
                       successInst = LookaroundExit,
                       requireEndAt = None).map(_._1)
              else
                lookbehindMatch(input, prevMatchEnd, subStartPc = pc + 1, parentSp = sp)
            val ok = subSlots.isDefined
            if ok != negative then
              if !negative then
                subSlots.foreach { arr =>
                  var i = 0
                  while i < slotN do
                    if arr(i) >= 0 then slots(i) = arr(i)
                    i += 1
                }
              pc = exitPc + 1
            else
              backtrack()

          case LookaroundExit =>
            // Parent VM should never see this — the sub-run is supposed
            // to stop at it. If it leaks through, something is wrong
            // with the bytecode shape; treat as a backtrack so we don't
            // silently miscount.
            backtrack()

          case Match =>
            // Reached the program's final Match while running as a
            // sub-program (successInst was LookaroundExit). That can
            // happen if the body somehow falls through — treat as a
            // backtrack.
            backtrack()

    if matched then Some((slots, sp)) else None

  end runOne

  /** Backward-lookbehind driver: try the sub-program at every UTF-16
    * boundary `i` from 0 up to `parentSp` inclusive, and succeed when
    * one of those starts ends exactly at `parentSp`.
    *
    * Returns `Some(slots)` with the sub-run's slot array on success
    * so the caller can merge captures back into the parent (Stage 6.B
    * positive-lookaround propagation). `None` means no start position
    * yielded a match ending at `parentSp`.
    *
    * This is the slow-but-correct algorithm — O(parentSp ×
    * subProgramSteps). The faster approach (compile a reverse-direction
    * sub-program) waits for a Stage 5+ rewrite. For the 36-grammar
    * TextMate corpus the pattern bodies are small and the inputs are
    * line-sized, so the slow form is fine. */
  private def lookbehindMatch(
      input: String,
      prevMatchEnd: Int,
      subStartPc: Int,
      parentSp: Int,
  ): Option[Array[Int]] =
    var i       = 0
    var result: Option[Array[Int]] = None
    var loop    = true
    while loop do
      runOne(input, prevMatchEnd,
             startPc = subStartPc, startSp = i,
             successInst = LookaroundExit,
             requireEndAt = Some(parentSp)) match
        case Some((slots, _)) =>
          result = Some(slots)
          loop   = false
        case None =>
          if i >= parentSp then loop = false
          else
            // Step forward one codepoint. If we'd overshoot `parentSp`, we
            // still want to test `i == parentSp` (zero-length lookbehind),
            // which the previous iteration already covered when i started
            // at parentSp.
            val cp = Character.codePointAt(input, i)
            val w  = Character.charCount(cp)
            if i + w > parentSp then
              // The next codepoint starts at i but would extend past
              // parentSp; advance i straight to parentSp so the next round
              // tries the zero-width-at-parentSp case explicitly.
              i = parentSp
            else
              i += w
    result

  /** ASCII-fold-aware substring match. Returns true iff `input[sp..]`
    * begins with the same sequence as `input[gStart..gEnd)`, comparing
    * codepoints case-insensitively when `ignoreCase` is set. The
    * codepoint walk handles supplementary characters correctly. */
  private def matchSubstring(
      input: String,
      sp: Int,
      gStart: Int,
      gEnd: Int,
      ignoreCase: Boolean,
  ): Boolean =
    val len  = input.length
    val need = gEnd - gStart
    if sp + need > len then false
    else
      var i = 0
      var ok = true
      while ok && i < need do
        val a = Character.codePointAt(input, gStart + i)
        val b = Character.codePointAt(input, sp + i)
        if a == b then ()
        else if ignoreCase && asciiFold(a) == asciiFold(b) then ()
        else ok = false
        i += Character.charCount(a)
      ok

  private inline def asciiFold(cp: Int): Int =
    if cp >= 'A' && cp <= 'Z' then cp + 0x20
    else cp

  private inline def isAsciiLetter(cp: Int): Boolean =
    (cp >= 'A' && cp <= 'Z') || (cp >= 'a' && cp <= 'z')

  // -- anchor evaluation --

  /** Test the named zero-width anchor at the given UTF-16 offset.
    * Onig's TextMate flavor treats `^` and `$` as line-mode by default
    * (matching at every newline boundary, not just input ends), so
    * this VM does the same regardless of any flags the AST carried —
    * Stage 4's flag plumbing won't change `^` / `$`. */
  private def checkAnchor(
      kind: AnchorKind,
      input: String,
      sp: Int,
      prevMatchEnd: Int,
  ): Boolean =
    kind match
      case AnchorKind.Caret =>
        sp == 0 || (sp > 0 && input.charAt(sp - 1) == '\n')

      case AnchorKind.Dollar =>
        sp == input.length || (sp < input.length && input.charAt(sp) == '\n')

      case AnchorKind.BeginInput =>
        sp == 0

      case AnchorKind.EndInput =>
        sp == input.length

      case AnchorKind.EndInputBeforeFinalNewline =>
        sp == input.length ||
          (sp == input.length - 1 && input.charAt(sp) == '\n')

      case AnchorKind.PrevMatchEnd =>
        sp == prevMatchEnd

      case AnchorKind.WordBoundary =>
        wordBoundary(input, sp)

      case AnchorKind.NonWordBoundary =>
        !wordBoundary(input, sp)

  /** A boundary is wherever the "word-ness" flips between the
    * codepoint immediately before `sp` and the one immediately at `sp`,
    * with input boundaries treated as non-word. The word-set definition
    * matches the parser's `\w` (ASCII: `[A-Za-z0-9_]`). */
  private def wordBoundary(input: String, sp: Int): Boolean =
    val left =
      if sp == 0 then false
      else isWord(Character.codePointBefore(input, sp))
    val right =
      if sp >= input.length then false
      else isWord(Character.codePointAt(input, sp))
    left != right

  private inline def isWord(cp: Int): Boolean =
    (cp >= '0' && cp <= '9') ||
      (cp >= 'A' && cp <= 'Z') ||
      (cp >= 'a' && cp <= 'z') ||
      cp == '_'

end VM

object VM:

  /** A pushed backtrack point. Records everything the VM needs to
    * restart the alternative after a failure: where to resume, where
    * `sp` was, the full capture-slot snapshot, the atomic-stack depth
    * (so atomic regions stay coherent across backtracking), and the
    * subroutine-call IP-stack snapshot (so backtracking out of a
    * partially-completed subroutine call unwinds frames correctly). */
  final case class Choice(
      pc: Int,
      sp: Int,
      slots: Array[Int],
      atomicDepth: Int,
      ipFrames: Array[IpFrame],
  )

  /** A pushed subroutine-call frame. `returnTo` is the pc to resume at
    * once the callee body completes (one past the `Call`). `leaveAt`
    * is the pc-sentinel that signals the body has ended — the VM's
    * pre-dispatch check pops the frame the moment `pc == leaveAt`.
    * `slotsSnapshot` is the slot array taken at push time; it's
    * restored on pop so subroutine calls don't propagate captures
    * back to the caller. */
  final case class IpFrame(returnTo: Int, leaveAt: Int, slotsSnapshot: Array[Int])

end VM
