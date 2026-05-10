package io.github.edadma.oniguruma

import scala.collection.mutable

/** Bytecode error raised when the AST contains a construct the compiler
  * doesn't yet know how to lower. The parser resolves relative refs
  * (`\k<-1>`/`\g<+1>`) to absolute indices before lowering, so the
  * `ByRelative` cases below are defensive backstops for hand-built ASTs
  * — they should never fire from a parsed regex. The [[message]] names
  * the offending construct so the [[Regex.compile]] caller can surface
  * it directly. */
final case class CompileError(message: String)

private[oniguruma] final class CompileException(val error: CompileError)
    extends RuntimeException(error.message)

/** AST → [[Program]] lowering. The compiler emits instructions into a
  * single mutable buffer, patches `Split` / `Jmp` / `LookaroundEnter`
  * targets once both arms are positioned, then freezes the result.
  *
  * Stage 4 supports: literals, concat, alternation, dot, char classes,
  * groups (capturing/non-capturing/atomic), all three quantifier modes,
  * anchors, comments (skipped), [[Node.FlagsScope]] (with effective
  * `IgnoreCase` and `DotAll` applied at emit-time), [[Node.Lookaround]]
  * (forward + backward, both polarities), and [[Node.Backref]]
  * (numeric + named, including the branch-reset multi-slot form).
  * Stage 5 added subroutine calls; Stage 6.C (relative refs) is parser
  * resolution, so the compiler sees only absolute `ByNumber`/`ByName`.
  *
  * Compile-time invariants:
  *   - each capturing group has a stable 1-based index assigned by the
  *     parser, so `Save` slots fall out of `2 * idx` / `2 * idx + 1`;
  *   - `Save 0` / `Save 1` wrap the whole program so the VM never has
  *     to special-case "the overall match" — it's just group 0;
  *   - the program always ends in `Match`. */
object Compiler:

  /** Compile an AST into a runnable [[Program]], or throw
    * [[CompileException]] (caught by [[Regex.compile]]) on an
    * unsupported construct. */
  def compile(node: Node): Program =
    val captureCount = analyseCaptureCount(node)
    val namedGroups  = analyseNamedGroups(node)
    val c            = new Compiler(captureCount, namedGroups)
    c.emit(Inst.Save(0))
    c.compileNode(node, Flags.Empty)
    c.emit(Inst.Save(1))
    val matchPc = c.emit(Inst.Match)
    // Group 0 is the whole pattern: \g<0> recurses by jumping to pc 0
    // and exiting the moment pc reaches the terminal Match.
    c.recordGroupEntry(0, entry = 0, leaveAt = matchPc)
    c.patchPendingCalls()
    Program(c.code.toVector, captureCount, namedGroups, c.groupEntries.toMap)

  /** Walk the AST and find the maximum capturing-group index. The
    * parser assigns indices left-to-right as groups open, so the count
    * equals the largest index seen. */
  private def analyseCaptureCount(node: Node): Int =
    var maxIdx = 0
    def walk(n: Node): Unit = n match
      case Node.Group(GroupKind.Capturing(idx, _), body) =>
        if idx > maxIdx then maxIdx = idx
        walk(body)
      case Node.Group(_, body)         => walk(body)
      case Node.Concat(items)          => items.foreach(walk)
      case Node.Alt(branches)          => branches.foreach(walk)
      case Node.Quantified(body, _)    => walk(body)
      case Node.Lookaround(_, _, body) => walk(body)
      case Node.FlagsScope(_, _, b)    => b.foreach(walk)
      case _                           => ()
    walk(node)
    maxIdx

  /** Build the name → indices map. A name may map to several indices
    * (Onig's branch-reset idiom: `(?<x>a)|(?<x>b)`). The list preserves
    * left-to-right declaration order. */
  private def analyseNamedGroups(node: Node): Map[String, List[Int]] =
    val acc = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[Int]]
    def walk(n: Node): Unit = n match
      case Node.Group(GroupKind.Capturing(idx, Some(name)), body) =>
        acc.getOrElseUpdate(name, mutable.ListBuffer.empty) += idx
        walk(body)
      case Node.Group(_, body)         => walk(body)
      case Node.Concat(items)          => items.foreach(walk)
      case Node.Alt(branches)          => branches.foreach(walk)
      case Node.Quantified(body, _)    => walk(body)
      case Node.Lookaround(_, _, body) => walk(body)
      case Node.FlagsScope(_, _, b)    => b.foreach(walk)
      case _                           => ()
    walk(node)
    acc.view.mapValues(_.toList).toMap

end Compiler

/** One compile session — keeps the emit buffer + patch helpers private. */
private final class Compiler(captureCount: Int, namedGroups: Map[String, List[Int]]):
  import Inst.*

  /** The instruction buffer. Indexed by absolute pc. */
  private[oniguruma] val code: mutable.ArrayBuffer[Inst] =
    mutable.ArrayBuffer.empty

  /** Per-group `(entryPc, leaveAt)` pairs used to lower `\g<…>`. Group 0
    * is registered after the program is fully emitted (whole-program
    * recursion); each explicit group records its body span as the
    * compiler exits the group. */
  private[oniguruma] val groupEntries: mutable.Map[Int, (Int, Int)] =
    mutable.Map.empty

  /** Subroutine-call sites that still need patching once their target
    * group's entry is known. Each entry is the call-site address paired
    * with the group reference it points at. Resolved in
    * [[patchPendingCalls]] at the very end of compilation, which is the
    * earliest point at which every group's body has been emitted. */
  private val pendingCalls: mutable.ListBuffer[(Int, GroupRef)] =
    mutable.ListBuffer.empty

  /** Append `i` and return its address (so callers can patch it later). */
  def emit(i: Inst): Int =
    val a = code.size
    code += i
    a

  /** Replace the instruction at `addr` with `i`. Used to patch
    * forward-reference `Split` / `Jmp` / `LookaroundEnter` targets once
    * both arms are positioned in the buffer. */
  def patch(addr: Int, i: Inst): Unit =
    code(addr) = i

  /** Current pc (i.e. the address of the NEXT instruction to be emitted). */
  def here: Int = code.size

  // -- main lowering --

  /** Lower one AST node under the given effective flag set. The flags
    * thread through siblings via [[compileConcat]] (so a leaked
    * `(?i)` inside a concat updates subsequent siblings' flags) and
    * inherit naturally into groups, alternation arms, quantified
    * bodies, and lookaround sub-bodies. */
  def compileNode(node: Node, flags: Flags): Unit = node match
    case Node.Empty                      => ()
    case Node.Literal(s)                 => compileLiteral(s, flags)
    case Node.Concat(items)              => compileConcat(items, flags)
    case Node.Dot                        =>
      emit(if Flags.has(flags, Flags.DotAll) then AnyCharIncludingNewline else AnyChar)
    case Node.Klass(set, neg)            =>
      val effSet = if Flags.has(flags, Flags.IgnoreCase) then set.caseFoldAscii else set
      emit(CharClass(effSet, neg))
    case Node.Anchor(kind)               => emit(Inst.Anchor(kind))
    case Node.Group(kind, body)          => compileGroup(kind, body, flags)
    case Node.Alt(branches)              => compileAlt(branches, flags)
    case Node.Quantified(body, q)        => compileQuant(body, q, flags)
    case Node.FlagsScope(set, clear, b)  =>
      // Scoped form: compile body under the adjusted flag set; the
      // outer flag state is unchanged for siblings. The bare-leak form
      // (b == None) reaches here only when it's not inside a Concat;
      // in that position it has no body to lower, so emit nothing.
      b.foreach(body => compileNode(body, Flags.diff(Flags.union(flags, set), clear)))
    case Node.Comment(_)                 => ()
    case Node.Lookaround(fwd, neg, body) => compileLookaround(fwd, neg, body, flags)
    case Node.Backref(ref)               => compileBackref(ref, flags)
    case Node.SubroutineCall(ref)        => compileSubroutineCall(ref)

  /** Compile a concat, threading flag-leakage between siblings. A
    * bare `(?i)` (FlagsScope with `None` body) updates the running
    * flags for subsequent siblings; everything else compiles under
    * the current flags. */
  private def compileConcat(items: List[Node], flags: Flags): Unit =
    var f = flags
    for item <- items do
      item match
        case Node.FlagsScope(set, clear, None) =>
          f = Flags.diff(Flags.union(f, set), clear)
        case _ =>
          compileNode(item, f)

  /** Emit one instruction per codepoint in `s`. Under `IgnoreCase`,
    * ASCII letters get [[CharIgnoreCase]] (matching either case);
    * everything else is a plain [[Char]]. */
  private def compileLiteral(s: String, flags: Flags): Unit =
    val ic = Flags.has(flags, Flags.IgnoreCase)
    var i  = 0
    while i < s.length do
      val cp = Character.codePointAt(s, i)
      if ic && isAsciiLetter(cp) then emit(CharIgnoreCase(cp))
      else emit(Char(cp))
      i += Character.charCount(cp)

  private inline def isAsciiLetter(cp: Int): Boolean =
    (cp >= 'A' && cp <= 'Z') || (cp >= 'a' && cp <= 'z')

  /** Compile a group node. Capturing groups bracket the body with
    * `Save` instructions; non-capturing groups inline the body;
    * atomic groups bracket with `AtomicMark` / `AtomicCommit`. */
  private def compileGroup(kind: GroupKind, body: Node, flags: Flags): Unit = kind match
    case GroupKind.NonCapturing =>
      compileNode(body, flags)
    case GroupKind.Capturing(idx, _) =>
      emit(Save(2 * idx))
      val entryPc = here          // pc of the body's first instruction
      compileNode(body, flags)
      emit(Save(2 * idx + 1))
      val leaveAt = here          // pc just past the closing Save —
      // Subroutine calls into group `idx` use this entry/leave pair.
      // The `leaveAt` sentinel is matched by the VM's pre-dispatch
      // check, so the same inline body serves both normal flow and
      // subroutine flow without needing a separate exit instruction.
      recordGroupEntry(idx, entryPc, leaveAt)
    case GroupKind.Atomic =>
      emit(AtomicMark)
      compileNode(body, flags)
      emit(AtomicCommit)

  /** N-way alternation. The classic encoding chains `Split`s:
    * {{{
    *   Split B1, L2
    *   B1: <branch1>
    *       Jmp END
    *   L2: Split B2, L3
    *   B2: <branch2>
    *       Jmp END
    *   …
    *   LK: <branchK>
    *   END:
    * }}}
    * Two-arm `Split` lets the VM commit to the preferred branch first
    * and push the alternative as a backtrack point. */
  private def compileAlt(branches: List[Node], flags: Flags): Unit =
    branches match
      case Nil       => ()                  // an Alt() shouldn't reach here, but be safe
      case x :: Nil  => compileNode(x, flags)
      case _         =>
        val jmpEnds = mutable.ListBuffer.empty[Int]
        compileAltLoop(branches, jmpEnds, flags)
        val endPc = here
        // Patch every Jmp END to land at endPc.
        for addr <- jmpEnds do patch(addr, Jmp(endPc))

  private def compileAltLoop(
      rest: List[Node],
      jmpEnds: mutable.ListBuffer[Int],
      flags: Flags,
  ): Unit =
    rest match
      case last :: Nil =>
        compileNode(last, flags)
      case head :: tail =>
        val splitAddr = emit(Split(-1, -1))     // patched once we know both arms
        val preferPc  = here
        compileNode(head, flags)
        jmpEnds += emit(Jmp(-1))                // patched later to END
        val altPc     = here
        patch(splitAddr, Split(preferPc, altPc))
        compileAltLoop(tail, jmpEnds, flags)
      case Nil => ()

  // -- quantifiers --

  /** Lower a [[Node.Quantified]] node according to its mode. The mode
    * shows up in three places:
    *   - `Greedy`: `Split prefer-body alt-skip`
    *   - `Reluctant`: `Split prefer-skip alt-body` (arms swapped)
    *   - `Possessive`: same as `Greedy`, wrapped in `AtomicMark` /
    *     `AtomicCommit` so the body's choice points are dropped on
    *     commit. */
  private def compileQuant(body: Node, q: Quant, flags: Flags): Unit =
    q.mode match
      case Possessive =>
        emit(AtomicMark)
        compileQuantNonAtomic(body, q.copy(mode = Greedy), flags)
        emit(AtomicCommit)
      case _ =>
        compileQuantNonAtomic(body, q, flags)

  /** Lower the non-atomic shape — `Greedy` or `Reluctant`. The two
    * differ only in which arm of the loop `Split` is preferred; the
    * shared structure is "min mandatory copies, then max-min optional
    * copies, with the tail being open-ended if `q.max == None`". */
  private def compileQuantNonAtomic(body: Node, q: Quant, flags: Flags): Unit =
    val Quant(min, maxOpt, mode) = q
    if min < 0 then fail(s"negative quantifier min: $min")
    maxOpt.foreach(m => if m < min then fail(s"quantifier max < min: $m < $min"))

    // 1) Mandatory copies.
    var k = 0
    while k < min do
      compileNode(body, flags)
      k += 1

    // 2) Optional / open-ended tail.
    maxOpt match
      case Some(max) =>
        val extra = max - min
        var i = 0
        while i < extra do
          compileOptionalOnce(body, mode, flags)
          i += 1
      case None =>
        compileUnbounded(body, mode, flags)

  /** Compile one "may-or-may-not match" copy of the body — used for
    * the `m - n` optional iterations of a `{n,m}` quantifier and for
    * `?` / `??`. */
  private def compileOptionalOnce(body: Node, mode: QuantMode, flags: Flags): Unit =
    val splitAddr = emit(Split(-1, -1))
    mode match
      case Reluctant =>
        // prefer skip, alt body
        val skipPc = here                    // we'll fall through to here after the body
        val bodyPc = here
        compileNode(body, flags)
        val endPc  = here
        patch(splitAddr, Split(endPc, bodyPc)) // prefer skip(=end), alt body
      case _ =>
        // greedy / possessive — prefer body, alt skip
        val bodyPc = here
        compileNode(body, flags)
        val endPc  = here
        patch(splitAddr, Split(bodyPc, endPc))

  /** Compile an unbounded `*` / `+` tail. The body lives between two
    * splits/jmps; greedy prefers re-entering the loop, reluctant
    * prefers exiting. */
  private def compileUnbounded(body: Node, mode: QuantMode, flags: Flags): Unit =
    // Layout (greedy):
    //   Lstart: Split body, end       -- prefer body
    //   body:   <X>
    //           Jmp Lstart
    //   end:
    val startSplitAddr = emit(Split(-1, -1))
    val bodyPc         = here
    compileNode(body, flags)
    emit(Jmp(startSplitAddr))
    val endPc          = here
    mode match
      case Reluctant => patch(startSplitAddr, Split(endPc, bodyPc))
      case _         => patch(startSplitAddr, Split(bodyPc, endPc))

  // -- lookaround --

  /** Lower a lookaround assertion. Emits a [[LookaroundEnter]] /
    * [[LookaroundExit]] envelope around the sub-body; the VM will
    * spawn a fresh sub-state at the enter, treat reaching the exit as
    * "the candidate matched", and then either commit-and-skip or
    * backtrack depending on polarity. The exit is positioned right
    * after the body, so the parent VM continues at `exitPc + 1`
    * (zero-width: the parent's `sp` is unchanged). */
  private def compileLookaround(
      forward: Boolean,
      negative: Boolean,
      body: Node,
      flags: Flags,
  ): Unit =
    val enterAddr = emit(LookaroundEnter(forward, negative, -1))
    compileNode(body, flags)
    val exitAddr  = emit(LookaroundExit)
    patch(enterAddr, LookaroundEnter(forward, negative, exitAddr))

  // -- backreferences --

  /** Lower a `\1` / `\k<name>` backreference. Named refs resolve
    * through [[namedGroups]]. The branch-reset case (one name → many
    * indices) lowers to an alternation of single-slot backrefs so the
    * VM can pick whichever matched.
    *
    * Numeric refs are NOT validated against the running capture count.
    * Oniguruma C accepts `\N` for any positive `N`, with a runtime
    * semantic of "behave like an uncaptured group" (i.e. always
    * backtrack) when the referenced group doesn't exist in the
    * pattern. Several real TextMate grammars rely on this: a pattern
    * like `(\3)|FALLBACK` has only one group but uses `\3` as a
    * guaranteed never-match in the first arm, so the alt always
    * falls through to the second. The VM's `Backref` case
    * bounds-checks the slot index against the actual slot-array
    * size and treats out-of-range as uncaptured. We still reject
    * `n < 1` because group 0 is the whole-match: `\0` would mean
    * "match what I've matched so far", which has no well-defined
    * semantics here.
    *
    * Relative refs (`\k<-1>` / `\k<+2>`) are resolved to absolute
    * `ByNumber` by the parser, so the [[GroupRef.ByRelative]] arm here
    * only fires for hand-built ASTs that bypass the parser. */
  private def compileBackref(ref: GroupRef, flags: Flags): Unit =
    val ic = Flags.has(flags, Flags.IgnoreCase)
    ref match
      case GroupRef.ByNumber(n) =>
        if n < 1 then
          fail(s"backreference to invalid group number: \\$n")
        emit(Backref(2 * n, ic))
      case GroupRef.ByName(name) =>
        namedGroups.get(name) match
          case None | Some(Nil) =>
            fail(s"backreference to undefined named group: \\k<$name>")
          case Some(idx :: Nil) =>
            emit(Backref(2 * idx, ic))
          case Some(indices) =>
            // Branch-reset: emit `Split A1,A2 / A1: Backref(slot1) / Jmp END /
            // A2: Split A2a,A3 / A2a: Backref(slot2) / Jmp END / …`. Same
            // shape as compileAlt but with single-instruction arms.
            compileBackrefBranchReset(indices, ic)
      case GroupRef.ByRelative(_) =>
        fail("internal: relative backref reached the compiler (parser should have resolved it)")

  /** Branch-reset alt-of-backrefs lowering. Mirrors [[compileAlt]]'s
    * two-arm `Split` chain but each arm is a single [[Backref]]
    * instruction. */
  private def compileBackrefBranchReset(indices: List[Int], ic: Boolean): Unit =
    val jmpEnds = mutable.ListBuffer.empty[Int]
    def loop(rest: List[Int]): Unit = rest match
      case Nil => ()
      case last :: Nil =>
        emit(Backref(2 * last, ic))
      case head :: tail =>
        val splitAddr = emit(Split(-1, -1))
        val preferPc  = here
        emit(Backref(2 * head, ic))
        jmpEnds += emit(Jmp(-1))
        val altPc     = here
        patch(splitAddr, Split(preferPc, altPc))
        loop(tail)
    loop(indices)
    val endPc = here
    for addr <- jmpEnds do patch(addr, Jmp(endPc))

  // -- subroutine calls --

  /** Lower a `\g<…>` subroutine call. Numeric refs are validated against
    * `0 .. captureCount` (where 0 is whole-pattern recursion). Named
    * refs resolve through [[namedGroups]] — only the single-index case
    * is accepted for subroutine calls; a branch-reset name with several
    * indices is rejected because there's no obvious "which one" to
    * call. Relative calls (`\g<+1>` / `\g<-1>`) are resolved to
    * absolute `ByNumber` by the parser; the [[GroupRef.ByRelative]]
    * arm here only fires for hand-built ASTs.
    *
    * Emits a `Call(-1, -1)` placeholder; [[patchPendingCalls]]
    * substitutes the actual entry/leave once compilation is done. */
  private def compileSubroutineCall(ref: GroupRef): Unit =
    ref match
      case GroupRef.ByNumber(n) =>
        if n < 0 || n > captureCount then
          fail(s"subroutine call to undefined group: \\g<$n> (only $captureCount declared)")
        val addr = emit(Call(-1, -1))
        pendingCalls += ((addr, ref))
      case GroupRef.ByName(name) =>
        namedGroups.get(name) match
          case None | Some(Nil) =>
            fail(s"subroutine call to undefined named group: \\g<$name>")
          case Some(_ :: Nil) =>
            val addr = emit(Call(-1, -1))
            pendingCalls += ((addr, ref))
          case Some(_) =>
            fail(s"subroutine call to branch-reset name '$name' is ambiguous")
      case GroupRef.ByRelative(_) =>
        fail("internal: relative subroutine call reached the compiler (parser should have resolved it)")

  /** Record a group's body span for later subroutine-call patching.
    * Idempotent in practice — group indices are unique. Called from
    * [[compileGroup]] for each capturing group as the compiler emits
    * its closing `Save`, and from [[Compiler.compile]] for group 0
    * once the whole program has been emitted. */
  private[oniguruma] def recordGroupEntry(idx: Int, entry: Int, leaveAt: Int): Unit =
    groupEntries(idx) = (entry, leaveAt)

  /** Resolve every pending Call placeholder to its target group's
    * `(entry, leaveAt)` pair. Runs once at the end of [[Compiler.compile]]
    * — the earliest point at which every group's body has been emitted,
    * which lets a `\g<n>` forward-reference (the call site precedes the
    * group's definition in source order) resolve cleanly. */
  private[oniguruma] def patchPendingCalls(): Unit =
    for (addr, ref) <- pendingCalls do
      val idx = ref match
        case GroupRef.ByNumber(n)   => n
        case GroupRef.ByName(name)  => namedGroups(name).head
        case GroupRef.ByRelative(_) => -1   // fail() above kept these out
      val (entry, leaveAt) = groupEntries.getOrElse(
        idx,
        fail(s"internal error: no entry recorded for group $idx"),
      )
      patch(addr, Call(entry, leaveAt))

  // -- error helper --

  private def fail(msg: String): Nothing =
    throw CompileException(CompileError(msg))

end Compiler
