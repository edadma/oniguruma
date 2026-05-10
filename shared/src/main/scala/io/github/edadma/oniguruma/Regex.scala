package io.github.edadma.oniguruma

/** Compiled regex — top-level entry point for matching. Build via
  * [[Regex.compile]], then call [[findFirstMatchIn]] /
  * [[findAllMatchesIn]] to drive the VM.
  *
  * Stage 3's API is deliberately small. `findFirstMatchIn` walks the
  * input position-by-position trying the program at each one;
  * `findAllMatchesIn` chains repeated searches, threading
  * `prevMatchEnd` so `\G` does the right thing. The TextMate-scanner
  * driver (Stage 4) will sit on top of these. */
final class Regex private[oniguruma] (val program: Program):

  /** Number of explicit capture groups. Slot count for the matcher is
    * `2 * (captureCount + 1)`. */
  def captureCount: Int = program.captureCount

  /** Map from each declared name to the list of capture indices that
    * share it (Onig's branch-reset can register the same name twice). */
  def namedGroups: Map[String, List[Int]] = program.namedGroups

  /** Disassembled bytecode for diagnostics. */
  def disassemble: String = program.disassemble

  /** Try to match starting at every UTF-16 offset of `input`, returning
    * the first successful [[Match]] or `None`. Anchors enforce
    * positional constraints inside the VM, so an anchored pattern that
    * only matches at sp=0 will simply backtrack at every other start. */
  def findFirstMatchIn(input: String): Option[Match] =
    findMatchAt(input, 0, 0)

  /** Try to match starting at exactly `start` (no scanning). Used by
    * the iterator that powers [[findAllMatchesIn]] and by callers
    * — like the TextMate scanner — that want full control of where to
    * resume. `prevMatchEnd` is what `\G` evaluates against. */
  def matchAt(input: String, start: Int, prevMatchEnd: Int): Option[Match] =
    runOnce(input, start, prevMatchEnd)

  /** Iterator over every non-overlapping match in `input`. Successive
    * calls to `next()` advance `sp` past each match (or by one
    * codepoint after a zero-width hit), and thread the previous match
    * end into `\G`. */
  def findAllMatchesIn(input: String): Iterator[Match] =
    new Iterator[Match]:
      private var pos: Int   = 0
      private var prev: Int  = 0
      private var pending: Option[Match] = None
      private var ended: Boolean = false

      private def advance(): Unit =
        if ended || pending.isDefined then ()
        else
          findMatchAt(input, pos, prev) match
            case Some(m) =>
              pending = Some(m)
              prev    = m.end
              // Zero-width matches force at least one codepoint of
              // progress so we don't infinite-loop at the same offset.
              if m.end == m.start then
                pos =
                  if m.end >= input.length then input.length + 1
                  else m.end + Character.charCount(Character.codePointAt(input, m.end))
              else pos = m.end
            case None =>
              ended = true

      def hasNext: Boolean =
        if pending.isEmpty && !ended then advance()
        pending.isDefined

      def next(): Match =
        if !hasNext then throw new NoSuchElementException("no more matches")
        val m = pending.get
        pending = None
        m

  /** Walk forward through `input` from `start` looking for a match
    * starting at any offset; bail out as soon as one succeeds. The VM
    * itself handles per-position retry logic via backtracking, so all
    * we do is bump `sp` codepoint-by-codepoint until either the VM
    * succeeds or we run off the end. */
  private def findMatchAt(input: String, start: Int, prevMatchEnd: Int): Option[Match] =
    var pos = start
    val len = input.length
    while pos <= len do
      runOnce(input, pos, prevMatchEnd) match
        case some @ Some(_) => return some
        case None           =>
          if pos >= len then return None
          val cp = Character.codePointAt(input, pos)
          pos += Character.charCount(cp)
    None

  private def runOnce(input: String, start: Int, prevMatchEnd: Int): Option[Match] =
    val vm = new VM(program)
    vm.run(input, start, prevMatchEnd).map(slots => new Match(input, slots, captureCount))

end Regex

object Regex:

  /** Compile a regex source string. Returns `Right(regex)` on success
    * or `Left(message)` describing a parse or compile error. */
  def compile(src: String): Either[String, Regex] =
    Parser.parseRegex(src) match
      case Left(err)  => Left(s"parse error at ${err.position}: ${err.message}")
      case Right(ast) =>
        try Right(new Regex(Compiler.compile(ast)))
        catch case e: CompileException => Left(s"compile error: ${e.error.message}")

  /** Throwing version — convenient at the REPL and in tests. */
  def apply(src: String): Regex = compile(src) match
    case Right(r)  => r
    case Left(msg) => throw new IllegalArgumentException(msg)

end Regex
