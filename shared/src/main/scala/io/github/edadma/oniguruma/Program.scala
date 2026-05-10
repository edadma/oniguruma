package io.github.edadma.oniguruma

/** A compiled regex program: the bytecode plus enough metadata to make
  * sense of its captures. Immutable; once built, [[VM]] reads it many
  * times.
  *
  * @param code         the instruction stream, addressed by index
  * @param captureCount the number of EXPLICIT capture groups (group 0
  *                     "the whole match" is in addition to this — total
  *                     slots is `2 * (captureCount + 1)`)
  * @param namedGroups  reverse map from declared name → list of capture
  *                     indices that share the name. Onig's branch-reset
  *                     idiom can register the same name several times;
  *                     `\k<name>` resolution at match time picks "the
  *                     one that actually matched" (Stage 4).
  * @param groupEntries map from group index → (entryPc, leaveAt) pairs
  *                     used to lower subroutine calls. Group 0 maps to
  *                     `(0, matchPc)` — i.e. the whole program, with
  *                     `\g<0>` recursion entering at the top and
  *                     leaving just before the terminal `Match`. Each
  *                     explicit group `n` maps to the pc just past its
  *                     opening `Save(2*n)` through the pc just past its
  *                     closing `Save(2*n+1)`. */
final case class Program(
    code: Vector[Inst],
    captureCount: Int,
    namedGroups: Map[String, List[Int]],
    groupEntries: Map[Int, (Int, Int)] = Map.empty,
):

  /** Pretty-print the bytecode for debugging — addresses left-aligned,
    * one instruction per line. Tests use this to assert against a
    * golden-text representation when the structural-equality check is
    * too brittle. */
  def disassemble: String =
    val sb = StringBuilder()
    for (inst, idx) <- code.zipWithIndex do
      sb.append(f"$idx%4d  ").append(inst).append('\n')
    sb.toString

end Program

object Program:

  /** Build an empty program — only used for "this should never run"
    * placeholders during partial test setup. */
  val Empty: Program = Program(Vector.empty, 0, Map.empty)

end Program
