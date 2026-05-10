package io.github.edadma.oniguruma

/** Result of a successful match. Wraps the input string plus the slot
  * array the VM produced. Slot indexing convention: slot `2*n` holds
  * group `n`'s start UTF-16 index, slot `2*n+1` holds its end. Group 0
  * is the overall match. An unmatched group has `-1` in both slots
  * (e.g. a capture inside an alt arm that wasn't taken).
  *
  * Captures are reported in UTF-16 indices because that's what `String`
  * uses; supplementary codepoints occupy two indices each. The
  * [[matched]] / [[group]] accessors substring against the same indices
  * so callers see normal Scala strings.
  *
  * @param input  the source the match was run against
  * @param slots  the raw slot array; the constructor takes ownership
  *               and never mutates it after construction
  * @param groupCount  number of EXPLICIT capture groups (so valid `n`
  *                    range is `0..groupCount`) */
final class Match(
    val input: String,
    private val slots: Array[Int],
    val groupCount: Int,
):

  /** UTF-16 index where the overall match begins. */
  def start: Int = slots(0)

  /** UTF-16 index just past the end of the overall match. */
  def end: Int = slots(1)

  /** The matched substring. */
  def matched: String = input.substring(start, end)

  /** Substring for capture group `n`. `n == 0` returns the overall match;
    * `n >= 1` returns the named/unnamed capture. `None` when the group
    * was declared but not entered (e.g. on the un-taken alt arm). */
  def group(n: Int): Option[String] =
    groupRange(n).map((s, e) => input.substring(s, e))

  /** UTF-16 (start, end) range for capture group `n`, or `None` if the
    * group never matched. */
  def groupRange(n: Int): Option[(Int, Int)] =
    if n < 0 || n > groupCount then None
    else
      val s = slots(2 * n)
      val e = slots(2 * n + 1)
      if s < 0 || e < 0 then None else Some((s, e))

  /** Total number of slots backing this match. Useful for tests that
    * want to verify the slot vector was sized correctly. */
  def slotCount: Int = slots.length

  /** Read-only access to the raw slot array — intended for diagnostics
    * and the test suite. Returns a defensive copy. */
  def rawSlots: Array[Int] = slots.clone()

  override def toString: String =
    s"Match(start=$start, end=$end, matched=${matched.take(40)})"

end Match
