package io.github.edadma.oniguruma

import scala.annotation.tailrec

/** A sorted, disjoint, immutable set of inclusive codepoint intervals.
  *
  * The data structure is the unsung hero of the regex engine: every char
  * class, every POSIX bracket, every Unicode property class, and every
  * shorthand (`\d`, `\w`, `\s`, `\h`) collapses to one of these. By the
  * time the compiler sees a class, all the union/intersect/complement
  * decisions have been made — the VM only has to ask `contains(cp)`.
  *
  * Ranges are stored as a `Vector[(Int, Int)]` of inclusive `(lo, hi)`
  * pairs, sorted by `lo`, with no two pairs adjacent (`hi(i)+1 < lo(i+1)`)
  * and no two overlapping. The constructor enforces the invariant; any
  * factory that takes user-supplied input runs it through [[normalize]].
  *
  * The "universe" of codepoints is `[0, 0x10FFFF]` — the Unicode scalar
  * range. Surrogate codepoints `0xD800..0xDFFF` are valid members of an
  * [[IntervalSet]]; the engine itself is responsible for never decoding
  * a UTF-16 surrogate to a standalone codepoint at match time.
  */
final class IntervalSet private (val ranges: Vector[(Int, Int)]):

  /** True iff the set has no codepoints. */
  inline def isEmpty: Boolean = ranges.isEmpty

  /** True iff the set has at least one codepoint. */
  inline def nonEmpty: Boolean = ranges.nonEmpty

  /** Number of disjoint intervals (not codepoints). */
  inline def size: Int = ranges.size

  /** Total codepoint count. Cheap because we only iterate the ranges. */
  def codepointCount: Long =
    var sum = 0L
    var i   = 0
    while i < ranges.length do
      val (lo, hi) = ranges(i)
      sum += (hi - lo + 1).toLong
      i += 1
    sum

  /** Binary-search membership test, O(log n) in the number of intervals. */
  def contains(cp: Int): Boolean =
    var lo = 0
    var hi = ranges.length
    while lo < hi do
      val mid      = (lo + hi) >>> 1
      val (a, b)   = ranges(mid)
      if cp < a then hi = mid
      else if cp > b then lo = mid + 1
      else return true
    false

  /** Set union. Both inputs are sorted+disjoint, so this is a merge. */
  infix def union(other: IntervalSet): IntervalSet =
    if isEmpty then other
    else if other.isEmpty then this
    else
      val out = Vector.newBuilder[(Int, Int)]
      out.sizeHint(ranges.size + other.ranges.size)
      var i = 0
      var j = 0
      val a = ranges
      val b = other.ranges
      // First, interleave by `lo` so the merge that follows is a single pass.
      val merged = Array.ofDim[(Int, Int)](a.size + b.size)
      var k = 0
      while i < a.length && j < b.length do
        if a(i)._1 <= b(j)._1 then
          merged(k) = a(i); i += 1
        else
          merged(k) = b(j); j += 1
        k += 1
      while i < a.length do { merged(k) = a(i); i += 1; k += 1 }
      while j < b.length do { merged(k) = b(j); j += 1; k += 1 }

      // Then collapse: any (lo, hi) whose `lo` is within `curHi+1` extends
      // the current run. The +1 is what merges adjacent ranges like
      // [1..3] and [4..6] into [1..6].
      var (curLo, curHi) = merged(0)
      var idx            = 1
      while idx < merged.length do
        val (lo, hi) = merged(idx)
        if lo <= curHi + 1 then
          if hi > curHi then curHi = hi
        else
          out += ((curLo, curHi))
          curLo = lo
          curHi = hi
        idx += 1
      out += ((curLo, curHi))
      new IntervalSet(out.result())

  /** Set intersection. Walk both sorted lists in tandem, emitting the
    * overlap each time the current pair from each side overlaps. */
  infix def intersect(other: IntervalSet): IntervalSet =
    if isEmpty || other.isEmpty then IntervalSet.empty
    else
      val out = Vector.newBuilder[(Int, Int)]
      val a   = ranges
      val b   = other.ranges
      var i   = 0
      var j   = 0
      while i < a.length && j < b.length do
        val (al, ah) = a(i)
        val (bl, bh) = b(j)
        val lo       = math.max(al, bl)
        val hi       = math.min(ah, bh)
        if lo <= hi then out += ((lo, hi))
        if ah <= bh then i += 1 else j += 1
      new IntervalSet(out.result())

  /** Set difference: `this \ other`. Walk `this` and subtract each
    * `other` range that overlaps. */
  infix def diff(other: IntervalSet): IntervalSet =
    if isEmpty then this
    else if other.isEmpty then this
    else
      val out = Vector.newBuilder[(Int, Int)]
      val a   = ranges
      val b   = other.ranges
      var i   = 0
      var j   = 0
      var (curLo, curHi) = a(0)
      var live           = true
      i += 1
      // We process `this` one range at a time, slicing it against `other`.
      // To keep the loop simple, restart `live=false` whenever a range is
      // exhausted, then advance `i` and load the next one.
      def loadNext(): Unit =
        if i < a.length then
          val (l, h) = a(i)
          curLo = l
          curHi = h
          live  = true
          i += 1
        else live = false

      // Fall through if the very first range was the empty set's.
      while live do
        // Skip `other` ranges entirely below current.
        while j < b.length && b(j)._2 < curLo do j += 1
        if j >= b.length || b(j)._1 > curHi then
          // No more `other` overlap touches the current range — emit and advance.
          out += ((curLo, curHi))
          loadNext()
        else
          val (bl, bh) = b(j)
          if bl > curLo then out += ((curLo, bl - 1))
          if bh < curHi then
            curLo = bh + 1
            j     += 1
          else
            // `other` swallowed the rest of current; move to next `this` range.
            loadNext()
      new IntervalSet(out.result())

  /** Complement relative to the full Unicode codepoint universe. */
  def complement: IntervalSet =
    IntervalSet.universe.diff(this)

  /** ASCII case-fold extension. For every ASCII letter (`A-Z` / `a-z`)
    * present in the set, also include its opposite-case mate. Stage 4
    * uses this to lower `(?i)`-flagged char classes; the full Unicode
    * fold (driven by `CaseFolding.txt`) waits for Stage 5's UCD tables.
    *
    * Implementation walks each interval and, where it overlaps either
    * ASCII alphabetic range, projects the overlap into the other case
    * and unions it back in. Cheap because the projection is just a
    * `±0x20` shift. */
  def caseFoldAscii: IntervalSet =
    if isEmpty then this
    else
      val UpperLo = 'A'.toInt
      val UpperHi = 'Z'.toInt
      val LowerLo = 'a'.toInt
      val LowerHi = 'z'.toInt
      var folded = IntervalSet.empty
      var i = 0
      while i < ranges.length do
        val (lo, hi) = ranges(i)
        // Upper-case slice → fold to lower
        val upLo = math.max(lo, UpperLo)
        val upHi = math.min(hi, UpperHi)
        if upLo <= upHi then
          folded = folded union IntervalSet.range(upLo + 0x20, upHi + 0x20)
        // Lower-case slice → fold to upper
        val loLo = math.max(lo, LowerLo)
        val loHi = math.min(hi, LowerHi)
        if loLo <= loHi then
          folded = folded union IntervalSet.range(loLo - 0x20, loHi - 0x20)
        i += 1
      this union folded

  /** Equality is structural over the canonicalised range list. */
  override def equals(other: Any): Boolean = other match
    case s: IntervalSet => ranges == s.ranges
    case _              => false

  override def hashCode: Int = ranges.hashCode

  /** Human-readable form: `[U+0041-U+005A, U+0061-U+007A]`.  Single-point
    * ranges render as just `U+XXXX`. */
  override def toString: String =
    ranges
      .map:
        case (lo, hi) if lo == hi => f"U+$lo%04X"
        case (lo, hi)             => f"U+$lo%04X-U+$hi%04X"
      .mkString("[", ", ", "]")

end IntervalSet

object IntervalSet:

  /** The empty set — no codepoints. */
  val empty: IntervalSet = new IntervalSet(Vector.empty)

  /** The whole Unicode codepoint universe `[0, 0x10FFFF]`. */
  val universe: IntervalSet = new IntervalSet(Vector((0, 0x10FFFF)))

  /** Set containing exactly one codepoint. */
  def single(cp: Int): IntervalSet =
    require(cp >= 0 && cp <= 0x10FFFF, s"codepoint out of Unicode range: $cp")
    new IntervalSet(Vector((cp, cp)))

  /** Set containing one inclusive range. `lo > hi` produces the empty set. */
  def range(lo: Int, hi: Int): IntervalSet =
    require(lo >= 0 && hi <= 0x10FFFF, s"range out of Unicode range: [$lo, $hi]")
    if lo > hi then empty else new IntervalSet(Vector((lo, hi)))

  /** Build from arbitrary unsorted, possibly-overlapping ranges. */
  def of(ranges: (Int, Int)*): IntervalSet =
    if ranges.isEmpty then empty
    else
      ranges.foreach:
        case (lo, hi) =>
          require(lo >= 0 && hi <= 0x10FFFF, s"range out of Unicode range: [$lo, $hi]")
      new IntervalSet(normalize(ranges.toVector))

  /** Build from a Scala character iterable — handy for tests. */
  def fromChars(chars: Iterable[Char]): IntervalSet =
    if chars.isEmpty then empty
    else of(chars.map(c => (c.toInt, c.toInt)).toSeq*)

  /** Build from an iterable of inclusive `Int` codepoints. */
  def fromCodepoints(cps: Iterable[Int]): IntervalSet =
    if cps.isEmpty then empty
    else of(cps.map(cp => (cp, cp)).toSeq*)

  /** Sort, drop reversed pairs, merge overlapping and adjacent ranges. */
  private def normalize(input: Vector[(Int, Int)]): Vector[(Int, Int)] =
    val cleaned = input.filter { case (l, h) => l <= h }
    if cleaned.isEmpty then Vector.empty
    else
      val sorted = cleaned.sortBy(_._1)
      val out    = Vector.newBuilder[(Int, Int)]
      var (curLo, curHi) = sorted.head
      var i              = 1
      while i < sorted.length do
        val (lo, hi) = sorted(i)
        if lo <= curHi + 1 then
          if hi > curHi then curHi = hi
        else
          out += ((curLo, curHi))
          curLo = lo
          curHi = hi
        i += 1
      out += ((curLo, curHi))
      out.result()

end IntervalSet
