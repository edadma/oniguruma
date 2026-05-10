package io.github.edadma.oniguruma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Unit coverage for [[IntervalSet]]. The data structure is the foundation
  * for char classes, Unicode properties, and shorthands; every regex
  * feature funnels through it, so every operation gets boundary coverage
  * here even when the cases look redundant. */
class IntervalSetSpec extends AnyFreeSpec with Matchers:

  // ---- construction --------------------------------------------------------

  "construction" - {

    "empty has no ranges and no codepoints" in {
      IntervalSet.empty.ranges shouldBe empty
      IntervalSet.empty.isEmpty shouldBe true
      IntervalSet.empty.nonEmpty shouldBe false
      IntervalSet.empty.codepointCount shouldBe 0L
    }

    "universe spans 0..0x10FFFF" in {
      IntervalSet.universe.ranges shouldBe Vector((0, 0x10FFFF))
      IntervalSet.universe.codepointCount shouldBe (0x10FFFF + 1).toLong
    }

    "single(cp) creates one-codepoint set" in {
      IntervalSet.single('A').ranges shouldBe Vector((65, 65))
      IntervalSet.single(0).ranges shouldBe Vector((0, 0))
      IntervalSet.single(0x10FFFF).ranges shouldBe Vector((0x10FFFF, 0x10FFFF))
    }

    "single(cp) rejects negative codepoints" in {
      an[IllegalArgumentException] should be thrownBy IntervalSet.single(-1)
    }

    "single(cp) rejects codepoints above 0x10FFFF" in {
      an[IllegalArgumentException] should be thrownBy IntervalSet.single(0x110000)
    }

    "range(lo, hi) builds an inclusive range" in {
      IntervalSet.range('a', 'z').ranges shouldBe Vector(('a'.toInt, 'z'.toInt))
    }

    "range(lo, hi) with lo == hi degenerates to a single" in {
      IntervalSet.range(5, 5).ranges shouldBe Vector((5, 5))
    }

    "range(lo, hi) with lo > hi gives the empty set" in {
      IntervalSet.range(10, 5).isEmpty shouldBe true
    }

    "of() with no args is empty" in {
      IntervalSet.of() shouldBe IntervalSet.empty
    }

    "of() sorts unsorted ranges" in {
      val s = IntervalSet.of((10, 20), (1, 5), (30, 40))
      s.ranges shouldBe Vector((1, 5), (10, 20), (30, 40))
    }

    "of() merges overlapping ranges" in {
      val s = IntervalSet.of((1, 10), (5, 15), (8, 12))
      s.ranges shouldBe Vector((1, 15))
    }

    "of() merges adjacent ranges (gap of 1 codepoint)" in {
      // [1..3] and [4..6] are *adjacent* (3+1 == 4) and should fuse.
      val s = IntervalSet.of((1, 3), (4, 6))
      s.ranges shouldBe Vector((1, 6))
    }

    "of() leaves a 2-codepoint gap intact" in {
      // [1..3] and [5..7] have one missing codepoint (4), must stay disjoint.
      val s = IntervalSet.of((1, 3), (5, 7))
      s.ranges shouldBe Vector((1, 3), (5, 7))
    }

    "of() drops reversed pairs (lo > hi) silently" in {
      val s = IntervalSet.of((10, 5), (1, 3))
      s.ranges shouldBe Vector((1, 3))
    }

    "of() handles a long shuffled run with overlaps" in {
      // (5,7) and (6,8) merge -> (5,8); (1,2) and (3,4) merge -> (1,4);
      // adjacency 4-5 is "(curHi=4)+1 == 5", so (1,4) ∪ (5,8) -> (1,8).
      val s = IntervalSet.of((6, 8), (3, 4), (1, 2), (5, 7))
      s.ranges shouldBe Vector((1, 8))
    }

    "fromChars builds from a string" in {
      IntervalSet.fromChars("abc") shouldBe IntervalSet.of(('a', 'a'), ('b', 'b'), ('c', 'c'))
      // and after normalization that is [a..c]
      IntervalSet.fromChars("abc").ranges shouldBe Vector(('a'.toInt, 'c'.toInt))
    }

    "fromChars on empty returns empty" in {
      IntervalSet.fromChars("").isEmpty shouldBe true
    }

    "fromCodepoints builds from arbitrary scalars" in {
      val s = IntervalSet.fromCodepoints(List(0x1F600, 0x1F601, 0x1F602)) // emoji range
      s.ranges shouldBe Vector((0x1F600, 0x1F602))
    }
  }

  // ---- contains ------------------------------------------------------------

  "contains" - {

    "empty contains nothing" in {
      IntervalSet.empty.contains(0) shouldBe false
      IntervalSet.empty.contains('a') shouldBe false
      IntervalSet.empty.contains(0x10FFFF) shouldBe false
    }

    "universe contains everything in [0, 0x10FFFF]" in {
      IntervalSet.universe.contains(0) shouldBe true
      IntervalSet.universe.contains(0x10FFFF) shouldBe true
      IntervalSet.universe.contains(0xD800) shouldBe true     // surrogate is still a codepoint
      IntervalSet.universe.contains(0x1F600) shouldBe true    // emoji
    }

    "single contains itself and nothing else" in {
      val s = IntervalSet.single(65)
      s.contains(65) shouldBe true
      s.contains(64) shouldBe false
      s.contains(66) shouldBe false
    }

    "range contains lo, hi, and middle but not the boundaries" in {
      val s = IntervalSet.range(10, 20)
      s.contains(9) shouldBe false
      s.contains(10) shouldBe true
      s.contains(15) shouldBe true
      s.contains(20) shouldBe true
      s.contains(21) shouldBe false
    }

    "multi-range contains within each, not in the gaps" in {
      val s = IntervalSet.of((1, 3), (10, 12), (20, 22))
      s.contains(2) shouldBe true
      s.contains(11) shouldBe true
      s.contains(21) shouldBe true
      s.contains(0) shouldBe false
      s.contains(5) shouldBe false
      s.contains(15) shouldBe false
      s.contains(25) shouldBe false
    }

    "binary search works with many ranges" in {
      // 1000 disjoint ranges scattered across the universe — the test
      // exercises the binary search far past the linear-scan size where
      // any indexing bug becomes load-bearing.
      val ranges = (0 until 1000).map(i => (i * 100, i * 100 + 50)).toVector
      val s      = IntervalSet.of(ranges*)
      s.contains(0) shouldBe true
      s.contains(50) shouldBe true
      s.contains(51) shouldBe false
      s.contains(99) shouldBe false
      s.contains(100) shouldBe true
      s.contains(99950) shouldBe true
      s.contains(99951) shouldBe false
    }
  }

  // ---- size, codepointCount ------------------------------------------------

  "size and codepointCount" - {

    "empty has size 0 and 0 codepoints" in {
      IntervalSet.empty.size shouldBe 0
      IntervalSet.empty.codepointCount shouldBe 0L
    }

    "single range size 1, codepointCount = hi - lo + 1" in {
      val s = IntervalSet.range(10, 20)
      s.size shouldBe 1
      s.codepointCount shouldBe 11L
    }

    "multi-range size and total" in {
      val s = IntervalSet.of((1, 3), (10, 12), (20, 30))
      s.size shouldBe 3
      s.codepointCount shouldBe (3 + 3 + 11).toLong
    }

    "universe codepointCount" in {
      IntervalSet.universe.codepointCount shouldBe 0x110000L
    }
  }

  // ---- union ---------------------------------------------------------------

  "union" - {

    "empty | empty = empty" in {
      (IntervalSet.empty union IntervalSet.empty) shouldBe IntervalSet.empty
    }

    "empty | x = x" in {
      val s = IntervalSet.range(10, 20)
      (IntervalSet.empty union s) shouldBe s
      (s union IntervalSet.empty) shouldBe s
    }

    "disjoint sets concatenate" in {
      val a = IntervalSet.range(1, 3)
      val b = IntervalSet.range(10, 12)
      (a union b).ranges shouldBe Vector((1, 3), (10, 12))
    }

    "adjacent ranges merge across union" in {
      val a = IntervalSet.range(1, 3)
      val b = IntervalSet.range(4, 6)
      (a union b).ranges shouldBe Vector((1, 6))
    }

    "touching ranges merge" in {
      val a = IntervalSet.range(1, 5)
      val b = IntervalSet.range(5, 10)
      (a union b).ranges shouldBe Vector((1, 10))
    }

    "overlapping ranges merge" in {
      val a = IntervalSet.range(1, 7)
      val b = IntervalSet.range(5, 10)
      (a union b).ranges shouldBe Vector((1, 10))
    }

    "containment merges to the larger" in {
      val a = IntervalSet.range(1, 100)
      val b = IntervalSet.range(20, 30)
      (a union b).ranges shouldBe Vector((1, 100))
    }

    "identical sets union to themselves" in {
      val a = IntervalSet.of((1, 5), (10, 15))
      (a union a) shouldBe a
    }

    "union with universe gives universe" in {
      val s = IntervalSet.range(100, 200)
      (s union IntervalSet.universe) shouldBe IntervalSet.universe
    }

    "interleaved multi-range union" in {
      val a = IntervalSet.of((1, 5), (20, 25), (40, 45))
      val b = IntervalSet.of((10, 15), (30, 35))
      (a union b).ranges shouldBe Vector((1, 5), (10, 15), (20, 25), (30, 35), (40, 45))
    }

    "interleaved with merging" in {
      // a: [1..5] [20..25] [40..45]
      // b: [4..21] [30..50]
      // result: [1..25] [30..50]
      val a = IntervalSet.of((1, 5), (20, 25), (40, 45))
      val b = IntervalSet.of((4, 21), (30, 50))
      (a union b).ranges shouldBe Vector((1, 25), (30, 50))
    }
  }

  // ---- intersect -----------------------------------------------------------

  "intersect" - {

    "empty & x = empty" in {
      val s = IntervalSet.range(1, 10)
      (IntervalSet.empty intersect s) shouldBe IntervalSet.empty
      (s intersect IntervalSet.empty) shouldBe IntervalSet.empty
    }

    "disjoint sets give empty" in {
      val a = IntervalSet.range(1, 5)
      val b = IntervalSet.range(10, 15)
      (a intersect b) shouldBe IntervalSet.empty
    }

    "adjacent (no overlap) gives empty" in {
      // [1..5] and [6..10] are adjacent for *union* purposes but have no
      // codepoints in common — intersect must be empty.
      val a = IntervalSet.range(1, 5)
      val b = IntervalSet.range(6, 10)
      (a intersect b) shouldBe IntervalSet.empty
    }

    "single-codepoint overlap" in {
      val a = IntervalSet.range(1, 5)
      val b = IntervalSet.range(5, 10)
      (a intersect b).ranges shouldBe Vector((5, 5))
    }

    "partial overlap" in {
      val a = IntervalSet.range(1, 10)
      val b = IntervalSet.range(5, 15)
      (a intersect b).ranges shouldBe Vector((5, 10))
    }

    "containment yields the smaller" in {
      val a = IntervalSet.range(1, 100)
      val b = IntervalSet.range(20, 30)
      (a intersect b) shouldBe b
      (b intersect a) shouldBe b
    }

    "identity" in {
      val a = IntervalSet.of((1, 5), (20, 25))
      (a intersect a) shouldBe a
    }

    "intersect with universe is identity" in {
      val a = IntervalSet.of((1, 5), (20, 25))
      (a intersect IntervalSet.universe) shouldBe a
      (IntervalSet.universe intersect a) shouldBe a
    }

    "multi-range intersection" in {
      // a: [1..10] [20..30] [40..50]
      // b: [5..25] [45..55]
      // expect: [5..10] [20..25] [45..50]
      val a = IntervalSet.of((1, 10), (20, 30), (40, 50))
      val b = IntervalSet.of((5, 25), (45, 55))
      (a intersect b).ranges shouldBe Vector((5, 10), (20, 25), (45, 50))
    }
  }

  // ---- diff ----------------------------------------------------------------

  "diff" - {

    "empty - x = empty" in {
      (IntervalSet.empty diff IntervalSet.range(1, 10)) shouldBe IntervalSet.empty
    }

    "x - empty = x" in {
      val s = IntervalSet.range(1, 10)
      (s diff IntervalSet.empty) shouldBe s
    }

    "disjoint subtrahend has no effect" in {
      val a = IntervalSet.range(1, 10)
      val b = IntervalSet.range(20, 30)
      (a diff b) shouldBe a
    }

    "subtrahend covering the whole range yields empty" in {
      val a = IntervalSet.range(5, 10)
      val b = IntervalSet.range(1, 20)
      (a diff b) shouldBe IntervalSet.empty
    }

    "subtrahend in the middle splits the range" in {
      val a = IntervalSet.range(1, 10)
      val b = IntervalSet.range(4, 6)
      (a diff b).ranges shouldBe Vector((1, 3), (7, 10))
    }

    "subtrahend at the start trims the lower edge" in {
      val a = IntervalSet.range(1, 10)
      val b = IntervalSet.range(1, 5)
      (a diff b).ranges shouldBe Vector((6, 10))
    }

    "subtrahend at the end trims the upper edge" in {
      val a = IntervalSet.range(1, 10)
      val b = IntervalSet.range(5, 10)
      (a diff b).ranges shouldBe Vector((1, 4))
    }

    "subtrahend exactly matching gives empty" in {
      val a = IntervalSet.range(1, 10)
      (a diff a) shouldBe IntervalSet.empty
    }

    "single-codepoint hole" in {
      val a = IntervalSet.range(1, 10)
      val b = IntervalSet.single(5)
      (a diff b).ranges shouldBe Vector((1, 4), (6, 10))
    }

    "many holes from a multi-range subtrahend" in {
      // a: [1..30]
      // b: [3..3] [7..9] [15..20] [25..25]
      // expect: [1..2] [4..6] [10..14] [21..24] [26..30]
      val a = IntervalSet.range(1, 30)
      val b = IntervalSet.of((3, 3), (7, 9), (15, 20), (25, 25))
      (a diff b).ranges shouldBe Vector((1, 2), (4, 6), (10, 14), (21, 24), (26, 30))
    }

    "subtrahend overlapping multi-range minuend" in {
      // a: [1..10] [20..30] [40..50]
      // b: [5..25]
      // expect: [1..4] [26..30] [40..50]
      val a = IntervalSet.of((1, 10), (20, 30), (40, 50))
      val b = IntervalSet.range(5, 25)
      (a diff b).ranges shouldBe Vector((1, 4), (26, 30), (40, 50))
    }
  }

  // ---- complement ----------------------------------------------------------

  "complement" - {

    "empty's complement is the whole universe" in {
      IntervalSet.empty.complement shouldBe IntervalSet.universe
    }

    "universe's complement is empty" in {
      IntervalSet.universe.complement shouldBe IntervalSet.empty
    }

    "complement of a single range" in {
      val s = IntervalSet.range(10, 20)
      s.complement.ranges shouldBe Vector((0, 9), (21, 0x10FFFF))
    }

    "complement of [0..N] is [N+1..0x10FFFF]" in {
      val s = IntervalSet.range(0, 10)
      s.complement.ranges shouldBe Vector((11, 0x10FFFF))
    }

    "complement of [N..0x10FFFF] is [0..N-1]" in {
      val s = IntervalSet.range(0x10FF00, 0x10FFFF)
      s.complement.ranges shouldBe Vector((0, 0x10FEFF))
    }

    "complement of multi-range" in {
      val s = IntervalSet.of((10, 20), (30, 40))
      s.complement.ranges shouldBe Vector((0, 9), (21, 29), (41, 0x10FFFF))
    }

    "double complement is identity" in {
      val s = IntervalSet.of((10, 20), (50, 60), (1000, 2000))
      s.complement.complement shouldBe s
    }
  }

  // ---- equality, hashcode, toString ----------------------------------------

  "equality" - {

    "equal sets compare equal regardless of construction order" in {
      val a = IntervalSet.of((1, 5), (10, 15))
      val b = IntervalSet.of((10, 15), (1, 5))
      a shouldBe b
      a.hashCode shouldBe b.hashCode
    }

    "merged-from-overlap equals plain construction" in {
      val a = IntervalSet.of((1, 5), (4, 8))
      val b = IntervalSet.range(1, 8)
      a shouldBe b
    }

    "different sets compare unequal" in {
      val a = IntervalSet.range(1, 5)
      val b = IntervalSet.range(1, 6)
      (a == b) shouldBe false
    }

    "empty equals empty" in {
      IntervalSet.empty shouldBe IntervalSet.empty
      IntervalSet.empty shouldBe IntervalSet.of()
    }

    "non-IntervalSet objects compare unequal" in {
      val a: Any = IntervalSet.empty
      a == "foo" shouldBe false
    }
  }

  "toString" - {

    "empty renders as []" in {
      IntervalSet.empty.toString shouldBe "[]"
    }

    "single codepoint renders as one U+ token" in {
      IntervalSet.single('A').toString shouldBe "[U+0041]"
    }

    "range renders as lo-hi" in {
      IntervalSet.range('a', 'z').toString shouldBe "[U+0061-U+007A]"
    }

    "multi-range comma-separated" in {
      val s = IntervalSet.of(('a', 'z'), ('A', 'Z'))
      s.toString shouldBe "[U+0041-U+005A, U+0061-U+007A]"
    }

    "high codepoints use 5-hex-digit width" in {
      IntervalSet.single(0x10FFFF).toString shouldBe "[U+10FFFF]"
    }
  }

  // ---- combined-operation properties --------------------------------------

  "algebraic identities" - {

    val a = IntervalSet.of((1, 10), (20, 30))
    val b = IntervalSet.of((5, 15), (25, 35))
    val c = IntervalSet.of((100, 200))

    "union is commutative" in {
      (a union b) shouldBe (b union a)
    }

    "union is associative" in {
      ((a union b) union c) shouldBe (a union (b union c))
    }

    "intersection is commutative" in {
      (a intersect b) shouldBe (b intersect a)
    }

    "intersection is associative" in {
      ((a intersect b) intersect c) shouldBe (a intersect (b intersect c))
    }

    "intersection distributes over union" in {
      val u = b union c
      (a intersect u) shouldBe ((a intersect b) union (a intersect c))
    }

    "diff equals intersect with complement" in {
      (a diff b) shouldBe (a intersect b.complement)
    }

    "set minus self is empty" in {
      (a diff a) shouldBe IntervalSet.empty
    }

    "union of set with its complement is universe" in {
      (a union a.complement) shouldBe IntervalSet.universe
    }

    "intersection of set with its complement is empty" in {
      (a intersect a.complement) shouldBe IntervalSet.empty
    }
  }

end IntervalSetSpec
