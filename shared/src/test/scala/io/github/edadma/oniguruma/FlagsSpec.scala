package io.github.edadma.oniguruma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** [[Flags]] is just a packed-int set, but its semantics get used as the
  * authority for case-insensitivity, dot-matches-newline, extended mode,
  * and multiline `^`/`$` — every wrong bit costs us a behavior change at
  * runtime. The whole API is small enough that we cover every method here. */
class FlagsSpec extends AnyFreeSpec with Matchers:

  import Flags.*

  // ---- atomic flag values --------------------------------------------------

  "atomic values" - {

    "Empty has no bits" in {
      asInt(Empty) shouldBe 0
    }

    "individual flag bits do not overlap" in {
      // Bit-distinctness is the precondition for `union` / `has` / `diff`
      // to behave correctly. If two flags ever shared a bit we'd silently
      // mis-match every regex that used either one.
      asInt(IgnoreCase) should not be 0
      asInt(DotAll) should not be 0
      asInt(Extended) should not be 0
      asInt(MultiLine) should not be 0

      val all = List(IgnoreCase, DotAll, Extended, MultiLine).map(asInt)
      all.distinct.size shouldBe all.size

      // And no two share a bit (AND of any pair is zero).
      for
        i <- all.indices
        j <- (i + 1) until all.size
      do (all(i) & all(j)) shouldBe 0
    }

    "fromInt and asInt round-trip" in {
      asInt(fromInt(0)) shouldBe 0
      asInt(fromInt(0x0F)) shouldBe 0x0F
      val combined = fromInt(asInt(IgnoreCase) | asInt(DotAll))
      has(combined, IgnoreCase) shouldBe true
      has(combined, DotAll) shouldBe true
    }
  }

  // ---- union ---------------------------------------------------------------

  "union" - {

    "Empty | x = x" in {
      union(Empty, IgnoreCase) shouldBe IgnoreCase
      union(IgnoreCase, Empty) shouldBe IgnoreCase
    }

    "self-union is identity" in {
      union(IgnoreCase, IgnoreCase) shouldBe IgnoreCase
    }

    "two distinct flags combine to set both bits" in {
      val u = union(IgnoreCase, DotAll)
      has(u, IgnoreCase) shouldBe true
      has(u, DotAll) shouldBe true
    }

    "all four together" in {
      val all = union(union(IgnoreCase, DotAll), union(Extended, MultiLine))
      has(all, IgnoreCase) shouldBe true
      has(all, DotAll) shouldBe true
      has(all, Extended) shouldBe true
      has(all, MultiLine) shouldBe true
    }
  }

  // ---- has -----------------------------------------------------------------

  "has" - {

    "Empty has no flags" in {
      has(Empty, IgnoreCase) shouldBe false
      has(Empty, DotAll) shouldBe false
    }

    "single flag set is detected" in {
      has(IgnoreCase, IgnoreCase) shouldBe true
      has(IgnoreCase, DotAll) shouldBe false
    }

    "has(Empty) on any value is false" in {
      // `has(_, Empty)` would otherwise be vacuously true under the naive
      // (a & b) != 0 definition; we explicitly disallow it so callers get
      // a usable answer when they ask "is the empty set set?". (No, it isn't.)
      has(IgnoreCase, Empty) shouldBe false
      has(Empty, Empty) shouldBe false
    }

    "extension method `hasFlag` mirrors `has`" in {
      val u = union(IgnoreCase, DotAll)
      (u hasFlag IgnoreCase) shouldBe true
      (u hasFlag Extended) shouldBe false
    }

    "compound flag is membership-tested as a whole" in {
      // `has(a, b)` requires every bit of b to be present in a, not just
      // any bit. This matters when checking "are all of these flags set?".
      val u           = union(IgnoreCase, DotAll)
      val both        = union(IgnoreCase, DotAll)
      val justOne     = IgnoreCase
      val withMissing = union(IgnoreCase, Extended)
      has(u, both) shouldBe true
      has(u, justOne) shouldBe true
      has(u, withMissing) shouldBe false // Extended is not in u
    }
  }

  // ---- diff ----------------------------------------------------------------

  "diff" - {

    "diff(x, Empty) = x" in {
      val u = union(IgnoreCase, DotAll)
      diff(u, Empty) shouldBe u
    }

    "diff with a flag that is not present is identity" in {
      diff(IgnoreCase, DotAll) shouldBe IgnoreCase
    }

    "diff removes the flag" in {
      val u = union(IgnoreCase, DotAll)
      val d = diff(u, IgnoreCase)
      has(d, IgnoreCase) shouldBe false
      has(d, DotAll) shouldBe true
    }

    "diff with a multi-bit mask removes all the requested bits" in {
      val all  = union(union(IgnoreCase, DotAll), union(Extended, MultiLine))
      val mask = union(IgnoreCase, MultiLine)
      val d    = diff(all, mask)
      has(d, IgnoreCase) shouldBe false
      has(d, MultiLine) shouldBe false
      has(d, DotAll) shouldBe true
      has(d, Extended) shouldBe true
    }
  }

  // ---- intersect -----------------------------------------------------------

  "intersect" - {

    "intersect with Empty is Empty" in {
      intersect(IgnoreCase, Empty) shouldBe Empty
      intersect(Empty, IgnoreCase) shouldBe Empty
    }

    "intersect of disjoint flags is Empty" in {
      intersect(IgnoreCase, DotAll) shouldBe Empty
    }

    "intersect of equal flags is identity" in {
      intersect(IgnoreCase, IgnoreCase) shouldBe IgnoreCase
    }

    "intersect picks shared bits" in {
      val a = union(IgnoreCase, DotAll)
      val b = union(DotAll, Extended)
      intersect(a, b) shouldBe DotAll
    }
  }

end FlagsSpec
