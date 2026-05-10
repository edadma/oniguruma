package io.github.edadma.oniguruma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end TextMate scanner behaviour. The shape mirrors what
  * `vscode-textmate` expects from an Onig binding: at the earliest
  * position where ANY pattern matches, the LOWEST-indexed pattern
  * that matches at that position wins. Tied positions are broken by
  * pattern order; nothing-matches-anywhere returns None. */
class TmScannerSpec extends AnyFreeSpec with Matchers:

  /** Compile-or-fail-the-test helper — keeps the spec body terse. */
  private def scanner(patterns: String*): TmScanner =
    TmScanner.compile(patterns) match
      case Right(s)  => s
      case Left(msg) => fail(s"compile failed: $msg")

  "empty scanner" - {

    "always returns None" in {
      val s = scanner()
      s.findNextMatch("anything", 0, 0) shouldBe None
    }

    "size is 0" in {
      scanner().size shouldBe 0
    }
  }

  "single pattern" - {

    "matches at start of input" in {
      val s = scanner("foo")
      val m = s.findNextMatch("foo bar", 0, 0).get
      m.patternIndex shouldBe 0
      m.regexMatch.matched shouldBe "foo"
      m.regexMatch.start shouldBe 0
    }

    "scans forward to find a later match" in {
      val s = scanner("bar")
      val m = s.findNextMatch("foo bar baz", 0, 0).get
      m.patternIndex shouldBe 0
      m.regexMatch.start shouldBe 4
    }

    "returns None when nothing matches" in {
      scanner("xyz").findNextMatch("abc def", 0, 0) shouldBe None
    }

    "respects start offset" in {
      val s = scanner("\\w+")
      val m = s.findNextMatch("foo bar", 4, 0).get
      m.regexMatch.matched shouldBe "bar"
    }
  }

  "multi-pattern tie-break" - {

    "lowest-index pattern wins on a tie at the same position" in {
      // Both `abc` and `ab` would match at pos 0; the lower-indexed
      // (first-listed) pattern claims it.
      val s = scanner("abc", "ab")
      val m = s.findNextMatch("abc def", 0, 0).get
      m.patternIndex shouldBe 0
      m.regexMatch.matched shouldBe "abc"
    }

    "swapping pattern order changes the matched text" in {
      // Same two patterns, but with their order swapped — index 0 still
      // wins on the tie, but the matched substring is now "ab" since
      // that's what pattern 0 spells.
      val s = scanner("ab", "abc")
      val m = s.findNextMatch("abc def", 0, 0).get
      m.patternIndex shouldBe 0
      m.regexMatch.matched shouldBe "ab"
    }
  }

  "earliest-position wins across patterns" - {

    "later-listed pattern wins if it matches earlier" in {
      // Pattern 0 ("baz") could match at pos 8; pattern 1 ("foo")
      // matches at pos 0. Earliest position wins.
      val s = scanner("baz", "foo")
      val m = s.findNextMatch("foo bar baz", 0, 0).get
      m.patternIndex shouldBe 1
      m.regexMatch.start shouldBe 0
    }

    "the scanner walks until something matches" in {
      val s = scanner("xyz", "bar")
      val m = s.findNextMatch("foo bar baz", 0, 0).get
      m.patternIndex shouldBe 1
      m.regexMatch.matched shouldBe "bar"
    }
  }

  "\\G respects prevMatchEnd" - {

    "first call: prevMatchEnd = 0 means \\G fires at start" in {
      val s = scanner("\\Gfoo")
      val m = s.findNextMatch("foofoo", 0, 0).get
      m.patternIndex shouldBe 0
      m.regexMatch.start shouldBe 0
    }

    "later call: \\G fires only at the supplied prevMatchEnd" in {
      val s = scanner("\\Gfoo")
      // After consuming the first "foo", we re-scan from pos 3 with
      // prevMatchEnd = 3 — \G now matches there.
      val m = s.findNextMatch("foofoo", 3, 3).get
      m.regexMatch.start shouldBe 3

      // But if prevMatchEnd lags behind start, \G can't fire — and
      // nothing else in the pattern can match either, so the whole
      // scan gives up.
      s.findNextMatch("foofoo", 3, 0) shouldBe None
    }
  }

  "supplementary codepoints" - {

    "scanner walks past a non-matching emoji to find a literal" in {
      val s = scanner("hi")
      val m = s.findNextMatch("😀hi", 0, 0).get
      // Emoji is U+1F600, two UTF-16 code units; "hi" starts at index 2.
      m.regexMatch.start shouldBe 2
      m.regexMatch.matched shouldBe "hi"
    }
  }

  "compile errors" - {

    "first bad pattern surfaces with its index" in {
      TmScanner.compile(Seq("ok", "(unbalanced")) match
        case Right(_) => fail("expected compile failure")
        case Left(m)  =>
          m should include("pattern 1")
          m should include("parse")
    }

    "throwing apply matches" in {
      assertThrows[IllegalArgumentException] {
        TmScanner("ok", "(broken")
      }
    }
  }

  "successive findNextMatch — typical TextMate scan loop" in {
    // The conventional TM scan: keep calling findNextMatch with
    // updated prevMatchEnd until None. This is how a tokenizer
    // walks a line.
    val s = scanner("\\d+", "[a-z]+")
    val input = "abc 123 def 456"
    val out   = collection.mutable.ListBuffer.empty[(Int, String)]
    var pos   = 0
    var prev  = 0
    var more  = true
    while more do
      s.findNextMatch(input, pos, prev) match
        case None => more = false
        case Some(m) =>
          out += ((m.patternIndex, m.regexMatch.matched))
          // Advance past the match; for zero-width matches we'd need
          // a one-codepoint nudge but none of these patterns are
          // zero-width.
          pos  = m.regexMatch.end
          prev = m.regexMatch.end
    out.toList shouldBe List(
      (1, "abc"),
      (0, "123"),
      (1, "def"),
      (0, "456"),
    )
  }

end TmScannerSpec
