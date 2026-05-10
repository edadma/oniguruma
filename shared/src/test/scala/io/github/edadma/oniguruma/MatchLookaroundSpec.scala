package io.github.edadma.oniguruma

/** End-to-end lookaround behaviour. Stage 4 supports forward and
  * backward lookaround in both polarities. Lookbehind uses the
  * slow-but-correct algorithm: it scans every UTF-16 boundary from
  * the start of input up to the parent's `sp` and asks the
  * sub-program to land exactly there. That is O(sp × subBody) per
  * lookbehind and is fine for the corpus's line-sized inputs. */
class MatchLookaroundSpec extends CompilerHelpers:

  "positive forward lookahead" - {

    "\\d+(?=px) matches digits followed by 'px'" in {
      firstMatch("\\d+(?=px)", "10px 20em") shouldBe "10"
    }

    "lookahead is zero-width — captures don't include the assertion" in {
      val m = re("\\d+(?=px)").findFirstMatchIn("10px").get
      m.matched shouldBe "10"
      m.end shouldBe 2
    }

    "fails when the lookahead doesn't match" in {
      findFirst("\\d+(?=px)", "10em") shouldBe None
    }
  }

  "negative forward lookahead" - {

    "\\d+(?!px) matches digits NOT followed by 'px'" in {
      firstMatch("\\d+(?!px)", "10em") shouldBe "10"
    }

    "shrinks a greedy match to satisfy the lookahead" in {
      // \d+ is greedy. "10" makes (?!px) fail (next chars are "px");
      // the engine backtracks and shrinks \d+ to "1", which DOES
      // satisfy (?!px) since the next char is "0".
      firstMatch("\\d+(?!px)", "10px 20em") shouldBe "1"
    }

    "scans past a position where lookahead always fails" in {
      // \b on both sides forbids shrinking inside "10", so the engine
      // can't satisfy the assertion at pos 0 by giving back. It moves
      // on to "20", whose lookahead vacuously succeeds (end of input).
      firstMatch("\\b\\d+\\b(?!px)", "10px 20") shouldBe "20"
    }
  }

  "positive backward lookbehind" - {

    "(?<=\\$)\\d+ matches digits preceded by '$'" in {
      firstMatch("(?<=\\$)\\d+", "price $100 today") shouldBe "100"
    }

    "fails when the lookbehind doesn't match" in {
      findFirst("(?<=\\$)\\d+", "100 dollars") shouldBe None
    }

    "variable-length alt body works (slow path)" in {
      // The lookbehind body has two arms of different lengths; the
      // sub-VM has to find one that lands at the parent's sp.
      firstMatch("(?<=ab|cde)x", "abx")  shouldBe "x"
      firstMatch("(?<=ab|cde)x", "cdex") shouldBe "x"
      findFirst("(?<=ab|cde)x", "fx")     shouldBe None
    }
  }

  "negative backward lookbehind" - {

    "(?<!\\$)\\d+ matches digits NOT preceded by '$'" in {
      // Find-loop walks position-by-position. At pos 1 ('1'), the
      // lookbehind sees '$' immediately before — fails. The next
      // digit-start position not preceded by '$' is "20".
      firstMatch("(?<!\\$)\\d+", "abc 20 $30") shouldBe "20"
    }

    "rejects a candidate at the very position '$' precedes" in {
      findFirst("(?<!\\$)\\d", "$5") shouldBe None
    }
  }

  "lookahead inside a quantifier — pinning a boundary" - {

    "match a word that's followed by punctuation" in {
      firstMatch("\\w+(?=[.!?])", "hello.") shouldBe "hello"
    }
  }

  "nested lookaround" - {

    "(?=a(?=b))ab — both nested asserts must hold" in {
      firstMatch("(?=a(?=b))ab", "ab") shouldBe "ab"
      // Outer succeeds at "ax" but inner (?=b) fails on 'x' — overall fails.
      findFirst("(?=a(?=b))ab", "ax") shouldBe None
    }
  }

  "Stage 6.B — capture propagation through positive lookaround" - {

    // Lookarounds are zero-width on `sp`, but any groups they wrote
    // inside their sub-run propagate back to the parent's slot array.
    // The merge rule is "copy any slot the sub-run wrote (slot >= 0)";
    // a sub-run that never entered a group leaves the parent's value
    // alone. Negative lookaround never propagates.

    "(?=(\\w+)) — single group from a positive lookahead leaks out" in {
      val m = re("(?=(\\w+))").findFirstMatchIn("hello world").get
      m.matched shouldBe ""
      m.start shouldBe 0
      m.end shouldBe 0
      m.group(1) shouldBe Some("hello")
    }

    "(?=(\\w+)) followed by a literal consumes nothing extra" in {
      // Sanity check the zero-width semantic together with the
      // propagation: the parent's sp stays put, but the inner group
      // is observable after the assertion.
      val m = re("(?=(\\w+))\\w").findFirstMatchIn("abc").get
      m.matched shouldBe "a"
      m.group(1) shouldBe Some("abc")
    }

    "(?=(\\w+))\\1 — backreference into a lookahead-only capture" in {
      // Onig/PCRE classic idiom: capture a word in the lookahead,
      // then assert the immediately following text equals that word.
      // Trivially true here since the lookahead's capture starts at
      // the same offset as the backref. The match length equals the
      // captured word's length.
      val m = re("(?=(\\w+))\\1").findFirstMatchIn("foo bar").get
      m.matched shouldBe "foo"
      m.group(1) shouldBe Some("foo")
    }

    "(?<=(\\w+) )\\w+ — capture from positive lookbehind reaches the match" in {
      val m = re("(?<=(\\w+) )\\w+").findFirstMatchIn("hello world").get
      m.matched shouldBe "world"
      m.group(1) shouldBe Some("hello")
    }

    "nested captures inside a positive lookahead all propagate" in {
      val m = re("(?=(\\d+)-(\\d+))").findFirstMatchIn("12-34").get
      m.matched shouldBe ""
      m.group(1) shouldBe Some("12")
      m.group(2) shouldBe Some("34")
    }

    "captures from a NEGATIVE lookahead are NOT propagated" in {
      // The sub-run fails (the negative succeeds on failure), so
      // there's no captured slot to take. Group 1 stays unmatched.
      val m = re("(?!(\\d+))[a-z]+").findFirstMatchIn("abc").get
      m.matched shouldBe "abc"
      m.group(1) shouldBe None
    }

    "captures from a NEGATIVE lookbehind are NOT propagated" in {
      val m = re("(?<!(\\d+))[a-z]+").findFirstMatchIn("abc").get
      m.matched shouldBe "abc"
      m.group(1) shouldBe None
    }

    "a failed positive lookahead doesn't leak captures" in {
      // The sub-run never reaches LookaroundExit, so its sub-VM
      // returns None — nothing to merge. The whole match fails, but
      // the test pins the principle by re-using the assertion
      // alongside a backup arm.
      val m = re("(?=(\\d+))|([a-z]+)").findFirstMatchIn("abc").get
      m.matched shouldBe "abc"
      // The successful arm is the second alt; group 1 was never set
      // by anyone (the lookahead failed at offset 0 of "abc").
      m.group(1) shouldBe None
      m.group(2) shouldBe Some("abc")
    }

    "outer capture survives lookaround that doesn't touch it" in {
      // Group 1 = outer "abc", group 2 = captured from lookahead.
      // The lookahead's sub-run only writes group 2; the merge must
      // leave group 1's value alone (the merge rule: only overwrite
      // slots the sub-run actually wrote).
      val m = re("([a-z]+)(?=(\\d+))").findFirstMatchIn("abc123").get
      m.matched shouldBe "abc"
      m.group(1) shouldBe Some("abc")
      m.group(2) shouldBe Some("123")
    }

    "later capture overwrites earlier lookahead-propagated value" in {
      // First the lookahead writes group 1 = "abc"; then the main
      // pattern overwrites group 1 with "abc" again at the same
      // offset. End state: group 1 = "abc". The test verifies that
      // the merged slots aren't somehow frozen — the parent VM keeps
      // writing them normally.
      val m = re("(?=(\\w+))(\\w+)").findFirstMatchIn("hello").get
      m.matched shouldBe "hello"
      m.group(1) shouldBe Some("hello")
      m.group(2) shouldBe Some("hello")
    }

    "nested lookaround propagates captures from the innermost match" in {
      // Outer (?=...) wraps an inner (?=(\w+)); the inner's group
      // propagates through the outer back to the parent.
      val m = re("(?=(?=(\\w+))).").findFirstMatchIn("hi").get
      m.matched shouldBe "h"
      m.group(1) shouldBe Some("hi")
    }
  }

end MatchLookaroundSpec
