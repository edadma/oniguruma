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

end MatchLookaroundSpec
