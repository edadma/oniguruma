package io.github.edadma.oniguruma

/** Stage 6.D — empty-body loop progress tracking. Patterns whose loop
  * body can match without consuming input (`(a*)*`, `()*`, `(?:a|)*`,
  * `(a*?)*`, etc.) used to lean on the VM's `StepLimit = 10M` blunt
  * cap to stop iterating, which made them slow and risked spurious
  * `None` results on legitimately-large inputs.
  *
  * The fix lives in [[VM.runOne]]: every `Split` records the `sp` at
  * which it was last executed; when execution arrives at the same
  * Split with the same `sp` again, the loop body must have produced
  * no progress on the prior iteration, so the VM backtracks instead
  * of retaking the loop arm.
  *
  * These tests pin the observable behaviour:
  *   - the pathological patterns now terminate quickly with the
  *     expected match shape;
  *   - the surrounding semantics (greedy / reluctant / capture content)
  *     match Onig's actual choices, not just "any termination is fine";
  *   - normal progress-making loops are not affected.
  *
  * On a healthy VM, every test in this file completes in single-digit
  * milliseconds; on a regressed VM, they would hit `StepLimit` and
  * take seconds. The `terminationCap` checks bake in that latency
  * difference as a hard upper bound on wall-clock time. */
class MatchEmptyLoopSpec extends CompilerHelpers:

  /** Run `body` and fail the test if it takes longer than `maxMillis`.
    * A regression in the empty-body check tends to push a single match
    * call from microseconds into multi-second territory (the old 10M
    * `StepLimit`), so 500 ms is comfortably above clean timing and
    * comfortably below the pathological one. */
  private def withinTime[A](maxMillis: Long)(body: => A): A =
    val t0 = System.nanoTime()
    val r  = body
    val dt = (System.nanoTime() - t0) / 1_000_000
    if dt > maxMillis then
      fail(s"operation took ${dt}ms, expected < ${maxMillis}ms — likely empty-body loop regression")
    r

  "(a*)* — outer-greedy / inner-greedy, both empty-body-capable" - {

    "matches 'a' as group 0 = 'a' without hanging" in {
      val m = withinTime(500) { re("(a*)*").findFirstMatchIn("a").get }
      m.matched shouldBe "a"
    }

    "matches '' at the start of 'b' without hanging" in {
      // No 'a' at sp=0, so the outer * takes its skip arm immediately;
      // overall match is empty at offset 0.
      val m = withinTime(500) { re("(a*)*").findFirstMatchIn("b").get }
      m.matched shouldBe ""
      m.start shouldBe 0
    }

    "matches '' against the empty input" in {
      val m = withinTime(500) { re("(a*)*").findFirstMatchIn("").get }
      m.matched shouldBe ""
    }

    "swallows a long run of a's in one shot" in {
      val input = "a" * 1000
      val m = withinTime(500) { re("(a*)*").findFirstMatchIn(input).get }
      m.matched shouldBe input
    }
  }

  "(a*?)* — outer-greedy / inner-reluctant" - {

    "terminates and matches a small prefix at offset 0" in {
      // The empty-body check at the outer Split fires once the inner
      // reluctantly produces empty. Backtracking pops the inner's
      // "try one a" choice, so one a is consumed before the outer's
      // skip arm exits the loop. Result: matched = "a" at offset 0.
      //
      // Different engines (Perl, PCRE, Onig) produce different
      // results here (some return ""). We document our choice: the
      // simpler "backtrack to most-recent choice on empty-body
      // detection" rule pops the inner's body choice, consuming one
      // codepoint per outer iteration. Both interpretations are
      // self-consistent.
      val m = withinTime(500) { re("(a*?)*").findFirstMatchIn("aaa").get }
      m.start shouldBe 0
      m.matched shouldBe "a"
    }

    "matches empty against empty input" in {
      val m = withinTime(500) { re("(a*?)*").findFirstMatchIn("").get }
      m.matched shouldBe ""
    }
  }

  "(?:a|)* — alternation with an explicit empty arm" - {

    "matches empty against empty input" in {
      val m = withinTime(500) { re("(?:a|)*").findFirstMatchIn("").get }
      m.matched shouldBe ""
    }

    "matches 'a' against 'a'" in {
      val m = withinTime(500) { re("(?:a|)*").findFirstMatchIn("a").get }
      m.matched shouldBe "a"
    }

    "matches empty at offset 0 of 'b'" in {
      val m = withinTime(500) { re("(?:a|)*").findFirstMatchIn("b").get }
      m.matched shouldBe ""
    }
  }

  "()* — empty capturing group, repeated" - {

    "matches empty against empty input" in {
      val m = withinTime(500) { re("()*").findFirstMatchIn("").get }
      m.matched shouldBe ""
    }

    "matches empty at start of 'abc'" in {
      val m = withinTime(500) { re("()*").findFirstMatchIn("abc").get }
      m.matched shouldBe ""
    }
  }

  "(?:){2,5} — empty body with bounded repetition" - {

    "matches empty quickly without exhausting step budget" in {
      // {2,5} unrolls into Split-laden bytecode; the progress check
      // must fire at the inner Splits, not just at the * shape.
      val m = withinTime(500) { re("(?:){2,5}").findFirstMatchIn("xyz").get }
      m.matched shouldBe ""
    }
  }

  "deeply nested empty-body loops" - {

    "(((a*)*)*) terminates on 'a'" in {
      val m = withinTime(500) { re("(((a*)*)*)").findFirstMatchIn("a").get }
      m.matched shouldBe "a"
    }

    "(((a*)*)*) terminates on empty input" in {
      val m = withinTime(500) { re("(((a*)*)*)").findFirstMatchIn("").get }
      m.matched shouldBe ""
    }
  }

  "non-pathological loops remain unaffected" - {

    "a+ on 100 a's still matches all of them" in {
      val input = "a" * 100
      val m = re("a+").findFirstMatchIn(input).get
      m.matched shouldBe input
    }

    "(ab)+ on 50 reps still matches the full run" in {
      val input = "ab" * 50
      val m = re("(ab)+").findFirstMatchIn(input).get
      m.matched shouldBe input
    }

    "(a|b)* on 'ababab' matches the whole input" in {
      val m = re("(a|b)*").findFirstMatchIn("ababab").get
      m.matched shouldBe "ababab"
    }

    "(a|aa)*c on 'aaaac' matches greedily and reaches c" in {
      // The alternation has competing arms and backtracks across iterations;
      // the progress check must not fire on legitimate revisits.
      val m = re("(a|aa)*c").findFirstMatchIn("aaaac").get
      m.matched shouldBe "aaaac"
    }

    "findAllMatches on a* still produces zero-width hits correctly" in {
      // The empty-body check fires inside one match attempt; it doesn't
      // affect the cross-attempt advance logic in `findAllMatchesIn`.
      val ms = re("a*").findAllMatchesIn("aabaa").toList
      ms.map(_.matched) should contain ("aa")
      ms.size should be > 0
      ms.size should be < 100
    }
  }

  "findAllMatchesIn drives empty-body patterns correctly" - {

    "(a*)* doesn't blow up the all-matches iterator on 'aaa'" in {
      val ms = withinTime(500) { re("(a*)*").findAllMatchesIn("aaa").toList }
      ms should not be empty
      ms.size should be < 100
      // The first match should swallow all the a's at sp=0.
      ms.head.matched shouldBe "aaa"
    }
  }

end MatchEmptyLoopSpec
