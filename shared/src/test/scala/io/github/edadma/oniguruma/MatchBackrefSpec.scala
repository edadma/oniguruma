package io.github.edadma.oniguruma

/** End-to-end backreference behaviour. Stage 4 supports numeric
  * (`\1`) and named (`\k<name>`) refs. Captured-empty refs match
  * zero-width; uncaptured refs (e.g. inside an alt arm that didn't
  * fire) fail. Under `(?i)` the substring comparison folds ASCII
  * letters; non-ASCII case-folding waits on Stage 5's UCD tables. */
class MatchBackrefSpec extends CompilerHelpers:

  "numeric backrefs" - {

    "(a+)\\1 matches a doubled run of a's" in {
      val m = re("(a+)\\1").findFirstMatchIn("aaaa").get
      // Greedy a+ takes 2; backref needs the same 2.
      m.matched shouldBe "aaaa"
      m.group(1) shouldBe Some("aa")
    }

    "(.)\\1 matches any doubled char" in {
      firstMatch("(.)\\1", "abccd") shouldBe "cc"
      firstMatch("(.)\\1", "ZZyy") shouldBe "ZZ"
    }

    "(.)(.)\\2\\1 matches a 4-char palindrome" in {
      firstMatch("(.)(.)\\2\\1", "abba") shouldBe "abba"
      findFirst("(.)(.)\\2\\1", "abcd") shouldBe None
    }

    "captured-empty group still gives a (zero-width) successful backref" in {
      // (a*) on input "b…" captures empty; \1 then matches empty.
      val m = re("(a*)b\\1").findFirstMatchIn("b").get
      m.matched shouldBe "b"
      m.group(1) shouldBe Some("")
    }

    "uncaptured group via alt arm fails the backref" in {
      // Only one of the two groups will capture. The backref must point
      // at the captured one to succeed; pointing at the other fails.
      // Force backref to group 1; the alt that captured group 2
      // leaves group 1 unmatched, so the overall pattern fails.
      findFirst("(?:(a)|(b))\\1", "bb") shouldBe None
      // But it succeeds when group 1 fires.
      firstMatch("(?:(a)|(b))\\1", "aa") shouldBe "aa"
    }
  }

  "named backrefs" - {

    "\\k<name> matches the captured substring" in {
      val m = re("(?<word>\\w+)\\s+\\k<word>").findFirstMatchIn("the the").get
      m.matched shouldBe "the the"
      m.group(1) shouldBe Some("the")
    }

    "no match when the repeated word differs" in {
      findFirst("(?<word>\\w+)\\s+\\k<word>", "the dog") shouldBe None
    }
  }

  "case-insensitive backrefs" - {

    "(?i)(a)\\1 — backref folds ASCII case" in {
      firstMatch("(?i)(a)\\1", "aA") shouldBe "aA"
      firstMatch("(?i)(a)\\1", "Aa") shouldBe "Aa"
    }

    "(?i)(.+)\\1 — multi-char case-folded backref" in {
      firstMatch("(?i)(.+)\\1", "abcABC") shouldBe "abcABC"
    }

    "without (?i), case mismatch fails the backref" in {
      findFirst("(.+)\\1", "abcABC") shouldBe None
    }
  }

  "branch-reset alt-of-backrefs" - {

    "\\k<x> against a branch-reset name picks whichever matched" in {
      // (?<x>a)|(?<x>b) creates `x -> List(1, 2)`. Then a `\k<x>`
      // backref tries each in turn. Matching "ba" with first arm
      // failing should still let the backref against group 2 succeed.
      // Pattern: ((?<x>a)|(?<x>b))\k<x>
      firstMatch("((?<x>a)|(?<x>b))\\k<x>", "aa") shouldBe "aa"
      firstMatch("((?<x>a)|(?<x>b))\\k<x>", "bb") shouldBe "bb"
    }
  }

  "relative numeric backrefs" - {
    // Stage 6.C — `\k<-N>` / `\k<+N>` resolve at parse time using the
    // running group-counter. Once resolved, runtime semantics match
    // an absolute backref exactly.

    "(.)\\k<-1> behaves like (.)\\1" in {
      firstMatch("(.)\\k<-1>", "abccd") shouldBe "cc"
    }

    "(\\w+)\\s+\\k<-1> matches a doubled word" in {
      val m = re("(\\w+)\\s+\\k<-1>").findFirstMatchIn("the the").get
      m.matched shouldBe "the the"
      m.group(1) shouldBe Some("the")
    }

    "(a)(b)\\k<-2> reaches across one intervening group" in {
      // `\k<-2>` from after (a)(b) refers to group 1.
      firstMatch("(a)(b)\\k<-2>", "aba") shouldBe "aba"
      findFirst("(a)(b)\\k<-2>", "abb") shouldBe None
    }

    "\\k<+1>(\\w+) — forward ref fails when the target group hasn't fired" in {
      // Onig/PCRE semantics: a backref to a NOT-YET-CAPTURED group
      // fails. The "captured-empty" (start == end ≥ 0) and "uncaptured"
      // (slot < 0) cases differ: empty matches zero-width, uncaptured
      // backtracks. This pins the parser-resolved forward ref running
      // through the runtime path that distinguishes them.
      findFirst("\\k<+1>(\\w+)", "foo") shouldBe None
    }

    "(\\w+) (\\k<+1>|(\\w+)) — forward ref succeeds once group 3 fires" in {
      // The alt's first arm tries `\k<+1>` (= \k<3>), which fails on
      // first attempt because group 3 hasn't matched. Fallback arm
      // captures group 3. On subsequent re-entries (here just one
      // top-level match) the backref would succeed if input matched.
      // Pin the no-loop case: on "the dog" the forward ref fails, the
      // fallback arm fires, and the overall pattern matches.
      val m = re("(\\w+) (\\k<+1>|(\\w+))").findFirstMatchIn("the dog").get
      m.matched shouldBe "the dog"
      m.group(1) shouldBe Some("the")
      m.group(3) shouldBe Some("dog")
    }
  }

  "backrefs beyond the declared count (Onig-compat, downstream Bug 2)" - {

    // Real Oniguruma C compiles `\N` for any positive `N`, with the
    // runtime semantic of "treat as an uncaptured group" when the
    // referenced number is out of range. Several TextMate grammars
    // rely on this — `(\3)|...` uses `\3` as a guaranteed never-match
    // marker so the alt always falls through to its second arm.
    //
    // The compile-side test lives in CompileBackrefSpec (instruction
    // shape with an out-of-range slot index); this section pins the
    // end-to-end runtime behaviour.

    "(\\3)|abc — out-of-range backref falls through to the second alt" in {
      val m = re("(\\3)|abc").findFirstMatchIn("abc").get
      m.matched shouldBe "abc"
      // Group 1 never fires because the first alt always fails.
      m.group(1) shouldBe None
    }

    "(\\3)|(\\w+) — group 2 captures via the fallback arm" in {
      // The first alt tries `\3` (group 3 doesn't exist) → always
      // fails. The second alt captures the word into group 2.
      val m = re("(\\3)|(\\w+)").findFirstMatchIn("hello").get
      m.matched shouldBe "hello"
      m.group(1) shouldBe None
      m.group(2) shouldBe Some("hello")
    }

    "(a)\\2 — backref past the declared count always fails" in {
      // No match anywhere — the backref is unsatisfiable regardless
      // of input.
      findFirst("(a)\\2", "a") shouldBe None
      findFirst("(a)\\2", "aa") shouldBe None
      findFirst("(a)\\2", "abc") shouldBe None
    }

    "\\1 with no groups at all always fails" in {
      // Slot index 2 is past the slot array (sized 2 for group 0
      // only). The VM bounds-checks and backtracks immediately.
      findFirst("\\1", "anything") shouldBe None
      findFirst("\\1abc", "abc") shouldBe None
    }

    "^\\s*(\\2)(?![A-Za-z0-9_])  — corpus pattern compiles and runs" in {
      // From the TextMate corpus (CompileCorpusSpec used to count
      // this as a failing pattern). The leading group is `(\2)`, so
      // group 1's body is the backref to group 2 — which doesn't
      // exist. The first arm of any match attempt always fails the
      // backref, the overall pattern never matches.
      val r = re("^\\s*(\\2)(?![A-Za-z0-9_])")
      r.findFirstMatchIn("  xyz")        shouldBe None
      r.findFirstMatchIn("xyz")          shouldBe None
      r.findFirstMatchIn("")             shouldBe None
    }

    "(\\3)|(?=$|\\*/) — corpus pattern compiles and runs" in {
      // From the TextMate corpus. First alt always fails (`\3` →
      // out of range). Second alt is a positive lookahead for either
      // end-of-input or `*/`. The scan walks forward until one of
      // them fires — at the end of input, `$` matches, giving a
      // zero-width match at sp=length.
      val r = re("(\\3)|(?=$|\\*/)")
      val m = r.findFirstMatchIn("abc").get
      m.start shouldBe 3
      m.end   shouldBe 3
      m.group(1) shouldBe None
      // The same pattern fires zero-width right before `*/`.
      val m2 = r.findFirstMatchIn("foo*/bar").get
      m2.start shouldBe 3
      m2.end   shouldBe 3
    }
  }

end MatchBackrefSpec
