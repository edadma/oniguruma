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

end MatchBackrefSpec
