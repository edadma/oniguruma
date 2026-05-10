package io.github.edadma.oniguruma

/** End-to-end matching under runtime flags. Stage 4 supports `(?i)`
  * (ASCII-only case-insensitive) and `(?m)` (DotAll in Onig flavor —
  * dot matches newline). `(?x)` extended-mode is a parse-time concern
  * and stays in the parser's domain; line-mode `^`/`$` are always-on
  * in the Onig flavor and need no flag. (Onig's `m` is what PCRE
  * spells `s`.) */
class MatchFlagsSpec extends CompilerHelpers:

  "(?i) case-insensitive matching" - {

    "matches both cases of a letter" in {
      firstMatch("(?i)a", "ABC") shouldBe "A"
      firstMatch("(?i)a", "abc") shouldBe "a"
    }

    "matches a multi-letter literal in any case" in {
      firstMatch("(?i)abc", "XYZABCDEF") shouldBe "ABC"
      firstMatch("(?i)abc", "AbCdef") shouldBe "AbC"
    }

    "applies to char classes too" in {
      firstMatch("(?i)[a-z]+", "Hello!") shouldBe "Hello"
      firstMatch("(?i)[a-z]+", "ABCDEF") shouldBe "ABCDEF"
    }

    "non-letters are unaffected by the flag" in {
      firstMatch("(?i)1+", "111") shouldBe "111"
      findFirst("(?i)1+", "abc") shouldBe None
    }

    "scoped (?i:…) restores case sensitivity outside" in {
      // Inside the group: 'a' matches 'A'. Outside: 'b' must be lowercase.
      findFirst("(?i:a)b", "Ab") shouldBe defined
      findFirst("(?i:a)b", "AB") shouldBe None
    }

    "leaked (?i) propagates to siblings in a Concat" in {
      firstMatch("(?i)ab", "AB") shouldBe "AB"
    }
  }

  "(?m) DotAll (Onig flavor)" - {

    "without (?m), . skips newline" in {
      findFirst("a.b", "a\nb") shouldBe None
    }

    "with (?m), . matches newline" in {
      firstMatch("(?m)a.b", "a\nb") shouldBe "a\nb"
    }

    "scoped (?m:…) only enables DotAll inside the group" in {
      // Inside the group, . matches newline; outside, the next . has
      // to land on a non-newline.
      firstMatch("(?m:.)c", "\nc") shouldBe "\nc"
      findFirst("(?m:.).c", "\n\nc") shouldBe None
    }
  }

  "combined flags" - {

    "(?im) folds case AND lets dot match newline" in {
      firstMatch("(?im)a.B", "A\nb") shouldBe "A\nb"
    }
  }

end MatchFlagsSpec
