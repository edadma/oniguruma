package io.github.edadma.oniguruma

/** End-to-end match behaviour. Drives the full pipeline (parse →
  * compile → VM) and asserts the surface API works the way callers
  * will use it. The earlier `Compile*Spec` files pin the bytecode
  * layout; this one pins the observable behaviour.
  *
  * Coverage groups:
  *   - basic literal/dot/anchor matching
  *   - char classes, including negated and POSIX
  *   - quantifier modes (greedy, reluctant, possessive)
  *   - alternation precedence
  *   - capture groups (overall + numbered + named)
  *   - findFirstMatchIn vs findAllMatchesIn
  *   - anchors against multi-line input */
class MatchSpec extends CompilerHelpers:

  "basic literals" - {

    "matches identical literal" in {
      firstMatch("abc", "abc") shouldBe "abc"
    }

    "scans for literal in larger input" in {
      firstMatch("foo", "the foo bar") shouldBe "foo"
    }

    "no match returns None" in {
      findFirst("xyz", "abcdef") shouldBe None
    }

    "match starts at the first occurrence" in {
      val m = re("foo").findFirstMatchIn("aaa foo bbb foo").get
      m.start shouldBe 4
      m.end shouldBe 7
    }

    "supplementary codepoints round-trip" in {
      val emoji = "😀" // U+1F600
      firstMatch(emoji, s"hello $emoji world") shouldBe emoji
    }
  }

  "the dot" - {

    "matches any non-newline" in {
      firstMatch("a.c", "abc") shouldBe "abc"
      firstMatch("a.c", "a@c") shouldBe "a@c"
    }

    "fails on a newline by default" in {
      findFirst("a.c", "a\nc") shouldBe None
    }
  }

  "char classes" - {

    "[abc] picks any of three" in {
      firstMatch("[abc]", "xyzbpq") shouldBe "b"
    }

    "[^abc] excludes" in {
      firstMatch("[^abc]", "abcz") shouldBe "z"
    }

    "[a-z]+ greedy" in {
      firstMatch("[a-z]+", "###hello###") shouldBe "hello"
    }

    "\\d+" in {
      firstMatch("\\d+", "abc123def") shouldBe "123"
    }

    "[[:alpha:]]+" in {
      firstMatch("[[:alpha:]]+", "12abc34") shouldBe "abc"
    }
  }

  "quantifiers" - {

    "a* greedy on no a's matches empty at start" in {
      val m = re("a*").findFirstMatchIn("xyz").get
      m.matched shouldBe ""
      m.start shouldBe 0
    }

    "a* greedy consumes all a's" in {
      firstMatch("a*", "aaab") shouldBe "aaa"
    }

    "a*? reluctant matches empty preferentially" in {
      val m = re("a*?").findFirstMatchIn("aaab").get
      m.matched shouldBe ""
    }

    "a+ requires at least one" in {
      findFirst("a+", "bbb") shouldBe None
      firstMatch("a+", "baaab") shouldBe "aaa"
    }

    "a? matches zero or one" in {
      firstMatch("a?", "ab") shouldBe "a"
      val m = re("a?").findFirstMatchIn("xyz").get
      m.matched shouldBe ""
    }

    "{n} exact" in {
      firstMatch("a{3}", "aaaaa") shouldBe "aaa"
      findFirst("a{3}", "aa") shouldBe None
    }

    "{n,m} bounded" in {
      firstMatch("a{2,4}", "aaaaaa") shouldBe "aaaa"
      firstMatch("a{2,4}", "aaa") shouldBe "aaa"
    }

    "{n,} unbounded above" in {
      firstMatch("a{2,}", "aaaaa") shouldBe "aaaaa"
      findFirst("a{2,}", "a") shouldBe None
    }

    "possessive a*+ doesn't give back" in {
      // 'a*+a' can never match: a*+ swallows everything, then 'a' has
      // nothing left and can't backtrack into the possessive quantifier.
      findFirst("a*+a", "aaaa") shouldBe None
    }

    "greedy a*a does match by giving back" in {
      firstMatch("a*a", "aaaa") shouldBe "aaaa"
    }
  }

  "alternation" - {

    "leftmost branch is preferred when both could match" in {
      firstMatch("abc|abcd", "abcd") shouldBe "abc"
    }

    "second branch when first fails" in {
      firstMatch("xy|ab", "ab") shouldBe "ab"
    }

    "three-way" in {
      firstMatch("cat|dog|fish", "I have a dog") shouldBe "dog"
    }
  }

  "capture groups" - {

    "single group" in {
      val m = re("a(b+)c").findFirstMatchIn("abbbc").get
      m.matched shouldBe "abbbc"
      m.group(0) shouldBe Some("abbbc")
      m.group(1) shouldBe Some("bbb")
    }

    "two sibling groups" in {
      val m = re("(a+)(b+)").findFirstMatchIn("aaabbb").get
      m.group(1) shouldBe Some("aaa")
      m.group(2) shouldBe Some("bbb")
    }

    "nested groups" in {
      val m = re("(a(b)c)").findFirstMatchIn("abc").get
      m.group(1) shouldBe Some("abc")
      m.group(2) shouldBe Some("b")
    }

    "non-capturing group doesn't take an index" in {
      val r = re("(?:a)(b)")
      r.captureCount shouldBe 1
      // The 'b' is group 1 (the only explicit capture)
      val m = r.findFirstMatchIn("ab").get
      m.group(1) shouldBe Some("b")
    }

    "unmatched group reports None" in {
      // Only one of the alt arms captures; the other group is None.
      val m = re("(a)|(b)").findFirstMatchIn("b").get
      m.group(1) shouldBe None
      m.group(2) shouldBe Some("b")
    }

    "named group accessible by index" in {
      val r = re("(?<word>\\w+)")
      val m = r.findFirstMatchIn("hello world").get
      m.group(1) shouldBe Some("hello")
      r.namedGroups("word") shouldBe List(1)
    }

    "rawSlots reports correct length" in {
      val m = re("(a)(b)").findFirstMatchIn("ab").get
      // 2 explicit groups + 1 overall = 3 groups × 2 slots = 6
      m.slotCount shouldBe 6
    }
  }

  "anchors" - {

    "^ matches at input start" in {
      firstMatch("^abc", "abc def") shouldBe "abc"
      findFirst("^abc", "xyz abc") shouldBe None
    }

    "^ also matches after a newline (line mode default)" in {
      firstMatch("^abc", "xyz\nabc") shouldBe "abc"
    }

    "$ matches at input end" in {
      firstMatch("xyz$", "abc xyz") shouldBe "xyz"
    }

    "$ also matches before a newline" in {
      firstMatch("xyz$", "abc xyz\nmore") shouldBe "xyz"
    }

    "\\A only matches at input start" in {
      firstMatch("\\Aabc", "abc def") shouldBe "abc"
      findFirst("\\Aabc", "xyz\nabc") shouldBe None
    }

    "\\z only matches at input end" in {
      firstMatch("xyz\\z", "abc xyz") shouldBe "xyz"
      findFirst("xyz\\z", "abc xyz\n") shouldBe None
    }

    "\\Z allows one trailing newline" in {
      firstMatch("xyz\\Z", "abc xyz\n") shouldBe "xyz"
      findFirst("xyz\\Z", "abc xyz\nmore") shouldBe None
    }

    "\\b word boundary" in {
      firstMatch("\\bfoo\\b", "  foo bar") shouldBe "foo"
      findFirst("\\bfoo\\b", "barfoo") shouldBe None
    }

    "\\B non-word-boundary" in {
      // Should NOT match on a word boundary. 'foo' inside 'foobar' is
      // bordered by word chars, so \Bfoo\B works there.
      firstMatch("\\Bfoo\\B", "abfoobar") shouldBe "foo"
    }

    "\\G — first attempt is start of input" in {
      firstMatch("\\Gabc", "abc") shouldBe "abc"
      // At pos 1, \G doesn't match because prev=0
      findFirst("\\Gxyz", "xxyz") shouldBe None
    }
  }

  "findAllMatchesIn" - {

    "returns every non-overlapping match" in {
      val ms = re("\\d+").findAllMatchesIn("a1b22c333").toList
      ms.map(_.matched) shouldBe List("1", "22", "333")
    }

    "no matches → empty iterator" in {
      re("xyz").findAllMatchesIn("abcdef").toList shouldBe empty
    }

    "advances past zero-width hits" in {
      // A zero-width pattern would otherwise loop forever; the iterator
      // must force at least one codepoint of progress between matches.
      val ms = re("a*").findAllMatchesIn("aabaa").toList
      ms.map(_.matched) should contain ("aa")
      ms.size should be > 0
      ms.size should be < 100  // sanity: not infinite
    }
  }

  "errors" - {

    // Lookaround / backref / subroutine behaviour are exercised by the
    // dedicated Match*Spec files. Stage 4 added lookaround + backref
    // first-class support; Stage 5 added subroutine calls; Stage 6.C
    // added relative refs (resolved at parse time). This section pins
    // the constructs that are still rejected.

    "relative backref with no preceding group is rejected at parse time" in {
      // `\k<-1>` with groupCounter = 0 resolves to group 0 — caught at
      // the parser, not deferred to the compiler.
      compileFail("\\k<-1>") should include("before group 1")
    }

    "relative subroutine call with no preceding group is rejected" in {
      compileFail("\\g<-1>") should include("before group 1")
    }

    "backreference to undefined group is rejected" in {
      compileFail("\\1") should include("undefined group")
    }

    "subroutine call to undefined group is rejected" in {
      compileFail("\\g<7>") should include("undefined")
    }
  }

end MatchSpec
