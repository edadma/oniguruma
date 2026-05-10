package io.github.edadma.oniguruma

/** End-to-end subroutine-call behaviour. Stage 5 supports `\g<n>`,
  * `\g<name>`, and `\g<0>` (whole-pattern recursion). The classic
  * use case is balanced-bracket matching — the Go grammar in the
  * TextMate corpus uses `\g<n>` for nested type expressions, and
  * the `\g<0>` form shows up in any "match nested parens" idiom.
  *
  * Capture semantics: a subroutine call does NOT propagate captures
  * back to the caller (PCRE `(?R)`-style). The VM snapshots the slot
  * array on `Call` and restores it on return — so the called group's
  * outer captures stay tied to the original (caller) match, even
  * though `Save` instructions inside the call fire normally. */
class MatchSubroutineSpec extends CompilerHelpers:

  "\\g<n> — re-execute a group's body" - {

    "(a)\\g<1> matches 'aa'" in {
      firstMatch("(a)\\g<1>", "aa") shouldBe "aa"
    }

    "(a)\\g<1> fails on 'ab'" in {
      findFirst("(a)\\g<1>", "ab") shouldBe None
    }

    "subroutine call captures don't propagate to caller" in {
      // PCRE `(?R)`-style: after `(a)\g<1>` matches 'aa', group 1's
      // captured text is from the original `(a)` match (the FIRST 'a'),
      // not from the subroutine call (the SECOND 'a'). Either would
      // print as "a" so test the start offset, which differs.
      val m = re("(a)\\g<1>").findFirstMatchIn("aa").get
      m.matched shouldBe "aa"
      m.group(1) shouldBe Some("a")
      m.groupRange(1) shouldBe Some((0, 1))  // original capture, not the call's
    }

    "forward-referenced subroutine works after patch" in {
      // `\g<1>(a)` calls group 1 before its body has been emitted in
      // source order. The compiler patches the Call once the program
      // is complete.
      firstMatch("\\g<1>(a)", "aa") shouldBe "aa"
    }
  }

  "\\g<name> — named subroutine call" - {

    "(?<word>\\w+)\\s\\g<word>" in {
      // Match a word, a single space, then re-run the same word body —
      // matches "the dog" (any two words separated by space) because
      // the body re-matches independently.
      firstMatch("(?<word>\\w+)\\s\\g<word>", "the dog") shouldBe "the dog"
    }
  }

  "\\g<0> — whole-pattern recursion" - {

    "balanced parentheses (\\(\\g<0>?\\)) — nests one level" in {
      firstMatch("\\(\\g<0>?\\)", "()") shouldBe "()"
      firstMatch("\\(\\g<0>?\\)", "(())") shouldBe "(())"
      firstMatch("\\(\\g<0>?\\)", "((()))") shouldBe "((()))"
    }

    "balanced parens fail when unbalanced" in {
      // No '(' at start — the outer body's first '\(' fails immediately.
      findFirst("^\\(\\g<0>?\\)", "()(") shouldBe defined
      // Pure mismatched: neither '(' nor ')' in the right order.
      findFirst("^\\(\\g<0>?\\)$", "(()") shouldBe None
    }

    "nested via captured group: (\\((?:[^()]|\\g<1>)*\\)) self-references" in {
      // Group 1 is the whole bracket body; the recursion handles nested
      // pairs while the alt-arm `[^()]` consumes ordinary content.
      firstMatch("(\\((?:[^()]|\\g<1>)*\\))", "before(a(b)c)after") shouldBe "(a(b)c)"
    }

    "recursion guard prevents infinite \\g<0>" in {
      // `\g<0>` with no progress requirement would normally recurse
      // forever; the VM's `MaxRecursionDepth` cap stops it cleanly.
      // The pattern below has no chance of matching against an empty
      // input but must NOT hang or stack-overflow.
      findFirst("\\g<0>", "") shouldBe None
    }
  }

  "relative subroutine calls" - {
    // Stage 6.C — `\g<-N>` / `\g<+N>` resolve at parse time using the
    // running group-counter; runtime semantics match the absolute form.

    "(a)\\g<-1> — call back to the most-recent group" in {
      firstMatch("(a)\\g<-1>", "aa") shouldBe "aa"
      findFirst("(a)\\g<-1>", "ab") shouldBe None
    }

    "(a)(b)\\g<-2> — call back two groups" in {
      // -2 from after (a)(b) calls group 1 — re-matches 'a' rather than
      // 'b'.
      firstMatch("(a)(b)\\g<-2>", "aba") shouldBe "aba"
      findFirst("(a)(b)\\g<-2>", "abb") shouldBe None
    }

    "\\g<+1>(a) — forward call invokes group 1's body before its definition" in {
      firstMatch("\\g<+1>(a)", "aa") shouldBe "aa"
    }

    "(a\\g<-1>?) — self-referential group via -1" in {
      // Inside (...), groupCounter = 1 → -1 maps to group 1 (this
      // group). Equivalent to `\g<0>` for a single top-level group,
      // but expressed via the relative form.
      firstMatch("(a\\g<-1>?)", "aaa") shouldBe "aaa"
    }
  }

end MatchSubroutineSpec
