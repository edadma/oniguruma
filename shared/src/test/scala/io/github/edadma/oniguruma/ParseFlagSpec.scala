package io.github.edadma.oniguruma

import Node.*

/** Inline flag groups: bare `(?i)` and scoped `(?i:…)`, with optional
  * `-` to clear flags. Bare form's body is `None`; scoped form's body
  * is `Some(node)`. The corpus has 454 inline-flag uses so this path
  * gets a lot of exercise. */
class ParseFlagSpec extends ParserHelpers:

  "bare flag scopes (no body)" - {

    "(?i)" in {
      parsed("(?i)") shouldBe FlagsScope(Flags.IgnoreCase, Flags.Empty, None)
    }

    "(?m)" in {
      parsed("(?m)") shouldBe FlagsScope(Flags.DotAll, Flags.Empty, None)
    }

    "(?x)" in {
      parsed("(?x)") shouldBe FlagsScope(Flags.Extended, Flags.Empty, None)
    }

    "(?im) combines" in {
      parsed("(?im)") shouldBe FlagsScope(
        Flags.union(Flags.IgnoreCase, Flags.DotAll),
        Flags.Empty,
        None,
      )
    }

    "(?imx) all three" in {
      val all = Flags.union(Flags.union(Flags.IgnoreCase, Flags.DotAll), Flags.Extended)
      parsed("(?imx)") shouldBe FlagsScope(all, Flags.Empty, None)
    }

    "(?-i) clears" in {
      parsed("(?-i)") shouldBe FlagsScope(Flags.Empty, Flags.IgnoreCase, None)
    }

    "(?i-x) sets and clears" in {
      parsed("(?i-x)") shouldBe FlagsScope(Flags.IgnoreCase, Flags.Extended, None)
    }

    "(?im-x) sets two clears one" in {
      parsed("(?im-x)") shouldBe FlagsScope(
        Flags.union(Flags.IgnoreCase, Flags.DotAll),
        Flags.Extended,
        None,
      )
    }
  }

  "scoped flag groups" - {

    "(?i:body)" in {
      parsed("(?i:abc)") shouldBe FlagsScope(
        Flags.IgnoreCase,
        Flags.Empty,
        Some(Literal("abc")),
      )
    }

    "(?-i:body) — only clears" in {
      parsed("(?-i:abc)") shouldBe FlagsScope(
        Flags.Empty,
        Flags.IgnoreCase,
        Some(Literal("abc")),
      )
    }

    "(?im-x:body)" in {
      parsed("(?im-x:abc)") shouldBe FlagsScope(
        Flags.union(Flags.IgnoreCase, Flags.DotAll),
        Flags.Extended,
        Some(Literal("abc")),
      )
    }

    "(?:) is non-capturing, not flag scope" in {
      // Empty flags before `:` produces an empty FlagsScope wrapper —
      // this is technically distinguishable from `(?:body)` non-capturing
      // group. Onig treats `(?:body)` as the non-capturing form, so the
      // parser dispatches `(?:` to non-capturing before reaching the flag
      // path. Confirm we still get NonCapturing here.
      parsed("(?:abc)") shouldBe Group(GroupKind.NonCapturing, Literal("abc"))
    }

    "scoped with alternation" in {
      parsed("(?i:a|b)") shouldBe FlagsScope(
        Flags.IgnoreCase,
        Flags.Empty,
        Some(Alt(List(Literal("a"), Literal("b")))),
      )
    }

    "scoped with quantifier outside" in {
      parsed("(?i:a)+") shouldBe Quantified(
        FlagsScope(Flags.IgnoreCase, Flags.Empty, Some(Literal("a"))),
        Quant(1, None, Greedy),
      )
    }

    "nested scopes" in {
      parsed("(?i:(?-i:a))") shouldBe FlagsScope(
        Flags.IgnoreCase,
        Flags.Empty,
        Some(FlagsScope(Flags.Empty, Flags.IgnoreCase, Some(Literal("a")))),
      )
    }

    "scope does not allocate group index" in {
      parsed("(?i:a)(b)") shouldBe Concat(
        List(
          FlagsScope(Flags.IgnoreCase, Flags.Empty, Some(Literal("a"))),
          Group(GroupKind.Capturing(1, None), Literal("b")),
        )
      )
    }
  }

  "errors" - {

    "unknown flag" in {
      failParse("(?z)").message should include("unknown flag")
    }

    "duplicate dash" in {
      failParse("(?--i)").message should include("duplicate")
    }

    "unterminated bare flag" in {
      failParse("(?i").message should include("unterminated")
    }

    "unterminated scoped flag" in {
      failParse("(?i:a").message should include("expected ')'")
    }
  }

  "extended-mode whitespace skipping" - {

    "(?x) skips literal spaces between atoms" in {
      // Without (?x), `a b` is "a then space then b". With (?x), the
      // space is invisible and the result collapses to the literal "ab".
      parsed("(?x)a b") shouldBe Concat(
        List(FlagsScope(Flags.Extended, Flags.Empty, None), Literal("ab"))
      )
    }

    "(?x) skips tabs and newlines too" in {
      parsed("(?x)a\tb\nc") shouldBe Concat(
        List(FlagsScope(Flags.Extended, Flags.Empty, None), Literal("abc"))
      )
    }

    "(?x) skips # line-comment to end of line" in {
      parsed("(?x)a # comment\nb") shouldBe Concat(
        List(FlagsScope(Flags.Extended, Flags.Empty, None), Literal("ab"))
      )
    }

    "(?x) does NOT skip whitespace inside char classes" in {
      // Inside [...] whitespace is literal even under extended mode.
      parsed("(?x)[ ]") shouldBe Concat(
        List(
          FlagsScope(Flags.Extended, Flags.Empty, None),
          Klass(IntervalSet.single(' '), false),
        )
      )
    }

    "(?x:body) is scoped — extended mode does not leak past )" in {
      // After the scope closes, the trailing ' b' should NOT be skipped:
      // it parses as literal " b".
      parsed("(?x:a)b c") shouldBe Concat(
        List(
          FlagsScope(Flags.Extended, Flags.Empty, Some(Literal("a"))),
          Literal("b c"),
        )
      )
    }

    "(?x) leaks within its surrounding group" in {
      // Inside the outer (...), bare (?x) makes the rest extended;
      // outside the outer ), extension does not apply.
      parsed("((?x)a b)c d") shouldBe Concat(
        List(
          Group(
            GroupKind.Capturing(1, None),
            Concat(List(FlagsScope(Flags.Extended, Flags.Empty, None), Literal("ab"))),
          ),
          Literal("c d"),
        )
      )
    }

    "(?x) skips before a quantifier too" in {
      // `a *` with extended mode is `a*` because the space is invisible.
      parsed("(?x)a *") shouldBe Concat(
        List(
          FlagsScope(Flags.Extended, Flags.Empty, None),
          Quantified(Literal("a"), Quant(0, None, Greedy)),
        )
      )
    }

    "(?-x:body) clears extended mode in nested scope" in {
      // Inside (?-x:...), whitespace should be literal again.
      parsed("(?x)(?-x: )a") shouldBe Concat(
        List(
          FlagsScope(Flags.Extended, Flags.Empty, None),
          FlagsScope(Flags.Empty, Flags.Extended, Some(Literal(" "))),
          Literal("a"),
        )
      )
    }
  }

end ParseFlagSpec
