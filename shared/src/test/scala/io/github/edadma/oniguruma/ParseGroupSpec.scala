package io.github.edadma.oniguruma

import Node.*
import GroupKind.{Capturing, NonCapturing, Atomic}

/** Group syntax: capturing, non-capturing `(?:…)`, named `(?<name>…)`,
  * atomic `(?>…)`, capture-index assignment, nesting, and the error
  * cases. Lookaround is its own spec, even though it shares the
  * `(?` opener. */
class ParseGroupSpec extends ParserHelpers:

  "capturing groups" - {

    "single capturing group gets index 1" in {
      parsed("(a)") shouldBe Group(Capturing(1, None), Literal("a"))
    }

    "two sibling capturing groups get 1 and 2" in {
      parsed("(a)(b)") shouldBe Concat(
        List(
          Group(Capturing(1, None), Literal("a")),
          Group(Capturing(2, None), Literal("b")),
        )
      )
    }

    "nested capturing groups number outermost-first" in {
      parsed("(a(b))") shouldBe Group(
        Capturing(1, None),
        Concat(List(Literal("a"), Group(Capturing(2, None), Literal("b")))),
      )
    }

    "non-capturing groups don't increment counter" in {
      parsed("(a(?:x)b)") shouldBe Group(
        Capturing(1, None),
        Concat(List(Literal("a"), Group(NonCapturing, Literal("x")), Literal("b"))),
      )
    }

    "atomic groups don't increment counter" in {
      parsed("(?>a)(b)") shouldBe Concat(
        List(
          Group(Atomic, Literal("a")),
          Group(Capturing(1, None), Literal("b")),
        )
      )
    }

    "empty body" in {
      parsed("()") shouldBe Group(Capturing(1, None), Empty)
    }

    "alternation inside" in {
      parsed("(a|b)") shouldBe Group(
        Capturing(1, None),
        Alt(List(Literal("a"), Literal("b"))),
      )
    }
  }

  "non-capturing groups" - {

    "(?:body)" in {
      parsed("(?:a)") shouldBe Group(NonCapturing, Literal("a"))
    }

    "(?:) empty body" in {
      parsed("(?:)") shouldBe Group(NonCapturing, Empty)
    }

    "alternation inside non-capturing" in {
      parsed("(?:a|b)") shouldBe Group(
        NonCapturing,
        Alt(List(Literal("a"), Literal("b"))),
      )
    }

    "nested non-capturing doesn't allocate index" in {
      parsed("(?:(?:a))") shouldBe Group(NonCapturing, Group(NonCapturing, Literal("a")))
    }
  }

  "named groups" - {

    "(?<name>body) gets index 1" in {
      parsed("(?<x>a)") shouldBe Group(Capturing(1, Some("x")), Literal("a"))
    }

    "longer name" in {
      parsed("(?<myName>a)") shouldBe Group(Capturing(1, Some("myName")), Literal("a"))
    }

    "two named groups get sequential indices" in {
      parsed("(?<a>x)(?<b>y)") shouldBe Concat(
        List(
          Group(Capturing(1, Some("a")), Literal("x")),
          Group(Capturing(2, Some("b")), Literal("y")),
        )
      )
    }

    "named and unnamed mixed" in {
      parsed("(a)(?<b>c)") shouldBe Concat(
        List(
          Group(Capturing(1, None), Literal("a")),
          Group(Capturing(2, Some("b")), Literal("c")),
        )
      )
    }

    "duplicate names are accepted (Onig branch-reset idiom)" in {
      // Onig allows multiple `(?<name>…)` declarations sharing a name —
      // the alt-arm "branch reset" pattern relies on it. Each occurrence
      // gets its own capture index; they all share the name for `\k<name>`
      // resolution at compile time.
      parsed("(?<x>a)(?<x>b)") shouldBe Concat(
        List(
          Group(Capturing(1, Some("x")), Literal("a")),
          Group(Capturing(2, Some("x")), Literal("b")),
        )
      )
    }

    "empty name fails" in {
      failParse("(?<>a)").message should include("empty")
    }

    "unterminated name fails" in {
      failParse("(?<x").message should include("unterminated")
    }
  }

  "atomic groups" - {

    "(?>body)" in {
      parsed("(?>a)") shouldBe Group(Atomic, Literal("a"))
    }

    "atomic doesn't capture" in {
      // The named-group-after-atomic test confirms atomic doesn't take an index.
      parsed("(?>a)(b)") shouldBe Concat(
        List(
          Group(Atomic, Literal("a")),
          Group(Capturing(1, None), Literal("b")),
        )
      )
    }

    "atomic alt inside" in {
      parsed("(?>a|b)") shouldBe Group(Atomic, Alt(List(Literal("a"), Literal("b"))))
    }

    "atomic with quantifier" in {
      parsed("(?>a)+") shouldBe Quantified(Group(Atomic, Literal("a")), Quant(1, None, Greedy))
    }
  }

  "errors" - {

    "unterminated capturing group" in {
      failParse("(a").message should include("expected ')'")
    }

    "unterminated non-capturing group" in {
      failParse("(?:a").message should include("expected ')'")
    }

    "stray close paren is an error" in {
      // The top-level cleanup catches anything left after parseAlt returns.
      failParse(")").message should include("unexpected")
    }
  }

end ParseGroupSpec
