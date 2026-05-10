package io.github.edadma.oniguruma

import Node.*
import GroupKind.{Capturing, NonCapturing}

/** Top-level alternation: branches with `|`, empty branches, interaction
  * with grouping, and quantifiers in branches. */
class ParseAlternationSpec extends ParserHelpers:

  "two-branch alternation" in {
    parsed("a|b") shouldBe Alt(List(Literal("a"), Literal("b")))
  }

  "three-branch alternation" in {
    parsed("a|b|c") shouldBe Alt(List(Literal("a"), Literal("b"), Literal("c")))
  }

  "alternation with multi-char literals" in {
    parsed("foo|bar") shouldBe Alt(List(Literal("foo"), Literal("bar")))
  }

  "empty branches" - {

    "trailing empty branch" in {
      parsed("a|") shouldBe Alt(List(Literal("a"), Empty))
    }

    "leading empty branch" in {
      parsed("|a") shouldBe Alt(List(Empty, Literal("a")))
    }

    "all-empty alternation" in {
      parsed("||") shouldBe Alt(List(Empty, Empty, Empty))
    }

    "double pipe in middle" in {
      parsed("a||b") shouldBe Alt(List(Literal("a"), Empty, Literal("b")))
    }
  }

  "nested via groups" - {

    "alternation inside capturing group" in {
      parsed("(a|b)") shouldBe Group(Capturing(1, None), Alt(List(Literal("a"), Literal("b"))))
    }

    "alternation inside non-capturing group" in {
      parsed("(?:a|b)") shouldBe Group(NonCapturing, Alt(List(Literal("a"), Literal("b"))))
    }

    "outer alt with grouped inner alt" in {
      parsed("a|(b|c)|d") shouldBe Alt(
        List(
          Literal("a"),
          Group(Capturing(1, None), Alt(List(Literal("b"), Literal("c")))),
          Literal("d"),
        )
      )
    }
  }

  "mixed with quantifiers" - {

    "branches with quantifiers" in {
      parsed("a*|b+") shouldBe Alt(
        List(
          Quantified(Literal("a"), Quant(0, None, Greedy)),
          Quantified(Literal("b"), Quant(1, None, Greedy)),
        )
      )
    }

    "branch is concat with quantifier" in {
      parsed("ab*|c") shouldBe Alt(
        List(
          Concat(List(Literal("a"), Quantified(Literal("b"), Quant(0, None, Greedy)))),
          Literal("c"),
        )
      )
    }
  }

  "single branch is not wrapped in Alt" in {
    // The parser only emits `Alt` when there are at least two branches.
    parsed("abc") shouldBe Literal("abc")
  }

end ParseAlternationSpec
