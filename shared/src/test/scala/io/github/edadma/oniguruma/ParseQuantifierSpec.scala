package io.github.edadma.oniguruma

import Node.*
import GroupKind.{Capturing, NonCapturing}

/** Quantifiers: `?`, `*`, `+`, `{n}`, `{n,}`, `{n,m}` × greedy / reluctant
  * (`?`) / possessive (`+`) modes, including the brace-quantifier corner
  * cases where `{` should be treated as a literal. */
class ParseQuantifierSpec extends ParserHelpers:

  "primitive quantifiers" - {

    "? is 0 or 1, greedy" in {
      parsed("a?") shouldBe Quantified(Literal("a"), Quant(0, Some(1), Greedy))
    }

    "* is 0 or more, greedy" in {
      parsed("a*") shouldBe Quantified(Literal("a"), Quant(0, None, Greedy))
    }

    "+ is 1 or more, greedy" in {
      parsed("a+") shouldBe Quantified(Literal("a"), Quant(1, None, Greedy))
    }
  }

  "brace quantifiers" - {

    "{n} is exactly n" in {
      parsed("a{3}") shouldBe Quantified(Literal("a"), Quant(3, Some(3), Greedy))
    }

    "{n,} is at least n" in {
      parsed("a{3,}") shouldBe Quantified(Literal("a"), Quant(3, None, Greedy))
    }

    "{n,m} is between n and m" in {
      parsed("a{3,5}") shouldBe Quantified(Literal("a"), Quant(3, Some(5), Greedy))
    }

    "{0} is allowed" in {
      parsed("a{0}") shouldBe Quantified(Literal("a"), Quant(0, Some(0), Greedy))
    }

    "{0,0} is allowed" in {
      parsed("a{0,0}") shouldBe Quantified(Literal("a"), Quant(0, Some(0), Greedy))
    }

    "{0,1} equivalent to ?" in {
      parsed("a{0,1}") shouldBe Quantified(Literal("a"), Quant(0, Some(1), Greedy))
    }

    "min > max fails" in {
      failParse("a{5,3}").message should include("invalid quantifier range")
    }

    "{ alone is literal brace" in {
      parsed("{") shouldBe Literal("{")
    }

    "{x} is literal" in {
      parsed("{x}") shouldBe Literal("{x}")
    }

    "{,3} is literal (no leading n)" in {
      parsed("{,3}") shouldBe Literal("{,3}")
    }

    "{3 unterminated is literal" in {
      parsed("{3") shouldBe Literal("{3")
    }
  }

  "modes" - {

    "*? is reluctant" in {
      parsed("a*?") shouldBe Quantified(Literal("a"), Quant(0, None, Reluctant))
    }

    "+? is reluctant" in {
      parsed("a+?") shouldBe Quantified(Literal("a"), Quant(1, None, Reluctant))
    }

    "?? is reluctant" in {
      parsed("a??") shouldBe Quantified(Literal("a"), Quant(0, Some(1), Reluctant))
    }

    "{n,m}? is reluctant" in {
      parsed("a{3,5}?") shouldBe Quantified(Literal("a"), Quant(3, Some(5), Reluctant))
    }

    "*+ is possessive" in {
      parsed("a*+") shouldBe Quantified(Literal("a"), Quant(0, None, Possessive))
    }

    "++ is possessive" in {
      parsed("a++") shouldBe Quantified(Literal("a"), Quant(1, None, Possessive))
    }

    "?+ is possessive" in {
      parsed("a?+") shouldBe Quantified(Literal("a"), Quant(0, Some(1), Possessive))
    }

    "{n,m}+ is possessive" in {
      parsed("a{2,4}+") shouldBe Quantified(Literal("a"), Quant(2, Some(4), Possessive))
    }
  }

  "applied to different primaries" - {

    "applied to dot" in {
      parsed(".+") shouldBe Quantified(Dot, Quant(1, None, Greedy))
    }

    "applied to char class" in {
      parsed("[abc]+") shouldBe Quantified(Klass(IntervalSet.fromChars("abc"), false), Quant(1, None, Greedy))
    }

    "applied to capturing group" in {
      parsed("(a)*") shouldBe Quantified(Group(Capturing(1, None), Literal("a")), Quant(0, None, Greedy))
    }

    "applied to non-capturing group" in {
      parsed("(?:a)?") shouldBe Quantified(Group(NonCapturing, Literal("a")), Quant(0, Some(1), Greedy))
    }

    "applied to escape literal" in {
      parsed("\\.+") shouldBe Quantified(Literal("."), Quant(1, None, Greedy))
    }
  }

  "binding" - {

    "binds to last char of literal run" in {
      // "abc*" parses as "ab" then "c*" — the quantifier binds to one char.
      parsed("abc*") shouldBe Concat(
        List(Literal("ab"), Quantified(Literal("c"), Quant(0, None, Greedy)))
      )
    }

    "binds to last char of escape" in {
      parsed("ab\\n+") shouldBe Concat(
        List(Literal("ab"), Quantified(Literal("\n"), Quant(1, None, Greedy)))
      )
    }
  }

  "errors" - {

    "lone * fails" in {
      failParse("*").message should include("nothing to apply")
    }

    "lone + fails" in {
      failParse("+").message should include("nothing to apply")
    }

    "lone ? fails" in {
      failParse("?").message should include("nothing to apply")
    }

    "{3} alone fails" in {
      failParse("{3}").message should include("nothing to apply")
    }

    "after | fails" in {
      failParse("a|*").message should include("nothing to apply")
    }
  }

end ParseQuantifierSpec
