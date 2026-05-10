package io.github.edadma.oniguruma

import Node.*

/** All four lookaround constructs. Lookarounds in Onig don't allocate
  * group indices and they accept arbitrary contents (including nested
  * lookaround and lookahead capturing groups whose indices count). */
class ParseLookaroundSpec extends ParserHelpers:

  "(?=…) lookahead" in {
    parsed("(?=a)") shouldBe Lookaround(true, false, Literal("a"))
  }

  "(?!…) negative lookahead" in {
    parsed("(?!a)") shouldBe Lookaround(true, true, Literal("a"))
  }

  "(?<=…) lookbehind" in {
    parsed("(?<=a)") shouldBe Lookaround(false, false, Literal("a"))
  }

  "(?<!…) negative lookbehind" in {
    parsed("(?<!a)") shouldBe Lookaround(false, true, Literal("a"))
  }

  "empty bodies are allowed" - {

    "(?=) is a zero-width always-succeeds" in {
      parsed("(?=)") shouldBe Lookaround(true, false, Empty)
    }

    "(?!) is zero-width always-fails" in {
      parsed("(?!)") shouldBe Lookaround(true, true, Empty)
    }

    "(?<=)" in {
      parsed("(?<=)") shouldBe Lookaround(false, false, Empty)
    }

    "(?<!)" in {
      parsed("(?<!)") shouldBe Lookaround(false, true, Empty)
    }
  }

  "structured bodies" - {

    "alternation inside" in {
      parsed("(?=a|b)") shouldBe Lookaround(true, false, Alt(List(Literal("a"), Literal("b"))))
    }

    "concat inside" in {
      parsed("(?=ab)") shouldBe Lookaround(true, false, Literal("ab"))
    }

    "nested lookaround" in {
      parsed("(?=ab(?!c))") shouldBe Lookaround(
        true,
        false,
        Concat(List(Literal("ab"), Lookaround(true, true, Literal("c")))),
      )
    }
  }

  "lookaround does not advance group counter" in {
    // The capturing group AFTER the lookahead should still be #1.
    parsed("(?=ignore)(a)") shouldBe Concat(
      List(
        Lookaround(true, false, Literal("ignore")),
        Group(GroupKind.Capturing(1, None), Literal("a")),
      )
    )
  }

  "capturing groups inside lookaround DO count" in {
    // Inside a lookahead, a capturing group is still numbered.
    parsed("(?=(a))(b)") shouldBe Concat(
      List(
        Lookaround(true, false, Group(GroupKind.Capturing(1, None), Literal("a"))),
        Group(GroupKind.Capturing(2, None), Literal("b")),
      )
    )
  }

  "errors" - {

    "unterminated lookahead" in {
      failParse("(?=a").message should include("expected ')'")
    }

    "unterminated lookbehind" in {
      failParse("(?<=a").message should include("expected ')'")
    }
  }

end ParseLookaroundSpec
