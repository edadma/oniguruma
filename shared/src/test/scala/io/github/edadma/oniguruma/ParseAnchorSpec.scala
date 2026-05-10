package io.github.edadma.oniguruma

import Node.*

/** Zero-width anchors. The `\G` (previous-match-end) anchor is critical
  * for the TextMate scanner — its 201 corpus uses make this load-bearing
  * for the engine. */
class ParseAnchorSpec extends ParserHelpers:

  "^" in {
    parsed("^") shouldBe Anchor(AnchorKind.Caret)
  }

  "$" in {
    parsed("$") shouldBe Anchor(AnchorKind.Dollar)
  }

  "\\A — begin input" in {
    parsed("\\A") shouldBe Anchor(AnchorKind.BeginInput)
  }

  "\\z — end input" in {
    parsed("\\z") shouldBe Anchor(AnchorKind.EndInput)
  }

  "\\Z — end before final newline" in {
    parsed("\\Z") shouldBe Anchor(AnchorKind.EndInputBeforeFinalNewline)
  }

  "\\G — previous match end" in {
    parsed("\\G") shouldBe Anchor(AnchorKind.PrevMatchEnd)
  }

  "\\b — word boundary" in {
    parsed("\\b") shouldBe Anchor(AnchorKind.WordBoundary)
  }

  "\\B — non-word boundary" in {
    parsed("\\B") shouldBe Anchor(AnchorKind.NonWordBoundary)
  }

  "anchor mixed with literal" - {

    "^foo" in {
      parsed("^foo") shouldBe Concat(List(Anchor(AnchorKind.Caret), Literal("foo")))
    }

    "foo$" in {
      parsed("foo$") shouldBe Concat(List(Literal("foo"), Anchor(AnchorKind.Dollar)))
    }

    "^foo$" in {
      parsed("^foo$") shouldBe Concat(
        List(Anchor(AnchorKind.Caret), Literal("foo"), Anchor(AnchorKind.Dollar))
      )
    }

    "\\bfoo\\b" in {
      parsed("\\bfoo\\b") shouldBe Concat(
        List(
          Anchor(AnchorKind.WordBoundary),
          Literal("foo"),
          Anchor(AnchorKind.WordBoundary),
        )
      )
    }

    "\\Gfoo" in {
      parsed("\\Gfoo") shouldBe Concat(List(Anchor(AnchorKind.PrevMatchEnd), Literal("foo")))
    }
  }

  "anchors inside groups" - {

    "(^a)" in {
      parsed("(^a)") shouldBe Group(
        GroupKind.Capturing(1, None),
        Concat(List(Anchor(AnchorKind.Caret), Literal("a"))),
      )
    }

    "(a$|b)" in {
      parsed("(a$|b)") shouldBe Group(
        GroupKind.Capturing(1, None),
        Alt(
          List(
            Concat(List(Literal("a"), Anchor(AnchorKind.Dollar))),
            Literal("b"),
          )
        ),
      )
    }
  }

end ParseAnchorSpec
