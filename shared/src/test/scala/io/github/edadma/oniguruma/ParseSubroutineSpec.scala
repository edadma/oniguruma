package io.github.edadma.oniguruma

import Node.*

/** Subroutine calls: `\g<n>`, `\g<name>`, `\g<0>` (whole-pattern
  * recursion), `\g<+1>`, `\g<-1>`. The Go grammar relies on `\g<n>`,
  * which is why TextMate support requires this even though most flavors
  * don't have it. */
class ParseSubroutineSpec extends ParserHelpers:

  "by number" - {

    "\\g<1>" in {
      parsed("\\g<1>") shouldBe SubroutineCall(GroupRef.ByNumber(1))
    }

    "\\g<5>" in {
      parsed("\\g<5>") shouldBe SubroutineCall(GroupRef.ByNumber(5))
    }

    "\\g<0> recursion" in {
      // The whole-pattern recursion: equivalent to (?R) in PCRE flavor.
      parsed("\\g<0>") shouldBe SubroutineCall(GroupRef.ByNumber(0))
    }
  }

  "by name" - {

    "\\g<x>" in {
      parsed("\\g<x>") shouldBe SubroutineCall(GroupRef.ByName("x"))
    }

    "\\g<longerName>" in {
      parsed("\\g<longerName>") shouldBe SubroutineCall(GroupRef.ByName("longerName"))
    }

    "named subroutine call after named capture" in {
      parsed("(?<x>a)\\g<x>") shouldBe Concat(
        List(
          Group(GroupKind.Capturing(1, Some("x")), Literal("a")),
          SubroutineCall(GroupRef.ByName("x")),
        )
      )
    }
  }

  "relative" - {
    // Stage 6.C — relative refs resolve to absolute group indices at
    // parse time using the running group-counter. The AST never carries
    // `ByRelative` after parsing.

    "\\g<+1>(a) resolves a forward call to group 1" in {
      parsed("\\g<+1>(a)") shouldBe Concat(
        List(SubroutineCall(GroupRef.ByNumber(1)), Group(GroupKind.Capturing(1, None), Literal("a")))
      )
    }

    "(a)\\g<-1> resolves a backward call to the prior group" in {
      parsed("(a)\\g<-1>") shouldBe Concat(
        List(Group(GroupKind.Capturing(1, None), Literal("a")), SubroutineCall(GroupRef.ByNumber(1)))
      )
    }

    "\\g<+2>(a)(b) resolves a multi-step forward call" in {
      parsed("\\g<+2>(a)(b)") shouldBe Concat(
        List(
          SubroutineCall(GroupRef.ByNumber(2)),
          Group(GroupKind.Capturing(1, None), Literal("a")),
          Group(GroupKind.Capturing(2, None), Literal("b")),
        )
      )
    }

    "(a\\g<-1>?) resolves a self-call inside the group it lives in" in {
      // groupCounter = 1 inside the body of (...) — so -1 means the
      // group currently being parsed. Equivalent to `\g<1>` here.
      parsed("(a\\g<-1>?)") shouldBe Group(
        GroupKind.Capturing(1, None),
        Concat(
          List(
            Literal("a"),
            Quantified(SubroutineCall(GroupRef.ByNumber(1)), Quant(0, Some(1), Greedy)),
          )
        ),
      )
    }
  }

  "self-recursive group pattern" in {
    // `(a\g<0>?)` — one of the standard "balanced parens" patterns.
    parsed("(a\\g<0>?)") shouldBe Group(
      GroupKind.Capturing(1, None),
      Concat(
        List(
          Literal("a"),
          Quantified(SubroutineCall(GroupRef.ByNumber(0)), Quant(0, Some(1), Greedy)),
        )
      ),
    )
  }

  "errors" - {

    "\\g without <" in {
      failParse("\\ga").message should include("expected '<'")
    }

    "\\g<>" in {
      failParse("\\g<>").message should include("empty")
    }

    "\\g<x unterminated" in {
      failParse("\\g<x").message should include("unterminated")
    }

    "\\g<+0> rejected" in {
      failParse("\\g<+0>").message should include("offset 0")
    }

    "\\g<-1> with no preceding group is rejected at parse time" in {
      // groupCounter = 0 → -1 resolves to group 0, which is reserved
      // for whole-pattern recursion only. Caught at parse, not deferred
      // to the compiler.
      failParse("\\g<-1>").message should include("before group 1")
    }
  }

end ParseSubroutineSpec
