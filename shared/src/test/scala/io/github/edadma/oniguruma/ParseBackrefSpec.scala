package io.github.edadma.oniguruma

import Node.*

/** Backreferences: `\1`-`\9` (and beyond), `\k<name>`, `\k<n>`,
  * `\k<-1>`, `\k<+1>`. Relative refs are resolved to absolute indices
  * at parse time using the running group-counter — the AST never
  * carries `ByRelative` from a parsed regex. Forward `+N` references
  * resolve to a number that may not yet be defined; the compiler
  * validates that against the final capture count. */
class ParseBackrefSpec extends ParserHelpers:

  "single-digit numeric backref" - {

    "\\1" in {
      parsed("\\1") shouldBe Backref(GroupRef.ByNumber(1))
    }

    "\\9" in {
      parsed("\\9") shouldBe Backref(GroupRef.ByNumber(9))
    }
  }

  "multi-digit numeric backref" - {

    "\\10 reads both digits" in {
      parsed("\\10") shouldBe Backref(GroupRef.ByNumber(10))
    }

    "\\123 reads all digits" in {
      parsed("\\123") shouldBe Backref(GroupRef.ByNumber(123))
    }
  }

  "after a capture" - {

    "(a)\\1 simplest case" in {
      parsed("(a)\\1") shouldBe Concat(
        List(Group(GroupKind.Capturing(1, None), Literal("a")), Backref(GroupRef.ByNumber(1)))
      )
    }
  }

  "k<...> form" - {

    "\\k<1>" in {
      parsed("\\k<1>") shouldBe Backref(GroupRef.ByNumber(1))
    }

    "\\k<3>" in {
      parsed("\\k<3>") shouldBe Backref(GroupRef.ByNumber(3))
    }

    "\\k<name>" in {
      parsed("\\k<myname>") shouldBe Backref(GroupRef.ByName("myname"))
    }

    "named backref after named capture" in {
      parsed("(?<x>a)\\k<x>") shouldBe Concat(
        List(
          Group(GroupKind.Capturing(1, Some("x")), Literal("a")),
          Backref(GroupRef.ByName("x")),
        )
      )
    }

    "(a)\\k<-1> resolves -1 to the most-recently-opened group" in {
      parsed("(a)\\k<-1>") shouldBe Concat(
        List(Group(GroupKind.Capturing(1, None), Literal("a")), Backref(GroupRef.ByNumber(1)))
      )
    }

    "(a)(b)\\k<-1> picks the more recent of two groups" in {
      parsed("(a)(b)\\k<-1>") shouldBe Concat(
        List(
          Group(GroupKind.Capturing(1, None), Literal("a")),
          Group(GroupKind.Capturing(2, None), Literal("b")),
          Backref(GroupRef.ByNumber(2)),
        )
      )
    }

    "(a)(b)\\k<-2> reaches the older group" in {
      parsed("(a)(b)\\k<-2>") shouldBe Concat(
        List(
          Group(GroupKind.Capturing(1, None), Literal("a")),
          Group(GroupKind.Capturing(2, None), Literal("b")),
          Backref(GroupRef.ByNumber(1)),
        )
      )
    }

    "\\k<+1>(a) resolves a forward reference" in {
      parsed("\\k<+1>(a)") shouldBe Concat(
        List(Backref(GroupRef.ByNumber(1)), Group(GroupKind.Capturing(1, None), Literal("a")))
      )
    }

    "(a\\k<-1>) resolves to the surrounding group (still being parsed)" in {
      // groupCounter = 1 inside the body of (...) — so -1 points back
      // to that very group. Useful for self-referential patterns.
      parsed("(a\\k<-1>)") shouldBe Group(
        GroupKind.Capturing(1, None),
        Concat(List(Literal("a"), Backref(GroupRef.ByNumber(1)))),
      )
    }
  }

  "errors" - {

    "\\k without <" in {
      failParse("\\ka").message should include("expected '<'")
    }

    "\\k<> empty" in {
      failParse("\\k<>").message should include("empty")
    }

    "\\k<-0> rejected" in {
      failParse("\\k<-0>").message should include("offset 0")
    }

    "\\k<-1> with no surrounding groups is rejected at parse time" in {
      // groupCounter = 0 → -1 resolves to group 0, which doesn't exist.
      // The parser catches this directly rather than letting the
      // compiler wave it through as "undefined group".
      failParse("\\k<-1>").message should include("before group 1")
    }

    "\\k<-3> when only two groups have opened is rejected" in {
      failParse("(a)(b)\\k<-3>").message should include("before group 1")
    }

    "\\k<x unterminated" in {
      failParse("\\k<x").message should include("unterminated")
    }
  }

end ParseBackrefSpec
