package io.github.edadma.oniguruma

import Inst.*

/** Group-kind lowering: capturing groups bracket their body with
  * `Save(2*idx)` / `Save(2*idx+1)`; non-capturing groups are
  * structurally invisible (just inline the body); atomic groups
  * bracket with `AtomicMark` / `AtomicCommit`. The capture-count
  * metadata reflects the highest index assigned. */
class CompileGroupSpec extends CompilerHelpers:

  "capturing groups" - {

    "(a) gets Save 2 / Save 3 around body" in {
      compile("(a)").code shouldBe Vector(
        Save(0),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Save(1),
        Match,
      )
    }

    "(a)(b) — sibling groups get sequential save slots" in {
      compile("(a)(b)").code shouldBe Vector(
        Save(0),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Save(4),
        Char('b'.toInt),
        Save(5),
        Save(1),
        Match,
      )
    }

    "(a(b)) — nested groups, outermost-first numbering" in {
      compile("(a(b))").code shouldBe Vector(
        Save(0),
        Save(2),
        Char('a'.toInt),
        Save(4),
        Char('b'.toInt),
        Save(5),
        Save(3),
        Save(1),
        Match,
      )
    }

    "captureCount reflects highest index" in {
      compile("(a)(b)(c)").captureCount shouldBe 3
      compile("a").captureCount shouldBe 0
      compile("(a(b)c)").captureCount shouldBe 2
    }
  }

  "named groups" - {

    "(?<x>a) — name maps to its capture index" in {
      val p = compile("(?<x>a)")
      p.namedGroups shouldBe Map("x" -> List(1))
      p.captureCount shouldBe 1
    }

    "branch-reset names — same name, multiple indices" in {
      val p = compile("(?<x>a)|(?<x>b)")
      p.namedGroups shouldBe Map("x" -> List(1, 2))
      p.captureCount shouldBe 2
    }
  }

  "non-capturing groups" - {

    "(?:abc) inlines body, no Save instructions" in {
      compile("(?:abc)").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        Char('b'.toInt),
        Char('c'.toInt),
        Save(1),
        Match,
      )
    }

    "(?:a)(b) — non-capturing doesn't take an index" in {
      // The (b) is group 1, not group 2.
      compile("(?:a)(b)").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        Save(2),
        Char('b'.toInt),
        Save(3),
        Save(1),
        Match,
      )
    }
  }

  "atomic groups" - {

    "(?>a) wraps body in AtomicMark / AtomicCommit" in {
      compile("(?>a)").code shouldBe Vector(
        Save(0),
        AtomicMark,
        Char('a'.toInt),
        AtomicCommit,
        Save(1),
        Match,
      )
    }

    "(?>a)(b) — atomic doesn't take an index either" in {
      compile("(?>a)(b)").code shouldBe Vector(
        Save(0),
        AtomicMark,
        Char('a'.toInt),
        AtomicCommit,
        Save(2),
        Char('b'.toInt),
        Save(3),
        Save(1),
        Match,
      )
    }
  }

end CompileGroupSpec
