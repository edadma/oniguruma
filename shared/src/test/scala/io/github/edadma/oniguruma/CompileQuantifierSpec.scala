package io.github.edadma.oniguruma

import Inst.*

/** Quantifier lowering across the three modes (greedy / reluctant /
  * possessive) and the four shapes (`?`, `*`, `+`, `{n,m}`). Greedy
  * Splits prefer the body; reluctant Splits prefer the skip;
  * possessive wraps a greedy expansion in `AtomicMark` /
  * `AtomicCommit` so its choice points get dropped on commit. */
class CompileQuantifierSpec extends CompilerHelpers:

  "? quantifier" - {

    "a? greedy" in {
      // 0: Save 0
      // 1: Split 2, 3    -- prefer body
      // 2: Char 'a'
      // 3: Save 1
      // 4: Match
      compile("a?").code shouldBe Vector(
        Save(0),
        Split(2, 3),
        Char('a'.toInt),
        Save(1),
        Match,
      )
    }

    "a?? reluctant — split arms swapped" in {
      compile("a??").code shouldBe Vector(
        Save(0),
        Split(3, 2),
        Char('a'.toInt),
        Save(1),
        Match,
      )
    }

    "a?+ possessive — wrapped in atomic" in {
      compile("a?+").code shouldBe Vector(
        Save(0),
        AtomicMark,
        Split(3, 4),
        Char('a'.toInt),
        AtomicCommit,
        Save(1),
        Match,
      )
    }
  }

  "* quantifier (unbounded)" - {

    "a* greedy" in {
      // 0: Save 0
      // 1: Split 2, 4    -- prefer body
      // 2: Char 'a'
      // 3: Jmp 1         -- loop back
      // 4: Save 1
      // 5: Match
      compile("a*").code shouldBe Vector(
        Save(0),
        Split(2, 4),
        Char('a'.toInt),
        Jmp(1),
        Save(1),
        Match,
      )
    }

    "a*? reluctant" in {
      compile("a*?").code shouldBe Vector(
        Save(0),
        Split(4, 2),
        Char('a'.toInt),
        Jmp(1),
        Save(1),
        Match,
      )
    }

    "a*+ possessive" in {
      compile("a*+").code shouldBe Vector(
        Save(0),
        AtomicMark,
        Split(3, 5),
        Char('a'.toInt),
        Jmp(2),
        AtomicCommit,
        Save(1),
        Match,
      )
    }
  }

  "+ quantifier" - {

    "a+ greedy = one mandatory then *" in {
      // 0: Save 0
      // 1: Char 'a'   -- the mandatory one
      // 2: Split 3, 5
      // 3: Char 'a'
      // 4: Jmp 2
      // 5: Save 1
      // 6: Match
      compile("a+").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        Split(3, 5),
        Char('a'.toInt),
        Jmp(2),
        Save(1),
        Match,
      )
    }

    "a+? reluctant" in {
      compile("a+?").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        Split(5, 3),
        Char('a'.toInt),
        Jmp(2),
        Save(1),
        Match,
      )
    }
  }

  "brace quantifiers {n,m}" - {

    "a{2}" in {
      compile("a{2}").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        Char('a'.toInt),
        Save(1),
        Match,
      )
    }

    "a{1,3} — one mandatory, two optional" in {
      // 0: Save 0
      // 1: Char 'a'      -- mandatory #1
      // 2: Split 3, 4    -- optional #1
      // 3: Char 'a'
      // 4: Split 5, 6    -- optional #2
      // 5: Char 'a'
      // 6: Save 1
      // 7: Match
      compile("a{1,3}").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        Split(3, 4),
        Char('a'.toInt),
        Split(5, 6),
        Char('a'.toInt),
        Save(1),
        Match,
      )
    }

    "a{2,} — two mandatory then *" in {
      // 0: Save 0
      // 1: Char 'a'   -- mandatory #1
      // 2: Char 'a'   -- mandatory #2
      // 3: Split 4, 6
      // 4: Char 'a'
      // 5: Jmp 3
      // 6: Save 1
      // 7: Match
      compile("a{2,}").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        Char('a'.toInt),
        Split(4, 6),
        Char('a'.toInt),
        Jmp(3),
        Save(1),
        Match,
      )
    }

    "a{0,2} — fully optional, two slots" in {
      compile("a{0,2}").code shouldBe Vector(
        Save(0),
        Split(2, 3),
        Char('a'.toInt),
        Split(4, 5),
        Char('a'.toInt),
        Save(1),
        Match,
      )
    }

    "a{0,2}? reluctant — split arms swapped on each optional slot" in {
      compile("a{0,2}?").code shouldBe Vector(
        Save(0),
        Split(3, 2),
        Char('a'.toInt),
        Split(5, 4),
        Char('a'.toInt),
        Save(1),
        Match,
      )
    }
  }

  "quantified group" - {

    "(a)+ greedy" in {
      // 0: Save 0
      // 1: Save 2     -- mandatory copy of (a)
      // 2: Char 'a'
      // 3: Save 3
      // 4: Split 5, 9
      // 5: Save 2
      // 6: Char 'a'
      // 7: Save 3
      // 8: Jmp 4
      // 9: Save 1
      // 10: Match
      compile("(a)+").code shouldBe Vector(
        Save(0),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Split(5, 9),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Jmp(4),
        Save(1),
        Match,
      )
    }
  }

end CompileQuantifierSpec
