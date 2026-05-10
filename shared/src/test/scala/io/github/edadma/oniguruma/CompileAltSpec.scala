package io.github.edadma.oniguruma

import Inst.*

/** Alternation lowering. Two-arm alternation emits `Split` + body +
  * `Jmp END`, then the second body and the END label. N-arm chains
  * the same shape so the VM commits to the leftmost matching branch
  * and pushes the remaining branches as backtrack points. */
class CompileAltSpec extends CompilerHelpers:

  "two-branch alternation" - {

    "a|b" in {
      // Layout:
      //   0: Save 0
      //   1: Split 2, 4    -- prefer "a", alt jump to "b"
      //   2: Char 'a'
      //   3: Jmp 5         -- jump past "b" to END
      //   4: Char 'b'
      //   5: Save 1
      //   6: Match
      compile("a|b").code shouldBe Vector(
        Save(0),
        Split(2, 4),
        Char('a'.toInt),
        Jmp(5),
        Char('b'.toInt),
        Save(1),
        Match,
      )
    }

    "ab|cd" in {
      compile("ab|cd").code shouldBe Vector(
        Save(0),
        Split(2, 5),
        Char('a'.toInt),
        Char('b'.toInt),
        Jmp(7),
        Char('c'.toInt),
        Char('d'.toInt),
        Save(1),
        Match,
      )
    }
  }

  "three-branch alternation" - {

    "a|b|c" in {
      // Two daisy-chained Splits.
      compile("a|b|c").code shouldBe Vector(
        Save(0),
        Split(2, 4),       // a vs (b|c)
        Char('a'.toInt),
        Jmp(8),
        Split(5, 7),       // b vs c
        Char('b'.toInt),
        Jmp(8),
        Char('c'.toInt),
        Save(1),
        Match,
      )
    }
  }

  "alt with empty branches" - {

    "a| — empty branch on the right" in {
      // |seq returns Empty for an empty alt arm, which compiles to no
      // instructions — so the right arm is just the `Save 1`/`Match` epilogue.
      compile("a|").code shouldBe Vector(
        Save(0),
        Split(2, 4),
        Char('a'.toInt),
        Jmp(4),
        Save(1),
        Match,
      )
    }

    "|a — empty branch on the left" in {
      compile("|a").code shouldBe Vector(
        Save(0),
        Split(2, 3),
        Jmp(4),
        Char('a'.toInt),
        Save(1),
        Match,
      )
    }
  }

  "alt inside a group" - {

    "(a|b)" in {
      compile("(a|b)").code shouldBe Vector(
        Save(0),
        Save(2),
        Split(3, 5),
        Char('a'.toInt),
        Jmp(6),
        Char('b'.toInt),
        Save(3),
        Save(1),
        Match,
      )
    }
  }

end CompileAltSpec
