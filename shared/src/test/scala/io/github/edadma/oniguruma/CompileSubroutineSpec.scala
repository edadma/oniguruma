package io.github.edadma.oniguruma

import Inst.*

/** Stage 5 subroutine-call lowering. `\g<n>` / `\g<name>` / `\g<0>`
  * each lower to a single [[Call]] instruction whose `entryPc` is the
  * pc just past the target group's opening `Save` and whose `leaveAt`
  * is the pc just past its closing `Save`. For `\g<0>` (whole-pattern
  * recursion) the entry is 0 and the leaveAt is the pc of the program's
  * terminal `Match`.
  *
  * The compiler patches Call placeholders at the very end of
  * compilation so a forward reference (`\g<2>(a)(b)`) resolves to the
  * group's eventually-known entry/leave pair. These tests pin both
  * forward and backward reference shapes. */
class CompileSubroutineSpec extends CompilerHelpers:

  "by-number subroutine call" - {

    "(a)\\g<1> — call after the group is defined" in {
      // Layout:
      //   0  Save(0)
      //   1  Save(2)        — group 1 open
      //   2  Char('a')
      //   3  Save(3)        — group 1 close
      //   4  Call(2, 4)     — \g<1>: entry=2, leave=4
      //   5  Save(1)
      //   6  Match
      compile("(a)\\g<1>").code shouldBe Vector(
        Save(0),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Call(2, 4),
        Save(1),
        Match,
      )
    }

    "\\g<1>(a) — forward reference patches once group's body is known" in {
      // Layout:
      //   0  Save(0)
      //   1  Call(?, ?)     — patched after compilation completes
      //   2  Save(2)        — group 1 open
      //   3  Char('a')
      //   4  Save(3)        — group 1 close
      //   5  Save(1)
      //   6  Match
      // Group 1's body entry is pc 3, leaveAt is pc 5.
      compile("\\g<1>(a)").code shouldBe Vector(
        Save(0),
        Call(3, 5),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Save(1),
        Match,
      )
    }
  }

  "by-name subroutine call" - {

    "(?<x>a)\\g<x>" in {
      // Same shape as numeric — the resolution happens in the patch.
      compile("(?<x>a)\\g<x>").code shouldBe Vector(
        Save(0),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Call(2, 4),
        Save(1),
        Match,
      )
    }
  }

  "\\g<0> whole-pattern recursion" - {

    "lowers to Call(0, matchPc)" in {
      // Layout:
      //   0  Save(0)
      //   1  Char('a')
      //   2  Call(0, 4)     — \g<0>: entry=0 (top), leave=4 (Match's pc)
      //   3  Save(1)
      //   4  Match
      compile("a\\g<0>").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        Call(0, 4),
        Save(1),
        Match,
      )
    }
  }

  "compiler error cases" - {

    "subroutine call to a number out of range" in {
      compileFail("(a)\\g<7>") should include("undefined")
    }

    "subroutine call to an undefined name" in {
      compileFail("\\g<missing>") should include("undefined")
    }

    "branch-reset name is ambiguous for subroutine call" in {
      // Two groups share the name 'x'; \g<x> can't pick one.
      compileFail("(?<x>a)(?<x>b)\\g<x>") should include("ambiguous")
    }

    "relative subroutine call lowers like the resolved absolute call" in {
      // Stage 6.C — `(a)\g<-1>` resolves to `(a)\g<1>` at parse time,
      // so the bytecode matches the explicit by-number form exactly.
      compile("(a)\\g<-1>").code shouldBe Vector(
        Save(0),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Call(2, 4),
        Save(1),
        Match,
      )
    }

    "forward relative subroutine call \\g<+1>(a) patches like \\g<1>(a)" in {
      compile("\\g<+1>(a)").code shouldBe Vector(
        Save(0),
        Call(3, 5),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Save(1),
        Match,
      )
    }
  }

end CompileSubroutineSpec
