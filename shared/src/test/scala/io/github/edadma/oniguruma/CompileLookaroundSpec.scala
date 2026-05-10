package io.github.edadma.oniguruma

import Inst.*

/** Stage 4 lookaround lowering. Each [[Node.Lookaround]] becomes a
  * [[LookaroundEnter]] / [[LookaroundExit]] envelope. The enter
  * carries the polarity (forward/backward, positive/negative) and
  * the absolute address of its matching exit; the body's bytecode
  * sits between them. Captures inside the body would still be
  * encoded by `Save` instructions, but the VM discards them — the
  * lookaround is zero-width on the parent. */
class CompileLookaroundSpec extends CompilerHelpers:

  "forward lookahead" - {

    "(?=abc) — positive" in {
      // 0: Save 0
      // 1: LookaroundEnter(forward=true, neg=false, exitPc=5)
      // 2: Char 'a'
      // 3: Char 'b'
      // 4: Char 'c'
      // 5: LookaroundExit
      // 6: Save 1
      // 7: Match
      compile("(?=abc)").code shouldBe Vector(
        Save(0),
        LookaroundEnter(forward = true, negative = false, exitPc = 5),
        Char('a'.toInt),
        Char('b'.toInt),
        Char('c'.toInt),
        LookaroundExit,
        Save(1),
        Match,
      )
    }

    "(?!abc) — negative" in {
      compile("(?!abc)").code shouldBe Vector(
        Save(0),
        LookaroundEnter(forward = true, negative = true, exitPc = 5),
        Char('a'.toInt),
        Char('b'.toInt),
        Char('c'.toInt),
        LookaroundExit,
        Save(1),
        Match,
      )
    }
  }

  "backward lookbehind" - {

    "(?<=abc) — positive" in {
      compile("(?<=abc)").code shouldBe Vector(
        Save(0),
        LookaroundEnter(forward = false, negative = false, exitPc = 5),
        Char('a'.toInt),
        Char('b'.toInt),
        Char('c'.toInt),
        LookaroundExit,
        Save(1),
        Match,
      )
    }

    "(?<!abc) — negative" in {
      compile("(?<!abc)").code shouldBe Vector(
        Save(0),
        LookaroundEnter(forward = false, negative = true, exitPc = 5),
        Char('a'.toInt),
        Char('b'.toInt),
        Char('c'.toInt),
        LookaroundExit,
        Save(1),
        Match,
      )
    }
  }

  "lookaround + surrounding pattern" - {

    "a(?=b)c — lookahead between two literals" in {
      // 0: Save 0
      // 1: Char 'a'
      // 2: LookaroundEnter(true, false, 4)
      // 3: Char 'b'
      // 4: LookaroundExit
      // 5: Char 'c'
      // 6: Save 1
      // 7: Match
      compile("a(?=b)c").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        LookaroundEnter(forward = true, negative = false, exitPc = 4),
        Char('b'.toInt),
        LookaroundExit,
        Char('c'.toInt),
        Save(1),
        Match,
      )
    }
  }

  "nested lookaround" - {

    "(?=a(?=b)) — lookahead inside lookahead" in {
      // The outer enter's exitPc must point at the OUTER LookaroundExit,
      // skipping past the inner one.
      // 0: Save 0
      // 1: LookaroundEnter(true, false, exitPc=6)   -- outer
      // 2: Char 'a'
      // 3: LookaroundEnter(true, false, exitPc=5)   -- inner
      // 4: Char 'b'
      // 5: LookaroundExit                            -- inner
      // 6: LookaroundExit                            -- outer
      // 7: Save 1
      // 8: Match
      compile("(?=a(?=b))").code shouldBe Vector(
        Save(0),
        LookaroundEnter(forward = true, negative = false, exitPc = 6),
        Char('a'.toInt),
        LookaroundEnter(forward = true, negative = false, exitPc = 5),
        Char('b'.toInt),
        LookaroundExit,
        LookaroundExit,
        Save(1),
        Match,
      )
    }
  }

end CompileLookaroundSpec
