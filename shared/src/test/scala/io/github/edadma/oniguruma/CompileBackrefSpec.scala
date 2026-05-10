package io.github.edadma.oniguruma

import Inst.*

/** Stage 4 backreference lowering. `\1` / `\k<name>` resolve to a
  * [[Backref]] instruction whose `slot` is the START slot of the
  * referenced capture (`2 * idx`). The `ignoreCase` flag is folded in
  * at emit time from the active flags. Branch-reset names (one name
  * → many capture indices) lower to an alternation of single-slot
  * backrefs so the VM can pick whichever group actually matched. */
class CompileBackrefSpec extends CompilerHelpers:

  "numeric backreferences" - {

    "(a)\\1 emits Backref to slot 2" in {
      compile("(a)\\1").code shouldBe Vector(
        Save(0),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Backref(2, ignoreCase = false),
        Save(1),
        Match,
      )
    }

    "(a)(b)\\2 references the second group" in {
      compile("(a)(b)\\2").code shouldBe Vector(
        Save(0),
        Save(2), Char('a'.toInt), Save(3),
        Save(4), Char('b'.toInt), Save(5),
        Backref(4, ignoreCase = false),  // slot 4 = 2 * 2 (group 2 start)
        Save(1),
        Match,
      )
    }

    "(?i)(a)\\1 carries the ignoreCase flag" in {
      compile("(?i)(a)\\1").code shouldBe Vector(
        Save(0),
        Save(2),
        CharIgnoreCase('a'.toInt),
        Save(3),
        Backref(2, ignoreCase = true),
        Save(1),
        Match,
      )
    }
  }

  "named backreferences" - {

    "\\k<name> resolves through namedGroups" in {
      val p = compile("(?<x>a)\\k<x>")
      p.namedGroups("x") shouldBe List(1)
      p.code shouldBe Vector(
        Save(0),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Backref(2, ignoreCase = false),
        Save(1),
        Match,
      )
    }
  }

  "errors" - {

    "undefined numeric ref is rejected" in {
      compileFail("\\1") should include("undefined group")
    }

    "backref past the declared count is rejected" in {
      compileFail("(a)\\2") should include("undefined group")
    }

    "undefined named ref is rejected" in {
      compileFail("\\k<missing>") should include("undefined named group")
    }

    "relative ref lowers like the resolved absolute ref" in {
      // Stage 6.C — the parser resolves `\k<-1>` to `\k<1>` here, so
      // the lowering matches `(a)\1` exactly.
      compile("(a)\\k<-1>").code shouldBe Vector(
        Save(0),
        Save(2),
        Char('a'.toInt),
        Save(3),
        Backref(2, ignoreCase = false),
        Save(1),
        Match,
      )
    }

    "forward relative ref \\k<+1>(a) lowers to a backref to group 1" in {
      // The parser resolves `\k<+1>` to `\k<1>`. The compiler doesn't
      // care that the reference precedes the group definition — slot
      // 2 is fine, it's just empty when the reference fires.
      compile("\\k<+1>(a)").code shouldBe Vector(
        Save(0),
        Backref(2, ignoreCase = false),
        Save(2), Char('a'.toInt), Save(3),
        Save(1),
        Match,
      )
    }
  }

end CompileBackrefSpec
