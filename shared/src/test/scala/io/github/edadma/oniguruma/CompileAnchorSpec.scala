package io.github.edadma.oniguruma

import Inst.*

/** Anchor lowering — every [[Node.Anchor]] kind maps to one
  * [[Inst.Anchor]] with the same kind. The interpretation (input vs
  * line boundaries, word boundary, `\G`) is the VM's job. */
class CompileAnchorSpec extends CompilerHelpers:

  "every anchor lowers to a one-inst Anchor" - {

    "^ caret" in {
      compile("^").code shouldBe Vector(
        Save(0), Inst.Anchor(AnchorKind.Caret), Save(1), Match,
      )
    }

    "$ dollar" in {
      compile("$").code shouldBe Vector(
        Save(0), Inst.Anchor(AnchorKind.Dollar), Save(1), Match,
      )
    }

    "\\A begin-input" in {
      compile("\\A").code shouldBe Vector(
        Save(0), Inst.Anchor(AnchorKind.BeginInput), Save(1), Match,
      )
    }

    "\\z end-input" in {
      compile("\\z").code shouldBe Vector(
        Save(0), Inst.Anchor(AnchorKind.EndInput), Save(1), Match,
      )
    }

    "\\Z end-input-before-final-newline" in {
      compile("\\Z").code shouldBe Vector(
        Save(0), Inst.Anchor(AnchorKind.EndInputBeforeFinalNewline), Save(1), Match,
      )
    }

    "\\G prev-match-end" in {
      compile("\\G").code shouldBe Vector(
        Save(0), Inst.Anchor(AnchorKind.PrevMatchEnd), Save(1), Match,
      )
    }

    "\\b word-boundary" in {
      compile("\\b").code shouldBe Vector(
        Save(0), Inst.Anchor(AnchorKind.WordBoundary), Save(1), Match,
      )
    }

    "\\B non-word-boundary" in {
      compile("\\B").code shouldBe Vector(
        Save(0), Inst.Anchor(AnchorKind.NonWordBoundary), Save(1), Match,
      )
    }
  }

  "anchor + literal combinations" - {

    "^abc" in {
      compile("^abc").code shouldBe Vector(
        Save(0),
        Inst.Anchor(AnchorKind.Caret),
        Char('a'.toInt),
        Char('b'.toInt),
        Char('c'.toInt),
        Save(1),
        Match,
      )
    }

    "abc$" in {
      compile("abc$").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        Char('b'.toInt),
        Char('c'.toInt),
        Inst.Anchor(AnchorKind.Dollar),
        Save(1),
        Match,
      )
    }
  }

end CompileAnchorSpec
