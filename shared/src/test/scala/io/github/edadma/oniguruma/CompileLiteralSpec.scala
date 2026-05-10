package io.github.edadma.oniguruma

import Inst.*

/** Lowering of literal text and the surrounding `Save 0 / Save 1 /
  * Match` envelope. Confirms one [[Char]] per codepoint, including
  * supplementary chars that occupy two UTF-16 units. */
class CompileLiteralSpec extends CompilerHelpers:

  "envelope" - {

    "empty pattern compiles to Save0/Save1/Match" in {
      compile("").code shouldBe Vector(Save(0), Save(1), Match)
    }

    "single literal char" in {
      compile("a").code shouldBe Vector(Save(0), Char('a'.toInt), Save(1), Match)
    }

    "three-char literal flattens to three Char insts" in {
      compile("abc").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        Char('b'.toInt),
        Char('c'.toInt),
        Save(1),
        Match,
      )
    }
  }

  "supplementary codepoints" - {

    "U+1F600 (emoji) emits one Char with the full codepoint" in {
      // The parser keeps the codepoint together; the compiler emits
      // one Char inst, even though String stores it as two UTF-16
      // surrogates.
      val cp = 0x1F600
      val src = new String(Character.toChars(cp))
      compile(src).code shouldBe Vector(Save(0), Char(cp), Save(1), Match)
    }
  }

  "control chars and escape sequences round-trip" - {

    "newline escape" in {
      compile("\\n").code shouldBe Vector(Save(0), Char('\n'.toInt), Save(1), Match)
    }

    "tab escape" in {
      compile("\\t").code shouldBe Vector(Save(0), Char('\t'.toInt), Save(1), Match)
    }

    "hex escape \\x41" in {
      compile("\\x41").code shouldBe Vector(Save(0), Char(0x41), Save(1), Match)
    }

    "hex escape \\x{1F600}" in {
      compile("\\x{1F600}").code shouldBe Vector(Save(0), Char(0x1F600), Save(1), Match)
    }
  }

  "metadata" - {

    "no groups → captureCount 0" in {
      compile("abc").captureCount shouldBe 0
    }

    "no named groups → empty namedGroups" in {
      compile("abc").namedGroups shouldBe empty
    }
  }

end CompileLiteralSpec
