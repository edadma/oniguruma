package io.github.edadma.oniguruma

import Inst.*

/** Char-class lowering. The parser collapses every [...] body into an
  * [[IntervalSet]]; the compiler emits a single [[CharClass]] inst
  * carrying the set and the negation flag. `.` becomes [[AnyChar]];
  * shortcut classes outside a [...] still go through [[CharClass]]
  * because the parser wraps them in a `Klass` node. */
class CompileClassSpec extends CompilerHelpers:

  "char-class compilation" - {

    "[abc] becomes CharClass with non-negated set" in {
      compile("[abc]").code shouldBe Vector(
        Save(0),
        CharClass(IntervalSet.fromChars("abc"), negated = false),
        Save(1),
        Match,
      )
    }

    "[^abc] becomes CharClass with negated=true" in {
      compile("[^abc]").code shouldBe Vector(
        Save(0),
        CharClass(IntervalSet.fromChars("abc"), negated = true),
        Save(1),
        Match,
      )
    }

    "[a-z]" in {
      compile("[a-z]").code shouldBe Vector(
        Save(0),
        CharClass(IntervalSet.range('a', 'z'), negated = false),
        Save(1),
        Match,
      )
    }
  }

  "shortcuts outside a class" - {

    "\\d" in {
      compile("\\d").code shouldBe Vector(
        Save(0),
        CharClass(IntervalSet.range('0', '9'), negated = false),
        Save(1),
        Match,
      )
    }

    "\\D — emitted with the complement set, not the negation flag" in {
      // The parser pre-complements the set when it sees \D, so the
      // emitted CharClass has negated=false.
      val set = IntervalSet.range('0', '9').complement
      compile("\\D").code shouldBe Vector(
        Save(0),
        CharClass(set, negated = false),
        Save(1),
        Match,
      )
    }
  }

  "the dot" - {

    "dot becomes AnyChar" in {
      compile(".").code shouldBe Vector(Save(0), AnyChar, Save(1), Match)
    }

    "a.b" in {
      compile("a.b").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        AnyChar,
        Char('b'.toInt),
        Save(1),
        Match,
      )
    }
  }

  "POSIX classes" - {

    "[:digit:]" in {
      compile("[[:digit:]]").code shouldBe Vector(
        Save(0),
        CharClass(IntervalSet.range('0', '9'), negated = false),
        Save(1),
        Match,
      )
    }
  }

end CompileClassSpec
