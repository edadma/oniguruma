package io.github.edadma.oniguruma

import Inst.*

/** Stage 4 flag-application lowering. The compiler folds `(?i)` and
  * `(?m)` into the bytecode at emit time:
  *
  *  - `(?i)` flips ASCII letter literals from [[Char]] to
  *    [[CharIgnoreCase]] and folds char-class sets via
  *    [[IntervalSet.caseFoldAscii]];
  *  - `(?m)` (Onig flavor) is DotAll — rewrites `.` from [[AnyChar]]
  *    to [[AnyCharIncludingNewline]]. Onig uses `m` where PCRE uses
  *    `s`; line-mode `^`/`$` are always-on in this flavor and need
  *    no flag.
  *
  * The bare-leak form `(?i)abc` and the scoped form `(?i:abc)` both
  * end up the same here at the bytecode level — the difference (where
  * the flag stops applying) is a parse-tree shape concern. */
class CompileFlagsSpec extends CompilerHelpers:

  "(?i) — case-insensitive literals" - {

    "(?i)a folds the ASCII letter to CharIgnoreCase" in {
      compile("(?i)a").code shouldBe Vector(
        Save(0),
        CharIgnoreCase('a'.toInt),
        Save(1),
        Match,
      )
    }

    "(?i)1 leaves the digit as a plain Char" in {
      // Non-letters aren't case-folded, so the instruction is the same
      // as without the flag — only the AST FlagsScope is different.
      compile("(?i)1").code shouldBe Vector(
        Save(0),
        Char('1'.toInt),
        Save(1),
        Match,
      )
    }

    "(?i:a) scoped — same bytecode for the body" in {
      compile("(?i:a)").code shouldBe Vector(
        Save(0),
        CharIgnoreCase('a'.toInt),
        Save(1),
        Match,
      )
    }

    "leaked (?i) only affects the rest of the surrounding scope" in {
      // `(?i:a)b` — flag scoped to the group; `b` is back to plain Char.
      compile("(?i:a)b").code shouldBe Vector(
        Save(0),
        CharIgnoreCase('a'.toInt),
        Char('b'.toInt),
        Save(1),
        Match,
      )
    }

    "leaked bare (?i) propagates across siblings in a Concat" in {
      // `(?i)ab` — the bare leak applies to both letters.
      compile("(?i)ab").code shouldBe Vector(
        Save(0),
        CharIgnoreCase('a'.toInt),
        CharIgnoreCase('b'.toInt),
        Save(1),
        Match,
      )
    }
  }

  "(?i) — case-insensitive char classes" - {

    "[a-z] under (?i) folds to A-Z + a-z" in {
      val folded = IntervalSet.range('a'.toInt, 'z'.toInt).caseFoldAscii
      compile("(?i)[a-z]").code shouldBe Vector(
        Save(0),
        CharClass(folded, negated = false),
        Save(1),
        Match,
      )
    }

    "negated class still folds the underlying set" in {
      val folded = IntervalSet.range('a'.toInt, 'z'.toInt).caseFoldAscii
      compile("(?i)[^a-z]").code shouldBe Vector(
        Save(0),
        CharClass(folded, negated = true),
        Save(1),
        Match,
      )
    }
  }

  "(?m) — DotAll on the dot (Onig flavor)" - {

    "(?m). becomes AnyCharIncludingNewline" in {
      compile("(?m).").code shouldBe Vector(
        Save(0),
        AnyCharIncludingNewline,
        Save(1),
        Match,
      )
    }

    "without the flag . stays AnyChar" in {
      compile(".").code shouldBe Vector(
        Save(0),
        AnyChar,
        Save(1),
        Match,
      )
    }

    "(?m:a.b) — only the body sees DotAll" in {
      compile("(?m:a.b)").code shouldBe Vector(
        Save(0),
        Char('a'.toInt),
        AnyCharIncludingNewline,
        Char('b'.toInt),
        Save(1),
        Match,
      )
    }
  }

end CompileFlagsSpec
