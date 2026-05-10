package io.github.edadma.oniguruma

import Node.*

/** Char-class syntax: simple sets, ranges, negation, escape inside class,
  * the `]`-as-first-char rule, dash-position rules, and shorthand
  * shortcuts inside a class. POSIX `[[:alpha:]]` lives in its own spec. */
class ParseClassSpec extends ParserHelpers:

  "simple sets" - {

    "[abc]" in {
      parsed("[abc]") shouldBe Klass(IntervalSet.fromChars("abc"), false)
    }

    "[a] single-char class" in {
      parsed("[a]") shouldBe Klass(IntervalSet.single('a'), false)
    }

    "negated [^abc]" in {
      parsed("[^abc]") shouldBe Klass(IntervalSet.fromChars("abc"), true)
    }

    "negated single-char class" in {
      parsed("[^a]") shouldBe Klass(IntervalSet.single('a'), true)
    }
  }

  "ranges" - {

    "[a-z]" in {
      parsed("[a-z]") shouldBe Klass(IntervalSet.range('a', 'z'), false)
    }

    "[A-Z]" in {
      parsed("[A-Z]") shouldBe Klass(IntervalSet.range('A', 'Z'), false)
    }

    "[0-9]" in {
      parsed("[0-9]") shouldBe Klass(IntervalSet.range('0', '9'), false)
    }

    "multiple ranges merge" in {
      parsed("[a-zA-Z0-9]") shouldBe Klass(
        IntervalSet.of(('0'.toInt, '9'.toInt), ('A'.toInt, 'Z'.toInt), ('a'.toInt, 'z'.toInt)),
        false,
      )
    }

    "range plus single chars" in {
      parsed("[a-zA-Z0-9_]") shouldBe Klass(
        IntervalSet.of(
          ('0'.toInt, '9'.toInt),
          ('A'.toInt, 'Z'.toInt),
          ('_'.toInt, '_'.toInt),
          ('a'.toInt, 'z'.toInt),
        ),
        false,
      )
    }

    "range end < start fails" in {
      failParse("[z-a]").message should include("invalid range")
    }
  }

  "dash positions" - {

    "leading dash is literal" in {
      parsed("[-abc]") shouldBe Klass(IntervalSet.fromChars("-abc"), false)
    }

    "trailing dash is literal" in {
      parsed("[abc-]") shouldBe Klass(IntervalSet.fromChars("abc-"), false)
    }

    "leading dash with negation" in {
      parsed("[^-]") shouldBe Klass(IntervalSet.single('-'), true)
    }

    "range followed by literal dash" in {
      parsed("[a-z-]") shouldBe Klass(
        IntervalSet.of(('-'.toInt, '-'.toInt), ('a'.toInt, 'z'.toInt)),
        false,
      )
    }
  }

  "escapes inside class" - {

    "\\] is literal close-bracket" in {
      parsed("[\\]]") shouldBe Klass(IntervalSet.single(']'), false)
    }

    "\\\\ is literal backslash" in {
      parsed("[\\\\]") shouldBe Klass(IntervalSet.single('\\'), false)
    }

    "\\n is newline" in {
      parsed("[\\n]") shouldBe Klass(IntervalSet.single('\n'), false)
    }

    "\\t is tab" in {
      parsed("[\\t]") shouldBe Klass(IntervalSet.single('\t'), false)
    }

    "\\b is backspace inside class (not boundary)" in {
      parsed("[\\b]") shouldBe Klass(IntervalSet.single(0x08), false)
    }

    "\\xNN inside class" in {
      parsed("[\\x41-\\x5A]") shouldBe Klass(IntervalSet.range('A', 'Z'), false)
    }

    "any non-meta escape is literal letter" in {
      parsed("[\\.]") shouldBe Klass(IntervalSet.single('.'), false)
      parsed("[\\^]") shouldBe Klass(IntervalSet.single('^'), false)
    }
  }

  "shortcut classes inside char class" - {

    "\\d inside class" in {
      parsed("[\\d]") shouldBe Klass(IntervalSet.range('0', '9'), false)
    }

    "\\d combined with literal" in {
      parsed("[\\d_]") shouldBe Klass(
        IntervalSet.of(('0'.toInt, '9'.toInt), ('_'.toInt, '_'.toInt)),
        false,
      )
    }

    "\\w inside class" in {
      parsed("[\\w]") shouldBe Klass(
        IntervalSet.of(
          ('0'.toInt, '9'.toInt),
          ('A'.toInt, 'Z'.toInt),
          ('_'.toInt, '_'.toInt),
          ('a'.toInt, 'z'.toInt),
        ),
        false,
      )
    }

    "\\D negates inside class" in {
      val digits = IntervalSet.range('0', '9')
      parsed("[\\D]") shouldBe Klass(digits.complement, false)
    }

    "shortcut adjacent to dash is literal dash" in {
      // Range with shortcut endpoint isn't allowed; `-` here is a literal.
      // `[\d-z]` is `\d` ∪ `-` ∪ `z`.
      parsed("[\\d-z]") shouldBe Klass(
        IntervalSet.range('0', '9')
          .union(IntervalSet.single('-'))
          .union(IntervalSet.single('z')),
        false,
      )
    }

    "shortcut as range endpoint fails" in {
      failParse("[a-\\d]").message should include("range end")
    }
  }

  "errors" - {

    "unterminated class" in {
      failParse("[abc").message should include("unterminated")
    }

    "trailing backslash inside class" in {
      failParse("[a\\").message should include("trailing backslash")
    }
  }

  "complex unions" - {

    "ranges plus shortcuts plus literals" in {
      val expected = IntervalSet
        .range('a', 'z')
        .union(IntervalSet.range('A', 'Z'))
        .union(IntervalSet.range('0', '9'))
        .union(IntervalSet.single('_'))
      parsed("[\\w]") shouldBe Klass(expected, false)
    }
  }

  "nested classes" - {

    "[[abc]] flattens to [abc]" in {
      // Onig allows arbitrary nesting; a singly-nested class with no
      // negation is equivalent to its inner content.
      parsed("[[abc]]") shouldBe Klass(IntervalSet.fromChars("abc"), false)
    }

    "[[abc][def]] is the union" in {
      parsed("[[abc][def]]") shouldBe Klass(IntervalSet.fromChars("abcdef"), false)
    }

    "outer class can mix nested + plain items" in {
      // [[ab]c] = a ∪ b ∪ c
      parsed("[[ab]c]") shouldBe Klass(IntervalSet.fromChars("abc"), false)
    }

    "negation inside nested class" in {
      // [[^a]] — the inner class is ¬a, contributed to outer; outer not
      // negated → final set is ¬a.
      parsed("[[^a]]") shouldBe Klass(IntervalSet.single('a').complement, false)
    }

    "outer + inner negation" in {
      // [^[^a]] — outer negated, inner negated. Net = a.
      parsed("[^[^a]]") shouldBe Klass(IntervalSet.single('a').complement, true)
    }

    "real corpus shape: [[-\\w]…]" in {
      // The dash-then-shortcut shape that triggered the original
      // 'range end can't be a class shorthand' error.
      val inner = IntervalSet.single('-').union(
        IntervalSet.of(
          ('0'.toInt, '9'.toInt),
          ('A'.toInt, 'Z'.toInt),
          ('_'.toInt, '_'.toInt),
          ('a'.toInt, 'z'.toInt),
        )
      )
      parsed("[[-\\w]]") shouldBe Klass(inner, false)
    }
  }

end ParseClassSpec
