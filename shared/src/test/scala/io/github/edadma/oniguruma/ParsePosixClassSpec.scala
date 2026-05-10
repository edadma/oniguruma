package io.github.edadma.oniguruma

import Node.*

/** POSIX bracket expressions inside char classes. The seven classes
  * actually used by TextMate grammars must round-trip to the right
  * [[IntervalSet]]; the remaining classes (punct/cntrl/print/graph/
  * blank/ascii) are also exercised because the cost is one factory
  * call each. */
class ParsePosixClassSpec extends ParserHelpers:

  "[[:alpha:]]" in {
    parsed("[[:alpha:]]") shouldBe Klass(
      IntervalSet.of(('A'.toInt, 'Z'.toInt), ('a'.toInt, 'z'.toInt)),
      false,
    )
  }

  "[[:alnum:]]" in {
    parsed("[[:alnum:]]") shouldBe Klass(
      IntervalSet.of(
        ('0'.toInt, '9'.toInt),
        ('A'.toInt, 'Z'.toInt),
        ('a'.toInt, 'z'.toInt),
      ),
      false,
    )
  }

  "[[:digit:]]" in {
    parsed("[[:digit:]]") shouldBe Klass(IntervalSet.range('0', '9'), false)
  }

  "[[:upper:]]" in {
    parsed("[[:upper:]]") shouldBe Klass(IntervalSet.range('A', 'Z'), false)
  }

  "[[:lower:]]" in {
    parsed("[[:lower:]]") shouldBe Klass(IntervalSet.range('a', 'z'), false)
  }

  "[[:xdigit:]]" in {
    parsed("[[:xdigit:]]") shouldBe Klass(
      IntervalSet.of(
        ('0'.toInt, '9'.toInt),
        ('A'.toInt, 'F'.toInt),
        ('a'.toInt, 'f'.toInt),
      ),
      false,
    )
  }

  "[[:word:]]" in {
    parsed("[[:word:]]") shouldBe Klass(
      IntervalSet.of(
        ('0'.toInt, '9'.toInt),
        ('A'.toInt, 'Z'.toInt),
        ('_'.toInt, '_'.toInt),
        ('a'.toInt, 'z'.toInt),
      ),
      false,
    )
  }

  "[[:space:]]" in {
    parsed("[[:space:]]") shouldBe Klass(
      IntervalSet.fromCodepoints(Seq(0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x20)),
      false,
    )
  }

  "[[:punct:]]" in {
    parsed("[[:punct:]]") shouldBe Klass(
      IntervalSet.of((0x21, 0x2F), (0x3A, 0x40), (0x5B, 0x60), (0x7B, 0x7E)),
      false,
    )
  }

  "[[:cntrl:]]" in {
    parsed("[[:cntrl:]]") shouldBe Klass(IntervalSet.of((0x00, 0x1F), (0x7F, 0x7F)), false)
  }

  "[[:print:]]" in {
    parsed("[[:print:]]") shouldBe Klass(IntervalSet.range(0x20, 0x7E), false)
  }

  "[[:graph:]]" in {
    parsed("[[:graph:]]") shouldBe Klass(IntervalSet.range(0x21, 0x7E), false)
  }

  "[[:blank:]]" in {
    parsed("[[:blank:]]") shouldBe Klass(IntervalSet.of((0x09, 0x09), (0x20, 0x20)), false)
  }

  "[[:ascii:]]" in {
    parsed("[[:ascii:]]") shouldBe Klass(IntervalSet.range(0x00, 0x7F), false)
  }

  "negation" - {

    "[[:^digit:]]" in {
      parsed("[[:^digit:]]") shouldBe Klass(IntervalSet.range('0', '9').complement, false)
    }

    "outer + inner negation" in {
      // [^[:^digit:]] = ¬(¬digit) = digit
      parsed("[^[:^digit:]]") shouldBe Klass(IntervalSet.range('0', '9').complement, true)
    }
  }

  "combinations" - {

    "POSIX plus literal" in {
      // [[:alpha:]_] = letters ∪ underscore
      val letters = IntervalSet.of(('A'.toInt, 'Z'.toInt), ('a'.toInt, 'z'.toInt))
      parsed("[[:alpha:]_]") shouldBe Klass(letters.union(IntervalSet.single('_')), false)
    }

    "two POSIX in one class" in {
      val expected = IntervalSet
        .of(('A'.toInt, 'Z'.toInt), ('a'.toInt, 'z'.toInt))
        .union(IntervalSet.range('0', '9'))
      parsed("[[:alpha:][:digit:]]") shouldBe Klass(expected, false)
    }
  }

  "errors" - {

    "unknown POSIX name" in {
      failParse("[[:foo:]]").message should include("unknown POSIX")
    }

    "unterminated POSIX class" in {
      failParse("[[:alpha").message should include("unterminated")
    }
  }

end ParsePosixClassSpec
