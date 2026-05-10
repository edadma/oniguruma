package io.github.edadma.oniguruma

import Node.*

/** Plain literals, escape sequences, and the empty-input case. The parser
  * yields one [[Literal]] per maximal run of unquantified literal chars;
  * adjacent escape-produced literals fold in too, so `\\n\\t` collapses
  * to a single two-char [[Literal]]. */
class ParseLiteralSpec extends ParserHelpers:

  "literals" - {

    "empty input parses as Empty" in {
      parsed("") shouldBe Empty
    }

    "single ASCII char" in {
      parsed("a") shouldBe Literal("a")
    }

    "multi-char ASCII run folds into one literal" in {
      parsed("hello") shouldBe Literal("hello")
    }

    "literal with mixed ASCII chars" in {
      parsed("ab12") shouldBe Literal("ab12")
    }

    "literal containing whitespace" in {
      parsed("a b") shouldBe Literal("a b")
    }

    "supplementary codepoint stays one literal" in {
      // U+1F600 (😀) is encoded as a UTF-16 surrogate pair but is one codepoint.
      val emoji = new String(Character.toChars(0x1F600))
      parsed(emoji) shouldBe Literal(emoji)
    }

    "BMP and supplementary codepoints fold together" in {
      val emoji = new String(Character.toChars(0x1F600))
      parsed(s"a${emoji}b") shouldBe Literal(s"a${emoji}b")
    }
  }

  "escape sequences" - {

    "\\n is newline" in {
      parsed("\\n") shouldBe Literal("\n")
    }

    "\\t is tab" in {
      parsed("\\t") shouldBe Literal("\t")
    }

    "\\r is carriage return" in {
      parsed("\\r") shouldBe Literal("\r")
    }

    "\\f is form feed" in {
      parsed("\\f") shouldBe Literal("\f")
    }

    "\\a is BEL" in {
      parsed("\\a") shouldBe Literal(0x07.toChar.toString)
    }

    "\\e is ESC" in {
      parsed("\\e") shouldBe Literal(0x1B.toChar.toString)
    }

    "\\xNN is one-byte hex" in {
      parsed("\\x41") shouldBe Literal("A")
    }

    "\\xNN with lower-case hex digits" in {
      parsed("\\x6a") shouldBe Literal("j")
    }

    "\\x{...} for supplementary codepoint" in {
      parsed("\\x{1F600}") shouldBe Literal(new String(Character.toChars(0x1F600)))
    }

    "\\x{...} accepts arbitrary length" in {
      parsed("\\x{0041}") shouldBe Literal("A")
    }
  }

  "escaped specials" - {

    "\\\\ is literal backslash" in {
      parsed("\\\\") shouldBe Literal("\\")
    }

    "\\. is literal dot" in {
      parsed("\\.") shouldBe Literal(".")
    }

    "\\* is literal asterisk" in {
      parsed("\\*") shouldBe Literal("*")
    }

    "\\+ is literal plus" in {
      parsed("\\+") shouldBe Literal("+")
    }

    "\\? is literal question mark" in {
      parsed("\\?") shouldBe Literal("?")
    }

    "\\( and \\) are literal parens" in {
      parsed("\\(") shouldBe Literal("(")
      parsed("\\)") shouldBe Literal(")")
    }

    "\\[ and \\] are literal brackets" in {
      parsed("\\[") shouldBe Literal("[")
      parsed("\\]") shouldBe Literal("]")
    }

    "\\{ and \\} are literal braces" in {
      parsed("\\{") shouldBe Literal("{")
      parsed("\\}") shouldBe Literal("}")
    }

    "\\| is literal pipe" in {
      parsed("\\|") shouldBe Literal("|")
    }

    "\\^ is literal caret" in {
      parsed("\\^") shouldBe Literal("^")
    }

    "\\$ is literal dollar" in {
      parsed("\\$") shouldBe Literal("$")
    }
  }

  "non-meta escapes are literal letter" in {
    // The default flavor treats `\X`, `\Y`, `\R` etc. as the literal letter
    // (TextMate grammars rely on this, and the audit found no actual uses
    // of `\X`/`\Y`/`\R` as Onig metas).
    parsed("\\Q") shouldBe Literal("Q")
    parsed("\\R") shouldBe Literal("R")
    parsed("\\X") shouldBe Literal("X")
    parsed("\\Y") shouldBe Literal("Y")
  }

  "fold behaviour" - {

    "escape + plain char fold into one literal" in {
      parsed("\\nfoo") shouldBe Literal("\nfoo")
    }

    "plain + escape fold into one literal" in {
      parsed("foo\\n") shouldBe Literal("foo\n")
    }

    "two adjacent escapes fold" in {
      parsed("\\n\\t") shouldBe Literal("\n\t")
    }

    "non-literals (Dot etc.) break the fold run" in {
      parsed("a.b") shouldBe Concat(List(Literal("a"), Dot, Literal("b")))
    }
  }

  "trailing backslash fails" in {
    val e = failParse("\\")
    e.message should include("trailing backslash")
  }

  "bad \\x hex fails" in {
    failParse("\\xZZ").message should include("bad hex")
  }

  "unterminated \\x{...} fails" in {
    failParse("\\x{12").message should include("unterminated")
  }

end ParseLiteralSpec
