package io.github.edadma.oniguruma

import Node.*

/** `(?#comment)` syntax. Onig comments don't honour escapes — the body
  * runs verbatim until the first `)` is hit. Comments are preserved in
  * the AST so the engine can pretty-print regexes round-trip. */
class ParseCommentSpec extends ParserHelpers:

  "(?#hello)" in {
    parsed("(?#hello)") shouldBe Comment("hello")
  }

  "empty body (?#)" in {
    parsed("(?#)") shouldBe Comment("")
  }

  "body containing whitespace" in {
    parsed("(?# hello world )") shouldBe Comment(" hello world ")
  }

  "body containing escapes is literal" in {
    // `\n` inside a comment is the two characters '\' 'n', not a newline.
    parsed("(?#a\\nb)") shouldBe Comment("a\\nb")
  }

  "comment in the middle of a sequence" in {
    parsed("a(?#x)b") shouldBe Concat(List(Literal("a"), Comment("x"), Literal("b")))
  }

  "comment at start of group" in {
    parsed("((?#x)a)") shouldBe Group(
      GroupKind.Capturing(1, None),
      Concat(List(Comment("x"), Literal("a"))),
    )
  }

  "comment is not a quantifiable atom in practice but parses if quantified" in {
    // Quantifying a comment is allowed by the parser; the compiler can
    // reject if it cares. We don't enforce here.
    parsed("(?#x)?") shouldBe Quantified(Comment("x"), Quant(0, Some(1), Greedy))
  }

  "comment doesn't allocate group index" in {
    parsed("(?#x)(a)") shouldBe Concat(
      List(Comment("x"), Group(GroupKind.Capturing(1, None), Literal("a")))
    )
  }

  "errors" - {

    "unterminated comment" in {
      failParse("(?#abc").message should include("unterminated comment")
    }
  }

end ParseCommentSpec
