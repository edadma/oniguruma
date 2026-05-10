package io.github.edadma.oniguruma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Common test scaffolding shared by the parser-test suite. Provides
  * `parsed(s)` to expect a successful parse and surface the AST, and
  * `failParse(s)` to expect a failure and surface the error so the test
  * can pin its message or position. Test files extend this trait
  * directly instead of repeating boilerplate. */
trait ParserHelpers extends AnyFreeSpec with Matchers:

  /** Run the parser; on success return the AST node, on failure call
    * ScalaTest's `fail` so the test reports the parse error directly. */
  def parsed(input: String): Node =
    Parser.parseRegex(input) match
      case Right(node) => node
      case Left(err)   => fail(s"unexpected parse failure for '$input': $err")

  /** Run the parser; expect a failure and return the [[ParseError]] for
    * further inspection. The opposite of [[parsed]]. */
  def failParse(input: String): ParseError =
    Parser.parseRegex(input) match
      case Right(node) => fail(s"expected parse failure for '$input' but got: $node")
      case Left(err)   => err

end ParserHelpers
