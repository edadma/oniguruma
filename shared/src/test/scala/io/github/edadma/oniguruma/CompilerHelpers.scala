package io.github.edadma.oniguruma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Shared scaffolding for the compiler / VM test suite. Provides:
  *
  *  - `compile(src)` — parses + compiles a regex source string and
  *     returns the [[Program]], failing the test on parse or compile
  *     error;
  *  - `compileFail(src)` — expect a compile error and return the
  *     message for further assertions;
  *  - `re(src)` — convenient `Regex.compile` wrapper that fails the
  *     test on either parse OR compile error;
  *  - `findFirst(src, input)` / `firstMatch(src, input)` — common
  *     end-to-end shapes for the [[MatchSpec]] tests. */
trait CompilerHelpers extends AnyFreeSpec with Matchers:

  /** Parse + compile in one step; fail the test on either failure. */
  def compile(src: String): Program =
    Parser.parseRegex(src) match
      case Left(err)  => fail(s"parse failure for '$src': $err")
      case Right(ast) =>
        try Compiler.compile(ast)
        catch case e: CompileException => fail(s"compile failure for '$src': ${e.error.message}")

  /** Expect a parse-or-compile failure; return the message. */
  def compileFail(src: String): String =
    Regex.compile(src) match
      case Right(_)  => fail(s"expected compile failure for '$src' but got a Regex")
      case Left(msg) => msg

  /** Build a [[Regex]]; fail the test on any error. */
  def re(src: String): Regex =
    Regex.compile(src) match
      case Right(r)  => r
      case Left(msg) => fail(s"compile failure for '$src': $msg")

  /** First match in `input`, or `None`. */
  def findFirst(src: String, input: String): Option[Match] =
    re(src).findFirstMatchIn(input)

  /** First match's [[Match.matched]] string, or fail the test. */
  def firstMatch(src: String, input: String): String =
    findFirst(src, input)
      .getOrElse(fail(s"no match for '$src' in '$input'"))
      .matched

end CompilerHelpers
