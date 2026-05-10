package io.github.edadma.oniguruma

/** A parser failure. Holds a message and a 0-based character position into
  * the source string pointing at the first character that didn't fit. The
  * parser bails on the first error it sees; there is no recovery and no
  * second error reported. */
final case class ParseError(message: String, position: Int):
  override def toString: String = s"ParseError at $position: $message"
