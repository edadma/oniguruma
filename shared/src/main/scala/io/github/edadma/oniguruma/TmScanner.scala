package io.github.edadma.oniguruma

/** TextMate-flavor multi-pattern scanner — the shape that
  * `vscode-textmate` and every TextMate grammar interpreter
  * expects from an Onig binding.
  *
  * A scanner wraps a fixed `Vector[Regex]` (the begin/match/end
  * patterns of a grammar's rules) and answers a single question:
  * "from this offset, what's the next position where any of my
  * patterns match, and which pattern won?" The answer threads
  * `prevMatchEnd` through to the underlying VM so `\G` works
  * naturally — TextMate `while` rules rely on it.
  *
  * Tie-breaking semantics match the Onig C library and
  * vscode-textmate: at the earliest position where ANY pattern
  * matches, the LOWEST-indexed pattern that matches at that
  * position wins. The walk advances one codepoint at a time when
  * no pattern matches (so supplementary codepoints don't get
  * cleaved into surrogate halves). */
final class TmScanner private[oniguruma] (val patterns: Vector[Regex]):

  /** Number of compiled patterns in the scanner. */
  def size: Int = patterns.length

  /** Find the next match of any pattern at or after `start`.
    *
    * @param start         UTF-16 offset to begin scanning from
    * @param prevMatchEnd  what `\G` should compare against — pass 0
    *                      for the very first call; pass the previous
    *                      result's `m.end` for subsequent calls
    * @return [[TmScanner.Match]] on hit, `None` if no pattern matches
    *         anywhere from `start` to end of input. */
  def findNextMatch(input: String, start: Int, prevMatchEnd: Int): Option[TmScanner.Match] =
    if patterns.isEmpty then None
    else
      var pos       = start
      val len       = input.length
      var result    = Option.empty[TmScanner.Match]
      var done      = false
      while !done && pos <= len do
        var i = 0
        // At this position, try every pattern in order; the FIRST hit
        // wins because rules are pre-ranked by author intent.
        while i < patterns.length && result.isEmpty do
          patterns(i).matchAt(input, pos, prevMatchEnd) match
            case Some(m) => result = Some(TmScanner.Match(i, m))
            case None    => i += 1
        if result.isDefined then
          done = true
        else if pos == len then
          done = true
        else
          // Advance one codepoint. Walking by codepoint (not UTF-16
          // unit) keeps supplementary characters atomic for callers
          // that reason about positions in codepoint terms; the index
          // arithmetic stays in UTF-16 since that's what `String`
          // exposes.
          val cp = Character.codePointAt(input, pos)
          pos += Character.charCount(cp)
      result

end TmScanner

object TmScanner:

  /** A successful scanner match: which pattern fired and its
    * underlying [[Match]] (groups, range, the works).
    *
    * @param patternIndex  index into [[TmScanner.patterns]] of the
    *                      rule that won
    * @param regexMatch    the underlying engine match — call
    *                      `regexMatch.start` / `.end` / `.group(n)`
    *                      exactly as you would on a single-pattern
    *                      [[Regex]] result */
  final case class Match(patternIndex: Int, regexMatch: io.github.edadma.oniguruma.Match)

  /** Compile a list of source strings into a runnable scanner.
    * Returns `Right(scanner)` on success, or `Left(message)` on the
    * first parse-or-compile error — the message includes the
    * offending pattern index so callers can flag the bad rule. */
  def compile(sources: Seq[String]): Either[String, TmScanner] =
    val builder = Vector.newBuilder[Regex]
    var i       = 0
    val it      = sources.iterator
    while it.hasNext do
      val src = it.next()
      Regex.compile(src) match
        case Right(r)  => builder += r; i += 1
        case Left(msg) => return Left(s"pattern $i: $msg")
    Right(new TmScanner(builder.result()))

  /** Throwing form for tests / REPL — fails the call on the first
    * unparseable pattern. */
  def apply(sources: String*): TmScanner =
    compile(sources) match
      case Right(s)  => s
      case Left(msg) => throw new IllegalArgumentException(msg)

end TmScanner
