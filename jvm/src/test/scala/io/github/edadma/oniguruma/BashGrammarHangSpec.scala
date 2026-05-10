package io.github.edadma.oniguruma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Regression for a downstream highlighter bug: the patterns from the
  * Bash TextMate grammar's `#qstring-double` set (and the top-level
  * for-statement context) used to spin the VM for multiple seconds on
  * tiny inputs (`echo "$HOME"` — 17 chars; `for i in 1 2 3; do echo
  * $i; done` — 32 chars). Reference Oniguruma C handles them in
  * milliseconds, so the difference is a backtracking-strategy choice
  * in this port — most likely the empty-body-loop pathology, since
  * many of the patterns involve `*` and `+` over alternation with
  * possibly-empty arms.
  *
  * The patterns below are extracted verbatim from
  * `juicer/docs/grammars/bash.tmLanguage.json` — the qstring-double
  * inner pattern set (escape + #variable + #interpolation refs) and
  * the top-level rules that fire on a for-loop. Whatever happens to
  * those patterns at the grammar level must reproduce here, against
  * the same input strings.
  *
  * The wall-clock budgets (`<500ms` per scan) are 50× headroom over
  * what reference Onig achieves and 10× tighter than what the
  * downstream user saw under oniguruma 0.0.3 (>5s). A regression
  * either bumps the time past 500ms (slow but terminating) or
  * trips the `StepLimit` to return `None` on patterns that should
  * match. Both fail the test. */
class BashGrammarHangSpec extends AnyFreeSpec with Matchers:

  /** Wall-clock cap per scan. 6.D's per-Split progress check should
    * make these scans finish in well under 50ms; 500ms is the
    * regression boundary. */
  private val ScanBudgetMs = 500L

  /** Run `body`; fail if it takes longer than `ScanBudgetMs`. The
    * returned value of `body` is propagated for the caller's
    * assertions. */
  private def withinBudget[A](body: => A): A =
    val t0 = System.nanoTime()
    val r  = body
    val dt = (System.nanoTime() - t0) / 1_000_000
    if dt > ScanBudgetMs then
      fail(s"scan took ${dt}ms, expected < ${ScanBudgetMs}ms — likely backtracking pathology regression")
    r

  // ─────────────────────────────────────────────────────────────────
  // qstring-double inner pattern set
  // ─────────────────────────────────────────────────────────────────
  //
  // From bash.tmLanguage.json's `qstring-double` rule:
  //   begin: "
  //   end:   "
  //   patterns: [ escape, #variable, #interpolation ]
  //
  // The patterns the scanner has in scope INSIDE the body of a
  // double-quoted string are: the end (close-quote) regex, the escape
  // regex, every regex from the #variable repository entry, and every
  // (non-include) regex from the #interpolation repository entry.
  // This is the exact set the highlighter feeds into a TmScanner when
  // its rule stack tops out at `qstring-double`.
  private val qstringDoubleInnerPatterns: Seq[String] = Seq(
    // end: closing `"` (zero-width in TmScanner terms — the scanner
    // returns the position and the caller's stack-pop logic handles it)
    "\"",
    // escape sequences inside the string body
    "\\\\[\\$\\n`\"\\\\]",
    // #variable repository — six match/begin patterns
    "(?:(\\$)(\\@(?!\\w)))",
    "(?:(\\$)([0-9](?!\\w)))",
    "(?:(\\$)([-*#?$!0_](?!\\w)))",
    "(?:(\\$)(\\{)(?:[ \\t]*+)(?=\\d))",
    "(?:(\\$)(\\{))",
    "(?:(\\$)((?:\\w+)(?!\\w)))",
    // #interpolation repository — subshell begin + backtick begin
    "(?:\\$\\()",
    "`",
  )

  "qstring-double inner pattern set" - {

    "finds the $HOME variable in `echo \"$HOME\"` body within budget" in {
      val scanner = TmScanner(qstringDoubleInnerPatterns*)
      // Scan from position 6 of `echo "$HOME"` — i.e. the `$` right
      // after the opening quote, which is the spot the highlighter
      // would resume from once the begin-rule has fired.
      val input = """echo "$HOME""""
      val m = withinBudget(scanner.findNextMatch(input, 6, 0))
      m shouldBe defined
      // The winning pattern should be the `$word` form (index 7 in
      // the list above: the sixth #variable pattern, after the four
      // before it).
      m.get.patternIndex shouldBe 7
      val rm = m.get.regexMatch
      rm.start shouldBe 6
      rm.end shouldBe 11 // covers "$HOME"
    }

    "finds the closing quote after the variable expansion" in {
      val scanner = TmScanner(qstringDoubleInnerPatterns*)
      val input   = """echo "$HOME""""
      // After matching $HOME (positions 6..11), the scanner resumes
      // at position 11 looking for the closing quote at position 11.
      val m = withinBudget(scanner.findNextMatch(input, 11, 11))
      m shouldBe defined
      // The closing `"` is pattern index 0 in the list above.
      m.get.patternIndex shouldBe 0
      m.get.regexMatch.start shouldBe 11
    }

    "scanner doesn't hang at the position right before a `\"`" in {
      // The position right before the closing quote — exactly the
      // configuration where no #variable / #interpolation pattern
      // would match but the end pattern does. A regression in the
      // empty-body check might cause the variable patterns to spin
      // before failing.
      val scanner = TmScanner(qstringDoubleInnerPatterns*)
      val m = withinBudget(scanner.findNextMatch("\"", 0, 0))
      m shouldBe defined
      m.get.patternIndex shouldBe 0
    }

    "full body of `echo \"$HOME\"` scans end-to-end within budget" in {
      // Simulates the highlighter's scan loop INSIDE the string body:
      // start at position 6 (after the opening quote), advance past
      // each match, until findNextMatch returns None or hits the
      // closing quote.
      val scanner = TmScanner(qstringDoubleInnerPatterns*)
      val input   = """echo "$HOME""""
      var pos     = 6
      var prev    = 6
      var hits    = List.empty[TmScanner.Match]
      var done    = false
      withinBudget {
        while !done && pos <= input.length do
          scanner.findNextMatch(input, pos, prev) match
            case Some(m) =>
              hits = m :: hits
              prev = m.regexMatch.end
              if m.regexMatch.end == m.regexMatch.start then
                // Zero-width match — advance one codepoint so the
                // scanner doesn't pin at this offset.
                pos =
                  if pos >= input.length then input.length + 1
                  else pos + Character.charCount(Character.codePointAt(input, pos))
              else pos = m.regexMatch.end
              if m.patternIndex == 0 then done = true // closing quote
            case None =>
              done = true
      }
      hits.reverse.map(_.patternIndex) shouldBe List(7, 0)
    }
  }

  // ─────────────────────────────────────────────────────────────────
  // Top-level for-statement patterns
  // ─────────────────────────────────────────────────────────────────
  //
  // The for-loop case is harder to isolate because the grammar's
  // top-level scope is huge. The downstream symptom is `for i in 1
  // 2 3; do echo $i; done` taking >20s. The smoking guns are the
  // patterns that fire greedily across whitespace and operator
  // boundaries — the keyword and command/variable patterns.
  //
  // This pattern set is a subset that covers the constructs in the
  // 32-char input above. If any single pattern is the culprit, this
  // scan will exceed the budget.
  private val forLoopPatterns: Seq[String] = Seq(
    // keyword: control words around `for`, `in`, `do`, `done`, etc.
    "(?<=^|;|&|\\s)(then|else|elif|fi|for|in|do|done|select|continue|esac|while|until|return)(?=\\s|;|&|$)",
    // line continuation
    "\\\\(?=\\n)",
    // numeric literal
    "(?<=^|\\s|;|&|\\()[0-9]+\\b",
    // simple $variable
    "(?:(\\$)((?:\\w+)(?!\\w)))",
    // statement separator
    "[;&]",
    // whitespace skip
    "[ \\t]+",
    // bare command word
    "(?<=^|;|&|\\s)[a-zA-Z_][a-zA-Z0-9_]*",
  )

  "for-statement top-level pattern set" - {

    "finds `for` keyword in `for i in ...; done` within budget" in {
      val scanner = TmScanner(forLoopPatterns*)
      val input   = "for i in 1 2 3; do echo $i; done"
      val m = withinBudget(scanner.findNextMatch(input, 0, 0))
      m shouldBe defined
      // The `for` keyword should win at index 0.
      m.get.patternIndex shouldBe 0
      m.get.regexMatch.matched shouldBe "for"
    }

    "tokenises the full for-loop within budget" in {
      val scanner = TmScanner(forLoopPatterns*)
      val input   = "for i in 1 2 3; do echo $i; done"
      var pos     = 0
      var prev    = 0
      var count   = 0
      val cap     = 200 // sanity: at most ~50 tokens in a 32-char string
      withinBudget {
        var done = false
        while !done && pos <= input.length && count < cap do
          scanner.findNextMatch(input, pos, prev) match
            case Some(m) =>
              count += 1
              prev = m.regexMatch.end
              pos =
                if m.regexMatch.end == m.regexMatch.start then
                  if pos >= input.length then input.length + 1
                  else pos + Character.charCount(Character.codePointAt(input, pos))
                else m.regexMatch.end
            case None =>
              done = true
      }
      count should be > 0
      count should be < cap
    }
  }

  // ─────────────────────────────────────────────────────────────────
  // Per-pattern hang detector — every corpus pattern individually
  // ─────────────────────────────────────────────────────────────────
  //
  // Stress-test EVERY pattern from the deduplicated TextMate corpus
  // (3418 patterns spanning 36 grammars) against the two reproducer
  // inputs and the input alone. We're looking for any single pattern
  // whose `matchAt` takes longer than a generous per-pattern budget.
  // Without 6.D, the bash grammar's qstring-double inner patterns
  // hang at ~1 sec each (the StepLimit ceiling) before returning
  // None — that's the failure mode this test pins.
  //
  // Budget is generous (50ms per pattern) so a wide grammar set's
  // worst case doesn't false-positive on CI noise; a real hang ran
  // at multiple seconds, so the gap to a regression is enormous.

  private val PerPatternBudgetMs = 50L

  private val bugInputs: Seq[(String, String)] = Seq(
    "echo-quoted-home" -> """echo "$HOME"""",
    "for-loop"         -> "for i in 1 2 3; do echo $i; done",
    "empty"            -> "",
    "spaces"           -> "   ",
  )

  /** Time every pattern against `input` using the highlighter's exact
    * `matchAt(line, pos, pos)` call shape — anchored at each codepoint
    * boundary, with `prevMatchEnd == pos` so `\G` evaluates the way
    * the highlighter sees it. Returns each (pattern, ms) that exceeded
    * `budgetMs`, longest first. */
  private def slowestPatterns(
      patterns: Seq[String],
      input: String,
      budgetMs: Long,
  ): List[(String, Long)] =
    var slow = List.empty[(String, Long)]
    val it   = patterns.iterator
    while it.hasNext do
      val src = it.next()
      Regex.compile(src) match
        case Right(rx) =>
          val t0 = System.nanoTime()
          var pos = 0
          val len = input.length
          while pos <= len do
            rx.matchAt(input, pos, pos)
            pos += 1
          val dt = (System.nanoTime() - t0) / 1_000_000
          if dt > budgetMs then slow = (src, dt) :: slow
        case Left(_) => ()
    slow.sortBy(-_._2)

  "no single corpus pattern hangs on the bug-report inputs" - {

    for (label, input) <- bugInputs do
      label in {
        slowestPatterns(Corpus.patterns, input, PerPatternBudgetMs) match
          case Nil  => ()
          case slow =>
            val report = slow.take(10).map { case (p, ms) =>
              val abbrev = if p.length > 80 then p.take(77) + "..." else p
              s"  ${ms}ms — $abbrev"
            }.mkString("\n")
            fail(
              s"${slow.size} pattern(s) exceeded ${PerPatternBudgetMs}ms on '$input':\n$report"
            )
      }
  }

  // ─────────────────────────────────────────────────────────────────
  // Bash-only patterns — full 195 entries, BEFORE corpus dedup
  // ─────────────────────────────────────────────────────────────────
  //
  // The bundled `textmate-corpus.txt` deduplicates across 36 grammars,
  // so a pathological-only-in-isolation bash pattern can disappear if
  // any other grammar happens to use the same string. To rule that
  // out we ship `bash-grammar-patterns.txt` — the raw match/begin/end
  // strings extracted from `bash.tmLanguage.json` in source order,
  // NOT deduplicated. 195 patterns, 149 unique. This is the test that
  // directly mirrors the highlighter's pattern set when running on a
  // bash file.

  private lazy val bashPatterns: Vector[String] =
    val src = scala.io.Source.fromInputStream(
      getClass.getResourceAsStream("/bash-grammar-patterns.txt"),
      "UTF-8",
    )
    val dec = java.util.Base64.getDecoder
    try
      src.getLines()
        .filter(_.nonEmpty)
        .map(line => new String(dec.decode(line), "UTF-8"))
        .toVector
    finally src.close()

  "no single bash-grammar pattern hangs on the bug-report inputs (no-dedup)" - {

    for (label, input) <- bugInputs do
      label in {
        val patterns = bashPatterns
        patterns should not be empty
        slowestPatterns(patterns, input, PerPatternBudgetMs) match
          case Nil  => ()
          case slow =>
            val report = slow.take(10).map { case (p, ms) =>
              val abbrev = if p.length > 80 then p.take(77) + "..." else p
              s"  ${ms}ms — $abbrev"
            }.mkString("\n")
            fail(
              s"${slow.size} bash pattern(s) exceeded ${PerPatternBudgetMs}ms on '$input':\n$report"
            )
      }
  }

end BashGrammarHangSpec
