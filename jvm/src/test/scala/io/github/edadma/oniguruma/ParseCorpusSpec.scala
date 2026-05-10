package io.github.edadma.oniguruma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Run the parser against every bundled TextMate-grammar pattern. The
  * goal is high coverage, not 100%: a handful of patterns in the corpus
  * use Onig forms we intentionally do not support (single-quote group
  * forms, conditional groups, `\Q…\E`, etc.), and a handful are
  * borderline cases the parser may legitimately reject. The threshold
  * below is set well below the actual pass rate so the test catches
  * regressions while staying robust to scope changes.
  *
  * On failure, the spec prints the first few unparseable patterns
  * grouped by error message — that's the diagnostic surface for the
  * stage-5 fully-validating compile pass. */
class ParseCorpusSpec extends AnyFreeSpec with Matchers:

  /** Minimum acceptable parse-success rate. Stage 2 currently parses
    * 100% of the corpus; the threshold is set just below that so a
    * regression of more than a couple of patterns trips the test. */
  private val MinPassRate: Double = 0.99

  "Parser against the TextMate corpus" - {

    "parses at least 99% of corpus patterns successfully" in {
      val patterns = Corpus.patterns
      patterns should not be empty

      val (oks, errs) = patterns.partition(p => Parser.parseRegex(p).isRight)
      val rate        = oks.size.toDouble / patterns.size

      info(s"parsed ${oks.size} / ${patterns.size} (${(rate * 100).formatted("%.2f")}%)")

      // Group failures by error message, show the top 10 to ease diagnosis.
      val grouped = errs
        .map { p =>
          val msg = Parser.parseRegex(p).left.toOption.fold("?")(_.message)
          (msg, p)
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toList
        .sortBy(-_._2.size)

      if rate < MinPassRate then
        val report = grouped
          .take(10)
          .map { case (msg, ps) =>
            s"  ${ps.size}× $msg\n     example: ${ps.head}"
          }
          .mkString("\n")
        fail(s"corpus parse rate $rate below threshold $MinPassRate.\nTop errors:\n$report")
      else
        // Even on success, log the top error groups so newly-broken
        // categories get noticed during a healthy run.
        if grouped.nonEmpty then
          info("top error groups (informational):")
          grouped.take(5).foreach { case (msg, ps) =>
            info(s"  ${ps.size}× $msg — e.g. ${ps.head.take(80)}")
          }
    }

    "no parse hangs or throws (only Either left/right)" in {
      // Catch any uncaught throwable — parser must always return Either.
      Corpus.patterns.foreach { p =>
        try
          Parser.parseRegex(p)
        catch
          case t: Throwable =>
            fail(s"parser threw on pattern '$p': $t")
      }
    }
  }

end ParseCorpusSpec
