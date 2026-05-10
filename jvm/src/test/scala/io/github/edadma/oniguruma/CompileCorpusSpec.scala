package io.github.edadma.oniguruma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Compile + smoke-run every bundled TextMate-grammar pattern. Where
  * [[ParseCorpusSpec]] only asks "does the parser produce an AST?",
  * this spec asks "does the AST lower to a runnable [[Program]] and
  * can the VM step through it without throwing?" Stage 5 is the first
  * stage in which we have any real chance of surviving the corpus end
  * to end — backrefs, lookaround, runtime flags, properties, and
  * subroutine calls all landed in stages 4–5.
  *
  * The smoke-run uses each pattern's first 200 chars of source as the
  * test input. The choice is deliberate: pattern source tends to
  * contain enough character variety (punctuation, letters, digits,
  * whitespace) to exercise interesting branches of its own bytecode
  * without crafting a per-pattern fixture. We don't assert on whether
  * a match was found — only that the VM didn't throw and didn't blow
  * the recursion / step caps. That separates "engine implementation
  * holes" (what we want to surface) from "pattern doesn't match this
  * text" (which is per-pattern and uninteresting at this scope).
  *
  * The threshold sits just below the live pass rate so a regression
  * of more than a handful of patterns trips the test. Failures are
  * grouped by message for diagnostic ergonomics. */
class CompileCorpusSpec extends AnyFreeSpec with Matchers:

  /** Minimum acceptable compile-success rate. */
  private val MinCompileRate: Double = 0.95

  /** Minimum acceptable smoke-run-without-throw rate. Counted against
    * the patterns that compiled — a compile failure is already counted
    * by [[MinCompileRate]] above. */
  private val MinSmokeRunRate: Double = 0.99

  "Compiler against the TextMate corpus" - {

    "compiles at least 95% of corpus patterns" in {
      val patterns = Corpus.patterns
      patterns should not be empty

      val results = patterns.map(p => p -> Regex.compile(p))
      val (oks, errs) = results.partition(_._2.isRight)
      val rate = oks.size.toDouble / patterns.size

      info(s"compiled ${oks.size} / ${patterns.size} (${"%.2f".format(rate * 100)}%)")

      val grouped = errs
        .map { case (p, r) => r.left.toOption.getOrElse("?") -> p }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toList
        .sortBy(-_._2.size)

      if rate < MinCompileRate then
        val report = grouped
          .take(10)
          .map { case (msg, ps) => s"  ${ps.size}× $msg\n     example: ${ps.head.take(120)}" }
          .mkString("\n")
        fail(s"corpus compile rate $rate below threshold $MinCompileRate.\nTop errors:\n$report")
      else if grouped.nonEmpty then
        info("top compile-error groups (informational):")
        grouped.take(5).foreach { case (msg, ps) =>
          info(s"  ${ps.size}× $msg — e.g. ${ps.head.take(80)}")
        }
    }

    "VM survives smoke-running every pattern without throwing" in {
      val patterns       = Corpus.patterns
      val compiled       = patterns.flatMap(p => Regex.compile(p).toOption.map(p -> _))
      val sampleInputBy  = (p: String) => p.take(200)

      val (oks, errs) = compiled.partition { case (p, r) =>
        try
          // findFirstMatchIn drives the VM end to end and returns
          // Option, so a None vs Some answer is irrelevant here —
          // we're just checking the engine doesn't throw.
          r.findFirstMatchIn(sampleInputBy(p))
          true
        catch case _: Throwable => false
      }

      val rate = oks.size.toDouble / compiled.size
      info(s"VM ran without throwing on ${oks.size} / ${compiled.size} (${"%.2f".format(rate * 100)}%)")

      val grouped = errs
        .map { case (p, r) =>
          val msg = try
            r.findFirstMatchIn(sampleInputBy(p)); "?"
          catch case t: Throwable => Option(t.getMessage).getOrElse(t.getClass.getSimpleName)
          (msg, p)
        }
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toList
        .sortBy(-_._2.size)

      if rate < MinSmokeRunRate then
        val report = grouped
          .take(10)
          .map { case (msg, ps) => s"  ${ps.size}× $msg\n     example: ${ps.head.take(120)}" }
          .mkString("\n")
        fail(s"VM smoke-run rate $rate below threshold $MinSmokeRunRate.\nTop errors:\n$report")
    }
  }

end CompileCorpusSpec
