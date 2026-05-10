package io.github.edadma.oniguruma

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** Smoke-test that the corpus loader works and the bundled file contains
  * the expected number of unique patterns. The substantive parser/compiler
  * exercises against the corpus land in later stages — this just keeps the
  * loader honest as the corpus is regenerated. */
class CorpusSpec extends AnyFreeSpec with Matchers:

  "Corpus loader" - {

    "loads the bundled textmate-corpus.txt" in {
      Corpus.patterns should not be empty
    }

    "contains 3418 unique patterns from the 36-grammar TextMate corpus" in {
      // If this number changes you regenerated the corpus — update the
      // memo in `MEMORY.md` and the grammar count if the source set grew.
      Corpus.patterns.size shouldBe 3418
    }

    "every pattern is non-empty" in {
      Corpus.patterns.foreach { p =>
        p should not be empty
      }
    }

    "patterns are deduplicated" in {
      Corpus.patterns.distinct.size shouldBe Corpus.patterns.size
    }
  }

end CorpusSpec
