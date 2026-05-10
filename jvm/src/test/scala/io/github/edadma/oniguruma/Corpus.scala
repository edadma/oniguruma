package io.github.edadma.oniguruma

import java.util.Base64
import scala.io.Source

/** Loader for the TextMate-grammar regex corpus.
  *
  * The corpus lives at `jvm/src/test/resources/textmate-corpus.txt` and
  * is regenerated from the `*.tmLanguage.json` files under
  * `~/dev/juicer/docs/grammars/` by the `tools/regen-corpus.sh` script.
  * Each line is one base64-encoded regex pattern (the `match`, `begin`,
  * `end`, or `while` field of some grammar rule); duplicates have been
  * deduplicated. Base64 keeps multi-line patterns verbatim — encoding
  * line terminators as `\n` escapes would silently change the meaning
  * of `(?x)`-mode `#` line-comments, since those terminate only on a
  * real newline.
  *
  * Stage 2's parser test exercises every pattern through `Parser.parse`,
  * counting parse successes. Stage 5's validation test exercises every
  * pattern through `Regex.compile` and reports compile-failures grouped
  * by feature.
  */
object Corpus:

  /** Lazily load every pattern from the bundled corpus and base64-decode
    * each line. The result is memoized for the whole test run. */
  lazy val patterns: Vector[String] =
    val src = Source.fromInputStream(
      getClass.getResourceAsStream("/textmate-corpus.txt"),
      "UTF-8",
    )
    val dec = Base64.getDecoder
    try
      src
        .getLines()
        .filter(_.nonEmpty)
        .map(line => new String(dec.decode(line), "UTF-8"))
        .toVector
    finally src.close()
end Corpus
