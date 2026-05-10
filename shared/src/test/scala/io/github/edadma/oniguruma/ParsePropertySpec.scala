package io.github.edadma.oniguruma

import Node.*

/** Unicode property classes. The corpus only uses four — `\p{L}`,
  * `\p{M}`, `\p{N}`, `\p{Print}`. Stage 5 wires these to
  * [[UCDProperty]], which builds the sets lazily from the host
  * runtime's `Character.getType` tables. The parser-shape tests below
  * use [[UCDProperty]] directly so they're robust to whichever Unicode
  * version the host JVM/JS/Native build ships. */
class ParsePropertySpec extends ParserHelpers:

  "supported properties parse" - {

    "\\p{L}" in {
      parsed("\\p{L}") shouldBe Klass(UCDProperty.Letter, false)
    }

    "\\p{M}" in {
      parsed("\\p{M}") shouldBe Klass(UCDProperty.Mark, false)
    }

    "\\p{N}" in {
      parsed("\\p{N}") shouldBe Klass(UCDProperty.Number, false)
    }

    "\\p{Print}" in {
      parsed("\\p{Print}") shouldBe Klass(UCDProperty.Print, false)
    }
  }

  "inside char class" - {

    "[\\p{L}]" in {
      // Inside a class, the property contributes its set to the union;
      // the singleton-element class is just the property's set.
      parsed("[\\p{L}]") shouldBe Klass(UCDProperty.Letter, false)
    }

    "[\\p{L}_]" in {
      // Letter property ∪ underscore.
      parsed("[\\p{L}_]") shouldBe Klass(UCDProperty.Letter union IntervalSet.single('_'), false)
    }
  }

  "with quantifier" in {
    parsed("\\p{L}+") shouldBe Quantified(Klass(UCDProperty.Letter, false), Quant(1, None, Greedy))
  }

  "errors" - {

    "unknown property name" in {
      failParse("\\p{Foo}").message should include("unsupported")
    }

    "unterminated property" in {
      failParse("\\p{L").message should include("unterminated")
    }

    "missing { after \\p" in {
      failParse("\\pL").message should include("expected '{'")
    }
  }

end ParsePropertySpec
