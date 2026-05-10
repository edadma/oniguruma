package io.github.edadma.oniguruma

/** End-to-end Unicode-property matching. Stage 5 wires `\p{L}`,
  * `\p{M}`, `\p{N}`, and `\p{Print}` to [[UCDProperty]], which builds
  * each set lazily from the host runtime's `Character.getType` tables.
  *
  * The test inputs stay in well-defined Unicode regions where the
  * category assignments have been stable for many years, so the
  * specs are robust to whichever Unicode version the JVM/JS/Native
  * build ships with — no need to pin a specific UCD revision. */
class MatchPropertySpec extends CompilerHelpers:

  "\\p{L} — Letter" - {

    "matches ASCII letters" in {
      firstMatch("\\p{L}+", "abc123") shouldBe "abc"
    }

    "matches non-ASCII letters" in {
      // Greek alpha-beta-gamma — all general category Ll (lowercase letter).
      firstMatch("\\p{L}+", "αβγ123") shouldBe "αβγ"
    }

    "rejects digits" in {
      findFirst("\\p{L}", "123") shouldBe None
    }

    "rejects punctuation" in {
      findFirst("\\p{L}", ".,!?") shouldBe None
    }
  }

  "\\p{N} — Number" - {

    "matches ASCII digits" in {
      firstMatch("\\p{N}+", "abc123def") shouldBe "123"
    }

    "matches Roman numerals (LETTER_NUMBER)" in {
      // U+2160 (Ⅰ) is general category Nl — letter-like number.
      firstMatch("\\p{N}", "Ⅰ") shouldBe "Ⅰ"
    }

    "rejects letters" in {
      findFirst("\\p{N}", "abc") shouldBe None
    }
  }

  "\\p{M} — Mark" - {

    "matches a combining mark" in {
      // U+0301 COMBINING ACUTE ACCENT — general category Mn.
      val m = re("\\p{M}").findFirstMatchIn("é").get
      m.matched shouldBe "́"
    }

    "rejects ordinary letters" in {
      findFirst("\\p{M}", "abc") shouldBe None
    }
  }

  "\\p{Print} — printable" - {

    "matches letters, digits, punctuation, and space" in {
      // The full ASCII printable range gets matched.
      firstMatch("\\p{Print}+", "Hello, world!") shouldBe "Hello, world!"
    }

    "rejects control characters" in {
      // \t (U+0009) is a control char, not in Print.
      findFirst("\\p{Print}", "\t") shouldBe None
    }

    "rejects newlines" in {
      // \n (U+000A) is a line separator at the category level — not Print.
      findFirst("\\p{Print}", "\n") shouldBe None
    }
  }

  "negated property" - {

    "\\P{L} matches non-letters" in {
      firstMatch("\\P{L}+", "abc123def") shouldBe "123"
    }
  }

  "in char-class composition" - {

    "[\\p{L}_]+ matches identifier-like runs" in {
      firstMatch("[\\p{L}_]+", "_foo_bar 123") shouldBe "_foo_bar"
    }

    "[\\p{L}\\p{N}]+ matches letters AND numbers together" in {
      firstMatch("[\\p{L}\\p{N}]+", "abc123!def") shouldBe "abc123"
    }
  }

end MatchPropertySpec
