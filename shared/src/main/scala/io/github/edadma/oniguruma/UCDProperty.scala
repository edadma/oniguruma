package io.github.edadma.oniguruma

/** Unicode general-category property sets used by `\p{…}` in the
  * TextMate-flavor parser. Stage 5 ships exactly the four properties
  * the 36-grammar TextMate corpus actually references — `L`, `M`, `N`,
  * `Print` — built lazily from `java.lang.Character.getType` so we
  * don't ship a copy of `UnicodeData.txt`. Each property maps to one
  * [[IntervalSet]] computed once on first use, then memoized.
  *
  * The data source is whatever Unicode version the host runtime ships:
  * the JVM picks up the JDK's bundled UCD, Scala.js delegates to the
  * JS engine, Scala Native uses its own embedded tables. They agree to
  * within a few hundred unassigned codepoints across recent versions —
  * fine for grammar matching where the live letters/numbers are stable
  * far below those edges.
  *
  * Property semantics:
  *   - `L` — every "Letter" general category (Lu/Ll/Lt/Lm/Lo)
  *   - `M` — every "Mark" general category (Mn/Mc/Me)
  *   - `N` — every "Number" general category (Nd/Nl/No)
  *   - `Print` — printable / visible characters: L | M | N |
  *     Punctuation (P*) | Symbol (S*) | Space-separator (Zs). Excludes
  *     control codes, line separators, paragraph separators, format,
  *     surrogate, private-use, and unassigned. Matches Onig's
  *     `Print` interpretation. */
object UCDProperty:
  import java.lang.Character

  /** `\p{L}` — Letter. */
  lazy val Letter: IntervalSet = buildSet(isLetterCategory)

  /** `\p{M}` — Mark. */
  lazy val Mark: IntervalSet = buildSet(isMarkCategory)

  /** `\p{N}` — Number. */
  lazy val Number: IntervalSet = buildSet(isNumberCategory)

  /** `\p{Print}` — printable: L | M | N | P | S | Zs. */
  lazy val Print: IntervalSet = buildSet(isPrintCategory)

  /** **The set `\b` is about**, which is NOT the set `\w` is about — `L | M | N | Pc`.
   *
   * Onig keeps the two apart and this is the only place that shows: `\w` is ASCII, so `Á` does not
   * match it, and `\b` is Unicode, so there is no boundary either side of `Á` in `xÁy`. Measured
   * against Onig itself rather than reasoned about — Ruby's regexes are Onig, and it answers `none`
   * for `x\bÁ`, for `caf\bé`, for `a\b٣`, for a combining mark and for `a\b名`, while answering
   * `boundary` for a space and for `-`. `\p{N}` rather than the decimal digits alone, and `Pc`
   * rather than `_` alone, for the same reason: `Ⅷ`, `¹` and `‿` all answer `none` there, and `$`
   * answers `boundary`.
   *
   * That is UTS #18's word definition, which is what an encoding-aware `ONIGENC_IS_CODE_WORD`
   * amounts to for UTF-8.
   */
  lazy val BoundaryWord: IntervalSet = buildSet(isBoundaryWordCategory)

  private inline def isLetterCategory(t: Int): Boolean =
    t == Character.UPPERCASE_LETTER       ||
      t == Character.LOWERCASE_LETTER     ||
      t == Character.TITLECASE_LETTER     ||
      t == Character.MODIFIER_LETTER      ||
      t == Character.OTHER_LETTER

  private inline def isMarkCategory(t: Int): Boolean =
    t == Character.NON_SPACING_MARK       ||
      t == Character.COMBINING_SPACING_MARK ||
      t == Character.ENCLOSING_MARK

  private inline def isNumberCategory(t: Int): Boolean =
    t == Character.DECIMAL_DIGIT_NUMBER   ||
      t == Character.LETTER_NUMBER        ||
      t == Character.OTHER_NUMBER

  private inline def isBoundaryWordCategory(t: Int): Boolean =
    isLetterCategory(t) || isMarkCategory(t) || isNumberCategory(t) ||
      t == Character.CONNECTOR_PUNCTUATION

  private inline def isPunctCategory(t: Int): Boolean =
    t == Character.CONNECTOR_PUNCTUATION  ||
      t == Character.DASH_PUNCTUATION     ||
      t == Character.START_PUNCTUATION    ||
      t == Character.END_PUNCTUATION      ||
      t == Character.INITIAL_QUOTE_PUNCTUATION ||
      t == Character.FINAL_QUOTE_PUNCTUATION   ||
      t == Character.OTHER_PUNCTUATION

  private inline def isSymbolCategory(t: Int): Boolean =
    t == Character.MATH_SYMBOL            ||
      t == Character.CURRENCY_SYMBOL      ||
      t == Character.MODIFIER_SYMBOL      ||
      t == Character.OTHER_SYMBOL

  private inline def isPrintCategory(t: Int): Boolean =
    isLetterCategory(t) || isMarkCategory(t) || isNumberCategory(t) ||
      isPunctCategory(t) || isSymbolCategory(t) ||
      t == Character.SPACE_SEPARATOR

  /** Walk 0..0x10FFFF, group consecutive codepoints whose category
    * matches `predicate` into inclusive ranges. The result is built
    * directly as a sorted-disjoint Vector, skipping `IntervalSet.of`'s
    * normalize pass — the construction is single-pass and already
    * disjoint by definition.
    *
    * Cost: ~1.1M `getType` calls per property. Each is a couple of
    * lookups; the whole walk takes well under a second on the JVM
    * even cold. The result is memoized, so subsequent accesses are
    * free. */
  private def buildSet(predicate: Int => Boolean): IntervalSet =
    val builder    = Vector.newBuilder[(Int, Int)]
    var inRange    = false
    var rangeStart = 0
    var cp         = 0
    while cp <= 0x10FFFF do
      val matches = predicate(Character.getType(cp))
      if matches then
        if !inRange then
          rangeStart = cp
          inRange    = true
      else
        if inRange then
          builder += ((rangeStart, cp - 1))
          inRange  = false
      cp += 1
    if inRange then builder += ((rangeStart, 0x10FFFF))
    // Build directly via the public `of` factory; the input is already
    // sorted+disjoint, but normalize() is cheap on already-canonical
    // input so we don't bother bypassing it.
    IntervalSet.of(builder.result()*)

end UCDProperty
