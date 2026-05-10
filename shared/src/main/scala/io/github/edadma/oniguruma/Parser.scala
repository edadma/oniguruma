package io.github.edadma.oniguruma

import scala.collection.mutable

/** Recursive-descent parser for the TextMate-flavor Oniguruma syntax.
  *
  * Public API is a single function on the companion. The class itself
  * holds all the mutable cursor / group-counter / named-group state that
  * the parse needs, lives only for the duration of one call, and is not
  * thread-safe. Errors propagate out via a private exception that the
  * companion catches at the top level — this keeps the inner methods
  * straight-line, no Either-plumbing.
  *
  * The grammar implemented (informally):
  * {{{
  *   regex     ::= alt
  *   alt       ::= seq ('|' seq)*
  *   seq       ::= atom*
  *   atom      ::= primary quantifier?
  *   primary   ::= literal | dot | anchor | charclass | group | escape
  *   group     ::= '(' grouphead alt ')'
  *   grouphead ::= ':' | '>' | '=' | '!' | '<' name '>' | '<=' | '<!'
  *               | '#' comment | flags ':'? | (no head ⇒ capturing)
  * }}}
  *
  * Character-class internals, escape handling, and Unicode-property
  * classes are parsed in their own dedicated methods. */
object Parser:

  /** Parse a regex source string into an AST. Returns `Right(node)` on
    * success or `Left(ParseError)` on the first failure. */
  def parseRegex(input: String): Either[ParseError, Node] =
    val p = new Parser(input)
    try
      val node = p.parseAlt()
      if p.pos < input.length then
        Left(ParseError(s"unexpected '${input.charAt(p.pos)}'", p.pos))
      else Right(node)
    catch case e: ParseException => Left(e.error)

end Parser

/** Internal sentinel used to unwind a parse cleanly to the top level. */
private final class ParseException(val error: ParseError) extends RuntimeException(error.toString)

/** Hand-written recursive-descent parser. Held mutable for one parse only. */
final class Parser private[oniguruma] (input: String):
  import Node.*
  import GroupKind.{NonCapturing, Capturing, Atomic}

  // ---- mutable parse state ----

  /** 0-based cursor into [[input]]. */
  private[oniguruma] var pos: Int = 0

  /** Running group counter; incremented when a capturing or named group
    * opens, regardless of textual order. Non-capturing and atomic groups
    * do not advance it. */
  private var groupCounter: Int = 0

  /** Map from declared name → 1-based capture indices. Onig allows
    * multiple captures sharing a name (the branch-reset idiom — each
    * alternation arm uses the same name for "the matching one"); the
    * parser tracks every occurrence and lets the compiler decide how
    * to resolve a `\k<name>` reference. */
  private val namedGroups: mutable.LinkedHashMap[String, mutable.ListBuffer[Int]] =
    mutable.LinkedHashMap.empty

  /** Currently-active flag set, used by the parser itself to drive
    * extended-mode whitespace + `#`-comment skipping. Tracked alongside
    * the AST [[FlagsScope]] nodes (which capture the *change* and let
    * later stages restore at scope exit). Group-bracketed scopes save
    * and restore this var; bare `(?x)` mutates it and lets the leak
    * propagate to the rest of the surrounding group. */
  private var activeFlags: Flags = Flags.Empty

  /** Skip the whitespace + `# line comment` that extended mode renders
    * invisible. Called at every "atom-position" boundary in [[parseSeq]]
    * and [[parseAtom]], but never inside character classes or inside
    * comment / name / escape syntax — those keep their literal bytes. */
  private def skipExtendedWhitespace(): Unit =
    if Flags.has(activeFlags, Flags.Extended) then
      var keepGoing = true
      while keepGoing && !atEnd do
        val c = peekChar
        if c == '#' then
          while !atEnd && peekChar != '\n' do advance()
          if !atEnd then advance() // consume the newline too
        else if c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == 0x0B then
          advance()
        else keepGoing = false

  /** Save activeFlags around `body`. Used by every group form whose
    * inner flag changes must NOT leak out of the group's `)`. Bare
    * `(?x)` deliberately bypasses this so its mutation persists. */
  private def withSavedFlags[T](body: => T): T =
    val saved = activeFlags
    try body finally activeFlags = saved

  // ---- top-level grammar ----

  private[oniguruma] def parseAlt(): Node =
    val branches = mutable.ListBuffer.empty[Node]
    branches += parseSeq()
    while peek('|') do
      advance()
      branches += parseSeq()
    if branches.size == 1 then branches.head
    else Alt(branches.toList)

  private[oniguruma] def parseSeq(): Node =
    val items = mutable.ListBuffer.empty[Node]
    skipExtendedWhitespace()
    while !atEnd && !atSeqEnd do
      items += parseAtom()
    foldAndCollapse(items.toList)

  private def atSeqEnd: Boolean = peek('|') || peek(')')

  /** One atom = primary + optional trailing quantifier. Extended-mode
    * whitespace is skipped at three boundaries: before the primary, between
    * the primary and a possible quantifier, and after the quantifier so the
    * next loop iteration sees the next real token. */
  private def parseAtom(): Node =
    val primary = parsePrimary()
    skipExtendedWhitespace()
    val quantified = parseQuantifier(primary)
    skipExtendedWhitespace()
    quantified

  // ---- primary ----

  private def parsePrimary(): Node =
    if atEnd then return Empty
    val c = peekChar
    c match
      case '.'                           => advance(); Dot
      case '^'                           => advance(); Anchor(AnchorKind.Caret)
      case '$'                           => advance(); Anchor(AnchorKind.Dollar)
      case '['                           => parseCharClass()
      case '('                           => parseGroup()
      case '\\'                          => parseBackslash()
      case '*' | '+' | '?'               => fail(s"quantifier '$c' has nothing to apply to")
      case '{' if hasBraceQuantifier     => fail("brace quantifier has nothing to apply to")
      case _                             => parseLiteralPrimary()

  /** A literal-position char read consumes one Unicode codepoint (which
    * may take two UTF-16 units if it's a supplementary character) and
    * returns it as a single-char [[Literal]]. Adjacent literals get
    * folded together by [[foldAndCollapse]] at the end of [[parseSeq]],
    * but keeping each codepoint separate at this stage means a trailing
    * quantifier always binds to exactly one char, which matches Onig
    * semantics ("abc*" → "ab" then "c*"). */
  private def parseLiteralPrimary(): Node =
    val cp = readCodepoint()
    Literal(new String(Character.toChars(cp)))

  // ---- quantifier ----

  private def parseQuantifier(body: Node): Node =
    if atEnd then return body
    peekChar match
      case '?'                           => advance(); finishQuant(body, 0, Some(1))
      case '*'                           => advance(); finishQuant(body, 0, None)
      case '+'                           => advance(); finishQuant(body, 1, None)
      case '{' if hasBraceQuantifier =>
        val (min, max) = parseBraceQuantifier()
        finishQuant(body, min, max)
      case _                             => body

  private def finishQuant(body: Node, min: Int, max: Option[Int]): Node =
    val mode =
      if peek('?') then { advance(); Reluctant }
      else if peek('+') then { advance(); Possessive }
      else Greedy
    Quantified(body, Quant(min, max, mode))

  /** A `{` is a brace-quantifier opener iff the next characters are
    * `digits ('}' | ',' digits? '}')`. Anywhere else `{` is a literal
    * (this matches Onig's lenient default — many TextMate patterns rely
    * on it, e.g. literal `{` in code-fence delimiters). */
  private def hasBraceQuantifier: Boolean =
    if !peek('{') then return false
    var i        = pos + 1
    val numStart = i
    while i < input.length && input.charAt(i).isDigit do i += 1
    if i == numStart then return false
    if i < input.length && input.charAt(i) == ',' then
      i += 1
      while i < input.length && input.charAt(i).isDigit do i += 1
    i < input.length && input.charAt(i) == '}'

  private def parseBraceQuantifier(): (Int, Option[Int]) =
    expect('{')
    val min = readDigits()
    val max =
      if peek(',') then
        advance()
        if peek('}') then None else Some(readDigits())
      else Some(min)
    expect('}')
    if max.exists(_ < min) then fail(s"invalid quantifier range: $min > ${max.get}")
    (min, max)

  // ---- groups ----

  private def parseGroup(): Node =
    expect('(')
    if peek('?') then
      advance()
      peekChar match
        case ':' =>
          advance(); withSavedFlags { val b = parseAlt(); expect(')'); Group(NonCapturing, b) }
        case '>' =>
          advance(); withSavedFlags { val b = parseAlt(); expect(')'); Group(Atomic, b) }
        case '=' =>
          advance(); withSavedFlags { val b = parseAlt(); expect(')'); Lookaround(true, false, b) }
        case '!' =>
          advance(); withSavedFlags { val b = parseAlt(); expect(')'); Lookaround(true, true, b) }
        case '<' =>
          advance()
          if peek('=') then
            advance(); withSavedFlags { val b = parseAlt(); expect(')'); Lookaround(false, false, b) }
          else if peek('!') then
            advance(); withSavedFlags { val b = parseAlt(); expect(')'); Lookaround(false, true, b) }
          else withSavedFlags { parseNamedGroup() }
        case '#' => advance(); parseComment()
        case _   => parseInlineFlags()  // bare form may leak; scoped form saves/restores internally
    else withSavedFlags { parseCapturingGroup() }

  private def parseCapturingGroup(): Node =
    groupCounter += 1
    val idx  = groupCounter
    val body = parseAlt()
    expect(')')
    Group(Capturing(idx, None), body)

  /** Consume a `(?<name>body)`. The leading `(?<` is already consumed.
    * Duplicate names are accepted (Onig's branch-reset idiom uses
    * `(?<x>a)|(?<x>b)`); each occurrence allocates its own capture
    * index and joins the name-to-indices list. */
  private def parseNamedGroup(): Node =
    if peek('>') then fail("empty group name")
    val nameStart = pos
    while !atEnd && peekChar != '>' do advance()
    if atEnd then fail("unterminated group name")
    val name = input.substring(nameStart, pos)
    expect('>')
    groupCounter += 1
    val idx = groupCounter
    namedGroups.getOrElseUpdate(name, mutable.ListBuffer.empty) += idx
    val body = parseAlt()
    expect(')')
    Group(Capturing(idx, Some(name)), body)

  /** Consume a `(?#text)`. The leading `(?#` is already consumed. The
    * comment body is everything up to the first `)` — Onig comments
    * don't honour escapes, and don't nest. */
  private def parseComment(): Node =
    val start = pos
    while !atEnd && peekChar != ')' do advance()
    if atEnd then fail("unterminated comment")
    val text = input.substring(start, pos)
    advance() // ')'
    Comment(text)

  /** Consume `(?flags)` or `(?flags:body)`. The leading `(?` is already
    * consumed. Bare form (no body) returns `FlagsScope(set, clear, None)`
    * — the runtime applies it to the rest of the surrounding group;
    * scoped form returns `Some(body)` with flags restored on exit. */
  private def parseInlineFlags(): Node =
    var set: Flags    = Flags.Empty
    var clear: Flags  = Flags.Empty
    var negating      = false
    while !atEnd && peekChar != ':' && peekChar != ')' do
      peekChar match
        case '-' =>
          if negating then fail("duplicate '-' in flag list")
          negating = true
        case c @ ('i' | 'm' | 'x') =>
          val f = flagFromChar(c)
          if negating then clear = Flags.union(clear, f)
          else set = Flags.union(set, f)
        case other =>
          fail(s"unknown flag: '$other'")
      advance()
    if atEnd then fail("unterminated inline flag group")
    if peekChar == ')' then
      advance()
      // Bare form: mutate activeFlags so the rest of the surrounding
      // group sees the change. The enclosing parseGroup didn't wrap
      // us in withSavedFlags, so the leak survives until that
      // surrounding group's `)`.
      activeFlags = Flags.diff(Flags.union(activeFlags, set), clear)
      FlagsScope(set, clear, None)
    else
      advance() // ':'
      // Scoped form: save, apply, parse body under new flags, restore.
      val saved = activeFlags
      activeFlags = Flags.diff(Flags.union(activeFlags, set), clear)
      val body = parseAlt()
      expect(')')
      activeFlags = saved
      FlagsScope(set, clear, Some(body))

  private def flagFromChar(c: Char): Flags = c match
    case 'i' => Flags.IgnoreCase
    case 'm' => Flags.DotAll       // Onig's default-flavor m == dot-matches-newline
    case 'x' => Flags.Extended
    case _   => fail(s"unknown flag: '$c'")

  // ---- character classes ----

  private def parseCharClass(): Node =
    expect('[')
    val negated = peek('^') && { advance(); true }
    val set     = parseClassItems()
    if atEnd then fail("unterminated character class")
    expect(']')
    Klass(set, negated)

  /** The `[…]` body shared by the outer class and any nested class.
    * Onig treats `]` as literal only at the FIRST item position;
    * thereafter it terminates the class. */
  private def parseClassItems(): IntervalSet =
    var set   = IntervalSet.empty
    var first = true
    while !atEnd && (first || peekChar != ']') do
      val item = parseClassItem()
      set = set.union(item)
      first = false
    set

  /** Recurse into a `[...]` that appears inside another class. Onig
    * allows arbitrary nesting; `[[abc][def]]` is the union of its two
    * inner classes (i.e. equivalent to `[abcdef]`), and `[[^a]]` is
    * the inner class's complement contributed to the outer set. */
  private def parseNestedClass(): IntervalSet =
    expect('[')
    val negated = peek('^') && { advance(); true }
    val set     = parseClassItems()
    if atEnd then fail("unterminated character class")
    expect(']')
    if negated then set.complement else set

  /** Read one item from inside a `[...]`. Returns the contributing
    * [[IntervalSet]], which is later unioned into the running class set.
    * Items are: POSIX `[:name:]`, a nested `[...]` class, escaped class
    * shorthand, escaped literal, plain char, or a range whose endpoints
    * are single chars. */
  private def parseClassItem(): IntervalSet =
    if peekChar == '[' && pos + 1 < input.length && input.charAt(pos + 1) == ':' then
      return parsePosixClass()
    if peekChar == '[' then
      return parseNestedClass()
    parseClassChar() match
      case Right(set) => set
      case Left(cp) =>
        if peek('-') && pos + 1 < input.length && input.charAt(pos + 1) != ']' then
          advance() // consume '-'
          parseClassChar() match
            case Right(_) => fail("range end can't be a class shorthand")
            case Left(hi) =>
              if cp > hi then fail(s"invalid range: U+${cp.toHexString} > U+${hi.toHexString}")
              IntervalSet.range(cp, hi)
        else IntervalSet.single(cp)

  /** Inside-class char read. `Left(cp)` is a single-codepoint result that
    * may participate in a range; `Right(set)` is a multi-codepoint
    * shorthand or property class that cannot. */
  private def parseClassChar(): Either[Int, IntervalSet] =
    if peek('\\') then
      advance()
      if atEnd then fail("trailing backslash in char class")
      val c = peekChar
      c match
        case 'd' | 'D' | 'w' | 'W' | 's' | 'S' | 'h' | 'H' =>
          advance(); Right(shortcutSet(c))
        case 'p' | 'P' =>
          // Inside a class there's no per-element "negated" flag — the
          // \P{…} form contributes the complement of the property set
          // to the running union, so unioning multiple negated props or
          // mixing with positive ranges still gives the right answer.
          val negated = c == 'P'
          advance(); expect('{')
          val nameStart = pos
          while !atEnd && peekChar != '}' do advance()
          if atEnd then fail("unterminated property")
          val name = input.substring(nameStart, pos)
          expect('}')
          val s = propertySet(name)
          Right(if negated then s.complement else s)
        case 'n' => advance(); Left('\n'.toInt)
        case 't' => advance(); Left('\t'.toInt)
        case 'r' => advance(); Left('\r'.toInt)
        case 'f' => advance(); Left('\f'.toInt)
        case 'a' => advance(); Left(0x07)
        case 'e' => advance(); Left(0x1B)
        case 'b' => advance(); Left(0x08)            // \b inside class is backspace, not word boundary
        case 'x' => advance(); Left(parseHex())
        case _   => advance(); Left(c.toInt)
    else Left(readCodepoint())

  private def parsePosixClass(): IntervalSet =
    expect('[')
    expect(':')
    val negated   = peek('^') && { advance(); true }
    val nameStart = pos
    while !atEnd && peekChar != ':' do advance()
    if atEnd then fail("unterminated POSIX class")
    val name = input.substring(nameStart, pos)
    expect(':')
    expect(']')
    val set = posixSet(name)
    if negated then set.complement else set

  // ---- backslash escapes (outside char class) ----

  private def parseBackslash(): Node =
    expect('\\')
    if atEnd then fail("trailing backslash")
    val c = peekChar
    c match
      case 'A' => advance(); Anchor(AnchorKind.BeginInput)
      case 'z' => advance(); Anchor(AnchorKind.EndInput)
      case 'Z' => advance(); Anchor(AnchorKind.EndInputBeforeFinalNewline)
      case 'G' => advance(); Anchor(AnchorKind.PrevMatchEnd)
      case 'b' => advance(); Anchor(AnchorKind.WordBoundary)
      case 'B' => advance(); Anchor(AnchorKind.NonWordBoundary)
      case 'd' | 'D' | 'w' | 'W' | 's' | 'S' | 'h' | 'H' =>
        advance(); Klass(shortcutSet(c), negated = false)
      case 'p' | 'P' =>
        val negated = c == 'P'
        advance(); expect('{')
        val nameStart = pos
        while !atEnd && peekChar != '}' do advance()
        if atEnd then fail("unterminated property")
        val name = input.substring(nameStart, pos)
        expect('}')
        Klass(propertySet(name), negated = negated)
      case d if d.isDigit && d != '0' =>
        val n = readDigits()
        Backref(GroupRef.ByNumber(n))
      case 'k' =>
        advance(); expect('<')
        val ref = readGroupRef('>')
        expect('>')
        Backref(ref)
      case 'g' =>
        advance(); expect('<')
        val ref = readGroupRef('>')
        expect('>')
        SubroutineCall(ref)
      case 'n' => advance(); Literal("\n")
      case 't' => advance(); Literal("\t")
      case 'r' => advance(); Literal("\r")
      case 'f' => advance(); Literal("\f")
      case 'a' => advance(); Literal("")
      case 'e' => advance(); Literal("")
      case 'x' =>
        advance()
        val cp = parseHex()
        Literal(new String(Character.toChars(cp)))
      case _   => advance(); Literal(c.toString)

  /** Read the body of `\k<...>` or `\g<...>`. Number forms consume the
    * full digit run (so `\k<10>` resolves to group 10, not `\k<1>` then
    * `0` literal). Relative forms `+N` / `-N` resolve to absolute group
    * indices at parse time using the running [[groupCounter]]: `-N` maps
    * to `groupCounter - N + 1` (so `-1` is the most-recently-opened
    * group), and `+N` maps to `groupCounter + N` (a forward reference to
    * the Nth group still to be opened). Negative offsets that would
    * resolve to a group ≤ 0 fail at parse time with a clear message;
    * out-of-range positive offsets fall through to the compiler's
    * "undefined group" check, which already reports what's available. */
  private def readGroupRef(terminator: Char): GroupRef =
    if peek(terminator) then fail("empty group reference")
    val c = peekChar
    c match
      case '+' | '-' =>
        val sign = if c == '+' then 1 else -1
        advance()
        val n = readDigits()
        if n == 0 then fail("relative group reference with offset 0")
        val absolute = if sign < 0 then groupCounter - n + 1 else groupCounter + n
        if absolute <= 0 then
          fail(s"relative group reference points before group 1 (offset -$n at group $groupCounter)")
        GroupRef.ByNumber(absolute)
      case d if d.isDigit =>
        val n = readDigits()
        GroupRef.ByNumber(n)
      case _ =>
        val start = pos
        while !atEnd && peekChar != terminator do advance()
        if atEnd then fail("unterminated group reference")
        GroupRef.ByName(input.substring(start, pos))

  /** `\xNN` (exactly two hex digits) or `\x{HEX...}` (any length, used
    * for supplementary codepoints). */
  private def parseHex(): Int =
    if peek('{') then
      advance()
      val start = pos
      while !atEnd && peekChar != '}' do advance()
      if atEnd then fail("unterminated \\x{...}")
      val hex = input.substring(start, pos)
      expect('}')
      try Integer.parseInt(hex, 16)
      catch case _: NumberFormatException => fail(s"bad hex value: \\x{$hex}")
    else
      if pos + 1 >= input.length then fail("\\x needs two hex digits")
      val s = input.substring(pos, pos + 2)
      if !s.forall(isHexDigit) then fail(s"bad hex value: \\x$s")
      pos += 2
      Integer.parseInt(s, 16)

  private def isHexDigit(c: Char): Boolean =
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

  // ---- low-level cursor helpers ----

  private[oniguruma] def atEnd: Boolean         = pos >= input.length
  private[oniguruma] def peekChar: Char         = input.charAt(pos)
  private[oniguruma] def peek(c: Char): Boolean = !atEnd && input.charAt(pos) == c
  private def advance(): Unit                   = pos += 1
  private def expect(c: Char): Unit =
    if !peek(c) then fail(s"expected '$c'")
    advance()
  private def readDigits(): Int =
    val start = pos
    while !atEnd && peekChar.isDigit do advance()
    if pos == start then fail("expected digits")
    input.substring(start, pos).toInt
  private def readCodepoint(): Int =
    val cp = Character.codePointAt(input, pos)
    pos += Character.charCount(cp)
    cp
  private def fail(msg: String): Nothing =
    throw ParseException(ParseError(msg, pos))

  // ---- shortcut + POSIX + property sets ----

  private inline def cr(lo: Char, hi: Char): (Int, Int) = (lo.toInt, hi.toInt)

  private val DigitSet: IntervalSet = IntervalSet.range('0', '9')
  private val WordSet: IntervalSet =
    IntervalSet.of(cr('0', '9'), cr('A', 'Z'), cr('a', 'z'), cr('_', '_'))
  private val SpaceSet: IntervalSet =
    IntervalSet.fromCodepoints(Seq(0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x20))
  private val HexSet: IntervalSet =
    IntervalSet.of(cr('0', '9'), cr('A', 'F'), cr('a', 'f'))

  /** Resolve `\d \D \w \W \s \S \h \H` to its [[IntervalSet]]. All ASCII
    * for now; the corpus doesn't ask for Unicode-aware shorthands. */
  private def shortcutSet(c: Char): IntervalSet = c match
    case 'd' => DigitSet
    case 'D' => DigitSet.complement
    case 'w' => WordSet
    case 'W' => WordSet.complement
    case 's' => SpaceSet
    case 'S' => SpaceSet.complement
    case 'h' => HexSet
    case 'H' => HexSet.complement

  /** ASCII-defined POSIX classes. The corpus exercises only the seven
    * actually used by TextMate grammars; we expand the full set anyway
    * because the cost is just a few [[IntervalSet]] literals. */
  private def posixSet(name: String): IntervalSet = name match
    case "alnum"  => IntervalSet.of(cr('0', '9'), cr('A', 'Z'), cr('a', 'z'))
    case "alpha"  => IntervalSet.of(cr('A', 'Z'), cr('a', 'z'))
    case "digit"  => DigitSet
    case "upper"  => IntervalSet.range('A', 'Z')
    case "lower"  => IntervalSet.range('a', 'z')
    case "xdigit" => HexSet
    case "word"   => WordSet
    case "space"  => SpaceSet
    case "punct"  =>
      IntervalSet.of(cr(0x21, 0x2F), cr(0x3A, 0x40), cr(0x5B, 0x60), cr(0x7B, 0x7E))
    case "cntrl"  => IntervalSet.of(cr(0x00, 0x1F), cr(0x7F, 0x7F))
    case "print"  => IntervalSet.range(0x20, 0x7E)
    case "graph"  => IntervalSet.range(0x21, 0x7E)
    case "blank"  => IntervalSet.of(cr(0x09, 0x09), cr(0x20, 0x20))
    case "ascii"  => IntervalSet.range(0x00, 0x7F)
    case _        => fail(s"unknown POSIX class: [:$name:]")

  /** Resolve a Unicode property name to an [[IntervalSet]]. Stage 5
    * supports the four general categories the TextMate corpus actually
    * uses (L, M, N, Print); resolution is delegated to
    * [[UCDProperty]], which builds them lazily from the host runtime's
    * `Character.getType` tables on first use and memoizes thereafter.
    * Properties outside the supported set fail outright so unknown
    * `\p{Foo}` doesn't silently match nothing. */
  private def propertySet(name: String): IntervalSet = name match
    case "L"     => UCDProperty.Letter
    case "M"     => UCDProperty.Mark
    case "N"     => UCDProperty.Number
    case "Print" => UCDProperty.Print
    case other   => fail(s"unsupported Unicode property: \\p{$other}")

  // ---- post-processing ----

  /** Collapse the parsed atom list into the canonical AST shape: empty →
    * `Empty`, single → that single, otherwise `Concat`. Adjacent
    * unquantified [[Literal]] atoms are folded into one literal so the
    * tree stays small and pretty-prints cleanly. */
  private def foldAndCollapse(items: List[Node]): Node =
    val folded = foldLiterals(items)
    folded match
      case Nil          => Empty
      case one :: Nil   => one
      case many         => Concat(many)

  private def foldLiterals(items: List[Node]): List[Node] =
    val out = mutable.ListBuffer.empty[Node]
    val sb  = StringBuilder()
    for item <- items do
      item match
        case Literal(s) => sb.append(s)
        case other =>
          if sb.nonEmpty then { out += Literal(sb.toString); sb.clear() }
          out += other
    if sb.nonEmpty then out += Literal(sb.toString)
    out.toList

end Parser
