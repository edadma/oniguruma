package io.github.edadma.oniguruma

/** Regex AST nodes. The shape covers every Oniguruma feature used by the
  * TextMate grammar corpus: groups (capturing/non-capturing/named/atomic),
  * lookaround, atomic + possessive quantifiers, backrefs, subroutine calls,
  * inline flag scopes, char classes via [[IntervalSet]], and the full
  * anchor set including `\G`. Conditional groups, the absent operator,
  * char-class intersection, `\K`, `\X`, and `\Q…\E` are intentionally
  * absent — none of the 36-grammar corpus uses them. */
sealed trait Node

object Node:

  /** Empty alternative or zero-length sub-pattern. */
  case object Empty extends Node

  /** Literal text. Multi-char only when neighbouring chars share no special
    * adornment; the parser may emit single-char [[Literal]]s where needed. */
  final case class Literal(text: String) extends Node

  /** Concatenation of sub-patterns. Always at least two children; the parser
    * collapses singletons before constructing this. */
  final case class Concat(items: List[Node]) extends Node

  /** Top-level alternation `a|b|c`. */
  final case class Alt(branches: List[Node]) extends Node

  /** A quantifier applied to a sub-pattern. */
  final case class Quantified(body: Node, q: Quant) extends Node

  /** `.` — matches one codepoint. The interpretation of "what counts as a
    * line terminator" is decided at compile time from the active flags;
    * the AST stays neutral. */
  case object Dot extends Node

  /** Character class. The [[IntervalSet]] holds the positive set; if
    * [[negated]] is true the class matches the complement (relative to the
    * full Unicode codepoint range). POSIX classes, `\d \w \s \h` etc. and
    * `\p{…}` properties are all expanded to interval sets at parse time. */
  final case class Klass(set: IntervalSet, negated: Boolean) extends Node

  /** A group. The kind decides whether captures happen, whether subroutine
    * calls and backrefs can refer to it, and whether backtracking can re-enter
    * after exiting (atomic groups can't). */
  final case class Group(kind: GroupKind, body: Node) extends Node

  /** Lookaround: `(?=…)`, `(?!…)`, `(?<=…)`, `(?<!…)`. */
  final case class Lookaround(forward: Boolean, negative: Boolean, body: Node) extends Node

  /** Numeric or named backreference: `\1`-`\9`, `\k<name>`, `\k<n>`,
    * `\k<-1>`, `\k<+1>`. */
  final case class Backref(ref: GroupRef) extends Node

  /** Subroutine call / pattern recursion: `\g<name>`, `\g<n>`, `\g<0>`,
    * `\g<-1>`, `\g<+1>`. `\g<0>` is "the whole pattern" (recursion). */
  final case class SubroutineCall(ref: GroupRef) extends Node

  /** Anchor of zero width — see [[AnchorKind]]. */
  final case class Anchor(kind: AnchorKind) extends Node

  /** A scoped change of the active flags. Body is `None` for the bare
    * `(?im-x)` form (which leaks its effect to the rest of the surrounding
    * group); `Some(body)` for `(?i:…)` which restores flags on exit. */
  final case class FlagsScope(set: Flags, clear: Flags, body: Option[Node]) extends Node

  /** `(?#comment)` — preserved in the AST so we can round-trip pretty-print. */
  final case class Comment(text: String) extends Node

end Node

/** Quantifier `{min,max}` plus mode. `max == None` means unbounded. */
final case class Quant(min: Int, max: Option[Int], mode: QuantMode)

sealed trait QuantMode
case object Greedy     extends QuantMode
case object Reluctant  extends QuantMode
case object Possessive extends QuantMode

/** Group-kind discriminator. The capture index is assigned during parsing —
  * non-capturing and atomic groups get index `-1`. Named groups also have
  * a numeric index, since `\k<name>` and `\1` may both refer to the same
  * group. */
sealed trait GroupKind
object GroupKind:
  /** `(?:…)` */
  case object NonCapturing extends GroupKind
  /** `(…)` and `(?<name>…)` */
  final case class Capturing(index: Int, name: Option[String]) extends GroupKind
  /** `(?>…)` */
  case object Atomic extends GroupKind

/** A reference to a group, used both by backrefs and subroutine calls. */
sealed trait GroupRef
object GroupRef:
  /** `\1`, `\k<3>`, `\g<3>` */
  final case class ByNumber(n: Int) extends GroupRef
  /** `\k<name>`, `\g<name>` */
  final case class ByName(name: String) extends GroupRef
  /** `\k<-1>` (one back), `\g<+2>` (two ahead). The reference resolves at
    * compile time, given the parser's running group counter. */
  final case class ByRelative(offset: Int) extends GroupRef

/** Zero-width anchors. `Caret` and `Dollar` are line-flag-sensitive at
  * compile time; `BeginInput`/`EndInput*` are absolute regardless of flags;
  * `PrevMatchEnd` is the TextMate-critical `\G` anchor; word-boundary checks
  * use the flags-resolved word-character set. */
sealed trait AnchorKind
object AnchorKind:
  /** `^` — beginning of input, or beginning of line under multiline. */
  case object Caret extends AnchorKind
  /** `$` — end of input, or end of line under multiline. */
  case object Dollar extends AnchorKind
  /** `\A` — always beginning of input, ignoring multiline. */
  case object BeginInput extends AnchorKind
  /** `\z` — always end of input. */
  case object EndInput extends AnchorKind
  /** `\Z` — end of input, optionally allowing one trailing newline. */
  case object EndInputBeforeFinalNewline extends AnchorKind
  /** `\G` — at the end of the previous match, or start of input on the
    * very first match attempt. The TextMate scanner depends on this. */
  case object PrevMatchEnd extends AnchorKind
  /** `\b` — between word and non-word codepoints (or at input boundaries
    * when the adjacent codepoint is word). */
  case object WordBoundary extends AnchorKind
  /** `\B` — exactly where `\b` does not match. */
  case object NonWordBoundary extends AnchorKind

/** Flag bits — packed into an [[Int]] so set/clear masks compose with `|`.
  * Kept as an opaque type so a stray `Int` can't be passed where flags are
  * expected; all the actual bit ops happen inside the companion (where the
  * alias is transparent) via named methods to avoid the recursive-operator
  * trap that the `extension def |` form would create. */
opaque type Flags = Int
object Flags:
  /** No flags set. */
  val Empty: Flags = 0

  /** `(?i)` — case-insensitive matching. */
  val IgnoreCase: Flags = 1 << 0

  /** `(?m)` in Onig's default flavor — "dot-matches-newline". (Java's `m`
    * flag is what we call [[MultiLine]] below; the parser disambiguates
    * based on the active syntax flavor before raising a [[FlagsScope]].) */
  val DotAll: Flags = 1 << 1

  /** `(?x)` — extended mode: ignore whitespace and `# comments`. */
  val Extended: Flags = 1 << 2

  /** Line-mode `^`/`$` — match at every newline, not just at input ends. */
  val MultiLine: Flags = 1 << 3

  /** Bitwise OR (set union). */
  def union(a: Flags, b: Flags): Flags = a | b

  /** Bitwise AND (set intersection). */
  def intersect(a: Flags, b: Flags): Flags = a & b

  /** Drop the bits in `mask` from `a`. */
  def diff(a: Flags, mask: Flags): Flags = a & ~mask

  /** Membership: is every bit of `bit` set in `a`? */
  def has(a: Flags, bit: Flags): Boolean = (a & bit) == bit && bit != 0

  /** Round-trip access to the underlying packed int — for tests and
    * pretty-printing only. The compiler/VM should never need this. */
  def asInt(a: Flags): Int = a

  /** Cast a raw int back to [[Flags]] — paired with [[asInt]] for tests. */
  def fromInt(i: Int): Flags = i

  extension (a: Flags)
    /** Convenient `flags hasFlag IgnoreCase` test. */
    infix def hasFlag(bit: Flags): Boolean = has(a, bit)
end Flags
