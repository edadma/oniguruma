# oniguruma

A Scala port of the Oniguruma regular-expression engine, designed to support
every regex feature used by [TextMate-flavor language grammars][tm]. Cross-built
for the JVM, Scala.js, and Scala Native.

[tm]: https://macromates.com/manual/en/regular_expressions

## Status

**Working** end-to-end. The engine parses, compiles, and matches every
feature on the must-support list below. The 36-grammar TextMate corpus
compiles at **100.0%** (3418 / 3418 patterns), and 100.0% of the
compiled patterns survive a smoke-run through the VM without throwing.

Stage roadmap:

| # | Scope | State |
|---|---|---|
| 1 | Scaffold + AST + `IntervalSet` + `Flags` + corpus loader | done |
| 2 | Parser covering every feature on the must-support list | done |
| 3 | Bytecode IR + compiler + VM (basic alternation / quantifiers / captures / anchors) | done |
| 4 | Lookaround + atomic + possessive + backrefs + `\G` + inline flags | done |
| 5 | Subroutines + recursion + UCD `\p{L,M,N,Print}` + grammar validation | done |
| 6.A | `TmScanner` multi-pattern driver (TextMate-grammar-shaped API) | done |
| 6.B | Capture propagation through positive lookaround | done |
| 6.C | Relative refs (`\k<-N>`, `\g<+N>`) resolved at parse time | done |
| 6.D | Empty-body loop progress tracking (replaces 10M `StepLimit` blunt cap) | done |
| 6.E | Onig-classic `\g<n>` capture semantics | optional (only on demand) |
| 6.F | Out-of-range numeric backrefs accepted at parse, always-fail at runtime (Onig-compat) | done |

## Scope

The library targets the **default Onig syntax flavor** as used by TextMate
grammars — not the full multi-syntax matrix supported by upstream Oniguruma.
Scope was set by mining the actual feature set used across 36 real grammars
(`~/dev/juicer/docs/grammars/`); the corpus contains 5,239 unique regex
patterns and is bundled as a JVM test resource for validation.

**Supported** (every feature that appears in the corpus):

- alternation, all quantifiers (greedy / reluctant / possessive), `{n,m}`
- groups: capturing, non-capturing `(?:…)`, named `(?<name>…)`, atomic `(?>…)`
- all four lookarounds, including variable-length lookbehind
- char classes, POSIX brackets (`[[:alpha:]]` etc.), shorthands (`\d \w \s \h`)
- anchors: `^ $ \A \z \Z \G \b \B`
- subroutines: `\g<name>`, `\g<n>`, `\g<0>` (whole-pattern recursion)
- backrefs: `\N` (any positive N), `\k<name>`, `\k<n>`, `\k<-1>` (relative); out-of-range numeric refs compile cleanly and behave as Onig's "always-fail uncaptured group" at runtime
- inline flags `(?i:…)`, `(?im-x:…)`, comments `(?#…)`
- Unicode properties: `\p{L}`, `\p{M}`, `\p{N}`, `\p{Print}`, plus `\P{…}` negation

**Intentionally not supported** (zero occurrences in the corpus):

- conditional groups `(?(…))`
- absent operator `(?~…)`
- char-class intersection `[a&&b]`
- `\K`, `\X`, `\Q…\E`
- `\v\V`, `\uNNNN`, `\cX`, octal escapes
- single-quote group/backref forms `(?'n'…)`, `\k'n'`, `\g'n'`
- multi-encoding pluggability (UTF-16 char input only)

## Building

```sh
sbt onigurumaJVM/test         # JVM tests, including the corpus loader
sbt onigurumaJS/test          # Scala.js tests
sbt onigurumaNative/test      # Scala Native tests
```

### Scala Native version

The published Native artifact is built against **`sbt-scala-native` 0.5.11**
(and uses 0.5.11's `javalib`, which references symbols like
`java.lang.AbstractStringBuilder` that aren't present in 0.5.10's
`javalib`). A downstream library pinned to 0.5.10 that consumes
oniguruma will fail to link with a `"Bad symbolic reference"` error
— bump the consumer to 0.5.11 to resolve.

## Regenerating the corpus

The corpus lives at `jvm/src/test/resources/textmate-corpus.txt`. To
regenerate it from a directory of `.tmLanguage.json` grammars:

```sh
tools/regen-corpus.sh                                # uses ~/dev/juicer/docs/grammars
tools/regen-corpus.sh /path/to/grammars              # explicit dir
```

Update `CorpusSpec`'s expected count if the corpus size changes.
