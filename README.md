# purekell

Bidirectional Haskell/PureScript expression translator. Parses expressions from either language into a shared AST, then prints them back in the target language with correct syntax.

Built to complement [purescript-bridge](https://hackage.haskell.org/package/purescript-bridge) — while purescript-bridge generates PureScript *types* from Haskell types, purekell translates *expressions* (specifically typeclass instance method bodies).

## Usage

```haskell
import Purekell

-- Translate expressions between languages
hsToPs "uid x == uid y"           -- Right "x.uid == y.uid"
psToHs "x.uid == y.uid"           -- Right "uid x == uid y"

-- Parse and print instance method bodies
import Purekell.Instance

parseMethodBody "eq x y = x == y" -- Right [MethodEquation ...]
printMethodBody Haskell eqs       -- "eq x y = x == y"
printMethodBody PureScript eqs    -- "eq x y = x == y"
```

## Supported syntax

Expressions: literals (int, float, char, string), variables, constructors, application, infix operators (symbolic and backtick), lambda, if/then/else, case/of, let/in, do notation, negation, tuples, lists, operator sections, where clauses, type annotations, record construction/update, qualified names.

Patterns: variable, constructor, literal, wildcard, tuple, list, cons, as-pattern, negated literal, record.

Types: constructors, variables, application, function arrows, qualified constructors.

## Divergent syntax

Most syntax is shared between Haskell and PureScript. The printer handles these divergences:

| Construct | Haskell | PureScript |
|---|---|---|
| Record access | `field rec` | `rec.field` |
| Tuples | `(a, b, c)` | `Tuple a (Tuple b c)` |
| Cons patterns | `x : xs` | `Cons x xs` |
| Record construction | `Foo { bar = 1 }` | `Foo { bar: 1 }` |
| Record patterns | `Foo { bar = x }` | `Foo { bar: x }` |

## Architecture

- **`Purekell.AST`** — Shared AST types (`Expr`, `Pat`, `Type`, `Binding`, etc.)
- **`Purekell.Parser`** — Megaparsec-based parser, parameterized for language-specific postfix (e.g. PS dot access)
- **`Purekell.Printer`** — Target-aware printer with correct parenthesization
- **`Purekell.Codec`** — `Codec a` pairs a parser with a printer; provides `runParse`, `runPrint`, `roundtrip`
- **`Purekell.Haskell`** / **`Purekell.PureScript`** — Language-specific codec instances
- **`Purekell.Instance`** — Parsing/printing of typeclass method equations (`name pat1 pat2 = body`)

## Testing

```bash
cd purekell && stack test
```

331 tests covering parse, print, roundtrip (Haskell, PureScript, cross-language), and property-based roundtrip tests using QuickCheck with `Arbitrary` instances for all AST types.
