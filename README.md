# vibeparser 😎🤙🏻🔥

A tiny, pleasant-to-use **declarative** parsing mini-library with a compact pattern DSL, per-alternative precedence, limited left-recursive operator support, and convenient repetition/quantifier syntax.

**Goal**: make it easy and pleasant to write small grammars in Python using a compact pattern DSL combined with Python semantic functions.

**Best for**: education, experiments, quick prototypes, parsers for small config languages, calculators, tiny DSLs.

> [!WARNING]
> **This library was entirely written by an LLM.** It *appears* to work and the examples run in my tests, but treat it as experimental research/learning code — not something to deploy in production. Use at your own risk; if it eats your grammar, try again with coffee ☕. (Also: it may be charmingly verbose about errors.)

---

## Installation

I'm not planning on uploading **vibeparser** to PyPi, so if you want to install, run the following:

```bash
pip install git+https://github.com/samedit66/vibeparser.git
```

---

## Quick example — calculator

```py
from math import prod
from vibeparser import Grammar, token, rule

class Calculator(Grammar):
    # token: numbers will be captured and converted to int
    number = token(r"\d+", int)

    # postfix factorial: high precedence (unary postfix)
    @rule("<expr:x> '!'", prec=40, unary="postfix")
    def expr_postfix_fact(self, x):
        # x is the evaluated expression to the left (an int)
        return prod(range(1, int(x) + 1))

    # additive operators (left-assoc)
    @rule("<expr:x> '+' <expr:y>", prec=10, assoc="left")
    def expr_add(self, x, y):
        return x + y

    @rule("<expr:x> '-' <expr:y>", prec=10, assoc="left")
    def expr_sub(self, x, y):
        return x - y

    # multiplicative operators (higher precedence)
    @rule("<expr:x> '*' <expr:y>", prec=20, assoc="left")
    def expr_mul(self, x, y):
        return x * y

    @rule("<expr:x> '/' <expr:y>", prec=20, assoc="left")
    def expr_div(self, x, y):
        return x / y

    # unary prefix minus
    @rule("'-' <expr:x>", prec=30, unary="prefix")
    def expr_neg(self, x):
        return -x

    # numbers and parentheses
    @rule("<number:n>")
    def expr_number(self, n):
        return n

    @rule("'(' <expr:val> ')'")
    def expr_paren(self, val):
        return val

    # top rule — entry point
    @rule("<expr:res>")
    def top(self, res):
        return res

if __name__ == "__main__":
    calc = Calculator()
    tests = [
        "1 + 2 * 3",    # 7
        "-1 + 4",       # 3
        "2 * 3 + 4",    # 10
        "2 * (3 + 4)",  # 14
        "3! + 1",       # 7
        "5! / 5",       # 24.0
    ]
    for t in tests:
        print(t, "->", calc.parse(t))
```

See [examples](./examples) folder for other grammars like [JSON](./examples/json_grammar.py) or [TinyBasic](./examples/tiny_basic.py).

---

## Features 💎

This section describes what makes vibeparser enjoyable and useful.

- **Compact pattern DSL.** Write patterns inside the `@rule(...)` decorator using a readable, small syntax for literals, references, groups and quantifiers.
- **Rule name on the left (`::=`).** A pattern may start with an explicit left-hand rule name: `"<expr> ::= <expr:left> '+' <expr:right>"`. When present this name overrides the decorator `name=` and function-name heuristics.
- **Flexible captures.** Use `<NAME:var>` to capture and name a submatch. Optional references (`?`), zero-or-more (`*`) and one-or-more (`+`) quantifiers inside `<...>` are supported, and default values for optional refs are supported: `<id?:name='anon'>`.
- **Anonymous (positional) captures.** `@rule(..., capture_anon=True)` causes submatches without explicit `:var` to be captured and passed positionally to your semantic action (handy for short rules and operator forms).
- **Transform & validate hooks.** `@rule(..., transform=..., validate=...)` lets you normalize or reject an alternative *before* calling the semantic function. Returning `None` from `transform` or `False` from `validate` makes the alt behave as if it didn't match.
- **Smart tokens.** `token(pattern, ...)` auto-decides whether to wrap a token with `\b...\b` using heuristics that avoid breaking punctuation or exponent-number forms. New `capture_by_default` supports tokens that capture even when referenced without `:var`.
- **Group sugar for separators.** A common pattern like `[',' <expr>] :rest` is treated intelligently: the separator is ignored in group captures and you get a convenient list of captured items (no manual filtering required).
- **Prefix/postfix unary forms.** Define unary operators by marking an alternative with `unary='prefix'` or `unary='postfix'` so the engine treats them correctly when mixing with binary operators.
- **Per-alternative precedence and associativity.** `prec=` and `assoc=` (left/right) can be provided per alternative to express operator precedence cleanly.
- **Limited left-recursive operator support.** The engine supports left-recursive growth with guarded memoization and per-alternative precedence. This enables natural expression grammars like `expr + expr * expr` while avoiding infinite recursion.
- **Conservative memoization (per-parse).** A memo cache keyed on `(rule_name, pos, min_prec)` speeds repeated work but preserves left-recursion semantics.
- **Linter warnings.** The pattern parser detects likely mistakes (e.g. named group capturing unnamed tokens with `capture_by_default=True`, nested named captures inside groups, unclosed groups) and emits `UserWarning`s to help you find fragile patterns.
- **Improved error messages.** `ParseError` includes line/column, a snippet with caret, and a short `Found: '...'` lookahead and `Expected:` hints.
- **Robust literal quoting.** Single-quoted literals use SQL-style doubling for embedded single quotes (`''` → `'`). Double-quoted literals accept backslash escapes.
- **Default values in references.** Optional references can specify a default value (Python literal syntax attempted), e.g. `<NAME?:v=42>`.
- **Group repetition capture semantics.** `[ ... ]:name` (zero-or-more) and `{ ... }:name` (one-or-more) produce lists of items where each item is either a single capture or a tuple of captures depending on what the inner sequence produced.

---

## Pattern syntax (complete summary)

Use the compact pattern language inside `@rule("...")` strings.

- **Literals**: `'...'` or `"..."` — literal text. Single-quoted literals use SQL-style doubling for `'` inside the literal: `'''` becomes `'` in the value. Double-quoted literals accept backslash escapes (`\n`, `\\`, `\'`, `\"`, etc.). Literals may include spaces and punctuation.

- **References**: `<NAME:var>` — reference to a token or rule `NAME`. When `:var` is present the matched value is passed to the semantic function as that positional argument.

- **Optional**: `<NAME?:var>` — optional reference. If missing and `:var` provided the variable receives `None` (or the default value if specified). You can also write `<NAME?:var=DEFAULT>` where `DEFAULT` is attempted to be parsed as a Python literal.

- **Repetition inside refs**: `<NAME*:xs>` — zero-or-more occurrences of a single reference. Named capture yields a list (possibly empty). `<NAME+:xs>` — one-or-more occurrences; named capture yields a non-empty list.

- **Groups (sequence repetition)**:
  - `[ ... ]:name` — zero-or-more repetition of the inner sequence; if `:name` is present you receive a list where each item is either a single value or a tuple of values depending on the inner captures.
  - `{ ... }:name` — one-or-more repetition (same capture semantics, but fails if zero occurrences).

- **Group sugar for separators**: a common shorthand like `[',' <expr>] :rest` is recognized by the parser. If the inner sequence is exactly `SEP ITEM` and `ITEM` is capturable, `rest` becomes the list of ITEM captures while separators are ignored.

- **Rule name on the left**: `"<rule> ::= ..."` — explicitly name the pattern's rule. This name takes precedence over decorator `name=` or function name heuristics.

- **Anonymous captures**: `@rule(..., capture_anon=True)` will capture unnamed references (those without `:var`) and provide them positionally to the semantic action. This affects inner groups and repetitions accordingly.

- **Examples**:
  - `@rule("<expr:x> [',' <expr:y>]:rest")` → `x` is the first expression, `rest` is a list of following expressions.
  - `@rule("<id?:name='anon'>")` → `name` will either be the matched id or the default `'anon'`.
  - `@rule("<num*:ns>")` → `ns` becomes a list of numbers (maybe empty).
  - `@rule("<assignment> ::= <varname> '=' <expr>", capture_anon=True)` → the semantic function receives positional args for `varname` and `expr`.

---

## token() details and heuristics

`token(pattern, convert=None, with_word_boundaries=None, ignore_case=False, capture_by_default=False)` creates a lexical token.

- When `with_word_boundaries` is omitted the library uses heuristics to decide whether to wrap the regex in `\b...\b`:
  - If the pattern *starts* with an alphanumeric character it's likely a word-like token → wrap with `\b`.
  - If the pattern looks purely punctuation/operator-like (after removing escape sequences) do **not** wrap.
  - If the pattern contains alphabetic letters but doesn't start with alphanumeric (rare), be conservative and do not wrap.

- `capture_by_default=True` causes an unnamed reference `<TOKEN>` (without `:var`) to still produce a capture. This is convenient for simple token rules like `COMMA = token(',', capture_by_default=True)` where you want separators to sometimes be captured by default.

- `ignore_case=True` will compile the token regex case-insensitively.

---

## Gotchas — common mistakes and probably-wrong grammars

This library is expressive, but several patterns commonly trip people up. Here are gotchas and tips to avoid surprises.

1. **Relying on unnamed token captures accidentally.**
   - If you set `capture_by_default=True` for a token, any unnamed reference to that token inside a named group may accidentally capture values into the group's list. The library emits a warning for this case — heed it.

2. **Nested named captures inside a named group.**
   - Writing `[ SEP <ITEM:name> ]:list` will create nested named captures (a group's named `:list` and the inner `<ITEM:name>`), which is usually not what you want. The linter will warn; prefer `[ SEP <ITEM> ]:list` and capture the inner item via `capture_by_default` on the token if needed, or remove the inner `:name` and handle naming at the group level.

3. **Zero-length matches in repetition.**
   - Be cautious when writing patterns or tokens that can match the empty string. Repetition constructs (`*`, `+`, group loops) will try to prevent infinite loops by checking for progress, but semantic behavior becomes confusing. Avoid zero-width tokens.

4. **Mistaking literal quoting rules.**
   - Single-quote doubling (`''`) is used, not backslash escapes. If you need backslashes in a literal, prefer double quotes.

5. **Overly-complex transforms.**
   - `transform` is a powerful hook but should stay simple (normalization / small pre-checks). Heavy logic or raising exceptions inside `transform` will be surfaced as parse errors with less obvious stack traces.

6. **Expecting a missing `top` rule to work.**
   - The engine requires a `top` rule to be defined (unless you explicitly supply `start_rule` to `parse`). Define `@rule("<your_start:res>") def top(self, res): ...` as the entry point.

7. **Assuming left-recursive constructs are unlimited.**
   - Left-recursive operator support is limited to operator-style growth that the engine's algorithm recognizes. Highly-ambiguous or unconstrained left recursion can still fail or be slow.

8. **Confusing `capture_anon` and named args.**
   - If you enable `capture_anon=True`, the function will receive positional arguments for unnamed refs. If the semantic function signature expects named keyword args, mismatch errors will happen; prefer explicit `:var` when in doubt.

9. **Wrong use of defaults that aren't valid Python literals.**
   - `<NAME?:v=foo>` will attempt to parse `foo` as a Python literal; if parsing fails the raw string `foo` will be used. Prefer quoted defaults when the default is a string: `<id?:name='anon'>`.

---

## Debugging tips

- When a parse fails, read the `ParseError` — it shows line/column, code snippet, a caret and a short `Found:` preview of the input at the failure point.
- Enable `warnings` in your test harness (`import warnings; warnings.simplefilter('always')`) to see pattern-linter messages while iterating on grammar design.
- Keep semantic actions small during debugging — validate/transform hooks are useful to reject invalid choices close to the grammar and get clearer failures.

---

## Design notes & why some choices were made

- The pattern DSL prioritizes readability and short grammars over full generality. It intentionally mixes lexical and grammar references inside the same notation for convenience.
- The left-recursive growth algorithm is conservative and paired with per-parse memoization to avoid extremely subtle infinite-recursion bugs while still supporting usual expression grammars.
- The library deliberately emits `UserWarning`s for fragile or ambiguous pattern constructs — early feedback beats silently wrong parsers.

---

## Contributing & examples

This is an experimental library. If you copy it into a project and improve something, please add an `examples/` folder with small grammars (calculator, JSON, tiny-basic) so others can iterate.
