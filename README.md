# grammar

Tiny, pleasant-to-use, declarative parsing mini-library that combines a small pattern DSL with per-rule precedence and limited left-recursion handling. It’s intended for small DSLs and teaching, **not** as a production-grade parser generator.

# Installation

I'm not planning on uploading `grammar` to **PyPi**, so if you want to install, run the following:

```bash
pip install ...
```

# Example

```python
from math import prod

from grammar import (
    Grammar,
    token,
    rule,
)


class Calculator(Grammar):
    number = token(r"\d+", int)

    @rule("<expr:x> '!'", prec=40, unary="postfix")
    def expr_postfix_inc(self, x):
        return prod(range(1, x + 1))

    @rule("<expr:x> '+' <expr:y>", prec=10, assoc="left")
    def expr_add(self, x, y):
        return x + y

    @rule("<expr:x> '-' <expr:y>", prec=10, assoc="left")
    def expr_sub(self, x, y):
        return x - y

    @rule("<expr:x> '*' <expr:y>", prec=20, assoc="left")
    def expr_mul(self, x, y):
        return x * y

    @rule("'-' <expr:x>", prec=30, unary="prefix")
    def expr_neg(self, x):
        return -x

    @rule("<number:n>")
    def expr_number(self, n):
        return n

    @rule("'(' <expr:val> ')'")
    def expr_paren(self, val):
        return val

    @rule("<expr:res>")
    def top(self, res):
        return res

if __name__ == "__main__":
    calc = Calculator()
    tests = [
        "1 + 2 * 3", # 7
        "-1 + 4", # 3
        "2 * 3 + 4", # 10
        "2 * (3 + 4)", # 14
        "3! + 1", # 7
    ]
    for t in tests:
        print(t, "->", calc.parse(t))
```

# API summary

Each grammar is a separate class. To define one, you should subclass `Grammar` and define tokens and rules.

Say we have the following grammar:

```python
from grammar import Grammar, token, rule

class MyGrammar(Grammar):
    ...
```

Any following token or rule definition is inside the `MyGrammar` class.

Declare tokens:

```python
# token(
#     pattern: str,
#     convert: Optional[Callable[[str], Any]] = None,
#     with_word_boundaries: Optional[bool] = None,
#     ignore_case: bool = False,
# )
NUMBER = token(r'\d+', int)
VAR    = token(r'[A-Za-z]', lambda s: s.upper())
```

- *pattern* is a regular expression (used with `regex.match` at current parse position).
- *convert* is optional; when provided it is called with the matched text and its return value is passed to semantic actions.
- *with_word_boundaries* controls whether word boundaries (aka `\b`) are inserted; defaults to automatic detection.
- *ignore_case* ignores case of string literals.

Declare rules with `@rule(...)` decorating methods:

```python
@rule("<expr:x> '+' <expr:y>", prec=10, assoc='left')
def expr_add(self, x, y): return x + y
```

## Pattern syntax:

`<NAME:var>` -- reference to a token or rule named NAME. If `:var` is present, that element is passed as an argument to the semantic function.

Literals: `'+', '(', ')'` or bare punctuation `+` (single tokens without quotes are also accepted).

Alternatives separated by `|` are supported in a single pattern string (but many examples use multiple decorated functions).

Whitespace is skipped between pattern elements automatically (configurable via `Grammar(skip_whitespace=False)`).

`@rule` keyword args:

- `name` (optional): the logical rule name this alternative belongs to. If omitted, the library derives it from the function name by stripping the last _suffix (i.e. expr_add → rule name expr; expr_list_append → expr_list). Use name to be explicit.

- `prec` (optional): integer precedence for left-recursive operator-style alternatives. Larger means binds tighter.

- `assoc`: `'left'` or `'right'`. Default `'left'`.

- unary: `None`, `'prefix'`, or `'postfix'`. Use for unary forms (e.g. `- <expr>`).

Start parsing:

```python
g = MyGrammar()
result = g.parse("...")           # starts at rule "top" if present, otherwise first collected rule
result = g.parse("...", start_rule="line")  # optionally pick a start rule
```

## How to describe multiple alternatives for the same rule

Two recommended ways:

1. By function name convention -- give functions names of the form <rulename>_<altname>:

```python
@rule("<expr:x> '+' <expr:y>", prec=10)
def expr_add(self, x, y): ...

@rule("<expr:x> '*' <expr:y>", prec=20)
def expr_mul(self, x, y): ...
```

The engine derives the rule name by removing the last _suffix:
- expr_add -> rule expr.
- expr_list_items -> rule expr.

2. Explicit rule name -- pass `name="expr"` to `@rule`:

```python
@rule("<expr:x> '+' <expr:y>", name="expr", prec=10)
def add(self, x, y): ...
```

This is recommended when your function name doesn't follow the `<rulename>_<suffix>` pattern or for multi-word rule names (like `exprlist`) to avoid ambiguity.

## Error messages & location

`ParseError` includes line, column, a snippet of the offending line and a caret pointing to the position. When possible it lists expected tokens.

## Implementation notes & limitations

- Supports direct left-recursive operator patterns such as `<expr> '+' <expr>`.

- Precedence is per-alternative (`prec`) and associativity is supported (`assoc`).

- Unary prefix/postfix alternatives supported via unary argument.

## Limitations:

- No full packrat memoization (no memoization). Some grammars may backtrack and run slowly.

- Repetition `((A (',' A)*))` is implemented by left-recursive append rules or manual loop in semantic actions. There is no built-in `many()` or repetition operator yet.

- Tail recursion inside left-recursive alternatives is permitted for the immediate RHS (common operator grammars) but complex nested left-recursion patterns may not be supported.

- Error messages are 'best effort'--they reflect the matching attempt where failure occurred.
