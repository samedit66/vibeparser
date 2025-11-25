# json_grammar.py
import ast
from vibeparser import Grammar, token, rule


# helpers for token conversion
def _json_string_unescape(s: str):
    # s includes quotes (the token regex below retains quotes). Use ast.literal_eval to decode escapes.
    try:
        return ast.literal_eval(s)
    except Exception:
        # fallback: strip quotes if any
        if len(s) >= 2 and s[0] in "\"'" and s[-1] == s[0]:
            return s[1:-1]
        return s


def _json_number_conv(s: str):
    if "." in s or "e" in s or "E" in s:
        return float(s)
    try:
        return int(s)
    except Exception:
        return float(s)


class JSONGrammar(Grammar):
    # Tokens
    # STRING: quoted string with escapes (we allow \" and \\ etc.)
    STRING = token(r'"(?:\\.|[^"\\])*"', _json_string_unescape)

    # NUMBER: integer or float with optional exponent
    NUMBER = token(r"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?", _json_number_conv)

    # literals
    TRUE = token(r"true", lambda s: True)
    FALSE = token(r"false", lambda s: False)
    NULL = token(r"null", lambda s: None)

    # punctuation
    LBRACE = token(r"\{")
    RBRACE = token(r"\}")
    LBRACK = token(r"\[")
    RBRACK = token(r"\]")
    COLON = token(r":")
    COMMA = token(r",")

    # ---- grammar rules ----
    # value: STRING | NUMBER | object | array | true | false | null
    @rule("<STRING:s>")
    def value_string(self, s):
        return s

    @rule("<NUMBER:n>")
    def value_number(self, n):
        return n

    @rule("<object:o>")
    def value_object(self, o):
        return o

    @rule("<array:a>")
    def value_array(self, a):
        return a

    @rule("<TRUE:t>")
    def value_true(self, t):
        return t

    @rule("<FALSE:f>")
    def value_false(self, f):
        return f

    @rule("<NULL:nu>")
    def value_null(self, nu):
        return nu

    # pair: STRING ':' value
    @rule("<STRING:key> <COLON> <value:val>", name="pair")
    def pair(self, key, val):
        return (key, val)

    # object: {} | { pair (',' pair)* }
    @rule("<LBRACE> <RBRACE>", name="object")
    def object_empty(self):
        return {}

    @rule("<LBRACE> <pair:first> [ <COMMA> <pair:p> ]:rest <RBRACE>", name="object")
    def object_nonempty(self, first, rest):
        items = [first] + rest
        return dict(items)

    # array: [] | [ value (',' value)* ]
    @rule("<LBRACK> <RBRACK>", name="array")
    def array_empty(self):
        return []

    @rule("<LBRACK> <value:first> [ <COMMA> <value:v> ]:rest <RBRACK>", name="array")
    def array_nonempty(self, first, rest):
        return [first] + rest

    # top rule: allow top-level whitespace trimmed by Grammar.parse
    @rule("<value:res>")
    def top(self, res):
        return res


if __name__ == "__main__":
    jg = JSONGrammar()

    tests = [
        "null",
        "true",
        "false",
        '"hello"',
        '"esc\\\\nline"',
        "123",
        "-12.34e+2",
        "[]",
        "[1, 2, 3]",
        '[ "a", null, true, 3.14 ]',
        "{}",
        '{"a": 1, "b": [true, false], "c": {"x": "y"}}',
        """
        {
          "name": "Alice",
          "age": 30,
          "tags": ["dev", "py"],
          "prefs": {"dark": true}
        }
        """,
    ]

    for t in tests:
        try:
            val = jg.parse(t)
            print("INPUT:", t.strip())
            print("PARSED:", val)
            print("-" * 40)
        except Exception as e:
            print("INPUT:", t.strip())
            print("ERROR:", e)
            print("-" * 40)
