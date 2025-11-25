from vibeparser import Grammar, token, rule


# --- TinyBASIC grammar ----
class TinyBasicGrammar(Grammar):
    NUMBER = token(r"\d+", int)

    # Variable: single letter A-Z (accept lowercase too, convert to uppercase)
    VAR = token(r"[A-Za-z]", lambda s: s.upper())

    # String literal (simple double-quoted string with backslash escapes)
    STRING = token(
        r'"(?:\\.|[^"\\])*"',
        lambda s: bytes(s[1:-1], "utf8").decode("unicode_escape"),
    )

    # Relational operators
    RELOP = token(r"(<=|>=|<>|<|>|=)")

    # Keywords (case-insensitive).
    KW_PRINT = token("PRINT", ignore_case=True)
    KW_IF = token("IF", ignore_case=True)
    KW_THEN = token("THEN", ignore_case=True)
    KW_GOTO = token("GOTO", ignore_case=True)
    KW_INPUT = token("INPUT", ignore_case=True)
    KW_LET = token("LET", ignore_case=True)
    KW_GOSUB = token("GOSUB", ignore_case=True)
    KW_RETURN = token("RETURN", ignore_case=True)
    KW_CLEAR = token("CLEAR", ignore_case=True)
    KW_LIST = token("LIST", ignore_case=True)
    KW_RUN = token("RUN", ignore_case=True)
    KW_END = token("END", ignore_case=True)

    # Binary +/-
    @rule("<expr:x> '+' <expr:y>", prec=10, assoc="left")
    def expr_add(self, x, y):
        return ("+", x, y)

    @rule("<expr:x> '-' <expr:y>", prec=10, assoc="left")
    def expr_sub(self, x, y):
        return ("-", x, y)

    # Binary */
    @rule("<expr:x> '*' <expr:y>", prec=20, assoc="left")
    def expr_mul(self, x, y):
        return ("*", x, y)

    @rule("<expr:x> '/' <expr:y>", prec=20, assoc="left")
    def expr_div(self, x, y):
        return ("/", x, y)

    # Prefix unary +/-
    @rule("'-' <expr:x>", prec=30, unary="prefix")
    def expr_neg(self, x):
        return ("neg", x)

    @rule("'+' <expr:x>", prec=30, unary="prefix")
    def expr_pos(self, x):
        return ("pos", x)

    # Numbers and variables and parenthesis seeds (non-left alternatives)
    @rule("<NUMBER:n>")
    def expr_number(self, n):
        # NUMBER token already converts to int
        return n

    @rule("<VAR:v>")
    def expr_var(self, v):
        # Represent variable as ('var', 'A')
        return ("var", v)

    @rule("'(' <expr:inner> ')'")
    def expr_paren(self, inner):
        return inner

    # --- expr-list: (string | expression) separated by commas ---
    # item: either string or expression
    @rule("<STRING:s>")
    def item_string(self, s):
        return s

    @rule("<expr:e>")
    def item_expr(self, e):
        return e

    # exprlist single
    @rule("<item:i>")
    def exprlist_single(self, i):
        return [i]

    # exprlist left-recursive append: <exprlist> ',' <item>
    @rule("<exprlist:xs> ',' <item:i>", prec=1, assoc="right")
    def exprlist_append(self, xs, i):
        return xs + [i]

    # --- var-list: var (',' var)* ---
    @rule("<VAR:v>")
    def varsingle(self, v):
        return [v]

    @rule("<varlist:xs> ',' <VAR:v>", prec=1, assoc="right")
    def varlist_append(self, xs, v):
        return xs + [v]

    # --- statements (grouped under rule name 'statement' via method-name prefix) ---
    @rule("<KW_PRINT> <exprlist:items>")
    def statement_print(self, items):
        return ("PRINT", items)

    @rule("<KW_IF> <expr:left> <RELOP:op> <expr:right> <KW_THEN> <statement:then_stmt>")
    def statement_if(self, left, op, right, then_stmt):
        return ("IF", op, left, right, then_stmt)

    @rule("<KW_GOTO> <expr:dest>")
    def statement_goto(self, dest):
        return ("GOTO", dest)

    @rule("<KW_INPUT> <varlist:vars>")
    def statement_input(self, vars):
        return ("INPUT", vars)

    @rule("<KW_LET> <VAR:v> '=' <expr:val>")
    def statement_let(self, v, val):
        return ("LET", v, val)

    @rule("<KW_GOSUB> <expr:dest>")
    def statement_gosub(self, dest):
        return ("GOSUB", dest)

    @rule("<KW_RETURN>")
    def statement_return(self):
        return ("RETURN",)

    @rule("<KW_CLEAR>")
    def statement_clear(self):
        return ("CLEAR",)

    @rule("<KW_LIST>")
    def statement_list(self):
        return ("LIST",)

    @rule("<KW_RUN>")
    def statement_run(self):
        return ("RUN",)

    @rule("<KW_END>")
    def statement_end(self):
        return ("END",)

    @rule("<statement:res>")
    def top(self, res):
        return res


if __name__ == "__main__":
    g = TinyBasicGrammar()

    print(g.parse("LET A = 5"))
    # -> ('LINE', 10, ('LET', 'A', 5))

    print(g.parse('PRINT "HI", A+1'))
    # -> ('LINE', None, ('PRINT', ['HI', ('+', ('var','A'), 1)] ))

    print(g.parse("IF 2 < 3 THEN GOTO 100"))
    # -> ('LINE', None, ('IF', '<', 2, 3, ('GOTO', 100)))
