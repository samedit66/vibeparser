# pl0_grammar.py
# PL/0-like grammar using the quantifier/group-repetition DSL.
#
# EBNF:
# program = block "." ;
# block = [ "const" ident "=" number {"," ident "=" number} ";"]
#         [ "var" ident {"," ident} ";"]
#         { "procedure" ident ";" block ";" } statement ;
# statement = ident ":=" expression | "call" ident
#              | "?" ident | "!" expression
#              | "begin" statement {";" statement } "end"
#              | "if" condition "then" statement
#              | "while" condition "do" statement
# condition = "odd" expression |
#             expression ("="|"#"|"<"|"<="|">"|">=") expression ;
# expression = [ "+"|"-"] term { ("+"|"-") term};
# term = factor {("*"|"/") factor};
# factor = ident | number | "(" expression ")";

from grammar import Grammar, token, rule


class PL0Grammar(Grammar):
    # --- tokens (keywords first so they win over IDENT) ---
    KW_CONST     = token(r'const', ignore_case=True)
    KW_VAR       = token(r'var', ignore_case=True)
    KW_PROCEDURE = token(r'procedure', ignore_case=True)
    KW_CALL      = token(r'call', ignore_case=True)
    KW_BEGIN     = token(r'begin', ignore_case=True)
    KW_END       = token(r'end', ignore_case=True)
    KW_IF        = token(r'if', ignore_case=True)
    KW_THEN      = token(r'then', ignore_case=True)
    KW_WHILE     = token(r'while', ignore_case=True)
    KW_DO        = token(r'do', ignore_case=True)
    KW_ODD       = token(r'odd', ignore_case=True)

    # basic tokens
    NUMBER   = token(r'\d+', int, with_word_boundaries=False)
    IDENT    = token(r'[A-Za-z]\w*', lambda ident: ident.upper())

    # relational operators
    RELOP    = token(r'(<=|>=|=|#|<|>)')

    # ------------------ consts ------------------
    # const_item := IDENT '=' NUMBER
    @rule("<IDENT:name> '=' <NUMBER:value>", name="const_item")
    def const_item(self, name, value):
        return (name, value)

    # const_list := const_item (',' const_item)*
    @rule("<const_item*:items>", name="const_list")
    def const_list(self, items):
        return items

    # const_decl := 'const' const_list ';' | <empty>
    @rule("<KW_CONST> <const_list:list> ';'", name="const_decl")
    def const_decl_present(self, list):
        return list

    @rule("", name="const_decl")
    def const_decl_empty(self):
        return []

    # ------------------ vars ------------------
    # var_list := IDENT (',' IDENT)*
    @rule("<IDENT:first> [',' <IDENT:next>]:rest", name="var_list")
    def var_list(self, first, rest):
        return [first] + rest

    @rule("", name="var_list")
    def var_list_empty(self):
        return []

    # var_decl := 'var' var_list ';' | <empty>
    @rule("<KW_VAR> <var_list:vars> ';'", name="var_decl")
    def var_decl_present(self, vars):
        return vars

    @rule("", name="var_decl")
    def var_decl_empty(self):
        return []

    # ------------------ procedures ------------------
    # proc_item := 'procedure' IDENT ';' block ';'
    @rule("<KW_PROCEDURE> <IDENT:name> ';' <block:b> ';'", name="proc_item")
    def proc_item(self, name, b):
        return (name, b)

    # proc_list := proc_item*  (explicit empty alt added)
    @rule("<proc_item*:ps>", name="proc_list")
    def proc_list(self, ps):
        return ps

    # ------------------ block & program ------------------
    # block = [ const_decl ] [ var_decl ] proc_list statement
    @rule("<const_decl:consts> <var_decl:vars> <proc_list:procs> <statement:stmt>", name="block")
    def block(self, consts, vars, procs, stmt):
        # consts, vars, procs are lists (possibly empty)
        return ('BLOCK', consts or [], vars or [], procs or [], stmt)

    @rule("<block:b> '.'", name="top")
    def top(self, b):
        return ('PROGRAM', b)

    # ------------------ statements ------------------
    @rule("<IDENT:name> ':=' <expression:expr>", name="statement")
    def statement_assign(self, name, expr):
        return ('assign', name, expr)

    @rule("<KW_CALL> <IDENT:name>", name="statement")
    def statement_call(self, name):
        return ('call', name)

    @rule("'?' <IDENT:name>", name="statement")
    def statement_input(self, name):
        return ('read', name)

    @rule("'!' <expression:expr>", name="statement")
    def statement_output(self, expr):
        return ('write', expr)

    # statement sequence for begin ... end:
    # stmt_seq := statement ( ';' statement )*
    @rule("<statement:first> [ ';' <statement:next> ]:rest", name="stmt_seq")
    def stmt_seq(self, first, rest):
        return [first] + rest

    @rule("", name="stmt_seq")
    def stmt_seq_empty(self):
        return []

    @rule("<KW_BEGIN> <stmt_seq:stmts> <KW_END>", name="statement")
    def statement_begin(self, stmts):
        return ('begin', stmts)

    @rule("<KW_IF> <condition:c> <KW_THEN> <statement:st>", name="statement")
    def statement_if(self, c, st):
        return ('if', c, st)

    @rule("<KW_WHILE> <condition:c> <KW_DO> <statement:st>", name="statement")
    def statement_while(self, c, st):
        return ('while', c, st)

    # ------------------ condition ------------------
    @rule("<KW_ODD> <expression:e>", name="condition")
    def condition_odd(self, e):
        return ('odd', e)

    @rule("<expression:left> <RELOP:op> <expression:right>", name="condition")
    def condition_relop(self, left, op, right):
        return ('cond', op, left, right)

    # ------------------ expressions ------------------
    @rule("'-' <expression:x>", name="expression", prec=30, unary='prefix')
    def expression_neg(self, x):
        return ('neg', x)

    @rule("'+' <expression:x>", name="expression", prec=30, unary='prefix')
    def expression_pos(self, x):
        return x

    @rule("<NUMBER:n>", name="expression")
    def expression_number(self, n):
        return n

    @rule("<IDENT:v>", name="expression")
    def expression_ident(self, v):
        return ('var', v)

    @rule("'(' <expression:e> ')'", name="expression")
    def expression_paren(self, e):
        return e

    @rule("<expression:x> '*' <expression:y>", name="expression", prec=20, assoc='left')
    def expression_mul(self, x, y):
        return ('*', x, y)

    @rule("<expression:x> '/' <expression:y>", name="expression", prec=20, assoc='left')
    def expression_div(self, x, y):
        return ('/', x, y)

    @rule("<expression:x> '+' <expression:y>", name="expression", prec=10, assoc='left')
    def expression_add(self, x, y):
        return ('+', x, y)

    @rule("<expression:x> '-' <expression:y>", name="expression", prec=10, assoc='left')
    def expression_sub(self, x, y):
        return ('-', x, y)


if __name__ == '__main__':
    g = PL0Grammar()

    samples = [
        """
        VAR x, squ;

        PROCEDURE square;
        BEGIN
           squ:= x * x
        END;

        BEGIN
           x := 1;
           WHILE x <= 10 DO
           BEGIN
              CALL square;
              ! squ;
              x := x + 1
           END
        END.
        """,

        """
        VAR x, y, z, q, r, n, f;

        PROCEDURE multiply;
        VAR a, b;
        BEGIN
          a := x;
          b := y;
          z := 0;
          WHILE b > 0 DO
          BEGIN
            IF ODD b THEN z := z + a;
            a := 2 * a;
            b := b / 2
          END
        END;

        PROCEDURE divide;
        VAR w;
        BEGIN
          r := x;
          q := 0;
          w := y;
          WHILE w <= r DO w := 2 * w;
          WHILE w > y DO
          BEGIN
            q := 2 * q;
            w := w / 2;
            IF w <= r THEN
            BEGIN
              r := r - w;
              q := q + 1
            END
          END
        END;

        PROCEDURE gcd;
        VAR f, g;
        BEGIN
          f := x;
          g := y;
          WHILE f # g DO
          BEGIN
            IF f < g THEN g := g - f;
            IF g < f THEN f := f - g
          END;
          z := f
        END;

        PROCEDURE fact;
        BEGIN
          IF n > 1 THEN
          BEGIN
            f := n * f;
            n := n - 1;
            CALL fact
          END
        END;

        BEGIN
          ?x; ?y; CALL multiply; !z;
          ?x; ?y; CALL divide; !q; !r;
          ?x; ?y; CALL gcd; !z;
          ?n; f := 1; CALL fact; !f
        END.
        """,
    ]

    for s in samples:
        try:
            print("SOURCE:", s)
            ast = g.parse(s)
            print("AST  :", ast)
        except Exception as e:
            print("ERROR:", e)
