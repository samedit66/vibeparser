from math import prod
from vibeparser import Grammar, token, rule


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

    @rule("<expr:x> '/' <expr:y>", prec=20, assoc="left")
    def expr_div(self, x, y):
        return x / y

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
        "1 + 2 * 3",  # 7
        "-1 + 4",  # 3
        "2 * 3 + 4",  # 10
        "2 * (3 + 4)",  # 14
        "3! + 1",  # 7
        "5! / 5",  # 24.0
    ]
    for t in tests:
        print(t, "->", calc.parse(t))
