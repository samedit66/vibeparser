import re
from abc import ABC
from typing import Optional, Callable, List, Tuple, Any


class ParseError(Exception):
    pass


# ---------------- DSL spec objects ----------------
class TokenSpec:
    def __init__(self, pattern: str, convert: Optional[Callable[[str], Any]] = None):
        self.pattern = pattern
        self.convert = convert
        self._is_token = True


def token(
    pattern: str,
    convert: Optional[Callable[[str], Any]] = None,
    with_word_boundaries: Optional[bool] = None,
    ignore_case: bool = False,
):
    """
    Smarter token() that auto-decides word boundaries if None:

    - If pattern starts with [A-Za-z0-9] → use word boundaries
    - If pattern contains only punctuation → do NOT use word boundaries
    - Otherwise, if pattern contains letters → apply word boundaries
    - Otherwise → do not apply
    """

    def auto_decide(pattern: str) -> bool:
        # strip grouping parentheses
        p = pattern.strip()

        # Heuristic 1: looks like a bare IDENTIFIER or KEYWORD token
        # Example: "PRINT" or "[A-Za-z]"
        if re.match(r"[A-Za-z0-9]", p):
            return True

        # Extract raw characters (ignoring escape sequences)
        raw = re.sub(r"\\.", "", p)

        # Heuristic 2: only punctuation operators
        # Example: "<=", "<>", "<", "=", "+", "-"
        if re.fullmatch(r"[\W_]+", raw) and not re.search(r"[A-Za-z0-9]", raw):
            return False

        # Heuristic 3: complex pattern containing alpha → probably identifiers
        if re.search(r"[A-Za-z]", raw):
            return True

        # Default: no word boundaries
        return False

    # If user did NOT specify with_word_boundaries → compute
    if with_word_boundaries is None:
        apply_wb = auto_decide(pattern)
    else:
        apply_wb = with_word_boundaries

    # Apply word boundaries if appropriate
    if apply_wb:
        pattern = rf"\b{pattern}\b"

    # Case-insensitive wrapping
    if ignore_case:
        pattern = f"(?i:{pattern})"

    return TokenSpec(pattern, convert)


def rule(
    pattern: str,
    *,
    name: Optional[str] = None,
    prec: Optional[int] = None,
    assoc: str = "left",
    unary: Optional[str] = None,
):
    """Decorator for rule alternatives.

    pattern: DSL with <name:var> references and literals (same as earlier).
    Optional keyword args:
      - prec: precedence (int). If provided and the alt is left-recursive (starts with <rule:...>),
              it will be considered a binary operator alt with that precedence.
      - assoc: 'left' or 'right' (used when prec provided).
      - unary: None or 'prefix' or 'postfix' (for unary ops, e.g. "- <expr>" or "<expr> '!'")
    """
    if assoc not in ("left", "right"):
        raise ValueError("assoc must be 'left' or 'right'")
    if unary not in (None, "prefix", "postfix"):
        raise ValueError("unary must be None|'prefix'|'postfix'")

    def decorator(fn):
        # attach metadata to function; a function may be decorated multiple times
        if not hasattr(fn, "_rule_alts"):
            fn._rule_alts = []
        fn._rule_alts.append(
            {
                "pattern": pattern,
                "prec": prec,
                "assoc": assoc,
                "unary": unary,
                "rule_name": name,
            }
        )
        fn._is_rule = True
        return fn

    return decorator


# ---------------- Grammar engine ----------------
class Grammar(ABC):
    def __init__(self, skip_whitespace: bool = True):
        self.skip_whitespace = skip_whitespace

        # tokens: name -> (compiled_regex, convert)
        self._tokens = {}
        for name, val in self.__class__.__dict__.items():
            if hasattr(val, "_is_token"):
                self._tokens[name] = (re.compile(val.pattern), val.convert)

        # rules: name -> list of alts. Each alt is dict with parsed pattern + metadata + function
        self._rules = {}
        self._rule_order = []

        for func_name, attr in self.__class__.__dict__.items():
            if hasattr(attr, "_is_rule"):
                for alt_meta in getattr(attr, "_rule_alts", []):
                    # explicit rule name if provided, otherwise derive:
                    # - if name supplied in decorator use it
                    # - else if function name contains '_' take prefix before '_' (expr_add -> expr)
                    # - else use full function name
                    rule_name = alt_meta.get("rule_name")
                    if not rule_name:
                        if "_" in func_name:
                            rule_name = func_name.split("_", 1)[0]
                        else:
                            rule_name = func_name

                    parsed = self._parse_pattern(alt_meta["pattern"])
                    alt = {
                        "pattern_raw": alt_meta["pattern"],
                        "elems": parsed,
                        "prec": alt_meta["prec"],
                        "assoc": alt_meta["assoc"],
                        "unary": alt_meta["unary"],
                        "func": getattr(self, func_name),  # bound method
                    }
                    if rule_name not in self._rules:
                        self._rules[rule_name] = []
                        self._rule_order.append(rule_name)
                    self._rules[rule_name].append(alt)

    # pattern parser: same DSL as before
    def _parse_pattern(self, pattern: str):
        elements = []
        pos = 0
        token_re = re.compile(r"<([^:>]+)(?::([^>]+))?>")
        while pos < len(pattern):
            m = token_re.search(pattern, pos)
            if not m:
                chunk = pattern[pos:].strip()
                if chunk:
                    parts = re.findall(r"'([^']*)'|\"([^\"]*)\"|(\S+)", chunk)
                    for a, b, c in parts:
                        lit = a or b or c
                        elements.append(("lit", lit))
                break
            # literal before
            if m.start() > pos:
                chunk = pattern[pos : m.start()].strip()
                if chunk:
                    parts = re.findall(r"'([^']*)'|\"([^\"]*)\"|(\S+)", chunk)
                    for a, b, c in parts:
                        lit = a or b or c
                        elements.append(("lit", lit))
            name = m.group(1)
            var = m.group(2)
            elements.append(("ref", name, var))
            pos = m.end()
        return elements

    # location helpers
    def _loc(self, text: str, pos: int):
        if pos < 0:
            pos = 0
        line = text.count("\n", 0, pos) + 1
        last_n = text.rfind("\n", 0, pos)
        if last_n == -1:
            col = pos + 1
            line_start = 0
        else:
            col = pos - last_n
            line_start = last_n + 1
        return line, col, line_start

    def _format_error(
        self, message: str, text: str, pos: int, expected: Optional[List[str]] = None
    ):
        line, col, line_start = self._loc(text, pos)
        line_end = text.find("\n", line_start)
        if line_end == -1:
            line_end = len(text)
        snippet = text[line_start:line_end]
        caret = " " * (col - 1) + "^"
        exp_part = f"\nExpected: {', '.join(expected)}" if expected else ""
        return ParseError(
            f"{message}\nLine {line}, Column {col}:{exp_part}\n{snippet}\n{caret}"
        )

    # public parse entrypoint
    def parse(self, text: str, start_rule: Optional[str] = None):
        if start_rule is None:
            start_rule = (
                "top"
                if "top" in self._rules
                else (self._rule_order[0] if self._rule_order else None)
            )
        if start_rule is None:
            raise RuntimeError("No rules defined")
        val, pos = self._match_rule(start_rule, text, 0, min_prec=0)
        if self.skip_whitespace:
            while pos < len(text) and text[pos].isspace():
                pos += 1
        if pos != len(text):
            raise self._format_error("Unconsumed input", text, pos)
        return val

    # Core matcher: supports direct left-recursive alts with precedence and prefix/postfix unary alts.
    def _match_rule(
        self, rule_name: str, text: str, pos: int, min_prec: int = 0
    ) -> Tuple[Any, int]:
        if rule_name not in self._rules:
            raise self._format_error(f"Unknown rule '{rule_name}'", text, pos)

        alts = self._rules[rule_name]

        # remember the original min_prec for this invocation
        orig_min = min_prec

        # partition alts into categories
        non_left = []  # alts that do NOT start with a ref to the same rule
        left_rec = []  # alts that DO start with ('ref', rule_name, var)  => treat as left-recursive operator patterns
        prefix_unary = []  # alts with unary=='prefix'
        postfix_unary = []  # alts with unary=='postfix'
        for alt in alts:
            elems = alt["elems"]
            if elems and elems[0][0] == "ref" and elems[0][1] == rule_name:
                left_rec.append(alt)
            elif alt["unary"] == "prefix":
                prefix_unary.append(alt)
            elif alt["unary"] == "postfix":
                postfix_unary.append(alt)
            else:
                non_left.append(alt)

        expected = []

        # Helper: match a plain alternative (no special left-rec growth) starting at pos.
        def try_match_alt(alt, start_pos):
            cur = start_pos
            args = []
            for elem in alt["elems"]:
                if self.skip_whitespace:
                    while cur < len(text) and text[cur].isspace():
                        cur += 1
                if elem[0] == "lit":
                    lit = elem[1]
                    if text.startswith(lit, cur):
                        cur += len(lit)
                    else:
                        expected.append(lit)
                        return None
                elif elem[0] == "ref":
                    name, var = elem[1], elem[2]
                    if name in self._tokens:
                        regex, convert = self._tokens[name]
                        m = regex.match(text, cur)
                        if not m:
                            expected.append(f"<{name}>")
                            return None
                        raw = m.group(0)
                        cur = m.end()
                        val = convert(raw) if convert else raw
                        if var:
                            args.append(val)
                    else:
                        # recursive rule call (normal recursion) - pass down min_prec=0 for nested calls
                        try:
                            val, new_pos = self._match_rule(name, text, cur, min_prec=0)
                        except ParseError:
                            expected.append(f"<{name}>")
                            return None
                        cur = new_pos
                        if var:
                            args.append(val)
                else:
                    return None
            try:
                return alt["func"](*args), cur
            except Exception as e:
                raise self._format_error(
                    f"Error in semantic action for rule '{rule_name}': {e}",
                    text,
                    start_pos,
                )

        # 1) Try prefix unary forms first (they create a seed)
        seeds = []
        for alt in prefix_unary:
            # prefix form: match initial elements up to first <rule_name:...> which should be the recursive target
            # But in our convention, prefix unary alt pattern should be like "'-' <{rule}:x>"
            elems = alt["elems"]
            cur = pos
            args = []
            ok = True
            for elem in elems:
                if self.skip_whitespace:
                    while cur < len(text) and text[cur].isspace():
                        cur += 1
                if elem[0] == "lit":
                    if text.startswith(elem[1], cur):
                        cur += len(elem[1])
                    else:
                        ok = False
                        expected.append(elem[1])
                        break
                elif elem[0] == "ref":
                    name, var = elem[1], elem[2]
                    if name == rule_name:
                        # parse nested rule with min_prec = alt.prec (prefix binds to that precedence)
                        try:
                            inner, new_pos = self._match_rule(
                                name,
                                text,
                                cur,
                                min_prec=alt["prec"] if alt["prec"] is not None else 0,
                            )
                        except ParseError:
                            ok = False
                            expected.append(f"<{name}>")
                            break
                        cur = new_pos
                        if var:
                            args.append(inner)
                    else:
                        if name in self._tokens:
                            regex, convert = self._tokens[name]
                            m = regex.match(text, cur)
                            if not m:
                                ok = False
                                expected.append(f"<{name}>")
                                break
                            raw = m.group(0)
                            cur = m.end()
                            val = convert(raw) if convert else raw
                            if var:
                                args.append(val)
                        else:
                            try:
                                val, new_pos = self._match_rule(
                                    name, text, cur, min_prec=0
                                )
                            except ParseError:
                                ok = False
                                expected.append(f"<{name}>")
                                break
                            cur = new_pos
                            if var:
                                args.append(val)
            if ok:
                try:
                    seeds.append((alt, alt["func"](*args), cur))
                except Exception as e:
                    raise self._format_error(
                        f"Error in semantic action for rule '{rule_name}': {e}",
                        text,
                        pos,
                    )

        # 2) Try non-left alternatives as seeds (in order)
        if not seeds:
            for alt in non_left:
                res = try_match_alt(alt, pos)
                if res is not None:
                    val, newpos = res
                    seeds.append((alt, val, newpos))
                    break

        if not seeds:
            # nothing matched as seed -> failure
            raise self._format_error(
                f"Failed to match rule '{rule_name}'",
                text,
                pos,
                expected=list(dict.fromkeys(expected)),
            )

        # We'll grow each successful seed via left-recursive alts and postfix unary alts
        # For simplicity, pick the first seed (most rules are deterministic)
        seed_alt, left_val, curpos = seeds[0]

        # growth loop: repeatedly try to apply left-recursive alts (and postfix unary) while precedence allows
        while True:
            progressed = False

            # First: postfix unary alternatives (they are like left-rec tails with no new LHS)
            for alt in postfix_unary:
                # pattern typically "<{rule}:x> '!'"
                elems = alt["elems"]
                # match elems but substitute the first ref value with current left (we don't consume it from text)
                # We expect elems to start with ('ref', rule_name, var)
                if not (elems and elems[0][0] == "ref" and elems[0][1] == rule_name):
                    continue
                tail = elems[1:]
                cur = curpos
                args = [left_val]  # first param is the LHS
                ok = True
                for elem in tail:
                    if self.skip_whitespace:
                        while cur < len(text) and text[cur].isspace():
                            cur += 1
                    if elem[0] == "lit":
                        if text.startswith(elem[1], cur):
                            cur += len(elem[1])
                        else:
                            ok = False
                            break
                    elif elem[0] == "ref":
                        name, var = elem[1], elem[2]
                        if name in self._tokens:
                            regex, convert = self._tokens[name]
                            m = regex.match(text, cur)
                            if not m:
                                ok = False
                                break
                            raw = m.group(0)
                            cur = m.end()
                            val = convert(raw) if convert else raw
                            if var:
                                args.append(val)
                        else:
                            try:
                                val, new_pos = self._match_rule(
                                    name, text, cur, min_prec=0
                                )
                            except ParseError:
                                ok = False
                                break
                            cur = new_pos
                            if var:
                                args.append(val)
                if ok:
                    # call semantic function - first arg is LHS
                    try:
                        new_left = alt["func"](*args)
                    except Exception as e:
                        raise self._format_error(
                            f"Error in semantic action for rule '{rule_name}': {e}",
                            text,
                            curpos,
                        )
                    # update
                    left_val = new_left
                    curpos = cur
                    progressed = True
                    # continue growth after successful postfix
                    break
            if progressed:
                continue

            # Second: left-recursive binary-like alternatives
            # We'll attempt any left-rec alt whose prec >= min_prec
            # To make associativity consistent, we iterate through alts sorted by precedence descending (higher binds tighter)
            left_rec_sorted = sorted(
                [a for a in left_rec if a["prec"] is not None], key=lambda x: -x["prec"]
            )
            applied_any = False
            for alt in left_rec_sorted:
                p = alt["prec"]
                assoc = alt["assoc"]
                # respect the original min_prec for this invocation
                if p < orig_min:
                    continue
                # tail elements after the leading self-ref
                tail = alt["elems"][1:]
                cur = curpos
                args = [
                    left_val
                ]  # first parameter corresponds to the leading <rule:...>
                ok = True
                for elem in tail:
                    if self.skip_whitespace:
                        while cur < len(text) and text[cur].isspace():
                            cur += 1
                    if elem[0] == "lit":
                        if text.startswith(elem[1], cur):
                            cur += len(elem[1])
                        else:
                            ok = False
                            break
                    elif elem[0] == "ref":
                        name, var = elem[1], elem[2]
                        if name in self._tokens:
                            regex, convert = self._tokens[name]
                            m = regex.match(text, cur)
                            if not m:
                                ok = False
                                break
                            raw = m.group(0)
                            cur = m.end()
                            val = convert(raw) if convert else raw
                            if var:
                                args.append(val)
                        else:
                            # previously we rejected recursive references to the same rule in the tail.
                            # Instead, parse the RHS recursively with the correct min_prec so expressions like
                            # "<expr:x> '+' <expr:y>" work.
                            if name == rule_name:
                                # compute min precedence for parsing RHS based on associativity:
                                # - for left-assoc operators, RHS should be parsed with min_prec = p+1
                                # - for right-assoc operators, RHS should be parsed with min_prec = p
                                rhs_min = p if assoc == "right" else (p + 1)
                                try:
                                    val, new_pos = self._match_rule(
                                        name, text, cur, min_prec=rhs_min
                                    )
                                except ParseError:
                                    ok = False
                                    break
                                cur = new_pos
                                if var:
                                    args.append(val)
                            else:
                                try:
                                    val, new_pos = self._match_rule(
                                        name, text, cur, min_prec=0
                                    )
                                except ParseError:
                                    ok = False
                                    break
                                cur = new_pos
                                if var:
                                    args.append(val)
                if ok:
                    # call semantic func - with left_val as first arg
                    try:
                        new_left = alt["func"](*args)
                    except Exception as e:
                        raise self._format_error(
                            f"Error in semantic action for rule '{rule_name}': {e}",
                            text,
                            curpos,
                        )

                    # apply the growth (do not change orig_min here — keep the original min_prec).
                    left_val = new_left
                    curpos = cur
                    applied_any = True
                    # continue scanning left-recursive alts (don't mutate orig_min)
                    break  # restart scanning left-rec alts after successful application

            if applied_any:
                progressed = True
                continue

            # nothing applied this iteration -> break
            if not progressed:
                break

        return left_val, curpos
