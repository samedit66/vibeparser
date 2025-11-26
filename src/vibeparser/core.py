import re
import ast
import inspect
import warnings
from abc import ABC
from typing import Optional, Callable, List, Tuple, Any


class ParseError(Exception):
    pass


# ---------------- DSL spec objects ----------------
class TokenSpec:
    def __init__(
        self,
        pattern: str,
        convert: Optional[Callable[[str], Any]] = None,
        ignore_case: bool = False,
        capture_by_default: bool = False,
    ):
        self.pattern = pattern
        self.convert = convert
        self.ignore_case = ignore_case
        # if True, an unnamed reference <TOKEN> will produce a capture by default
        self.capture_by_default = capture_by_default
        self._is_token = True


def token(
    pattern: str,
    convert: Optional[Callable[[str], Any]] = None,
    with_word_boundaries: Optional[bool] = None,
    ignore_case: bool = False,
    capture_by_default: bool = False,
):
    """
    Smarter token() that auto-decides word boundaries if None.

    New argument:
      - capture_by_default: if True, a bare <TOKEN> reference will capture
                            its value even when not written as <TOKEN:var>.
                            Default: False.
    """

    def auto_decide(pattern: str) -> bool:
        p = pattern.strip()

        # Heuristic A: pattern starts with alnum → use word boundaries
        if re.match(r"[A-Za-z0-9]", p):
            return True

        # Strip backslash-escapes for analysis
        raw = re.sub(r"\\.", "", p)

        # Heuristic B: only punctuation/operator-like → do NOT use word boundaries
        if re.fullmatch(r"[\W_]+", raw) and not re.search(r"[A-Za-z0-9]", raw):
            return False

        # Heuristic C (changed): If the pattern contains alphabetic letters,
        # only apply word boundaries if the original pattern also starts with
        # an alphanumeric (to avoid cases like "-? ... e ..." being wrapped).
        if re.search(r"[A-Za-z]", raw) and re.match(r"[A-Za-z0-9]", p):
            return True

        # Default: do not apply word boundaries
        return False

    # If user did NOT specify with_word_boundaries → compute
    if with_word_boundaries is None:
        apply_wb = auto_decide(pattern)
    else:
        apply_wb = with_word_boundaries

    # Apply word boundaries if appropriate
    if apply_wb:
        pattern = rf"\b{pattern}\b"

    # DO NOT embed inline (?i:...) — store ignore_case in the TokenSpec instead
    # Case-insensitive will be applied when compiling the regex.
    return TokenSpec(
        pattern, convert, ignore_case=ignore_case, capture_by_default=capture_by_default
    )


def rule(
    pattern: str,
    *,
    name: Optional[str] = None,
    prec: Optional[int] = None,
    assoc: str = "left",
    unary: Optional[str] = None,
    transform: Optional[Callable[[List[Any]], Any]] = None,
    validate: Optional[Callable[[List[Any]], bool]] = None,
    capture_anon: bool = False,
):
    """
    rule decorator. New kwargs:
      - transform: callable(args_list) -> iterable/new args (or None to reject alt)
      - validate: callable(args_list) -> bool (False => treat as not matched)
      - capture_anon: if True, capture refs without explicit var names as
                      positional arguments (useful for patterns like
                      "<assignment> ::= <varname> '=' <expr>" and semantic
                      actions that expect positional args for those refs).
    """
    if assoc not in ("left", "right"):
        raise ValueError("assoc must be 'left' or 'right'")
    if unary not in (None, "prefix", "postfix"):
        raise ValueError("unary must be None|'prefix'|'postfix'")

    def decorator(fn):
        if not hasattr(fn, "_rule_alts"):
            fn._rule_alts = []
        fn._rule_alts.append(
            {
                "pattern": pattern,
                "prec": prec,
                "assoc": assoc,
                "unary": unary,
                "rule_name": name,
                "transform": transform,
                "validate": validate,
                "capture_anon": capture_anon,
            }
        )
        fn._is_rule = True
        return fn

    return decorator


# ---------------- Grammar engine ----------------
class Grammar(ABC):
    def __init__(self, skip_whitespace: bool = True):
        self.skip_whitespace = skip_whitespace

        # tokens: name -> (compiled_regex, convert, capture_by_default)
        self._tokens = {}
        for name, val in self.__class__.__dict__.items():
            if hasattr(val, "_is_token"):
                flags = re.IGNORECASE if getattr(val, "ignore_case", False) else 0
                self._tokens[name] = (
                    re.compile(val.pattern, flags),
                    val.convert,
                    getattr(val, "capture_by_default", False),
                )

        # rules: name -> list of alts. Each alt is dict with parsed pattern + metadata + function
        self._rules = {}
        self._rule_order = []

        for func_name, attr in self.__class__.__dict__.items():
            if hasattr(attr, "_is_rule"):
                for alt_meta in getattr(attr, "_rule_alts", []):
                    # alt_meta may contain a pattern with ::= left-hand side
                    parsed_rule = self._parse_pattern(alt_meta["pattern"])
                    # _parse_pattern now returns (maybe_rule_name, elems)
                    parsed_rule_name, parsed_elems = parsed_rule

                    # precedence: pattern-specified name > name kwarg > function name heuristic
                    rule_name = parsed_rule_name or alt_meta.get("rule_name")
                    if not rule_name:
                        if "_" in func_name:
                            rule_name = func_name.split("_", 1)[0]
                        else:
                            rule_name = func_name

                    alt = {
                        "pattern_raw": alt_meta["pattern"],
                        "elems": parsed_elems,
                        "prec": alt_meta["prec"],
                        "assoc": alt_meta["assoc"],
                        "unary": alt_meta["unary"],
                        "func": getattr(self, func_name),  # bound method
                        "transform": alt_meta.get("transform"),
                        "validate": alt_meta.get("validate"),
                        "capture_anon": alt_meta.get("capture_anon", False),
                    }
                    if rule_name not in self._rules:
                        self._rules[rule_name] = []
                        self._rule_order.append(rule_name)
                    self._rules[rule_name].append(alt)

        # Ensure empty-alternative alts are tried last (prevents zero-length seeds stealing precedence)
        for _rname, _alts in self._rules.items():
            _alts.sort(key=lambda a: 1 if not a.get("elems") else 0)

    # new pattern parser: supports:
    #  - <name:var> or <name?:var> (optional)
    #  - <name*:var> and <name+:var> (star/plus quantifiers inside angle brackets)
    #  - [ ... ]:var  zero-or-more (group)
    #  - { ... }:var  one-or-more (group)
    #  - optional var default: <name?:var=42>
    #  - literals in single/double quotes or bare punctuation tokens (',' etc.)
    #  - left-hand rule name via ::= e.g. "<expr> ::= <expr:left> '+' <expr:right>"
    def _parse_pattern(self, pattern: str):
        s = pattern
        n = len(s)

        # Detect top-level ::= (left-hand rule name) if present.
        # We do a simple split on the first occurrence of '::=' that isn't inside quotes.
        def find_top_level_assign(s):
            i = 0
            in_sq = False
            in_dq = False
            while i < len(s) - 2:
                ch = s[i]
                if ch == "'" and not in_dq:
                    in_sq = not in_sq
                    i += 1
                    continue
                if ch == '"' and not in_sq:
                    in_dq = not in_dq
                    i += 1
                    continue
                if not in_sq and not in_dq:
                    if s[i : i + 3] == "::=":
                        return i
                i += 1
            return -1

        assign_idx = find_top_level_assign(s)
        lhs_name = None
        rhs = s
        if assign_idx != -1:
            lhs = s[:assign_idx].strip()
            rhs = s[assign_idx + 3 :].strip()
            # if lhs is like "<expr>" extract expr, otherwise if plain word, use it
            if lhs.startswith("<") and lhs.endswith(">"):
                inner = lhs[1:-1].strip()
                if re.match(r"[A-Za-z_]\w*$", inner):
                    lhs_name = inner
            elif re.match(r"[A-Za-z_]\w*$", lhs):
                lhs_name = lhs

        s = rhs
        n = len(s)

        def skip_spaces(p):
            while p < n and s[p].isspace():
                p += 1
            return p

        ident_re = re.compile(r"[A-Za-z_]\w*")

        def parse_sequence(p, stop_char=None):
            elems = []
            p = skip_spaces(p)
            while p < n:
                if stop_char and s[p] == stop_char:
                    return elems, p
                c = s[p]
                if c == "[" or c == "{":
                    kind = "star" if c == "[" else "plus"
                    endc = "]" if c == "[" else "}"
                    p += 1
                    inner_elems, p = parse_sequence(p, stop_char=endc)
                    if p >= n or s[p] != endc:
                        raise ValueError(f"Unclosed {c} in pattern: {pattern}")
                    p += 1
                    p = skip_spaces(p)
                    var = None
                    default = None
                    if p < n and s[p] == ":":
                        p += 1
                        p = skip_spaces(p)
                        m = ident_re.match(s, p)
                        if not m:
                            raise ValueError(
                                f"Expected identifier after ':' in pattern: {pattern}"
                            )
                        var = m.group(0)
                        p = m.end()

                    # --- group sugar normalization ---
                    # If the user wrote a tail of the common form: [ SEP ITEM ]:name
                    # (where SEP is a literal or unnamed token and ITEM is a named ref),
                    # rewrite the group into a 'group_sugar' node that during matching
                    # only captures the ITEM values (ignoring separators).
                    group_node = ("group", kind, inner_elems, var)
                    if var is not None and len(inner_elems) == 2:
                        first, second = inner_elems[0], inner_elems[1]
                        # second must be a ref and should be capturable (named var or token with capture_by_default)
                        if second[0] == "ref":
                            sec_name, sec_var = second[1], second[2]
                            token_info = None
                            if sec_name in getattr(self, "_tokens", {}):
                                token_info = self._tokens[sec_name]
                            # consider token capturable if user named it, or if the token
                            # looks like a word-like token (contains \w or alnum in regex),
                            # or if the token explicitly has capture_by_default=True
                            tok_pattern = token_info[0].pattern if token_info is not None else ""
                            looks_wordlike = bool(re.search(r"\\w|[A-Za-z0-9]", tok_pattern))
                            sec_capturable = (sec_var is not None) or (
                                token_info is not None and (token_info[2] or looks_wordlike)
                            )
                    elems.append(group_node)
                    p = skip_spaces(p)
                    continue
                elif c == "<":
                    # find matching '>' (no nesting expected for refs)
                    end = p + 1
                    depth = 1
                    while end < n and depth > 0:
                        if s[end] == "<":
                            depth += 1
                        elif s[end] == ">":
                            depth -= 1
                        end += 1
                    if depth != 0:
                        raise ValueError(f"Unclosed < in pattern: {pattern}")
                    content = s[p + 1 : end - 1].strip()

                    # parse with regex: name, optional quant (? * +), optional :var[=default]
                    m = re.match(
                        r"^([A-Za-z_]\w*)([?*+]?)\s*(?::\s*([A-Za-z_]\w*(?:\s*=\s*(?:'[^']*'|\"[^\"]*\"|[^'\"]\S*))?))?$",
                        content,
                    )
                    if not m:
                        raise ValueError(
                            f"Invalid <...> content: '{content}' in pattern: {pattern}"
                        )
                    name = m.group(1)
                    quant_sym = m.group(2) or ""
                    varpart = m.group(3)

                    quant = None
                    if quant_sym == "?":
                        quant = "optional"
                    elif quant_sym == "*":
                        quant = "star"
                    elif quant_sym == "+":
                        quant = "plus"

                    var = None
                    default = None
                    if varpart:
                        # varpart could be "x" or "x=42" or "x='a b'"
                        if "=" in varpart:
                            varname, defraw = varpart.split("=", 1)
                            var = varname.strip()
                            ds = defraw.strip()
                            # try to literal_eval
                            try:
                                default = ast.literal_eval(ds)
                            except Exception:
                                # strip quotes if any or keep raw
                                if (ds.startswith("'") and ds.endswith("'")) or (
                                    ds.startswith('"') and ds.endswith('"')
                                ):
                                    default = ds[1:-1]
                                else:
                                    default = ds
                        else:
                            var = varpart.strip()

                    elems.append(("ref", name, var, quant, default))
                    p = end
                    p = skip_spaces(p)
                    continue
                elif c == "'" or c == '"':
                    quote = c
                    end = p + 1
                    lit_chars = []
                    if quote == "'":
                        # SQL-style doubling: '' -> single quote inside literal
                        while end < n:
                            if s[end] == "'":
                                # if doubled single quote -> append one and advance by 2
                                if end + 1 < n and s[end + 1] == "'":
                                    lit_chars.append("'")
                                    end += 2
                                    continue
                                # otherwise end of literal
                                end += 1
                                break
                            elif s[end] == "\\" and end + 1 < n:
                                # allow backslash escapes as well
                                lit_chars.append(s[end + 1])
                                end += 2
                            else:
                                lit_chars.append(s[end])
                                end += 1
                        else:
                            raise ValueError(
                                f"Unclosed string literal in pattern: {pattern}"
                            )
                    else:
                        # double-quoted: allow backslash escapes
                        while end < n:
                            if s[end] == "\\" and end + 1 < n:
                                lit_chars.append(s[end + 1])
                                end += 2
                            elif s[end] == quote:
                                end += 1
                                break
                            else:
                                lit_chars.append(s[end])
                                end += 1
                        else:
                            raise ValueError(
                                f"Unclosed string literal in pattern: {pattern}"
                            )
                    lit = "".join(lit_chars)
                    elems.append(("lit", lit))
                    p = skip_spaces(end)
                    continue
                else:
                    # bare token/literal chunk (e.g. commas or other punctuation)
                    m = re.match(r"[^<>\[\]\{\}\s]+", s[p:])
                    if not m:
                        raise ValueError(
                            f"Unexpected char '{s[p]}' in pattern: {pattern}"
                        )
                    tok = m.group(0)
                    elems.append(("lit", tok))
                    p += len(tok)
                    p = skip_spaces(p)
                    continue
            if stop_char:
                raise ValueError(
                    f"Unclosed group in pattern: {pattern} (expected '{stop_char}')"
                )
            return elems, p

        elems, _ = parse_sequence(0, stop_char=None)

        # --- Pattern linter/warnings ---
        # Warn if a named group may capture anonymous token values (likely a mistake).
        def walk_and_warn(items, in_group_named=False):
            for e in items:
                if e[0] == "group" or e[0] == "group_sugar":
                    # group node: ('group', kind, inner_elems, var) or sugar variant
                    varname = e[3] if e[0] == "group" else e[4]
                    inner = e[2] if e[0] == "group" else e[3]
                    if varname is not None:
                        # Warn if any inner element is a token ref with capture_by_default True and is unnamed
                        for ie in inner:
                            if ie[0] == "ref":
                                refname = ie[1]
                                refvar = ie[2]
                                if refvar is None and refname in getattr(
                                    self, "_tokens", {}
                                ):
                                    tokinfo = self._tokens[refname]
                                    if tokinfo[2]:
                                        warnings.warn(
                                            f"Pattern '{pattern}': group ':{varname}' contains unnamed token <{refname}> which has capture_by_default=True — this may capture punctuation tokens unexpectedly.",
                                            UserWarning,
                                        )
                                # NEW: warn if the inner ref itself has a named capture — nested named captures
                                if refvar is not None:
                                    warnings.warn(
                                        f"Pattern '{pattern}': group ':{varname}' contains an inner named capture '<{refname}:{refvar}>' — nested named captures inside a group can cause parsing to fail. Consider removing the inner ':name' or the group's name.",
                                        UserWarning,
                                    )
                    walk_and_warn(inner, in_group_named=(varname is not None))
                elif e[0] == "ref":
                    # nothing else to do here
                    continue
                else:
                    # lit etc
                    continue

        try:
            walk_and_warn(elems)
        except Exception:
            # linter should never fail the parse; if it does, ignore
            pass

        return lhs_name, elems

    # location helpers kept same
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
        # small lookahead to show what token or chars were actually found
        lookahead_len = 20
        found = text[pos : pos + lookahead_len]
        # trim whitespace in found for clarity
        found_disp = found.replace("\n", "\\n")
        found_part = f"\nFound: '{found_disp}'" if found_disp else ""
        return ParseError(
            f"{message}\nLine {line}, Column {col}:{exp_part}{found_part}\n{snippet}\n{caret}"
        )

    # public parse entrypoint kept same, but we initialize memo cache per-parse
    def parse(self, text: str, start_rule: Optional[str] = None):
        if start_rule is None:
            # Require an explicit 'top' rule in every grammar
            if "top" not in self._rules:
                raise RuntimeError(
                    "Grammar must define a 'top' rule as the required start symbol"
                )
            start_rule = "top"

        # memoization cache: key -> (value, pos) or 'INPROG'
        self._memo = {}

        # Skip initial whitespace so parsing starts at the first real token.
        start_pos = 0
        if self.skip_whitespace:
            while start_pos < len(text) and text[start_pos].isspace():
                start_pos += 1

        val, pos = self._match_rule(start_rule, text, start_pos, min_prec=0)

        # Allow trailing whitespace after successful parse.
        if self.skip_whitespace:
            while pos < len(text) and text[pos].isspace():
                pos += 1
        if pos != len(text):
            raise self._format_error("Unconsumed input", text, pos)
        return val

    # Core matcher: extended to handle 'ref' with optional quant and 'group' elems (star/plus)
    def _match_rule(
        self, rule_name: str, text: str, pos: int, min_prec: int = 0
    ) -> Tuple[Any, int]:
        if rule_name not in self._rules:
            raise self._format_error(f"Unknown rule '{rule_name}'", text, pos)

        # memoization check
        key = (rule_name, pos, min_prec)
        if hasattr(self, "_memo"):
            cached = self._memo.get(key, None)
            if cached is not None and cached != "INPROG":
                return cached
            if cached == "INPROG":
                # avoid using memo while left-recursive computation in progress
                pass

        # mark in-progress (to avoid infinite recursion in naive cycles)
        if hasattr(self, "_memo"):
            self._memo[key] = "INPROG"

        alts = self._rules[rule_name]
        orig_min = min_prec

        non_left = []
        left_rec = []
        prefix_unary = []
        postfix_unary = []
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

        expected: List[str] = []

        # helper to match a sequence of elements and return (captured_args_list, newpos)
        # on failure returns None
        def match_sequence(elems, start_pos, capture_anon=False):
            cur = start_pos
            captures = []
            for elem in elems:
                if self.skip_whitespace:
                    while cur < len(text) and text[cur].isspace():
                        cur += 1
                res = match_element(elem, cur, capture_anon=capture_anon)
                if res is None:
                    return None
                elem_caps, new_pos = res
                captures.extend(elem_caps)
                cur = new_pos
            return captures, cur

        # create readable description for an element (used in error hints)
        def elem_desc(e):
            if e[0] == "lit":
                return f"'{e[1]}'"
            if e[0] == "ref":
                return f"<{e[1]}>"
            if e[0] == "group" or e[0] == "group_sugar":
                return "group"
            return "item"

        # match a single element; return (captured_vals_list, newpos) or None on failure
        def match_element(elem, cur, capture_anon=False):
            typ = elem[0]
            if typ == "lit":
                lit = elem[1]
                if text.startswith(lit, cur):
                    return ([], cur + len(lit))
                else:
                    expected.append(lit)
                    return None

            elif typ == "ref":
                name, var, quant, default = elem[1], elem[2], elem[3], elem[4]
                # ---- token case ----
                if name in self._tokens:
                    regex, convert, tok_capture_by_default = self._tokens[name]
                    m = regex.match(text, cur)
                    if not m:
                        if quant == "optional":
                            if var:
                                val = default if default is not None else None
                                return ([val], cur)
                            elif capture_anon:
                                # anonymous capture requested for optional token -> None
                                return ([None], cur)
                            else:
                                return ([], cur)
                        if quant in ("star", "plus"):
                            # repetition for tokens:
                            items = []
                            curpos = cur
                            first = regex.match(text, curpos)
                            if not first:
                                if quant == "plus":
                                    expected.append(f"one or more {elem_desc(elem)}")
                                    return None
                                else:
                                    # star, zero occurrences
                                    if var or capture_anon:
                                        return ([[]], curpos)
                                    else:
                                        return ([], curpos)
                            while True:
                                m2 = regex.match(text, curpos)
                                if not m2:
                                    break
                                raw = m2.group(0)
                                curpos = m2.end()
                                items.append(convert(raw) if convert else raw)
                                if curpos == m2.start():
                                    break
                            if var or tok_capture_by_default or capture_anon:
                                return ([items], curpos)
                            else:
                                return ([], curpos)
                        expected.append(f"<{name}>")
                        return None
                    raw = m.group(0)
                    newcur = m.end()
                    val = convert(raw) if convert else raw
                    if quant in ("star", "plus"):
                        items = [val]
                        curpos = newcur
                        while True:
                            m2 = regex.match(text, curpos)
                            if not m2:
                                break
                            raw2 = m2.group(0)
                            curpos = m2.end()
                            items.append(convert(raw2) if convert else raw2)
                            if curpos == m2.start():
                                break
                        if var or tok_capture_by_default or capture_anon:
                            return ([items], curpos)
                        else:
                            return ([], curpos)
                    # single token match: capture if explicit var, token default capture, or anonymous-capture requested
                    if var or tok_capture_by_default or capture_anon:
                        return ([val], newcur)
                    else:
                        return ([], newcur)

            elif typ == "group_sugar":
                # sugar node: ('group_sugar', kind, sep_elem, item_elem, var)
                kind, sep_elem, item_elem, varname = elem[1], elem[2], elem[3], elem[4]
                items = []
                curpos = cur

                def match_once(p):
                    # first match sep, then item
                    # sep shouldn't be captured — pass capture_anon=False
                    res1 = match_element(sep_elem, p, capture_anon=False)
                    if res1 is None:
                        return None
                    _, pos_after_sep = res1
                    res2 = match_element(item_elem, pos_after_sep, capture_anon=False)
                    if res2 is None:
                        return None
                    item_caps, newp = res2
                    # normalize item value
                    if len(item_caps) == 0:
                        item = None
                    elif len(item_caps) == 1:
                        item = item_caps[0]
                    else:
                        item = tuple(item_caps)
                    return item, newp

                first_match = match_once(curpos)
                if kind == "plus":
                    if first_match is None:
                        if sep_elem:
                            expected.append(f"one or more {elem_desc(sep_elem)}")
                        else:
                            expected.append("one or more (group)")
                        return None
                    item, curpos = first_match
                    items.append(item)
                    while True:
                        nxt = match_once(curpos)
                        if nxt is None:
                            break
                        item, curpos = nxt
                        items.append(item)
                else:  # star
                    while True:
                        nxt = match_once(curpos)
                        if nxt is None:
                            break
                        item, curpos = nxt
                        items.append(item)
                if varname:
                    return ([items], curpos)
                else:
                    return ([], curpos)

            elif typ == "group":
                kind, inner_elems, var = elem[1], elem[2], elem[3]
                items = []
                curpos = cur

                # helper to attempt one iteration of the inner sequence
                def match_once(p):
                    # If this group itself has a capture name (var), then we enable
                    # anonymous capture inside so that unnamed rule refs inside become captures.
                    inner_capture_anon = capture_anon or (var is not None)

                    res = match_sequence(
                        inner_elems, p, capture_anon=inner_capture_anon
                    )
                    if res is None:
                        return None
                    inner_caps, newp = res

                    # prevent zero-length inner matches (would cause infinite loops)
                    if newp == p:
                        return None

                    # Convert inner captures into a sensible item:
                    if len(inner_caps) == 0:
                        item = None
                    elif len(inner_caps) == 1:
                        item = inner_caps[0]
                    else:
                        item = tuple(inner_caps)
                    return item, newp

                first_match = match_once(curpos)
                if kind == "plus":
                    if first_match is None:
                        # better error hint: what group expected
                        if inner_elems:
                            expected.append(f"one or more {elem_desc(inner_elems[0])}")
                        else:
                            expected.append("one or more (group)")
                        return None
                    item, curpos = first_match
                    items.append(item)
                    while True:
                        nxt = match_once(curpos)
                        if nxt is None:
                            break
                        item, curpos = nxt
                        items.append(item)
                else:  # star (zero-or-more)
                    while True:
                        nxt = match_once(curpos)
                        if nxt is None:
                            break
                        item, curpos = nxt
                        items.append(item)
                if var:
                    return ([items], curpos)
                elif capture_anon:
                    return ([items], curpos)
                else:
                    return ([], curpos)
            else:
                return None

        # helper to run transform/validate and call semantic function (centralized)
        # RETURNS: (ok: bool, value)
        def call_semantic(alt, raw_args, start_pos):
            transform_fn = alt.get("transform")
            validate_fn = alt.get("validate")
            args_for_call = raw_args
            if transform_fn:
                try:
                    transformed = transform_fn(list(raw_args))
                except ParseError:
                    # rethrow parse errors from transform as is
                    raise
                except Exception as e:
                    raise self._format_error(
                        f"Error in transform for rule '{rule_name}': {e}",
                        text,
                        start_pos,
                    )
                # None means treat as alt not matching
                if transformed is None:
                    return (False, None)
                # normalize into iterable of args
                if isinstance(transformed, (list, tuple)):
                    args_for_call = list(transformed)
                else:
                    # single value -> wrap
                    args_for_call = [transformed]
            if validate_fn:
                try:
                    ok = validate_fn(list(raw_args))
                except ParseError:
                    raise
                except Exception as e:
                    raise self._format_error(
                        f"Error in validate for rule '{rule_name}': {e}",
                        text,
                        start_pos,
                    )
                if not ok:
                    # treat as no match
                    return (False, None)
            # call semantic action
            try:
                # attempt to match function signature if bound method expects fewer args
                sig = inspect.signature(alt["func"])
                # build a simple positional dispatch: if the function expects N params
                # (excluding self for bound methods), supply only the first N args.
                params = list(sig.parameters.values())
                # bound method: first parameter is usually 'self' -> omit it
                if params and params[0].name == "self":
                    params = params[1:]
                expected_param_count = len(
                    [
                        p
                        for p in params
                        if p.kind
                        in (
                            inspect.Parameter.POSITIONAL_ONLY,
                            inspect.Parameter.POSITIONAL_OR_KEYWORD,
                        )
                    ]
                )
                if expected_param_count != 0:
                    call_args = args_for_call[:expected_param_count]
                else:
                    call_args = args_for_call
                val = alt["func"](*call_args)
                return (True, val)
            except Exception as e:
                raise self._format_error(
                    f"Error in semantic action for rule '{rule_name}': {e}",
                    text,
                    start_pos,
                )

        # Helper: match a plain alternative (no special left-rec growth) starting at pos.
        def try_match_alt(alt, start_pos):
            cur = start_pos
            args = []
            for elem in alt["elems"]:
                if self.skip_whitespace:
                    while cur < len(text) and text[cur].isspace():
                        cur += 1
                res = match_element(
                    elem, cur, capture_anon=alt.get("capture_anon", False)
                )
                if res is None:
                    return None
                elem_caps, new_pos = res
                args.extend(elem_caps)
                cur = new_pos
            # apply transform/validate and call semantic; if transform returns None or validate False -> treat as no match
            sem_ok, sem_val = call_semantic(alt, args, start_pos)
            if not sem_ok:
                return None
            return sem_val, cur

        # 1) Try prefix unary forms first (they create a seed)
        seeds = []
        for alt in prefix_unary:
            elems = alt["elems"]
            cur = pos
            args = []
            ok = True
            for elem in elems:
                if self.skip_whitespace:
                    while cur < len(text) and text[cur].isspace():
                        cur += 1
                if elem[0] == "ref" and elem[1] == rule_name:
                    name = elem[1]
                    var = elem[2]
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
                    elif alt.get("capture_anon", False):
                        args.append(inner)
                else:
                    res = match_element(
                        elem, cur, capture_anon=alt.get("capture_anon", False)
                    )
                    if res is None:
                        ok = False
                        break
                    elem_caps, new_pos = res
                    args.extend(elem_caps)
                    cur = new_pos
            if ok:
                sem_ok, sem_val = call_semantic(alt, args, pos)
                if sem_ok:
                    seeds.append((alt, sem_val, cur))
                # else transform/validate rejected alt -> not a seed

        # 2) Try non-left alternatives as seeds (prefer non-empty alts first)
        if not seeds:
            # prefer alts that actually consume something (elems != [])
            sorted_non_left = sorted(non_left, key=lambda a: 0 if a["elems"] else 1)
            for alt in sorted_non_left:
                res = try_match_alt(alt, pos)
                if res is not None:
                    val, newpos = res
                    seeds.append((alt, val, newpos))
                    break

        if not seeds:
            # nothing matched as seed -> failure
            # clean memo in-progress
            if hasattr(self, "_memo"):
                self._memo[key] = None
            raise self._format_error(
                f"Failed to match rule '{rule_name}'",
                text,
                pos,
                expected=list(dict.fromkeys(expected)),
            )

        # We'll grow each successful seed via left-recursive alts and postfix unary alts
        seed_alt, left_val, curpos = seeds[0]

        # growth loop: repeatedly try to apply left-recursive alts and postfix unary alts
        while True:
            progressed = False

            # First: postfix unary alternatives (they are like left-rec tails with no new LHS)
            for alt in postfix_unary:
                elems = alt["elems"]
                if not (elems and elems[0][0] == "ref" and elems[0][1] == rule_name):
                    continue
                tail = elems[1:]
                cur = curpos
                args = [left_val]
                ok = True
                for elem in tail:
                    if self.skip_whitespace:
                        while cur < len(text) and text[cur].isspace():
                            cur += 1
                    res = match_element(
                        elem, cur, capture_anon=alt.get("capture_anon", False)
                    )
                    if res is None:
                        ok = False
                        break
                    elem_caps, new_pos = res
                    args.extend(elem_caps)
                    cur = new_pos
                if ok:
                    sem_ok, sem_val = call_semantic(alt, args, curpos)
                    if not sem_ok:
                        # transform/validate rejected this application -> skip
                        continue
                    left_val = sem_val
                    curpos = cur
                    progressed = True
                    break
            if progressed:
                continue

            # Second: left-recursive binary-like alternatives
            left_rec_sorted = sorted(
                [a for a in left_rec if a["prec"] is not None], key=lambda x: -x["prec"]
            )
            applied_any = False
            for alt in left_rec_sorted:
                p = alt["prec"]
                assoc = alt["assoc"]
                if p < orig_min:
                    continue
                tail = alt["elems"][1:]
                cur = curpos
                args = [left_val]
                ok = True
                for elem in tail:
                    if self.skip_whitespace:
                        while cur < len(text) and text[cur].isspace():
                            cur += 1
                    if elem[0] == "ref" and elem[1] == rule_name:
                        rhs_min = p if assoc == "right" else (p + 1)
                        try:
                            val, new_pos = self._match_rule(
                                elem[1], text, cur, min_prec=rhs_min
                            )
                        except ParseError:
                            ok = False
                            break
                        cur = new_pos
                        if elem[2]:
                            args.append(val)
                        elif alt.get("capture_anon", False):
                            args.append(val)
                    else:
                        res = match_element(
                            elem, cur, capture_anon=alt.get("capture_anon", False)
                        )
                        if res is None:
                            ok = False
                            break
                        elem_caps, new_pos = res
                        args.extend(elem_caps)
                        cur = new_pos
                if ok:
                    sem_ok, sem_val = call_semantic(alt, args, curpos)
                    if not sem_ok:
                        # transform/validate rejected this alt application
                        continue
                    left_val = sem_val
                    curpos = cur
                    applied_any = True
                    break

            if applied_any:
                progressed = True
                continue

            if not progressed:
                break

        # store memoized result
        if hasattr(self, "_memo"):
            self._memo[key] = (left_val, curpos)

        return left_val, curpos
