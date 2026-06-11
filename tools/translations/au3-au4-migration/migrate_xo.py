#!/usr/bin/env python3
"""Rewrite XO/XXO/XC/Verbatim/_() / .Format() in au3/ and src/ to
TranslatableString + au3::trc. Dry-run by default; --apply writes.
Emits manual_*_review.txt files for sites the rewriter cannot handle."""

from __future__ import annotations

import argparse
import csv
import difflib
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]
MIG_DIR = REPO / "tools" / "translations" / "au3-au4-migration"
SCAN_PATHS = REPO / "tools" / "translations" / "au4_scan_paths.txt"
CTX_CSV = MIG_DIR / "au3_contexts.csv"

MANUAL_MACRO = MIG_DIR / "manual_macro_review.txt"
MANUAL_FORMAT = MIG_DIR / "manual_format_review.txt"
MANUAL_CTOR = MIG_DIR / "manual_ctor_review.txt"
MANUAL_GETTEXT = MIG_DIR / "manual_gettext_review.txt"


# ----- low-level helpers -------------------------------------------------- #

# Gettext-style placeholder pattern. Captures: flags, width, .precision, length,
# specifier. Conservative -- only specifiers we expect in i18n strings.
_PH_RE = re.compile(
    r"%(?P<percent>%)|"
    r"%(?P<flags>[-+# 0]*)(?P<width>\d*)(?P<dot>\.?)(?P<prec>\d*)"
    r"(?P<length>ll|l|h|hh|j|z|t|L)?"
    r"(?P<spec>[sdfgieuxoEXc])"
)


def positionalize(literal: str) -> tuple[str, int]:
    """Rewrite gettext %s/%d/etc. to Qt %1, %2, .... Returns (text, count).
    Preserves `%%`. Counter starts at 1."""
    counter = [0]

    def repl(m: re.Match) -> str:
        if m.group("percent"):
            return "%%"
        counter[0] += 1
        return f"%{counter[0]}"

    return _PH_RE.sub(repl, literal), counter[0]


def parse_string_literal(text: str, i: int) -> tuple[str | None, int]:
    """Parse a C string literal at text[i], joining adjacent "foo" "bar"
    pieces. Skips whitespace + comments first. Returns (text or None, end_i)."""
    n = len(text)
    i = _skip_ws_and_comments(text, i, n)

    if i >= n or text[i] != '"':
        return None, i

    pieces: list[str] = []
    while i < n and text[i] == '"':
        i += 1
        start = i
        while i < n:
            ch = text[i]
            if ch == "\\" and i + 1 < n:
                i += 2
                continue
            if ch == '"':
                break
            i += 1
        if i >= n:
            return None, i
        pieces.append(text[start:i])
        i += 1  # past closing quote
        i = _skip_ws_and_comments(text, i, n)

    # Keep escapes encoded; rewriter re-emits the literal characters.
    return "".join(pieces), i


def _skip_ws_and_comments(text: str, i: int, n: int) -> int:
    """Skip whitespace + // line comments + /* block comments */. Returns new i."""
    while i < n:
        ch = text[i]
        if ch in " \t\r\n":
            i += 1
            continue
        if ch == "/" and i + 1 < n:
            if text[i + 1] == "/":
                # line comment
                i += 2
                while i < n and text[i] != "\n":
                    i += 1
                continue
            if text[i + 1] == "*":
                # block comment
                i += 2
                while i + 1 < n and not (text[i] == "*" and text[i + 1] == "/"):
                    i += 1
                i = min(i + 2, n)
                continue
        break
    return i


def find_matching_paren(text: str, open_pos: int) -> int:
    """Find ')' matching text[open_pos] == '('. Respects strings/chars/comments.
    Returns -1 on no match."""
    n = len(text)
    depth = 0
    i = open_pos
    while i < n:
        ch = text[i]
        if ch == "(":
            depth += 1
        elif ch == ")":
            depth -= 1
            if depth == 0:
                return i
        elif ch == '"':
            # skip string literal
            i += 1
            while i < n:
                c2 = text[i]
                if c2 == "\\" and i + 1 < n:
                    i += 2
                    continue
                if c2 == '"':
                    break
                i += 1
        elif ch == "'":
            # skip char literal
            i += 1
            while i < n:
                c2 = text[i]
                if c2 == "\\" and i + 1 < n:
                    i += 2
                    continue
                if c2 == "'":
                    break
                i += 1
        elif ch == "/" and i + 1 < n:
            if text[i + 1] == "/":
                # line comment
                while i < n and text[i] != "\n":
                    i += 1
                continue
            if text[i + 1] == "*":
                # block comment
                i += 2
                while i + 1 < n and not (text[i] == "*" and text[i + 1] == "/"):
                    i += 1
                i += 1  # past trailing '/'
        i += 1
    return -1


def line_col(text: str, pos: int) -> tuple[int, int]:
    line = text.count("\n", 0, pos) + 1
    last_nl = text.rfind("\n", 0, pos)
    col = pos - last_nl
    return line, col


# ----- comment/region helpers -------------------------------------------- #

def is_inside_line_comment(text: str, pos: int) -> bool:
    """Cheap heuristic: pos is inside a `// ... \n` comment on the same line."""
    line_start = text.rfind("\n", 0, pos) + 1
    line_prefix = text[line_start:pos]
    # Strip strings out of the prefix so a `//` inside a literal doesn't count.
    in_str = False
    i = 0
    while i < len(line_prefix):
        ch = line_prefix[i]
        if in_str:
            if ch == "\\" and i + 1 < len(line_prefix):
                i += 2
                continue
            if ch == '"':
                in_str = False
        else:
            if ch == '"':
                in_str = True
            elif ch == "/" and i + 1 < len(line_prefix) and line_prefix[i + 1] == "/":
                return True
        i += 1
    return False


# ----- Pass A+B (coupled) ------------------------------------------------ #

_MACRO_RE = re.compile(r"\b(XO|XXO|XC|XP|XPC|Verbatim|wxGetTranslation)\s*\(")
_GETTEXT_RE = re.compile(r"(?<![A-Za-z0-9_])_\(")
_FORMAT_RE = re.compile(r"\.Format\s*\(")
_DEFINE_RE = re.compile(r"^\s*#\s*define\b")
# Macro DEFINITIONS we must never rewrite (rewriting them would either
# create a tautology -- XO defined as muse::TS(ctx, XO_arg) -- or break the
# fallback alias chain XXO->XO).
_DEFINE_MACRO_RE = re.compile(r"^\s*#\s*define\s+(XO|XXO|XC|XP|XPC|Verbatim)\b")


def _line_starts_with_define(text: str, pos: int) -> bool:
    """True if pos is inside a `#define XO/XXO/...` line; those macro
    definitions must not be rewritten (would create tautology)."""
    line_start = text.rfind("\n", 0, pos) + 1
    line_end = text.find("\n", pos)
    if line_end == -1:
        line_end = len(text)
    return bool(_DEFINE_MACRO_RE.match(text[line_start:line_end]))


def rewrite_pass_ab(content: str, file_rel: str, context: str,
                    manual_macro: list[str], manual_format: list[str]) -> str:
    """Macro expansion + placeholder normalisation.
    XO/XXO/XC/Verbatim/_() / wxGetTranslation -> muse::TranslatableString.
    %s/%d/%lld/... -> %1, %2, ... XP/XPC always flagged."""
    out: list[str] = []
    i = 0
    n = len(content)

    while i < n:
        # Fast forward to next macro or gettext invocation
        m = _MACRO_RE.search(content, i)
        m_get = _GETTEXT_RE.search(content, i)
        # Format calls handled here only when they immediately follow a
        # macro rewrite; standalone .Format calls are still handled by
        # the receiver-recognition pass below.

        # Find earliest of the macro/gettext matches
        candidates = []
        if m is not None:
            candidates.append((m.start(), "macro", m))
        if m_get is not None:
            candidates.append((m_get.start(), "gettext", m_get))
        if not candidates:
            out.append(content[i:])
            break
        candidates.sort()
        start, kind, hit = candidates[0]

        if start > i:
            out.append(content[i:start])
        i = start

        # Skip if this site is inside a #define line or a line comment.
        if _line_starts_with_define(content, start) or is_inside_line_comment(content, start):
            # emit one char and continue
            out.append(content[i])
            i += 1
            continue

        if kind == "gettext":
            # Pass D handles this -- leave alone here.
            out.append(content[i])
            i += 1
            continue

        # macro: hit.group(1) is the macro name; '(' is right before hit.end()
        macro = hit.group(1)
        open_paren = hit.end() - 1
        close_paren = find_matching_paren(content, open_paren)
        if close_paren == -1:
            manual_macro.append(f"{file_rel}:{line_col(content, start)[0]}: unmatched '(' after {macro}")
            out.append(content[i])
            i += 1
            continue

        args_blob = content[open_paren + 1:close_paren]

        # XP / XPC always to manual review -- too risky to rewrite mechanically.
        if macro in ("XP", "XPC"):
            line = line_col(content, start)[0]
            manual_macro.append(f"{file_rel}:{line}: {macro}{args_blob[:80]}... -> hand-resolve (XP/XPC plural)")
            out.append(content[start:close_paren + 1])
            i = close_paren + 1
            continue

        # wxGetTranslation: Pass D handles this -- leave alone here.
        if macro == "wxGetTranslation":
            out.append(content[i])
            i += 1
            continue

        # Parse arguments. For XO/XXO/Verbatim: one arg; for XC: two args.
        first, after_first = parse_string_literal(args_blob, 0)

        replacement: str | None = None
        if macro in ("XO", "XXO"):
            if first is None:
                # non-literal arg -> manual review
                manual_macro.append(
                    f"{file_rel}:{line_col(content, start)[0]}: {macro}({args_blob[:80]}...) -- non-literal arg"
                )
                out.append(content[start:close_paren + 1])
                i = close_paren + 1
                continue
            normalised, _ = positionalize(first)
            literal = '"' + normalised + '"'
            replacement = f'muse::TranslatableString("{context}", {literal})'
        elif macro == "XC":
            # XC takes (string_literal, string_literal_disamb)
            if first is None:
                manual_macro.append(
                    f"{file_rel}:{line_col(content, start)[0]}: XC({args_blob[:80]}...) -- non-literal first arg"
                )
                out.append(content[start:close_paren + 1])
                i = close_paren + 1
                continue
            # skip comma
            rest = args_blob[after_first:].lstrip()
            if not rest.startswith(","):
                manual_macro.append(
                    f"{file_rel}:{line_col(content, start)[0]}: XC({args_blob[:80]}...) -- missing comma"
                )
                out.append(content[start:close_paren + 1])
                i = close_paren + 1
                continue
            rest = rest[1:]
            second, _after = parse_string_literal(rest, 0)
            if second is None:
                manual_macro.append(
                    f"{file_rel}:{line_col(content, start)[0]}: XC({args_blob[:80]}...) -- non-literal disamb"
                )
                out.append(content[start:close_paren + 1])
                i = close_paren + 1
                continue
            normalised, _ = positionalize(first)
            replacement = (
                f'muse::TranslatableString("{context}", "{normalised}", "{second}")'
            )
        elif macro == "Verbatim":
            # Verbatim("lit") or Verbatim(wxExpr).
            #   - Literal string: emit muse-native untranslatable() with a
            #     const char* -- muse stays wx-agnostic.
            #   - Variable / expression: emit au3::untranslatable(EXPR) from
            #     the au3-side bridge header. Verbatim is exclusively a
            #     wxString convention in au3, so the arg is always wxString-
            #     compatible. The bridge function routes through UTF-8
            #     wxToMuse so non-ASCII bytes survive on Windows.
            if first is not None and after_first == len(args_blob.rstrip()):
                replacement = f'muse::TranslatableString::untranslatable("{first}")'
            else:
                if "\n" in args_blob:
                    arg_blob_text = args_blob
                else:
                    arg_blob_text = args_blob.strip()
                replacement = f'au3::untranslatable({arg_blob_text})'

        if replacement is None:
            out.append(content[start:close_paren + 1])
            i = close_paren + 1
            continue

        # Look ahead: is there a .Format(...) chained?
        next_pos = close_paren + 1
        m_fmt = _FORMAT_RE.match(content, next_pos)
        if m_fmt:
            fmt_open = m_fmt.end() - 1
            fmt_close = find_matching_paren(content, fmt_open)
            if fmt_close == -1:
                manual_format.append(
                    f"{file_rel}:{line_col(content, next_pos)[0]}: unmatched '(' after .Format"
                )
                out.append(replacement)
                i = next_pos
                continue
            # Rewrite .Format(a, b, c) to .arg(a).arg(b).arg(c) (split on
            # top-level commas inside fmt args).
            fmt_args = split_top_level_commas(content[fmt_open + 1:fmt_close])
            # The literal we emitted in `replacement` was already placeholder-
            # normalised in Pass A. Verify count matches.
            ph_count = replacement.count("%1") + replacement.count("%2") + 0  # placeholder
            # Better: re-positionalise the literal we used and count.
            # We already did positionalize above for XO/XXO/XC; for Verbatim
            # with a literal, count via _PH_RE on the literal portion.
            actual_ph = count_placeholders(first if first is not None else "")
            if first is not None and actual_ph != len(fmt_args):
                manual_format.append(
                    f"{file_rel}:{line_col(content, next_pos)[0]}: "
                    f"{macro}(...).Format(...) -- placeholder count {actual_ph} != arg count {len(fmt_args)}"
                )
            arg_chain = "".join(f".arg({a.strip()})" for a in fmt_args)
            replacement_with_format = replacement + arg_chain
            out.append(replacement_with_format)
            i = fmt_close + 1
            continue

        out.append(replacement)
        i = close_paren + 1

    return "".join(out)


def count_placeholders(literal: str) -> int:
    """Count gettext-style placeholders (after potential post-positionalize, all %N)."""
    # Count gettext-style (non-%% percent followed by spec) before normalisation,
    # OR Qt-style positional %1, %2 after.
    total = 0
    for m in _PH_RE.finditer(literal):
        if m.group("percent"):
            continue
        total += 1
    return total


_COMMA_SPLIT_RE = re.compile(r",")


def split_top_level_commas(blob: str) -> list[str]:
    """Split `blob` on top-level commas (respecting parens/braces/strings)."""
    out: list[str] = []
    depth = 0
    last = 0
    i = 0
    n = len(blob)
    while i < n:
        ch = blob[i]
        if ch in "([{":
            depth += 1
        elif ch in ")]}":
            depth -= 1
        elif ch == '"':
            i += 1
            while i < n:
                c2 = blob[i]
                if c2 == "\\" and i + 1 < n:
                    i += 2
                    continue
                if c2 == '"':
                    break
                i += 1
        elif ch == "'":
            i += 1
            while i < n:
                c2 = blob[i]
                if c2 == "\\" and i + 1 < n:
                    i += 2
                    continue
                if c2 == "'":
                    break
                i += 1
        elif ch == "," and depth == 0:
            out.append(blob[last:i])
            last = i + 1
        i += 1
    if blob[last:].strip():
        out.append(blob[last:])
    return out


# ----- Pass B (standalone) -- receiver-recognised .Format ----------------- #

_RECEIVER_PATTERNS = [
    # Already-rewritten Pass A site
    re.compile(r"muse::TranslatableString\([^)]*\)"),
    re.compile(r"muse::TranslatableString::untranslatable\([^)]*\)"),
    # Variable receiver (any identifier) -- only safe if TranslatableString-typed
    # (we accept these and emit; placeholder normalisation only applies to
    # literal-source sites which Pass A already handled).
]


def rewrite_pass_b_standalone(content: str, file_rel: str,
                              manual_format: list[str]) -> str:
    """Flag standalone .Format calls (var.Format / chains) for review.
    Auto-rewriting would risk hitting wxString::Format on a wxString variable
    misclassified as TranslatableString."""
    for m in _FORMAT_RE.finditer(content):
        pos = m.start()
        if _line_starts_with_define(content, pos):
            continue
        if is_inside_line_comment(content, pos):
            continue
        # Find the receiver: walk backward over identifier/paren chain.
        end = pos
        start = end
        depth = 0
        while start > 0:
            ch = content[start - 1]
            if ch in ")]}":
                depth += 1
                start -= 1
                continue
            if ch in "([{":
                if depth == 0:
                    break
                depth -= 1
                start -= 1
                continue
            if depth > 0:
                start -= 1
                continue
            if ch.isalnum() or ch in "_.>:":
                start -= 1
                continue
            break
        receiver = content[start:end].strip()

        # Skip if receiver is already known-TS-shape (a muse::TranslatableString...)
        # -- Pass A+B already chained .arg into those. The text replacement is
        # already in `content` post-Pass A, so a Pass A site followed by .Format
        # never reaches here (Pass A consumed the .Format).
        #
        # The remaining cases are: identifier (variable), or chains like
        # XO(...).Stripped().Format() -- none of which Pass A merged.
        if not receiver:
            continue
        # Known TS receiver shapes -- Pass A+B and Pass C/D already handled.
        if "muse::TranslatableString" in receiver:
            continue
        if "muse::trc" in receiver:
            continue
        # Obviously non-TS receivers -- wxString::Format static, wxDateTime,
        # wxFileName methods, etc. Skip flagging; the original code worked
        # against these wx types and stays valid.
        non_ts_prefixes = (
            "wxString::", "wxString(", "wxDateTime",
            "wxFileName", "wxStandardPaths", "wxRegEx",
        )
        if receiver.startswith(non_ts_prefixes):
            continue
        # Bare identifiers ending with `dt`, `date`, `time`, `path`, `name`,
        # `format` etc. -- too noisy to flag here; rely on compile to surface
        # any real type mismatch.
        if "::" not in receiver and "." not in receiver and "(" not in receiver:
            continue
        # FromUTF8 chain -- Pass D produced this and Pass A+B's literal
        # detection above already handled the explicit gettext+.Format pattern.
        if receiver.startswith("wxString::FromUTF8"):
            # Leave this as a real flag -- the wxString::Format-on-instance hazard.
            manual_format.append(
                f"{file_rel}:{line_col(content, pos)[0]}: wxString::FromUTF8(...).Format(...) "
                f"-- rewrite as au3::museToWx(muse::trc(...).arg(...))"
            )
            continue
        manual_format.append(
            f"{file_rel}:{line_col(content, pos)[0]}: .Format on receiver '{receiver}' "
            f"-- hand-resolve as .arg(...) chain"
        )
    return content


# ----- Pass C -- method renames + brace-init review ----------------------- #

_RENAME_PAIRS = [
    (re.compile(r"\.MSGID\s*\(\s*\)"), ".msgid()"),
    (re.compile(r"\.Stripped\s*\("), ".stripped("),
    (re.compile(r"\.Debug\s*\(\s*\)"), ".debugStr()"),
]


def rewrite_pass_c(content: str, file_rel: str,
                   manual_ctor: list[str]) -> str:
    """Method renames (.MSGID->.msgid, .Stripped->.stripped, .Debug->.debugStr)
    and TranslatableString{...} brace-init review (non-literal first arg flagged)."""
    new = content
    for pat, repl in _RENAME_PAIRS:
        new = pat.sub(repl, new)

    # Brace-init review + auto-rewrite where safe.
    brace_re = re.compile(r"\bTranslatableString\s*\{")
    cursor = 0
    out_parts: list[str] = []
    while True:
        m = brace_re.search(new, cursor)
        if m is None:
            out_parts.append(new[cursor:])
            break
        open_brace = m.end() - 1
        close_brace = _find_matching_brace(new, open_brace)
        if close_brace == -1:
            out_parts.append(new[cursor:m.end()])
            cursor = m.end()
            continue
        inner = new[open_brace + 1:close_brace].strip()
        out_parts.append(new[cursor:m.start()])

        if inner == "":
            # Default construction -- safe.
            out_parts.append(new[m.start():close_brace + 1])
            cursor = close_brace + 1
            continue

        args = split_top_level_commas(inner)
        first_arg = args[0].strip() if args else ""

        if first_arg.startswith('"'):
            # String literal -- leave as-is (XO/XXO Pass A would normally
            # handle these; brace-init slip-through is rare).
            out_parts.append(new[m.start():close_brace + 1])
            cursor = close_brace + 1
            continue

        if first_arg.startswith("wxT("):
            # wxT(...) literal -- wxString-typed, route through au3 helper
            # so muse stays wx-agnostic.
            inner_str = first_arg[len("wxT("):-1] if first_arg.endswith(")") else first_arg
            out_parts.append(f"au3::untranslatable(wxT({inner_str}))")
            cursor = close_brace + 1
            continue

        rest = args[1:]
        if rest and all(r.strip() in ("", "{}") for r in rest):
            # `TranslatableString{ X, {} }` -- au3 idiom (msgid + empty formatter)
            # where X is typically a wxString variable. Route through the
            # au3-side helper so muse stays wx-agnostic.
            out_parts.append(f"au3::untranslatable({first_arg})")
            cursor = close_brace + 1
            continue
        if not rest:
            # Single-arg brace-init: ambiguous -- could be copy-construction
            # from another TranslatableString, or wxString-to-msgid conversion.
            # Leave as-is and let the compiler resolve.
            out_parts.append(new[m.start():close_brace + 1])
            cursor = close_brace + 1
            continue

        line = line_col(new, m.start())[0]
        snippet = new[m.start():close_brace + 1].replace("\n", " ")
        manual_ctor.append(
            f"{file_rel}:{line}: {snippet[:120]}... -- non-literal first arg in TranslatableString{{}} brace-init"
        )
        out_parts.append(new[m.start():close_brace + 1])
        cursor = close_brace + 1

    new = "".join(out_parts)
    return new


def _find_matching_brace(text: str, open_pos: int) -> int:
    n = len(text)
    depth = 0
    i = open_pos
    while i < n:
        ch = text[i]
        if ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return i
        elif ch == '"':
            i += 1
            while i < n:
                c2 = text[i]
                if c2 == "\\" and i + 1 < n:
                    i += 2
                    continue
                if c2 == '"':
                    break
                i += 1
        elif ch == "'":
            i += 1
            while i < n:
                c2 = text[i]
                if c2 == "\\" and i + 1 < n:
                    i += 2
                    continue
                if c2 == "'":
                    break
                i += 1
        i += 1
    return -1


# ----- Pass D -- gettext _() and wxGetTranslation ------------------------- #

def rewrite_pass_d(content: str, file_rel: str, context: str,
                   manual_gettext: list[str]) -> str:
    """Rewrite _("lit") -> wxString::FromUTF8(muse::trc(...)). Special-cases
    wxString::Format(_(...), args) to avoid the static-on-instance trap."""
    # First handle wxString::Format(_("lit"...), args) -- only when _("lit") is
    # the first arg, literal, single-call.
    out_parts: list[str] = []
    i = 0
    n = len(content)
    fmt_call_re = re.compile(r"wxString::Format\s*\(\s*_\s*\(")
    while True:
        m = fmt_call_re.search(content, i)
        if m is None:
            break
        if _line_starts_with_define(content, m.start()) or is_inside_line_comment(content, m.start()):
            out_parts.append(content[i:m.end()])
            i = m.end()
            continue
        out_parts.append(content[i:m.start()])
        # m matches up through the `(` after `_`. So the next thing is the literal.
        gettext_open = m.end() - 1
        gettext_close = find_matching_paren(content, gettext_open)
        if gettext_close == -1:
            manual_gettext.append(
                f"{file_rel}:{line_col(content, m.start())[0]}: unmatched '(' after _ inside wxString::Format"
            )
            out_parts.append(content[m.start():m.end()])
            i = m.end()
            continue
        inner = content[gettext_open + 1:gettext_close]
        lit_val, lit_end = parse_string_literal(inner, 0)
        if lit_val is None or inner[lit_end:].strip():
            manual_gettext.append(
                f"{file_rel}:{line_col(content, m.start())[0]}: wxString::Format(_(...), ...) -- non-literal gettext arg"
            )
            out_parts.append(content[m.start():m.end()])
            i = m.end()
            continue
        # Find the outer wxString::Format(... )
        fmt_call_open = content.find("(", m.start())
        fmt_call_close = find_matching_paren(content, fmt_call_open)
        if fmt_call_close == -1:
            manual_gettext.append(
                f"{file_rel}:{line_col(content, m.start())[0]}: wxString::Format(...) unmatched"
            )
            out_parts.append(content[m.start():m.end()])
            i = m.end()
            continue
        # Args inside wxString::Format(...): first is _("lit"); the rest are .arg() values
        outer_args = split_top_level_commas(content[fmt_call_open + 1:fmt_call_close])
        # outer_args[0] is the `_("lit")` blob; the rest are args
        rest_args = outer_args[1:]
        normalised, ph_count = positionalize(lit_val)
        if ph_count != len(rest_args):
            manual_gettext.append(
                f"{file_rel}:{line_col(content, m.start())[0]}: wxString::Format(_(\"{lit_val[:40]}...\"), ...) -- placeholder count {ph_count} != arg count {len(rest_args)}"
            )
        chain = "".join(f".arg(au3::wxToMuse({a.strip()}))" for a in rest_args)
        replacement = (
            f'au3::museToWx(muse::trc("{context}", "{normalised}"){chain})'
        )
        out_parts.append(replacement)
        i = fmt_call_close + 1
    out_parts.append(content[i:])
    content = "".join(out_parts)

    # Now handle plain _("lit") sites.
    # When a `.Format(args)` follows immediately, emit the muse-only form
    # to avoid the wxString::Format-on-instance hazard.
    out_parts = []
    i = 0
    gettext_re = re.compile(r"(?<![A-Za-z0-9_])_\s*\(")
    while True:
        m = gettext_re.search(content, i)
        if m is None:
            break
        if _line_starts_with_define(content, m.start()) or is_inside_line_comment(content, m.start()):
            out_parts.append(content[i:m.end()])
            i = m.end()
            continue
        out_parts.append(content[i:m.start()])
        open_p = m.end() - 1
        close_p = find_matching_paren(content, open_p)
        if close_p == -1:
            manual_gettext.append(
                f"{file_rel}:{line_col(content, m.start())[0]}: unmatched '(' after _"
            )
            out_parts.append(content[m.start():m.end()])
            i = m.end()
            continue
        inner = content[open_p + 1:close_p]
        lit_val, lit_end = parse_string_literal(inner, 0)
        if lit_val is None or inner[lit_end:].strip():
            manual_gettext.append(
                f"{file_rel}:{line_col(content, m.start())[0]}: _({inner[:40]}...) -- non-literal arg"
            )
            out_parts.append(content[m.start():close_p + 1])
            i = close_p + 1
            continue
        normalised, _n = positionalize(lit_val)

        # Look ahead for chained .Format(args). If present, emit a muse TS
        # -> arg chain -> museToWx form so the result is a wxString without
        # ever running through wxString::Format's static-on-instance trap.
        fmt_match = _FORMAT_RE.match(content, close_p + 1)
        if fmt_match:
            fmt_open = fmt_match.end() - 1
            fmt_close = find_matching_paren(content, fmt_open)
            if fmt_close != -1:
                fmt_args = split_top_level_commas(content[fmt_open + 1:fmt_close])
                chain = "".join(f".arg(au3::wxToMuse({a.strip()}))" for a in fmt_args)
                replacement = (
                    f'au3::museToWx(muse::TranslatableString("{context}", "{normalised}"){chain}.translated())'
                )
                out_parts.append(replacement)
                i = fmt_close + 1
                continue

        replacement = f'wxString::FromUTF8(muse::trc("{context}", "{normalised}").c_str())'
        out_parts.append(replacement)
        i = close_p + 1
    out_parts.append(content[i:])
    content = "".join(out_parts)

    # Handle wxGetTranslation("lit")
    out_parts = []
    i = 0
    wxt_re = re.compile(r"\bwxGetTranslation\s*\(")
    while True:
        m = wxt_re.search(content, i)
        if m is None:
            break
        if _line_starts_with_define(content, m.start()) or is_inside_line_comment(content, m.start()):
            out_parts.append(content[i:m.end()])
            i = m.end()
            continue
        out_parts.append(content[i:m.start()])
        open_p = m.end() - 1
        close_p = find_matching_paren(content, open_p)
        if close_p == -1:
            manual_gettext.append(
                f"{file_rel}:{line_col(content, m.start())[0]}: unmatched '(' after wxGetTranslation"
            )
            out_parts.append(content[m.start():m.end()])
            i = m.end()
            continue
        inner = content[open_p + 1:close_p]
        lit_val, lit_end = parse_string_literal(inner, 0)
        if lit_val is None or inner[lit_end:].strip():
            # Internat.cpp's GetCustomTranslation is a runtime wx helper that
            # legitimately takes a non-literal wxString -- leave alone, don't
            # flag. This is the only known site.
            if file_rel == "au3/libraries/au3-strings/Internat.cpp":
                out_parts.append(content[m.start():close_p + 1])
                i = close_p + 1
                continue
            manual_gettext.append(
                f"{file_rel}:{line_col(content, m.start())[0]}: wxGetTranslation({inner[:40]}...) -- non-literal"
            )
            out_parts.append(content[m.start():close_p + 1])
            i = close_p + 1
            continue
        normalised, _n = positionalize(lit_val)
        replacement = f'wxString::FromUTF8(muse::trc("{context}", "{normalised}").c_str())'
        out_parts.append(replacement)
        i = close_p + 1
    out_parts.append(content[i:])
    content = "".join(out_parts)

    return content


# ----- i18n-hint conversion --------------------------------------------- #

_LINE_HINT_RE = re.compile(r"//\s*i18n-hint:?\s*", re.MULTILINE)
_BLOCK_HINT_RE = re.compile(r"/\*\s*i18n-hint:?\s*", re.MULTILINE)


def rewrite_i18n_hints(content: str) -> str:
    # `// i18n-hint: foo` -> `//: foo`
    content = _LINE_HINT_RE.sub("//: ", content)
    # `/* i18n-hint: foo */` -> `/*: foo */`
    content = _BLOCK_HINT_RE.sub("/*: ", content)
    return content


# ----- driver ----------------------------------------------------------- #

def load_contexts() -> dict[str, str]:
    ctx_map: dict[str, str] = {}
    with CTX_CSV.open() as f:
        r = csv.reader(f)
        next(r)  # header
        for row in r:
            if len(row) >= 2:
                ctx_map[row[0]] = row[1]
    return ctx_map


def rewrite_pass_e_cleanup(content: str) -> str:
    """Strip .ToStdString() after .Translation() and audacity::ToUTF8(...)
    wraps around it -- muse's Translation() already returns std::string."""
    # .Translation().ToStdString() -> .Translation()
    new = re.sub(r"\.Translation\(\)\.ToStdString\(\)", ".Translation()", content)
    # audacity::ToUTF8( <expr>.Translation() ) -> <expr>.Translation()
    # Match balanced parens.
    out_parts: list[str] = []
    i = 0
    n = len(new)
    pat = re.compile(r"audacity::ToUTF8\s*\(")
    while True:
        m = pat.search(new, i)
        if m is None:
            break
        open_p = m.end() - 1
        close_p = find_matching_paren(new, open_p)
        if close_p == -1:
            out_parts.append(new[i:m.end()])
            i = m.end()
            continue
        inner = new[open_p + 1:close_p].strip()
        # Detect inner ending with .Translation()
        if inner.endswith(".Translation()"):
            out_parts.append(new[i:m.start()])
            out_parts.append(inner)
            i = close_p + 1
        else:
            out_parts.append(new[i:close_p + 1])
            i = close_p + 1
    out_parts.append(new[i:])
    return "".join(out_parts)


def rewrite_one(file_rel: str, context: str,
                manual_macro: list[str], manual_format: list[str],
                manual_ctor: list[str], manual_gettext: list[str]) -> tuple[str, str]:
    """Return (original, rewritten)."""
    p = REPO / file_rel
    if not p.exists():
        return "", ""
    original = p.read_text()
    new = original
    new = rewrite_i18n_hints(new)
    new = rewrite_pass_ab(new, file_rel, context, manual_macro, manual_format)
    new = rewrite_pass_c(new, file_rel, manual_ctor)
    new = rewrite_pass_d(new, file_rel, context, manual_gettext)
    new = rewrite_pass_e_cleanup(new)
    # Pass B standalone runs LAST so it sees the post-rewrite state and
    # doesn't flag patterns that earlier passes have already cleaned up.
    new = rewrite_pass_b_standalone(new, file_rel, manual_format)
    return original, new


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--apply", action="store_true",
                    help="write files in place (default: dry-run / --diff)")
    ap.add_argument("--diff", action="store_true",
                    help="explicit dry-run; default behaviour")
    ap.add_argument("--only", default=None,
                    help="limit to files matching this substring (debugging)")
    args = ap.parse_args()

    if args.apply and args.diff:
        raise SystemExit("--apply and --diff are mutually exclusive")

    ctx_map = load_contexts()
    paths = SCAN_PATHS.read_text().splitlines()

    # Also include src/ files that use the legacy XO/XC/XXO/Verbatim macros.
    # These are AU4-specific consumers (not au3 library code) but they share
    # the same macro vocabulary. lupdate already scans src/, so adding them
    # to the rewriter scope is a natural extension.
    import subprocess
    src_files = subprocess.run(
        ["grep", "-rlE", "--include=*.cpp", "--include=*.h", "--include=*.mm",
         r"\b(XO|XXO|XC|Verbatim)\(", "src/"],
        cwd=REPO, capture_output=True, text=True
    ).stdout.splitlines()
    for sp in src_files:
        if not sp.startswith("src/"):
            continue
        paths.append(sp)
        # Derive a context: src/effects/builtin_collection/<name>/... -> effects/<name>
        # src/importexport/<sub>/... -> import/export
        # otherwise -> audacity
        parts = sp.split("/")
        if len(parts) >= 4 and parts[1] == "effects" and parts[2] == "builtin_collection":
            ctx_map[sp] = f"effects/{parts[3]}"
        elif len(parts) >= 3 and parts[1] == "effects":
            ctx_map[sp] = f"effects/{parts[2]}"
        elif len(parts) >= 2 and parts[1] == "importexport":
            ctx_map[sp] = "import/export"
        elif len(parts) >= 2 and parts[1] == "audio":
            ctx_map[sp] = "audio/devices"
        else:
            ctx_map[sp] = "audacity"

    manual_macro: list[str] = []
    manual_format: list[str] = []
    manual_ctor: list[str] = []
    manual_gettext: list[str] = []

    touched = 0
    for rel in paths:
        if args.only and args.only not in rel:
            continue
        # Skip files whose content the rewriter shouldn't touch:
        # - Internat.h: legacy XO/XC/XP/XPC/Verbatim macro definitions
        # - TranslatableString.h/.cpp: legacy class being replaced wholesale
        # NyquistBase.cpp IS rewritten -- the wxLocale-lookup replacement is
        # a separate change targeting different sites (gettext/gettextc/etc),
        # not the XO/Verbatim/.Format calls the general rewriter handles.
        _SKIP_FILES = {
            "au3/libraries/au3-strings/Internat.h",
            "au3/libraries/au3-strings/TranslatableString.h",
            "au3/libraries/au3-strings/TranslatableString.cpp",
        }
        if rel in _SKIP_FILES:
            continue
        ctx = ctx_map.get(rel, "audacity")
        orig, new = rewrite_one(rel, ctx, manual_macro, manual_format,
                                manual_ctor, manual_gettext)
        if not orig:
            continue
        if orig == new:
            continue
        touched += 1
        if args.apply:
            (REPO / rel).write_text(new)
        else:
            diff = difflib.unified_diff(
                orig.splitlines(keepends=True),
                new.splitlines(keepends=True),
                fromfile=rel, tofile=rel + ".new",
                n=1,
            )
            sys.stdout.writelines(diff)

    def dump(p: Path, lines: list[str]):
        if args.apply:
            if lines:
                p.write_text("".join(line + "\n" for line in lines))
            elif p.exists():
                p.unlink()

    dump(MANUAL_MACRO, manual_macro)
    dump(MANUAL_FORMAT, manual_format)
    dump(MANUAL_CTOR, manual_ctor)
    dump(MANUAL_GETTEXT, manual_gettext)

    if args.apply:
        install_type_shim()

    print(f"[migrate] touched {touched} files; "
          f"macro_review={len(manual_macro)} format_review={len(manual_format)} "
          f"ctor_review={len(manual_ctor)} gettext_review={len(manual_gettext)}",
          file=sys.stderr)


def install_type_shim():
    """Replace au3's TranslatableString.h with the muse type-shim alias."""
    shim_h = REPO / "au3" / "libraries" / "au3-strings" / "TranslatableString.h"
    shim_cpp = REPO / "au3" / "libraries" / "au3-strings" / "TranslatableString.cpp"

    shim_h.write_text("""/**********************************************************************

 Audacity: A Digital Audio Editor

 @file TranslatableString.h

 Type shim: au3's legacy TranslatableString class is gone; everything
 now resolves to muse::TranslatableString. The bridge header provides
 wxString interop.

 **********************************************************************/

#ifndef __AUDACITY_TRANSLATABLE_STRING__
#define __AUDACITY_TRANSLATABLE_STRING__

#include "types/translatablestring.h"
#include "translation.h"

#include "WxMuseStringCompat.h"

using TranslatableString = muse::TranslatableString;
using TranslatableStrings = muse::TranslatableStrings;

//! Sort comparator (au3 convention) -- compare by translated form.
inline bool TranslationLess(const TranslatableString& a, const TranslatableString& b)
{
    return a.translated() < b.translated();
}

//! Stream operator (au3 convention) -- emits the wxString translation,
//! so `out << someTS` against wxTextOutputStream / wxString sinks keeps working.
template<typename Sink>
inline Sink& operator<<(Sink& sink, const TranslatableString& s)
{
    return sink << wxString::FromUTF8(s.Translation().c_str());
}

#endif
""")

    shim_cpp.write_text("""/**********************************************************************

 Audacity: A Digital Audio Editor

 @file TranslatableString.cpp

 Stub. The legacy au3 TranslatableString class was retired in favour
 of muse::TranslatableString (see TranslatableString.h). This file
 stays in the source list to keep CMakeLists.txt unchanged; it has
 no compile-time content.

 **********************************************************************/
""")


if __name__ == "__main__":
    main()
