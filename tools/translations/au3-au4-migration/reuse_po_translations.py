#!/usr/bin/env python3
"""Fuzzy-fill remaining unfinished .ts entries from au3/locale/<lang>.po
under configurable normalisation. Run after import_po_to_ts.py and
salvage_case_mismatch.py. Fills stay `type="unfinished"` for review.

Normalisation: --min-len N, --case-insensitive, --strip-punct,
--strip-whitespace. Fix-ups before insertion: strip `&` mnemonics
(mnemonic-aware: 'Bass & Treble' stays), reconcile trailing punctuation
to source, replicate case style across same-language pairs."""

from __future__ import annotations

import argparse
import re
import string
import sys
from pathlib import Path
from xml.etree import ElementTree as ET

# Share the importer's parser + placeholder normaliser.
sys.path.insert(0, str(Path(__file__).resolve().parent))
from import_po_to_ts import (  # noqa: E402
    parse_po,
    normalize_translation_placeholders,
    write_ts,
)

REPO = Path(__file__).resolve().parents[3]
PO_DIR = REPO / "au3" / "locale"
TS_DIR = REPO / "share" / "locale"

VANISHED_CTX_NAME = "_au3_legacy_vanished"

_PUNCT_DROP = str.maketrans("", "", string.punctuation + "...")

# wx menu mnemonic: '&' before a non-space non-'&' char, not part of '&&'.
# Distinguishes 'R&escan' (mnemonic) from 'Bass & Treble' (word "and").
_MNEMONIC_AMP = re.compile(r"(?<!\s)(?<!&)&(?=[^\s&])(?!&)")


def has_mnemonic(s: str) -> bool:
    return _MNEMONIC_AMP.search(s) is not None


def strip_mnemonic(s: str) -> str:
    return _MNEMONIC_AMP.sub("", s)


# Menu/label terminators; both `...` (ellipsis) and 3-dot `...` are recognised.
_TRAIL_PUNCT_CHARS = ".,:;!?..."


def trailing_punct(s: str) -> str:
    """Last punctuation token (`...` as one unit), ignoring trailing whitespace."""
    s = s.rstrip()
    if s.endswith("..."):
        return "..."
    if s and s[-1] in _TRAIL_PUNCT_CHARS:
        return s[-1]
    return ""


def _strip_trailing_punct_runs(s: str) -> str:
    s = s.rstrip()
    while True:
        if s.endswith("..."):
            s = s[:-3].rstrip()
            continue
        if s and s[-1] in _TRAIL_PUNCT_CHARS:
            s = s[:-1].rstrip()
            continue
        break
    return s


def match_trailing_punct(src: str, translation: str) -> str:
    """Reconcile translation's trailing punct with src's. If src has form
    but translation has none, no change (respect translator's choice)."""
    src_p = trailing_punct(src)
    trans_p = trailing_punct(translation)
    if src_p == trans_p or not trans_p:
        return translation
    body = _strip_trailing_punct_runs(translation)
    if src_p:
        body += src_p
    return body


def _first_alpha(s: str) -> str:
    return next((c for c in s if c.isalpha()), "")


def _word_runs(s: str):
    for w in s.split():
        if any(c.isalpha() for c in w):
            yield w


def _alphas_after_first(w: str):
    seen = False
    for c in w:
        if c.isalpha():
            if not seen:
                seen = True
            else:
                yield c


def detect_case_style(s: str) -> str:
    """One of: all_lower, all_upper (>1 alpha), title (multi-word),
    sentence (first cap, rest lower), mixed (everything else incl.
    single-word title/sentence ambiguity)."""
    letters = [c for c in s if c.isalpha()]
    if not letters:
        return "mixed"
    if all(c.islower() for c in letters):
        return "all_lower"
    if all(c.isupper() for c in letters) and len(letters) > 1:
        return "all_upper"
    words = list(_word_runs(s))
    if not words:
        return "mixed"
    firsts = [_first_alpha(w) for w in words]
    rests_lower = [all(c.islower() for c in _alphas_after_first(w)) for w in words]
    if all(c.isupper() for c in firsts) and all(rests_lower):
        return "title" if len(words) > 1 else "sentence"
    if (firsts[0].isupper()
            and all(c.islower() for c in firsts[1:])
            and all(rests_lower)):
        return "sentence"
    return "mixed"


def _apply_title(s: str) -> str:
    out = []
    saw_alpha_this_word = False
    for c in s:
        if c.isspace():
            saw_alpha_this_word = False
            out.append(c)
        elif c.isalpha():
            if not saw_alpha_this_word:
                out.append(c.upper())
                saw_alpha_this_word = True
            else:
                out.append(c.lower())
        else:
            out.append(c)
    return "".join(out)


def _apply_sentence(s: str) -> str:
    out = []
    seen = False
    for c in s:
        if c.isalpha():
            if not seen:
                out.append(c.upper())
                seen = True
            else:
                out.append(c.lower())
        else:
            out.append(c)
    return "".join(out)


def apply_case_style(s: str, style: str) -> str:
    if style == "all_lower":
        return s.lower()
    if style == "all_upper":
        return s.upper()
    if style == "title":
        return _apply_title(s)
    if style == "sentence":
        return _apply_sentence(s)
    return s


def replicate_case(src: str, translation: str) -> str:
    """Replicate src's case style onto translation when same-language
    (first alpha matches case-folded). all_upper/all_lower fire freely;
    title/sentence require char-identical strings (avoids miscasing
    loaned-word translations like 'Bass und Hoehen' across languages)."""
    if not src or not translation:
        return translation
    style = detect_case_style(src)
    if style == "mixed":
        return translation
    src_first = _first_alpha(src)
    trans_first = _first_alpha(translation)
    if not src_first or not trans_first:
        return translation
    if src_first.casefold() != trans_first.casefold():
        return translation
    if style in ("all_lower", "all_upper"):
        return apply_case_style(translation, style)
    if src.casefold() == translation.casefold():
        return apply_case_style(translation, style)
    return translation


def _strip_outer_whitespace(src: str, translation: str) -> str:
    """Match translation's outer whitespace to src's; preserve internal."""
    src_leading = len(src) - len(src.lstrip())
    src_trailing = len(src) - len(src.rstrip())
    body = translation.strip()
    return src[:src_leading] + body + (src[-src_trailing:] if src_trailing else "")


def adjust_translation(src: str, po_src: str, translation: str) -> str:
    """Fix-up pipeline: drop mnemonic if po_src has one but src doesn't;
    reconcile trailing punct; trim outer whitespace; replicate case."""
    if has_mnemonic(po_src) and not has_mnemonic(src):
        translation = strip_mnemonic(translation)
    translation = match_trailing_punct(src, translation)
    translation = _strip_outer_whitespace(src, translation)
    translation = replicate_case(src, translation)
    return translation


def make_normaliser(*, case_insensitive: bool, strip_punct: bool,
                    strip_whitespace: bool):
    """Normaliser applied in order: punct -> whitespace -> case."""
    def normalise(s: str) -> str:
        if strip_punct:
            s = s.translate(_PUNCT_DROP)
        if strip_whitespace:
            s = " ".join(s.split())
        if case_insensitive:
            s = s.casefold()
        return s
    return normalise


def reuse_one(ts_path: Path, collisions_path: Path,
              po_entries, normalise, min_len: int,
              dry_run: bool, verbose: bool = False,
              lang: str = "") -> tuple[int, int]:
    """Returns (filled, examined). Writes per-language collision log when
    multiple PO sources normalise to the same key — first one wins, but
    the runner-ups are recorded so a re-run can diagnose silent picks."""
    # normalised source -> (original po source, po translation). First wins on collision.
    po_index: dict[str, tuple[str, str]] = {}
    # normalised key -> list of (msgid, msgstr) candidates seen, in order.
    po_collisions: dict[str, list[tuple[str, str]]] = {}
    for e in po_entries:
        if e.obsolete or not e.msgstr or len(e.msgid) < min_len:
            continue
        key = normalise(e.msgid)
        if not key:
            continue
        if key in po_index:
            po_collisions.setdefault(key, [po_index[key]]).append((e.msgid, e.msgstr))
        else:
            po_index[key] = (e.msgid, e.msgstr)

    if po_collisions:
        if not dry_run:
            lines = []
            for key, candidates in sorted(po_collisions.items()):
                lines.append(f"key={key!r} ({len(candidates)} candidates, first wins):")
                for i, (src, tr) in enumerate(candidates):
                    marker = "  ->" if i == 0 else "    "
                    lines.append(f"{marker} msgid={src!r}")
                    lines.append(f"      msgstr={tr!r}")
            collisions_path.write_text("\n".join(lines) + "\n")
    elif collisions_path.exists():
        collisions_path.unlink()

    if not po_index:
        return (0, 0)

    tree = ET.parse(ts_path)
    root = tree.getroot()

    filled = 0
    examined = 0
    for ctx in root.findall("context"):
        name_el = ctx.find("name")
        if name_el is not None and name_el.text == VANISHED_CTX_NAME:
            continue
        for msg in ctx.findall("message"):
            src_el = msg.find("source")
            tr_el = msg.find("translation")
            if src_el is None or src_el.text is None or tr_el is None:
                continue
            if tr_el.text or any(nf.text for nf in tr_el.findall("numerusform")):
                continue
            if tr_el.get("type") not in (None, "unfinished"):
                continue
            src = src_el.text
            if len(src) < min_len:
                continue
            examined += 1
            key = normalise(src)
            if not key or key not in po_index:
                continue
            po_src, po_tr = po_index[key]
            adjusted = adjust_translation(src, po_src, po_tr)
            adjusted = normalize_translation_placeholders(adjusted)
            if verbose:
                tag = f"[{lang}] " if lang else ""
                print(f"{tag}{src!r}", file=sys.stderr)
                print(f"{tag}  po_src={po_src!r}", file=sys.stderr)
                print(f"{tag}  po_tr ={po_tr!r}", file=sys.stderr)
                if adjusted != po_tr:
                    print(f"{tag}  fixed-> {adjusted!r}", file=sys.stderr)
            if not dry_run:
                tr_el.text = adjusted
                tr_el.set("type", "unfinished")  # translator confirms fuzzy match
            filled += 1

    if filled and not dry_run:
        write_ts(tree, ts_path)

    return (filled, examined)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--min-len", type=int, default=4,
                    metavar="N", help="skip sources shorter than N chars (default 4)")
    ap.add_argument("--case-insensitive", action="store_true",
                    help="match modulo letter case")
    ap.add_argument("--strip-punct", action="store_true",
                    help="drop ASCII punctuation + ... before comparing")
    ap.add_argument("--strip-whitespace", action="store_true",
                    help="collapse runs of whitespace to a single space")
    ap.add_argument("--lang", default=None,
                    help="comma-separated language codes (default: all)")
    ap.add_argument("--dry-run", action="store_true",
                    help="report matches without writing")
    ap.add_argument("--verbose", action="store_true",
                    help="emit each match with source/PO-source/PO-translation "
                         "and the post-fix-up result")
    args = ap.parse_args()

    lang_filter = set(args.lang.split(",")) if args.lang else None
    normalise = make_normaliser(
        case_insensitive=args.case_insensitive,
        strip_punct=args.strip_punct,
        strip_whitespace=args.strip_whitespace,
    )

    total_filled = 0
    languages_touched = 0
    for po_path in sorted(PO_DIR.glob("*.po")):
        lang = po_path.stem
        if lang_filter and lang not in lang_filter:
            continue
        ts_path = TS_DIR / f"audacity_{lang}.ts"
        if not ts_path.exists():
            continue
        collisions_path = TS_DIR / f"po_reuse_collisions.{lang}.txt"
        filled, examined = reuse_one(
            ts_path, collisions_path, parse_po(po_path), normalise,
            args.min_len, args.dry_run,
            verbose=args.verbose, lang=lang,
        )
        if filled:
            languages_touched += 1
            verb = "would fill" if args.dry_run else "filled"
            print(f"  {lang}: {verb} {filled} of {examined} unfinished entries",
                  file=sys.stderr)
        total_filled += filled

    verb = "Would fill" if args.dry_run else "Filled"
    print(f"[reuse-po] {verb} {total_filled} entries across "
          f"{languages_touched} language(s).", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
