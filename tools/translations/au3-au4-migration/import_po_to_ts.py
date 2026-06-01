#!/usr/bin/env python3
"""Import au3/locale/*.po into share/locale/audacity_*.ts.

Primary match: (source, msgctxt). Secondary: (positionalize(source), msgctxt)
to catch sources whose %s/%d the rewriter normalised to %1/%2.
Entries already non-`unfinished` are skipped (preserves QML translations).
Unmatched PO entries land in a synthetic `_au3_legacy_vanished` context for
translator salvage. Idempotent: the vanished block is rebuilt each run."""

from __future__ import annotations

import argparse
import io
import re
import sys
from pathlib import Path
from xml.etree import ElementTree as ET
from xml.sax.saxutils import escape as xml_escape

REPO = Path(__file__).resolve().parents[3]
PO_DIR = REPO / "au3" / "locale"
TS_DIR = REPO / "share" / "locale"


def write_ts(tree: ET.ElementTree, ts_path: Path) -> None:
    """Emit a .ts file in Qt-linguist-native format: one-line header,
    <context> at column 0, 4-space indent, `"/>` self-close (no space).
    Keeps re-imports diff-minimal against lupdate-touched catalogues."""
    ET.indent(tree, space="    ")
    buf = io.BytesIO()
    tree.write(buf, encoding="utf-8", xml_declaration=False)
    xml = buf.getvalue().decode("utf-8")
    # Dedent element-tag lines by 4 (so <context> sits at col 0); leave text content alone.
    indented_tag = re.compile(r"^    +<")
    out = []
    for line in xml.splitlines():
        out.append(line[4:] if indented_tag.match(line) else line)
    body = "\n".join(out).replace(" />", "/>")
    if body.startswith("<TS"):
        final = '<?xml version="1.0" ?><!DOCTYPE TS>' + body
    else:
        final = '<?xml version="1.0" ?>\n<!DOCTYPE TS>\n' + body
    if not final.endswith("\n"):
        final += "\n"
    ts_path.write_text(final, encoding="utf-8")


# Matches gettext placeholders (%s, %d, %.2f, %lld, ...). Space is
# excluded from the flags so "% des" / "1000 % sein" do not match.
_GETTEXT_PH_RE = re.compile(
    r"%(?P<percent>%)|"
    r"%(?P<flags>[-+#0]*)(?P<width>\d*)(?P<dot>\.?)(?P<prec>\d*)"
    r"(?P<length>ll|l|h|hh|j|z|t|L)?"
    r"(?P<spec>[sdfgieuxoEXc])"
)

# POSIX positional: %2$s, %1$lld -- translators use this to reorder args.
_POSIX_POS_PH_RE = re.compile(
    r"%(?P<percent>%)|"
    r"%(?P<pos>\d+)\$"
    r"(?P<flags>[-+#0]*)(?P<width>\d*)(?P<dot>\.?)(?P<prec>\d*)"
    r"(?P<length>ll|l|h|hh|j|z|t|L)?"
    r"(?P<spec>[sdfgieuxoEXc])"
)


def positionalize(src: str) -> str:
    """%s %d -> %1 %2 (sequential, in order). For indexing PO sources
    against rewriter-emitted Qt-style sources."""
    counter = [0]

    def repl(m: re.Match) -> str:
        if m.group("percent"):
            return "%%"
        counter[0] += 1
        return f"%{counter[0]}"

    return _GETTEXT_PH_RE.sub(repl, src)


def normalize_translation_placeholders(translation: str) -> str:
    """Rewrite PO placeholders to Qt positional form for QString::arg().
    Pass 1: POSIX %N$spec -> %N (preserves translator reordering).
    Pass 2: sequential %s/%d -> %1/%2 in order, but only if the text
    has no unambiguous Qt positional markers already. `%4h` is ambiguous
    (could be `%h` width 4 or positional `%4`+literal h), but `%1 ` is
    not — its presence proves the whole translation is Qt-positional."""
    translation = _POSIX_POS_PH_RE.sub(
        lambda m: "%%" if m.group("percent") else f"%{m.group('pos')}",
        translation,
    )

    # Idempotency guard: a `%N` followed by anything outside the gettext
    # spec set and not another digit is unambiguously Qt-positional. If
    # any such marker exists, skip the gettext pass — `%4g`/`%6s` in the
    # same text are positional+literal, not gettext.
    if re.search(r"%\d+(?![\dsdfgieuxoEXc])", translation):
        return translation

    counter = [0]

    def repl(m: re.Match) -> str:
        if m.group("percent"):
            return "%%"
        counter[0] += 1
        return f"%{counter[0]}"

    return _GETTEXT_PH_RE.sub(repl, translation)


def _unescape_po_string(s: str) -> str:
    """Decode a PO string literal body (escapes only -- no surrounding quotes)."""
    out = []
    i = 0
    while i < len(s):
        c = s[i]
        if c == "\\" and i + 1 < len(s):
            nxt = s[i + 1]
            if nxt == "n":
                out.append("\n")
            elif nxt == "t":
                out.append("\t")
            elif nxt == "r":
                out.append("\r")
            elif nxt == "\\":
                out.append("\\")
            elif nxt == '"':
                out.append('"')
            else:
                out.append(nxt)
            i += 2
            continue
        out.append(c)
        i += 1
    return "".join(out)


class PoEntry:
    def __init__(self) -> None:
        self.msgid = ""
        self.msgid_plural: str | None = None
        self.msgctxt: str | None = None
        self.msgstr = ""
        self.msgstr_plural: list[str] = []
        self.fuzzy = False
        self.obsolete = False
        self.translator_comment = ""  # `#`
        self.extracted_comment = ""   # `#.`


def parse_po(path: Path) -> list[PoEntry]:
    entries: list[PoEntry] = []
    cur = PoEntry()

    lines = path.read_text(encoding="utf-8", errors="replace").splitlines()

    def commit_entry():
        nonlocal cur
        # Drop the header entry (msgid="" with no plural/ctx).
        if cur.msgid == "" and cur.msgid_plural is None and not cur.msgctxt:
            cur = PoEntry()
            return
        entries.append(cur)
        cur = PoEntry()

    slot = "msgid"
    plural_index = -1

    i = 0
    while i < len(lines):
        line = lines[i]

        if line.startswith("# ") or line == "#":
            cur.translator_comment += (line[2:] if len(line) > 1 else "") + "\n"
            i += 1
            continue
        if line.startswith("#. "):
            cur.extracted_comment += line[3:] + "\n"
            i += 1
            continue
        if line.startswith("#:"):  # source location, discard
            i += 1
            continue
        if line.startswith("#,"):
            if "fuzzy" in line:
                cur.fuzzy = True
            i += 1
            continue
        if line.startswith("#~"):
            cur.obsolete = True
            line = line[2:].lstrip()
            # Fall through to parse the rest as a PO line.

        line_stripped = line.strip()
        if not line_stripped:
            # Trigger commit on any populated slot including msgstr -- the
            # header has msgid="" (falsy) but populated msgstr, and missing
            # it here leaks its translator_comment into the next entry.
            if (cur.msgid or cur.msgctxt is not None or cur.msgid_plural is not None
                    or cur.msgstr or cur.msgstr_plural):
                if not cur.obsolete:
                    commit_entry()
                else:
                    cur = PoEntry()  # drop obsolete
            i += 1
            continue

        if line_stripped.startswith("msgctxt "):
            slot = "msgctxt"
            cur.msgctxt = _read_po_value(line_stripped[len("msgctxt "):])
            i += 1
            continue
        if line_stripped.startswith("msgid_plural "):
            slot = "msgid_plural"
            cur.msgid_plural = _read_po_value(line_stripped[len("msgid_plural "):])
            i += 1
            continue
        if line_stripped.startswith("msgid "):
            slot = "msgid"
            cur.msgid = _read_po_value(line_stripped[len("msgid "):])
            i += 1
            continue
        if line_stripped.startswith("msgstr "):
            slot = "msgstr"
            cur.msgstr = _read_po_value(line_stripped[len("msgstr "):])
            i += 1
            continue
        m = re.match(r"msgstr\[(\d+)\]\s+(.*)$", line_stripped)
        if m:
            idx = int(m.group(1))
            slot = "msgstr_plural"
            plural_index = idx
            while len(cur.msgstr_plural) <= idx:
                cur.msgstr_plural.append("")
            cur.msgstr_plural[idx] = _read_po_value(m.group(2))
            i += 1
            continue
        if line_stripped.startswith('"'):  # continuation of previous slot
            piece = _read_po_value(line_stripped)
            if slot == "msgid":
                cur.msgid += piece
            elif slot == "msgctxt":
                cur.msgctxt = (cur.msgctxt or "") + piece
            elif slot == "msgid_plural":
                cur.msgid_plural = (cur.msgid_plural or "") + piece
            elif slot == "msgstr":
                cur.msgstr += piece
            elif slot == "msgstr_plural":
                cur.msgstr_plural[plural_index] += piece
            i += 1
            continue
        i += 1

    # commit final
    if cur.msgid or cur.msgctxt is not None or cur.msgid_plural is not None:
        if not cur.obsolete:
            commit_entry()

    return entries


def _read_po_value(token: str) -> str:
    t = token.strip()
    if t.startswith('"') and t.endswith('"'):
        return _unescape_po_string(t[1:-1])
    return t


def import_into_ts(po_path: Path, ts_path: Path, collisions_path: Path,
                   ctx_fallback: bool) -> None:
    po_entries = parse_po(po_path)

    primary: dict[tuple[str, str | None], PoEntry] = {}
    secondary: dict[tuple[str, str | None], PoEntry] = {}
    secondary_collisions: dict[tuple[str, str | None], list[PoEntry]] = {}

    for e in po_entries:
        primary.setdefault((e.msgid, e.msgctxt), e)
        key2 = (positionalize(e.msgid), e.msgctxt)
        if key2 in secondary:
            secondary_collisions.setdefault(key2, []).append(secondary[key2])
            secondary_collisions[key2].append(e)
        else:
            secondary[key2] = e

    if not ts_path.exists():
        lang = ts_path.stem.removeprefix("audacity_")
        ts_path.write_text(
            f'<?xml version="1.0" encoding="utf-8"?>\n'
            f'<!DOCTYPE TS>\n'
            f'<TS version="2.1" language="{lang}">\n'
            f'</TS>\n',
            encoding="utf-8",
        )

    tree = ET.parse(ts_path)
    root = tree.getroot()

    # Drop the _au3_legacy_vanished block; rebuilt from scratch below
    # (idempotent re-runs, no append duplicates).
    for ctx in list(root.findall("context")):
        name_el = ctx.find("name")
        if name_el is not None and name_el.text == "_au3_legacy_vanished":
            root.remove(ctx)

    used_po_entries: set[int] = set()
    collisions: list[str] = []
    matched_count = 0

    for ctx in list(root.findall("context")):
        ctx_name_el = ctx.find("name")
        ctx_name = ctx_name_el.text if ctx_name_el is not None else None
        for msg in list(ctx.findall("message")):
            src_el = msg.find("source")
            if src_el is None or src_el.text is None:
                continue
            src = src_el.text
            translation_el = msg.find("translation")
            already_translated = (
                translation_el is not None
                and translation_el.get("type") != "unfinished"
                and (translation_el.text or msg.find(".//numerusform") is not None)
            )

            # Look up even for already-translated entries -- we sync
            # translator/extracomment from the PO authoritatively.
            po: PoEntry | None = None
            for ctx_try in (ctx_name, None):
                key = (src, ctx_try)
                po = primary.get(key)
                if po is None:
                    po = secondary.get((positionalize(src), ctx_try))
                if po:
                    if (positionalize(src), ctx_try) in secondary_collisions:
                        collisions.append(f"{src!r} ctx={ctx_try!r}: multiple candidates")
                    break
            if po is None and ctx_fallback:
                # Last-resort: match by source alone, ignoring context.
                for (src_key, _ctx), candidate in secondary.items():
                    if src_key == positionalize(src):
                        po = candidate
                        break

            if po is None:
                continue
            used_po_entries.add(id(po))

            # Comments are PO-authoritative: set if PO has one, remove if not.
            tcm = msg.find("translatorcomment")
            if po.translator_comment.strip():
                if tcm is None:
                    tcm = ET.SubElement(msg, "translatorcomment")
                tcm.text = po.translator_comment.strip()
            elif tcm is not None:
                msg.remove(tcm)
            ecm = msg.find("extracomment")
            if po.extracted_comment.strip():
                if ecm is None:
                    ecm = ET.SubElement(msg, "extracomment")
                ecm.text = po.extracted_comment.strip()
            elif ecm is not None:
                msg.remove(ecm)

            if already_translated:
                matched_count += 1
                continue

            is_plural = msg.get("numerus") == "yes" or po.msgid_plural is not None
            if is_plural:
                msg.set("numerus", "yes")
                if translation_el is not None:
                    msg.remove(translation_el)
                translation_el = ET.SubElement(msg, "translation")
                for form in po.msgstr_plural:
                    nf = ET.SubElement(translation_el, "numerusform")
                    nf.text = normalize_translation_placeholders(form)
            else:
                if translation_el is None:
                    translation_el = ET.SubElement(msg, "translation")
                translation_el.text = normalize_translation_placeholders(po.msgstr)
                for child in list(translation_el):
                    translation_el.remove(child)

            if po.fuzzy:
                translation_el.set("type", "unfinished")
            else:
                translation_el.attrib.pop("type", None)

            matched_count += 1

    # Append unmatched PO entries to the _au3_legacy_vanished block.
    # Dedup on (msgid, msgctxt) -- legacy PO files often emit the same
    # source under multiple disambiguating contexts (e.g. "%1 dB" appears
    # across compressor/limiter/equalizer/normalize). lrelease would drop
    # the dupes anyway and warn loudly; pruning here keeps the .ts file
    # clean for Linguist and silences the warnings at build time.
    vanished_block: ET.Element | None = None
    appended = 0
    seen: set[tuple[str, str]] = set()
    for e in po_entries:
        if id(e) in used_po_entries:
            continue
        if e.obsolete:
            continue
        key = (e.msgid, e.msgctxt or "")
        if key in seen:
            continue
        seen.add(key)
        if vanished_block is None:
            for c in root.findall("context"):
                name = c.find("name")
                if name is not None and name.text == "_au3_legacy_vanished":
                    vanished_block = c
                    break
            if vanished_block is None:
                vanished_block = ET.SubElement(root, "context")
                ET.SubElement(vanished_block, "name").text = "_au3_legacy_vanished"
        msg = ET.SubElement(vanished_block, "message")
        if e.msgid_plural is not None:
            msg.set("numerus", "yes")
        src = ET.SubElement(msg, "source")
        src.text = e.msgid
        if e.msgid_plural is not None:
            src_plural = ET.SubElement(msg, "oldsource")
            src_plural.text = e.msgid_plural
        if e.msgctxt:
            cmt = ET.SubElement(msg, "comment")
            cmt.text = e.msgctxt
        if e.translator_comment.strip():
            tcm = ET.SubElement(msg, "translatorcomment")
            tcm.text = e.translator_comment.strip()
        if e.extracted_comment.strip():
            ecm = ET.SubElement(msg, "extracomment")
            ecm.text = e.extracted_comment.strip()
        tr = ET.SubElement(msg, "translation")
        tr.set("type", "vanished")
        if e.msgid_plural is not None:
            for form in e.msgstr_plural:
                nf = ET.SubElement(tr, "numerusform")
                nf.text = normalize_translation_placeholders(form)
        else:
            tr.text = normalize_translation_placeholders(e.msgstr)
        appended += 1

    write_ts(tree, ts_path)

    if collisions:
        collisions_path.write_text("\n".join(collisions) + "\n")
    elif collisions_path.exists():
        collisions_path.unlink()

    print(f"  {po_path.name}: matched={matched_count}, vanished={appended}", file=sys.stderr)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--lang", default=None, help="restrict to a single language code")
    ap.add_argument("--ctx-fallback", action="store_true",
                    help="match by source alone when (source,ctx) misses")
    args = ap.parse_args()

    linguas_path = PO_DIR / "LINGUAS"
    if linguas_path.exists():
        # LINGUAS may have multiple codes per line.
        langs = [line.strip() for line in linguas_path.read_text().splitlines()
                 if line.strip() and not line.startswith("#")]
        langs = [t for line in langs for t in line.split()]
    else:
        langs = [p.stem for p in PO_DIR.glob("*.po")]

    for lang in langs:
        if args.lang and lang != args.lang:
            continue
        po = PO_DIR / f"{lang}.po"
        ts = TS_DIR / f"audacity_{lang}.ts"
        if not po.exists():
            print(f"[import] {lang}: no PO file at {po}", file=sys.stderr)
            continue
        collisions = TS_DIR / f"po_import_collisions.{lang}.txt"
        import_into_ts(po, ts, collisions, args.ctx_fallback)


if __name__ == "__main__":
    main()
