#!/usr/bin/env python3
"""Fill unfinished entries from _au3_legacy_vanished when sources match
modulo letter case (typical: Nyquist effect titles). Run after
import_po_to_ts.py."""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
import xml.etree.ElementTree as ET

sys.path.insert(0, str(Path(__file__).resolve().parent))
from import_po_to_ts import write_ts  # noqa: E402

REPO = Path(__file__).resolve().parents[3]
TS_DIR = REPO / "share" / "locale"


def normalise(s: str) -> str:
    return " ".join(s.split()).casefold() if s else ""


def salvage_one(ts_path: Path, restrict_context: str | None) -> tuple[int, int]:
    tree = ET.parse(ts_path)
    root = tree.getroot()

    # normalised_source -> vanished translation text
    vanished_index: dict[str, str] = {}
    for ctx in root.findall("context"):
        name_el = ctx.find("name")
        if name_el is None or name_el.text != "_au3_legacy_vanished":
            continue
        for msg in ctx.findall("message"):
            src_el = msg.find("source")
            tr_el = msg.find("translation")
            if src_el is None or src_el.text is None:
                continue
            if tr_el is None or tr_el.text is None:
                continue
            if tr_el.get("type") != "vanished":
                continue
            vanished_index.setdefault(normalise(src_el.text), tr_el.text)

    if not vanished_index:
        return (0, 0)

    salvaged = 0
    examined = 0
    for ctx in root.findall("context"):
        name_el = ctx.find("name")
        ctx_name = name_el.text if name_el is not None else ""
        if ctx_name == "_au3_legacy_vanished":
            continue
        if restrict_context and ctx_name != restrict_context:
            continue
        for msg in ctx.findall("message"):
            tr_el = msg.find("translation")
            src_el = msg.find("source")
            if tr_el is None or src_el is None or src_el.text is None:
                continue
            if tr_el.get("type") != "unfinished" or tr_el.text:
                continue
            examined += 1
            key = normalise(src_el.text)
            if key in vanished_index:
                # Keep type="unfinished" so a translator confirms the case-imperfect match.
                tr_el.text = vanished_index[key]
                salvaged += 1

    if salvaged:
        write_ts(tree, ts_path)

    return (salvaged, examined)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--lang", default=None)
    ap.add_argument("--context", default=None,
                    help="restrict to a single TS context (e.g. effects/nyquist)")
    args = ap.parse_args()

    total = 0
    for ts in sorted(TS_DIR.glob("audacity_*.ts")):
        code = ts.stem.removeprefix("audacity_")
        if args.lang and code != args.lang:
            continue
        salvaged, examined = salvage_one(ts, args.context)
        if salvaged:
            print(f"  {code}: salvaged {salvaged} of {examined} unfinished entries",
                  file=sys.stderr)
        total += salvaged
    print(f"[salvage] total {total} entries filled across all languages", file=sys.stderr)


if __name__ == "__main__":
    main()
