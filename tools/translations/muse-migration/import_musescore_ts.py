#!/usr/bin/env python3
"""Seed muse-framework strings in audacity_*.ts from MuseScore catalogues.

The muse framework sources scanned by run_lupdate.sh contribute entries to
share/locale/audacity_en.ts whose <location> paths start with ../../muse/.
MuseScore's Transifex catalogues (musescore_*.ts) already contain finished
translations for these strings, keyed by the same context names and source
texts. This script copies the en-catalogue muse entries into every
audacity_<lang>.ts and fills their translations from musescore_<lang>.ts.

Matching is by (context, source, disambiguation comment); <location> metadata
is ignored for matching. Idempotent: existing translations are never
overwritten, only empty/unfinished entries are filled, so the script can be
re-run after a muse submodule bump against a newer MuseScore checkout.

Usage:
    python3 import_musescore_ts.py --musescore-locale /path/to/MuseScore/share/locale
"""

from __future__ import annotations

import argparse
import copy
import sys
from pathlib import Path
from xml.etree import ElementTree as ET

REPO = Path(__file__).resolve().parents[3]
TS_DIR = REPO / "share" / "locale"

sys.path.insert(0, str(Path(__file__).resolve().parents[1] / "au3-au4-migration"))
from import_po_to_ts import write_ts  # noqa: E402

MUSE_LOCATION_PREFIX = "../../muse/"


def message_key(context_name: str, message: ET.Element) -> tuple:
    source = message.findtext("source") or ""
    comment = message.findtext("comment")
    return (context_name, source, comment)


def is_muse_message(message: ET.Element) -> bool:
    return any(
        (loc.get("filename") or "").startswith(MUSE_LOCATION_PREFIX)
        for loc in message.findall("location")
    )


def translation_is_finished(message: ET.Element) -> bool:
    tr = message.find("translation")
    if tr is None or tr.get("type") in ("unfinished", "vanished", "obsolete"):
        return False
    if tr.get("numerus") or message.get("numerus") == "yes":
        forms = tr.findall("numerusform")
        return bool(forms) and all((f.text or "").strip() for f in forms)
    return bool((tr.text or "").strip())


def collect_muse_entries(en_path: Path) -> dict[tuple, ET.Element]:
    entries: dict[tuple, ET.Element] = {}
    root = ET.parse(en_path).getroot()
    for context in root.findall("context"):
        name = context.findtext("name") or ""
        for message in context.findall("message"):
            if message.get("type") in ("vanished", "obsolete"):
                continue
            if is_muse_message(message):
                entries[message_key(name, message)] = message
    return entries


def index_catalogue(path: Path) -> dict[tuple, ET.Element]:
    index: dict[tuple, ET.Element] = {}
    root = ET.parse(path).getroot()
    for context in root.findall("context"):
        name = context.findtext("name") or ""
        for message in context.findall("message"):
            if translation_is_finished(message):
                index[message_key(name, message)] = message.find("translation")
    return index


def musescore_catalogue_for(code: str, ms_dir: Path) -> Path | None:
    exact = ms_dir / f"musescore_{code}.ts"
    if exact.exists():
        return exact
    base = code.split("@")[0].split("_")[0]
    fallback = ms_dir / f"musescore_{base}.ts"
    if fallback.exists():
        return fallback
    return None


def make_seeded_message(en_message: ET.Element, ms_translation: ET.Element | None) -> ET.Element:
    message = copy.deepcopy(en_message)
    tr = message.find("translation")
    if tr is None:
        tr = ET.SubElement(message, "translation")
    if ms_translation is not None:
        tr.clear()
        tr.text = ms_translation.text
        for form in ms_translation.findall("numerusform"):
            tr.append(copy.deepcopy(form))
    else:
        tr.clear()
        tr.set("type", "unfinished")
        if en_message.get("numerus") == "yes":
            ET.SubElement(tr, "numerusform")
    return message


def fill_existing_message(message: ET.Element, ms_translation: ET.Element) -> None:
    tr = message.find("translation")
    if tr is None:
        tr = ET.SubElement(message, "translation")
    tr.clear()
    tr.attrib.pop("type", None)
    tr.text = ms_translation.text
    for form in ms_translation.findall("numerusform"):
        tr.append(copy.deepcopy(form))


def import_language(ts_path: Path, muse_entries: dict[tuple, ET.Element],
                    ms_index: dict[tuple, ET.Element]) -> dict:
    tree = ET.parse(ts_path)
    root = tree.getroot()

    contexts = {c.findtext("name") or "": c for c in root.findall("context")}
    existing: dict[tuple, ET.Element] = {}
    for context in root.findall("context"):
        name = context.findtext("name") or ""
        for message in context.findall("message"):
            existing[message_key(name, message)] = message

    stats = {"filled": 0, "already": 0, "unfinished": 0, "added": 0}

    for key, en_message in muse_entries.items():
        context_name = key[0]
        ms_translation = ms_index.get(key)

        message = existing.get(key)
        if message is not None:
            if translation_is_finished(message):
                stats["already"] += 1
            elif ms_translation is not None:
                fill_existing_message(message, ms_translation)
                stats["filled"] += 1
            else:
                stats["unfinished"] += 1
            continue

        context = contexts.get(context_name)
        if context is None:
            context = ET.SubElement(root, "context")
            name_el = ET.SubElement(context, "name")
            name_el.text = context_name
            contexts[context_name] = context

        context.append(make_seeded_message(en_message, ms_translation))
        stats["added"] += 1
        if ms_translation is not None:
            stats["filled"] += 1
        else:
            stats["unfinished"] += 1

    write_ts(tree, ts_path)
    return stats


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--musescore-locale", type=Path, required=True,
                        help="share/locale directory of a MuseScore checkout")
    parser.add_argument("--languages", nargs="*",
                        help="restrict to these language codes (default: all)")
    args = parser.parse_args()

    muse_entries = collect_muse_entries(TS_DIR / "audacity_en.ts")
    print(f"muse-framework entries in audacity_en.ts: {len(muse_entries)}")

    total = {"filled": 0, "unfinished": 0}
    unmatched_languages = []

    for ts_path in sorted(TS_DIR.glob("audacity_*.ts")):
        code = ts_path.stem[len("audacity_"):]
        if code == "en":
            continue
        if args.languages and code not in args.languages:
            continue

        ms_path = musescore_catalogue_for(code, args.musescore_locale)
        if ms_path is None:
            unmatched_languages.append(code)
            ms_index = {}
        else:
            ms_index = index_catalogue(ms_path)

        stats = import_language(ts_path, muse_entries, ms_index)
        total["filled"] += stats["filled"]
        total["unfinished"] += stats["unfinished"]
        coverage = 100 * stats["filled"] / len(muse_entries) if muse_entries else 0
        print(f"  {code:14s} filled {stats['filled']:4d}  already {stats['already']:4d}  "
              f"left unfinished {stats['unfinished']:4d}  ({coverage:.0f}% newly covered)"
              + ("" if ms_path else "  [no musescore catalogue]"))

    if unmatched_languages:
        print(f"languages without a musescore catalogue: {', '.join(unmatched_languages)}")
    print(f"total: filled {total['filled']}, left unfinished {total['unfinished']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
