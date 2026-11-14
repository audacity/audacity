#!/usr/bin/env python3
"""Walk build.ninja for the AU4 link closure; write:
  ../au4_scan_paths.txt  (byte-sorted lupdate scan paths)
  au3_contexts.csv       (per-file context slug)
Requires a configured CMake preset (default: audacity-asan)."""

from __future__ import annotations

import argparse
import csv
import json
import os
import re
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[3]


def load_preset_binary_dir(preset_name: str) -> Path:
    presets = json.loads((REPO / "CMakePresets.json").read_text())
    cps = presets.get("configurePresets", [])
    by_name = {p["name"]: p for p in cps}

    def resolve(name: str) -> dict:
        p = dict(by_name[name])
        inherits = p.get("inherits") or []
        if isinstance(inherits, str):
            inherits = [inherits]
        for parent in reversed(inherits):
            inh = resolve(parent)
            for k, v in inh.items():
                p.setdefault(k, v)
        return p

    p = resolve(preset_name)
    bin_dir = p.get("binaryDir") or "${sourceDir}/build/${presetName}"
    bin_dir = bin_dir.replace("${sourceDir}", str(REPO))
    bin_dir = bin_dir.replace("${presetName}", preset_name)
    return Path(bin_dir)


def parse_ninja(ninja_path: Path) -> dict:
    """Parse build.ninja's `build` statements only; ignore rules/vars.
    Returns {output: {"rule", "inputs", "implicit", "order"}}."""
    text = ninja_path.read_text()
    text = re.sub(r"\$\n\s*", " ", text)  # join $-continued lines

    build_re = re.compile(r"^build\s+(.+?):\s*(\S+)\s*(.*)$")
    builds: dict = {}
    for line in text.splitlines():
        if not line.startswith("build "):
            continue
        m = build_re.match(line)
        if not m:
            continue
        out_part, rule, in_part = m.group(1), m.group(2), m.group(3)
        outputs = out_part.strip().split()
        # Split inputs / implicit / order on " | " / " || "
        toks = in_part.strip().split()
        inputs, implicit, order = [], [], []
        bucket = inputs
        for t in toks:
            if t == "|":
                bucket = implicit
            elif t == "||":
                bucket = order
            else:
                bucket.append(t)
        entry = {"rule": rule, "inputs": inputs, "implicit": implicit, "order": order}
        for out in outputs:
            builds[out] = entry
    return builds


def is_static_lib_rule(rule: str) -> bool:
    return rule.startswith("CXX_STATIC_LIBRARY_LINKER")


def is_cxx_compile_rule(rule: str) -> bool:
    return rule.startswith("CXX_COMPILER")


def is_object(name: str) -> bool:
    return name.endswith(".o")


def is_static_lib(name: str) -> bool:
    return name.endswith(".a")


def expand_unity(cxx_path: Path) -> list[Path]:
    """Return the included .cpp/.cxx/.mm/.c paths from a unity_N_cxx.cxx chunk."""
    out: list[Path] = []
    if not cxx_path.exists():
        return out
    inc_re = re.compile(r'^\s*#include\s+"([^"]+)"')
    for line in cxx_path.read_text().splitlines():
        m = inc_re.match(line)
        if not m:
            continue
        p = Path(m.group(1))
        if p.suffix in {".cpp", ".cxx", ".mm", ".c"}:
            out.append(p)
    return out


def resolve_source_for_obj(obj_target: str, builds: dict, build_root: Path) -> list[Path]:
    """Return source TU paths for a .cpp.o target. Unity chunks expand to their includes."""
    rule = builds.get(obj_target)
    if rule is None or not is_cxx_compile_rule(rule["rule"]) or not rule["inputs"]:
        return []
    src_path = Path(rule["inputs"][0])
    if not src_path.is_absolute():
        src_path = (build_root / src_path).resolve()
    if src_path.name.endswith(".cxx") and "unity_" in src_path.name:
        return expand_unity(src_path)
    return [src_path]


def walk_closure(exe_target: str, builds: dict, build_root: Path) -> set[Path]:
    """Return source TU paths transitively reachable from exe_target."""
    seen_libs: set[str] = set()
    seen_objs: set[str] = set()
    sources: set[Path] = set()

    def visit_inputs(rule_entry):
        for t in list(rule_entry["inputs"]) + list(rule_entry["implicit"]):
            if is_object(t):
                if t in seen_objs:
                    continue
                seen_objs.add(t)
                for src in resolve_source_for_obj(t, builds, build_root):
                    sources.add(src)
            elif is_static_lib(t):
                if t in seen_libs:
                    continue
                seen_libs.add(t)
                lib_rule = builds.get(t)
                if lib_rule is None:
                    continue
                if not is_static_lib_rule(lib_rule["rule"]):
                    continue
                visit_inputs(lib_rule)

    exe_rule = builds.get(exe_target)
    if exe_rule is None:
        raise SystemExit(f"executable target {exe_target!r} not in build.ninja")
    visit_inputs(exe_rule)
    return sources


def filter_au3(sources: set[Path]) -> list[Path]:
    """Keep only sources under au3/libraries/ or au3/modules/ (repo-relative)."""
    out: list[Path] = []
    for s in sources:
        try:
            rel = s.resolve().relative_to(REPO)
        except ValueError:
            continue
        parts = rel.parts
        if len(parts) >= 3 and parts[0] == "au3" and parts[1] in ("libraries", "modules"):
            out.append(rel)
    return out


def expand_headers(cpp_relpaths: list[Path]) -> set[Path]:
    """Add sibling .h/.hpp/.hxx headers next to each .cpp (lupdate needs them)."""
    dirs: set[Path] = {p.parent for p in cpp_relpaths}
    out: set[Path] = set(cpp_relpaths)
    for d in dirs:
        abs_d = REPO / d
        if not abs_d.is_dir():
            continue
        for entry in os.listdir(abs_d):
            if entry.endswith((".h", ".hpp", ".hxx")):
                out.add(d / entry)
    return out


def slug_for(rel_path: Path) -> str:
    """Context slug for an au3 file. libraries strip the au3- prefix;
    the remaining name (e.g. 'wave-track-paint') is used verbatim."""
    parts = rel_path.parts
    if len(parts) < 3 or parts[0] != "au3":
        return "audacity"
    kind, name = parts[1], parts[2]
    if kind == "libraries" and name.startswith("au3-"):
        name = name[len("au3-"):]
    return name


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--preset", default="audacity-asan")
    ap.add_argument("--exe", default="src/app/audacity.app/Contents/MacOS/audacity",
                    help="ninja-relative path to executable target")
    args = ap.parse_args()

    build_root = load_preset_binary_dir(args.preset)
    ninja = build_root / "build.ninja"
    if not ninja.exists():
        raise SystemExit(f"missing {ninja}; configure the preset first")

    builds = parse_ninja(ninja)
    print(f"[scan] parsed {len(builds)} ninja build statements", file=sys.stderr)

    sources = walk_closure(args.exe, builds, build_root)
    print(f"[scan] closure: {len(sources)} source TUs", file=sys.stderr)

    au3 = filter_au3(sources)
    print(f"[scan] au3/libraries+modules sources: {len(au3)}", file=sys.stderr)

    with_headers = expand_headers(au3)
    print(f"[scan] with sibling headers: {len(with_headers)}", file=sys.stderr)

    rel_strs = sorted(str(p) for p in with_headers)  # byte-sort = LC_ALL=C

    out_paths = REPO / "tools" / "translations" / "au4_scan_paths.txt"
    out_paths.write_text("".join(s + "\n" for s in rel_strs))
    print(f"[scan] wrote {out_paths.relative_to(REPO)} ({len(rel_strs)} entries)",
          file=sys.stderr)

    csv_path = REPO / "tools" / "translations" / "au3-au4-migration" / "au3_contexts.csv"
    with csv_path.open("w", newline="") as f:
        w = csv.writer(f, lineterminator="\n")
        w.writerow(["path", "context"])
        for s in rel_strs:
            w.writerow([s, slug_for(Path(s))])
    print(f"[scan] wrote {csv_path.relative_to(REPO)}", file=sys.stderr)


if __name__ == "__main__":
    main()
