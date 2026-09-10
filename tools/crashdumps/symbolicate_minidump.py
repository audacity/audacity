#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-only
"""
Resolve a Crashpad/Breakpad minidump (.dmp) against binaries on this machine.

What it does:
  1. Runs `minidump_stackwalk -m` to list the dump's modules (name + debug ID) and
     which of them appear on the crashing thread.
  2. Looks under the search roots (.app bundles, build trees, single files) for Mach-O
     files with the same basename and keeps only those whose debug ID, as reported by
     `dump_syms -i`, equals the ID recorded in the dump.
  3. Runs `dump_syms` on each match and stores the output in the Breakpad layout
         <symbols>/<module>/<DEBUG_ID>/<module>.sym
     which is what minidump_stackwalk and the VS Code "Minidump Parser" extension read.
  4. Runs `minidump_stackwalk` with that symbol directory and prints the result.

It also prints the Crashpad annotations stored in the dump (the app's own tags such as
the plugin under validation, plus system notes like the uncaught-exception message),
which minidump_stackwalk does not show.

A .sym is only ever written from a binary whose identity matches the dump, so symbols
from the wrong build cannot land under the dump's debug ID. An existing .sym whose
MODULE header disagrees with its directory name is moved aside and regenerated.

Examples:
  # Crash of the installed app
  tools/crashdumps/symbolicate_minidump.py ~/Downloads/crash.dmp --search "/Applications/Audacity 4.app"

  # Crash of a local debug build, with file:line info (runs dsymutil; slow on big binaries)
  tools/crashdumps/symbolicate_minidump.py crash.dmp --search build/audacity-debug --dsymutil

  # Just show which modules the dump needs and which local binaries match
  tools/crashdumps/symbolicate_minidump.py crash.dmp --list

Tools: dump_syms and minidump_stackwalk are taken from --dump-syms / --stackwalk, the
DUMP_SYMS / MINIDUMP_STACKWALK environment variables, PATH, or ~/.dumpstorm/bin (where the
VS Code extension installs minidump_stackwalk). Prebuilt dump_syms archives are listed in
muse_deps/prebuilt.lock; the base URL is in muse_deps/prebuilt_url.txt.

Status messages go to stderr, the stack trace to stdout, so `-o` or a pipe gets only the trace.
"""

import argparse
import glob
import os
import shutil
import struct
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path

HOME = Path.home()
DUMPSTORM_BIN = HOME / ".dumpstorm" / "bin"
DEFAULT_SYMBOLS_DIR = HOME / ".dumpstorm" / "symbols"
REPO_ROOT = Path(__file__).resolve().parents[2]

DEFAULT_SEARCH = [
    "/Applications/Audacity*.app",
    str(REPO_ROOT / "build" / "*" / "src" / "app" / "audacity.app"),
]
if os.environ.get("QTDIR"):
    DEFAULT_SEARCH.append(os.path.join(os.environ["QTDIR"], "lib"))

MACHO_MAGICS = {
    b"\xfe\xed\xfa\xce", b"\xce\xfa\xed\xfe",  # 32-bit
    b"\xfe\xed\xfa\xcf", b"\xcf\xfa\xed\xfe",  # 64-bit
    b"\xca\xfe\xba\xbe", b"\xbe\xba\xfe\xca",  # fat
}
# minidump_stackwalk CPU names -> dump_syms -a names
ARCH_NAMES = {"amd64": "x86_64", "x86_64": "x86_64", "arm64": "arm64", "x86": "i386"}


def log(msg=""):
    print(msg, file=sys.stderr, flush=True)


def die(msg, code=2):
    log(f"error: {msg}")
    sys.exit(code)


def run(cmd, **kw):
    return subprocess.run([str(c) for c in cmd], text=True, **kw)


@dataclass
class Module:
    code_file: str
    debug_file: str
    debug_id: str
    main: bool

    @property
    def name(self):
        """Directory and .sym base name, as stackwalk and the extension expect it."""
        return os.path.basename(self.debug_file or self.code_file)


@dataclass
class DumpInfo:
    arch: str
    crash_thread: int
    modules: list
    frame_modules_by_thread: dict


# --------------------------------------------------------------------------- tools

def find_tool(name, explicit, env_var, flag):
    for source, cand in ((flag, explicit), (env_var, os.environ.get(env_var))):
        if cand:
            p = Path(cand).expanduser()
            if p.is_file() and os.access(p, os.X_OK):
                return p
            die(f"{source}={cand} is not an executable file")
    found = shutil.which(name)
    if found:
        return Path(found)
    p = DUMPSTORM_BIN / name
    if p.is_file() and os.access(p, os.X_OK):
        return p
    die(f"{name} not found on PATH or in {DUMPSTORM_BIN}. "
        f"Pass {flag} or set {env_var}. Prebuilts: muse_deps/prebuilt.lock.")


# --------------------------------------------------------------------------- dump

def read_dump(stackwalk, dump):
    r = run([stackwalk, "-m", dump], capture_output=True)
    if not r.stdout.strip():
        die(f"minidump_stackwalk could not read {dump}:\n{r.stderr.strip()}")
    arch, crash_thread, modules, frames = "", -1, [], {}
    for line in r.stdout.splitlines():
        f = line.split("|")
        if f[0] == "CPU" and len(f) > 1:
            arch = f[1]
        elif f[0] == "Crash" and len(f) > 3 and f[3].isdigit():
            crash_thread = int(f[3])
        elif f[0] == "Module" and len(f) >= 8:
            modules.append(Module(f[1], f[3], f[4].upper(), f[7] == "1"))
        elif len(f) >= 7 and f[0].isdigit() and f[1].isdigit():
            # thread|frame|module|function|file|line|offset
            frames.setdefault(int(f[0]), []).append(os.path.basename(f[2]))
    if not modules:
        die("the dump contains no module list")
    return DumpInfo(arch, crash_thread, modules, frames)


def crashpad_annotations(dump):
    """Annotations Crashpad stored in the dump, as (scope, text) pairs.

    scope is "" for process-level annotations (passed to crashpad_handler at start) and
    the module's basename for per-module ones: key=value pairs set through CrashpadInfo,
    and free-text notes such as "abort() called" or the uncaught-exception message."""
    try:
        data = dump.read_bytes()
        if data[:4] != b"MDMP":
            return []
        nstreams, dir_rva = struct.unpack_from("<II", data, 8)
        streams = {}
        for i in range(nstreams):
            stream_type, _size, rva = struct.unpack_from("<III", data, dir_rva + 12 * i)
            streams.setdefault(stream_type, rva)

        def u16(rva):
            n = struct.unpack_from("<I", data, rva)[0]
            return data[rva + 4:rva + 4 + n].decode("utf-16le", "replace")

        def u8(rva):
            n = struct.unpack_from("<I", data, rva)[0]
            return data[rva + 4:rva + 4 + n].decode("utf-8", "replace")

        def simple_dict(rva):
            if not rva:
                return {}
            n = struct.unpack_from("<I", data, rva)[0]
            return {u8(k): u8(v) for k, v in
                    (struct.unpack_from("<II", data, rva + 4 + 8 * i) for i in range(n))}

        module_names = []
        if 4 in streams:  # MINIDUMP_MODULE_LIST
            rva = streams[4]
            n = struct.unpack_from("<I", data, rva)[0]
            for i in range(n):
                name_rva = struct.unpack_from("<I", data, rva + 4 + 108 * i + 20)[0]
                module_names.append(os.path.basename(u16(name_rva)))

        out = []
        if 0x43500001 not in streams:  # MinidumpCrashpadInfo
            return out
        rva = streams[0x43500001]
        _sa_size, sa_rva, _ml_size, ml_rva = struct.unpack_from("<IIII", data, rva + 4 + 16 + 16)
        out += [("", f"{k}={v}") for k, v in simple_dict(sa_rva).items()]
        if ml_rva:
            n = struct.unpack_from("<I", data, ml_rva)[0]
            for i in range(n):
                idx, _loc_size, loc_rva = struct.unpack_from("<III", data, ml_rva + 4 + 12 * i)
                _ver, la_size, la_rva, _sa_size, msa_rva = struct.unpack_from("<IIIII", data, loc_rva)
                scope = module_names[idx] if idx < len(module_names) else f"module {idx}"
                out += [(scope, f"{k}={v}") for k, v in simple_dict(msa_rva).items()]
                if la_rva:
                    cnt = struct.unpack_from("<I", data, la_rva)[0]
                    out += [(scope, u8(struct.unpack_from("<I", data, la_rva + 4 + 4 * j)[0]))
                            for j in range(cnt)]
        return out
    except (struct.error, IndexError, UnicodeDecodeError, OSError) as e:
        log(f"warning: could not read Crashpad annotations: {e}")
        return []


def select_modules(info, args):
    by_name = {}
    for m in info.modules:
        by_name.setdefault(m.name, m)
        by_name.setdefault(os.path.basename(m.code_file), m)
    if args.modules:
        wanted = set(args.modules)
    else:
        wanted = {m.name for m in info.modules if m.main}
        if args.all_threads:
            for names in info.frame_modules_by_thread.values():
                wanted.update(names)
        else:
            wanted.update(info.frame_modules_by_thread.get(info.crash_thread, []))
    wanted.discard("")
    selected, unknown = {}, []
    for n in sorted(wanted):
        m = by_name.get(n)
        if m is None:
            unknown.append(n)
        else:
            selected[id(m)] = m
    ordered = sorted(selected.values(), key=lambda m: (not m.main, m.name.lower()))
    return ordered, unknown


# --------------------------------------------------------------------------- binaries

def expand_roots(patterns):
    roots = []
    for pat in patterns:
        pat = os.path.expanduser(pat)
        hits = sorted(glob.glob(pat))
        if not hits and not any(ch in pat for ch in "*?["):
            log(f"warning: search path does not exist: {pat}")
        roots.extend(Path(h) for h in hits)
    return roots


def is_macho(path):
    try:
        with open(path, "rb") as f:
            return f.read(4) in MACHO_MAGICS
    except OSError:
        return False


def find_candidates(roots, wanted_names):
    """Map basename -> Mach-O files with that name under the roots.

    Files given directly are always included, whatever their name."""
    found = {}
    for root in roots:
        if root.is_file():
            if is_macho(root):
                found.setdefault(root.name, []).append(root)
            continue
        for dirpath, _dirnames, filenames in os.walk(root, followlinks=False):
            for fn in filenames:
                if fn in wanted_names:
                    p = Path(dirpath) / fn
                    if not p.is_symlink() and is_macho(p):
                        found.setdefault(fn, []).append(p)
    return found


def parse_header(first_line):
    # MODULE <os> <arch> <debug_id> <name>
    parts = first_line.strip().split(" ", 4)
    if len(parts) != 5 or parts[0] != "MODULE":
        return None
    return {"os": parts[1], "arch": parts[2], "id": parts[3].upper(), "name": parts[4]}


def identity(dump_syms, path, arch):
    """MODULE header of a binary for the dump's architecture.

    If the file has no slice for that architecture, fall back to whatever it does
    contain so the listing can say e.g. "x86_64 only"; its ID cannot match anyway."""
    for arch_args in (["-a", arch] if arch else [], []):
        r = run([dump_syms, "-i", *arch_args, path], capture_output=True)
        if r.returncode == 0 and r.stdout:
            return parse_header(r.stdout.splitlines()[0])
    return None


def describe(header, dump_arch):
    if not header:
        return "(unreadable)"
    text = header["id"][:8]
    if dump_arch and header["arch"] != dump_arch:
        text += f" {header['arch']} only"
    return text


def find_dsym(binary):
    cands = [binary.with_name(binary.name + ".dSYM")]
    for parent in binary.parents:
        if parent.suffix in (".app", ".framework", ".vst3", ".component", ".bundle"):
            cands.append(parent.with_name(parent.name + ".dSYM"))
            break
    return next((c for c in cands if c.is_dir()), None)


def make_dsym(binary, tmpdir):
    out = Path(tmpdir) / (binary.name + ".dSYM")
    log(f"  running dsymutil on {binary} (may take a while and a lot of memory)")
    r = run(["dsymutil", binary, "-o", out], capture_output=True)
    if r.returncode != 0 or not out.is_dir():
        log(f"  dsymutil failed: {r.stderr.strip()[:400]}")
        return None
    return out


def dsym_argument(dsym):
    dwarf_dir = dsym / "Contents" / "Resources" / "DWARF"
    files = [p for p in dwarf_dir.iterdir()] if dwarf_dir.is_dir() else []
    return files[0] if len(files) == 1 else dsym


# --------------------------------------------------------------------------- symbols

def sym_path(symbols_dir, module):
    return symbols_dir / module.name / module.debug_id / f"{module.name}.sym"


def check_existing(symbols_dir, module):
    """Return (path, header) for an existing .sym, header None if unreadable."""
    out = sym_path(symbols_dir, module)
    if not out.is_file():
        return None, None
    try:
        with open(out, errors="replace") as f:
            return out, parse_header(f.readline())
    except OSError:
        return out, None


def write_sym(dump_syms, binary, module, arch, dsym, symbols_dir):
    out = sym_path(symbols_dir, module)
    out.parent.mkdir(parents=True, exist_ok=True)
    tmp = out.with_name(f".{module.name}.sym.partial")
    attempts = [True, False] if dsym else [False]
    for use_dsym in attempts:
        cmd = [dump_syms]
        if arch:
            cmd += ["-a", arch]
        if use_dsym:
            cmd += ["-g", dsym_argument(dsym)]
        cmd.append(binary)
        with open(tmp, "w") as f:
            r = run(cmd, stdout=f, stderr=subprocess.PIPE)
        header = None
        if r.returncode == 0:
            with open(tmp, errors="replace") as f:
                header = parse_header(f.readline())
        if header and header["id"] == module.debug_id:
            os.replace(tmp, out)
            return out, use_dsym
        why = r.stderr.strip()[:300] if r.returncode != 0 else f"header {header} does not match"
        log(f"  dump_syms {'with dSYM ' if use_dsym else ''}failed for {binary}: {why}")
    tmp.unlink(missing_ok=True)
    return None, False


def has_line_info(sym):
    """True if the .sym carries source line tables (FILE records), which dump_syms
    only emits when it had DWARF, i.e. a dSYM or an unstripped binary with debug info."""
    try:
        with open(sym, errors="replace") as f:
            for _ in range(2000):
                line = f.readline()
                if not line:
                    break
                if line.startswith("FILE "):
                    return True
    except OSError:
        pass
    return False


# --------------------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("dump", help="minidump file (.dmp)")
    ap.add_argument("-s", "--search", action="append", metavar="PATH",
                    help="where to look for the binaries: a .app, a build tree, a directory or a "
                         "single file; globs allowed; repeatable. Default: %s" % ", ".join(DEFAULT_SEARCH))
    ap.add_argument("--symbols", default=os.environ.get("BREAKPAD_SYMBOLS_DIR", str(DEFAULT_SYMBOLS_DIR)),
                    help="Breakpad symbol store (default: $BREAKPAD_SYMBOLS_DIR or %(default)s)")
    ap.add_argument("--dump-syms", metavar="BIN", help="path to Breakpad dump_syms")
    ap.add_argument("--stackwalk", metavar="BIN", help="path to minidump_stackwalk")
    ap.add_argument("-m", "--modules", nargs="+", metavar="NAME",
                    help="only these modules (default: main module + modules on the crashing thread)")
    ap.add_argument("--all-threads", action="store_true",
                    help="also symbolicate modules that appear on non-crashing threads")
    ap.add_argument("--dsymutil", action="store_true",
                    help="run dsymutil on matched binaries that have no dSYM, so the .sym gets file:line info")
    ap.add_argument("--force", action="store_true", help="regenerate .sym files that already exist")
    ap.add_argument("--list", action="store_true", help="list modules and matching binaries, write nothing")
    ap.add_argument("-c", "--crashing-thread-only", action="store_true",
                    help="print only the crashing thread (minidump_stackwalk -c)")
    ap.add_argument("-o", "--output", metavar="FILE", help="write the stack trace here instead of stdout")
    args = ap.parse_args()

    dump = Path(args.dump).expanduser()
    if not dump.is_file():
        die(f"dump file not found: {dump}")
    stackwalk = find_tool("minidump_stackwalk", args.stackwalk, "MINIDUMP_STACKWALK", "--stackwalk")
    dump_syms = find_tool("dump_syms", args.dump_syms, "DUMP_SYMS", "--dump-syms")
    symbols_dir = Path(args.symbols).expanduser()

    info = read_dump(stackwalk, dump)
    arch = ARCH_NAMES.get(info.arch, info.arch)
    selected, unknown = select_modules(info, args)
    log(f"dump:      {dump}")
    log(f"           {info.arch}, {len(info.modules)} modules, crashing thread {info.crash_thread}")
    for scope, text in crashpad_annotations(dump):
        log(f"annotation: {scope + ': ' if scope else ''}{text}")
    log(f"symbols:   {symbols_dir}")
    for n in unknown:
        log(f"warning: --modules {n}: no such module in the dump")

    roots = expand_roots(args.search or DEFAULT_SEARCH)
    log("searching: " + (", ".join(str(r) for r in roots) or "(nothing)"))
    wanted_names = {m.name for m in selected} | {os.path.basename(m.code_file) for m in selected}
    candidates = find_candidates(roots, wanted_names)

    # Identify every candidate once: debug ID -> binary.
    by_id, seen = {}, {}
    for name, paths in candidates.items():
        for p in paths:
            h = identity(dump_syms, p, arch)
            seen[p] = describe(h, arch)
            if h and h["arch"] == arch:
                by_id.setdefault(h["id"], p)

    log("")
    log("module".ljust(28) + "debug id".ljust(35) + "status")
    statuses, exit_code = {}, 0
    tmpdir = tempfile.mkdtemp(prefix="symbolicate-") if args.dsymutil else None
    try:
        for m in selected:
            label = f"{m.name}{' (main)' if m.main else ''}".ljust(28) + m.debug_id.ljust(35)
            binary = by_id.get(m.debug_id)
            existing, header = check_existing(symbols_dir, m)

            if args.list:
                if binary:
                    status = f"match: {binary}"
                elif existing:
                    status = "no local binary; .sym present"
                else:
                    status = "no local binary"
                same_name = [p for p in candidates.get(m.name, []) if p != binary]
                if same_name:
                    status += "; other builds: " + ", ".join(f"{p} [{seen[p]}]" for p in same_name)
                log(label + status)
                continue

            if existing and header and header["id"] == m.debug_id and not args.force:
                if args.dsymutil and binary and not has_line_info(existing):
                    pass  # regenerate below to pick up line info
                else:
                    log(label + f"already have {existing}")
                    statuses[m.name] = "cached"
                    continue
            if existing and not (header and header["id"] == m.debug_id):
                bad_id = header["id"] if header else "unreadable"
                aside = existing.with_name(f"{m.name}.sym.wrong-id-{bad_id}")
                os.replace(existing, aside)
                log(label + f"WRONG .sym (header id {bad_id}), moved to {aside.name}")
                label = " " * len(label)

            if not binary:
                same_name = candidates.get(m.name, [])
                if same_name:
                    detail = "no matching binary; other builds: " + ", ".join(
                        f"{p} [{seen[p]}]" for p in same_name)
                else:
                    detail = "no local binary"
                log(label + detail)
                statuses[m.name] = "missing"
                if m.main:
                    exit_code = 1
                continue

            dsym = find_dsym(binary)
            if dsym is None and args.dsymutil:
                dsym = make_dsym(binary, tmpdir)
            out, used_dsym = write_sym(dump_syms, binary, m, arch, dsym, symbols_dir)
            if out:
                kind = "with file:line" if used_dsym else "function names only (no dSYM)"
                log(label + f"generated from {binary} ({kind})")
                statuses[m.name] = "generated"
            else:
                log(label + f"FAILED for {binary}")
                statuses[m.name] = "failed"
                exit_code = 1
    finally:
        if tmpdir:
            shutil.rmtree(tmpdir, ignore_errors=True)

    if args.list:
        return 0

    missing = [n for n, s in statuses.items() if s == "missing"]
    log("")
    if missing:
        log("no symbols for: " + ", ".join(missing)
            + "  (system libraries are expected here; for anything else add --search <path>)")
    main_mod = next((m for m in selected if m.main), None)
    if main_mod and statuses.get(main_mod.name) == "missing":
        log(f"The dump's main module {main_mod.name} [{main_mod.debug_id}] was not found. "
            f"Pass --search with the .app bundle or build tree of the exact build that crashed.")

    cmd = [stackwalk] + (["-c"] if args.crashing_thread_only else []) + [dump, symbols_dir]
    r = run(cmd, capture_output=True)
    if args.output:
        Path(args.output).write_text(r.stdout)
        log(f"stack trace written to {args.output}")
    else:
        log("")
        sys.stdout.write(r.stdout)
        sys.stdout.flush()
    return exit_code


if __name__ == "__main__":
    sys.exit(main())
