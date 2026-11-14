# Muse framework -> AU4 translation import

## Overview

Muse framework strings (dialogs, shortcut preferences, workspaces, ...)
are translated in MuseScore already. This tool copies those
translations into our catalogues. How a string travels:

1. `run_lupdate.sh` scans the muse framework sources and adds the
   string to `share/locale/audacity_en.ts`.
2. `import_musescore_ts.py` adds the string to every
   `audacity_<lang>.ts` and looks it up in MuseScore's
   `musescore_<lang>.ts` (same context, same source text).
3. Found: the translation is copied. Not found: the entry stays
   `unfinished` and goes to translators via Transifex.

Existing translations are never overwritten, so the script is safe to
re-run anytime - for example after a muse submodule bump.

## Usage

```bash
# Refresh the source catalogue:
tools/translations/run_lupdate.sh en

# Get MuseScore's catalogues (sparse clone, share/locale only),
# or use an up-to-date MuseScore checkout you already have:
git clone --depth 1 --filter=blob:none --sparse \
    https://github.com/musescore/MuseScore.git /tmp/ms
git -C /tmp/ms sparse-checkout set share/locale

# Fill translations (all languages, or --languages pl nl ...):
python3 tools/translations/muse-migration/import_musescore_ts.py \
    --musescore-locale /tmp/ms/share/locale
```
