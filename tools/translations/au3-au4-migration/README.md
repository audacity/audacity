# au3 -> AU4 translation migration

One-shot scripts that ported Audacity's translations from au3's
gettext stack to AU4's Qt-native .ts/.qm. Kept for reproducibility;
not invoked by build or CI.

The live pipeline is `tools/translations/run_lupdate.sh`.

## Reproduce

```bash
cd tools/translations/au3-au4-migration

# Optional: regenerate the AU4-reachable au3 file list (only when
# au3 libraries have been added/removed since migration).
python3 build_scan_paths.py

# Rewrite XO/XXO/XC/Verbatim/_() in au3/ and src/ to
# muse::TranslatableString / muse::trc. In-place.
python3 migrate_xo.py --apply

# Import legacy translations from au3/locale/*.po into
# share/locale/audacity_*.ts. Idempotent.
python3 import_po_to_ts.py

# Recover entries whose source changed only in case.
python3 salvage_case_mismatch.py

# Fuzzy fill remaining unfinished entries against PO sources.
python3 reuse_po_translations.py \
    --case-insensitive --strip-punct --strip-whitespace --min-len 4
```

Each script takes `--help`.

## Inputs

- `au3/locale/*.po`, `au3/locale/LINGUAS` -- legacy PO catalogues.
- `share/locale/audacity_*.ts` -- pre-existing AU4 catalogues with
  translator-curated QML strings. Never run lupdate with `-no-obsolete`.
