#!/bin/sh
# Run this script with locale as the current directory
set -o errexit
echo ";; Recreating sneedacity.pot using .h, .cpp and .mm files"
for path in ../modules/mod-* ../libraries/lib-* ../include ../src ../crashreports ; do
   find $path -name \*.h -o -name \*.cpp -o -name \*.mm
done | LANG=c sort | \
sed -E 's/\.\.\///g' |\
xargs xgettext \
--no-wrap \
--default-domain=sneedacity \
--directory=.. \
--keyword=_ --keyword=XO --keyword=XC:1,2c --keyword=XXO --keyword=XXC:1,2c --keyword=XP:1,2 --keyword=XPC:1,2,4c \
--add-comments=" i18n" \
--add-location=file  \
--copyright-holder='Sneedacity Team' \
--package-name="sneedacity" \
--package-version='3.0.3' \
--msgid-bugs-address="sneedacity-translation@lists.sourceforge.net" \
--add-location=file -L C -o sneedacity.pot 
echo ";; Adding nyquist files to sneedacity.pot"
for path in ../plug-ins ; do find $path -name \*.ny -not -name rms.ny; done | LANG=c sort | \
sed -E 's/\.\.\///g' |\
xargs xgettext \
--no-wrap \
--default-domain=sneedacity \
--directory=.. \
--keyword=_ --keyword=_C:1,2c --keyword=ngettext:1,2 --keyword=ngettextc:1,2,4c \
--add-comments=" i18n" \
--add-location=file  \
--copyright-holder='Sneedacity Team' \
--package-name="sneedacity" \
--package-version='3.0.3' \
--msgid-bugs-address="sneedacity-translation@lists.sourceforge.net" \
--add-location=file -L Lisp -j -o sneedacity.pot 
if test "${SNEEDACITY_ONLY_POT:-}" = 'y'; then
    return 0
fi
echo ";; Updating the .po files - Updating Project-Id-Version"
for i in *.po; do
    sed -e '/^"Project-Id-Version:/c\
    "Project-Id-Version: sneedacity 3.0.3\\n"' $i > TEMP; mv TEMP $i
done
echo ";; Updating the .po files"
sed 's/.*/echo "msgmerge --lang=& &.po sneedacity.pot -o &.po";\
msgmerge --no-wrap --lang=& &.po sneedacity.pot -o &.po;/g' LINGUAS | bash
echo ";; Removing '#~|' (which confuse Windows version of msgcat)"
for i in *.po; do
    sed '/^#~|/d' $i > TEMP; mv TEMP $i
done
echo ""
echo ";;Translation updated"
echo ""
head -n 11 sneedacity.pot | tail -n 3
wc -l sneedacity.pot
