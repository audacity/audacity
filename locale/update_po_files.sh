#!/bin/sh
set -o errexit

# Enter the script directory
CDPATH= cd -- "$(dirname -- "$0")"

echo ";; Recreating audacity.pot using .h, .cpp and .mm files"
for path in ../modules/* ../libraries/lib-* ../include ../src ../crashreports ; do
   find $path -name \*.h -o -name \*.cpp -o -name \*.mm
done | LANG=c sort | \
sed -E 's/\.\.\///g' |\
xargs xgettext \
--no-wrap \
--default-domain=audacity \
--directory=.. \
--keyword=_ --keyword=XO --keyword=XC:1,2c --keyword=XXO --keyword=XXC:1,2c --keyword=XP:1,2 --keyword=XPC:1,2,4c \
--add-comments=" i18n" \
--add-location=file  \
--copyright-holder='Audacity Team' \
--package-name="audacity" \
--package-version='3.6.0' \
--msgid-bugs-address="audacity-translation@lists.sourceforge.net" \
--add-location=file -L C -o audacity.pot 
echo ";; Adding nyquist files to audacity.pot"
for path in ../plug-ins ; do find $path -name \*.ny -not -name rms.ny; done | LANG=c sort | \
sed -E 's/\.\.\///g' |\
xargs xgettext \
--no-wrap \
--default-domain=audacity \
--directory=.. \
--keyword=_ --keyword=_C:1,2c --keyword=ngettext:1,2 --keyword=ngettextc:1,2,4c \
--add-comments=" i18n" \
--add-location=file  \
--copyright-holder='Audacity Team' \
--package-name="audacity" \
--package-version='3.6.0' \
--msgid-bugs-address="audacity-translation@lists.sourceforge.net" \
--add-location=file -L Lisp -j -o audacity.pot 
echo ";; Adding resource files to audacity.pot"
for path in ../resources ; do 
   find $path -name \*.xml 
done | \
sed -E 's/\.\.\///g' |\
xargs xgettext \
--its=resources.its \
--no-wrap \
--default-domain=audacity \
--directory=.. \
--add-location=file  \
--copyright-holder='Audacity Team' \
--package-name="audacity" \
--package-version='3.6.0' \
--msgid-bugs-address="audacity-translation@lists.sourceforge.net" \
-j -o audacity.pot 

if test "${AUDACITY_ONLY_POT:-}" = 'y'; then
    return 0
fi
echo ";; Updating the .po files - Updating Project-Id-Version"
for i in *.po; do
    sed -e '/^"Project-Id-Version:/c\
    "Project-Id-Version: audacity 3.6.0\\n"' $i > TEMP; mv TEMP $i
done
echo ";; Updating the .po files"
sed 's/.*/echo "msgmerge --lang=& &.po audacity.pot -o &.po";\
msgmerge --no-wrap --lang=& &.po audacity.pot -o &.po;/g' LINGUAS | bash
echo ";; Removing '#~|' (which confuse Windows version of msgcat)"
for i in *.po; do
    sed '/^#~|/d' $i > TEMP; mv TEMP $i
done
echo ""
echo ";;Translation updated"
echo ""
head -n 11 audacity.pot | tail -n 3
wc -l audacity.pot
