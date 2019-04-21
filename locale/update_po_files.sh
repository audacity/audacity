#!/bin/sh
echo ";; Recreating audacity.pot using .h, .cpp and .mm files"
for path in ../lib-src/FileDialog ../lib-src/mod-script* ../lib-src/mod-nyq* ../include ../src ; do find $path -name \*.h -o -name \*.cpp -o -name \*.mm ; done | LANG=c sort | \
sed -E 's/\.\.\///g' |\
xargs xgettext \
--default-domain=audacity \
--directory=.. \
--keyword=_ --keyword=N_ --keyword=XO --keyword=XXO --keyword=wxPLURAL:1,2 \
--add-comments=" i18n" \
--add-location=file  \
--copyright-holder='Audacity Team' \
--package-name="audacity" \
--package-version='2.3.0' \
--msgid-bugs-address="audacity-translation@lists.sourceforge.net" \
--add-location=file -L C -o audacity.pot 
echo ";; Adding nyquist files to audacity.pot"
for path in ../plug-ins ; do find $path -name \*.ny -not -name rms.ny; done | LANG=c sort | \
sed -E 's/\.\.\///g' |\
xargs xgettext \
--default-domain=audacity \
--directory=.. \
--keyword=_ --keyword=N_ --keyword=XO --keyword=XXO --keyword=wxPLURAL:1,2 \
--add-comments=" i18n" \
--add-location=file  \
--copyright-holder='Audacity Team' \
--package-name="audacity" \
--package-version='2.3.0' \
--msgid-bugs-address="audacity-translation@lists.sourceforge.net" \
--add-location=file -L Lisp -j -o audacity.pot 
echo ";; Updating the .po files"
sed 's/.*/echo "msgmerge --lang=& &.po audacity.pot -o &.po";\
msgmerge --lang=& &.po audacity.pot -o &.po;/g' LINGUAS | bash
echo ";; Removing '#~|' (which confuse Windows version of msgcat)"
for i in *.po; do
    sed -i '/^#~|/d' $i
done
echo ""
echo ";;Translation updated"
echo ""
head -n 11 audacity.pot | tail -n 3
wc -l audacity.pot
