for path in ../plug-ins ; do find $path -name \*.ny ; done | \
sed -r 's/\.\.\///g' |\
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
--add-location=file -L EmacsLisp -j -o audacity.pot 





