#!/bin/bash
set -ev
DST="."
HOST=x86_64-w64-mingw32.static

ID=$(git show -s --format="%h")
DST="${DST}/${ID}"

mkdir -pv "${DST}"

# main binary
cp -uv audacity.exe "${DST}"

# misc files
cp -uv README.txt LICENSE.txt "${DST}"

# presets
cp -uv presets/* "${DST}"

# linked wxWidgets dlls
if [ -z "${WX_CONFIG}" ]; then
   WX_CONFIG=$(which "${HOST}-wx-config")
fi
WX_LIBDIR=$(dirname "${WX_CONFIG}")
WX_LIBDIR=$(readlink -f "${WX_LIBDIR}/../lib")

for DLL in $(${HOST}-objdump -x audacity.exe  | grep 'DLL Name: wx' | sed 's/^.*: //'); do
   cp -uv "${WX_LIBDIR}/${DLL}" "${DST}"
done

# strip debug symbols
${HOST}-strip "${DST}"/*.exe "${DST}"/*.dll

# plugins
mkdir -pv "${DST}/Plug-Ins"
cp -uv plug-ins/*.ny "${DST}/Plug-Ins"

# nyquist
mkdir -pv "${DST}/Nyquist"
cp -ruv nyquist/* "${DST}/Nyquist"

# translations
for POFILE in locale/*.po; do
   LANG=$(basename "${POFILE}")
   LANG=${LANG//.po/}
   mkdir -pv "${DST}/Languages/${LANG}"
   MOFILE="${DST}/Languages/${LANG}/Audacity.mo"
   if [ "${MOFILE}" -ot "${POFILE}" ]; then
      msgfmt -o "${MOFILE}" "${POFILE}"
   fi
done

if [ ! -d "${WX_SRC}" ]; then
   WX_SRC=wxWidgets-master
   wget https://github.com/audacity/wxWidgets/archive/master.zip
   unzip master.zip
fi

for POFILE in "${WX_SRC}/locale/"*.po; do
   LANG=$(basename "${POFILE}")
   LANG=${LANG//.po/}
   mkdir -pv "${DST}/Languages/${LANG}"
   MOFILE="${DST}/Languages/${LANG}/wxstd.mo"
   if [ "${MOFILE}" -ot "${POFILE}" ]; then
      msgfmt -o "${MOFILE}" "${POFILE}"
   fi
done

# help
#if [ ! -d "help/manual" ]; then
#   (cd scripts/mw2html_audacity && ./wiki2htm.sh)
#fi
#mkdir -pv "${DST}/help"
#cp -ruv help/manual "${DST}/help"

# for the poor souls that use notepad
for TEXTFILE in "${DST}"/*.txt "${DST}"/*.xml \
   "${DST}"/Plug-Ins/*.ny "${DST}"/Nyquist/*.lsp; do
   unix2dos "${TEXTFILE}"
done

# make the zip
rm -f "${DST}.zip"
zip -r "${DST}.zip" -r -9 "${DST}"
openssl sha256 "${DST}.zip" > "${DST}.zip.sha256"
