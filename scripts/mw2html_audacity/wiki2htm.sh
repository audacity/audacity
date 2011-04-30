#!/bin/sh

helpdir="../../help/manual"
tmpdir_="../../help/temp"
srcuri="http://manual.audacityteam.org/man"

mkdir -p "${tmpdir_}"
python mw2html.py "${srcuri}" "${tmpdir_}" -s
mv "${tmpdir_}/manual.audacityteam.org" "${helpdir}"
rm -r "${tmpdir_}"
