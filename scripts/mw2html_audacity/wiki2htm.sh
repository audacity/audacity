#!/bin/sh

helpdir="../../help/manual"
tmpdir_="../../help/temp"
srcuri="https://alphamanual.audacityteam.org/man"

mkdir -p "${tmpdir_}"
python3 mw2html.py "${srcuri}" "${tmpdir_}" -s
mv "${tmpdir_}/alphamanual.audacityteam.org" "${helpdir}"
rm -r "${tmpdir_}"
