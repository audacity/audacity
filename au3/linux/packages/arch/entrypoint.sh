#!/usr/bin/env bash

set -euo pipefail

cp -v /work_dir/*.tar.gz ./

ls -la .

sources=$(ls audacity-sources-*)
version=$(echo ${sources} | sed -e 's/audacity-sources-//g' -e 's/.tar.gz//g' -e 's/-/./g' -e 's/\.[0-9]\++/.r/g')

echo "Audacity version: ${version}"

buildLevel=2

if [[ "${sources}" == *alpha* ]]; then
    buildLevel=0
elif [[ "${sources}" == *beta* ]]; then
    buildLevel=1
fi

echo "Audacity build level: ${buildLevel}"

sed -i -e "s|TMPL_AUDACITY_VERSION|${version}|g" PKGBUILD
sed -i -e "s|TMPL_AUDACITY_SOURCES|${sources}|g" PKGBUILD

cat PKGBUILD

makepkg

ls -la /work_dir
id

cp *.zst /work_dir
