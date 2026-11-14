#!/usr/bin/env bash

set -euo pipefail

mkdir -p rpmbuild/SOURCES
cp /work_dir/*.tar.gz ./rpmbuild/SOURCES/

sources=$(ls ./rpmbuild/SOURCES/audacity-sources-*)
tar_dirname=$(basename ${sources} .tar.gz)
version=$(echo ${tar_dirname} | sed -e 's/audacity-sources-//g' -e 's/.tar.gz//g' -e 's/-/./g' -e 's/\.[0-9]\++/.r/g')

echo "Audacity version: ${version}"

buildLevel=2

if [[ "${tar_dirname}" == *alpha* ]]; then
    buildLevel=0
elif [[ "${tar_dirname}" == *beta* ]]; then
    buildLevel=1
fi

echo "Audacity build level: ${buildLevel}"

#deps=$(ls ./rpmbuild/SOURCES/audacity-dependencies-*)

sed -i -e "s|TMPL_AUDACITY_VERSION|${version}|g" audacity.spec
sed -i -e "s|TMPL_AUDACITY_SOURCES|${sources}|g" audacity.spec
sed -i -e "s|TMPL_AUDACITY_TAR_DIRNAME|${tar_dirname}|g" audacity.spec
#sed -i -e "s|TMPL_AUDACITY_DEPENDENCIES|${deps}|g" audacity.spec

rpmbuild -ba --rpmfcdebug audacity.spec

cp -rv ./rpmbuild/RPMS /work_dir
cp -rv ./rpmbuild/SRPMS /work_dir
