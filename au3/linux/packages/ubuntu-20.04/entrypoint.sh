#!/usr/bin/env bash

set -euxo pipefail

cp /work_dir/*.tar.gz ./

sources=$(ls ./audacity-sources-*)
tar_dirname=$(basename ${sources} .tar.gz)

version=$(echo ${tar_dirname} | sed -e 's/audacity-sources-//g' -e 's/.tar.gz//g' -e 's/-/./g' -e 's/\.[0-9]\++/.r/g')

echo "Audacity version: ${version}"

buildLevel=2

if [[ "${sources}" == *alpha* ]]; then
    buildLevel=0
elif [[ "${sources}" == *beta* ]]; then
    buildLevel=1
fi

echo "Audacity build level: ${buildLevel}"

deps=$(ls audacity-dependencies-*)

sed -i -e "s|TMPL_AUDACITY_VERSION|${version}|g" debian/changelog

tar zxf ${sources}
cp -R debian ${tar_dirname}

tar zxf ${deps} -C ${tar_dirname}

tar zcf audacity_${version}.orig.tar.gz ${tar_dirname}

pushd ${tar_dirname}
    tar zxf "../${deps}"

    debuild -us -uc
popd

cp -v *.deb /work_dir/
