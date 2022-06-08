#!/usr/bin/env bash

set -euxo pipefail

scriptLocation=$(dirname "$(readlink -f "$0")")
audacityLocation=$(readlink -f "$scriptLocation/../..")
workDir="$scriptLocation/work_dir"

mkdir -p "$workDir"

pushd "$workDir"

../prepare_offline_dependencies.sh

tar --exclude '.git' \
    --exclude "audacity-sources-3.2.0.tar.gz" \
    -C "$audacityLocation/.." \
    -czf audacity-sources-3.2.0.tar.gz audacity

for distro in "$@"
do
    echo "Building $distro"

    imageName="audacity-packaging-$distro"

    docker build -t $imageName "$scriptLocation/$distro"
    docker run -ti --volume=$workDir:/work_dir --rm --network none $imageName
done

rm audacity-sources-3.2.0.tar.gz

popd

cp -r $workDir/* ./
rm -R "$workDir"
