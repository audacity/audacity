#!/usr/bin/env bash

set -euxo pipefail

scriptLocation=$(dirname "$(readlink -f "$0")")
audacityLocation=$(readlink -f "$scriptLocation/../..")
workDir="$scriptLocation/work_dir"

mkdir -p "$workDir"

pushd "$workDir"

tar --exclude '.git' \
    --exclude "audacity-sources.tar.gz" \
    -C "$audacityLocation/.." \
    -czf audacity-sources.tar.gz audacity

for distro in "$@"
do
    echo "Building $distro"

    imageName="audacity-packaging-$distro"
        
    docker build -t $imageName "$scriptLocation/$distro"
    docker run -ti --volume=$workDir:/work_dir --rm  $imageName prepare
    docker run -ti --volume=$workDir:/work_dir --rm --network none $imageName build
    docker run -ti --volume=$workDir:/work_dir --rm --network none $imageName package
done

rm audacity-sources.tar.gz

popd

cp -r $workDir/* ./
rm -R "$workDir"
