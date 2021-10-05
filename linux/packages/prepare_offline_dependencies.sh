#!/usr/bin/env bash

# Usage prepare_offline_environment.sh cmake-options... 

set -euxo pipefail

packageName="audacity-offline-dependencies"
scriptLocation=$(dirname "$(readlink -f "$0")")
audacityLocation=$(readlink -f "$scriptLocation/../..")
resultsLocation=$(pwd)
packageLocation="$resultsLocation/$packageName"

echo "Audacity location: $audacityLocation"
echo "Result location: $resultsLocation"

mkdir -p $packageLocation

cd $packageLocation

# Create pip download cache containing all the
# packages requiredf to istall Conan

mkdir -p pip_cache

pushd pip_cache

    pip3 download conan

popd

# Cache all Conan dependencies

mkdir -p temp 

pushd temp
    python3 -m venv conan_env

    source conan_env/bin/activate
    pip3 install --no-index --find-links "$packageLocation/pip_cache" conan 

    export CONAN_USER_HOME="$packageLocation/conan_home"
    mkdir -p $CONAN_USER_HOME

    conan config home
    conan config init
    conan config set storage.download_cache="$CONAN_USER_HOME/download_cache"
    conan profile update settings.compiler.libcxx=libstdc++11 default

    # Configure Audacity so we can collect the required
    # packages.

    cmake ${@} $audacityLocation

    conan remove "*" --src --builds --packages --force 
popd

rm -R temp

cd $resultsLocation

tar -czf "$packageName.tar.gz" $packageName

rm -R $packageLocation
