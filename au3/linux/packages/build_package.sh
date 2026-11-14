#!/usr/bin/env bash

set -euxo pipefail

if [[ -d "audacity-offline-dependencies" ]]; then
    depsDir=$(readlink -f ./audacity-offline-dependencies)
    python3 -m venv conan_env

    source conan_env/bin/activate
    pip3 install --no-index --find-links "$depsDir/pip_cache" conan 

    export CONAN_USER_HOME="$depsDir/conan_home"
    mkdir -p $CONAN_USER_HOME

    conan config home
    conan config init
    conan config set storage.download_cache="$CONAN_USER_HOME/download_cache"
    conan profile update settings.compiler.libcxx=libstdc++11 default
fi

cmake -S audacity -B build $@
cmake --build build -- -j`nproc`

tar -czf audacity-linux_x86_64.tar.gz -C build/bin .
