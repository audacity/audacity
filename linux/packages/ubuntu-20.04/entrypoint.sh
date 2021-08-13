#!/usr/bin/env bash

set -euxo pipefail

cmake_args=(
    -D CMAKE_BUILD_TYPE=Release

    -D audacity_conan_allow_prebuilt_binaries=no

    -D audacity_lib_preference=system # Change the libs default to 'system'
    -D audacity_obey_system_dependencies=On # And force it!

    -D audacity_use_pch=no

    -D audacity_use_wxwidgets=local # wxWidgets 3.1 is not available
    -D audacity_use_vamp=local
    -D audacity_use_sbsms=local # We rely on 2.2.0, 2.0.l2 is API incompatible
)

if [[ $1 == "prepare" ]]; then
    tar -xzf /work_dir/audacity-sources.tar.gz
    
    audacity/linux/packages/prepare_offline_dependencies.sh "${cmake_args[@]}"

    cp audacity-offline-dependencies.tar.gz /work_dir/audacity-offline-dependencies-ubuntu-20.04.tar.gz
elif [[ $1 == "build" ]]; then
    tar -xzf /work_dir/audacity-sources.tar.gz
    tar -xzf /work_dir/audacity-offline-dependencies-ubuntu-20.04.tar.gz

    audacity/linux/packages/build_package.sh "${cmake_args[@]}"

    cp audacity-linux_x86_64.tar.gz /work_dir/audacity-ubuntu-20.04_x86_64.tar.gz
elif [[ $1 == "package" ]]; then
    mkdir orig

    pushd orig
        tar -xzf /work_dir/audacity-sources.tar.gz

        pushd audacity
            tar -xzf /work_dir/audacity-offline-dependencies-ubuntu-20.04.tar.gz
        popd

        mv audacity audacity-3.0.3

        tar -czf ../audacity_3.0.3.orig.tar.gz audacity-3.0.3
    popd

    cp -r orig/audacity-3.0.3/* audacity-3.0.3

    rm -r orig

    pushd audacity-3.0.3
        debuild -us -uc
    popd

    cp -v *.deb /work_dir/
fi
