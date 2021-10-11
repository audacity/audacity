#!/usr/bin/env bash

set -euxo pipefail

cmake_args=(
    -D CMAKE_BUILD_TYPE=Release

    -D audacity_conan_enabled=Off
    -D audacity_conan_allow_prebuilt_binaries=no

    -D audacity_lib_preference=system # Change the libs default to 'system'
    -D audacity_obey_system_dependencies=On # And force it!

    -D audacity_use_pch=no

    -D audacity_use_portsmf=local
    -D audacity_use_sbsms=local 
)

if [[ $1 == "prepare" ]]; then
    tar -xzf /work_dir/audacity-sources.tar.gz
    
    audacity/linux/packages/prepare_offline_dependencies.sh "${cmake_args[@]}"

    cp audacity-offline-dependencies.tar.gz /work_dir/audacity-offline-dependencies-fedora34.tar.gz
elif [[ $1 == "build" ]]; then
    tar -xzf /work_dir/audacity-sources.tar.gz
    tar -xzf /work_dir/audacity-offline-dependencies-fedora34.tar.gz

    audacity/linux/packages/build_package.sh "${cmake_args[@]}"

    cp audacity-linux_x86_64.tar.gz /work_dir/audacity-fedora34_x86_64.tar.gz
elif [[ $1 == "package" ]]; then
    mkdir -p rpmbuild/SOURCES
    cp /work_dir/*.tar.gz ./rpmbuild/SOURCES/

    rpmbuild -ba --rpmfcdebug audacity.spec

    cp -rv ./rpmbuild/RPMS /work_dir
    cp -rv ./rpmbuild/SRPMS /work_dir
fi
