#!/usr/bin/env bash

conan --version

if [ ! -d "audacity" ]
then
    git clone https://github.com/audacity/audacity
fi

mkdir -p build

cd build

cmake_options=(
    -G "Unix Makefiles"
    -DCMAKE_BUILD_TYPE=Release
    -Daudacity_lib_preference=system # Change the libs default to 'system'
    -Daudacity_obey_system_dependencies=On # And force it!
    -Daudacity_use_wxwidgets=local # wxWidgets 3.1 is not available
    -Daudacity_use_expat=system
    -Daudacity_use_lame=system
    -Daudacity_use_sndfile=system
    -Daudacity_use_soxr=system
    -Daudacity_use_portaudio=local # System portaudio is not yet usable
    -Daudacity_use_sqlite=local # We prefer using the latest version of SQLite
    -Daudacity_use_ffmpeg=loaded
    -Daudacity_use_id3tag=system # This library has bugs, that are fixed in *local* version
    -Daudacity_use_mad=system # This library has bugs, that are fixed in *local* version
    -Daudacity_use_nyquist=local # This library is not available
    -Daudacity_use_vamp=local # The dev package for this library is not available
    -Daudacity_use_ogg=system 
    -Daudacity_use_vorbis=system
    -Daudacity_use_flac=system
    -Daudacity_use_lv2=system
    -Daudacity_use_midi=system
    -Daudacity_use_portmixer=local # This library is not available
    -Daudacity_use_portsmf=system
    -Daudacity_use_sbsms=local # We prefer using the latest version of sbsms
    -Daudacity_use_soundtouch=system
    -Daudacity_use_twolame=system
    -Daudacity_has_networking=yes 
    -Daudacity_use_curl=system
)

cmake "${cmake_options[@]}" ../audacity

exit_status=$?

if [ $exit_status -ne 0 ]; then
    exit $exit_status
fi

make -j`nproc`

cd bin/Release
mkdir -p "Portable Settings"

ls -la .
