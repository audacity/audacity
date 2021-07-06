#!/usr/bin/env bash

conan --version

if [ ! -d "sneedacity" ]
then
    git clone https://github.com/sneedacity/sneedacity
fi

mkdir -p build

cd build

cmake_options=(
    -G "Unix Makefiles"
    -DCMAKE_BUILD_TYPE=Release
    -Dsneedacity_lib_preference=system # Change the libs default to 'system'
    -Dsneedacity_obey_system_dependencies=On # And force it!
    -Dsneedacity_use_wxwidgets=local # wxWidgets 3.1 is not available
    -Dsneedacity_use_expat=system
    -Dsneedacity_use_lame=system
    -Dsneedacity_use_sndfile=system
    -Dsneedacity_use_soxr=system
    -Dsneedacity_use_portaudio=local # System portaudio is not yet usable
    -Dsneedacity_use_sqlite=local # We prefer using the latest version of SQLite
    -Dsneedacity_use_ffmpeg=loaded
    -Dsneedacity_use_id3tag=system # This library has bugs, that are fixed in *local* version
    -Dsneedacity_use_mad=system # This library has bugs, that are fixed in *local* version
    -Dsneedacity_use_nyquist=local # This library is not available
    -Dsneedacity_use_vamp=local # The dev package for this library is not available
    -Dsneedacity_use_ogg=system 
    -Dsneedacity_use_vorbis=system
    -Dsneedacity_use_flac=system
    -Dsneedacity_use_lv2=system
    -Dsneedacity_use_midi=system
    -Dsneedacity_use_portmixer=local # This library is not available
    -Dsneedacity_use_portsmf=system
    -Dsneedacity_use_sbsms=local # We prefer using the latest version of sbsms
    -Dsneedacity_use_soundtouch=system
    -Dsneedacity_use_twolame=system
    -Dsneedacity_has_networking=yes 
    -Dsneedacity_use_curl=system
)

cmake "${cmake_options[@]}" ../sneedacity

exit_status=$?

if [ $exit_status -ne 0 ]; then
    exit $exit_status
fi

make -j`nproc`

cd bin/Release
mkdir -p "Portable Settings"

ls -la .
