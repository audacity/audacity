#!/usr/bin/env bash

conan --version

if [ ! -d "audacity" ]
then
    git clone https://github.com/audacity/audacity
fi

mkdir -p build

cd build

cmake -G "Unix Makefiles" -DCMAKE_BUILD_TYPE=Release \
    -Daudacity_obey_system_dependencies=On \
    -Daudacity_use_wxwidgets=local \
    -Daudacity_use_expat=system \
    -Daudacity_use_lame=system \
    -Daudacity_use_sndfile=system \
    -Daudacity_use_soxr=system \
    -Daudacity_use_portaudio=local \
    -Daudacity_use_sqlite=local \
    -Daudacity_use_ffmpeg=loaded \
    -Daudacity_use_id3tag=system \
    -Daudacity_use_mad=system \
    -Daudacity_use_nyquist=local \
    -Daudacity_use_vamp=local \
    -Daudacity_use_ogg=system \
    -Daudacity_use_vorbis=system \
    -Daudacity_use_flac=system \
    -Daudacity_use_lv2=system \
    -Daudacity_use_midi=system \
    -Daudacity_use_portmixer=local \
    -Daudacity_use_portsmf=system \
    -Daudacity_use_sbsms=local \
    -Daudacity_use_soundtouch=system \
    -Daudacity_use_twolame=system \
    ../audacity

make -j`nproc`

cd bin/Release
mkdir -p "Portable Settings"

ls -la .
