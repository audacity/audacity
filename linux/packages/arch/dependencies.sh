#!/usr/bin/env bash

build_deps=(
   gcc
   git
   cmake
   python-pip
)

deps=(
   zlib
   alsa-lib
   gtk2
   expat
   libid3tag
   libogg
   libvorbis
   flac
   lame
   twolame
   libsndfile
   jack
   lilv
   lv2
   portaudio
   portsmf
   portmidi
   suil
   vamp-plugin-sdk
   libsoxr
   soundtouch
   # sbsms is not available on arch
   # sbsms
   libpng
   libjpeg-turbo
   libsm
   ffmpeg
   harfbuzz
   freetype2
   fontconfig
   mesa
   mpg123
   wavpack
   wxwidgets-gtk3
   vst3sdk
   rapidjson
   opusfile
   opus
)

pacman -Syu --noconfirm \
   "${build_deps[@]}" \
   "${deps[@]}"

# fakeroot is broken since 1.35 as of Oct 1 2024, https://github.com/docker/for-mac/issues/7331
pacman -U --noconfirm https://archive.archlinux.org/packages/f/fakeroot/fakeroot-1.34-1-x86_64.pkg.tar.zst
