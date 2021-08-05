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
   libmad
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
)

pacman -Syu --noconfirm \
   "${build_deps[@]}" \
   "${deps[@]}" 
