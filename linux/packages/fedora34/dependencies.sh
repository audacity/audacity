#!/usr/bin/env bash

build_deps=(
   fedora-packager 
   @development-tools
   cmake
   gettext-devel
   gcc
   g++
   git
   python3
   python3-pip
)

deps=(
   libjpeg-turbo-devel
   alsa-lib-devel
   desktop-file-utils
   expat-devel
   flac-devel
   jack-audio-connection-kit-devel
   ladspa-devel
   lame-devel
   libid3tag-devel
   libmad-devel
   taglib-devel
   twolame-devel
   libogg-devel
   libsndfile-devel
   libvorbis-devel
   portaudio-devel
   portmidi-devel
   soundtouch-devel
   soxr-devel
   vamp-plugin-sdk-devel
   zlib-devel
   libuuid-devel
   wxGTK-devel
   gtk3-devel
   glib2-devel
   libappstream-glib
   #ffmpeg-devel
   sqlite-devel
   lv2-devel
   lilv-devel
   serd-devel
   sord-devel
   sratom-devel
   suil-devel
   flac-devel
)

dnf install -y \
   "${build_deps[@]}" \
   "${deps[@]}" 
