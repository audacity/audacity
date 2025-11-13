#!/usr/bin/env bash

set -euo pipefail

build_deps=(
   gcc
   git
   cmake
   python-pip
)

deps=(
  alsa-lib
  expat
  ffmpeg
  flac
  gcc-libs
  gdk-pixbuf2
  glib2
  glibc
  gtk3
  gtkmm3
  jack
  lame
  libid3tag
  libmad
  libogg
  libsbsms
  libsndfile
  libsoxr
  libvorbis
  libxkbcommon-x11
  lilv
  lv2
  mpg123
  opusfile
  portaudio
  portmidi
  portsmf
  rapidjson
  soundtouch
  serd
  sord
  sratom
  sqlite
  suil
  twolame
  vamp-plugin-sdk
  vst3sdk
  wavpack
  wxwidgets-gtk3
  xcb-util
  xcb-util-cursor
  xcb-util-keysyms
  zlib
)

pacman -Syu --noconfirm \
   "${build_deps[@]}" \
   "${deps[@]}"

# fakeroot is broken since 1.35 as of Oct 1 2024, https://github.com/docker/for-mac/issues/7331
pacman -U --noconfirm https://archive.archlinux.org/packages/f/fakeroot/fakeroot-1.34-1-x86_64.pkg.tar.zst
