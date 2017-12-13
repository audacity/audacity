#!/bin/bash

BUILD_PHASE=$1

echo $0:${TARGET} $BUILD_PHASE

case $TARGET in
linux-*)

   case $BUILD_PHASE in
   before_install)
      set -ex
      sudo apt-get update -qq
      sudo apt-get install -y libwxgtk3.0-dev libgtk2.0-dev
   ;;
   before_script)
      set -ex
   ;;
   script)
      set -ex
      CXX="g++-4.9" CC="gcc-4.9" ./configure --enable-openmp
      make -j2
   ;;
   *)
      echo "Invalid build phase: '$BUILD_PHASE'"
      exit 1
      ;;
   esac
;;
windows-*)
   # host triplet for cross compiling
   # expands to: i686-w64-mingw32.static
   #         or: x86_64-w64-mingw32.static
   export HOST=${TARGET//windows-/}-w64-mingw32.static

   case $BUILD_PHASE in
   before_install)
      set -ex
      # install mxe environment and a few dependencies
      # todo: download Audacity version of wxwidgets
      sudo sh -c 'echo "deb http://pkg.mxe.cc/repos/apt/debian wheezy main" > /etc/apt/sources.list.d/mxeapt.list'
      sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D43A795B73B16ABE9643FE1AFD8FFF16DB45C6AB
      sudo apt-get update -qq
      PKG_PREFIX="mxe-${HOST//_/-}"
      PKG_LIST=" gcc expat ogg vorbis wxwidgets libsndfile portmidi"
      sudo apt-get install -y ${PKG_LIST// / $PKG_PREFIX-}
   ;;
   before_script)
      set -ex
   ;;
   script)
      set -ex
      unset CC
      unset CXX
      export PATH=/usr/lib/mxe/usr/bin:${PATH}
      ${HOST}-g++ -v
      ./configure --host=${HOST} --enable-openmp
      make -j2
   ;;
   *)
      echo "Invalid build phase: '$BUILD_PHASE'"
      exit 1
      ;;
   esac
;;
esac
