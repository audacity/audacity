#!/bin/bash

case "$1" in
	w32)
		compiler_name=i686-w64-mingw32
		;;
	w64)
		compiler_name=x86_64-w64-mingw32
		;;
	*)
		echo "$0 (w32|w64) <other args>"
		exit 0
		;;
	esac

shift

build_cpu=$(dpkg-architecture -qDEB_BUILD_GNU_CPU)
build_host=$build_cpu-linux

./configure --host=$compiler_name --target=$compiler_name --build=$build_host \
		--program-prefix='' --disable-sqlite --disable-static $@
