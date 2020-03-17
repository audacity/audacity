#!/usr/bin/make -f

# If this is set to true (via the environment) then CRC checking will be
# disabled in libogg giving fuzzers a better chance at finding something.
disable_ogg_crc ?= false

# Build libsndfile as a dynamic/shared library, but statically link to
# libFLAC, libogg, libopus and libvorbis

ogg_version = libogg-1.3.3
ogg_sha256sum = 4f3fc6178a533d392064f14776b23c397ed4b9f48f5de297aba73b643f955c08

vorbis_version = libvorbis-1.3.6
vorbis_sha256sum = af00bb5a784e7c9e69f56823de4637c350643deedaf333d0fa86ecdba6fcb415

flac_version = flac-1.3.2
flac_sha256sum = 91cfc3ed61dc40f47f050a109b08610667d73477af6ef36dcad31c31a4a8d53f

opus_version = opus-1.3
opus_sha256sum = 4f3d69aefdf2dbaf9825408e452a8a414ffc60494c70633560700398820dc550

#-------------------------------------------------------------------------------
# Code follows.

ogg_tarball = $(ogg_version).tar.xz
vorbis_tarball = $(vorbis_version).tar.xz
flac_tarball = $(flac_version).tar.xz
opus_tarball = $(opus_version).tar.gz

download_url = http://downloads.xiph.org/releases/
tarball_dir = Build/Tarballs
stamp_dir = Build/Stamp

build_dir = $(shell pwd)/Build
config_options = --prefix=$(build_dir) --disable-shared --enable-option-checking

pwd = $(shell pwd)

help :
	@echo
	@echo "This script will build libsndfile as a dynamic/shared library but statically linked"
	@echo "to libFLAC, libogg and libvorbis. It should work on Linux and Mac OS X. It might"
	@echo "work on Windows with a correctly set up MinGW."
	@echo
	@echo "It requires all the normal build tools require to build libsndfile plus wget."
	@echo

config : Build/Stamp/configure

build : Build/Stamp/build

clean :
	rm -rf Build/flac-* Build/libogg-* Build/libvorbis-* Build/opus-*
	rm -rf Build/bin Build/include Build/lib Build/share
	rm -f Build/Stamp/install Build/Stamp/extract Build/Stamp/sha256sum Build/Stamp/build-ogg

Build/Stamp/init :
	mkdir -p $(stamp_dir) $(tarball_dir)
	touch $@

Build/Tarballs/$(flac_tarball) : Build/Stamp/init
	(cd $(tarball_dir) && wget $(download_url)flac/$(flac_tarball) -O $(flac_tarball))
	touch $@

Build/Tarballs/$(ogg_tarball) : Build/Stamp/init
	(cd $(tarball_dir) && wget $(download_url)ogg/$(ogg_tarball) -O $(ogg_tarball))
	touch $@

Build/Tarballs/$(vorbis_tarball) : Build/Stamp/init
	(cd $(tarball_dir) && wget $(download_url)vorbis/$(vorbis_tarball) -O $(vorbis_tarball))
	touch $@

Build/Tarballs/$(opus_tarball) : Build/Stamp/init
	(cd $(tarball_dir) && wget https://archive.mozilla.org/pub/opus/$(opus_tarball) -O $(opus_tarball))
	touch $@

Build/Stamp/tarballs : Build/Tarballs/$(flac_tarball) Build/Tarballs/$(ogg_tarball) Build/Tarballs/$(vorbis_tarball) Build/Tarballs/$(opus_tarball)
	touch $@

Build/Stamp/sha256sum : Build/Stamp/tarballs
	test `sha256sum $(tarball_dir)/$(ogg_tarball) | sed "s/ .*//"` = $(ogg_sha256sum)
	test `sha256sum $(tarball_dir)/$(vorbis_tarball) | sed "s/ .*//"` = $(vorbis_sha256sum)
	test `sha256sum $(tarball_dir)/$(flac_tarball) | sed "s/ .*//"` = $(flac_sha256sum)
	test `sha256sum $(tarball_dir)/$(opus_tarball) | sed "s/ .*//"` = $(opus_sha256sum)
	touch $@

Build/Stamp/extract : Build/Stamp/sha256sum
	# (cd Build && tar xf Tarballs/$(ogg_tarball))
	(cd Build && tar xf Tarballs/$(flac_tarball))
	(cd Build && tar xf Tarballs/$(vorbis_tarball))
	(cd Build && tar xf Tarballs/$(opus_tarball))
	touch $@

Build/Stamp/build-ogg : Build/Stamp/sha256sum
ifeq ($(disable_ogg_crc), true)
	echo "Ogg/CRC enabled"
	(cd Build && git clone https://github.com/xiph/ogg $(ogg_version))
	(cd Build/$(ogg_version) && ./autogen.sh && CFLAGS=-fPIC ./configure $(config_options) --disable-crc && make all install)
else
	echo "Ogg/CRC disabled"
	(cd Build && tar xf Tarballs/$(ogg_tarball))
	(cd Build/$(ogg_version) && CFLAGS=-fPIC ./configure $(config_options) && make all install)
endif
	touch $@

Build/Stamp/install-libs : Build/Stamp/extract Build/Stamp/build-ogg
	(cd Build/$(vorbis_version) && CFLAGS=-fPIC ./configure $(config_options) && make all install)
	(cd Build/$(flac_version) && CFLAGS=-fPIC ./configure $(config_options) && make all install)
	(cd Build/$(opus_version) && CFLAGS=-fPIC ./configure $(config_options) && make all install)
	touch $@

configure : configure.ac
	./autogen.sh

Build/Stamp/configure : Build/Stamp/install-libs configure
	PKG_CONFIG_LIBDIR=Build/lib/pkgconfig ./configure
	sed -i 's#^EXTERNAL_XIPH_CFLAGS.*#EXTERNAL_XIPH_CFLAGS = -I$(pwd)/Build/include#' Makefile
	sed -i 's#^EXTERNAL_XIPH_LIBS.*#EXTERNAL_XIPH_LIBS = -static $(pwd)/Build/lib/libFLAC.la $(pwd)/Build/lib/libvorbis.la $(pwd)/Build/lib/libvorbisenc.la $(pwd)/Build/lib/libopus.la $(pwd)/Build/lib/libogg.la -dynamic #' Makefile
	make clean
	touch $@

Build/Stamp/build : Build/Stamp/configure
	make all check
	touch $@

