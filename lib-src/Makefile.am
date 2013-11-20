EXTRA_DIST = \
	ffmpeg/libavcodec/avcodec.h \
	ffmpeg/libavformat/avformat.h \
	ffmpeg/libavformat/avio.h \
	ffmpeg/libavutil/attributes.h \
	ffmpeg/libavutil/avconfig.h \
	ffmpeg/libavutil/avutil.h \
	ffmpeg/libavutil/common.h \
	ffmpeg/libavutil/error.h \
	ffmpeg/libavutil/fifo.h \
	ffmpeg/libavutil/intfloat_readwrite.h \
	ffmpeg/libavutil/log.h \
	ffmpeg/libavutil/mathematics.h \
	ffmpeg/libavutil/mem.h \
	ffmpeg/libavutil/pixfmt.h \
	ffmpeg/libavutil/rational.h \
	ffmpeg/win32/inttypes.h \
	ffmpeg/win32/stdint.h \
	lame/lame/lame.h \
	$(NULL)

# Distribute patches
EXTRA_DIST += \
	portsmf/autotools.patch \
	portsmf/autotools-fix-make-dist.patch \
	sbsms/autotools.patch \
	sbsms/autotools-fix-make-dist.patch \
	sbsms/dont-mangle-cflags.patch \
	$(NULL)

DIST_SUBDIRS = \
	FileDialog \
	libnyquist \
	lib-widget-extra \
	lv2 \
	portmixer \
	portsmf \
	sbsms \
	$(NULL)

include dist-libsoxr.mk
include dist-libvamp.mk
include dist-portaudio.mk

# TODO: Distribute these libraries that do not use Automake:
# libresample
# TODO: Check and distribute these directories:
# libscorealign, mod-null, mod-nyq-bench, mod-script-pipe, mod-track-panel, portmidi

SUBDIRS = FileDialog

if USE_LOCAL_EXPAT
# Note: This library will not be distributed in the source tarball.
SUBDIRS += expat
endif

if USE_LOCAL_LIBID3TAG
# Note: This library will not be distributed in the source tarball.
SUBDIRS += libid3tag
endif

if USE_LOCAL_LIBMAD
# Note: This library will not be distributed in the source tarball.
SUBDIRS += libmad
endif

if USE_LOCAL_LIBNYQUIST
SUBDIRS += libnyquist
endif

if USE_LOCAL_LIBRESAMPLE
SUBDIRS += libresample
endif

if USE_LOCAL_LIBSAMPLERATE
# Note: This library will not be distributed in the source tarball.
SUBDIRS += libsamplerate
endif

if USE_LOCAL_LIBSNDFILE
# Note: This library will not be distributed in the source tarball.
SUBDIRS += libsndfile
endif

if USE_LOCAL_LIBSOUNDTOUCH
# Note: This library will not be distributed in the source tarball.
SUBDIRS += soundtouch
endif

if USE_LOCAL_LIBSOXR
SUBDIRS += libsoxr
endif

if USE_LOCAL_LIBTWOLAME
# Note: This library will not be distributed in the source tarball.
SUBDIRS += twolame
endif

if USE_LOCAL_LIBVORBIS
# Note: These libraries will not be distributed in the source tarball.
SUBDIRS += libogg libvorbis
endif

if USE_LOCAL_LIBFLAC
# Note: libflac needs to be linked against (the local or system) libogg.
# Note: This library will not be distributed in the source tarball.
SUBDIRS += libflac
endif

if USE_LOCAL_LV2
SUBDIRS += lv2
endif

if USE_LOCAL_PORTAUDIO
SUBDIRS += portaudio-v19
endif

# Note: portmixer needs to be linked against (the local or system) portaudio.
SUBDIRS += portmixer

if USE_LOCAL_PORTSMF
SUBDIRS += portsmf
endif

if USE_LOCAL_SBSMS
SUBDIRS += sbsms
endif

if USE_LOCAL_VAMP
SUBDIRS += libvamp
endif

if USE_LOCAL_WIDGETEXTRA
SUBDIRS += lib-widget-extra
endif

# Overwrite install* targets from automake. We do not want to install anything
# from the lib-src libraries, because we statically link these libraries.
install:
install-dvi:
install-data:
install-exec:
install-html:
install-info:
install-ps:
install-pdf:
installdirs:
uninstall:

# Overwrite check target. We do not want to run the tests from the libraries.
check:

# Overwrite distclean rule to avoid running distclean for unconfigured libraries
# (and then failing due to the missing Makefile).
distclean:
	@for subdir in $(SUBDIRS); do \
	   if test -f $$subdir/Makefile; then \
	      echo "Making $@ in $$subdir"; \
	      $(MAKE) $(AM_MAKEFLAGS) -C $$subdir $@ || exit 1; \
	   fi; \
	done
	rm -f Makefile

# Overwrite maintainer-clean rule to avoid running maintainer-clean for
# unconfigured libraries (and then failing due to the missing Makefile).
maintainer-clean:
	@for subdir in $(SUBDIRS); do \
	   if test -f $$subdir/Makefile; then \
	      echo "Making $@ in $$subdir"; \
	      $(MAKE) $(AM_MAKEFLAGS) -C $$subdir $@ || exit 1; \
	   fi; \
	done
	rm -f Makefile

.PHONY: check install install-dvi install-data install-exec install-html install-info install-ps install-pdf installdirs uninstall distclean maintainer-clean
