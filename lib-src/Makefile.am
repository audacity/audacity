EXTRA_DIST = \
	ffmpeg/libavcodec/avcodec.h \
	ffmpeg/libavcodec/avfft.h \
	ffmpeg/libavcodec/dxva2.h \
	ffmpeg/libavcodec/old_codec_ids.h \
	ffmpeg/libavcodec/vaapi.h \
	ffmpeg/libavcodec/vda.h \
	ffmpeg/libavcodec/vdpau.h \
	ffmpeg/libavcodec/version.h \
	ffmpeg/libavcodec/xvmc.h \
	ffmpeg/libavformat/avformat.h \
	ffmpeg/libavformat/avio.h \
	ffmpeg/libavformat/version.h \
	ffmpeg/libavutil/adler32.h \
	ffmpeg/libavutil/aes.h \
	ffmpeg/libavutil/attributes.h \
	ffmpeg/libavutil/audioconvert.h \
	ffmpeg/libavutil/audio_fifo.h \
	ffmpeg/libavutil/avassert.h \
	ffmpeg/libavutil/avconfig.h \
	ffmpeg/libavutil/avstring.h \
	ffmpeg/libavutil/avutil.h \
	ffmpeg/libavutil/base64.h \
	ffmpeg/libavutil/blowfish.h \
	ffmpeg/libavutil/bprint.h \
	ffmpeg/libavutil/bswap.h \
	ffmpeg/libavutil/buffer.h \
	ffmpeg/libavutil/channel_layout.h \
	ffmpeg/libavutil/common.h \
	ffmpeg/libavutil/cpu.h \
	ffmpeg/libavutil/crc.h \
	ffmpeg/libavutil/dict.h \
	ffmpeg/libavutil/downmix_info.h \
	ffmpeg/libavutil/error.h \
	ffmpeg/libavutil/eval.h \
	ffmpeg/libavutil/ffversion.h \
	ffmpeg/libavutil/fifo.h \
	ffmpeg/libavutil/file.h \
	ffmpeg/libavutil/frame.h \
	ffmpeg/libavutil/hmac.h \
	ffmpeg/libavutil/imgutils.h \
	ffmpeg/libavutil/intfloat.h \
	ffmpeg/libavutil/intfloat_readwrite.h \
	ffmpeg/libavutil/intreadwrite.h \
	ffmpeg/libavutil/lfg.h \
	ffmpeg/libavutil/log.h \
	ffmpeg/libavutil/lzo.h \
	ffmpeg/libavutil/macros.h \
	ffmpeg/libavutil/mathematics.h \
	ffmpeg/libavutil/md5.h \
	ffmpeg/libavutil/mem.h \
	ffmpeg/libavutil/murmur3.h \
	ffmpeg/libavutil/old_pix_fmts.h \
	ffmpeg/libavutil/opencl.h \
	ffmpeg/libavutil/opt.h \
	ffmpeg/libavutil/parseutils.h \
	ffmpeg/libavutil/pixdesc.h \
	ffmpeg/libavutil/pixfmt.h \
	ffmpeg/libavutil/random_seed.h \
	ffmpeg/libavutil/rational.h \
	ffmpeg/libavutil/ripemd.h \
	ffmpeg/libavutil/samplefmt.h \
	ffmpeg/libavutil/sha512.h \
	ffmpeg/libavutil/sha.h \
	ffmpeg/libavutil/stereo3d.h \
	ffmpeg/libavutil/timecode.h \
	ffmpeg/libavutil/time.h \
	ffmpeg/libavutil/timestamp.h \
	ffmpeg/libavutil/version.h \
	ffmpeg/libavutil/xtea.h \
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

# Distribute library schedule
EXTRA_DIST += audacity-patches.txt \
	$(NULL)

DIST_SUBDIRS = \
	FileDialog \
	libnyquist \
	libsoxr \
	lib-widget-extra \
	lv2 \
	portmixer \
	portsmf \
	sbsms \
	$(NULL)

include dist-libsoxr.mk
include dist-libvamp.mk
include dist-portaudio.mk

# TODO: Check and distribute these directories:
# libscorealign, mod-null, mod-nyq-bench, mod-script-pipe, mod-track-panel, portmidi

SUBDIRS = FileDialog
OPTIONAL_SUBDIRS =

if USE_LOCAL_EXPAT
OPTIONAL_SUBDIRS += expat
endif

if USE_LOCAL_LIBID3TAG
OPTIONAL_SUBDIRS += libid3tag
endif

if USE_LOCAL_LIBMAD
OPTIONAL_SUBDIRS += libmad
endif

if USE_LOCAL_LIBNYQUIST
SUBDIRS += libnyquist
endif

if USE_LOCAL_LIBSNDFILE
OPTIONAL_SUBDIRS += libsndfile
endif

if USE_LOCAL_LIBSOXR
OPTIONAL_SUBDIRS += libsoxr
endif

if USE_LOCAL_LIBSOUNDTOUCH
OPTIONAL_SUBDIRS += soundtouch
endif

if USE_LOCAL_LIBTWOLAME
OPTIONAL_SUBDIRS += twolame
endif

if USE_LOCAL_LIBVORBIS
OPTIONAL_SUBDIRS += libogg libvorbis
endif

if USE_LOCAL_LIBFLAC
# Note: libflac needs to be linked against (the local or system) libogg.
OPTIONAL_SUBDIRS += libflac
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

if USE_LOCAL_PORTMIDI
SUBDIRS += portmidi
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

# Note: These optional libraries will not be distributed in the source tarball.
SUBDIRS += $(OPTIONAL_SUBDIRS)

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
