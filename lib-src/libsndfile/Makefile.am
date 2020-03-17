## Process this file with automake to produce Makefile.in

AM_CPPFLAGS = -I$(top_srcdir)/src $(OS_SPECIFIC_CFLAGS)

DISTCHECK_CONFIGURE_FLAGS = --enable-werror

CLEANFILES = programs/*.wav octave-workspace

if FULL_SUITE
if BUILD_OCTAVE_MOD
SUBDIRS = Octave
endif
endif

EXTRA_DIST = libsndfile.spec.in sndfile.pc.in Scripts/android-configure.sh \
	Scripts/linux-to-win-cross-configure.sh \
	CMakeLists.txt $(cmake_files) Win32

cmake_files = cmake/ClipMode.cmake cmake/FindFLAC.cmake \
	cmake/CMakeAutoGen.cmake cmake/CMakeAutoGenScript.cmake \
	cmake/FindOgg.cmake cmake/FindVorbis.cmake cmake/FindSndio.cmake \
	cmake/FindSpeex.cmake cmake/FindSQLite3.cmake cmake/FindVorbisEnc.cmake \
	cmake/SndFileChecks.cmake cmake/TestInline.cmake \
	cmake/TestLargeFiles.cmake cmake/TestInline.c.in \
	cmake/FindOpus.cmake cmake/SndFileConfig.cmake.in

pkgconfig_DATA = sndfile.pc

#===============================================================================

test: check

# Need this target to force building of test programs.
checkprograms: $(check_PROGRAMS)

########
# src/ #
########

#BUILT_SOURCES = src/test_endswap.c

#SYMBOL_FILES = src/Symbols.gnu-binutils src/Symbols.darwin src/libsndfile-1.def src/Symbols.os2 src/Symbols.static

EXTRA_DIST += src/sndfile.h.in src/config.h.in src/test_endswap.tpl src/test_endswap.def \
	$(SYMBOL_FILES) src/create_symbols_file.py src/binheader_writef_check.py \
	src/GSM610/README src/GSM610/COPYRIGHT src/GSM610/ChangeLog \
	src/G72x/README src/G72x/README.original src/G72x/ChangeLog \
	src/make-static-lib-hidden-privates.sh \
	src/config.h.cmake

if USE_WIN_VERSION_FILE
WIN_VERSION_FILE = src/version-metadata.rc
else
WIN_VERSION_FILE =
endif

#===============================================================================

lib_LTLIBRARIES = src/libsndfile.la
include_HEADERS = src/sndfile.hh
nodist_include_HEADERS = src/sndfile.h
src_libsndfile_la_CFLAGS = $(EXTERNAL_XIPH_CFLAGS)
# MinGW requires -no-undefined if a DLL is to be built.
src_libsndfile_la_LDFLAGS = -no-undefined -version-info $(SHARED_VERSION_INFO) $(SHLIB_VERSION_ARG)
src_libsndfile_la_SOURCES = src/sndfile.c src/aiff.c src/au.c src/avr.c src/caf.c src/dwd.c src/flac.c src/g72x.c src/htk.c src/ircam.c \
	src/macos.c src/mat4.c src/mat5.c src/nist.c src/paf.c src/pvf.c src/raw.c src/rx2.c src/sd2.c \
	src/sds.c src/svx.c src/txw.c src/voc.c src/wve.c src/w64.c src/wavlike.c src/wav.c src/xi.c src/mpc2k.c src/rf64.c \
	src/ogg_vorbis.c src/ogg_speex.c src/ogg_pcm.c src/ogg_opus.c src/ogg_vcomment.c \
	src/common.h src/sfconfig.h src/sfendian.h src/wavlike.h src/sf_unistd.h src/ogg.h src/chanmap.h src/ogg_vcomment.h
nodist_src_libsndfile_la_SOURCES = $(nodist_include_HEADERS)
src_libsndfile_la_LIBADD = src/GSM610/libgsm.la src/G72x/libg72x.la src/ALAC/libalac.la \
	src/libcommon.la $(EXTERNAL_XIPH_LIBS) -lm
EXTRA_src_libsndfile_la_DEPENDENCIES = $(SYMBOL_FILES)

noinst_LTLIBRARIES = src/libcommon.la
src_libcommon_la_CFLAGS = $(EXTERNAL_XIPH_CFLAGS)
src_libcommon_la_SOURCES = src/common.c src/file_io.c src/command.c src/pcm.c src/ulaw.c src/alaw.c \
	src/float32.c src/double64.c src/ima_adpcm.c src/ms_adpcm.c src/gsm610.c src/dwvw.c src/vox_adpcm.c \
	src/interleave.c src/strings.c src/dither.c src/cart.c src/broadcast.c src/audio_detect.c \
	src/ima_oki_adpcm.c src/ima_oki_adpcm.h src/alac.c src/chunk.c src/ogg.c src/chanmap.c \
	src/windows.c src/id3.c src/nms_adpcm.c $(WIN_VERSION_FILE)

check_PROGRAMS = src/test_main
src_test_main_SOURCES = src/test_main.c src/test_main.h src/test_conversions.c src/test_float.c src/test_endswap.c \
	src/test_audio_detect.c src/test_log_printf.c src/test_file_io.c src/test_ima_oki_adpcm.c \
	src/test_strncpy_crlf.c src/test_broadcast_var.c src/test_cart_var.c \
	src/test_binheader_writef.c src/test_nms_adpcm.c
src_test_main_LDADD = src/libcommon.la

##############
# src/GSM610 #
##############

noinst_LTLIBRARIES += src/GSM610/libgsm.la
src_GSM610_libgsm_la_SOURCES = src/GSM610/config.h src/GSM610/gsm.h src/GSM610/gsm610_priv.h \
	src/GSM610/add.c src/GSM610/code.c src/GSM610/decode.c src/GSM610/gsm_create.c \
	src/GSM610/gsm_decode.c src/GSM610/gsm_destroy.c src/GSM610/gsm_encode.c \
	src/GSM610/gsm_option.c src/GSM610/long_term.c src/GSM610/lpc.c src/GSM610/preprocess.c \
	src/GSM610/rpe.c src/GSM610/short_term.c src/GSM610/table.c

############
# src/G72x #
############

noinst_LTLIBRARIES += src/G72x/libg72x.la
src_G72x_libg72x_la_SOURCES = src/G72x/g72x.h src/G72x/g72x_priv.h \
	src/G72x/g721.c src/G72x/g723_16.c src/G72x/g723_24.c src/G72x/g723_40.c src/G72x/g72x.c

check_PROGRAMS += src/G72x/g72x_test
src_G72x_g72x_test_SOURCES = src/G72x/g72x_test.c
src_G72x_g72x_test_LDADD = src/G72x/libg72x.la

############
# src/ALAC #
############

noinst_LTLIBRARIES += src/ALAC/libalac.la
src_ALAC_libalac_la_SOURCES = src/ALAC/ALACAudioTypes.h src/ALAC/ALACBitUtilities.h \
	src/ALAC/EndianPortable.h src/ALAC/aglib.h src/ALAC/dplib.h src/ALAC/matrixlib.h \
	src/ALAC/alac_codec.h src/ALAC/shift.h \
	src/ALAC/ALACBitUtilities.c src/ALAC/ag_dec.c \
	src/ALAC/ag_enc.c src/ALAC/dp_dec.c src/ALAC/dp_enc.c src/ALAC/matrix_dec.c \
	src/ALAC/matrix_enc.c src/ALAC/alac_decoder.c src/ALAC/alac_encoder.c

#===============================================================================
# Generate an OS specific Symbols files. This is done when the author
# builds the distribution tarball. There should be not need for the
# end user to create these files.

# "$<" cannot portably be used in the recipe across Make implementations
# https://www.gnu.org/software/autoconf/manual/autoconf.html#g_t_0024_003c-in-Ordinary-Make-Rules
SYMBOL_SCRIPT = $(top_srcdir)/src/create_symbols_file.py

src/Symbols.gnu-binutils: $(SYMBOL_SCRIPT)
	$(PYTHON) $(SYMBOL_SCRIPT) linux $(VERSION) > $(top_srcdir)/$@

src/Symbols.darwin: $(SYMBOL_SCRIPT)
	$(PYTHON) $(SYMBOL_SCRIPT) darwin $(VERSION) > $(top_srcdir)/$@

src/libsndfile-1.def: $(SYMBOL_SCRIPT)
	$(PYTHON) $(SYMBOL_SCRIPT) win32 $(VERSION) > $(top_srcdir)/$@

src/Symbols.os2: $(SYMBOL_SCRIPT)
	$(PYTHON) $(SYMBOL_SCRIPT) os2 $(VERSION) > $(top_srcdir)/$@

src/Symbols.static: $(SYMBOL_SCRIPT)
	$(PYTHON) $(SYMBOL_SCRIPT) static $(VERSION) > $(top_srcdir)/$@

#===============================================================================
# Building windows resource files (if needed).

.rc.lo:
	$(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --tag=RC --mode=compile $(RC) $(RCFLAGS) $< -o $@

#===============================================================================
# Disable autoheader.
AUTOHEADER=echo

########
# doc/ #
########

if FULL_SUITE
dist_html_DATA = doc/index.html doc/libsndfile.jpg doc/libsndfile.css doc/print.css doc/api.html \
	doc/command.html doc/bugs.html doc/sndfile_info.html doc/new_file_type.HOWTO \
	doc/win32.html doc/FAQ.html doc/lists.html doc/embedded_files.html doc/octave.html \
	doc/tutorial.html
endif

#############
# examples/ #
#############

check_PROGRAMS += examples/make_sine examples/sfprocess examples/list_formats examples/generate examples/sndfilehandle \
	examples/sndfile-to-text examples/sndfile-loopify

examples_sndfile_to_text_SOURCES = examples/sndfile-to-text.c
examples_sndfile_to_text_LDADD = src/libsndfile.la

examples_sndfile_loopify_SOURCES = examples/sndfile-loopify.c
examples_sndfile_loopify_LDADD = src/libsndfile.la

examples_make_sine_SOURCES = examples/make_sine.c
examples_make_sine_LDADD = src/libsndfile.la

examples_sfprocess_SOURCES = examples/sfprocess.c
examples_sfprocess_LDADD = src/libsndfile.la

examples_list_formats_SOURCES = examples/list_formats.c
examples_list_formats_LDADD = src/libsndfile.la

examples_generate_SOURCES = examples/generate.c
examples_generate_LDADD = src/libsndfile.la

examples_sndfilehandle_SOURCES = examples/sndfilehandle.cc
examples_sndfilehandle_LDADD = src/libsndfile.la

##########
# tests/ #
##########

TESTS_ENVIRONMENT = $(SHELL) tests/test_wrapper.sh

if ENABLE_TEST_COVERAGE
CPP_TEST =
else
CPP_TEST = tests/cpp_test
endif

TESTS = tests/pedantic-header-test.sh

check_PROGRAMS += tests/sfversion tests/floating_point_test tests/write_read_test \
	tests/lossy_comp_test tests/error_test tests/ulaw_test tests/alaw_test tests/dwvw_test \
	tests/peak_chunk_test tests/command_test tests/stdio_test \
	tests/pcm_test tests/headerless_test tests/pipe_test tests/header_test tests/misc_test \
	tests/raw_test tests/string_test tests/multi_file_test tests/chunk_test \
	tests/scale_clip_test tests/win32_test tests/aiff_rw_test tests/virtual_io_test \
	tests/locale_test tests/win32_ordinal_test tests/ogg_test tests/compression_size_test \
	tests/checksum_test tests/external_libs_test tests/rdwr_test tests/format_check_test $(CPP_TEST) \
	tests/channel_test tests/long_read_write_test tests/stdin_test tests/stdout_test \
	tests/dither_test tests/fix_this tests/largefile_test tests/benchmark tests/ogg_opus_test

#BUILT_SOURCES += \
#	tests/write_read_test.c \
#	tests/pcm_test.c \
#	tests/header_test.c \
#	tests/utils.c \
#	tests/scale_clip_test.c \
#	tests/pipe_test.c \
#	tests/rdwr_test.c \
#	tests/floating_point_test.c \
#	tests/benchmark.c

EXTRA_DIST += $(BUILT_SOURCES) \
	tests/write_read_test.tpl     tests/write_read_test.def \
	tests/pcm_test.tpl            tests/pcm_test.def \
	tests/header_test.tpl         tests/header_test.def \
	tests/utils.tpl               tests/utils.def \
	tests/scale_clip_test.tpl     tests/scale_clip_test.def \
	tests/pipe_test.tpl           tests/pipe_test.def \
	tests/rdwr_test.tpl           tests/rdwr_test.def \
	tests/floating_point_test.tpl tests/floating_point_test.def \
	tests/benchmark.tpl           tests/benchmark.def

# If we're cross compiling from Linux to Windows and running the test suite
# under Wine, we need a symbolic link to the generated libsndfile DLL.

if LINUX_MINGW_CROSS_TEST

$(check_PROGRAMS) : libsndfile-1.dll

libsndfile-1.dll :
	$(LN_S) src/.libs/$@ $@

clean-local :
	-rm -f libsndfile-1.dll

endif

#===============================================================================

tests_sfversion_SOURCES = tests/sfversion.c
tests_sfversion_LDADD = src/libsndfile.la

tests_write_read_test_SOURCES = tests/utils.c tests/generate.c tests/generate.h tests/write_read_test.c tests/utils.h
tests_write_read_test_LDADD = src/libsndfile.la

tests_lossy_comp_test_SOURCES = tests/utils.c tests/lossy_comp_test.c tests/utils.h
tests_lossy_comp_test_LDADD = src/libsndfile.la

tests_fix_this_SOURCES = tests/utils.c tests/fix_this.c tests/utils.h
tests_fix_this_LDADD = src/libsndfile.la

tests_error_test_SOURCES = tests/error_test.c tests/utils.c tests/utils.h
tests_error_test_LDADD = src/libsndfile.la

tests_ulaw_test_SOURCES = tests/utils.c tests/ulaw_test.c tests/utils.h
tests_ulaw_test_LDADD = src/libsndfile.la

tests_alaw_test_SOURCES = tests/utils.c tests/alaw_test.c tests/utils.h
tests_alaw_test_LDADD = src/libsndfile.la

tests_aiff_rw_test_SOURCES = tests/utils.c tests/aiff_rw_test.c tests/utils.h
tests_aiff_rw_test_LDADD = src/libsndfile.la

tests_command_test_SOURCES = tests/command_test.c tests/utils.c tests/utils.h
tests_command_test_LDADD = src/libsndfile.la

tests_locale_test_SOURCES = tests/locale_test.c tests/utils.c tests/utils.h
tests_locale_test_LDADD = src/libsndfile.la

tests_largefile_test_SOURCES = tests/largefile_test.c tests/utils.c tests/utils.h
tests_largefile_test_LDADD = src/libsndfile.la

tests_pcm_test_SOURCES = tests/pcm_test.c tests/utils.c tests/utils.h
tests_pcm_test_LDADD = src/libsndfile.la

tests_headerless_test_SOURCES = tests/utils.c tests/headerless_test.c tests/utils.h
tests_headerless_test_LDADD = src/libsndfile.la

tests_stdin_test_SOURCES = tests/stdin_test.c tests/utils.c tests/utils.h
tests_stdin_test_LDADD = src/libsndfile.la

tests_stdout_test_SOURCES = tests/stdout_test.c tests/utils.h
tests_stdout_test_LDADD = src/libsndfile.la

tests_stdio_test_SOURCES = tests/stdio_test.c tests/utils.c tests/utils.h
tests_stdio_test_LDADD = src/libsndfile.la

tests_pipe_test_SOURCES = tests/pipe_test.c tests/utils.c tests/utils.h
tests_pipe_test_LDADD = src/libsndfile.la

tests_benchmark_SOURCES = tests/benchmark.c tests/utils.h
tests_benchmark_LDADD = src/libsndfile.la

tests_header_test_SOURCES = tests/header_test.c tests/utils.c tests/utils.h
tests_header_test_LDADD = src/libsndfile.la

tests_misc_test_SOURCES = tests/misc_test.c tests/utils.c tests/utils.h
tests_misc_test_LDADD = src/libsndfile.la

tests_raw_test_SOURCES = tests/raw_test.c tests/utils.c tests/utils.h
tests_raw_test_LDADD = src/libsndfile.la

tests_string_test_SOURCES = tests/string_test.c tests/utils.c tests/utils.h
tests_string_test_LDADD = src/libsndfile.la

tests_dither_test_SOURCES = tests/dither_test.c tests/utils.c tests/utils.h
tests_dither_test_LDADD = src/libsndfile.la

tests_chunk_test_SOURCES = tests/chunk_test.c tests/utils.c tests/utils.h
tests_chunk_test_LDADD = src/libsndfile.la

tests_multi_file_test_SOURCES = tests/multi_file_test.c tests/utils.c tests/utils.h
tests_multi_file_test_LDADD = src/libsndfile.la

tests_virtual_io_test_SOURCES = tests/virtual_io_test.c tests/utils.c tests/utils.h
tests_virtual_io_test_LDADD = src/libsndfile.la

tests_ogg_test_SOURCES = tests/ogg_test.c tests/utils.c tests/utils.h
tests_ogg_test_LDADD = src/libsndfile.la

tests_ogg_opus_test_SOURCES = tests/ogg_opus_test.c tests/utils.c tests/utils.h
tests_ogg_opus_test_LDADD = src/libsndfile.la

tests_compression_size_test_SOURCES = tests/compression_size_test.c tests/utils.c tests/utils.h tests/dft_cmp.h
tests_compression_size_test_LDADD = src/libsndfile.la

tests_rdwr_test_SOURCES = tests/rdwr_test.c tests/utils.c tests/utils.h
tests_rdwr_test_LDADD = src/libsndfile.la

tests_win32_test_SOURCES = tests/win32_test.c
# Link lib here so that generating the testsuite tarball works correctly.
tests_win32_test_LDADD = src/libsndfile.la

tests_win32_ordinal_test_SOURCES = tests/win32_ordinal_test.c tests/utils.c tests/utils.h
tests_win32_ordinal_test_LDADD = src/libsndfile.la

tests_external_libs_test_SOURCES = tests/external_libs_test.c tests/utils.c tests/utils.h
tests_external_libs_test_LDADD = src/libsndfile.la

tests_format_check_test_SOURCES = tests/format_check_test.c tests/utils.c tests/utils.h
tests_format_check_test_LDADD = src/libsndfile.la

tests_channel_test_SOURCES = tests/channel_test.c tests/utils.c tests/utils.h
tests_channel_test_LDADD = src/libsndfile.la

tests_long_read_write_test_SOURCES = tests/long_read_write_test.c tests/utils.c tests/utils.h tests/dft_cmp.h
tests_long_read_write_test_LDADD = src/libsndfile.la

tests_cpp_test_SOURCES = tests/cpp_test.cc tests/utils.c tests/utils.h
tests_cpp_test_LDADD = src/libsndfile.la

tests_checksum_test_SOURCES = tests/checksum_test.c tests/utils.c tests/utils.h
tests_checksum_test_LDADD = src/libsndfile.la

# Lite remove start
tests_dwvw_test_SOURCES = tests/dwvw_test.c tests/utils.c tests/utils.h
tests_dwvw_test_LDADD = src/libsndfile.la

tests_floating_point_test_SOURCES = tests/utils.c tests/utils.h tests/dft_cmp.c tests/dft_cmp.h tests/floating_point_test.c
tests_floating_point_test_LDADD = src/libsndfile.la

tests_peak_chunk_test_SOURCES = tests/peak_chunk_test.c tests/utils.c tests/utils.h
tests_peak_chunk_test_LDADD = src/libsndfile.la

tests_scale_clip_test_SOURCES = tests/scale_clip_test.c tests/utils.c tests/utils.h
tests_scale_clip_test_LDADD = src/libsndfile.la
# Lite remove end

#===============================================================================
# Autogen generated sources.
# Coerce the multiple inputs -> multiple outputs problem
# into suffix rules by "linearising" the dependency graph.
# Yes, this sucks, but GNU make patterns aren't portable,
# see also https://github.com/erikd/libsndfile/issues/369

SUFFIXES = .tpl .def

.tpl.def:
	touch $@

# unconditionally running touch on the .def
# files is necessary to keep all timestamps
# consistent, in order to prevent stale files
# from calling autogen in tarball releases.
.def.c:
	$(MAKE) $(AM_MAKEFLAGS) $<
	cd $(top_srcdir)/$(@D) && autogen --writable $(<F)

# recommended Automake way for multi-output targets:
# https://www.gnu.org/software/automake/manual/html_node/Multiple-Outputs.html
tests/utils.h : tests/utils.c
	@if test -f $@; then :; else \
	  cd $(top_srcdir) && \
	  rm -f tests/utils.c && \
	  $(MAKE) $(AM_MAKEFLAGS) tests/utils.c; \
	fi

########
# man/ #
########

if FULL_SUITE
dist_man_MANS = man/sndfile-info.1 man/sndfile-play.1 man/sndfile-convert.1 man/sndfile-cmp.1 \
	man/sndfile-metadata-get.1 man/sndfile-metadata-set.1 man/sndfile-concat.1 \
	man/sndfile-interleave.1 man/sndfile-deinterleave.1 man/sndfile-salvage.1

# Same manpage for both programs.
man/sndfile-metadata-set.1: man/sndfile-metadata-get.1
	-rm -f $@
	cd $(top_srcdir)/man && $(LN_S) sndfile-metadata-get.1 sndfile-metadata-set.1

man/sndfile-deinterleave.1: man/sndfile-interleave.1
	-rm -f $@
	cd $(top_srcdir)/man && $(LN_S) sndfile-interleave.1 sndfile-deinterleave.1

#############
# programs/ #
#############

bin_PROGRAMS = programs/sndfile-info programs/sndfile-play programs/sndfile-convert programs/sndfile-cmp \
	programs/sndfile-metadata-set programs/sndfile-metadata-get programs/sndfile-interleave \
	programs/sndfile-deinterleave programs/sndfile-concat programs/sndfile-salvage
endif

# required by test-sndfile-metadata-set.py
check_PROGRAMS += programs/sndfile-metadata-set programs/sndfile-metadata-get

EXTRA_DIST += programs/test-sndfile-metadata-set.py

programs_sndfile_info_SOURCES = programs/sndfile-info.c programs/common.c programs/common.h
programs_sndfile_info_LDADD = src/libsndfile.la

programs_sndfile_play_SOURCES = programs/sndfile-play.c programs/common.c programs/common.h
programs_sndfile_play_LDADD = src/libsndfile.la $(OS_SPECIFIC_LINKS) $(ALSA_LIBS) $(SNDIO_LIBS)

programs_sndfile_convert_SOURCES = programs/sndfile-convert.c programs/common.c programs/common.h
programs_sndfile_convert_LDADD = src/libsndfile.la

programs_sndfile_cmp_SOURCES = programs/sndfile-cmp.c programs/common.c programs/common.h
programs_sndfile_cmp_LDADD = src/libsndfile.la

programs_sndfile_metadata_set_SOURCES = programs/sndfile-metadata-set.c programs/common.c programs/common.h
programs_sndfile_metadata_set_LDADD = src/libsndfile.la

programs_sndfile_metadata_get_SOURCES = programs/sndfile-metadata-get.c programs/common.c programs/common.h
programs_sndfile_metadata_get_LDADD = src/libsndfile.la

programs_sndfile_interleave_SOURCES = programs/sndfile-interleave.c programs/common.c programs/common.h
programs_sndfile_interleave_LDADD = src/libsndfile.la

programs_sndfile_deinterleave_SOURCES = programs/sndfile-deinterleave.c programs/common.c programs/common.h
programs_sndfile_deinterleave_LDADD = src/libsndfile.la

programs_sndfile_concat_SOURCES = programs/sndfile-concat.c programs/common.c programs/common.h
programs_sndfile_concat_LDADD = src/libsndfile.la

programs_sndfile_salvage_SOURCES = programs/sndfile-salvage.c programs/common.c programs/common.h
programs_sndfile_salvage_LDADD = src/libsndfile.la

############
# regtest/ #
############

if HAVE_SQLITE3
check_PROGRAMS += regtest/sndfile-regtest
endif

regtest_sndfile_regtest_SOURCES = regtest/regtest.h regtest/sndfile-regtest.c regtest/database.c regtest/checksum.c
regtest_sndfile_CPPFLAGS = -I$(top_srcdir)/src $(SQLITE3_CFLAGS) $(OS_SPECIFIC_CFLAGS)
regtest_sndfile_regtest_LDADD = src/libsndfile.la $(SQLITE3_LIBS)

################
# test-tarball #
################
test-tarball : Scripts/build-test-tarball.mk
	$(MAKE) $(bin_PROGRAMS) $(check_PROGRAMS)
	$(MAKE) -f Scripts/build-test-tarball.mk
