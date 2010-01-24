## Process this file with automake to produce Makefile.in

if ENABLE_TEST_COVERAGE
CPP_TEST =
else
CPP_TEST = cpp_test
endif

INCLUDES = -I$(top_srcdir)/src

noinst_PROGRAMS = sfversion floating_point_test write_read_test \
	lossy_comp_test error_test ulaw_test alaw_test dwvw_test \
	peak_chunk_test command_test stdin_test stdout_test stdio_test \
	pcm_test headerless_test pipe_test benchmark header_test misc_test \
	raw_test string_test multi_file_test dither_test \
	scale_clip_test win32_test fix_this aiff_rw_test virtual_io_test \
	locale_test largefile_test win32_ordinal_test ogg_test vorbis_test \
	checksum_test external_libs_test $(CPP_TEST)

noinst_HEADERS = dft_cmp.h utils.h generate.h

autogen_sources = write_read_test.tpl write_read_test.def	\
					pcm_test.tpl pcm_test.def				\
					header_test.tpl header_test.def			\
					utils.tpl utils.def						\
					scale_clip_test.tpl scale_clip_test.def	\
					pipe_test.tpl pipe_test.def				\
					floating_point_test.tpl floating_point_test.def	\
					benchmark.tpl benchmark.def

EXTRA_DIST = $(autogen_sources)

#===============================================================================

sfversion_SOURCES = sfversion.c
sfversion_LDADD = $(top_builddir)/src/libsndfile.la

write_read_test_SOURCES = utils.c generate.c write_read_test.c
write_read_test_LDADD = $(top_builddir)/src/libsndfile.la

lossy_comp_test_SOURCES = utils.c lossy_comp_test.c
lossy_comp_test_LDADD = $(top_builddir)/src/libsndfile.la

fix_this_SOURCES = utils.c fix_this.c
fix_this_LDADD = $(top_builddir)/src/libsndfile.la

error_test_SOURCES = error_test.c utils.c
error_test_LDADD = $(top_builddir)/src/libsndfile.la

ulaw_test_SOURCES = utils.c ulaw_test.c
ulaw_test_LDADD = $(top_builddir)/src/libsndfile.la

alaw_test_SOURCES = utils.c alaw_test.c
alaw_test_LDADD = $(top_builddir)/src/libsndfile.la

aiff_rw_test_SOURCES = utils.c aiff_rw_test.c
aiff_rw_test_LDADD = $(top_builddir)/src/libsndfile.la

command_test_SOURCES = command_test.c utils.c
command_test_LDADD = $(top_builddir)/src/libsndfile.la

locale_test_SOURCES = locale_test.c utils.c
locale_test_LDADD = $(top_builddir)/src/libsndfile.la

largefile_test_SOURCES = largefile_test.c utils.c
largefile_test_LDADD = $(top_builddir)/src/libsndfile.la

pcm_test_SOURCES = pcm_test.c utils.c
pcm_test_LDADD = $(top_builddir)/src/libsndfile.la

headerless_test_SOURCES = utils.c headerless_test.c
headerless_test_LDADD = $(top_builddir)/src/libsndfile.la

stdin_test_SOURCES = stdin_test.c utils.c
stdin_test_LDADD = $(top_builddir)/src/libsndfile.la

stdout_test_SOURCES = stdout_test.c
stdout_test_LDADD = $(top_builddir)/src/libsndfile.la

stdio_test_SOURCES = stdio_test.c utils.c
stdio_test_LDADD = $(top_builddir)/src/libsndfile.la

pipe_test_SOURCES = pipe_test.c utils.c
pipe_test_LDADD = $(top_builddir)/src/libsndfile.la

benchmark_SOURCES = benchmark.c
benchmark_LDADD = $(top_builddir)/src/libsndfile.la

header_test_SOURCES = header_test.c utils.c
header_test_LDADD = $(top_builddir)/src/libsndfile.la

misc_test_SOURCES = misc_test.c utils.c
misc_test_LDADD = $(top_builddir)/src/libsndfile.la

raw_test_SOURCES = raw_test.c utils.c
raw_test_LDADD = $(top_builddir)/src/libsndfile.la

string_test_SOURCES = string_test.c utils.c
string_test_LDADD = $(top_builddir)/src/libsndfile.la

dither_test_SOURCES = dither_test.c utils.c
dither_test_LDADD = $(top_builddir)/src/libsndfile.la

multi_file_test_SOURCES = multi_file_test.c utils.c
multi_file_test_LDADD = $(top_builddir)/src/libsndfile.la

virtual_io_test_SOURCES = virtual_io_test.c utils.c
virtual_io_test_LDADD = $(top_builddir)/src/libsndfile.la

ogg_test_SOURCES = ogg_test.c utils.c
ogg_test_LDADD = $(top_builddir)/src/libsndfile.la

vorbis_test_SOURCES = vorbis_test.c utils.c
vorbis_test_LDADD = $(top_builddir)/src/libsndfile.la

win32_test_SOURCES = win32_test.c
win32_test_LDADD =

win32_ordinal_test_SOURCES = win32_ordinal_test.c utils.c
win32_ordinal_test_LDADD = $(top_builddir)/src/libsndfile.la

external_libs_test_SOURCES = external_libs_test.c utils.c
external_libs_test_LDADD = $(top_builddir)/src/libsndfile.la

cpp_test_SOURCES = cpp_test.cc utils.c
cpp_test_LDADD = $(top_builddir)/src/libsndfile.la

checksum_test_SOURCES = checksum_test.c utils.c
checksum_test_LDADD = $(top_builddir)/src/libsndfile.la

# Lite remove start
dwvw_test_SOURCES = utils.c dwvw_test.c
dwvw_test_LDADD = $(top_builddir)/src/libsndfile.la

floating_point_test_SOURCES = utils.c dft_cmp.c floating_point_test.c
floating_point_test_LDADD = $(top_builddir)/src/libsndfile.la

peak_chunk_test_SOURCES = peak_chunk_test.c utils.c
peak_chunk_test_LDADD = $(top_builddir)/src/libsndfile.la

scale_clip_test_SOURCES = scale_clip_test.c utils.c
scale_clip_test_LDADD = $(top_builddir)/src/libsndfile.la
# Lite remove end

#===============================================================================

write_read_test.c: write_read_test.def write_read_test.tpl
	autogen --writable write_read_test.def

pcm_test.c: pcm_test.def pcm_test.tpl
	autogen --writable pcm_test.def

header_test.c: header_test.def header_test.tpl
	autogen --writable header_test.def

utils.c utils.h : utils.def utils.tpl
	autogen --writable utils.def

scale_clip_test.c: scale_clip_test.def scale_clip_test.tpl
	autogen --writable scale_clip_test.def

pipe_test.c: pipe_test.def pipe_test.tpl
	autogen --writable pipe_test.def

floating_point_test.c: floating_point_test.def floating_point_test.tpl
	autogen --writable floating_point_test.def

benchmark.c: benchmark.def benchmark.tpl
	autogen --writable benchmark.def

genfiles : write_read_test.c pcm_test.c header_test.c utils.c \
		scale_clip_test.c pipe_test.c floating_point_test.c benchmark.c

#===============================================================================
# If we're cross compiling from Linux to Windows and running the test suite
# under Wine, we need a symbolic link to the generated libsndfile DLL.

if LINUX_MINGW_CROSS_TEST

$(noinst_PROGRAMS) : libsndfile-1.dll

libsndfile-1.dll :
	ln -s $(top_builddir)/src/.libs/$@ $@

clean-local :
	-rm -f libsndfile-1.dll

endif

#===============================================================================

check: generic-tests wav-tests aiff-tests au-tests caf-tests raw-tests \
	paf-tests svx-tests nist-tests ircam-tests voc-tests w64-tests rf64-tests \
	mat4-tests mat5-tests pvf-tests xi-tests htk-tests avr-tests sds-tests \
	sd2-tests flac-tests caf-tests wve-tests vorbis-tests mpc2k-tests io-tests

generic-tests : $(noinst_PROGRAMS)
	uname -a
	./error_test$(EXEEXT)
	./pcm_test$(EXEEXT)
	./ulaw_test$(EXEEXT)
	./alaw_test$(EXEEXT)
	./dwvw_test$(EXEEXT)
	./command_test$(EXEEXT) ver
	./command_test$(EXEEXT) norm
	./command_test$(EXEEXT) format
	./command_test$(EXEEXT) peak
	./command_test$(EXEEXT) trunc
	./command_test$(EXEEXT) inst
	./command_test$(EXEEXT) current_sf_info
	./command_test$(EXEEXT) bext
	./command_test$(EXEEXT) bextch
	./floating_point_test$(EXEEXT)
	./checksum_test$(EXEEXT)
	./scale_clip_test$(EXEEXT)
	./headerless_test$(EXEEXT)
	./locale_test$(EXEEXT)
	./win32_ordinal_test$(EXEEXT)
	./external_libs_test$(EXEEXT)
if ENABLE_TEST_COVERAGE
	@echo "cpp_test not under test coverage"
else
	./cpp_test$(EXEEXT)
endif
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed common tests."
	@echo "----------------------------------------------------------------------"

wav-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) wav
	./lossy_comp_test$(EXEEXT) wav_pcm
	./lossy_comp_test$(EXEEXT) wav_ima
	./lossy_comp_test$(EXEEXT) wav_msadpcm
	./lossy_comp_test$(EXEEXT) wav_ulaw
	./lossy_comp_test$(EXEEXT) wav_alaw
	./lossy_comp_test$(EXEEXT) wav_gsm610
	./lossy_comp_test$(EXEEXT) wav_g721
	./peak_chunk_test$(EXEEXT) wav
	./header_test$(EXEEXT) wav
	./misc_test$(EXEEXT) wav
	./string_test$(EXEEXT) wav
	./multi_file_test$(EXEEXT) wav
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on WAV files."
	@echo "----------------------------------------------------------------------"

w64-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) w64
	./lossy_comp_test$(EXEEXT) w64_ima
	./lossy_comp_test$(EXEEXT) w64_msadpcm
	./lossy_comp_test$(EXEEXT) w64_ulaw
	./lossy_comp_test$(EXEEXT) w64_alaw
	./lossy_comp_test$(EXEEXT) w64_gsm610
	./header_test$(EXEEXT) w64
	./misc_test$(EXEEXT) w64
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on W64 files."
	@echo "----------------------------------------------------------------------"

rf64-tests: $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) rf64
	./header_test$(EXEEXT) rf64
	./misc_test$(EXEEXT) rf64
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on RF64 files."
	@echo "----------------------------------------------------------------------"

aiff-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) aiff
	./lossy_comp_test$(EXEEXT) aiff_ulaw
	./lossy_comp_test$(EXEEXT) aiff_alaw
	./lossy_comp_test$(EXEEXT) aiff_gsm610
	@echo "=========================="
	@echo "./lossy_comp_test$(EXEEXT) aiff_ima"
	@echo "=========================="
	./peak_chunk_test$(EXEEXT) aiff
	./header_test$(EXEEXT) aiff
	./misc_test$(EXEEXT) aiff
	./string_test$(EXEEXT) aiff
	./multi_file_test$(EXEEXT) aiff
	./aiff_rw_test$(EXEEXT)
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on AIFF files."
	@echo "----------------------------------------------------------------------"

caf-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) caf
	./lossy_comp_test$(EXEEXT) caf_ulaw
	./lossy_comp_test$(EXEEXT) caf_alaw
	./header_test$(EXEEXT) caf
	./misc_test$(EXEEXT) caf
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on CAF files."
	@echo "----------------------------------------------------------------------"

au-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) au
	./lossy_comp_test$(EXEEXT) au_ulaw
	./lossy_comp_test$(EXEEXT) au_alaw
	./lossy_comp_test$(EXEEXT) au_g721
	./lossy_comp_test$(EXEEXT) au_g723
	./header_test$(EXEEXT) au
	./misc_test$(EXEEXT) au
	./multi_file_test$(EXEEXT) au
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on AU files."
	@echo "----------------------------------------------------------------------"

raw-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) raw
	./lossy_comp_test$(EXEEXT) raw_ulaw
	./lossy_comp_test$(EXEEXT) raw_alaw
	./lossy_comp_test$(EXEEXT) raw_gsm610
	./lossy_comp_test$(EXEEXT) vox_adpcm
	./raw_test$(EXEEXT)
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on RAW (header-less) files."
	@echo "----------------------------------------------------------------------"

wve-tests : $(noinst_PROGRAMS)
	./lossy_comp_test$(EXEEXT) wve
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on WVE files."
	@echo "----------------------------------------------------------------------"

# Lite remove start
paf-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) paf
	./header_test$(EXEEXT) paf
	./misc_test$(EXEEXT) paf
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on PAF files."
	@echo "----------------------------------------------------------------------"

svx-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) svx
	./header_test$(EXEEXT) svx
	./misc_test$(EXEEXT) svx
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on SVX files."
	@echo "----------------------------------------------------------------------"

nist-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) nist
	./lossy_comp_test$(EXEEXT) nist_ulaw
	./lossy_comp_test$(EXEEXT) nist_alaw
	./header_test$(EXEEXT) nist
	./misc_test$(EXEEXT) nist
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on NIST files."
	@echo "----------------------------------------------------------------------"

ircam-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) ircam
	./lossy_comp_test$(EXEEXT) ircam_ulaw
	./lossy_comp_test$(EXEEXT) ircam_alaw
	./header_test$(EXEEXT) ircam
	./misc_test$(EXEEXT) ircam
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on IRCAM files."
	@echo "----------------------------------------------------------------------"

voc-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) voc
	./lossy_comp_test$(EXEEXT) voc_ulaw
	./lossy_comp_test$(EXEEXT) voc_alaw
	./header_test$(EXEEXT) voc
	./misc_test$(EXEEXT) voc
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on VOC files."
	@echo "----------------------------------------------------------------------"

mat4-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) mat4
	./header_test$(EXEEXT) mat4
	./misc_test$(EXEEXT) mat4
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on MAT4 files."
	@echo "----------------------------------------------------------------------"

mat5-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) mat5
	./header_test$(EXEEXT) mat5
	./misc_test$(EXEEXT) mat5
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on MAT5 files."
	@echo "----------------------------------------------------------------------"

pvf-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) pvf
	./header_test$(EXEEXT) pvf
	./misc_test$(EXEEXT) pvf
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on PVF files."
	@echo "----------------------------------------------------------------------"

xi-tests : $(noinst_PROGRAMS)
	./lossy_comp_test$(EXEEXT) xi_dpcm
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on XI files."
	@echo "----------------------------------------------------------------------"

htk-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) htk
	./header_test$(EXEEXT) htk
	./misc_test$(EXEEXT) htk
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on HTK files."
	@echo "----------------------------------------------------------------------"

avr-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) avr
	./header_test$(EXEEXT) avr
	./misc_test$(EXEEXT) avr
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on AVR files."
	@echo "----------------------------------------------------------------------"

sds-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) sds
	./header_test$(EXEEXT) sds
	./misc_test$(EXEEXT) sds
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on SDS files."
	@echo "----------------------------------------------------------------------"

sd2-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) sd2
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on SD2 files."
	@echo "----------------------------------------------------------------------"

flac-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) flac
	./string_test$(EXEEXT) flac
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on FLAC files."
	@echo "----------------------------------------------------------------------"

mpc2k-tests : $(noinst_PROGRAMS)
	./write_read_test$(EXEEXT) mpc2k
	./header_test$(EXEEXT) mpc2k
	./misc_test$(EXEEXT) mpc2k
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on MPC 2000 files."
	@echo "----------------------------------------------------------------------"

vorbis-tests : $(noinst_PROGRAMS)
	./ogg_test$(EXEEXT)
	./vorbis_test$(EXEEXT)
	./lossy_comp_test$(EXEEXT) ogg_vorbis
	./string_test$(EXEEXT) ogg
	./misc_test$(EXEEXT) ogg
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed tests on OGG/VORBIS files."
	@echo "----------------------------------------------------------------------"

# Lite remove end

io-tests : $(noinst_PROGRAMS)
	./stdio_test$(EXEEXT)
	./pipe_test$(EXEEXT)
	./virtual_io_test$(EXEEXT)
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion$(EXEEXT)` passed stdio/pipe/vio tests."
	@echo "----------------------------------------------------------------------"


