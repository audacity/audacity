## Process this file with automake to produce Makefile.in

if ENABLE_TEST_COVERAGE
CPP_TEST =
else
CPP_TEST = cpp_test
endif

AM_CPPFLAGS = -I$(top_srcdir)/src

noinst_PROGRAMS = sfversion floating_point_test write_read_test \
	lossy_comp_test error_test ulaw_test alaw_test dwvw_test \
	peak_chunk_test command_test stdin_test stdout_test stdio_test \
	pcm_test headerless_test pipe_test benchmark header_test misc_test \
	raw_test string_test multi_file_test dither_test \
	scale_clip_test win32_test fix_this aiff_rw_test virtual_io_test \
	locale_test largefile_test win32_ordinal_test ogg_test vorbis_test \
	checksum_test external_libs_test rdwr_test $(CPP_TEST)

noinst_HEADERS = dft_cmp.h utils.h generate.h

autogen_sources = write_read_test.tpl write_read_test.def	\
					pcm_test.tpl pcm_test.def				\
					header_test.tpl header_test.def			\
					utils.tpl utils.def						\
					scale_clip_test.tpl scale_clip_test.def	\
					pipe_test.tpl pipe_test.def				\
					rdwr_test.tpl rdwr_test.def				\
					floating_point_test.tpl floating_point_test.def	\
					benchmark.tpl benchmark.def

EXTRA_DIST = $(autogen_sources)

CLEANFILES = *~

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

rdwr_test_SOURCES = rdwr_test.c utils.c
rdwr_test_LDADD = $(top_builddir)/src/libsndfile.la

win32_test_SOURCES = win32_test.c
# Link lib here so that generating the testsuite tarball works correctly.
win32_test_LDADD = $(top_builddir)/src/libsndfile.la

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

rdwr_test.c: rdwr_test.def rdwr_test.tpl
	autogen --writable rdwr_test.def

floating_point_test.c: floating_point_test.def floating_point_test.tpl
	autogen --writable floating_point_test.def

benchmark.c: benchmark.def benchmark.tpl
	autogen --writable benchmark.def

genfiles : write_read_test.c pcm_test.c header_test.c utils.c \
		scale_clip_test.c pipe_test.c floating_point_test.c rdwr_test.c \
		benchmark.c

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

check: $(noinst_PROGRAMS) test_wrapper.sh
	sh test_wrapper.sh


