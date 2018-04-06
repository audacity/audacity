pkglib_LTLIBRARIES = mod-nyq-bench.la

ACLOCAL_AMFLAGS = ${ACLOCAL_AMFLAGS} -I m4

mod_nyq_bench_la_CPPFLAGS = $(WX_CXXFLAGS) -I$(top_srcdir)/../../include -I$(top_srcdir)/../../src -I$(top_builddir)/../../src -I$(top_srcdir)/../portaudio-v19/include -I$(top_srcdir)/../portmixer/include -I$(top_srcdir)/../libnyquist
mod_nyq_bench_la_LIBADD = $(WX_LIBS)
mod_nyq_bench_la_LDFLAGS = -module

mod_nyq_bench_la_SOURCES = \
	NyqBench.cpp \
	NyqBench.h
	$(NULL)

