pkglib_LTLIBRARIES = mod-null.la

ACLOCAL_AMFLAGS = ${ACLOCAL_AMFLAGS} -I m4

mod_null_la_CPPFLAGS = $(WX_CXXFLAGS) -I$(top_srcdir)/../../include -I$(top_srcdir)/../../src -I$(top_builddir)/../../src -I$(top_srcdir)/../portaudio-v19/include 
mod_null_la_LIBADD = $(WX_LIBS)
mod_null_la_LDFLAGS = -module

mod_null_la_SOURCES = \
	ModNullCallback.cpp \
	ModNullCallback.h
	$(NULL)

