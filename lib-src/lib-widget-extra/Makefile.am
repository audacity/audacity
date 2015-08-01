ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = libwidgetextra.pc.in

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = libwidgetextra.pc

lib_LTLIBRARIES = libwidgetextra.la

libwidgetextra_la_CPPFLAGS = $(WX_CXXFLAGS) -Wno-deprecated-declarations
libwidgetextra_la_LIBADD = $(WX_LIBS)
libwidgetextra_la_SOURCES = NonGuiThread.cpp NonGuiThread.h
