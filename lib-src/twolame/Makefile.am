
AUTOMAKE_OPTIONS = foreign

SUBDIRS = libtwolame frontend simplefrontend doc

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = twolame.pc


EXTRA_DIST = \
	autogen.sh \
	twolame.pc.in \
	win32/configwin.h \
	win32/libtwolame_dll.sln \
	win32/libtwolame_dll.vcproj \
	win32/libtwolame_static.sln \
	win32/libtwolame_static.vcproj \
	win32/winutil.h

#tell aclocal where extra macros can be found
ACLOCAL_AMFLAGS = -I build/m4
