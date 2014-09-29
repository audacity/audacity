ACLOCAL_AMFLAGS = -I m4

EXTRA_DIST = FileDialog.vcproj FileDialogPrivate.h.in

lib_LTLIBRARIES = libFileDialog.la

libFileDialog_la_CPPFLAGS = $(WX_CXXFLAGS)
libFileDialog_la_LIBADD = $(WX_LIBS)
libFileDialog_la_SOURCES = FileDialog.cpp FileDialog.h

if GENERIC
libFileDialog_la_SOURCES += \
	generic/FileDialogPrivate.cpp \
	generic/FileDialogPrivate.h \
	$(NULL)
endif

if GTK
libFileDialog_la_CPPFLAGS += $(GTK_CFLAGS)
libFileDialog_la_LIBADD += $(GTK_LIBS)
libFileDialog_la_SOURCES += \
	gtk/FileDialogPrivate.cpp \
	gtk/FileDialogPrivate.h \
	gtk/private.h \
	$(NULL)
endif

if MAC
libFileDialog_la_SOURCES += mac/FileDialogPrivate.cpp mac/FileDialogPrivate.h
endif

if WINDOWS
libFileDialog_la_SOURCES += win/FileDialogPrivate.cpp win/FileDialogPrivate.h
endif
