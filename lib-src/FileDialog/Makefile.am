ACLOCAL_AMFLAGS = -I m4

lib_LTLIBRARIES = libFileDialog.la

libFileDialog_la_LIBTOOLFLAGS = --tag=CXX
libFileDialog_la_CPPFLAGS = $(WX_CXXFLAGS) -Wno-deprecated-declarations
libFileDialog_la_LIBADD = $(WX_LIBS)
libFileDialog_la_SOURCES = FileDialog.cpp FileDialog.h

if GTK
libFileDialog_la_CPPFLAGS += $(GTK_CFLAGS)
libFileDialog_la_LIBADD += $(GTK_LIBS)
libFileDialog_la_SOURCES += \
	gtk/FileDialogPrivate.cpp \
	gtk/FileDialogPrivate.h \
	$(NULL)
endif

if MAC
libFileDialog_la_SOURCES += mac/FileDialogPrivate.mm \
									 mac/FileDialogPrivate.h \
									 $(NULL)
endif

if WINDOWS
libFileDialog_la_SOURCES += win/FileDialogPrivate.cpp \
									 win/FileDialogPrivate.h \
									 $(NULL)
endif
