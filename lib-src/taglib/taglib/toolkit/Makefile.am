INCLUDES = \
	-I$(top_srcdir)/taglib \
	$(all_includes)

noinst_LTLIBRARIES = libtoolkit.la

libtoolkit_la_SOURCES = \
	tstring.cpp tstringlist.cpp tbytevector.cpp \
	tbytevectorlist.cpp tfile.cpp tdebug.cpp unicode.cpp

taglib_include_HEADERS = \
	taglib.h tstring.h tlist.h tlist.tcc tstringlist.h \
	tbytevector.h tbytevectorlist.h tfile.h \
	tmap.h tmap.tcc

taglib_includedir = $(includedir)/taglib
