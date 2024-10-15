## Process this file with automake to produce Makefile.in
# Makefile(.in) for portSMF using Autotools (but not libtool).
# Written by Richard Ash following Gary Vaughan's Autobook

# tell aclocal that extra macros can be found in autotools/m4/
ACLOCAL_AMFLAGS = -I autotools/m4

# define the output library (link with -lportSMF)
lib_LIBRARIES = libportSMF.a

# define the source files to use
libportSMF_a_SOURCES = allegro.cpp \
	allegrosmfwr.cpp \
	allegrord.cpp \
	allegrowr.cpp \
	allegrosmfrd.cpp \
	mfmidi.cpp \
	strparse.cpp \
	algrd_internal.h \
	algsmfrd_internal.h

# and the header files for the library. At the moment these go into the include
# directory directly, it would be much better to have them in a subdirectory
# but that would mean re-organising them here (a subdirectory called portSMF to
# keep headers in, add -I$(srcdir)/portSMF/ to AM_CXXFLAGS, change the paths
# to the headers here and change the name of the variable to nobase_include_...
include_HEADERS = allegro.h \
	mfmidi.h \
	strparse.h \
	trace.h

# files that only really viscous cleans remove
MAINTAINERCLEANFILES = Makefile.in

# Documentation files that should be both distributed and installed in the doc
# directory,
dist_doc_DATA = README.txt license.txt

EXTRA_DIST = \
	allegro.htm \
	allegroserial.cpp \
	apps/allegroconvert.cpp \
	apps/allegroconvert.vcproj \
	apps/allegroplay.cpp \
	apps/midicode.h \
	apps/seq2midi.cpp \
	apps/seq2midi.h \
	changelog.txt \
	notes.txt \
	portsmf-VC8.sln \
	portsmf-VC8.vcproj \
	portsmf.sln \
	portsmf.suo \
	portsmf.vcproj \
	portsmf.xcodeproj \
	portsmf.xcodeproj/project.pbxproj \
	portsmf.xcodeproj/rbd.mode1 \
	portsmf_test \
	portsmf_test/Makefile.osx \
	portsmf_test/portsmf_test-VC8.vcproj \
	portsmf_test/portsmf_test.cpp \
	portsmf_test/portsmf_test.vcproj \
	portsmf_test/scale.gro \
	portsmf_test/test.gro \
	portsmf_test/test2.gro \
	portsmf_test/test5.gro \
	todo.txt \
	trace.cpp \
	$(NULL)
