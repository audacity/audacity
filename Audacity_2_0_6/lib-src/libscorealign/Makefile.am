## Process this file with automake to produce Makefile.in
# Makefile(.in) for scorealign using Autotools (but not libtool).
# Written by Richard Ash following Gary Vaughan's Autobook

# tell aclocal that extra macros can be found in autotools/m4/
ACLOCAL_AMFLAGS = -I autotools/m4

# define the output library (link with -lscorealign)
lib_LIBRARIES = libscorealign.a

# define the source files to use
libscorealign_a_SOURCES = audiomixerreader.cpp \
	audioreader.cpp \
	comp_chroma.cpp \
	curvefit.cpp \
	gen_chroma.cpp \
	hillclimb.cpp \
	regression.cpp \
	sautils.cpp \
	scorealign.cpp \
	scorealign-glue.cpp \
	fft3/FFT3.cpp \
	audiomixerreader.h \
	audioreader.h \
	comp_chroma.h \
	curvefit.h \
	gen_chroma.h \
	hillclimb.h \
	regression.h \
	sautils.h \
	fft3/FFT3.h

# and the header files for the library. At the moment these go into the include
# directory directly, it would be much better to have them in a subdirectory
# but that would mean re-organising them here (a subdirectory called scorealign to
# keep headers in, add -I$(srcdir)/scorealign/ to AM_CXXFLAGS, change the paths
# to the headers here and change the name of the variable to nobase_include_...
include_HEADERS = scorealign-glue.h \
	scorealign.h \
	ScoreAlignParams.h

override CXXFLAGS += -I../portsmf

# files that only really viscous cleans remove
MAINTAINERCLEANFILES = Makefile.in

# other distributed files in case users don't have a complete set of auto*
AUX_DIST = $(ac_aux_dir)/config.guess \
   $(ac_aux_dir)/config.sub \
   $(ac_aux_dir)/install-sh \
   $(ac_aux_dir)/mdate-sh \
   $(ac_aux_dir)/missing \
   $(ac_aux_dir)/mkinstalldirs \
   $(MAINTAINERCLEANFILES)

# Documentation files that should be both distributed and installed in the doc
# directory,
dist_doc_DATA = README.txt license.txt
