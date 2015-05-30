#  FLAC - Free Lossless Audio Codec
#  Copyright (C) 2001-2009  Josh Coalson
#  Copyright (C) 2011-2014  Xiph.Org Foundation
#
#  This file is part the FLAC project.  FLAC is comprised of several
#  components distributed under different licenses.  The codec libraries
#  are distributed under Xiph.Org's BSD-like license (see the file
#  COPYING.Xiph in this distribution).  All other programs, libraries, and
#  plugins are distributed under the GPL (see COPYING.GPL).  The documentation
#  is distributed under the Gnu FDL (see COPYING.FDL).  Each file in the
#  FLAC distribution contains at the top the terms under which it may be
#  distributed.
#
#  Since this particular file is relevant to all components of FLAC,
#  it may be distributed under the Xiph.Org license, which is the least
#  restrictive of those mentioned above.  See the file COPYING.Xiph in this
#  distribution.

#
# GNU makefile fragment for building a library
#

include $(topdir)/build/config.mk

ifeq ($(OS),Darwin)
    CC          = cc
    CCC         = c++
else
    CC          = gcc
    CCC         = g++
endif
NASM        = nasm
LINK        = ar cru
OBJPATH     = $(topdir)/objs
LIBPATH     = $(OBJPATH)/$(BUILD)/lib
DEBUG_LIBPATH     = $(OBJPATH)/debug/lib
RELEASE_LIBPATH   = $(OBJPATH)/release/lib
ifeq ($(OS),Darwin)
    STATIC_LIB_SUFFIX = a
    DYNAMIC_LIB_SUFFIX = dylib
else
ifeq ($(findstring Windows,$(OS)),Windows)
    STATIC_LIB_SUFFIX = a
    DYNAMIC_LIB_SUFFIX = dll
else
    STATIC_LIB_SUFFIX = a
    DYNAMIC_LIB_SUFFIX = so
endif
endif
STATIC_LIB_NAME     = $(LIB_NAME).$(STATIC_LIB_SUFFIX)
DYNAMIC_LIB_NAME    = $(LIB_NAME).$(DYNAMIC_LIB_SUFFIX)
STATIC_LIB          = $(LIBPATH)/$(STATIC_LIB_NAME)
DYNAMIC_LIB         = $(LIBPATH)/$(DYNAMIC_LIB_NAME)
DEBUG_STATIC_LIB    = $(DEBUG_LIBPATH)/$(STATIC_LIB_NAME)
DEBUG_DYNAMIC_LIB   = $(DEBUG_LIBPATH)/$(DYNAMIC_LIB_NAME)
RELEASE_STATIC_LIB  = $(RELEASE_LIBPATH)/$(STATIC_LIB_NAME)
RELEASE_DYNAMIC_LIB = $(RELEASE_LIBPATH)/$(DYNAMIC_LIB_NAME)
ifeq ($(OS),Darwin)
    LINKD       = $(CC) -dynamiclib -flat_namespace -undefined suppress -install_name $(DYNAMIC_LIB)
else
    LINKD       = $(CC) -shared
endif

debug   : CFLAGS = -g -O0 -DDEBUG $(CONFIG_CFLAGS) $(DEBUG_CFLAGS) -W -Wall -DVERSION=$(VERSION) $(DEFINES) $(INCLUDES)
valgrind: CFLAGS = -g -O0 -DDEBUG $(CONFIG_CFLAGS) $(DEBUG_CFLAGS) -DFLAC__VALGRIND_TESTING -W -Wall -DVERSION=$(VERSION) $(DEFINES) $(INCLUDES)
release : CFLAGS = -O3 -fomit-frame-pointer -funroll-loops -finline-functions -DNDEBUG $(CONFIG_CFLAGS) $(RELEASE_CFLAGS) -W -Wall -Winline -DFLaC__INLINE=__inline__ -DVERSION=$(VERSION) $(DEFINES) $(INCLUDES)

CFLAGS   = $(CFLAGS) -Wmissing-prototypes -Wstrict-prototypes
CXXFLAGS = $(CFLAGS)

LFLAGS   = -L$(LIBPATH)

DEBUG_OBJS = $(SRCS_C:%.c=%.debug.o) $(SRCS_CC:%.cc=%.debug.o) $(SRCS_CPP:%.cpp=%.debug.o) $(SRCS_NASM:%.nasm=%.debug.o)
RELEASE_OBJS = $(SRCS_C:%.c=%.release.o) $(SRCS_CC:%.cc=%.release.o) $(SRCS_CPP:%.cpp=%.release.o) $(SRCS_NASM:%.nasm=%.release.o)
ifeq ($(PROC),x86_64)
DEBUG_PIC_OBJS = $(SRCS_C:%.c=%.debug.pic.o) $(SRCS_CC:%.cc=%.debug.pic.o) $(SRCS_CPP:%.cpp=%.debug.pic.o) $(SRCS_NASM:%.nasm=%.debug.pic.o)
RELEASE_PIC_OBJS = $(SRCS_C:%.c=%.release.pic.o) $(SRCS_CC:%.cc=%.release.pic.o) $(SRCS_CPP:%.cpp=%.release.pic.o) $(SRCS_NASM:%.nasm=%.release.pic.o)
endif

debug   : $(DEBUG_STATIC_LIB) $(DEBUG_DYNAMIC_LIB)
valgrind: $(DEBUG_STATIC_LIB) $(DEBUG_DYNAMIC_LIB)
release : $(RELEASE_STATIC_LIB) $(RELEASE_DYNAMIC_LIB)

$(DEBUG_STATIC_LIB): $(DEBUG_OBJS)
	$(LINK) $@ $(DEBUG_OBJS) && ranlib $@

$(RELEASE_STATIC_LIB): $(RELEASE_OBJS)
	$(LINK) $@ $(RELEASE_OBJS) && ranlib $@

$(DEBUG_DYNAMIC_LIB) : $(DEBUG_OBJS) $(DEBUG_PIC_OBJS)
ifeq ($(OS),Darwin)
	echo Not building dynamic lib, command is: $(LINKD) -o $@ $(DEBUG_OBJS) $(LFLAGS) $(LIBS) -lc
else
ifeq ($(PROC),x86_64)
	$(LINKD) -o $@ $(DEBUG_PIC_OBJS) $(LFLAGS) $(LIBS)
else
	$(LINKD) -o $@ $(DEBUG_OBJS) $(LFLAGS) $(LIBS)
endif
endif

$(RELEASE_DYNAMIC_LIB) : $(RELEASE_OBJS) $(RELEASE_PIC_OBJS)
ifeq ($(OS),Darwin)
	echo Not building dynamic lib, command is: $(LINKD) -o $@ $(RELEASE_OBJS) $(LFLAGS) $(LIBS) -lc
else
ifeq ($(PROC),x86_64)
	$(LINKD) -o $@ $(RELEASE_PIC_OBJS) $(LFLAGS) $(LIBS)
else
	$(LINKD) -o $@ $(RELEASE_OBJS) $(LFLAGS) $(LIBS)
endif
endif

include $(topdir)/build/compile.mk

.PHONY : clean
clean :
	-rm -f $(DEBUG_OBJS) $(RELEASE_OBJS) $(DEBUG_PIC_OBJS) $(RELEASE_PIC_OBJS) $(OBJPATH)/*/lib/$(STATIC_LIB_NAME) $(OBJPATH)/*/lib/$(DYNAMIC_LIB_NAME)

.PHONY : depend
depend:
	makedepend -fMakefile.lite -- $(CFLAGS) $(INCLUDES) -- *.c *.cc *.cpp
