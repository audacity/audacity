#  FLAC - Free Lossless Audio Codec
#  Copyright (C) 2001-2009  Josh Coalson
#  Copyright (C) 2011-2013  Xiph.Org Foundation
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
# customizable settings from the make invocation
#

USE_OGG     ?= 1
USE_ICONV   ?= 1

#
# debug/release selection
#

DEFAULT_BUILD = release

# returns Linux, Darwin, FreeBSD, etc.
ifdef OS_OVERRIDE
    OS := $(OS_OVERRIDE)
else
    OS := $(shell uname -s)
endif
# returns i386, x86_64, powerpc, etc.
ifdef PROC_OVERRIDE
    PROC := $(PROC_OVERRIDE)
else
    ifeq ($(findstring MINGW,$(OS)),MINGW)
        PROC := i386 # failsafe
        # ifeq (mingw32,$(shell gcc -dumpmachine)) # MinGW (mainline): mingw32
        ifeq ($(findstring i686,$(shell gcc -dumpmachine)),i686) # MinGW-w64: i686-w64-mingw32
            USE_ICONV := 0
        else ifeq ($(findstring x86_64,$(shell gcc -dumpmachine)),x86_64) # MinGW-w64: x86_64-w64-mingw32
            USE_ICONV := 0
            PROC := x86_64
        endif
    else
        PROC := $(shell uname -p)
    endif
endif
ifeq ($(PROC),powerpc)
    PROC := ppc
endif

ifeq ($(OS),Linux)
    PROC := $(shell uname -m)
    USE_ICONV := 0
endif

debug    : BUILD = debug
valgrind : BUILD = debug
release  : BUILD = release

# override LINKAGE on OS X until we figure out how to get 'cc -static' to work
ifeq ($(OS),Darwin)
LINKAGE = -arch $(PROC)
else
debug    : LINKAGE = -static
valgrind : LINKAGE = -dynamic
release  : LINKAGE = -static
endif

all default: $(DEFAULT_BUILD)

#
# GNU makefile fragment for emulating stuff normally done by configure
#

VERSION=\"1.3.0\"

CONFIG_CFLAGS=-DHAVE_STDINT_H -DHAVE_INTTYPES_H -DHAVE_CXX_VARARRAYS -DHAVE_LANGINFO_CODESET -D_LARGEFILE_SOURCE -D_FILE_OFFSET_BITS=64

ifeq ($(OS),Darwin)
    CONFIG_CFLAGS += -DFLAC__SYS_DARWIN -arch $(PROC)
else
    CONFIG_CFLAGS += -DHAVE_SOCKLEN_T
endif

ifeq ($(PROC),ppc)
    CONFIG_CFLAGS += -DWORDS_BIGENDIAN=1
else
    CONFIG_CFLAGS += -DWORDS_BIGENDIAN=0
endif

ifeq ($(OS),Linux)
	ifeq ($(PROC),x86_64)
        CONFIG_CFLAGS += -fPIC
	endif
endif

ifneq (0,$(USE_ICONV))
    CONFIG_CFLAGS += -DHAVE_ICONV
    ICONV_LIBS = -liconv
else
    ICONV_LIBS =
endif

ifneq (0,$(USE_OGG))
    CONFIG_CFLAGS += -DFLAC__HAS_OGG=1
    OGG_INCLUDES = -I$(OGG_INCLUDE_DIR)
    OGG_EXPLICIT_LIBS = $(OGG_LIB_DIR)/libogg.a
    OGG_LIBS = -L$(OGG_LIB_DIR) -logg
    OGG_SRCS = $(OGG_SRCS_C)
else
    CONFIG_CFLAGS += -DFLAC__HAS_OGG=0
    OGG_INCLUDES =
    OGG_EXPLICIT_LIBS =
    OGG_LIBS =
    OGG_SRCS =
endif

OGG_INCLUDE_DIR=$(HOME)/local/include
OGG_LIB_DIR=$(HOME)/local/lib
