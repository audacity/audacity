# -----------------------------------------------------------------------------
# Nothing fancy...just build the darn thing...that's all
#

# -----------------------------------------------------------------------------
# NOTE: Change this to the base of the Audacity source distribution, or specify
#       via command line or environment
#       Default is to use the sources which this directory is part of
#
AUDACITY_DIR ?= ../..

# -----------------------------------------------------------------------------
# NOTE: Set to the names of your objects and final module name
#
OBJS = NyqBench.o
MOD = mod-nyq-bench.so

# -----------------------------------------------------------------------------
# NOTE: Set any custom flags you may need
#
CXXFLAGS += -Wall -O9

# -----------------------------------------------------------------------------
# Hopefully the rest is generic enough to satisfy most needs
# -----------------------------------------------------------------------------

CXXFLAGS += -DAUDACITY_DLL_API= -I$(AUDACITY_DIR)/src -I$(AUDACITY_DIR)/lib-src/portaudio-v19/include
CXXFLAGS += ${shell echo @CPPFLAGS@ | ../../config.status --file=- }
CXXFLAGS += ${shell echo @CXXFLAGS@ | ../../config.status --file=- | sed -e 's@$$(top_srcdir)@$(AUDACITY_DIR)@g' }
CXXFLAGS += ${shell echo @LIBNYQUIST_CFLAGS@ | ../../config.status --file=- | sed -e 's@$$(top_srcdir)@$(AUDACITY_DIR)@g' }
CXXFLAGS += ${shell echo @WX_CXXFLAGS@ | ../../config.status --file=- }
LDFLAGS += ${shell echo @LIBS@ | ../../config.status --file=- }

SYS = $(shell uname -s)

ifeq ($(SYS),Darwin)
   CXXFLAGS += -arch i386 -arch ppc -isysroot /Developer/SDKs/MacOSX10.4u.sdk -mmacosx-version-min=10.4 
   LDFLAGS += $(CXXFLAGS) -dynamiclib -undefined suppress
else
   CXXFLAGS += -fPIC
   LDFLAGS += -shared
endif

LD = g++

all: basecheck $(MOD)

# -----------------------------------------------------------------------------
# Make sure we can get to the Audacity source
#
basecheck:
	@if test -z "$(AUDACITY_DIR)/src/Audacity.h"                   ; \
	then                                                             \
	  echo "You need to set AUDACITY_DIR equal to the base"        ; \
	  echo "of your Audacity source directory.  You can do"        ; \
	  echo "this via an environemnt variable, include it on"       ; \
	  echo "the make command line or set it at the top of"         ; \
	  echo "the Makefile."                                         ; \
     exit 1                                                       ; \
	fi

# -----------------------------------------------------------------------------
# Build it
#
$(MOD): $(OBJS)
	$(LD) $(LDFLAGS) -o $(MOD) $(OBJS)
	@mkdir -p $(AUDACITY_DIR)/modules
	@cp $(MOD) $(AUDACITY_DIR)/modules
	@echo
	@echo "$(MOD) has been copied to $(AUDACITY_DIR)/modules"
	@echo

# -----------------------------------------------------------------------------
# Cleanup
#
clean: 
	-rm $(MOD) $(OBJS)
