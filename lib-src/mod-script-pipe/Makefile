# -----------------------------------------------------------------------------
# Build mod-script-pipe plugin
#
# EXPERIMENTAL!
# (Based on a Makefile by Leland)
#

# -----------------------------------------------------------------------------
# NOTE: Change this to the base of the Audacity source distribution, or specify
#       via command line or environment
#
AUDACITY_DIR = ../..

# -----------------------------------------------------------------------------
# NOTE: Set to the names of your objects and final module name
#
OBJS = PipeServer.o ScripterCallback.o
MOD = mod-script-pipe.so

# -----------------------------------------------------------------------------
# NOTE: Set any custom flags you may need
#
CXXFLAGS += -Wall -O9
CXXFLAGS += -DCC_HASVISIBILITY # Normally provided by configure
CXXFLAGS += -DBUILDING_SCRIPT_PIPE
CXXFLAGS += ${shell echo @WX_CXXFLAGS@ | ../../config.status --file=- }
CXXFLAGS += -D__WXDEBUG__ -D__WXGTK__

# -----------------------------------------------------------------------------
# Hopefully the rest is generic enough to satisfy most needs
# -----------------------------------------------------------------------------

CXXFLAGS += -DAUDACITY_DLL_API= -I$(AUDACITY_DIR)/src
CXXFLAGS += ${shell set -x ; sed -e '/override CXXFLAGS/!d;s/override CXXFLAGS += //;s@$$(top_srcdir)@$(AUDACITY_DIR)@g' $(AUDACITY_DIR)/src/Makefile}

LDFLAGS += ${shell sed -e '/^LIBS/!d;s/LIBS *=//' $(AUDACITY_DIR)/src/Makefile}

SYS = $(shell uname -s)

ifeq ($(SYS),Darwin)
   CXXFLAGS += -arch i386 -arch ppc -isysroot /Developer/SDKs/MacOSX10.4u.sdk -mmacosx-version-min=10.4
   LDFLAGS += $(CXXFLAGS) -dynamiclib -undefined suppress
else
   CXXFLAGS += -fPIC -fvisibility=hidden
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
