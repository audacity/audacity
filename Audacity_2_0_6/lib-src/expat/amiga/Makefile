#
# Makefile for AmigaOS
#

.PHONY: help all check clean package
.PHONY: clib2 newlib library so

vpath %.c ../lib ../examples ../xmlwf ../tests ../tests/benchmark
vpath %.h ../lib ../tests

#############################################################################

help:
	@echo "Requires:"
	@echo " AmigaOS 4.x"
	@echo " SDK 53.13"
	@echo ""
	@echo "Targets:"
	@echo " all     - make libraries, xmlwf, examples and runs tests"
	@echo " install - install expat libraries and tools into SDK"
	@echo " clean   - clean object files"
	@echo " check   - run all the tests"
	@echo " package - prepare distribution archive"

all: clib2 newlib library so check

clib2: clib2/libexpat.a clib2/xmlwf clib2/elements clib2/outline clib2/runtests clib2/benchmark

newlib: newlib/libexpat.a newlib/xmlwf newlib/elements newlib/outline newlib/runtests newlib/benchmark

library: libs/expat.library libs/xmlwf libs/elements libs/outline libs/runtests libs/benchmark

so: so/libexpat.so so/xmlwf so/elements so/outline so/runtests so/benchmark

check: clib2/runtests newlib/runtests libs/runtests so/runtests
	clib2/runtests
	newlib/runtests
	libs/runtests
	so/runtests

clean:
	-delete clib2/#?.o quiet
	-delete newlib/#?.o quiet
	-delete libs/#?.o quiet
	-delete so/#?.o quiet

package:
	$(MAKE) all
	-delete T:expat all force quiet
	makedir all T:expat/Workbench/Libs
	copy clone libs/expat.library T:expat/Workbench/Libs
	makedir all T:expat/Workbench/SObjs
	copy clone so/libexpat.so T:expat/Workbench/SObjs
	makedir all T:expat/SDK/Local/C
	copy clone libs/xmlwf T:expat/SDK/Local/C
	makedir all T:expat/SDK/Local/clib2/lib
	copy clone clib2/libexpat.a T:expat/SDK/Local/clib2/lib
	makedir all T:expat/SDK/Local/newlib/lib
	copy clone newlib/libexpat.a T:expat/SDK/Local/newlib/lib
	makedir all T:expat/SDK/Local/common/include
	copy clone /lib/expat.h /lib/expat_external.h T:expat/SDK/Local/common/include
	makedir all T:expat/SDK/Include/include_h/inline4
	copy clone include/inline4/expat.h T:expat/SDK/Include/include_h/inline4
	makedir all T:expat/SDK/Include/include_h/interfaces
	copy clone include/interfaces/expat.h T:expat/SDK/Include/include_h/interfaces
	makedir all T:expat/SDK/Include/include_h/libraries
	copy clone include/libraries/expat.h T:expat/SDK/Include/include_h/libraries
	makedir all T:expat/SDK/Include/include_h/proto
	copy clone include/proto/expat.h T:expat/SDK/Include/include_h/proto
	makedir all T:expat/SDK/Documentation/Libs/Expat
	copy clone /COPYING T:expat/SDK/Documentation/Libs/Expat
	copy clone /README T:expat/SDK/Documentation/Libs/Expat
	copy clone README.txt T:expat/SDK/Documentation/Libs/Expat/README.AmigaOS
	-delete expat.lha
	lha -r a expat.lha T:expat

#############################################################################

CC         := gcc
LIBTOOL    := ar
STRIP      := strip

CFLAGS     := -DNDEBUG -O3
LTFLAGS    := -crs
STRIPFLAGS := -R.comment

#############################################################################

clib2/libexpat.a: clib2/xmlparse.o clib2/xmltok.o clib2/xmlrole.o
	$(LIBTOOL) $(LTFLAGS) $@ $^
	protect $@ -e

clib2/xmlparse.o: xmlparse.c expat.h xmlrole.h xmltok.h \
	expat_external.h internal.h amigaconfig.h

clib2/xmlrole.o: xmlrole.c ascii.h xmlrole.h expat_external.h \
	internal.h amigaconfig.h

clib2/xmltok.o: xmltok.c xmltok_impl.c xmltok_ns.c ascii.h asciitab.h \
	iasciitab.h latin1tab.h nametab.h utf8tab.h xmltok.h xmltok_impl.h \
	expat_external.h internal.h amigaconfig.h

#############################################################################

clib2/xmlwf: clib2/xmlwf.o clib2/xmlfile.o clib2/codepage.o clib2/readfilemap.o
	$(CC) -mcrt=clib2 $^ -o $@ clib2/libexpat.a
	$(STRIP) $(STRIPFLAGS) $@

clib2/xmlwf.o: xmlwf.c

clib2/xmlfile.o: xmlfile.c

clib2/codepage.o: codepage.c

clib2/readfilemap.o: readfilemap.c

#############################################################################

clib2/elements: clib2/elements.o
	$(CC) -mcrt=clib2 $^ -o $@ clib2/libexpat.a
	$(STRIP) $(STRIPFLAGS) $@

clib2/elements.o: elements.c

#############################################################################

clib2/outline: clib2/outline.o
	$(CC) -mcrt=clib2 $^ -o $@ clib2/libexpat.a
	$(STRIP) $(STRIPFLAGS) $@

clib2/outline.o: outline.c

#############################################################################

clib2/runtests: clib2/runtests.o clib2/chardata.o clib2/minicheck.o
	$(CC) -mcrt=clib2 $^ -o $@ clib2/libexpat.a

clib2/chardata.o: chardata.c chardata.h

clib2/minicheck.o: minicheck.c minicheck.h

clib2/runtests.o: runtests.c chardata.h

#############################################################################

clib2/benchmark: clib2/benchmark.o
	$(CC) -mcrt=clib2 $^ -o $@ clib2/libexpat.a -lm

clib2/benchmark.o: benchmark.c

#############################################################################

newlib/libexpat.a: newlib/xmlparse.o newlib/xmltok.o newlib/xmlrole.o
	$(LIBTOOL) $(LTFLAGS) $@ $^
	protect $@ -e

newlib/xmlparse.o: xmlparse.c expat.h xmlrole.h xmltok.h \
	expat_external.h internal.h amigaconfig.h

newlib/xmlrole.o: xmlrole.c ascii.h xmlrole.h expat_external.h \
	internal.h amigaconfig.h

newlib/xmltok.o: xmltok.c xmltok_impl.c xmltok_ns.c ascii.h asciitab.h \
	iasciitab.h latin1tab.h nametab.h utf8tab.h xmltok.h xmltok_impl.h \
	expat_external.h internal.h amigaconfig.h

#############################################################################

newlib/xmlwf: newlib/xmlwf.o newlib/xmlfile.o newlib/codepage.o newlib/readfilemap.o
	$(CC) -mcrt=newlib $^ -o $@ newlib/libexpat.a
	$(STRIP) $(STRIPFLAGS) $@

newlib/xmlwf.o: xmlwf.c

newlib/xmlfile.o: xmlfile.c

newlib/codepage.o: codepage.c

newlib/readfilemap.o: readfilemap.c

#############################################################################

newlib/elements: newlib/elements.o
	$(CC) -mcrt=newlib $^ -o $@ newlib/libexpat.a
	$(STRIP) $(STRIPFLAGS) $@

newlib/elements.o: elements.c

#############################################################################

newlib/outline: newlib/outline.o
	$(CC) -mcrt=newlib $^ -o $@ newlib/libexpat.a
	$(STRIP) $(STRIPFLAGS) $@

newlib/outline.o: outline.c

#############################################################################

newlib/runtests: newlib/runtests.o newlib/chardata.o newlib/minicheck.o
	$(CC) -mcrt=newlib $^ -o $@ newlib/libexpat.a

newlib/chardata.o: chardata.c chardata.h

newlib/minicheck.o: minicheck.c minicheck.h

newlib/runtests.o: runtests.c chardata.h

#############################################################################

newlib/benchmark: newlib/benchmark.o
	$(CC) -mcrt=newlib $^ -o $@ newlib/libexpat.a

newlib/benchmark.o: benchmark.c

#############################################################################

libs/expat.library: libs/expat_lib.o libs/expat_68k.o libs/expat_68k_handler_stubs.o libs/expat_vectors.o newlib/libexpat.a
	$(CC) -mcrt=newlib -nostartfiles $^ -o $@ newlib/libexpat.a -Wl,--cref,-M,-Map=$@.map
	protect $@ -e
	$(STRIP) $(STRIPFLAGS) $@

libs/expat_lib.o: expat_lib.c expat_base.h

libs/expat_68k.o: expat_68k.c expat_68k.h expat_base.h

libs/expat_68k_handler_stubs.o: expat_68k_handler_stubs.c expat_68k.h

libs/expat_vectors.o: expat_vectors.c

libs/launch.o: launch.c

#############################################################################

libs/xmlwf: libs/xmlwf.o libs/xmlfile.o libs/codepage.o libs/readfilemap.o libs/launch.o
	$(CC) -mcrt=newlib $^ -o $@
	$(STRIP) $(STRIPFLAGS) $@

libs/xmlwf.o: xmlwf.c

libs/xmlfile.o: xmlfile.c

libs/codepage.o: codepage.c

libs/readfilemap.o: readfilemap.c

#############################################################################

libs/elements: libs/elements.o libs/launch.o
	$(CC) -mcrt=newlib $^ -o $@
	$(STRIP) $(STRIPFLAGS) $@

libs/elements.o: elements.c

#############################################################################

libs/outline: libs/outline.o libs/launch.o
	$(CC) -mcrt=newlib $^ -o $@
	$(STRIP) $(STRIPFLAGS) $@

libs/outline.o: outline.c

#############################################################################

libs/runtests: libs/runtests.o libs/chardata.o libs/minicheck.o libs/launch.o
	$(CC) -mcrt=newlib $^ -o $@

libs/chardata.o: chardata.c chardata.h

libs/minicheck.o: minicheck.c minicheck.h

libs/runtests.o: runtests.c chardata.h

#############################################################################

libs/benchmark: libs/benchmark.o libs/launch.o
	$(CC) -mcrt=newlib $^ -o $@

libs/benchmark.o: benchmark.c

#############################################################################

so/libexpat.so: so/xmlparse.o so/xmltok.o so/xmlrole.o
	$(CC) -mcrt=newlib -shared -o $@ $^
	protect $@ -e

so/xmlparse.o: xmlparse.c expat.h xmlrole.h xmltok.h \
	expat_external.h internal.h amigaconfig.h

so/xmlrole.o: xmlrole.c ascii.h xmlrole.h expat_external.h \
	internal.h amigaconfig.h

so/xmltok.o: xmltok.c xmltok_impl.c xmltok_ns.c ascii.h asciitab.h \
	iasciitab.h latin1tab.h nametab.h utf8tab.h xmltok.h xmltok_impl.h \
	expat_external.h internal.h amigaconfig.h

#############################################################################

so/xmlwf: newlib/xmlwf.o newlib/xmlfile.o newlib/codepage.o newlib/readfilemap.o
	$(CC) -mcrt=newlib -use-dynld $^ -o $@ -Lso -lexpat
	$(STRIP) $(STRIPFLAGS) $@

#############################################################################

so/elements: newlib/elements.o
	$(CC) -mcrt=newlib -use-dynld $^ -o $@ -Lso -lexpat
	$(STRIP) $(STRIPFLAGS) $@

#############################################################################

so/outline: newlib/outline.o
	$(CC) -mcrt=newlib -use-dynld $^ -o $@ -Lso -lexpat
	$(STRIP) $(STRIPFLAGS) $@

#############################################################################

so/runtests: newlib/runtests.o newlib/chardata.o newlib/minicheck.o
	$(CC) -mcrt=newlib -use-dynld $^ -o $@ -Lso -lexpat

#############################################################################

so/benchmark: newlib/benchmark.o
	$(CC) -mcrt=newlib -use-dynld $^ -o $@ -Lso -lexpat

#############################################################################

clib2/%.o: %.c
	$(CC) -mcrt=clib2 $(CFLAGS) -I../lib -c $< -o $@

newlib/%.o: %.c
	$(CC) -mcrt=newlib $(CFLAGS) -I../lib -c $< -o $@

libs/%.o: %.c
	$(CC) -mcrt=newlib $(CFLAGS) -D__USE_INLINE__ -I. -Iinclude -Iinclude/libraries -I../lib -c $< -o $@

so/%.o: %.c
	$(CC) -mcrt=newlib $(CFLAGS) -fPIC -I../lib -c $< -o $@
