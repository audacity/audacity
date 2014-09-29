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
# GNU makefile fragment for building a library
#

%.debug.o %.release.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@
%.debug.o %.release.o : %.cc
	$(CCC) $(CFLAGS) -c $< -o $@
%.debug.o %.release.o : %.cpp
	$(CCC) $(CFLAGS) -c $< -o $@
%.debug.pic.o %.release.pic.o : %.c
	$(CC) $(CFLAGS) -fPIC -DPIC -c $< -o $@
%.debug.pic.o %.release.pic.o : %.cc
	$(CCC) $(CFLAGS) -fPIC -DPIC -c $< -o $@
%.debug.pic.o %.release.pic.o : %.cpp
	$(CCC) $(CFLAGS) -fPIC -DPIC -c $< -o $@
%.debug.i %.release.i : %.c
	$(CC) $(CFLAGS) -E $< -o $@
%.debug.i %.release.i : %.cc
	$(CCC) $(CFLAGS) -E $< -o $@
%.debug.i %.release.i : %.cpp
	$(CCC) $(CFLAGS) -E $< -o $@

%.debug.o %.release.o : %.s
ifeq ($(OS),Darwin)
	#$(CC) -c -arch $(PROC) -Wall -force_cpusubtype_ALL $< -o $@
	$(AS) -arch $(PROC) -force_cpusubtype_ALL $< -o $@
else
	$(AS) $< -o $@
endif
%.debug.pic.o %.release.pic.o : %.s
ifeq ($(OS),Darwin)
	#$(CC) -c -arch $(PROC) -Wall -force_cpusubtype_ALL $< -o $@
	$(AS) -arch $(PROC) -force_cpusubtype_ALL $< -o $@
else
	$(AS) $< -o $@
endif

%.debug.o : %.nasm
	$(NASM) -f elf -d OBJ_FORMAT_elf -i ia32/ -g $< -o $@
%.release.o : %.nasm
	$(NASM) -f elf -d OBJ_FORMAT_elf -i ia32/ $< -o $@
%.debug.pic.o : %.nasm
	$(NASM) -f elf -d OBJ_FORMAT_elf -i ia32/ -g $< -o $@
%.release.pic.o : %.nasm
	$(NASM) -f elf -d OBJ_FORMAT_elf -i ia32/ $< -o $@
