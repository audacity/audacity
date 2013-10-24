## Process this file with automake to produce Makefile.in

INCLUDES = -I../lib -I$(top_srcdir)/include @OGG_CFLAGS@

EXTRA_PROGRAMS = latticebuild latticetune huffbuild distribution
CLEANFILES = $(EXTRA_PROGRAMS)

AM_LDFLAGS = -static
LDADD = ../lib/libvorbis.la

latticebuild_SOURCES = latticebuild.c vqgen.c bookutil.c\
	vqgen.h bookutil.h localcodebook.h
latticetune_SOURCES = latticetune.c vqgen.c bookutil.c\
	vqgen.h bookutil.h localcodebook.h
huffbuild_SOURCES = huffbuild.c vqgen.c bookutil.c\
	vqgen.h bookutil.h localcodebook.h
distribution_SOURCES = distribution.c bookutil.c\
	bookutil.h localcodebook.h

vqs_files = 16.vqs 16u.vqs 44c-1.vqs 44c0.vqs 44c1.vqs 44c2.vqs \
	44c3.vqs 44c4.vqs 44c5.vqs 44c6.vqs 44c7.vqs 44c8.vqs 44c9.vqs \
	44u0.vqs 44u1.vqs 44u2.vqs 44u3.vqs 44u4.vqs 44u5.vqs 44u6.vqs \
	44u7.vqs 44u8.vqs 44u9.vqs 8.vqs 8u.vqs floor_11.vqs floor_22.vqs \
	floor_44.vqs 44p-1.vqs 44p0.vqs 44p1.vqs 44p2.vqs 44p3.vqs 44p4.vqs \
	44p5.vqs 44p6.vqs 44p7.vqs 44p8.vqs 44p9.vqs  

EXTRA_DIST = $(vqs_files) make_floor_books.pl make_residue_books.pl \
	metrics.c

debugvq:
	$(MAKE) vq CFLAGS="@DEBUG@"

profilevq:
	$(MAKE) vq CFLAGS="@PROFILE@"

vq: 
	$(MAKE) $(EXTRA_PROGRAMS) 
