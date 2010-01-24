MAINTAINERCLEANFILES = aclocal.m4 config.guess config.h config.log \
			   config.status config.sub configure depcomp
			   install-sh libtool ltmain.sh Makefile Makefile.in
			   missing mkinstalldirs stamp-h

include_HEADERS = lrdf.h lrdf_types.h

EXTRA_DIST = autogen.sh lrdf.i

SUBDIRS = src

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = lrdf.pc

python: lrdf.i
	swig -python lrdf.i
	gcc -c src/lrdf.c src/lrdf_multi.c src/md5.c lrdf_wrap.c -I/usr/include/python2.3/
	ld -shared lrdf_multi.o lrdf.o lrdf_wrap.o md5.o @RAPTOR_LIBS@ -o _lrdf.so
