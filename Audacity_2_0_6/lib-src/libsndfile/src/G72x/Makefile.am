## Process this file with automake to produce Makefile.in

EXTRA_DIST = README README.original ChangeLog

noinst_HEADERS = g72x.h g72x_priv.h
noinst_LTLIBRARIES = libg72x.la

noinst_PROGRAMS = g72x_test

CFILES = g72x.c g721.c g723_16.c g723_24.c g723_40.c

libg72x_la_SOURCES = $(CFILES) $(noinst_HEADERS)

g72x_test_SOURCES = g72x_test.c
g72x_test_LDADD = ./libg72x.la -lm

check: g72x_test$(EXEEXT)
	./g72x_test$(EXEEXT) all

# Disable autoheader.
AUTOHEADER=echo

