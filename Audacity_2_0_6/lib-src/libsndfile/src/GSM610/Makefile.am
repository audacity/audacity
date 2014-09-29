## Process this file with automake to produce Makefile.in

EXTRA_DIST = README COPYRIGHT ChangeLog

noinst_HEADERS = gsm.h config.h gsm610_priv.h
noinst_LTLIBRARIES = libgsm.la

CFILES = add.c decode.c gsm_decode.c gsm_encode.c long_term.c preprocess.c \
	short_term.c code.c gsm_create.c gsm_destroy.c gsm_option.c lpc.c rpe.c table.c

libgsm_la_SOURCES = $(CFILES) $(noinst_HEADERS)

# Disable autoheader. 
AUTOHEADER=echo


