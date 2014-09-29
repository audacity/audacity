dnl @synopsis OCTAVE_MKOCTFILE_VERSION
dnl
dnl Find the version of mkoctfile.
dnl @version 1.0	Aug 23 2007
dnl @author Erik de Castro Lopo <erikd AT mega-nerd DOT com>
dnl
dnl Permission to use, copy, modify, distribute, and sell this file for any 
dnl purpose is hereby granted without fee, provided that the above copyright 
dnl and this permission notice appear in all copies.  No representations are
dnl made about the suitability of this software for any purpose.  It is 
dnl provided "as is" without express or implied warranty.
dnl

AC_DEFUN([OCTAVE_MKOCTFILE_VERSION],
[


AC_ARG_WITH(mkoctfile,
	AC_HELP_STRING([--with-mkoctfile], [choose the mkoctfile version]),
	[ with_mkoctfile=$withval ])

test -z "$with_mkoctfile" && with_mkoctfile=mkoctfile

AC_CHECK_PROG(HAVE_MKOCTFILE,$with_mkoctfile,yes,no)

if test "x$ac_cv_prog_HAVE_MKOCTFILE" = "xyes" ; then
	MKOCTFILE=$with_mkoctfile

	AC_MSG_CHECKING([for version of $MKOCTFILE])
	MKOCTFILE_VERSION=`$with_mkoctfile --version 2>&1 | sed 's/mkoctfile, version //g'`
	AC_MSG_RESULT($MKOCTFILE_VERSION)
	fi

AC_SUBST(MKOCTFILE)
AC_SUBST(MKOCTFILE_VERSION)

])# OCTAVE_MKOCTFILE_VERSION

