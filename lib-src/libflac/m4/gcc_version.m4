dnl @synopsis XIPH_GCC_VERSION
dnl
dnl Find the version of gcc.
dnl @version 1.0	Nov 05 2007
dnl @version 1.1	Mar 10 2013
dnl @author Erik de Castro Lopo <erikd AT mega-nerd DOT com>
dnl
dnl Permission to use, copy, modify, distribute, and sell this file for any
dnl purpose is hereby granted without fee, provided that the above copyright
dnl and this permission notice appear in all copies.  No representations are
dnl made about the suitability of this software for any purpose.  It is
dnl provided "as is" without express or implied warranty.
dnl

AC_DEFUN([XIPH_GCC_VERSION],
[
if test "x$ac_cv_c_compiler_gnu" = "xyes" ; then

	AC_MSG_CHECKING([for version of $CC])
	GCC_VERSION=`$CC -dumpversion`
	AC_MSG_RESULT($GCC_VERSION)

	GCC_MAJOR_VERSION=`echo $GCC_VERSION | cut -d. -f 1`
	GCC_MINOR_VERSION=`echo $GCC_VERSION | cut -d. -f 2`
	fi

AC_SUBST(GCC_VERSION)
AC_SUBST(GCC_MAJOR_VERSION)
AC_SUBST(GCC_MINOR_VERSION)

])# XIPH_GCC_VERSION
