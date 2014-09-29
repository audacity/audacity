dnl Evaluate an expression in octave
dnl
dnl OCTAVE_EVAL(expr,var) -> var=expr
dnl
dnl Stolen from octave-forge

AC_DEFUN([OCTAVE_EVAL],
[
AC_MSG_CHECKING([for $1 in $OCTAVE])
$2=`TERM=;$OCTAVE -qfH --eval "disp($1)"`
AC_MSG_RESULT($$2)
AC_SUBST($2)
]) # OCTAVE_EVAL

dnl @synopsis AC_OCTAVE_VERSION
dnl
dnl Find the version of Octave.
dnl @version 1.0	Aug 23 2007
dnl @author Erik de Castro Lopo <erikd AT mega-nerd DOT com>
dnl
dnl Permission to use, copy, modify, distribute, and sell this file for any 
dnl purpose is hereby granted without fee, provided that the above copyright 
dnl and this permission notice appear in all copies.  No representations are
dnl made about the suitability of this software for any purpose.  It is 
dnl provided "as is" without express or implied warranty.
dnl

AC_DEFUN([AC_OCTAVE_VERSION],
[

AC_ARG_WITH(octave,
	AC_HELP_STRING([--with-octave], [choose the octave version]),
	[ with_octave=$withval ])

test -z "$with_octave" && with_octave=octave

AC_CHECK_PROG(HAVE_OCTAVE,$with_octave,yes,no)

if test "x$ac_cv_prog_HAVE_OCTAVE" = "xyes" ; then
	OCTAVE=$with_octave
	OCTAVE_EVAL(OCTAVE_VERSION,OCTAVE_VERSION)
	fi

AC_SUBST(OCTAVE)
AC_SUBST(OCTAVE_VERSION)

])# AC_OCTAVE_VERSION

dnl @synopsis AC_OCTAVE_CONFIG_VERSION
dnl
dnl Find the version of Octave.
dnl @version 1.0	Aug 23 2007
dnl @author Erik de Castro Lopo <erikd AT mega-nerd DOT com>
dnl
dnl Permission to use, copy, modify, distribute, and sell this file for any 
dnl purpose is hereby granted without fee, provided that the above copyright 
dnl and this permission notice appear in all copies.  No representations are
dnl made about the suitability of this software for any purpose.  It is 
dnl provided "as is" without express or implied warranty.
dnl

AC_DEFUN([AC_OCTAVE_CONFIG_VERSION],
[

AC_ARG_WITH(octave-config,
	AC_HELP_STRING([--with-octave-config], [choose the octave-config version]),
	[ with_octave_config=$withval ])

test -z "$with_octave_config" && with_octave_config=octave-config

AC_CHECK_PROG(HAVE_OCTAVE_CONFIG,$with_octave_config,yes,no)

if test "x$ac_cv_prog_HAVE_OCTAVE_CONFIG" = "xyes" ; then
	OCTAVE_CONFIG=$with_octave_config
	AC_MSG_CHECKING([for version of $OCTAVE_CONFIG])
	OCTAVE_CONFIG_VERSION=`$OCTAVE_CONFIG --version`
	AC_MSG_RESULT($OCTAVE_CONFIG_VERSION)
	fi

AC_SUBST(OCTAVE_CONFIG)
AC_SUBST(OCTAVE_CONFIG_VERSION)

])# AC_OCTAVE_CONFIG_VERSION

dnl @synopsis AC_OCTAVE_BUILD
dnl
dnl Check programs and headers required for building octave plugins.
dnl @version 1.0	Aug 23 2007
dnl @author Erik de Castro Lopo <erikd AT mega-nerd DOT com>
dnl
dnl Permission to use, copy, modify, distribute, and sell this file for any
dnl purpose is hereby granted without fee, provided that the above copyright
dnl and this permission notice appear in all copies.  No representations are
dnl made about the suitability of this software for any purpose.  It is
dnl provided "as is" without express or implied warranty.


AC_DEFUN([AC_OCTAVE_BUILD],
[

dnl Default to no.
OCTAVE_BUILD=no

AC_OCTAVE_VERSION
OCTAVE_MKOCTFILE_VERSION
AC_OCTAVE_CONFIG_VERSION

prog_concat="$ac_cv_prog_HAVE_OCTAVE$ac_cv_prog_HAVE_OCTAVE_CONFIG$ac_cv_prog_HAVE_MKOCTFILE"

if test "x$prog_concat" = "xyesyesyes" ; then
	if test "x$OCTAVE_VERSION" != "x$MKOCTFILE_VERSION" ; then
		AC_MSG_WARN([** Mismatch between versions of octave and mkoctfile. **])
		AC_MSG_WARN([** Octave libsndfile modules will not be built.       **])
	elif test "x$OCTAVE_VERSION" != "x$OCTAVE_CONFIG_VERSION" ; then
		AC_MSG_WARN([** Mismatch between versions of octave and octave-config. **])
		AC_MSG_WARN([** Octave libsndfile modules will not be built.           **])
	else
		case "$MKOCTFILE_VERSION" in
			2.*)
				AC_MSG_WARN([Octave version 2.X is not supported.])
				;;
			3.0.*)
				OCTAVE_DEST_ODIR=`$OCTAVE_CONFIG --oct-site-dir | sed 's%^/usr%${prefix}%'`
				OCTAVE_DEST_MDIR=`$OCTAVE_CONFIG --m-site-dir | sed 's%^/usr%${prefix}%'`

				OCTAVE_BUILD=yes
				;;
			*)
				AC_MSG_WARN([Octave version $MKOCTFILE_VERSION is not supported.])
				;;
				esac
		fi
	AC_MSG_RESULT([building octave libsndfile module... $OCTAVE_BUILD])
	fi

AC_SUBST(OCTAVE_DEST_ODIR)
AC_SUBST(OCTAVE_DEST_MDIR)

AC_SUBST(MKOCTFILE)

AM_CONDITIONAL(BUILD_OCTAVE_MOD, test "x$OCTAVE_BUILD" = xyes)

])# AC_OCTAVE_BUILD
