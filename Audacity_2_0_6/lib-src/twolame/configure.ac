dnl Require autoconf version >= 2.60
AC_PREREQ(2.60)


dnl ############# Initialization

AC_INIT([twolame],[0.3.13],[twolame-discuss@lists.sourceforge.net])

AC_CONFIG_SRCDIR(libtwolame/twolame.h)
AC_CONFIG_AUX_DIR(build-scripts)
AC_CONFIG_MACRO_DIR(build-scripts)
AM_INIT_AUTOMAKE([1.10])

LT_PREREQ([2.2])
LT_INIT

AM_MAINTAINER_MODE



dnl ############# Library Version
dnl
dnl libtool version: current:revision:age
dnl
dnl If the library source code has changed at all since the last update, then
dnl increment revision (`c:r:a' becomes `c:r+1:a').
dnl
dnl If any interfaces have been added, removed, or changed since the last update,
dnl increment current, and set revision to 0.
dnl
dnl If any interfaces have been added since the last public release, then
dnl increment age.
dnl
dnl If any interfaces have been removed since the last public release, then set
dnl age to 0.
dnl 

TWOLAME_SO_VERSION=0:0:0
AC_SUBST(TWOLAME_SO_VERSION)



dnl ############# Compiler and tools Checks

AC_PROG_CC
AC_PROG_CC_C99
AC_PROG_INSTALL
AC_PROG_LN_S
AC_PROG_LIBTOOL

AC_C_BIGENDIAN
AC_C_INLINE
AC_C_CONST

if test "$USE_MAINTAINER_MODE" = yes; then
  AC_PATH_PROG([PATH_ASCIIDOC], [asciidoc])
  AC_PATH_PROG([PATH_DOXYGEN], [doxygen])
  AC_PATH_PROG([PATH_XMLTO], [xmlto])
fi



dnl ############## Check that types are the right size

AC_CHECK_SIZEOF(short)
AC_CHECK_SIZEOF(float)

if test "$ac_cv_sizeof_short" != "2"; then
	AC_MSG_ERROR([Size of short isn't 16-bits - please report this to $PACKAGE_BUGREPORT])
fi

if test "$ac_cv_sizeof_float" != "4"; then
	AC_MSG_ERROR([Size of float isn't 32-bits - please report this to $PACKAGE_BUGREPORT])
fi



dnl ############## Library Checks

AC_CHECK_LIB([m], [sqrt])
AC_CHECK_LIB([m], [lrintf])
AC_CHECK_LIB([mx], [powf])

PKG_CHECK_MODULES(SNDFILE, sndfile >= 1.0.0,
	[ HAVE_SNDFILE="yes" ],
	[ AC_CHECK_LIB( [sndfile], [sf_command],
		[ AC_CHECK_HEADER( [sndfile.h],
			[ HAVE_SNDFILE="yes"
			  SNDFILE_CFLAGS="" 
			  SNDFILE_LIBS="-lsndfile" ],
			[ AC_MSG_WARN([Can't find sndfile.h on your system]) ] ) ],
		[ AC_MSG_WARN([Can't find libsndfile library on your system]) ]
	) ]
)
			
AC_SUBST(SNDFILE_CFLAGS)
AC_SUBST(SNDFILE_LIBS)



dnl ############## Header Checks

AC_HEADER_STDC
AC_CHECK_HEADERS(malloc.h assert.h unistd.h inttypes.h)
AC_CHECK_HEADER(getopt.h, 
	[ HAVE_GETOPT_H="yes" ],
	[ HAVE_GETOPT_H="no"
	  AC_MSG_WARN([getopt.h is unavailable on your system]) ]
)



dnl ############## Only compile frontend if sndfile is available

TWOLAME_BIN=""
if test "x$HAVE_SNDFILE" = "xyes"; then
	if test "$HAVE_GETOPT_H" = "yes"; then
		TWOLAME_BIN="twolame${EXEEXT}"
	else
		AC_MSG_WARN([Not building twolame frontend because getopt.h is missing.])
	fi
else 
	AC_MSG_WARN([Not building twolame frontend because libsndfile is missing.])
fi
AC_SUBST(TWOLAME_BIN)



dnl ############## Compiler and Linker Flags

# If maintainer mode is enabled then make warnings errors
if test "$USE_MAINTAINER_MODE" = yes; then
	WARNING_CFLAGS="-Wall -Werror -pedantic -Wunused"
  WARNING_CFLAGS="$WARNING_CFLAGS -Wmissing-prototypes -Wmissing-declarations"
	AC_SUBST(WARNING_CFLAGS)
fi



dnl ############## Output files

AC_CONFIG_HEADERS([libtwolame/config.h])

AC_CONFIG_FILES([
	Makefile \
	twolame.pc \
	doc/Makefile \
	doc/html/Makefile \
	doc/html/Doxyfile \
	libtwolame/Makefile \
	frontend/Makefile \
	simplefrontend/Makefile \
	tests/Makefile \
])

AC_OUTPUT
