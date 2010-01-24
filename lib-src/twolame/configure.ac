
dnl Require autoconf version >= 2.57
AC_PREREQ(2.57)


dnl ############# Initialization

AC_INIT([twolame], [0.3.12], [twolame-discuss@lists.sourceforge.net])

AC_CONFIG_SRCDIR(libtwolame/twolame.h)
AC_CONFIG_AUX_DIR(build)
dnl we have some extra m4 macros to point at
AC_CONFIG_MACRO_DIR([build/m4])
AC_CANONICAL_SYSTEM

dnl Version 1.7 of automake is recommended
AM_INIT_AUTOMAKE(1.7)
dnl Audacity policy: don't enable automatic rebuild of configure et al if 
dnl sources change
AM_MAINTAINER_MODE([enabled])

AM_CONFIG_HEADER(build/config.h)



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
AC_PROG_INSTALL
AC_PROG_LN_S

AC_PROG_LIBTOOL
AC_C_BIGENDIAN
AC_C_INLINE


dnl ############# Configure Arguments

AC_ARG_ENABLE(debug,
	  [  --enable-debug=[no/yes]   turn on debugging],
	  [ Debugging="Enabled" 
		AC_DEFINE(DEBUG, 1, [ Define if debugging is enabled. ]) ],
	  [ Debugging="Disabled" ]
)

AC_ARG_ENABLE(programs, 
      [AS_HELP_STRING([--enable-programs],
		[compile front-end programs as well as libtwolame [default=yes]])],
	  FRONTEND_ARG=$enableval,
	  FRONTEND_ARG="yes")

BUILD_FRONTEND="no"
if test "x$FRONTEND_ARG" = "xyes" ; then
	BUILD_FRONTEND="yes"
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

dnl we only need to do this test if we are building front-ends
if test "x$BUILD_FRONTEND" = "xyes"; then
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
fi


dnl ############## Header Checks

AC_HEADER_STDC
AC_CHECK_HEADERS(malloc.h assert.h unistd.h inttypes.h)
AC_CHECK_HEADER(getopt.h, 
	[ HAVE_GETOPT_H="yes" ],
	[ HAVE_GETOPT_H="no"
	  AC_MSG_WARN([getopt.h is unavailable on your system]) ]
)



dnl ############## Compiler and Linker Flags

CFLAGS="$CFLAGS -std=c99 -Wunused -Wall"
LDFLAGS="$LDFLAGS"

# If debugging is enabled then make warnings errors
if test "$Debugging" = "Enabled"; then
	CFLAGS="$CFLAGS -g -Werror -pedantic"
else
	# Optimize flag. 3 is about as high as you can sanely go with GCC3.2.
	CFLAGS="$CFLAGS -O3"
fi



dnl ############## Only compile full frontend if sndfile is available, 
dnl only compile anything if it wasn't asked to turn it off

if test "x$BUILD_FRONTEND" = "xyes" ; then
	STWOLAME_BIN="stwolame${EXEEXT}"	;dnl build simple frontend - no deps
	TWOLAME_BIN=""	;dnl don't build full unless we have it's deps
	if test "x$HAVE_SNDFILE" = "xyes"; then
		if test "$HAVE_GETOPT_H" = "yes"; then
			TWOLAME_BIN="twolame${EXEEXT}"
		else
			AC_MSG_WARN([Not building twolame frontend because getopt.h is missing.])
		fi
	else 
		AC_MSG_WARN([Not building twolame frontend because libsndfile is missing.])
	fi
else
	TWOLAME_BIN=""	;dnl don't build
	STWOLAME_BIN="" ;dnl either binary to simplify build
fi

AC_SUBST(TWOLAME_BIN)
AC_SUBST(STWOLAME_BIN)


dnl ############## Final Output

AC_OUTPUT(
	Makefile \
	twolame.pc \
	doc/Makefile \
	libtwolame/Makefile \
	frontend/Makefile \
	simplefrontend/Makefile \
)
