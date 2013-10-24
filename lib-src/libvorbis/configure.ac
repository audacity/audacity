dnl Process this file with autoconf to produce a configure script

dnl ------------------------------------------------
dnl Initialization and Versioning
dnl ------------------------------------------------


AC_INIT([libvorbis],[1.3.3],[vorbis-dev@xiph.org])

AC_CONFIG_SRCDIR([lib/mdct.c])

AC_CANONICAL_TARGET([])

AM_INIT_AUTOMAKE($PACKAGE_NAME,$PACKAGE_VERSION)
AM_MAINTAINER_MODE
AM_CONFIG_HEADER([config.h])

dnl Add parameters for aclocal
AC_SUBST(ACLOCAL_AMFLAGS, "-I m4")

dnl Library versioning
dnl - library source changed -> increment REVISION
dnl - interfaces added/removed/changed -> increment CURRENT, REVISION = 0
dnl - interfaces added -> increment AGE
dnl - interfaces removed -> AGE = 0

V_LIB_CURRENT=4
V_LIB_REVISION=6
V_LIB_AGE=4

VF_LIB_CURRENT=6
VF_LIB_REVISION=5
VF_LIB_AGE=3

VE_LIB_CURRENT=2
VE_LIB_REVISION=9
VE_LIB_AGE=0

AC_SUBST(V_LIB_CURRENT)
AC_SUBST(V_LIB_REVISION)
AC_SUBST(V_LIB_AGE)
AC_SUBST(VF_LIB_CURRENT)
AC_SUBST(VF_LIB_REVISION)
AC_SUBST(VF_LIB_AGE)
AC_SUBST(VE_LIB_CURRENT)
AC_SUBST(VE_LIB_REVISION)
AC_SUBST(VE_LIB_AGE)

dnl --------------------------------------------------  
dnl Check for programs
dnl --------------------------------------------------  

dnl save $CFLAGS since AC_PROG_CC likes to insert "-g -O2"
dnl if $CFLAGS is blank
cflags_save="$CFLAGS"
AC_PROG_CC
AC_PROG_CPP
CFLAGS="$cflags_save"

AC_C_INLINE

AC_LIBTOOL_WIN32_DLL
AC_PROG_LIBTOOL
AM_PROG_CC_C_O

dnl Check for doxygen
if test "x$enable_docs" = xyes; then
  AC_CHECK_PROG(HAVE_DOXYGEN, doxygen, true, false)
  if test $HAVE_DOXYGEN = "false"; then
    AC_MSG_WARN([*** doxygen not found, API documentation will not be built])
  fi
else
  HAVE_DOXYGEN=false
fi
AM_CONDITIONAL(HAVE_DOXYGEN,$HAVE_DOXYGEN)

dnl latex tools for the specification document
AC_ARG_ENABLE(docs,
	AC_HELP_STRING([--enable-docs], [build the documentation]))

if test "x$enable_docs" = xyes; then
  AC_CHECK_PROGS([PDFLATEX], pdflatex, [/bin/false])
  AC_CHECK_PROGS([HTLATEX], htlatex, [/bin/false])
  if test "x$PDFLATEX" = x/bin/false || test "x$HTLATEX" = x/bin/false; then
    enable_docs=no
    AC_MSG_WARN([Documentation will not be built!])
  fi
fi

AM_CONDITIONAL(BUILD_DOCS, [test "x$enable_docs" = xyes])

AC_ARG_ENABLE(examples, 
  AS_HELP_STRING([--enable-examples], [build the examples])) 
 	
AM_CONDITIONAL(BUILD_EXAMPLES, [test "x$enable_examples" = xyes]) 

dnl --------------------------------------------------
dnl Set build flags based on environment
dnl --------------------------------------------------

dnl Set some target options

cflags_save="$CFLAGS"
if test -z "$GCC"; then
	case $host in 
	*-*-irix*)
		dnl If we're on IRIX, we wanna use cc even if gcc 
		dnl is there (unless the user has overriden us)...
		if test -z "$CC"; then
			CC=cc
		fi
		DEBUG="-g -signed" 
		CFLAGS="-O2 -w -signed"
		PROFILE="-p -g3 -O2 -signed" ;;
	sparc-sun-solaris*)
		DEBUG="-v -g"
		CFLAGS="-xO4 -fast -w -fsimple -native -xcg92"
		PROFILE="-v -xpg -g -xO4 -fast -native -fsimple -xcg92 -Dsuncc" ;;
	*)
		DEBUG="-g"
		CFLAGS="-O"
		PROFILE="-g -p" ;;
        esac
else

	AC_MSG_CHECKING([GCC version])
	GCC_VERSION=`$CC -dumpversion`
	AC_MSG_RESULT([$GCC_VERSION])
	case $host in 
	*86-*-linux*)
		DEBUG="-g -Wall -Wextra -D_REENTRANT -D__NO_MATH_INLINES -fsigned-char"
		CFLAGS="-O20 -ffast-math -mno-ieee-fp -D_REENTRANT -fsigned-char"
#              	PROFILE="-Wall -Wextra -pg -g -O20 -ffast-math -D_REENTRANT -fsigned-char -fno-inline -static"
		PROFILE="-Wall -Wextra -pg -g -O20 -ffast-math -mno-ieee-fp -D_REENTRANT -fsigned-char -fno-inline"

		# glibc < 2.1.3 has a serious FP bug in the math inline header
		# that will cripple Vorbis.  Look to see if the magic FP stack
		# clobber is missing in the mathinline header, thus indicating
		# the buggy version

		AC_EGREP_CPP(log10.*fldlg2.*fxch,[
				#define __LIBC_INTERNAL_MATH_INLINES 1
			     	#define __OPTIMIZE__
				#include <math.h>
				],bad=maybe,bad=no)
		if test ${bad} = "maybe" ;then
		      AC_EGREP_CPP(log10.*fldlg2.*fxch.*st\([[0123456789]]*\),
					[
					#define __LIBC_INTERNAL_MATH_INLINES 1
				     	#define __OPTIMIZE__
					#include <math.h>
					],bad=no,bad=yes)
		fi
 		if test ${bad} = "yes" ;then
 AC_MSG_WARN([                                                        ])
 AC_MSG_WARN([********************************************************])
 AC_MSG_WARN([* The glibc headers on this machine have a serious bug *])
 AC_MSG_WARN([* in /usr/include/bits/mathinline.h  This bug affects  *])
 AC_MSG_WARN([* all floating point code, not just Ogg, built on this *])
 AC_MSG_WARN([* machine.  Upgrading to glibc 2.1.3 is strongly urged *])
 AC_MSG_WARN([* to correct the problem.  Note that upgrading glibc   *])
 AC_MSG_WARN([* will not fix any previously built programs; this is  *])
 AC_MSG_WARN([* a compile-time time bug.                             *])
 AC_MSG_WARN([* To work around the problem for this build of Ogg,    *])
 AC_MSG_WARN([* autoconf is disabling all math inlining.  This will  *])
 AC_MSG_WARN([* hurt Ogg performace but is necessary for an Ogg that *])
 AC_MSG_WARN([* will actually work.  Once glibc is upgraded, rerun   *])
 AC_MSG_WARN([* configure and make to build with inlining.           *])
 AC_MSG_WARN([********************************************************])
 AC_MSG_WARN([                                                        ])

  		CFLAGS=${OPT}" -D__NO_MATH_INLINES"
  		PROFILE=${PROFILE}" -D__NO_MATH_INLINES"
  		fi;;
        powerpc-*-linux*spe) 
 	        DEBUG="-g -Wall -Wextra -D_REENTRANT -D__NO_MATH_INLINES" 
 	        CFLAGS="-O3 -Wall -Wextra -ffast-math -mfused-madd -D_REENTRANT" 
 	        PROFILE="-pg -g -O3 -ffast-math -mfused-madd -D_REENTRANT";; 
	powerpc-*-linux*)
		DEBUG="-g -Wall -Wextra -D_REENTRANT -D__NO_MATH_INLINES"
		CFLAGS="-O3 -Wall -Wextra -ffast-math -mfused-madd -mcpu=750 -D_REENTRANT"
		PROFILE="-pg -g -O3 -ffast-math -mfused-madd -mcpu=750 -D_REENTRANT";;
	*-*-linux*)
		DEBUG="-g -Wall -Wextra -D_REENTRANT -D__NO_MATH_INLINES -fsigned-char"
		CFLAGS="-O20 -Wall -Wextra -ffast-math -D_REENTRANT -fsigned-char"
		PROFILE="-pg -g -O20 -ffast-math -D_REENTRANT -fsigned-char";;
	sparc-sun-*)
		sparc_cpu=""
		AC_MSG_CHECKING([if gcc supports -mv8])
		old_cflags="$CFLAGS"
		CFLAGS="$CFLAGS -mv8"
		AC_TRY_COMPILE(, [return 0;], [
			AC_MSG_RESULT([yes])
			sparc_cpu="-mv8"
		])
		CFLAGS="$old_cflags"
		DEBUG="-g -Wall -Wextra -D__NO_MATH_INLINES -fsigned-char $sparc_cpu"
		CFLAGS="-O20 -Wall -Wextra -ffast-math -D__NO_MATH_INLINES -fsigned-char $sparc_cpu"
		PROFILE="-pg -g -O20 -D__NO_MATH_INLINES -fsigned-char $sparc_cpu" ;;
	*-*-darwin*)
		DEBUG="-DDARWIN -fno-common -force_cpusubtype_ALL -Wall -g -O0 -fsigned-char"
		CFLAGS="-DDARWIN -fno-common -force_cpusubtype_ALL -Wall -g -O4 -ffast-math -fsigned-char"
		PROFILE="-DDARWIN -fno-common -force_cpusubtype_ALL -Wall -g -pg -O4 -ffast-math -fsigned-char";;
	*-*-os2*)
		# Use -W instead of -Wextra because gcc on OS/2 is an old version.
		DEBUG="-g -Wall -W -D_REENTRANT -D__NO_MATH_INLINES -fsigned-char"
		CFLAGS="-O20 -Wall -W -ffast-math -D_REENTRANT -fsigned-char"
		PROFILE="-pg -g -O20 -ffast-math -D_REENTRANT -fsigned-char";;
 	*)
		DEBUG="-g -Wall -Wextra -D__NO_MATH_INLINES -fsigned-char"
		CFLAGS="-O20 -Wall -Wextra -D__NO_MATH_INLINES -fsigned-char"
		PROFILE="-O20 -g -pg -D__NO_MATH_INLINES -fsigned-char" ;;
        esac

        AC_ADD_CFLAGS([-Wdeclaration-after-statement])
fi
CFLAGS="$CFLAGS $cflags_save"

dnl --------------------------------------------------
dnl Check for headers
dnl --------------------------------------------------

AC_CHECK_HEADER(memory.h,CFLAGS="$CFLAGS -DUSE_MEMORY_H",:)

dnl --------------------------------------------------
dnl Check for typedefs, structures, etc
dnl --------------------------------------------------

dnl none

dnl --------------------------------------------------
dnl Check for libraries
dnl --------------------------------------------------

AC_CHECK_LIB(m, cos, VORBIS_LIBS="-lm", VORBIS_LIBS="")
AC_CHECK_LIB(pthread, pthread_create, pthread_lib="-lpthread", :)

PKG_PROG_PKG_CONFIG

HAVE_OGG=no
if test "x$PKG_CONFIG" != "x"
then
  PKG_CHECK_MODULES(OGG, ogg >= 1.0, HAVE_OGG=yes, HAVE_OGG=no)
fi
if test "x$HAVE_OGG" = "xno"
then
  dnl fall back to the old school test
  XIPH_PATH_OGG(, AC_MSG_ERROR(must have Ogg installed!))
  libs_save=$LIBS
  LIBS="$OGG_LIBS $VORBIS_LIBS"
  AC_CHECK_FUNC(oggpack_writealign, , AC_MSG_ERROR(Ogg >= 1.0 required !))
  LIBS=$libs_save
fi

dnl --------------------------------------------------
dnl Check for library functions
dnl --------------------------------------------------

AC_FUNC_ALLOCA
AC_FUNC_MEMCMP

dnl --------------------------------------------------
dnl Do substitutions
dnl --------------------------------------------------

AC_SUBST(VORBIS_LIBS)
AC_SUBST(DEBUG)
AC_SUBST(PROFILE)
AC_SUBST(pthread_lib)

dnl The following line causes the libtool distributed with the source
dnl to be replaced if the build system has a more recent version.
AC_SUBST(LIBTOOL_DEPS)

AC_OUTPUT([
Makefile
m4/Makefile
lib/Makefile
lib/modes/Makefile
lib/books/Makefile
lib/books/coupled/Makefile
lib/books/uncoupled/Makefile
lib/books/floor/Makefile
doc/Makefile doc/vorbisfile/Makefile doc/vorbisenc/Makefile doc/libvorbis/Makefile
doc/Doxyfile
include/Makefile include/vorbis/Makefile
examples/Makefile
test/Makefile
vq/Makefile
libvorbis.spec
vorbis.pc
vorbisenc.pc
vorbisfile.pc
vorbis-uninstalled.pc
vorbisenc-uninstalled.pc
vorbisfile-uninstalled.pc
])
