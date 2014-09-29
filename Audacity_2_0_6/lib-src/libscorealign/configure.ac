dnl
dnl scorealign configure.in script - version not using libtool
dnl
dnl Joshua Haberman
dnl Dominic Mazzoni
dnl Richard Ash
dnl

dnl
dnl Instructions: to create "configure" from "configure.in", run:
dnl aclocal
dnl autoconf

dnl Process this file with autoconf to produce a configure script.

dnl Require autoconf >= 2.61
AC_PREREQ(2.61)

dnl Init autoconf and automake
AC_INIT([scorealign],[0.1])
dnl check that we have some source code
AC_CONFIG_SRCDIR([scorealign.h])
dnl keep autools files in a subdirectory
AC_CONFIG_AUX_DIR(autotools)
# which also contains a subdir containing macros
AC_CONFIG_MACRO_DIR([autotools/m4])
dnl set up automake
AM_INIT_AUTOMAKE([-Wall foreign])
dnl Audacity policy: don't enable automatic rebuild of configure et al if 
dnl sources change
AM_MAINTAINER_MODE([disable])

dnl -------------------------------------------------------
dnl Checks for programs.
dnl -------------------------------------------------------
dnl save $CFLAGS etc. since AC_PROG_CC likes to insert "-g -O2"
dnl if $CFLAGS is blank and it finds GCC
cflags_save="$CFLAGS"
cppflags_save="$CPPFLAGS"
cxxflags_save="$CXXFLAGS"
AC_PROG_CC
AC_LANG([C++])
AC_PROG_CXX
AC_PROG_CXXCPP
CFLAGS="$cflags_save"
CPPFLAGS="$cppflags_save"
CXXFLAGS="$cxxflags_save"
AM_PROG_AR

dnl we will need an "install" program to be available
AC_PROG_INSTALL
dnl and ranlib for static libraries
AC_PROG_RANLIB

dnl Make the install prefix available to the program so that the pkg-config file
dnl can be created correctly
AC_PREFIX_DEFAULT(/usr/local)
if [[ $prefix = "NONE" ]] ; then
  AC_DEFINE(INSTALL_PREFIX, "/usr/local",
      [define as prefix where Audacity is installed])
else
  AC_DEFINE_UNQUOTED(INSTALL_PREFIX, "$prefix")
fi

dnl Build Options

AC_ARG_ENABLE(debug,
            [AS_HELP_STRING([--enable-debug],
                            [enable debug support [default=no]])],
            debug_preference="$enableval",
            debug_preference="no")

dnl ----------------------------------------------------
dnl If user asked for debug, put debug in compiler flags
dnl ----------------------------------------------------

if test x"$debug_preference" = "xyes" ; then
   dnl we want debuging on
   AC_MSG_NOTICE([Adding -g for debugging to CFLAGS and CXXFLAGS ...])
   CFLAGS="${CFLAGS} -g "
   CXXFLAGS="${CXXFLAGS} -g "
fi

dnl --------------------------------------------------------------------------
dnl We would like warnings enabled on the builds, but different compilers need
dnl different options for these. This bit tries to work out what flags we 
dnl should add to the compiler we are using.
dnl --------------------------------------------------------------------------

dnl Strict prototypes flags for C (only C because doesn't work for C++)
AX_CFLAGS_STRICT_PROTOTYPES(CFLAGS)

dnl Sensible warnings for C
AX_CFLAGS_WARN_ALL(wall_flags)
CFLAGS="${CFLAGS} $wall_flags"
dnl try and use it on C++ as well
AX_CXX_CHECK_FLAG([$wall_flags], [[int foo;]], [[foo = 1;]], cxx_does_wall="yes", cxx_does_wall="no")
if test "x$cxx_does_wall" = "xyes" ; then
   dnl can use all warnings flag on the C++ compiler
   CXXFLAGS="${CXXFLAGS} $wall_flags"
fi
AX_CXXCPP_CHECK_FLAG([$wall_flags], [[int foo;]], [[foo = 1;]], cpp_does_wall="yes", cpp_does_wall="no")
if test "x$cpp_does_wall" = "xyes" ; then
   dnl can use all warnings flag on the C++ pre-processor
   CPPFLAGS="${CPPFLAGS} $wall_flags"
fi

dnl --- check for required libraries ---
dnl system libraries
AC_CHECK_FUNCS([strchr memmove])
AC_FUNC_MALLOC
AC_HEADER_STDBOOL

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS([stdlib.h string.h])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T

AC_CONFIG_FILES([Makefile scorealign.pc])

AC_OUTPUT

echo ""
echo "Run 'configure --help' for an explanation of the possible options,"
echo "otherwise run 'make' to build scorealign."

dnl Indentation settings for Vim and Emacs and unique identifier for Arch, a
dnl version control system. Please do not modify past this point.
# Local Variables:
# c-basic-offset: 3
# indent-tabs-mode: nil
# End:
#
# vim: et sts=3 sw=3
