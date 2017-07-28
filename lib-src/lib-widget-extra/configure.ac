dnl
dnl libwidgetextra configure.ac script
dnl
dnl Joshua Haberman
dnl Dominic Mazzoni
dnl Richard Ash
dnl

dnl
dnl Instructions: to create "configure" from "configure.ac", run: autoreconf
dnl

dnl Require autoconf >= 2.60
AC_PREREQ([2.60])

dnl Init autoconf
AC_INIT([libwidgetextra], [1.0])
dnl check we have some source (are in the right directory)
AC_CONFIG_SRCDIR([NonGuiThread.h])
AC_CONFIG_AUX_DIR([autotools])
dnl we have some extra macros in m4/
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.11 dist-xz foreign no-dist-gzip -Wall])
AM_MAINTAINER_MODE([disable])

AM_PROG_AR
LT_INIT

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

dnl Build Options

AC_ARG_ENABLE(debug,
            [AS_HELP_STRING([--enable-debug],
                            [enable debug support [default=no]])],
            debug_preference="$enableval",
            debug_preference="no")

AC_ARG_WITH(wx-version,
            [AS_HELP_STRING([--with-wx-version],
                           [select wxWidgets version (if both installed) [3.1,]])],
           wx_preference="--version=$withval",
           wx_preference="")
dnl Right now only support wx 3.1

dnl ----------------------------------------------------
dnl If user asked for debug, put debug in compiler flags
dnl ----------------------------------------------------

if test x"$debug_preference" = "xyes" ; then
   dnl we want debuging on
   AC_MSG_NOTICE([Adding -g for debugging to CFLAGS and CXXFLAGS ...])
   CFLAGS="${CFLAGS} -g -DwxDEBUG_LEVEL=1"
   CXXFLAGS="${CXXFLAGS} -g -DwxDEBUG_LEVEL=1"
else
   CFLAGS="${CFLAGS} -DwxDEBUG_LEVEL=0"
   CXXFLAGS="${CXXFLAGS} -DwxDEBUG_LEVEL=0"
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

dnl wxWidgets -- we assume that if wx-config is found, wxWidgets is successfully installed.
AC_PATH_PROGS(WX_CONFIG, wx-config wx-config-3.1, no, $PATH:/usr/local/bin )
if [[ "$WX_CONFIG" = "no" ]] ; then
   AC_MSG_ERROR("Could not find wx-config: is wxWidgets installed? is wx-config in your path?")
fi

dnl --- Check that the wx version is at least 3.1.x---

if test "x$debug_preference" = "xyes"; then
   dnl want debug wx as well
   wxconfigargs="--debug=yes"
else
   dnl normal wx
   wxconfigargs=""
fi

dnl more things we always pass to wx-config
wxconfigargs="$static_wx_preference $unicode_preference $wxconfigargs $wx_preference"

wx_version=`${WX_CONFIG} $wxconfigargs --version`

AC_MSG_NOTICE([Checking that the chosen version of wxWidgets is 3.1.x (or 3.0.x)])

case "${wx_version}" in
  3.1.* | 3.0.*)
    echo "Great, you're using wxWidgets ${wx_version}!"
    ;;
  *)
  wx_list=`${WX_CONFIG} --list`
  AC_MSG_ERROR([Unable to locate a suitable configuration of wxWidgets v3.0.x or higher.
The currently available configurations are listed below.  If necessary, either
install the package for your distribution or download the latest version of
wxWidgets
from http://wxwidgets.org.
${wx_list}])
esac

dnl Gather wx arguments

WX_CXXFLAGS=$($WX_CONFIG $wxconfigargs --cxxflags)
WX_LIBS=$($WX_CONFIG $wxconfigargs --libs)

dnl Enable C++ 11 support. Use gnu++11 on GCC since wxWidgets uses extensions
if "${CXX}" -v 2>&1 | grep -q '^gcc version'; then
    WX_CXXFLAGS="${WX_CXXFLAGS} --std=gnu++11"
else
    WX_CXXFLAGS="${WX_CXXFLAGS} --std=c++11"
fi

AC_SUBST([WX_CXXFLAGS])
AC_SUBST([WX_LIBS])

dnl Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST
AC_TYPE_SIZE_T

AC_CONFIG_FILES([libwidgetextra.pc Makefile])

AC_OUTPUT

echo ""
echo "Run 'configure --help' for an explanation of the possible options,"
echo "otherwise run 'make' to build libwidgetextra."
