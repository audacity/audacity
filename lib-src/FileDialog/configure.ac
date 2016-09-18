#
# FileDialog
#

AC_PREREQ([2.59])
AC_INIT([FileDialog],[1.0],[feedback@audacityteam.org],[FileDialog])
AC_CONFIG_AUX_DIR([autotools])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.11 dist-xz foreign no-dist-gzip subdir-objects -Wall])
AM_MAINTAINER_MODE([disable])

#
# We use C++
#
AC_LANG_CPLUSPLUS

#
# Checks for programs.
#
AC_PROG_CXX
AM_PROG_AR
AC_PROG_OBJCXX

LT_INIT

#
# Checks for libraries.
#
AC_HEADER_STDC

#
# Check for debug
#
AC_ARG_ENABLE(debug,
            [AS_HELP_STRING([--enable-debug],[enable debug support (default=none)])],
            debug_preference="yes",
            debug_preference="no")

AC_ARG_WITH(wx-version,
            [AS_HELP_STRING([--with-wx-version],[override default wxWidgets version [2.6,2.8]])],
            wx_preference="--version=$withval",
            wx_preference="")

AC_ARG_WITH(wx-config,
            [AS_HELP_STRING([--with-wx-config],[override default wxWidgets config script])],
            wx_config="$withval",
            wx_config="")

dnl wxWidgets -- we assume that if wx-config is found, wxWidgets is successfully installed.
AC_PATH_PROGS(WX_CONFIG, wx-config wx-config-3.0, no, $PATH:/usr/local/bin )
if [[ "$WX_CONFIG" = "no" ]] ; then
   AC_MSG_ERROR("Could not find wx-config: is wxWidgets installed? is wx-config in your path?")
fi

dnl Gather wx arguments

wxconfigargs="$wx_preference"
WX_CXXFLAGS=$($WX_CONFIG $wxconfigargs --cxxflags)
WX_LIBS=""
AC_SUBST([WX_CXXFLAGS])
AC_SUBST([WX_LIBS])

if test x"$debug_preference" = "xyes" ; then
   dnl we want debuging on
   AC_MSG_NOTICE([Adding -g for debugging to CFLAGS and CXXFLAGS ...])
   CFLAGS="${CFLAGS} -g -DwxDEBUG_LEVEL=1"
   CXXFLAGS="${CXXFLAGS} -g -DwxDEBUG_LEVEL=1"
else
   CFLAGS="${CFLAGS} -DwxDEBUG_LEVEL=0"
   CXXFLAGS="${CXXFLAGS} -DwxDEBUG_LEVEL=0"
fi

AC_MSG_CHECKING([for gtk3 use in wxWidgets])
CPPFLAGS="${WX_CXXFLAGS}"
AC_EGREP_CPP(wxWidgets built with GTK3,
[#include <wx/wx.h>
#if defined(__WXGTK3__)
   wxWidgets built with GTK3
#endif
], enable_gtk3=yes, enable_gtk3=no)

if [[ "$enable_gtk3" = yes ]]; then
   gtk_version=gtk+-3.0
   AC_MSG_RESULT([yes])
else
   gtk_version=gtk+-2.0
   AC_MSG_RESULT([no])
fi

AC_SUBST(HAVE_GTK)
PKG_CHECK_MODULES(GTK, $gtk_version, have_gtk="yes", have_gtk="no")

dnl OS-specific configuration

AC_CANONICAL_HOST

IMPLEMENTATION=""
case "${host_os}" in
   darwin*)
      dnl Mac OS X configuration
      IMPLEMENTATION="mac"
   ;;

   cygwin*)
      dnl Windows/CygWin configuration
      IMPLEMENTATION="win"
   ;;

   *)
      if [[ "$have_gtk" = "yes" ]]
      then
         AC_DEFINE(HAVE_GTK, 1, [Define if GTK is available])
         CPPFLAGS="$CPPFLAGS $GTK_CFLAGS"
         LIBS="$LIBS $GTK_LIBS"
         IMPLEMENTATION="gtk"
      fi
   ;;
esac

AM_CONDITIONAL([GTK], test "$IMPLEMENTATION" = "gtk")
AM_CONDITIONAL([MAC], test "$IMPLEMENTATION" = "mac")
AM_CONDITIONAL([WINDOWS], test "$IMPLEMENTATION" = "win")

echo "Implementation to use: $IMPLEMENTATION"

AC_CONFIG_FILES([Makefile])

#
# Write it all out
#
AC_OUTPUT
