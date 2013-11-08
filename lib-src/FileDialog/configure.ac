#
# FileDialog
#

AC_PREREQ([2.59])
AC_INIT([FileDialog],[1.0],[audacity.sourceforge.net],[FileDialog])
AC_CONFIG_AUX_DIR([autotools])
AC_CONFIG_MACRO_DIR([m4])

AM_INIT_AUTOMAKE([1.11 dist-xz foreign no-dist-gzip subdir-objects -Wall])
AM_MAINTAINER_MODE([disable])

#
# Checks for programs.
#
AC_PROG_CXX
AM_PROG_AR

LT_INIT

#
# Checks for libraries.
#
AC_HEADER_STDC

#
# Check for debug
#
AC_ARG_ENABLE(static-wx,
            [AS_HELP_STRING([--enable-static-wx],[link wx statically (default=no)])],
            static_wx_preference="--static=$enableval",
            static_wx_preference="")

AC_ARG_ENABLE(unicode,
            [AS_HELP_STRING([--enable-unicode],[enable unicode support (default=no)])],
            unicode_preference="--unicode=$enableval",
            unicode_preference="")

AC_ARG_ENABLE(debug,
            [AS_HELP_STRING([--enable-debug],[enable debug support (default=none)])],
            debug_preference="--debug=$enableval",
            debug_preference="")

AC_ARG_WITH(wx-version,
            [AS_HELP_STRING([--with-wx-version],[override default wxWidgets version [2.6,2.8]])],
            wx_preference="--version=$withval",
            wx_preference="")

AC_ARG_WITH(wx-config,
            [AS_HELP_STRING([--with-wx-config],[override default wxWidgets config script])],
            wx_config="$withval",
            wx_config="")

dnl wxWidgets -- we assume that if wx-config is found, wxWidgets is successfully installed.
AC_PATH_PROG(WX_CONFIG, wx-config, no, $PATH:/usr/local/bin )
if [[ "$WX_CONFIG" = "no" ]] ; then
   AC_MSG_ERROR("Could not find wx-config: is wxWidgets installed? is wx-config in your path?")
fi

dnl Gather wx arguments

wxconfigargs="$static_wx_preference $unicode_preference $debug_preference $wx_preference"
WX_CXXFLAGS=$($WX_CONFIG $wxconfigargs --cxxflags)
WX_LIBS=""
AC_SUBST([WX_CXXFLAGS])
AC_SUBST([WX_LIBS])

PKG_CHECK_MODULES(GTK, gtk+-2.0, have_gtk="yes", have_gtk="no")

dnl OS-specific configuration

AC_CANONICAL_HOST

IMPLEMENTATION="generic"
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
      if test "$have_gtk" = "yes"; then
         AC_DEFINE(HAVE_GTK, 1, [Define to 1 if GTK+ is available.])
         IMPLEMENTATION="gtk"
      fi
   ;;
esac

AC_SUBST([IMPLEMENTATION])
AM_CONDITIONAL([GENERIC], test "$IMPLEMENTATION" = "generic")
AM_CONDITIONAL([GTK], test "$IMPLEMENTATION" = "gtk")
AM_CONDITIONAL([MAC], test "$IMPLEMENTATION" = "mac")
AM_CONDITIONAL([WINDOWS], test "$IMPLEMENTATION" = "win")

echo "Implementation to use: $IMPLEMENTATION"

AC_CONFIG_HEADERS([config.h])
AC_CONFIG_FILES([FileDialogPrivate.h Makefile])

#
# Write it all out
#
AC_OUTPUT
