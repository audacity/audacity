#
# FileDialog
#

AC_INIT(FileDialog, 1.0, audacity.sourceforge.net, FileDialog)
AC_CONFIG_FILES([Makefile config.h])
AC_SUBST(EXTRADEPS)
AC_SUBST(EXTRAOBJS)
AC_SUBST(HAVE_GTK)

#
# Checks for programs.
#
AC_PROG_CXX
AC_PROG_RANLIB
AC_PATH_PROG(AR, ar, no)
if [[ $AR = "no" ]] ; then
    AC_MSG_ERROR("Could not find ar - needed to create a library");
fi

#
# Checks for libraries.
#
AC_HEADER_STDC

#
# Check for debug
#
AC_ARG_ENABLE(static,
            [AC_HELP_STRING([--enable-static-wx],
                            [link wx statically (default=no)])],
            static_wx_preference="--static=$enableval",
            static_wx_preference="")


AC_ARG_ENABLE(unicode,
            [AC_HELP_STRING([--enable-unicode],
                            [enable unicode support (default=no)])],
            unicode_preference="--unicode=$enableval",
            unicode_preference="")

AC_ARG_ENABLE(debug,
            [AC_HELP_STRING([--enable-debug],
                            [enable debug support (default=none)])],
            debug_preference="--debug=$enableval",
            debug_preference="")

AC_ARG_WITH(wx-version,
            [AC_HELP_STRING([--with-wx-version],
                            [override default wxWidgets version [2.6,2.8]])],
            wx_preference="--version=$withval",
            wx_preference="")

AC_ARG_WITH(wx-config,
            [AC_HELP_STRING([--with-wx-config],
                            [override default wxWidgets config script])],
            wx_config="$withval",
            wx_config="")

dnl wxWidgets -- we assume that if wx-config is found, wxWidgets is successfully installed.
AC_PATH_PROG(WX_CONFIG, wx-config, no, $PATH:/usr/local/bin )
if [[ "$WX_CONFIG" = "no" ]] ; then
   AC_MSG_ERROR("Could not find wx-config: is wxWidgets installed? is wx-config in your path?")
fi

dnl Gather wx arguments

CPPFLAGS="$CPPFLAGS `$WX_CONFIG $static_wx_preference $unicode_preference $debug_preference $wx_preference --cxxflags`"

dnl OS-specific configuration

AC_CANONICAL_HOST

case "${host_os}" in
   darwin*)
      dnl Mac OS X configuration
      EXTRADEPS="mac/FileDialogPrivate.h"
      EXTRAOBJS="mac/FileDialogPrivate.o"
   ;;

   cygwin*)
      dnl Windows/CygWin configuration
      EXTRADEPS="win/FileDialogPrivate.h"
      EXTRAOBJS="win/FileDialogPrivate.o"
   ;;

   *)
      dnl Unix configuration
      AM_PATH_GTK_2_0(2.4.0,
            have_gtk="yes",
            have_gtk="no")
      if [[ "$have_gtk" = "yes" ]]
      then
         CPPFLAGS="$CPPFLAGS $GTK_CFLAGS"
         EXTRADEPS="gtk/FileDialogPrivate.h gtk/private.h"
         EXTRAOBJS="gtk/FileDialogPrivate.o"
         HAVE_GTK=1
      else
         EXTRADEPS="generic/FileDialogPrivate.h"
         EXTRAOBJS="generic/FileDialogPrivate.o"
         HAVE_GTK=0
      fi
   ;;
esac

#
# Write it all out
#
AC_OUTPUT
