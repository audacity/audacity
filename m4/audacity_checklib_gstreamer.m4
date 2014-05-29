dnl add license?
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_gstreamer.m4 serial 3

AC_DEFUN([AUDACITY_CHECKLIB_GSTREAMER], [

   AC_ARG_WITH(gstreamer,
               [AS_HELP_STRING([--with-gstreamer],
                               [include GStreamer import/export support])],
               GSTREAMER_ARGUMENT=$withval,
               GSTREAMER_ARGUMENT="unspecified")

   dnl see if GStreamer is installed on the system

   PKG_CHECK_MODULES(GSTREAMER, gstreamer-1.0 gstreamer-app-1.0,
                     GSTREAMER_SYSTEM_AVAILABLE="yes",
                     GSTREAMER_SYSTEM_AVAILABLE="no")

   if test "$GSTREAMER_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([GStreamer libraries are available as system libraries])
   else
      AC_MSG_NOTICE([GStreamer libraries are NOT available as system libraries])
   fi

   GSTREAMER_LOCAL_AVAILABLE="no"
])

AC_DEFUN([AUDACITY_CONFIG_GSTREAMER], [

   AC_SUBST([GSTREAMER_CFLAGS])
   AC_SUBST([GSTREAMER_LIBS])

   AM_CONDITIONAL([USE_GSTREAMER], [test "$GSTREAMER_USE_SYSTEM" = yes])

   if test "$GSTREAMER_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_GSTREAMER, 1,
                [Define if the GStreamer is present])
   fi
])
