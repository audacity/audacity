dnl Add Audacity license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libresample.m4 serial 3

AC_DEFUN([AUDACITY_CHECKLIB_LIBRESAMPLE], [

   AC_ARG_WITH(libresample,
               [AS_HELP_STRING([--with-libresample],
                               [use libresample for sample rate conversion])],
               LIBRESAMPLE_ARGUMENT=$withval,
               LIBRESAMPLE_ARGUMENT="unspecified")

   dnl see if libresample is installed on the system

   dnl ... but libresample isn't generally installed as a system library...

   LIBRESAMPLE_SYSTEM_AVAILABLE="no"

   dnl see if libresample is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libresample/include/libresample.h,
                 LIBRESAMPLE_LOCAL_AVAILABLE="yes",
                 LIBRESAMPLE_LOCAL_AVAILABLE="no")

   if test "$LIBRESAMPLE_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([libresample libraries are available in the local tree])
   else
      AC_MSG_NOTICE([libresample libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_LIBRESAMPLE], [
   if test "$LIBRESAMPLE_USE_LOCAL" = yes; then
      LIBRESAMPLE_CFLAGS='-I$(top_srcdir)/lib-src/libresample/include'
      LIBRESAMPLE_LIBS='$(top_builddir)/lib-src/libresample/libresample.a'
      AC_CONFIG_SUBDIRS([lib-src/libresample])
   fi

   AC_SUBST([LIBRESAMPLE_CFLAGS])
   AC_SUBST([LIBRESAMPLE_LIBS])

   AM_CONDITIONAL([USE_LIBRESAMPLE], [test "$LIBRESAMPLE_USE_LOCAL" = yes -o "$LIBRESAMPLE_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_LIBRESAMPLE], [test "$LIBRESAMPLE_USE_LOCAL" = yes])

   if test "$LIBRESAMPLE_USE_LOCAL" = yes -o "$LIBRESAMPLE_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_LIBRESAMPLE, 1,
                [Define if libresample support should be enabled])
   fi
])
