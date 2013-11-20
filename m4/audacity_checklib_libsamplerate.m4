dnl Add Audacity / libsamplerate license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libsamplerate.m4 serial 3

AC_DEFUN([AUDACITY_CHECKLIB_LIBSAMPLERATE], [

   AC_ARG_WITH(libsamplerate,
               [AS_HELP_STRING([--with-libsamplerate],
                               [use libsamplerate for sample rate conversion. Do not use in conjunction with VST plug-in support!])],
               LIBSAMPLERATE_ARGUMENT=$withval,
               LIBSAMPLERATE_ARGUMENT="unspecified")

   dnl see if libsamplerate is installed on the system

   PKG_CHECK_MODULES(SAMPLERATE, samplerate >= 0.1.2,
                     LIBSAMPLERATE_SYSTEM_AVAILABLE="yes",
                     LIBSAMPLERATE_SYSTEM_AVAILABLE="no")

   if test "$LIBSAMPLERATE_SYSTEM_AVAILABLE" = "yes" ; then
      AC_MSG_NOTICE([Libsamplerate libraries are available as system libraries])
   else
      AC_MSG_NOTICE([Libsamplerate libraries are NOT available as system libraries])
   fi

   dnl see if libsamplerate is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libsamplerate/src/samplerate.h,
                 LIBSAMPLERATE_LOCAL_AVAILABLE="yes",
                 LIBSAMPLERATE_LOCAL_AVAILABLE="no")

   if test "$LIBSAMPLERATE_LOCAL_AVAILABLE" = "yes" ; then
      AC_MSG_NOTICE([libsamplerate libraries are available in the local tree])
   else
      AC_MSG_NOTICE([libsamplerate libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_LIBSAMPLERATE], [
   if test "$LIBSAMPLERATE_USE_LOCAL" = yes; then
      SAMPLERATE_CFLAGS='-I$(top_srcdir)/lib-src/libsamplerate/src'
      SAMPLERATE_LIBS='$(top_builddir)/lib-src/libsamplerate/libsamplerate.a'
      AC_CONFIG_SUBDIRS([lib-src/libsamplerate])
   fi

   AC_SUBST([SAMPLERATE_CFLAGS])
   AC_SUBST([SAMPLERATE_LIBS])

   AM_CONDITIONAL([USE_LIBSAMPLERATE], [test "$LIBSAMPLERATE_USE_LOCAL" = yes -o "$LIBSAMPLERATE_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_LIBSAMPLERATE], [test "$LIBSAMPLERATE_USE_LOCAL" = yes])

   if test "$LIBSAMPLERATE_USE_LOCAL" = yes -o "$LIBSAMPLERATE_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_LIBSAMPLERATE, 1,
                [Define if libsamplerate support should be enabled])
   fi
])
