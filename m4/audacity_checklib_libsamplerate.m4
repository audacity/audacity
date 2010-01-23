dnl Add Audacity / libsamplerate license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libsamplerate.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_LIBSAMPLERATE], [

   AC_ARG_WITH(libsamplerate,
               [AS_HELP_STRING([--with-libsamplerate],
                               [use libsamplerate instead of libresample for sample rate conversion. Do not use in conjunction with VST plug-in support!])],
               LIBSAMPLERATE_ARGUMENT=$withval,
               LIBSAMPLERATE_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBSAMPLERATE, 1,
                [Define if libsamplerate support should be enabled])
   fi

   dnl see if libsamplerate is installed on the system

   PKG_CHECK_MODULES(SAMPLERATE, samplerate >= 0.1.2,
                     samplerate_available_system="yes",
                     samplerate_available_system="no")

   if test "x$samplerate_available_system" = "xyes" ; then
      LIBSAMPLERATE_SYSTEM_AVAILABLE="yes"
      LIBSAMPLERATE_SYSTEM_LIBS=$SAMPLERATE_LIBS
      LIBSAMPLERATE_SYSTEM_CXXFLAGS=$SAMPLERATE_CFLAGS
      LIBSAMPLERATE_SYSTEM_CPPSYMBOLS="USE_LIBSAMPLERATE"
      AC_MSG_NOTICE([Libsamplerate libraries are available as system libraries])
   else
      LIBSAMPLERATE_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libsamplerate libraries are NOT available as system libraries])
   fi

   dnl see if libsamplerate is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libsamplerate/src/samplerate.h,
                 samplerate_h_found="yes",
                 samplerate_h_found="no")

   if test "x$samplerate_h_found" = "xyes" ; then
      LIBSAMPLERATE_LOCAL_AVAILABLE="yes"
      LIBSAMPLERATE_LOCAL_LIBS="libsamplerate.a"
      LIBSAMPLERATE_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libsamplerate/src'
      LIBSAMPLERATE_LOCAL_CPPSYMBOLS="USE_LIBSAMPLERATE"

      if test ! -f lib-src/libsamplerate/Makefile ; then
         LIBSAMPLERATE_LOCAL_CONFIG_SUBDIRS="lib-src/libsamplerate"
      fi
      AC_MSG_NOTICE([libsamplerate libraries are available in the local tree])
   else
      LIBSAMPLERATE_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libsamplerate libraries are NOT available in the local tree])
   fi
])

