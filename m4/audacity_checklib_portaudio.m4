dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_portaudio.m4 serial 2

AC_DEFUN([AUDACITY_CHECKLIB_PORTAUDIO], [
   AC_ARG_WITH(portaudio,
               [AS_HELP_STRING([--with-portaudio], [use portaudio19])],
               PORTAUDIO_ARGUMENT=$withval,
               PORTAUDIO_ARGUMENT="unspecified")

   dnl see if portaudio is installed on the system

   PKG_CHECK_MODULES(PORTAUDIO, portaudio-2.0 >= 19,
                     PORTAUDIO_SYSTEM_AVAILABLE="yes",
                     PORTAUDIO_SYSTEM_AVAILABLE="no")

   if test "$PORTAUDIO_SYSTEM_AVAILABLE" = "yes"; then
      AC_EGREP_HEADER([Pa_GetStreamHostApiType], [portaudio.h],
                      [have_portaudio_support=yes], [have_portaudio_support=no])

      if test "$have_portaudio_support" = "yes"; then
         PORTAUDIO_SYSTEM_AVAILABLE="yes"
         AC_MSG_NOTICE([portaudio19 library is available as system library])
      else
         PORTAUDIO_SYSTEM_AVAILABLE="no"
         AC_MSG_NOTICE([portaudio19 library is available as system library, but does not have the Pa_GetStreamHostApiType function.])
      fi
   else
      AC_MSG_NOTICE([portaudio19 library is NOT available as system library])
   fi

   dnl see if portaudio is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/portaudio-v19/include/portaudio.h,
                 PORTAUDIO_LOCAL_AVAILABLE="yes",
                 PORTAUDIO_LOCAL_AVAILABLE="no")

   if test "$PORTAUDIO_LOCAL_AVAILABLE" = "yes"; then
      dnl We need to override the pkg-config check for portmixer by passing
      dnl PORTAUDIO_CFLAGS and PORTAUDIO_LIBS to the configure script of portmixer.
      portaudio_dir="$(pwd)/lib-src/portaudio-v19"
      PORTAUDIO_LOCAL_CONFIGURE_ARGS="PORTAUDIO_CFLAGS=-I${portaudio_dir}/include PORTAUDIO_LIBS=${portaudio_dir}/lib/libportaudio.la"
      AC_MSG_NOTICE([portaudio19 library is available in the local tree])
   else
      AC_MSG_NOTICE([portaudio19 library is NOT available in the local tree])
   fi

])

AC_DEFUN([AUDACITY_CONFIG_PORTAUDIO], [
   if test "$PORTAUDIO_USE_LOCAL" = yes; then
      PORTAUDIO_CFLAGS='-I$(top_srcdir)/lib-src/portaudio-v19/include'
      PORTAUDIO_LIBS='$(top_builddir)/lib-src/portaudio-v19/lib/libportaudio.la'
      AC_CONFIG_SUBDIRS([lib-src/portaudio-v19])
   fi

   AC_SUBST([PORTAUDIO_CFLAGS])
   AC_SUBST([PORTAUDIO_LIBS])

   AM_CONDITIONAL([USE_LOCAL_PORTAUDIO], [test "$PORTAUDIO_USE_LOCAL" = yes])
])
