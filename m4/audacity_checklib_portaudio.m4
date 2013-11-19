dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_portaudio.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_PORTAUDIO], [
   AC_ARG_WITH(portaudio,
               [AS_HELP_STRING([--with-portaudio], [use portaudio19])],
               PORTAUDIO_ARGUMENT=$withval,
               PORTAUDIO_ARGUMENT="unspecified")

   dnl see if portaudio is installed on the system

   PKG_CHECK_MODULES(PORTAUDIO, portaudio-2.0 >= 19,
                     portaudio_available_system="yes",
                     portaudio_available_system="no")

   if test "x$portaudio_available_system" = "xyes" ; then
      AC_EGREP_HEADER([Pa_GetStreamHostApiType], [portaudio.h],
                      [have_portaudio_support=yes], [have_portaudio_support=no])

      if test "x$have_portaudio_support" = "xyes" ; then
         PORTAUDIO_SYSTEM_AVAILABLE="yes"
         PORTAUDIO_SYSTEM_LIBS=$PORTAUDIO_LIBS
         PORTAUDIO_SYSTEM_CXXFLAGS=$PORTAUDIO_CFLAGS
         AC_MSG_NOTICE([portaudio19 library is available as system library])
      else
         PORTAUDIO_SYSTEM_AVAILABLE="no"
         AC_MSG_NOTICE([portaudio19 library is available as system library, but does not have the Pa_GetStreamHostApiType function.])
      fi
   else
      PORTAUDIO_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([portaudio19 library is NOT available as system library])
   fi

   dnl see if portaudio is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/portaudio-v19/include/portaudio.h,
                 portaudio_h_found="yes",
                 portaudio_h_found="no")

   if test "x$portaudio_h_found" = "xyes" ; then
      PORTAUDIO_LOCAL_AVAILABLE="yes"
      PORTAUDIO_LOCAL_LIBS="libportaudio.a"
      PORTAUDIO_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/portaudio-v19/include'

      dnl We need to override the pkg-config check for portmixer by passing
      dnl PORTAUDIO_CFLAGS and PORTAUDIO_LIBS to the configure script of portmixer.
      portaudio_dir="$(pwd)/lib-src/portaudio-v19"
      PORTAUDIO_LOCAL_CONFIGURE_ARGS="PORTAUDIO_CFLAGS=-I${portaudio_dir}/include PORTAUDIO_LIBS=${portaudio_dir}/lib/.libs/libportaudio.a"

      AC_MSG_NOTICE([portaudio19 library is available in the local tree])
   else
      PORTAUDIO_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([portaudio19 library is NOT available in the local tree])
   fi

])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_PORTAUDIO], [
   if test "$PORTAUDIO_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/portaudio-v19])
   fi
])
