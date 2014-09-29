dnl add audacity / flac license?
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libflac.m4 serial 3

AC_DEFUN([AUDACITY_CHECKLIB_LIBFLAC], [
   AC_ARG_WITH(libflac,
               [AS_HELP_STRING([--with-libflac],
                               [use libFLAC for FLAC support])],
               LIBFLAC_ARGUMENT=$withval,
               LIBFLAC_ARGUMENT="unspecified")

   dnl See if FLAC is installed in the system

   PKG_CHECK_MODULES([FLAC], [flac >= 1.3.0 flac++ >= 1.3.0],
                     [LIBFLAC_SYSTEM_AVAILABLE="yes"],
                     [LIBFLAC_SYSTEM_AVAILABLE="no"])

   dnl Check for flac < 1.3.0
   if test "$LIBFLAC_SYSTEM_AVAILABLE" = "no"; then
      PKG_CHECK_MODULES([FLAC], [flac flac++],
                        [LIBFLAC_SYSTEM_AVAILABLE="yes"],
                        [LIBFLAC_SYSTEM_AVAILABLE="no"])
      dnl flac < 1.3.0 adds its own FLAC and FLAC++ subdirectories to the search
      dnl path and ships a assert.h file there. This assert.h overwrites the
      dnl assert.h header from the C standard library. We need to strip /FLAC
      dnl and /FLAC++ from the include paths to make the assert.h from the C
      dnl standard library available again.
      [FLAC_CFLAGS=$(echo "$FLAC_CFLAGS" | sed 's@-I\([^ ]*\)/FLAC\(++\)\? @-I\1 @g')]
   fi

   if test "$LIBFLAC_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([FLAC libraries are available as system libraries])
   else
      AC_MSG_NOTICE([FLAC/FLAC++ libraries are NOT available as system libraries])
   fi

   dnl see if FLAC is available in the source dir

   AC_CHECK_FILE(${srcdir}/lib-src/libflac/include/FLAC/format.h,
                 flac_h_available="yes",
                 flac_h_available="no")

   AC_CHECK_FILE(${srcdir}/lib-src/libflac/include/FLAC++/decoder.h,
                 flacpp_h_available="yes",
                 flacpp_h_available="no")

   if test "$flac_h_available" = "yes" -a "$flacpp_h_available" = "yes"; then
      LIBFLAC_LOCAL_AVAILABLE="yes"
      LIBFLAC_LOCAL_CONFIGURE_ARGS="--disable-xmms-plugin --disable-doxygen-docs --disable-thorough-tests"
      AC_MSG_NOTICE([FLAC libraries are available in this source tree])
   else
      LIBFLAC_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([FLAC libraries are NOT available in this source tree])
   fi

   LIBFLAC_MIMETYPES="audio/flac;audio/x-flac;"
])

AC_DEFUN([AUDACITY_CONFIG_LIBFLAC], [
   if test "$LIBFLAC_USE_LOCAL" = yes; then
      FLAC_CFLAGS='-I$(top_srcdir)/lib-src/libflac/include'
      FLAC_LIBS='$(top_builddir)/lib-src/libflac/src/libFLAC++/libFLAC++.la $(top_builddir)/lib-src/libflac/src/libFLAC/libFLAC.la'
      AC_CONFIG_SUBDIRS([lib-src/libflac])
   fi

   AC_SUBST([FLAC_CFLAGS])
   AC_SUBST([FLAC_LIBS])

   AM_CONDITIONAL([USE_LIBFLAC], [test "$LIBFLAC_USE_LOCAL" = yes -o "$LIBFLAC_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_LIBFLAC], [test "$LIBFLAC_USE_LOCAL" = yes])

   if test "$LIBFLAC_USE_LOCAL" = yes -o "$LIBFLAC_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_LIBFLAC, 1,
                [Define if the FLAC library is present])
   fi
])
