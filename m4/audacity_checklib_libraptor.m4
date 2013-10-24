dnl Add Audacity / raptor license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libraptor.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_LIBRAPTOR], [
   AC_ARG_WITH(libraptor,
               [AS_HELP_STRING([--with-libraptor],
                               [libraptor is needed for categorisation of LADSPA plugins])],
               LIBRAPTOR_ARGUMENT=$withval,
               LIBRAPTOR_ARGUMENT="unspecified")

   dnl Check for a system copy of libraptor to use.

   PKG_CHECK_MODULES(LIBRAPTOR, raptor >= 1.4.17,
                     libraptor_available_system="yes",
                     libraptor_available_system="no")

   LIBRAPTOR_SYSTEM_AVAILABLE="no"
   if test "x$libraptor_available_system" = "xyes" ; then
      LIBRAPTOR_SYSTEM_AVAILABLE="yes"
      LIBRAPTOR_SYSTEM_LIBS="$LIBRAPTOR_LIBS"
      LIBRAPTOR_SYSTEM_CXXFLAGS="$LIBRAPTOR_CFLAGS"
      AC_MSG_NOTICE([libraptor available as system library])
   fi
   if test "x$LIBRAPTOR_SYSTEM_AVAILABLE" = "xno" ; then
      AC_MSG_NOTICE([libraptor NOT available as system library])
   fi

   dnl see if libraptor is available locally
   AC_CHECK_FILE(${srcdir}/lib-src/libraptor/src/raptor.h,
                 raptor_h_found="yes",
                 raptor_h_found="no")

   if test "x$raptor_h_found" = "xyes" ; then
      LIBRAPTOR_LOCAL_AVAILABLE="yes"
      LIBRAPTOR_LOCAL_LIBS="libraptor.a"
      LIBRAPTOR_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libraptor/src'
      AC_MSG_NOTICE([libraptor is available in the local tree])
      if test "x$LIBEXPAT_SYSTEM_AVAILABLE" = "xno" ; then
         LIBRAPTOR_LOCAL_CONFIGURE_ARGS="\"--with-expat-source=${srcdir}/src/include\""
      fi
      LIBRAPTOR_LOCAL_CONFIGURE_ARGS="$LIBRAPTOR_LOCAL_CONFIGURE_ARGS RAPTOR_CFLAGS='-I../../libraptor/src' RAPTOR_LIBS='-L../.. -lraptor'"
   else
      LIBRAPTOR_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libraptor is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_LIBRAPTOR], [
   if test "$LIBRAPTOR_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/libraptor])
   fi
])
