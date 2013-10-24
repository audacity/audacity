dnl Add audacity / libmad license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libmad.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_LIBMAD], [

   if false ; then
      AC_DEFINE(USE_LIBMAD, 1,
                [Define if mp3 support is implemented with the libmad library])
   fi

   AC_ARG_WITH(libmad,
               [AS_HELP_STRING([--with-libmad],
                               [use libmad for mp2/3 decoding support])],
               LIBMAD_ARGUMENT=$withval,
               LIBMAD_ARGUMENT="unspecified")

   dnl see if libmad is installed in the system >= 0.14.2b

   PKG_CHECK_MODULES(LIBMAD, mad >= 0.14.2b,
                     libmad_available_system="yes",
                     libmad_available_system="no")

   dnl if we don't have the version we want, do we have any at all?

   AC_CHECK_LIB(mad, mad_decoder_init,
                libmad_found="yes",
                libmad_found="no")

   if test "x$libmad_available_system" = "xyes" && test "x$libmad_found" = "xno" ; then
      AC_MSG_WARN([system installation of libmad found, but it is too old.  Upgrade to at least 0.14.2b to use with Audacity])
   fi

   if test "x$libmad_available_system" = "xyes" ; then
      LIBMAD_SYSTEM_AVAILABLE="yes"
      LIBMAD_SYSTEM_LIBS="$LIBMAD_LIBS"
	  LIBMAD_SYSTEM_CXXFLAGS="$LIBMAD_CFLAGS"
      LIBMAD_SYSTEM_CPPSYMBOLS="USE_LIBMAD"
      AC_MSG_NOTICE([libmad libraries are available as system libraries])
   else
      AC_MSG_NOTICE([libmad libraries are NOT available as system libraries])
   fi

   dnl see if libmad is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/libmad/frame.h,
                 frame_h_found="yes",
                 frame_h_found="no")

   if test "x$frame_h_found" = "xyes" ; then
      LIBMAD_LOCAL_AVAILABLE="yes"
      LIBMAD_LOCAL_LIBS="libmad.a"
      LIBMAD_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libmad'
      LIBMAD_LOCAL_CPPSYMBOLS="USE_LIBMAD"
      AC_MSG_NOTICE([libmad libraries are available in the local tree])
   else
      LIBMAD_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libmad libraries are NOT available in the local tree])
   fi
   LIBMAD_MIMETYPES="audio/mpeg;"
])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_LIBMAD], [
   if test "$LIBMAD_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/libmad])
   fi
])
