dnl Add Audacity / LRDF license?
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_liblrdf.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_LIBLRDF], [
   AC_ARG_WITH(liblrdf,
               [AS_HELP_STRING([--with-liblrdf],
                               [use liblrdf for categorisation of LADSPA plugins ])],
               LIBLRDF_ARGUMENT=$withval,
               LIBLRDF_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBLRDF, 1,
                [Define if liblrdf (metadata for LADSPA plugins) support should be enabled])
   fi

   dnl Check for a system copy of liblrdf to use. I've only tested with 
   dnl version 0.4.0, this requirement might be relaxed in the future if
   dnl someone else has it working with an earlier version.

   PKG_CHECK_MODULES(LIBLRDF, lrdf >= 0.4.0,
                     liblrdf_available_system="yes",
                     liblrdf_available_system="no")

   LIBLRDF_SYSTEM_AVAILABLE="no"
   if test "x$liblrdf_available_system" = "xyes" ; then
      LIBLRDF_SYSTEM_AVAILABLE="yes"
      LIBLRDF_SYSTEM_LIBS="$LIBLRDF_LIBS"
      LIBLRDF_SYSTEM_CXXFLAGS="$LIBLRDF_CFLAGS"
      LIBLRDF_SYSTEM_CPPSYMBOLS="USE_LIBLRDF"
      AC_MSG_NOTICE([liblrdf available as system library])
   fi
   if test "x$LIBLRDF_SYSTEM_AVAILABLE" = "xno" ; then
      AC_MSG_NOTICE([liblrdf NOT available as system library])
   fi

   dnl see if liblrdf is available locally
   AC_CHECK_FILE(${srcdir}/lib-src/liblrdf/lrdf.h,
                 lrdf_h_found="yes",
                 lrdf_h_found="no")

   if test "x$lrdf_h_found" = "xyes" ; then
      LIBLRDF_LOCAL_AVAILABLE="yes"
      LIBLRDF_LOCAL_LIBS="liblrdf.a"
      LIBLRDF_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/liblrdf'
      LIBLRDF_LOCAL_CPPSYMBOLS="USE_LIBLRDF"
      AC_MSG_NOTICE([liblrdf is available in the local tree])
   else
      LIBLRDF_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([liblrdf is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_LIBLRDF], [
   if test "$LIBLRDF_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/liblrdf])
   fi
])
