dnl add Audacity / Nyquist license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libnyquist.m4 serial 1

dnl Check for Nyquist as a library
AC_DEFUN([AUDACITY_CHECKLIB_LIBNYQUIST], [
   AC_ARG_ENABLE(nyquist,
               [AS_HELP_STRING([--enable-nyquist],
                               [enable Nyquist plug-in support [default=yes]])],
               LIBNYQUIST_ARGUMENT=$enableval,
               LIBNYQUIST_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_NYQUIST, 1,
                [Define if Nyquist support should be enabled])
   fi

   dnl Nyquist is never installed on the system
   dnl nyx is home-cooked by us, so system copy is unlikely
   LIBNYQUIST_SYSTEM_AVAILABLE="no"

   dnl see if Nyquist is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libnyquist/nyx.h,
                 nyx_h_found="yes",
                 nyx_h_found="no")

   if test "x$nyx_h_found" = "xyes" ; then
      LIBNYQUIST_LOCAL_AVAILABLE="yes"
      LIBNYQUIST_LOCAL_LIBS="libnyquist.a"
      LIBNYQUIST_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libnyquist'
      LIBNYQUIST_LOCAL_CPPSYMBOLS="USE_NYQUIST"
      LIBNYQUIST_LOCAL_OPTOBJS="effects/nyquist/Nyquist.o"
      LIBNYQUIST_LOCAL_OPTOBJS="$LIBNYQUIST_LOCAL_OPTOBJS effects/nyquist/LoadNyquist.o"

      LIBNYQUIST_LOCAL_CONFIG_SUBDIRS="lib-src/libnyquist"

      AC_MSG_NOTICE([nyquist libraries are available in the local tree])
   else
      LIBNYQUIST_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([nyquist libraries are NOT available in the local tree])
   fi
])

