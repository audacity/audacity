dnl add Audacity / Nyquist license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libnyquist.m4 serial 2

dnl Check for Nyquist as a library
AC_DEFUN([AUDACITY_CHECKLIB_LIBNYQUIST], [
   AC_ARG_ENABLE(nyquist,
               [AS_HELP_STRING([--enable-nyquist],
                               [enable Nyquist plug-in support [default=yes]])],
               LIBNYQUIST_ARGUMENT=$enableval,
               LIBNYQUIST_ARGUMENT="unspecified")

   dnl Nyquist is never installed on the system
   dnl nyx is home-cooked by us, so system copy is unlikely
   LIBNYQUIST_SYSTEM_AVAILABLE="no"

   dnl see if Nyquist is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libnyquist/nyx.h,
                 LIBNYQUIST_LOCAL_AVAILABLE="yes",
                 LIBNYQUIST_LOCAL_AVAILABLE="no")

   if test "$LIBNYQUIST_LOCAL_AVAILABLE" = "yes" ; then
      AC_MSG_NOTICE([nyquist libraries are available in the local tree])
   else
      AC_MSG_NOTICE([nyquist libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_LIBNYQUIST], [
   if test "$LIBNYQUIST_USE_LOCAL" = yes; then
      LIBNYQUIST_CFLAGS='-I$(top_srcdir)/lib-src/libnyquist'
      LIBNYQUIST_LIBS='$(top_builddir)/lib-src/libnyquist/libnyquist.a'
      AC_CONFIG_SUBDIRS([lib-src/libnyquist])
   fi

   AC_SUBST([LIBNYQUIST_CFLAGS])
   AC_SUBST([LIBNYQUIST_LIBS])

   AM_CONDITIONAL([USE_LIBNYQUIST], [test "$LIBNYQUIST_USE_LOCAL" = yes -o "$LIBNYQUIST_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_LIBNYQUIST], [test "$LIBNYQUIST_USE_LOCAL" = yes])

   if test "$LIBNYQUIST_USE_LOCAL" = yes -o "$LIBNYQUIST_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_NYQUIST, 1,
                [Define if Nyquist support should be enabled])
   fi
])
