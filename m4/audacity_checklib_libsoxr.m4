dnl Add Audacity license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libsoxr.m4 serial 3

AC_DEFUN([AUDACITY_CHECKLIB_LIBSOXR], [
   AC_ARG_WITH(libsoxr,
               [AS_HELP_STRING([--with-libsoxr],
                               [use libsoxr for sample rate conversion])],
               LIBSOXR_ARGUMENT=$withval,
               LIBSOXR_ARGUMENT="unspecified")

   dnl see if libsoxr is installed on the system (we need 0.0.5 or
   dnl more recent)

   PKG_CHECK_MODULES(SOXR, soxr >= 0.0.5,
                     LIBSOXR_SYSTEM_AVAILABLE="yes",
                     LIBSOXR_SYSTEM_AVAILABLE="no")

   if test "$LIBSOXR_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([Soxr libraries are available as system libraries])
   else
      AC_MSG_NOTICE([Soxr libraries are NOT available as system libraries])
   fi

   dnl see if libsoxr is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libsoxr/src/soxr.h,
                 LIBSOXR_LOCAL_AVAILABLE="yes",
                 LIBSOXR_LOCAL_AVAILABLE="no")

   if test "$LIBSOXR_LOCAL_AVAILABLE" = "yes"; then
      # Breaks other other libraries in Audacity tree; but why is ./configure
      # passing options specific to this library to other libraries?
      #LIBSOXR_LOCAL_CONFIGURE_ARGS="\"-DBUILD_SHARED_LIBS=OFF -DWITH_OPENMP=OFF\""
      AC_MSG_NOTICE([libsoxr libraries are available in the local tree])
   else
      AC_MSG_NOTICE([libsoxr libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_LIBSOXR], [
   if test "$LIBSOXR_USE_LOCAL" = yes; then
      SOXR_CFLAGS='-I$(top_srcdir)/lib-src/libsoxr/src'
      SOXR_LIBS='$(top_builddir)/lib-src/libsoxr/src/libsoxr.a'
      AC_CONFIG_SUBDIRS([lib-src/libsoxr])
   fi

   AC_SUBST([SOXR_CFLAGS])
   AC_SUBST([SOXR_LIBS])

   AM_CONDITIONAL([USE_LIBSOXR], [test "$LIBSOXR_USE_LOCAL" = yes -o "$LIBSOXR_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_LIBSOXR], [test "$LIBSOXR_USE_LOCAL" = yes])

   if test "$LIBSOXR_USE_LOCAL" = yes -o "$LIBSOXR_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_LIBSOXR, 1,
                [Define if libsoxr support should be enabled])
   fi
])
