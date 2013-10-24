dnl Add Audacity license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libsoxr.m4 serial 2

AC_DEFUN([AUDACITY_CHECKLIB_LIBSOXR], [

   dnl These four lines are never executed, but they document the USE_LIBSOXR
   dnl pre-processor directive (for configunix.h etc)
   if false ; then
      AC_DEFINE(USE_LIBSOXR, 1,
                [Define if libsoxr support should be enabled])
   fi

   AC_ARG_WITH(libsoxr,
               [AS_HELP_STRING([--with-libsoxr],
                               [use libsoxr for sample rate conversion])],
               LIBSOXR_ARGUMENT=$withval,
               LIBSOXR_ARGUMENT="unspecified")

   dnl see if libsoxr is installed on the system (we need 0.0.5 or
   dnl more recent)

   PKG_CHECK_MODULES(SOXR, soxr >= 0.0.5,
                     soxr_available_system="yes",
                     soxr_available_system="no")

   if test "x$soxr_available_system" = "xyes" ; then
      LIBSOXR_SYSTEM_AVAILABLE="yes"
      LIBSOXR_SYSTEM_LIBS=$SOXR_LIBS
      LIBSOXR_SYSTEM_CXXFLAGS=$SOXR_CFLAGS
      LIBSOXR_SYSTEM_CPPSYMBOLS="USE_LIBSOXR"
      AC_MSG_NOTICE([Soxr libraries are available as system libraries])
   else
      LIBSOXR_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Soxr libraries are NOT available as system libraries])
   fi

   dnl see if libsoxr is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libsoxr/src/soxr.h,
                 soxr_h_found="yes",
                 soxr_h_found="no")

   if test "x$soxr_h_found" = "xyes" ; then
      LIBSOXR_LOCAL_AVAILABLE="yes"
      LIBSOXR_LOCAL_LIBS="libsoxr.a"
      LIBSOXR_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libsoxr/src'
      LIBSOXR_LOCAL_CPPSYMBOLS="USE_LIBSOXR"

      # Breaks other other libraries in Audacity tree; but why is ./configure
      # passing options specific to this library to other libraries?
      #LIBSOXR_LOCAL_CONFIGURE_ARGS="\"-DBUILD_SHARED_LIBS=OFF -DWITH_OPENMP=OFF\""
      AC_MSG_NOTICE([libsoxr libraries are available in the local tree])
   else
      LIBSOXR_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libsoxr libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_LIBSOXR], [
   if test "$LIBSOXR_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/libsoxr])
   fi
])
