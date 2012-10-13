dnl Add Audacity license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libsoxr.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_LIBSOXR], [

   AC_ARG_WITH(libsoxr,
               [AS_HELP_STRING([--with-libsoxr],
                               [use libsoxr for sample rate conversion: [yes,no]])],
               LIBSOXR_ARGUMENT=$withval,
               LIBSOXR_ARGUMENT="unspecified")

   if true ; then
      AC_DEFINE(USE_LIBSOXR, 1,
                [Define if libsoxr support should be enabled])
   fi

   dnl see if libsoxr is installed on the system

   dnl ... but libsoxr isn't yet generally installed as a system library...

   LIBSOXR_SYSTEM_AVAILABLE="no"

   dnl see if libsoxr is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libsoxr/src/soxr-lsr.h,
                 soxr_h_found="yes",
                 soxr_h_found="no")

   if test "x$soxr_h_found" = "xyes" ; then
      LIBSOXR_LOCAL_AVAILABLE="yes"
      LIBSOXR_LOCAL_LIBS="libsoxr-lsr.a libsoxr.a"
      LIBSOXR_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libsoxr/src'
      LIBSOXR_LOCAL_CPPSYMBOLS="USE_LIBSOXR"

      if test ! -f lib-src/libsoxr/Makefile ; then
         LIBSOXR_LOCAL_CONFIG_SUBDIRS="lib-src/libsoxr"

         # Breaks other other libraries in Audacity tree; but why is ./configure
         # passing options specific to this library to other libraries?
         #LIBSOXR_LOCAL_CONFIGURE_ARGS="\"-DBUILD_SHARED_LIBS=OFF -DWITH_OPENMP=OFF\""
      fi
      AC_MSG_NOTICE([libsoxr libraries are available in the local tree])
   else
      LIBSOXR_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libsoxr libraries are NOT available in the local tree])
   fi
])

