dnl Add Audacity license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libresample.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_LIBRESAMPLE], [

   AC_ARG_WITH(libresample,
               [AS_HELP_STRING([--with-libresample],
                               [use libresample for variable-rate resampling: [yes,no]])],
               LIBRESAMPLE_ARGUMENT=$withval,
               LIBRESAMPLE_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBRESAMPLE, 1,
                [Define if libresample support should be enabled])
   fi

   dnl see if libresample is installed on the system

   dnl ... but libresample isn't generally installed as a system library...

   LIBRESAMPLE_SYSTEM_AVAILABLE="no"

   dnl see if libresample is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libresample/include/libresample.h,
                 resample_h_found="yes",
                 resample_h_found="no")

   if test "x$resample_h_found" = "xyes" ; then
      LIBRESAMPLE_LOCAL_AVAILABLE="yes"
      LIBRESAMPLE_LOCAL_LIBS="libresample.a"
      LIBRESAMPLE_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/libresample/include'
      LIBRESAMPLE_LOCAL_CPPSYMBOLS="USE_LIBRESAMPLE"

      if test ! -f lib-src/libresample/Makefile ; then
         LIBRESAMPLE_LOCAL_CONFIG_SUBDIRS="lib-src/libresample"
      fi
      AC_MSG_NOTICE([libresample libraries are available in the local tree])
   else
      LIBRESAMPLE_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libresample libraries are NOT available in the local tree])
   fi
])

