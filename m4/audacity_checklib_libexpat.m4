dnl add license?
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libexpat.m4 serial 2

AC_DEFUN([AUDACITY_CHECKLIB_EXPAT], [

   AC_ARG_WITH(expat,
               [AS_HELP_STRING([--with-expat],
                               [which expat to use for XML file support: [system,local]])],
               EXPAT_ARGUMENT=$withval,
               EXPAT_ARGUMENT="unspecified")

   dnl see if expat is installed on the system

   PKG_CHECK_MODULES(EXPAT, expat,
                     EXPAT_SYSTEM_AVAILABLE="yes",
                     EXPAT_SYSTEM_AVAILABLE="no")

   if test "$EXPAT_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([Expat libraries are available as system libraries])
   else
      AC_MSG_NOTICE([Expat libraries are NOT available as system libraries])
   fi

   dnl see if expat is available in the local tree

   AC_CHECK_FILE(${srcdir}/lib-src/expat/lib/expat.h,
                 EXPAT_LOCAL_AVAILABLE="yes",
                 EXPAT_LOCAL_AVAILABLE="no")

   if test "$EXPAT_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([Expat libraries are available in the local tree])
   else
      AC_MSG_NOTICE([Expat libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_EXPAT], [
   if test "$EXPAT_USE_LOCAL" = yes; then
      EXPAT_CFLAGS='-I$(top_srcdir)/lib-src/expat'
      EXPAT_LIBS='$(top_builddir)/lib-src/expat/libexpat.la'
      AC_CONFIG_SUBDIRS([lib-src/expat])
   fi

   AC_SUBST([EXPAT_CFLAGS])
   AC_SUBST([EXPAT_LIBS])

   AM_CONDITIONAL([USE_LOCAL_EXPAT], [test "$EXPAT_USE_LOCAL" = yes])
])
