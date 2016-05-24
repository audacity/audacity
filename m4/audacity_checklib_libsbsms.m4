dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libsbsms.m4 serial 2


AC_DEFUN([AUDACITY_CHECKLIB_LIBSBSMS], [
   AC_ARG_WITH(sbsms,
               [AS_HELP_STRING([--with-sbsms],
                      [use libsbsms for pitch and tempo changing])],
               LIBSBSMS_ARGUMENT=$withval,
               LIBSBSMS_ARGUMENT="unspecified")

   dnl see if sbsms is installed on the system

   PKG_CHECK_MODULES(SBSMS, sbsms >= 2.2.0,
                     LIBSBSMS_SYSTEM_AVAILABLE="yes",
                     LIBSBSMS_SYSTEM_AVAILABLE="no")


   if test "$LIBSBSMS_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([Libsbsms libraries are available as system libraries])
   else
      AC_MSG_NOTICE([Libsbsms libraries are NOT available as system libraries])
   fi

   dnl see if libsbsms is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/sbsms/include/sbsms.h,
                 LIBSBSMS_LOCAL_AVAILABLE="yes",
                 LIBSBSMS_LOCAL_AVAILABLE="no")

   if test "$LIBSBSMS_LOCAL_AVAILABLE" = "yes"; then
      dnl do not build programs we don't need
      LIBSBSMS_LOCAL_CONFIGURE_ARGS="--disable-programs"
      AC_MSG_NOTICE([libsbsms libraries are available in the local tree])
   else
      AC_MSG_NOTICE([libsbsms libraries are NOT available in the local tree])
   fi

])

AC_DEFUN([AUDACITY_CONFIG_LIBSBSMS], [
   if test "$LIBSBSMS_USE_LOCAL" = yes; then
      SBSMS_CFLAGS='-I$(top_srcdir)/lib-src/sbsms/include'
      SBSMS_LIBS='$(top_builddir)/lib-src/sbsms/src/.libs/libsbsms.a'
      AC_CONFIG_SUBDIRS([lib-src/sbsms])
   fi

   AC_SUBST([SBSMS_CFLAGS])
   AC_SUBST([SBSMS_LIBS])

   AM_CONDITIONAL([USE_SBSMS], [test "$LIBSBSMS_USE_LOCAL" = yes -o "$LIBSBSMS_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_SBSMS], [test "$LIBSBSMS_USE_LOCAL" = yes])

   if test "$LIBSBSMS_USE_LOCAL" = yes -o "$LIBSBSMS_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_SBSMS, 1,
                [Define if SBSMS support should be enabled])
   fi
])
