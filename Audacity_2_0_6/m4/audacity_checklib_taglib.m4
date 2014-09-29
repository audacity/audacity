dnl add Audacity / TagLib license ?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_taglib.m4 serial 2

AC_DEFUN([AUDACITY_CHECKLIB_TAGLIB], [
   AC_ARG_WITH([taglib],
               [AS_HELP_STRING([--with-taglib],
                               [use TagLib for metadata support ])],
               TAGLIB_ARGUMENT=$withval,
               TAGLIB_ARGUMENT="unspecified")

   dnl Check for a system copy of TagLib to use

   PKG_CHECK_MODULES([TAGLIB], [taglib >= 1.5],
                     [TAGLIB_SYSTEM_AVAILABLE="yes"],
                     [TAGLIB_SYSTEM_AVAILABLE="no"])

   if test "$TAGLIB_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([TagLib library available as system library])
   else
      AC_MSG_NOTICE([TagLib library NOT available as system library])
   fi

   dnl see if TagLib is available locally

   AC_CHECK_FILE([${srcdir}/lib-src/taglib/taglib/tag.h],
                 [TAGLIB_LOCAL_AVAILABLE="yes"],
                 [TAGLIB_LOCAL_AVAILABLE="no"])

   if test "$TAGLIB_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([TagLib library is available in the local tree])
   else
      AC_MSG_NOTICE([TagLib library is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_TAGLIB], [
   if test "$TAGLIB_USE_LOCAL" = yes; then
      TAGLIB_CFLAGS='-I$(top_srcdir)/lib-src/taglib/taglib'
      TAGLIB_LIBS='$(top_builddir)/lib-src/taglib/taglib/.libs/libtag.a'
      AC_CONFIG_SUBDIRS([lib-src/taglib])
   fi

   AC_SUBST([TAGLIB_CFLAGS])
   AC_SUBST([TAGLIB_LIBS])

   AM_CONDITIONAL([USE_TAGLIB], [test "$TAGLIB_USE_LOCAL" = yes -o "$TAGLIB_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_TAGLIB], [test "$TAGLIB_USE_LOCAL" = yes])

   if test "$TAGLIB_USE_LOCAL" = yes -o "$TAGLIB_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_TAGLIB, 1,
                [Define if TagLib (metadata export) support should be enabled])
   fi
])
