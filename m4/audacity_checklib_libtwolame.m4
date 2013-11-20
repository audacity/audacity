dnl add Audacity / Twolame license ?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libtwolame.m4 serial 2

AC_DEFUN([AUDACITY_CHECKLIB_LIBTWOLAME], [
   AC_ARG_WITH(libtwolame,
               [AS_HELP_STRING([--with-libtwolame],
                               [use libtwolame for MP2 export support ])],
               LIBTWOLAME_ARGUMENT=$withval,
               LIBTWOLAME_ARGUMENT="unspecified")

   dnl Check for a system copy of libtwolame to use, which needs to be
   dnl pretty current to work

   PKG_CHECK_MODULES(LIBTWOLAME, twolame >= 0.3.9,
                     LIBTWOLAME_SYSTEM_AVAILABLE="yes",
                     LIBTWOLAME_SYSTEM_AVAILABLE="no")

   if test "$LIBTWOLAME_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([Libtwolame library available as system library])
   else
      AC_MSG_NOTICE([Libtwolame library NOT available as system library])
   fi

   dnl see if libtwolame is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/twolame/libtwolame/twolame.h,
                 LIBTWOLAME_LOCAL_AVAILABLE="yes",
                 LIBTWOLAME_LOCAL_AVAILABLE="no")

   if test "$LIBTWOLAME_LOCAL_AVAILABLE" = "yes"; then
      dnl disable programs we don't need to build
      LIBTWOLAME_LOCAL_CONFIGURE_ARGS="--disable-programs"
      AC_MSG_NOTICE([libtwolame library is available in the local tree])
   else
      AC_MSG_NOTICE([libtwolame library is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_LIBTWOLAME], [
   if test "$LIBTWOLAME_USE_LOCAL" = yes; then
      LIBTWOLAME_CFLAGS='-I$(top_srcdir)/lib-src/twolame/libtwolame'
      LIBTWOLAME_LIBS='$(top_builddir)/lib-src/twolame/libtwolame/.libs/libtwolame.a'
      AC_CONFIG_SUBDIRS([lib-src/twolame])
   fi

   AC_SUBST([LIBTWOLAME_CFLAGS])
   AC_SUBST([LIBTWOLAME_LIBS])

   AM_CONDITIONAL([USE_LIBTWOLAME], [test "$LIBTWOLAME_USE_LOCAL" = yes -o "$LIBTWOLAME_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_LIBTWOLAME], [test "$LIBTWOLAME_USE_LOCAL" = yes])

   if test "$LIBTWOLAME_USE_LOCAL" = yes -o "$LIBTWOLAME_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_LIBTWOLAME, 1,
                [Define if libtwolame (MP2 export) support should be enabled])
   fi
])
