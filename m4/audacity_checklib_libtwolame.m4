dnl add Audacity / Twolame license ?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libtwolame.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_LIBTWOLAME], [
   AC_ARG_WITH(libtwolame,
               [AS_HELP_STRING([--with-libtwolame],
                               [use libtwolame for MP2 export support ])],
               LIBTWOLAME_ARGUMENT=$withval,
               LIBTWOLAME_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LIBTWOLAME, 1,
                [Define if libtwolame (MP2 export) support should be enabled])
   fi

   dnl Check for a system copy of libtwolame to use, which needs to be
   dnl pretty current to work

   PKG_CHECK_MODULES(LIBTWOLAME, twolame >= 0.3.9,
                     twolame_available_system="yes",
                     twolame_available_system="no")

   if test "x$twolame_available_system" = "xyes" ; then
      LIBTWOLAME_SYSTEM_AVAILABLE="yes"
      LIBTWOLAME_SYSTEM_LIBS=$LIBTWOLAME_LIBS
      LIBTWOLAME_SYSTEM_CXXFLAGS=$LIBTWOLAME_CFLAGS
      LIBTWOLAME_SYSTEM_CPPSYMBOLS="USE_LIBTWOLAME"
      AC_MSG_NOTICE([Libtwolame library available as system library])
   else
      LIBTWOLAME_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([Libtwolame library NOT available as system library])
   fi

   dnl see if libtwolame is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/twolame/libtwolame/twolame.h,
                 twolame_h_found="yes",
                 twolame_h_found="no")

   if test "x$twolame_h_found" = "xyes" ; then
      LIBTWOLAME_LOCAL_AVAILABLE="yes"
      LIBTWOLAME_LOCAL_LIBS="libtwolame.a"
      LIBTWOLAME_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/twolame/libtwolame'
      LIBTWOLAME_LOCAL_CPPSYMBOLS="USE_LIBTWOLAME"

      dnl disable programs we don't need to build
      LIBTWOLAME_LOCAL_CONFIGURE_ARGS="--disable-programs"

      AC_MSG_NOTICE([libtwolame library is available in the local tree])
   else
      LIBTWOLAME_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([libtwolame library is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_LIBTWOLAME], [
   if test "$LIBTWOLAME_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/twolame])
   fi
])
