dnl Todo: Add Audacity / LAME license
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_lame.m4 serial 2

dnl Check for a copy of lame, whoose headers we will use for the importer
AC_DEFUN([AUDACITY_CHECKLIB_LAME], [
   AC_ARG_WITH(lame,
               [AS_HELP_STRING([--with-lame],
                               [use lame for import and export support])],
               LAME_ARGUMENT=$withval,
               LAME_ARGUMENT="unspecified")

   dnl See if LAME is installed in the system

   AC_CHECK_LIB(mp3lame,
                lame_set_VBR_q,
                lib_found="yes",
                lib_found="no")

   AC_CHECK_HEADER(lame/lame.h,
                   header_found="yes",
                   header_found="no")

   if test "$lib_found" = "yes" -a "$header_found" = "yes"; then
      LAME_SYSTEM_AVAILABLE="yes"
      AC_MSG_NOTICE([LAME library is available as system library.])
   else
      LAME_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([LAME library is NOT available as system library.])
   fi

   dnl see if LAME is available in the source dir

   AC_CHECK_FILE(${srcdir}/lib-src/lame/lame/lame.h,
                 LAME_LOCAL_AVAILABLE="yes",
                 LAME_LOCAL_AVAILABLE="no")

   if test "$LAME_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([LAME headers are available in this source tree.])
   else
      AC_MSG_NOTICE([LAME headers are NOT available in this source tree.])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_LAME], [
   if test "$LAME_USE_LOCAL" = yes; then
      LAME_CFLAGS='-I$(top_srcdir)/lib-src/lame'
      LAME_LIBS=""
   fi
   if test "$LAME_USE_SYSTEM" = yes; then
      LAME_CFLAGS=""
      if test "$dynamic_loading" = "no"; then
         LAME_LIBS="-lmp3lame"
         AC_DEFINE(DISABLE_DYNAMIC_LOADING_LAME, 1,
                   [Define if LAME should be linked at compile time])
      else
         LAME_LIBS=""
      fi
   fi

   AC_SUBST([LAME_CFLAGS])
   AC_SUBST([LAME_LIBS])

   AM_CONDITIONAL([USE_LAME], [test "$LAME_USE_LOCAL" = yes -o "$LAME_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_LAME], [test "$LAME_USE_LOCAL" = yes])
])
