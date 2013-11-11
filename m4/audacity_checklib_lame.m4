dnl Todo: Add Audacity / LAME license
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_lame.m4 serial 1

dnl Check for a copy of lame, whoose headers we will use for the importer
AC_DEFUN([AUDACITY_CHECKLIB_LAME], [
   AC_ARG_WITH(lame,
               [AS_HELP_STRING([--with-lame],
                               [use lame for import and export support])],
               LAME_ARGUMENT=$withval,
               LAME_ARGUMENT="unspecified")

   dnl These four lines are never executed, but they document the DISABLE_DYNAMIC_LOADING_LAME
   dnl pre-processor directive (for configunix.h etc)
   if false ; then
      AC_DEFINE(DISABLE_DYNAMIC_LOADING_LAME, 1,
                [Define if LAME should be linked at compile time])
   fi

   dnl See if LAME is installed in the system

   AC_CHECK_LIB(mp3lame,
                lame_set_VBR_q,
                lib_found="yes",
                lib_found="no")

   AC_CHECK_HEADER(lame/lame.h,
                   header_found="yes",
                   header_found="no")

   if test "x$lib_found" = "xyes" && test "x$header_found" = "xyes" ; then
      LAME_SYSTEM_AVAILABLE="yes"
      AC_MSG_NOTICE([LAME library is available as system library.])
      if test "x$dynamic_loading" = "xno"; then
         LAME_SYSTEM_LIBS="-lmp3lame"
         LAME_SYSTEM_CPPSYMBOLS="DISABLE_DYNAMIC_LOADING_LAME"
      fi
   else
      LAME_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([LAME library is NOT available as system library.])
   fi

   dnl see if LAME is available in the source dir

   AC_CHECK_FILE(${srcdir}/lib-src/lame/lame/lame.h,
                 LAME_LOCAL_AVAILABLE="yes",
                 LAME_LOCAL_AVAILABLE="no")

   if test "$LAME_LOCAL_AVAILABLE" = "yes"; then
      LAME_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/lame'
      AC_MSG_NOTICE([LAME headers are available in this source tree.])
   else
      AC_MSG_NOTICE([LAME headers are NOT available in this source tree.])
   fi
])
