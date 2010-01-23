dnl Add Audacity / SLV2 license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_slv2.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_SLV2], [
   AC_ARG_WITH(slv2,
               [AS_HELP_STRING([--with-slv2],
                               [use SLV2 for loading LV2 plugins ])],
               SLV2_ARGUMENT=$withval,
               SLV2_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_SLV2, 1,
                [Define if SLV2 (library for loading LV2 plugins) should be enabled])
   fi

   dnl Check for a system copy of SLV2 to use. We need at least version 0.6.

   PKG_CHECK_MODULES(SLV2, slv2 >= 0.6.0,
                     slv2_available_system="yes",
                     slv2_available_system="no")
   SLV2_SYSTEM_AVAILABLE="no"
   if test "x$slv2_available_system" = "xyes" ; then
      SLV2_SYSTEM_AVAILABLE="yes"
      SLV2_SYSTEM_LIBS="$SLV2_LIBS"
      SLV2_SYSTEM_CXXFLAGS="$SLV2_CFLAGS"
      SLV2_SYSTEM_CPPSYMBOLS="USE_SLV2"
      SLV2_SYSTEM_OPTOBJS="effects/lv2/LoadLV2.o effects/lv2/LV2Effect.o effects/lv2/LV2PortGroup.o"
      AC_MSG_NOTICE([SLV2 available as system library])
   fi
   if test "x$SLV2_SYSTEM_AVAILABLE" = "xno" ; then
      AC_MSG_NOTICE([SLV2 NOT available as system library])
   fi

   dnl Check if SLV2 is available locally.
   AC_CHECK_FILE(${srcdir}/lib-src/slv2/slv2/slv2.h,
                 slv2_h_found="yes",
                 slv2_h_found="no")

   if test "x$slv2_h_found" = "xyes" ; then
      SLV2_LOCAL_AVAILABLE="yes"
      SLV2_LOCAL_LIBS="libslv2.a"
      SLV2_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/slv2'
      SLV2_LOCAL_CPPSYMBOLS="USE_SLV2"
      SLV2_LOCAL_OPTOBJS="effects/lv2/LoadLV2.o effects/lv2/LV2Effect.o effects/lv2/LV2PortGroup.o"
      if test ! -f lib-src/slv2/Makefile ; then
         SLV2_LOCAL_CONFIG_SUBDIRS="lib-src/slv2"
      fi
      AC_MSG_NOTICE([SLV2 is available in the local tree])
   else
      SLV2_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([SLV2 is NOT available in the local tree])
   fi
])

