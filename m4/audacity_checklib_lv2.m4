dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_lv2.m4 serial 1


AC_DEFUN([AUDACITY_CHECKLIB_LV2], [
   AC_ARG_WITH(lv2,
               [AS_HELP_STRING([--with-lv2],
                      [use for adding LV2 plug-in support])],
               LV2_ARGUMENT=$withval,
               LV2_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_LV2, 1,
                [Define if LV2 support should be enabled])
   fi

   dnl see if lv2 is installed on the system

   PKG_CHECK_MODULES(LV2, [lv2 lilv-0 >= 0.16],
                     lv2_available_system="yes",
                     lv2_available_system="no")


   if test "x$lv2_available_system" = "xyes" ; then
      LV2_SYSTEM_AVAILABLE="yes"
      LV2_SYSTEM_LIBS=$LV2_LIBS
      LV2_SYSTEM_CXXFLAGS=$LV2_CFLAGS
      LV2_SYSTEM_CPPSYMBOLS="USE_LV2"
      LV2_SYSTEM_OPTOBJS="effects/lv2/LoadLV2.o effects/lv2/LV2Effect.o effects/lv2/LV2PortGroup.o"

      AC_MSG_NOTICE([LV2 libraries are available as system libraries])
   else
      LV2_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([LV2 libraries are NOT available as system libraries])
   fi

   dnl see if LV2 is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/lv2/configure,
                 lv2_configure_found="yes",
                 lv2_configure_found="no")

   if test "x$lv2_configure_found" = "xyes" ; then
      LV2_LOCAL_AVAILABLE="yes"
      LV2_LOCAL_LIBS="liblv2.a"
      LV2_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/lv2/include'
      LV2_LOCAL_CPPSYMBOLS="USE_LV2"
      LV2_LOCAL_OPTOBJS="effects/lv2/LoadLV2.o effects/lv2/LV2Effect.o effects/lv2/LV2PortGroup.o"

      AC_MSG_NOTICE([LV2 libraries are available in the local tree])
   else
      LV2_LOCAL_AVAILABLE="no"
      AC_MSG_NOTICE([LV2 libraries are NOT available in the local tree])
   fi

])

AC_DEFUN([AUDACITY_CONFIG_SUBDIRS_LV2], [
   if test "$LV2_USE_LOCAL" = yes; then
      AC_CONFIG_SUBDIRS([lib-src/lv2])
   fi
])
