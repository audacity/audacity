dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_lv2.m4 serial 3

AC_DEFUN([AUDACITY_CHECKLIB_LV2], [
   AC_ARG_WITH(lv2,
               [AS_HELP_STRING([--with-lv2],
                      [use for adding LV2 plug-in support])],
               LV2_ARGUMENT=$withval,
               LV2_ARGUMENT="unspecified")

   dnl see if lv2 is installed on the system

   PKG_CHECK_MODULES(LV2, [lv2 >= 1.16 lilv-0 >= 0.24.4 suil-0 >= 0.10.4],
                     LV2_SYSTEM_AVAILABLE="yes",
                     LV2_SYSTEM_AVAILABLE="no")

   if test "$LV2_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([LV2 libraries are available as system libraries])
   else
      AC_MSG_NOTICE([LV2 libraries are NOT available as system libraries])
   fi

   dnl see if LV2 is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/lv2/configure,
                 LV2_LOCAL_AVAILABLE="yes",
                 LV2_LOCAL_AVAILABLE="no")

   if test "$LV2_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([LV2 libraries are available in the local tree])
   else
      AC_MSG_NOTICE([LV2 libraries are NOT available in the local tree])
   fi

])

AC_DEFUN([AUDACITY_CONFIG_LV2], [
   if test "$LV2_USE_LOCAL" = yes; then
      LV2_CFLAGS+=' -I$(top_builddir)/lib-src/lv2/build/include'
      LV2_CFLAGS+=' -I$(top_builddir)/lib-src/lv2/build/include/lilv-0'
      LV2_CFLAGS+=' -I$(top_builddir)/lib-src/lv2/build/include/suil-0'
      LV2_LIBS='$(top_builddir)/lib-src/lv2/build/lib/lib*.a'
      AC_CONFIG_SUBDIRS([lib-src/lv2])
   fi

   AC_SUBST([LV2_CFLAGS])
   AC_SUBST([LV2_LIBS])

   AM_CONDITIONAL([USE_LV2], [test "$LV2_USE_LOCAL" = yes -o "$LV2_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_LV2], [test "$LV2_USE_LOCAL" = yes])

   if test "$LV2_USE_LOCAL" = yes -o "$LV2_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_LV2, 1,
                [Define if LV2 support should be enabled])
   fi
])
