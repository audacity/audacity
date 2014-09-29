dnl Add Audacity/WX license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_widgetextra.m4 serial 3

dnl A function to check for the correct presence of lib-widget-extra in the
dnl lib-src tree. Note that this is a mandatory library, and
dnl that because we maintain it, we don't support system copies.

AC_DEFUN([AUDACITY_CHECKLIB_WIDGETEXTRA], [
   AC_ARG_WITH(widgetextra,
               [AS_HELP_STRING([--with-widgetextra],
                               [which libwidgetextra to use (required): [system,local]])],
               WIDGETEXTRA_ARGUMENT=$withval,
               WIDGETEXTRA_ARGUMENT="unspecified")

   dnl see if libwidgetextra is installed on the system

   PKG_CHECK_MODULES(WIDGETEXTRA, libwidgetextra,
                     WIDGETEXTRA_SYSTEM_AVAILABLE="yes",
                     WIDGETEXTRA_SYSTEM_AVAILABLE="no")

   if test "$WIDGETEXTRA_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([libwidgetextra library is available as system library.])
   else
      AC_MSG_NOTICE([libwidgetextra library is NOT available as system library.])
   fi

   dnl see if libwidgetextra is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/lib-widget-extra/NonGuiThread.h,
                 WIDGETEXTRA_LOCAL_AVAILABLE="yes",
                 WIDGETEXTRA_LOCAL_AVAILABLE="no")

   if test "$WIDGETEXTRA_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([libwidgetextra library is available in the local tree])
   else
      AC_MSG_NOTICE([libwidgetextra library is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_WIDGETEXTRA], [
   if test "$WIDGETEXTRA_USE_LOCAL" = yes; then
      WIDGETEXTRA_CFLAGS='-I$(top_srcdir)/lib-src/lib-widget-extra'
      WIDGETEXTRA_LIBS='$(top_builddir)/lib-src/lib-widget-extra/libwidgetextra.la'
      AC_CONFIG_SUBDIRS([lib-src/lib-widget-extra])
   fi

   AC_SUBST([WIDGETEXTRA_CFLAGS])
   AC_SUBST([WIDGETEXTRA_LIBS])

   AM_CONDITIONAL([USE_LOCAL_WIDGETEXTRA], [test "$WIDGETEXTRA_USE_LOCAL" = yes])
])
