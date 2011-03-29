dnl Add Audacity/WX license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_widgetextra.m4 serial 1

dnl A function to check for the correct presence of lib-widget-extra in the 
dnl lib-src tree. Note that this is a mandatory library, and
dnl that because we maintain it, we don't support system copies.

dnl You would have thought you could use pkg-config for this. But the
dnl pkg-config file doesn't exist until configure has been run for
dnl lib-widget-extra. Using AC_CONFIG_SUBDIRS, that doesn't happen until
dnl after everything in the main configure script has happened, so
dnl we can't detect anything about the configured library, because it isn't
dnl configured when this runs.
dnl To get round this we have created our own subdirectory configuration
dnl function, AX_CONFIG_DIR based on a subset of the code that implements
dnl AC_CONFIG_SUBDIRS.

AC_DEFUN([AUDACITY_CHECKLIB_WIDGETEXTRA], [
   dnl we always need to configure libwidgetextra, so just call the script
   dnl regardless.
   AX_CONFIG_DIR(["${srcdir}/lib-src/lib-widget-extra"])
   dnl having done that we get a pkg-config file we can use
   dnl add the directory with lib-widget-extra in to the pkg-config search path
   PKG_CONFIG_PATH="${srcdir}/lib-src/lib-widget-extra/:$PKG_CONFIG_PATH"
   export PKG_CONFIG_PATH
   PKG_CHECK_MODULES(WIDGETEXTRA, libwidgetextra,
                     widgetextra_available="yes",
                     widgetextra_available="no")

   if test "x$widgetextra_available" != "xyes" ; then
      AC_MSG_ERROR([lib-widget-extra is required to build audacity. A copy is included in the audacity source distribution at lib-src/lib-widget-extra/.])
   fi
   dnl otherwise good - got it. Flags will be available for use in
   dnl WIDGETEXTRA_LIBS and friends
])
