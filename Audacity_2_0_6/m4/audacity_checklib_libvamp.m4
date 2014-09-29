dnl Todo: add Vamp / Audacity license?
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_libvamp.m4 serial 3

dnl Check for system copy of libvamp we can use for Vamp plug-in support
AC_DEFUN([AUDACITY_CHECKLIB_LIBVAMP], [
   AC_ARG_WITH(libvamp,
               [AS_HELP_STRING([--with-libvamp],
                               [use libvamp for Vamp plug-in support [default=yes]])],
               LIBVAMP_ARGUMENT=$withval,
               LIBVAMP_ARGUMENT="unspecified")

   dnl System may include Vamp headers and library, though we prefer local ones

   PKG_CHECK_MODULES(VAMP, vamp-hostsdk >= 2.0,
                     LIBVAMP_SYSTEM_AVAILABLE="yes",
                     LIBVAMP_SYSTEM_AVAILABLE="no")

   if test "$LIBVAMP_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([Vamp libraries are available as system libraries])
   else
      AC_MSG_NOTICE([Vamp libraries are NOT available as system libraries])
   fi

   dnl see if Vamp is available locally

   AC_CHECK_FILE(${srcdir}/lib-src/libvamp/vamp-hostsdk/PluginLoader.h,
                 LIBVAMP_LOCAL_AVAILABLE="yes",
                 LIBVAMP_LOCAL_AVAILABLE="no")

   if test "$LIBVAMP_LOCAL_AVAILABLE" = "yes"; then
      LIBVAMP_LOCAL_CONFIGURE_ARGS="--disable-programs"
      AC_MSG_NOTICE([Vamp libraries are available in the local tree])
   else
      AC_MSG_NOTICE([Vamp libraries are NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_LIBVAMP], [
   if test "$LIBVAMP_USE_LOCAL" = yes; then
      VAMP_CFLAGS='-I$(top_srcdir)/lib-src/libvamp'
      VAMP_LIBS='$(top_builddir)/lib-src/libvamp/libvamp-hostsdk.a'
      AC_CONFIG_SUBDIRS([lib-src/libvamp])
   fi

   AC_SUBST([VAMP_CFLAGS])
   AC_SUBST([VAMP_LIBS])

   AM_CONDITIONAL([USE_VAMP], [test "$LIBVAMP_USE_LOCAL" = yes -o "$LIBVAMP_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_VAMP], [test "$LIBVAMP_USE_LOCAL" = yes])

   if test "$LIBVAMP_USE_LOCAL" = yes -o "$LIBVAMP_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_VAMP, 1,
                [Define if Vamp analysis plugin support should be enabled])
   fi
])
