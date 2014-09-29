dnl Add Audacity / portSMF license?
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_portsmf.m4 serial 2

AC_DEFUN([AUDACITY_CHECKLIB_PORTSMF], [

   AC_ARG_WITH(midi,
               [AS_HELP_STRING([--with-midi],
                               [use portSMF for Midi support])],
               PORTSMF_ARGUMENT=$withval,
               PORTSMF_ARGUMENT="unspecified")

   dnl see if libportsmf is installed on the system

   PKG_CHECK_MODULES(PORTSMF, portSMF,
                     PORTSMF_SYSTEM_AVAILABLE="yes",
                     PORTSMF_SYSTEM_AVAILABLE="no")

   if test "$PORTSMF_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([portSMF library is available as system library])
   else
      AC_MSG_NOTICE([portSMF library is NOT available as system library])
   fi

   AC_CHECK_FILE(${srcdir}/lib-src/portsmf/allegro.h,
                 PORTSMF_LOCAL_AVAILABLE="yes",
                 PORTSMF_LOCAL_AVAILABLE="no")

   if test "$PORTSMF_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([portSMF library is available in the local tree])
   else
      AC_MSG_NOTICE([portSMF library is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_PORTSMF], [
   if test "$PORTSMF_USE_LOCAL" = yes; then
      PORTSMF_CFLAGS='-I$(top_srcdir)/lib-src/portsmf'
      PORTSMF_LIBS='$(top_builddir)/lib-src/portsmf/libportSMF.a'
      AC_CONFIG_SUBDIRS([lib-src/portsmf])
   fi

   AC_SUBST([PORTSMF_CFLAGS])
   AC_SUBST([PORTSMF_LIBS])

   AM_CONDITIONAL([USE_PORTSMF], [test "$PORTSMF_USE_LOCAL" = yes -o "$PORTSMF_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_PORTSMF], [test "$PORTSMF_USE_LOCAL" = yes])

   if test "$PORTSMF_USE_LOCAL" = yes -o "$PORTSMF_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_MIDI, 1,
                [Define if midi support should be enabled])
   fi
])
