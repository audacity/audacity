dnl I frankly don't know what's supposed to go there
# audacity_checklib_portsmf.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_PORTMIDI], [

   AC_ARG_WITH(portmidi,
               [AS_HELP_STRING([--with-portmidi],
                               [use PortMIDI for MIDI playback support])],
               PORTMIDI_ARGUMENT=$withval,
               PORTMIDI_ARGUMENT="unspecified")

   dnl see if libportsmf is installed on the system

   PKG_CHECK_MODULES(PORTMIDI, portmidi,
                     PORTMIDI_SYSTEM_AVAILABLE="yes",
                     PORTMIDI_SYSTEM_AVAILABLE="no")

   if test "$PORTMIDI_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([portmidi library is available as system library])
   else
      AC_MSG_NOTICE([portmidi library is NOT available as system library])
   fi

   AC_CHECK_FILE(${srcdir}/lib-src/portmidi/pm_common/portmidi.h,
                 PORTMIDI_LOCAL_AVAILABLE="yes",
                 PORTMIDI_LOCAL_AVAILABLE="no")

   if test "$PORTMIDI_LOCAL_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([portmidi library is available in the local tree])
   else
      AC_MSG_NOTICE([portmidi library is NOT available in the local tree])
   fi
])

AC_DEFUN([AUDACITY_CONFIG_PORTMIDI], [
   if test "$PORTMIDI_USE_LOCAL" = yes; then
      PORTMIDI_CFLAGS='-I$(top_srcdir)/lib-src/portmidi'
      PORTMIDI_LIBS='$(top_builddir)/lib-src/portmidi/libportmidi_s.a'
      AC_CONFIG_SUBDIRS([lib-src/portmidi])
   fi

   AC_SUBST([PORTMIDI_CFLAGS])
   AC_SUBST([PORTMIDI_LIBS])

   AM_CONDITIONAL([USE_PORTMIDI], [test "$PORTMIDI_USE_LOCAL" = yes -o "$PORTMIDI_USE_SYSTEM" = yes])
   AM_CONDITIONAL([USE_LOCAL_PORTMIDI], [test "$PORTMIDI_USE_LOCAL" = yes])

   if test "$PORTMIDI_USE_LOCAL" = yes -o "$PORTMIDI_USE_SYSTEM" = yes; then
      AC_DEFINE(USE_PORTMIDI, 1,
                [Define if midi support should be enabled])
   fi
])
