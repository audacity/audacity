dnl I frankly don't know what's supposed to go there
# audacity_checklib_portsmf.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_PORTMIDI], [

   AC_ARG_WITH(portmidi,
               [AS_HELP_STRING([--with-portmidi],
                               [use PortMIDI for MIDI playback support])],
               PORTMIDI_ARGUMENT=$withval,
               PORTMIDI_ARGUMENT="unspecified")

   dnl see if libportmidi is installed on the system
   PKG_CHECK_MODULES(PORTMIDI, portmidi,
                     PORTMIDI_SYSTEM_AVAILABLE="yes",
                     PORTMIDI_SYSTEM_AVAILABLE="no")

   dnl portmidi may not have a pkg-config script
   if test "$PORTMIDI_SYSTEM_AVAILABLE" = "no"; then

      AC_MSG_NOTICE([checking for portmidi without pkg-config])

      AC_CHECK_HEADER(portmidi.h,
                      header_found="yes",
                      header_found="no")

      PORTMIDI_CFLAGS=""

      case "${host_os}" in
         cygwin* | mingw32*)
            PORTMIDI_LIBS="-lwinmm"
            ;;
         *)
            PORTMIDI_LIBS=""
            ;;
      esac

      if test "$header_found" = "yes"; then
         AC_CHECK_LIB(portmidi,
                      Pm_Initialize,
                      lib_found="yes",
                      lib_found="no",
                      "${PORTMIDI_LIBS}")

         if test "$lib_found" = "yes" -a "$header_found" = "yes"; then
           PORTMIDI_SYSTEM_AVAILABLE="yes"
           PORTMIDI_LIBS="-lportmidi ${PORTMIDI_LIBS}"
         fi
      fi
   fi

   if test "$PORTMIDI_SYSTEM_AVAILABLE" = "yes"; then
      AC_MSG_NOTICE([portmidi library is available as system library])
   else
      AC_MSG_NOTICE([portmidi library is NOT available as system library])
   fi

   if test -f ${srcdir}/lib-src/portmidi/pm_common/portmidi.h; then
      PORTMIDI_LOCAL_AVAILABLE="yes"
   else
      PORTMIDI_LOCAL_AVAILABLE="no"
   fi

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
