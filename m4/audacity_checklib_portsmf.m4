dnl Add Audacity / portSMF license?
dnl
dnl Please increment the serial number below whenever you alter this macro
dnl for the benefit of automatic macro update systems
# audacity_checklib_portsmf.m4 serial 1

AC_DEFUN([AUDACITY_CHECKLIB_PORTSMF], [

   AC_ARG_WITH(midi,
               [AS_HELP_STRING([--with-midi],
                               [use portSMF for Midi support])],
               PORTSMF_ARGUMENT=$withval,
               PORTSMF_ARGUMENT="unspecified")

   if false ; then
      AC_DEFINE(USE_MIDI, 1,
                [Define if midi support should be enabled])
   fi

   dnl see if libportsmf is installed on the system

   PKG_CHECK_MODULES(PORTSMF, portSMF,
                     portsmf_available_system="yes",
                     portsmf_available_system="no")

   if test "x$portsmf_available_system" = "xyes" ; then
      PORTSMF_SYSTEM_AVAILABLE="yes"
      PORTSMF_SYSTEM_LIBS="$PORTSMF_LIBS"
      PORTSMF_SYSTEM_CXXFLAGS="$PORTSMF_CFLAGS"
      PORTSMF_SYSTEM_CPPSYMBOLS="USE_MIDI"
      dnl extra objects we can now compile
      PORTSMF_SYSTEM_OPTOBJS="NoteTrack.o import/ImportMIDI.o"
      AC_MSG_NOTICE([portSMF library is available as system library])
   else
      PORTSMF_SYSTEM_AVAILABLE="no"
      AC_MSG_NOTICE([portSMF library is NOT available as system library])
   fi

   AC_CHECK_FILE(${srcdir}/lib-src/portsmf/allegro.h,
                 allegro_h_available="yes",
                 allegro_h_available="no")

   if test "x$allegro_h_available" = "xyes" ; then
      PORTSMF_LOCAL_AVAILABLE="yes"
      PORTSMF_LOCAL_LIBS="libportSMF.a"
      PORTSMF_LOCAL_CXXFLAGS='-I$(top_srcdir)/lib-src/portsmf'
      PORTSMF_LOCAL_CPPSYMBOLS="USE_MIDI"
      PORTSMF_LOCAL_CONFIG_SUBDIRS="lib-src/portsmf"
      dnl extra objects we can now compile
      PORTSMF_LOCAL_OPTOBJS="NoteTrack.o import/ImportMIDI.o"
   else
      PORTSMF_LOCAL_AVAILABLE="no"
   fi
])
