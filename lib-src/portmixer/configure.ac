#
# PortMixer 2.0
#

AC_PREREQ([2.60])
AC_INIT([PortMixer], [2.0], [feedback@audacityteam.org], [portmixer])
AC_CONFIG_AUX_DIR([autotools])
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_SRCDIR(src)

AM_INIT_AUTOMAKE([1.11 dist-xz foreign no-dist-gzip -Wall])
AM_MAINTAINER_MODE([disable])

#
# Checks for programs.
#
AC_PROG_CC
AM_PROG_CC_C_O
AM_PROG_AR

LT_INIT

#
# Checks for libraries.
#
AC_HEADER_STDC

#
# Check for debug
#
AC_MSG_CHECKING(whether to compile debug version)
AC_ARG_ENABLE([debug],
  AC_HELP_STRING([--enable-debug],
                 [enable debug compilation]),
  [AC_SUBST( cflags, ["$cflags -g"] )  AC_MSG_RESULT(yes)],
  [AC_SUBST( cflags, ["$cflags -O2"] ) AC_MSG_RESULT(no)])

PKG_CHECK_MODULES([PORTAUDIO], [portaudio-2.0 >= 19])

#
# Check which APIs are available
#

dnl The following tests need to have the header files of portaudio available.
cppflags_save="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $PORTAUDIO_CFLAGS"

have_oss=no
AC_CHECK_HEADERS([sys/soundcard.h linux/soundcard.h machine/soundcard.h], have_oss=yes)
if [[ $have_oss = "yes" ]] ; then
   AC_CHECK_HEADER(pa_unix_oss.h, ,have_oss=no, [#include "portaudio.h"])
   if [[ $have_oss = "yes" ]] ; then
      AC_EGREP_HEADER([PaOSS_GetStreamInputDevice], [pa_unix_oss.h], , [have_oss=no])
      if [[ $have_oss = "no" ]] ; then
         AC_MSG_WARN([Missing PaOSS_GetStreamInputDevice support in pa_unix_oss.h]);
      fi
   fi
fi

AC_CHECK_HEADER(alsa/asoundlib.h, have_alsa=yes, have_alsa=no)
if [[ $have_alsa = "yes" ]] ; then
   AC_CHECK_HEADER(pa_linux_alsa.h, ,have_alsa=no)
   if [[ $have_alsa = "yes" ]] ; then
      AC_EGREP_HEADER([PaAlsa_GetStreamInputCard], [pa_linux_alsa.h], , [have_alsa=no])
      if [[ $have_alsa = "no" ]] ; then
         AC_MSG_WARN([Missing PaAlsa_GetStreamInputCard support in pa_linux_alsa.h]);
      fi
   fi   
fi

AC_CHECK_HEADER(CoreAudio/CoreAudio.h, have_coreaudio=yes, have_coreaudio=no)
if [[ $have_coreaudio = "yes" ]] ; then
   AC_CHECK_HEADER(pa_mac_core.h, ,have_coreaudio=no, [#include "portaudio.h"])
   if [[ $have_alsa = "yes" ]] ; then
      AC_EGREP_HEADER([PaMacCore_GetStreamInputDevice], [pa_mac_core.h], , [have_support=no])
      if [[ $have_support = "no" ]] ; then
         AC_MSG_WARN([Missing support in pa_mac_core.h]);
      fi
   fi
fi

have_wmme=no
have_ds=no
AC_CHECK_HEADER(windows.h, have_windows=yes, have_windows=no)
if [[ $have_windows = "yes" ]] ; then
   AC_CHECK_HEADER(pa_win_wmme.h, have_wmme=yes)
   if [[ $have_wmme = "yes" ]] ; then
      AC_EGREP_HEADER([PaWinMME_GetStreamInputHandle], [pa_win_wmme.h], , [have_wmme=no])
      if [[ $have_wmme = "no" ]] ; then
         AC_MSG_WARN([Missing PaWinMME_GetStreamInputHandle support in pa_win_wmme.h]);
      fi
   fi

   AC_CHECK_HEADER(pa_win_ds.h, have_ds=yes)
   if [[ $have_ds = "yes" ]] ; then
      AC_EGREP_HEADER([PaWinDS_GetStreamInputGUID], [pa_win_ds.h], , [have_ds=no])
      if [[ $have_support = "no" ]] ; then
         AC_MSG_WARN([Missing PaWinDS_GetStreamInputGUID support in pa_win_ds.h]);
      fi
   fi
fi

AC_CHECK_HEADER(portaudio.h, have_portaudio=yes, have_portaudio=no)
if [[ $have_portaudio = "yes" ]] ; then
   AC_EGREP_HEADER([Pa_GetStreamHostApiType], [portaudio.h], , [have_portaudio=no])
   if [[ $have_portaudio = "no" ]] ; then
      AC_MSG_ERROR([Your version of portaudio does not include required Pa_GetStreamHostApiType function]);
   fi
fi

dnl Restore pre-processor flags
CPPFLAGS="$cppflags_save"

#
# Set up to use the identified ones
#
have_support=no
AC_MSG_NOTICE([---------------------------------------])
AM_CONDITIONAL([USE_UNIX_OSS], [test "$have_oss" = yes])
if [[ $have_oss = "yes" ]] ; then
   AC_MSG_NOTICE([Including support for OSS]);
   AC_DEFINE(PX_USE_UNIX_OSS)
   have_support=yes
fi

AM_CONDITIONAL([USE_LINUX_ALSA], [test "$have_alsa" = yes])
if [[ $have_alsa = "yes" ]] ; then
   AC_MSG_NOTICE([Including support for ALSA])
   AC_DEFINE(PX_USE_LINUX_ALSA)
   have_support=yes
fi

AM_CONDITIONAL([USE_MAC_COREAUDIO], [test "$have_coreaudio" = yes])
if [[ $have_coreaudio = "yes" ]] ; then
   AC_MSG_NOTICE([Including support for Coreaudio])
   AC_DEFINE(PX_USE_MAC_COREAUDIO)
   have_support=yes
fi

if [[ $have_support = "no" ]] ; then
   AC_MSG_ERROR([No supported mixer interfaces detected])
fi
AC_MSG_NOTICE([---------------------------------------])

AC_CONFIG_FILES([Makefile px_tests/Makefile src/Makefile])

#
# Write it all out
#
AC_OUTPUT
