#
# PortMixer 2.0
#

AC_INIT(PortMixer, 2.0, audacity.sourceforge.net, portmixer)
AC_CONFIG_SRCDIR(src)
AC_CONFIG_FILES([Makefile])

#
# Checks for programs.
#
AC_PROG_CC
AC_PROG_RANLIB
AC_PATH_PROG(AR, ar, no)
if [[ $AR = "no" ]] ; then
    AC_MSG_ERROR([Could not find ar - needed to create a library]);
fi

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

#
# Check for portaudio path
#
AC_ARG_WITH([pa-include],
  AC_HELP_STRING([--with-pa-include],
                 [specify path to portaudio v2.0 include directory]),
  [with_portaudio=$withval],
  [with_portaudio=""])

#
#
#
if test -n "$with_portaudio"; then
   CPPFLAGS="$CPPFLAGS -I$withval"
   AC_SUBST(cflags, ["$cflags -I$withval"] )
fi

#
# Check which APIs are available
#

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

#
#
#
AC_SUBST( include, [-Iinclude] )
AC_SUBST( objects, [px_mixer.o] )

#
# Set up to use the identified ones
#
have_support=no
AC_MSG_NOTICE([---------------------------------------])
if [[ $have_oss = "yes" ]] ; then
   AC_MSG_NOTICE([Including support for OSS]);
   AC_DEFINE(PX_USE_UNIX_OSS)
   objects="$objects px_unix_oss.o"
   have_support=yes
fi

if [[ $have_alsa = "yes" ]] ; then
   AC_MSG_NOTICE([Including support for ALSA])
   AC_DEFINE(PX_USE_LINUX_ALSA)
   objects="$objects px_linux_alsa.o"
   have_support=yes
fi

if [[ $have_coreaudio = "yes" ]] ; then
   AC_MSG_NOTICE([Including support for Coreaudio])
   AC_DEFINE(PX_USE_MAC_COREAUDIO)
   objects="$objects px_mac_coreaudio.o"
   have_support=yes
fi

if [[ $have_support = "no" ]] ; then
   AC_MSG_ERROR([No supported mixer interfaces detected])
fi
AC_MSG_NOTICE([---------------------------------------])

#
# Write it all out
#
AC_OUTPUT
