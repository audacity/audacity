# Process this file with autoconf to produce a configure script.
AC_INIT(RtAudio, 3.0, gary@ccrma.stanford.edu, rtaudio)
AC_CONFIG_SRCDIR(RtAudio.cpp)
AC_CONFIG_FILES([rtaudio-config Makefile tests/Makefile])

# Fill GXX with something before test.
AC_SUBST( GXX, ["no"] )

# Checks for programs.
AC_PROG_CC
AC_PROG_CXX(g++ CC c++ cxx)
AC_PROG_RANLIB
AC_PATH_PROG(AR, ar, no)
if [[ $AR = "no" ]] ; then
    AC_MSG_ERROR("Could not find ar - needed to create a library");
fi

# Checks for libraries.
AC_CHECK_LIB(pthread, pthread_create, , AC_MSG_ERROR(RtAudio requires the pthread library!))

# Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(sys/ioctl.h unistd.h)

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Check for debug
AC_MSG_CHECKING(whether to compile debug version)
AC_ARG_ENABLE(debug,
  [  --enable-debug = enable various debug output],
  [AC_SUBST( debug, [-D__RTAUDIO_DEBUG__] ) AC_SUBST( cflags, [-g] ) AC_SUBST( object_path, [Debug] ) AC_MSG_RESULT(yes)],
  [AC_SUBST( debug, [] ) AC_SUBST( cflags, [-O2] ) AC_SUBST( object_path, [Release] ) AC_MSG_RESULT(no)])

# Check compiler and use -Wall if gnu.
if [test $GXX = "yes" ;] then
  AC_SUBST( warn, [-Wall] )
fi

# Checks for package options and external software
AC_CANONICAL_HOST
AC_MSG_CHECKING(for audio API)
case $host in
  *-*-linux*)
  AC_SUBST( sound_api, [_NO_API_] )
  AC_ARG_WITH(jack, [  --with-jack = choose JACK server support (linux only)], [AC_SUBST( sound_api, [-D__LINUX_JACK__] ) AC_MSG_RESULT(using JACK)], )
  if [test $sound_api = -D__LINUX_JACK__;] then
    TEMP_LIBS=$LIBS
    AC_CHECK_LIB(jack, jack_client_new, , AC_MSG_ERROR(JACK support requires the jack library!))
    AC_CHECK_LIB(asound, snd_pcm_open, , AC_MSG_ERROR(Jack support also requires the asound library!))
    LIBS="`pkg-config --cflags --libs jack` $TEMP_LIBS -lasound"
    audio_apis="-D__LINUX_JACK__"
  fi

  # Look for Alsa flag
  AC_ARG_WITH(alsa, [  --with-alsa = choose native ALSA API support (linux only)], [AC_SUBST( sound_api, [-D__LINUX_ALSA__] ) AC_MSG_RESULT(using ALSA)], )
  if [test $sound_api = -D__LINUX_ALSA__;] then
    AC_CHECK_LIB(asound, snd_pcm_open, , AC_MSG_ERROR(ALSA support requires the asound library!))
    audio_apis="-D__LINUX_ALSA__ $audio_apis"
  fi

  # Look for OSS flag
  AC_ARG_WITH(oss, [  --with-oss = choose OSS API support (linux only)], [AC_SUBST( sound_api, [-D__LINUX_OSS__] ) AC_MSG_RESULT(using OSS)], )
  if test $sound_api = -D__LINUX_OSS__; then
    audio_apis="-D__LINUX_OSS__ $audio_apis"
  fi

  # If no audio api flags specified, use OSS
  if [test $sound_api = _NO_API_;] then
    AC_SUBST( sound_api, [-D__LINUX_OSS__] )
    AC_MSG_RESULT(using OSS)
    AC_SUBST( audio_apis, [-D__LINUX_OSS__] )
  fi
  ;;

  *-sgi*)
  AC_SUBST( audio_apis, ["-D__IRIX_AL__ -LANG:std -w"] )
  AC_MSG_RESULT(using IRIX AL)
  AC_CHECK_LIB(audio, alOpenPort, , AC_MSG_ERROR(IRIX audio support requires the audio library!) )
  ;;

  *-apple*)
  # Check for CoreAudio framework
  AC_CHECK_HEADER(CoreAudio/CoreAudio.h,
    [AC_SUBST( audio_apis, [-D__MACOSX_CORE__] )],
    [AC_MSG_ERROR(CoreAudio header files not found!)] )
  AC_SUBST( frameworks, ["-framework CoreAudio"] )
  AC_CHECK_LIB(stdc++, printf, , AC_MSG_ERROR(RtAudio requires the C++ library!) )
  ;;

  *)
  # Default case for unknown realtime systems.
  AC_MSG_ERROR(Unknown system type for realtime support!)
  ;;
esac

# Checks for library functions.
AC_PROG_GCC_TRADITIONAL

AC_OUTPUT

chmod oug+x rtaudio-config
