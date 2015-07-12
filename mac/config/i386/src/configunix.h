/* src/configunix.h.  Generated from configtemplate.h by configure.  */
/* src/configtemplate.h.  Generated from configure.ac by autoheader.  */

/* define if Audacity is being installed under a name other than "audacity",
   so it can find the files it needs at runtime */
#define AUDACITY_NAME "audacity"

/* Define we are compiling Audacity itself, not an Audacity plug-in */
#define BUILDING_AUDACITY 1

/* Use system FFmpeg library and disable dynamic loading of it. */
/* #undef DISABLE_DYNAMIC_LOADING_FFMPEG */

/* Define if LAME should be linked at compile time */
/* #undef DISABLE_DYNAMIC_LOADING_LAME */

/* Define to 1 if translation of program messages to the user's native
   language is requested. */
/* #undef ENABLE_NLS */

/* Define to enable sse */
#define ENABLE_SSE 1

/* Define to 1 if you have the <alloca.h> header file. */
#define HAVE_ALLOCA_H 1

/* Define to 1 if you have the MacOS X function CFLocaleCopyCurrent in the
   CoreFoundation framework. */
#define HAVE_CFLOCALECOPYCURRENT 1

/* Define to 1 if you have the MacOS X function CFPreferencesCopyAppValue in
   the CoreFoundation framework. */
#define HAVE_CFPREFERENCESCOPYAPPVALUE 1

/* Define to 1 if you have the `clock_gettime' function. */
/* #undef HAVE_CLOCK_GETTIME */

/* Define if the GNU dcgettext() function is already present or preinstalled.
   */
/* #undef HAVE_DCGETTEXT */

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define if the GNU gettext() function is already present or preinstalled. */
/* #undef HAVE_GETTEXT */

/* Define if GTK is available */
/* #undef HAVE_GTK */

/* Define if you have the iconv() function and it works. */
#define HAVE_ICONV 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the <libudev.h> header file. */
/* #undef HAVE_LIBUDEV_H */

/* Define if you have C99's lrint function. */
#define HAVE_LRINT 1

/* Define if you have C99's lrintf function. */
#define HAVE_LRINTF 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `nanosleep' function. */
/* #undef HAVE_NANOSLEEP */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 or 0, depending whether the compiler supports simple visibility
   declarations. */
#define HAVE_VISIBILITY 1

/* define as prefix where Audacity is installed */
#define INSTALL_PREFIX "/usr/local/"

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* Name of package */
#define PACKAGE "audacity"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT ""

/* Define to the full name of this package. */
#define PACKAGE_NAME "audacity"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "audacity 2.0.6"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "audacity"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "2.0.6"

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Define if Audio Unit plug-ins are enabled (Mac OS X only) */
#define USE_AUDIO_UNITS 1

/* Define if ffmpeg (multi-format import and export) support should be enabled
   */
#define USE_FFMPEG 1

/* Define if GStreamer 1 is present */
/* #undef USE_GSTREAMER */

/* Define if LADSPA plug-ins are enabled */
#define USE_LADSPA 1

/* Define if the FLAC library is present */
#define USE_LIBFLAC 1

/* Define if libid3tag is present */
#define USE_LIBID3TAG 1

/* Define if mp3 support is implemented with the libmad library */
#define USE_LIBMAD 1

/* Define if libtwolame (MP2 export) support should be enabled */
#define USE_LIBTWOLAME 1

/* Define if the ogg vorbis decoding library is present */
#define USE_LIBVORBIS 1

/* Define if LV2 support should be enabled */
#define USE_LV2 1

/* Define if midi support should be enabled */
#define USE_MIDI 1

/* Define if Nyquist support should be enabled */
#define USE_NYQUIST 1

/* Define if PortMixer support should be enabled */
#define USE_PORTMIXER 1

/* Define if QuickTime importing is enabled (Mac OS X only) */
#define USE_QUICKTIME 1

/* Define if SBSMS support should be enabled */
#define USE_SBSMS 1

/* Define if SoundTouch support should be enabled */
#define USE_SOUNDTOUCH 1

/* Define if Vamp analysis plugin support should be enabled */
#define USE_VAMP 1

/* Define if VST plug-in support is enabled */
#define USE_VST 1

/* Version number of package */
#define VERSION "2.0.6"

/* Placeholder for large file support */
/* #undef _FILE_OFFSET_BITS */

/* We're using cygwin */
/* #undef __CYGWIN__ */

/* Define to empty if `const' does not conform to ANSI C. */
/* #undef const */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */
