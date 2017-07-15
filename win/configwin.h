/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2011 Audacity Team.
   License: GPL v2.  See License.txt.

   configwin.h 
   Dominic Mazzoni, et al

******************************************************************//**

   Microsoft Windows specific include file

*//*******************************************************************/

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

/* Define if MIDI playback support using PortMIDI should be enabled */
#define USE_PORTMIDI 1

/* Define if PortMixer support should be enabled */
#define USE_PORTMIXER 1

/* Define if SBSMS support should be enabled */
#define USE_SBSMS 1

/* Define if SoundTouch support should be enabled */
#define USE_SOUNDTOUCH 1

/* Define if Vamp analysis plugin support should be enabled */
#define USE_VAMP 1

/* Define if VST plug-in support is enabled */
#define USE_VST 1

#define INSTALL_PREFIX "."

#if (_MSC_VER == 1500)
#define rint(x)   (floor((x)+0.5f)) 
#endif

#ifdef _DEBUG
    #ifdef _MSC_VER
        #include <crtdbg.h>
    #endif
#endif

// arch-tag: dcb2defc-1c07-4bae-a9ca-c5377cb470e4

