/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2011 Audacity Team.
   License: GPL v2.  See License.txt.

   configwin.h 
   Dominic Mazzoni, et al

******************************************************************//**

   Microsoft Windows specific include file

*//*******************************************************************/


#define MP3SUPPORT 1
#define USE_FFMPEG 1	//define this to build with ffmpeg import/export
#define USE_LADSPA 1
#define USE_LIBFLAC 1
#define USE_LIBID3TAG 1
// #define USE_LIBLRDF 1
#define USE_LIBMAD 1

// Resamplers.
// Only one of each type of resampler (constant or variable rate) should be defined.
// libsoxr is used for constant-rate and var-rate resampling.
// libsoxr currently our only const-rate resampler, so should always be #defined.
#define USE_LIBSOXR 1

// Should build only one of libsoxr, libresample, or libsamplerate for variable-rate resampling, 
// but if more than one are defined, priority is libresample over libsamplerate over libsoxr.
// We cannot release builds with libsamplerate, due to licensing. 
// Standard configuration is to have only USE_LIBSOXR #defined.
#undef USE_LIBRESAMPLE
#undef USE_LIBSAMPLERATE

#define USE_LIBTWOLAME 1
#define USE_LIBVORBIS 1
#define USE_NYQUIST 1
#define USE_PORTMIXER 1
// #define USE_SLV2 1
#define USE_SBSMS 1
#define USE_SOUNDTOUCH 1
#define USE_VAMP 1
#define USE_VST 1
#define USE_MIDI 1 // define this to use portSMF and PortMidi for midi file support

#define INSTALL_PREFIX "."

#define rint(x)   (floor((x)+0.5f)) 

#ifdef _DEBUG
    #ifdef _MSC_VER
        #include <crtdbg.h>
    #endif
#endif

// arch-tag: dcb2defc-1c07-4bae-a9ca-c5377cb470e4

