/**********************************************************************

   Audacity: A Digital Audio Editor

   Types.h

   Leland Lucius

   Copyright (c) 2014, Audacity Team 
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.
   
**********************************************************************/

#ifndef __AUDACITY_TYPES_H__
#define __AUDACITY_TYPES_H__

#include <wx/string.h>
#include <wx/arrstr.h>

// ----------------------------------------------------------------------------
// TODO:  I'd imagine this header may be replaced by other public headers. But,
//        to try and minimize more changes to the base code, we can use this
//        until proper public headers are created for the stuff in here.
// ----------------------------------------------------------------------------

// ----------------------------------------------------------------------------
// A native 64-bit integer...used when referring to any number of samples
// ----------------------------------------------------------------------------
#if (defined(__VISUALC__) && defined(__WIN32__))
typedef __int64 sampleCount;
#else
typedef long long sampleCount;
#endif

// ----------------------------------------------------------------------------
// Supported sample formats
// ----------------------------------------------------------------------------
typedef enum
{
   int16Sample = 0x00020001,
   int24Sample = 0x00040001,
   floatSample = 0x0004000F
} sampleFormat;

// ----------------------------------------------------------------------------
// Provide the number of bytes a specific sample will take
// ----------------------------------------------------------------------------
#define SAMPLE_SIZE(SampleFormat) (SampleFormat >> 16)

// ----------------------------------------------------------------------------
// Generic pointer to sample data
// ----------------------------------------------------------------------------
typedef char *samplePtr;
typedef const char *constSamplePtr;

// ----------------------------------------------------------------------------
// The type for plugin IDs
// ----------------------------------------------------------------------------
typedef wxString PluginID;

// ----------------------------------------------------------------------------
// Supported channel assignments
// ----------------------------------------------------------------------------

typedef enum
{
   // Use to mark end of list
   ChannelNameEOL = -1,
   // The default channel assignment
   ChannelNameMono,
   // From this point, the channels follow the 22.2 surround sound format
   ChannelNameFrontLeft,
   ChannelNameFrontRight,
   ChannelNameFrontCenter,
   ChannelNameLowFrequency1,
   ChannelNameBackLeft,
   ChannelNameBackRight,
   ChannelNameFrontLeftCenter,
   ChannelNameFrontRightCenter,
   ChannelNameBackCenter,
   ChannelNameLowFrequency2,
   ChannelNameSideLeft,
   ChannelNameSideRight,
   ChannelNameTopFrontLeft,
   ChannelNameTopFrontRight,
   ChannelNameTopFrontCenter,
   ChannelNameTopCenter,
   ChannelNameTopBackLeft,
   ChannelNameTopBackRight,
   ChannelNameTopSideLeft,
   ChannelNameTopSideRight,
   ChannelNameTopBackCenter,
   ChannelNameBottomFrontCenter,
   ChannelNameBottomFrontLeft,
   ChannelNameBottomFrontRight,
} ChannelName, *ChannelNames;

// LLL FIXME: Until a complete API is devised, we have to use
//            AUDACITY_DLL_API when defining API classes.  This
//            it ugly, but a part of the game.  Remove it when
//            the API is complete.

#if !defined(AUDACITY_DLL_API)
   // This was copied from "Audacity.h" so these headers wouldn't have
   // to include it.

   /* Magic for dynamic library import and export. This is unfortunately
    * compiler-specific because there isn't a standard way to do it. Currently it
    * works with the Visual Studio compiler for windows, and for GCC 4+. Anything
    * else gets all symbols made public, which gets messy */
   /* The Visual Studio implementation */
   #ifdef _MSC_VER
      #ifndef AUDACITY_DLL_API
         #ifdef BUILDING_AUDACITY
            #define AUDACITY_DLL_API _declspec(dllexport)
         #else
            #ifdef _DLL
               #define AUDACITY_DLL_API _declspec(dllimport)
            #else
               #define AUDACITY_DLL_API
            #endif
         #endif
      #endif
   #endif //_MSC_VER

   #ifdef __GNUC__
   #include "configunix.h"
   #endif

   /* The GCC-elf implementation */
   #ifdef HAVE_VISIBILITY // this is provided by the configure script, is only
   // enabled for suitable GCC versions
   /* The incantation is a bit weird here because it uses ELF symbol stuff. If we
    * make a symbol "default" it makes it visible (for import or export). Making it
    * "hidden" means it is invisible outside the shared object. */
      #ifndef AUDACITY_DLL_API
         #ifdef BUILDING_AUDACITY
            #define AUDACITY_DLL_API __attribute__((visibility("default")))
         #else
            #define AUDACITY_DLL_API __attribute__((visibility("default")))
         #endif
      #endif
   #endif

   /* The GCC-win32 implementation */
   // bizzarely, GCC-for-win32 supports Visual Studio style symbol visibility, so
   // we use that if building on Cygwin
   #if defined __CYGWIN__ && defined __GNUC__
      #ifndef AUDACITY_DLL_API
         #ifdef BUILDING_AUDACITY
            #define AUDACITY_DLL_API _declspec(dllexport)
         #else
            #ifdef _DLL
               #define AUDACITY_DLL_API _declspec(dllimport)
            #else
               #define AUDACITY_DLL_API
            #endif
         #endif
      #endif
   #endif
#endif

#endif // __AUDACITY_TYPES_H__
