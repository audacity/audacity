/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2015 Audacity Team.
   License: GPL v2.  See License.txt.

   Audacity.h

   Dominic Mazzoni
   Joshua Haberman
   Vaughan Johnson
   et alii

********************************************************************//*!

\file Audacity.h

  This is the main include file for Audacity.  All files which need
  any Audacity-specific \#defines or need to access any of Audacity's
  global functions should \#include this file.

*//********************************************************************/

#ifndef __AUDACITY_H__
#define __AUDACITY_H__

// If building with GNU compiler, then must be 4.9 or later.
// TODO: This would be much nicer as a standalone test in configure.ac
#if !defined(__APPLE__) && !defined(__clang__) && \
    defined __GNUC__ && ( __GNUC__ < 4 || (__GNUC__ == 4 && __GNUC_MINOR__ < 9))

    #error Audacity requires at least GCC 4.9
#endif


// We only do alpha builds, beta builds, and release versions.
// Most of the time we're in development, so AUDACITY_BUILD_LEVEL should be
// defined to 0.
// Its value may be more than 0 for pre-release "Beta" builds that differ only
// in the welcome screen, and hiding of some development menu commands, but
// still link to the alpha manual online.
#define AUDACITY_BUILD_LEVEL 0

// used #ifdef not #if for IS_ALPHA, IS_BETA, IS_RELEASE, USE_ALPHA_MANUAL
#undef IS_ALPHA
#undef IS_BETA
#undef IS_RELEASE
#undef USE_ALPHA_MANUAL

#if AUDACITY_BUILD_LEVEL == 0
   #define IS_ALPHA
   #define USE_ALPHA_MANUAL
#elif AUDACITY_BUILD_LEVEL == 1
   #define IS_BETA
   #define USE_ALPHA_MANUAL
#else
   #define IS_RELEASE
#endif



// Increment as appropriate every time we release a NEW version.
#define AUDACITY_VERSION   3
#define AUDACITY_RELEASE   0
#define AUDACITY_REVISION  1
#define AUDACITY_MODLEVEL  0

#if defined(IS_BETA)
   #define AUDACITY_SUFFIX wxT("-beta-") __TDATE__
#elif defined(IS_ALPHA)
   #define AUDACITY_SUFFIX wxT("-alpha-") __TDATE__
#else
   #define AUDACITY_SUFFIX    wxT("") // for a stable release
   //#define AUDACITY_SUFFIX wxT("x  ") __TDATE__
#endif

#define AUDACITY_MAKESTR( x ) #x
#define AUDACITY_QUOTE( x ) AUDACITY_MAKESTR( x )

// Version string for visual display
#define AUDACITY_VERSION_STRING wxT( AUDACITY_QUOTE( AUDACITY_VERSION ) ) wxT(".") \
                                wxT( AUDACITY_QUOTE( AUDACITY_RELEASE ) ) wxT(".") \
                                wxT( AUDACITY_QUOTE( AUDACITY_REVISION ) ) \
                                AUDACITY_SUFFIX

// DA: x on end of version string.
#ifdef EXPERIMENTAL_DA
// Version string for file info (under Windows)
#define AUDACITY_FILE_VERSION AUDACITY_QUOTE( AUDACITY_VERSION ) "," \
                              AUDACITY_QUOTE( AUDACITY_RELEASE ) "," \
                              AUDACITY_QUOTE( AUDACITY_REVISION ) "," \
                              AUDACITY_QUOTE( AUDACITY_MODLEVEL ) " x"
#else
#define AUDACITY_FILE_VERSION AUDACITY_QUOTE( AUDACITY_VERSION ) "," \
                              AUDACITY_QUOTE( AUDACITY_RELEASE ) "," \
                              AUDACITY_QUOTE( AUDACITY_REVISION ) "," \
                              AUDACITY_QUOTE( AUDACITY_MODLEVEL )
#endif


// Increment this every time the prefs need to be reset
// the first part (before the r) indicates the version the reset took place
// the second part (after the r) indicates the number of times the prefs have been reset within the same version
#define AUDACITY_PREFS_VERSION_STRING "1.1.1r1"

// Don't change this unless the file format changes
// in an irrevocable way
#define AUDACITY_FILE_FORMAT_VERSION "1.3.0"

class wxWindow;

// Please try to support unlimited path length instead of using PLATFORM_MAX_PATH!
// Define one constant for maximum path value, so we don't have to do
// platform-specific conditionals everywhere we want to check it.
#define PLATFORM_MAX_PATH 260 // Play it safe for default, with same value as Windows' MAX_PATH.

#ifdef __WXMAC__
#include "configmac.h"
#undef PLATFORM_MAX_PATH
#define PLATFORM_MAX_PATH PATH_MAX
#endif

#ifdef __WXGTK__
#ifndef __CONFIG_UNIX_INCLUDED
   #define __CONFIG_UNIX_INCLUDED
   #include "configunix.h"
#endif

// Some systems do not restrict the path length and therefore PATH_MAX is undefined
#ifdef PATH_MAX
#undef PLATFORM_MAX_PATH
#define PLATFORM_MAX_PATH PATH_MAX
#endif
#endif

#ifdef __WXX11__
#ifndef __CONFIG_UNIX_INCLUDED
   #define __CONFIG_UNIX_INCLUDED
   #include "configunix.h"
#endif
// wxX11 should also get the platform-specific definition of PLATFORM_MAX_PATH, so do not declare here.
#endif

#ifdef __WXMSW__
#include "configwin.h"
#undef PLATFORM_MAX_PATH
#define PLATFORM_MAX_PATH MAX_PATH
#endif

/* The dynamic library import and export for Microsoft Windows.
 * Supported by Visual Studio and for GCC 4+ */
#if defined _WIN32 || (defined __CYGWIN__ && defined __GNUC__)
   #ifndef AUDACITY_DLL_API
      #ifdef BUILDING_AUDACITY
         #define AUDACITY_DLL_API __declspec(dllexport)
      #else
         #ifdef _DLL
            #define AUDACITY_DLL_API __declspec(dllimport)
         #else
            #define AUDACITY_DLL_API
         #endif
      #endif
   #endif
#endif //_WIN32 || (__CYGWIN__ && __GNUC__)

// Put extra symbol information in the release build, for the purpose of gathering
// profiling information (as from Windows Process Monitor), when there otherwise
// isn't a need for AUDACITY_DLL_API.
#ifdef IS_ALPHA
   #define PROFILE_DLL_API AUDACITY_DLL_API
#else
   #define PROFILE_DLL_API
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

// These macros are used widely, so declared here.
#define QUANTIZED_TIME(time, rate) (floor(((double)(time) * (rate)) + 0.5) / (rate))
// dB - linear amplitude conversions
#define DB_TO_LINEAR(x) (pow(10.0, (x) / 20.0))
#define LINEAR_TO_DB(x) (20.0 * log10(x))

#define MAX_AUDIO (1. - 1./(1<<15))
#define JUST_BELOW_MAX_AUDIO (1.f - 1.f/(1<<14))


// This renames a good use of this C++ keyword that we don't need to review when hunting for leaks.
#define PROHIBITED = delete

// Reviewed, certified, non-leaky uses of NEW that immediately entrust their results to RAII objects.
// You may use it in NEW code when constructing a wxWindow subclass with non-NULL parent window.
// You may use it in NEW code when the NEW expression is the constructor argument for a standard smart
// pointer like std::unique_ptr or std::shared_ptr.
#define safenew new

// Right to left languages fail in many wx3 dialogs with missing buttons.
// The workaround is to use LTR in those dialogs.
#ifndef __WXMAC__
#define RTL_WORKAROUND( pWnd ) \
   if ( gPrefs->Read( "/GUI/RtlWorkaround", true) ) \
       pWnd->SetLayoutDirection(wxLayout_LeftToRight); 
#else
   #define RTL_WORKAROUND( pWnd ) 
#endif

// Define/undefine _DEBUG based on the (CMake provided) NDEBUG symbol
#if defined(NDEBUG)
   #undef _DEBUG
#else
   #define _DEBUG 1
#endif

#endif // __AUDACITY_H__
