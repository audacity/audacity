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
#define AUDACITY_REVISION  3
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


class wxWindow;

#ifdef __WXMAC__
#include "configmac.h"
#endif

#ifdef __WXGTK__
#ifndef __CONFIG_UNIX_INCLUDED
   #define __CONFIG_UNIX_INCLUDED
   #include "configunix.h"
#endif
#endif

#ifdef __WXX11__
#ifndef __CONFIG_UNIX_INCLUDED
   #define __CONFIG_UNIX_INCLUDED
   #include "configunix.h"
#endif
#endif

#ifdef __WXMSW__
#include "configwin.h"
#endif

// This renames a good use of this C++ keyword that we don't need to review when hunting for leaks.
#define PROHIBITED = delete

// Reviewed, certified, non-leaky uses of NEW that immediately entrust their results to RAII objects.
// You may use it in NEW code when constructing a wxWindow subclass with non-NULL parent window.
// You may use it in NEW code when the NEW expression is the constructor argument for a standard smart
// pointer like std::unique_ptr or std::shared_ptr.
#define safenew new

// Define/undefine _DEBUG based on the (CMake provided) NDEBUG symbol
#if defined(NDEBUG)
   #undef _DEBUG
#else
   #define _DEBUG 1
#endif

#endif // __AUDACITY_H__
