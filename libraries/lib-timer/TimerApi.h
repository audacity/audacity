/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file TimerApi.h
 @brief Declare macros for the Timer library DLL API

 Dmitry Vedenko
 **********************************************************************/

#pragma once

/* The dynamic library import and export for Microsoft Windows.
 * Supported by Visual Studio and for GCC 4+ */
#if defined _WIN32 || (defined __CYGWIN__ && defined __GNUC__)
#   ifndef TIMER_API
#       ifdef BUILDING_LIB_TIMER
#           define TIMER_API __declspec(dllexport)
#       else
#           ifdef _DLL
#               define TIMER_API __declspec(dllimport)
#           else
#               define TIMER_API
#           endif
#       endif
#   endif
#else
#   ifndef TIMER_API
#       define TIMER_API __attribute__((visibility("default")))
#   endif
#endif //_WIN32 || (__CYGWIN__ && __GNUC__)
