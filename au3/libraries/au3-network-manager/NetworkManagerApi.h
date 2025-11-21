/*!********************************************************************

 Audacity: A Digital Audio Editor

 @file NetworkManagerApi.h
 @brief Declare macros for the Network Manager library DLL API

 Dmitry Vedenko
 **********************************************************************/

#pragma once

/* The dynamic library import and export for Microsoft Windows.
 * Supported by Visual Studio and for GCC 4+ */
#if defined _WIN32 || (defined __CYGWIN__ && defined __GNUC__)
#   ifndef NETWORK_MANAGER_API
#       ifdef BUILDING_LIB_NETWORK_MANAGER
#           define NETWORK_MANAGER_API __declspec(dllexport)
#       else
#           ifdef _DLL
#               define NETWORK_MANAGER_API __declspec(dllimport)
#           else
#               define NETWORK_MANAGER_API
#           endif
#       endif
#   endif
#else
#   ifndef NETWORK_MANAGER_API
#       define NETWORK_MANAGER_API __attribute__((visibility("default")))
#   endif
#endif //_WIN32 || (__CYGWIN__ && __GNUC__)
