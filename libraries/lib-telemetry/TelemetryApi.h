#pragma once

/* The dynamic library import and export for Microsoft Windows.
 * Supported by Visual Studio and for GCC 4+ */
#if defined _WIN32 || (defined __CYGWIN__ && defined __GNUC__)
#   ifndef TELEMETRY_API
#       ifdef BUILDING_LIB_TELEMETRY
#           define TELEMETRY_API __declspec(dllexport)
#       else
#           ifdef _DLL
#               define TELEMETRY_API __declspec(dllimport)
#           else
#               define TELEMETRY_API
#           endif
#       endif
#   endif
#else
#   ifndef TELEMETRY_API
#       define TELEMETRY_API __attribute__((visibility("default")))
#   endif
#endif //_WIN32 || (__CYGWIN__ && __GNUC__)
