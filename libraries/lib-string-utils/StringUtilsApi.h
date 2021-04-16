#pragma once

/* The dynamic library import and export for Microsoft Windows.
 * Supported by Visual Studio and for GCC 4+ */
#if defined _WIN32 || (defined __CYGWIN__ && defined __GNUC__)
#   ifndef STRING_UTILS_API
#       ifdef BUILDING_LIB_STRING_UTILS
#           define STRING_UTILS_API __declspec(dllexport)
#       else
#           ifdef _DLL
#               define STRING_UTILS_API __declspec(dllimport)
#           else
#               define STRING_UTILS_API
#           endif
#       endif
#   endif
#else
#   ifndef STRING_UTILS_API
#       define STRING_UTILS_API __attribute__((visibility("default")))
#   endif
#endif //_WIN32 || (__CYGWIN__ && __GNUC__)
