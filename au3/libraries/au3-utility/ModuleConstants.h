/**********************************************************************

  Audacity: A Digital Audio Editor

  ModuleConstants.h

  Paul Licameli split from ModuleManager.h

**********************************************************************/

#ifndef __AUDACITY_MODULE_CONSTANTS__
#define __AUDACITY_MODULE_CONSTANTS__

#include <string>

#define ModuleDispatchName "ModuleDispatch"

#ifdef _MSC_VER
   #define DLL_API _declspec(dllexport)
   #define DLL_IMPORT _declspec(dllimport)
#else
   #define DLL_API __attribute__ ((visibility("default")))
   #define DLL_IMPORT
#endif

enum ModuleDispatchTypes
{
    ModuleInitialize,
    ModuleTerminate,
    AppInitialized,
    AppQuiting,
    ProjectInitialized,
    ProjectClosing
};

// Macro generates one of the required entry points of a module
// GetVersionString
// REQUIRED for the module to be accepted by Audacity.
// Without it Audacity will see a version number mismatch.
#define DEFINE_VERSION_CHECK                                           \
    extern "C" {                                                           \
    DLL_API const wchar_t* GetVersionString()                          \
    {                                                                   \
        /* Make sure that this version of the module requires the version \
         of Audacity it is built with.                                    \
         For now, the versions must match exactly for Audacity to         \
         agree to load the module. */                                  \
        return AUDACITY_VERSION_STRING;                                  \
    }                                                                   \
    }

// Macro generates minimal required entry points to load a module
// Use it when you don't care about event notifications from the application
#define DEFINE_MODULE_ENTRIES                                          \
    DEFINE_VERSION_CHECK                                                   \
    extern "C" DLL_API int ModuleDispatch(ModuleDispatchTypes type) { return 1; }

//! This program's name
UTILITY_API extern const std::wstring AppName;

#endif
