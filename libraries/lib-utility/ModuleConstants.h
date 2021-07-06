/**********************************************************************

  Sneedacity: A Digital Audio Editor

  ModuleConstants.h

  Paul Licameli split from ModuleManager.h

**********************************************************************/

#ifndef __SNEEDACITY_MODULE_CONSTANTS__
#define __SNEEDACITY_MODULE_CONSTANTS__

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
// REQUIRED for the module to be accepted by Sneedacity.
// Without it Sneedacity will see a version number mismatch.
#define DEFINE_VERSION_CHECK                                           \
extern "C" {                                                           \
   DLL_API const wchar_t * GetVersionString()                          \
   {                                                                   \
     /* Make sure that this version of the module requires the version \
      of Sneedacity it is built with.                                    \
      For now, the versions must match exactly for Sneedacity to         \
      agree to load the module. */                                     \
      return SNEEDACITY_VERSION_STRING;                                  \
   }                                                                   \
}

// Macro generates minimal required entry points to load a module
// Use it when you don't care about event notifications from the application
#define DEFINE_MODULE_ENTRIES                                          \
DEFINE_VERSION_CHECK                                                   \
extern "C" DLL_API int ModuleDispatch(ModuleDispatchTypes type){ return 1; }

#endif
