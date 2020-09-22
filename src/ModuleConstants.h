/**********************************************************************

  Audacity: A Digital Audio Editor

  ModuleConstants.h

  Paul Licameli split from ModuleManager.h

**********************************************************************/

#ifndef __AUDACITY_MODULE_CONSTANTS__
#define __AUDACITY_MODULE_CONSTANTS__

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

#endif
