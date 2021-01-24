/**********************************************************************

  Audacity: A Digital Audio Editor

  ModNullCallback.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

********************************************************************//**

\class ModNullCallback
\brief ModNullCallback is a class containing all the callback 
functions for this demonstartion module.  These functions are 
added into the standard Audacity Project Menus.

*//*****************************************************************//**

\class ModNullCommandFunctor
\brief We create one of these functors for each menu item or 
command which we register with the Command Manager.  These take the 
click from the menu into the actual function to be called.

*//********************************************************************/

#include <wx/wx.h>
#include "ModNullCallback.h"
#include "Audacity.h"
#include "ModuleManager.h"
#include "ShuttleGui.h"
#include "Project.h"
#include "commands/CommandManager.h"
#include "CommonCommandFlags.h"

/*
There are several functions that can be used in a GUI module.

//#define versionFnName   "GetVersionString"
If the version is wrong, the module will be rejected.
That is it will be loaded and then unloaded.

//#define ModuleDispatchName "ModuleDispatch"
The most useful function.  See the example in this 
file.  It has several cases/options in it.

//#define scriptFnName    "RegScriptServerFunc"
This function is run from a non gui thread.  It was originally 
created for the benefit of mod-script-pipe.

//#define mainPanelFnName "MainPanelFunc"
This function is the hijacking function, to take over Audacity
and replace the main project window with our own wxFrame.

*/

#ifdef _MSC_VER
   #define DLL_API _declspec(dllexport)
   #define DLL_IMPORT _declspec(dllimport)
#else
   #define DLL_API __attribute__ ((visibility("default")))
   #define DLL_IMPORT
#endif

// derived from wxFrame as it needs to be some kind of event handler.
class ModNullCallback : public wxFrame
{
public:
// Name these OnFuncXXX functions by whatever they will do.
   void OnFuncFirst(const CommandContext &);
   void OnFuncSecond(const CommandContext &);
};

void ModNullCallback::OnFuncFirst(const CommandContext &)
{
   int k=32;
}

void ModNullCallback::OnFuncSecond(const CommandContext &)
{
   int k=42;
}
ModNullCallback * pModNullCallback=NULL;

#define ModNullFN(X) static_cast<CommandFunctorPointer>((&ModNullCallback:: X))

extern "C" {

static CommandHandlerObject &ident(AudacityProject&project)
{
// no specific command handler object ...  use the project.
return project;
}

// GetVersionString
// REQUIRED for the module to be accepted by Audacity.
// Without it Audacity will see a version number mismatch.
extern DLL_API const wxChar * GetVersionString(); 
const wxChar * GetVersionString()
{
   // Make sure that this version of the module requires the version 
   // of Audacity it is built with. 
   // For now, the versions must match exactly for Audacity to 
   // agree to load the module.
   return AUDACITY_VERSION_STRING;
}

namespace {
void RegisterMenuItems()
{
   // Get here only after the module version check passes
   using namespace MenuTable;
   // We add two new commands into the Analyze menu.
   static AttachedItem sAttachment{ wxT("Analyze"),
      ( FinderScope( ident ), Section( wxT("NullModule"),
         Command(
            _T("A New Command"), // internal name
            XXO("1st Experimental Command..."), //displayed name
            ModNullFN( OnFuncFirst ),
            AudioIONotBusyFlag() ),
         Command(
            _T("Another New Command"),
            XXO("2nd Experimental Command"),
            ModNullFN( OnFuncSecond ),
            AudioIONotBusyFlag() )
      ) )
   };
}
}

// This is the function that connects us to Audacity.
extern int DLL_API ModuleDispatch(ModuleDispatchTypes type);
int ModuleDispatch(ModuleDispatchTypes type)
{
   switch (type)
   {
   case ModuleInitialize:
      RegisterMenuItems();
      break;
   case AppInitialized:
      break;
   case AppQuiting:
      break;
   default:
      break;
   }

   return 1;
}

//Example code commented out.
#if 0
// This is an example function to hijack the main panel
int DLL_API MainPanelFunc(int ix)
{
   ix=ix;//compiler food
   // If we wanted to hide Audacity's Project, 
   // we'd create a new wxFrame right here and return a pointer to it
   // as our return result.

// Don't hijack the main panel, just return a NULL;
   return NULL;
}
#endif

} // End extern "C"
