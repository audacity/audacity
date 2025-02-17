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
#include "ModuleConstants.h"
#include "ShuttleGui.h"
#include "Project.h"
#include "MenuRegistry.h"
#include "CommonCommandFlags.h"

/*
//#define ModuleDispatchName "ModuleDispatch"
See the example in this file.  It has several cases/options in it.
*/

// derived from wxFrame as it needs to be some kind of event handler.
class ModNullCallback : public wxFrame
{
public:
// Name these OnFuncXXX functions by whatever they will do.
    void OnFuncFirst(const CommandContext&);
    void OnFuncSecond(const CommandContext&);
};

void ModNullCallback::OnFuncFirst(const CommandContext&)
{
    int k=32;
}

void ModNullCallback::OnFuncSecond(const CommandContext&)
{
    int k=42;
}

ModNullCallback* pModNullCallback=NULL;

#define ModNullFN(X) (&ModNullCallback:: X)

extern "C" {
static CommandHandlerObject& ident(AudacityProject& project)
{
// no specific command handler object ...  use the project.
    return project;
}

DEFINE_VERSION_CHECK

namespace {
void RegisterMenuItems()
{
    // Get here only after the module version check passes
    using namespace MenuRegistry;
    // We add two new commands into the Analyze menu.
    static AttachedItem sAttachment{ wxT("Analyze"),
                                     (FinderScope(ident), Section(wxT("NullModule"),
                                                                  Command(
                                                                      _T("A New Command"), // internal name
                                                                      XXO("1st Experimental Command..."), //displayed name
                                                                      ModNullFN(OnFuncFirst),
                                                                      AudioIONotBusyFlag()),
                                                                  Command(
                                                                      _T("Another New Command"),
                                                                      XXO("2nd Experimental Command"),
                                                                      ModNullFN(OnFuncSecond),
                                                                      AudioIONotBusyFlag())
                                                                  ))
    };
}
}

// This is the function that connects us to Audacity.
extern int DLL_API ModuleDispatch(ModuleDispatchTypes type);
int ModuleDispatch(ModuleDispatchTypes type)
{
    switch (type) {
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
