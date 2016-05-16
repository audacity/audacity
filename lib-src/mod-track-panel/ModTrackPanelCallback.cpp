/**********************************************************************

  Audacity: A Digital Audio Editor

  ModTrackPanelCallback.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

********************************************************************//**

\class ModTrackPanelCallback
\brief ModTrackPanelCallback is a class containing all the callback 
functions for the second generation TrackPanel.  These functions are 
added into the standard Audacity Project Menus.

*//*****************************************************************//**

\class ModTrackPanelCommandFunctor
\brief We create one of these functors for each menu item or 
command which we register with the Command Manager.  These take the 
click from the menu into the actual function to be called.

*//********************************************************************/

#include <wx/wx.h>
#include "ModTrackPanelCallback.h"
#include "Audacity.h"
#include "ShuttleGui.h"
#include "Project.h"
#include "ModuleManager.h"
#include "Registrar.h"
#include "TrackPanel2.h"

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

// The machinery here is somewhat overkill for what we need.
// It allows us to add lots of menu and other actions into Audacity.
// We need to jump through these hoops even if only adding
// two menu items into Audacity.

// The OnFunc functrions are functions which can be invoked 
// by Audacity.  Mostly they are for menu items.  They could
// be for buttons.  They could be for commands invokable by
// script (but no examples of that yet).
class ModTrackPanelCallback
{
public:
   void OnFuncShowAudioExplorer();
   void OnFuncReplaceTrackPanel();
};

typedef void (ModTrackPanelCallback::*ModTrackPanelCommandFunction)();

// We have an instance of this CommandFunctor for each 
// instance of a command we attach to Audacity.
// Although the commands have void argument,
// they do receive an instance of ModTrackPanelCallback as a 'this', 
// so if we want to, we can pass data to each function instance.
class ModTrackPanelCommandFunctor:public CommandFunctor
{
public:
   ModTrackPanelCommandFunctor(ModTrackPanelCallback *pData,
      ModTrackPanelCommandFunction pFunction);
   virtual void operator()(int index = 0, const wxEvent * evt=NULL);
public:
   ModTrackPanelCallback * mpData;
   ModTrackPanelCommandFunction mpFunction;
};

// If pData is NULL we will later be passing a NULL 'this' to pFunction.
// This may be quite OK if the class function is written as if it
// could be static.
ModTrackPanelCommandFunctor::ModTrackPanelCommandFunctor(ModTrackPanelCallback *pData,
      ModTrackPanelCommandFunction pFunction)
{
   mpData = pData;
   mpFunction = pFunction;
}

// The dispatching function in the command functor.
void ModTrackPanelCommandFunctor::operator()(int index, const wxEvent * WXUNUSED(evt) )
{
   (mpData->*(mpFunction))();
}
                              
#define ModTrackPanelFN(X) new ModTrackPanelCommandFunctor(pModTrackPanelCallback, \
   (ModTrackPanelCommandFunction)(&ModTrackPanelCallback::X)) 


void ModTrackPanelCallback::OnFuncShowAudioExplorer()
{
   int k=3;
   Registrar::ShowNewPanel();
}

void ModTrackPanelCallback::OnFuncReplaceTrackPanel()
{
   // Upgrade the factory.  Now all TrackPanels will be created as TrackPanel 2's

#if 0
   AudacityProject *p = GetActiveProject();
   wxASSERT( p!= NULL );
   // Change it's type (No new storage allocated).
   TrackPanel2::Upgrade( &p->mTrackPanel );
   int k=4;
#endif
}

// Oooh look, we're using a NULL object, and hence a NULL 'this'.
// That's OK.
ModTrackPanelCallback * pModTrackPanelCallback=NULL;

//This is the DLL related machinery that actually gets called by Audacity
//as prt of loading and using a DLL.
//It is MUCH simpler to use C for this interface because then the
//function names are not 'mangled'.
//The function names are important, because they are what Audacity looks
//for.  Change the name and they won't be found.
//Change the signature, e.g. return type, and you probably have a crash.
extern "C" {
// GetVersionString
// REQUIRED for the module to be accepted by Audacity.
// Without it Audacity will see a version number mismatch.
MOD_TRACK_PANEL_DLL_API wxChar * GetVersionString()
{
   // Make sure that this version of the module requires the version 
   // of Audacity it is built with. 
   // For now, the versions must match exactly for Audacity to 
   // agree to load the module.
   return AUDACITY_VERSION_STRING;
}

// This is the function that connects us to Audacity.
MOD_TRACK_PANEL_DLL_API int ModuleDispatch(ModuleDispatchTypes type)
{
   switch (type)
   {
   case AppInitialized:
      Registrar::Start();
      // Demand that all track panels be created using the TrackPanel2Factory.
      TrackPanel::FactoryFunction = TrackPanel2Factory;
      break;
   case AppQuiting:
      Registrar::Finish();
      break;
   case ProjectInitialized:
   case MenusRebuilt:
      {
         AudacityProject *p = GetActiveProject();
         if( p== NULL )
            return 0;

         wxMenuBar * pBar = p->GetMenuBar();
         wxMenu * pMenu = pBar->GetMenu( 7 );  // Menu 7 is the Analyze Menu.
         CommandManager * c = p->GetCommandManager();

         c->SetToMenu( pMenu );
         c->AddSeparator();
         // We add two new commands into the Analyze menu.
         c->AddItem( _T("Extra Dialog..."), _T("Experimental Extra Dialog for whatever you want."),
            ModTrackPanelFN( OnFuncShowAudioExplorer ) );
         //Second menu tweak no longer needed as we always make TrackPanel2's.
         //c->AddItem( _T("Replace TrackPanel..."), _T("Replace Current TrackPanel with TrackPanel2"),
         //   ModTrackPanelFN( OnFuncReplaceTrackPanel ) );
      }
      break;
   default:
      break;
   }

   return 1;
}

//Example code commented out.
#if 1
// This is an example function to hijack the main panel
int MOD_TRACK_PANEL_DLL_API MainPanelFunc(int ix)
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
