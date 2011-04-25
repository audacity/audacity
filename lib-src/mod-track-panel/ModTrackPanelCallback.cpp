/**********************************************************************

  Audacity: A Digital Audio Editor

  ModTrackPanelCallback.cpp

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

********************************************************************//**

\class ModTrackPanelCallback
\brief ModTrackPanelCallback is a class containing all the callback 
functions for the second generation ModTrackPanel.  These functions are 
added into the standard Audacity Project Menus.

*//*****************************************************************//**

\class ModTrackPanelCommandFunctor
\brief We create one of these functors for each menu item or 
command which we register with the Command Manager.  These take the 
click from the menu into the actaul function to be called.

*//********************************************************************/

#include <wx/wx.h>
#include "ModTrackPanelCallback.h"
#include "../../src/Audacity.h"
#include "../../src/ShuttleGui.h"
#include "../../src/Project.h"
#include "../../src/LoadModules.h"

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

class ModTrackPanelCallback
{
public:
   void OnFuncShow();
   void OnFuncHide();
};

typedef void (ModTrackPanelCallback::*ModTrackPanelCommandFunction)();

class ModTrackPanelCommandFunctor:public CommandFunctor
{
public:
   ModTrackPanelCommandFunctor(ModTrackPanelCallback *pData,
      ModTrackPanelCommandFunction pFunction);
   virtual void operator()(int index = 0);
public:
   ModTrackPanelCallback * mpData;
   ModTrackPanelCommandFunction mpFunction;
};

ModTrackPanelCommandFunctor::ModTrackPanelCommandFunctor(ModTrackPanelCallback *pData,
      ModTrackPanelCommandFunction pFunction)
{
   mpData = pData;
   mpFunction = pFunction;
}

// The dispatching function.
void ModTrackPanelCommandFunctor::operator()(int index )
{
   (mpData->*(mpFunction))();
}
                              
#define ModTrackPanelFN(X) new ModTrackPanelCommandFunctor(pModTrackPanelCallback, \
   (ModTrackPanelCommandFunction)(&ModTrackPanelCallback::X)) 


void ModTrackPanelCallback::OnFuncShow()
{
   int k=3;
}

void ModTrackPanelCallback::OnFuncHide()
{
   int k=4;
}

ModTrackPanelCallback * pModTrackPanelCallback=NULL;

extern "C" {
// GetVersionString
// REQUIRED for the module to be accepted by Audacity.
// Without it Audacity will see a version number mismatch.
MOD_TRACK_PANEL_DLL_API wchar_t * GetVersionString()
{
   // Make sure that this version of the module requires the version 
   // of Audacity it is built with. 
   // For now, the versions must match exactly for Audacity to 
   // agree to load the module.
   return AUDACITY_VERSION_STRING;
}

// This is the function that connects us to Audacity.
int MOD_TRACK_PANEL_DLL_API ModuleDispatch(ModuleDispatchTypes type)
{
   switch (type)
   {
   case AppInitialized:
      break;
   case AppQuiting:
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
         c->AddItem( _T("Audio Explorer..."), _T("Experimental GUI for audio analysis"),
            ModTrackPanelFN( OnFuncShow ) );
         c->AddItem( _T("Another Extension..."), _T("Experimental GUI for other things"),
            ModTrackPanelFN( OnFuncHide ) );
      }
      break;
   default:
      break;
   }

   return 1;
}

//Example code commented out.
#if 0
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
