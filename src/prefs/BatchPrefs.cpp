/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchPrefs.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class BatchPrefs
\brief A PrefsPanel that builds up a chain of effects in BatchCommands

*//*******************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/textdlg.h>

#include "BatchPrefs.h"
#include "../Envelope.h"
#include "../Languages.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../BatchCommandDialog.h"
#include "../ShuttleGui.h"
#include "../Menus.h"
#include "../toolbars/ToolManager.h"

#define ChainsListID             7005
#define AddButtonID              7006
#define RemoveButtonID           7007
#define CommandsListID           7008
#define ImportButtonID           7009
#define ExportButtonID           7010
#define DefaultsButtonID         7011
#define UpButtonID               7012
#define DownButtonID             7013
#define RenameButtonID           7014

BEGIN_EVENT_TABLE(BatchPrefs, wxPanel)
END_EVENT_TABLE()

enum { CleanSpeechID,
   MP3ConversionID,
};

/// Constructor
BatchPrefs::BatchPrefs(wxWindow * parent):
   PrefsPanel(parent, _("Batch"))
{
   Populate();
}

/// Creates the dialog and its contents.
void BatchPrefs::Populate( )
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is 
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

/// Defines the dialog and does data exchange with it.
void BatchPrefs::PopulateOrExchange( ShuttleGui & S )
{
   S.StartHorizontalLay( wxEXPAND, 0 );
   S.SetBorder( 2 );
   S.StartStatic( _("Behaviors"),1 );
   {
#ifdef __WXDEBUG__
      S.TieCheckBox( _("&Don't apply effects in batch mode"),  
         wxT("/Batch/Debug"), false);
#endif
      S.TieCheckBox( _("Cl&eanSpeech Mode (Customized GUI)"), 
         wxT("/Batch/CleanSpeechMode"), false);
   }
   S.EndStatic();
   S.EndHorizontalLay();

   return;
}

// This commented out code might be useful as a first step if we want an immediate response to 
// switching in and out of CleanSpeech mode.
// As things currently stand, the batch commands available will NOT reflect changes in
// CleanSpeech mode until we close and reopen the preferences dialog.
#if 0
   int mode;
   AudacityProject *proj = GetActiveProject();
   mode = gPrefs->Read(wxT("/Batch/CleanSpeechMode"), 1L);
   proj->GetControlToolBar()->SetCleanSpeechMode(mode == 1);
#endif

/// Send changed values back to Prefs, and update Audacity.
bool BatchPrefs::Apply()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   unsigned mode;
   mode = gPrefs->Read(wxT("/Batch/CleanSpeechMode"), 1L);
   for(unsigned i=0; i<gAudacityProjects.GetCount(); i++)
      if(gAudacityProjects[i])
      {
         gAudacityProjects[i]->SetCleanSpeechMode(mode == 1);
         gAudacityProjects[i]->mToolManager->LayoutToolBars(); // Just to add/remove the CleanSpeech button.
      }
   return true;
}

BatchPrefs::~BatchPrefs()
{
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 7e997d04-6b94-4abb-b3d6-748400f86598
