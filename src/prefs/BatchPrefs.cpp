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
#include "../Languages.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../BatchCommandDialog.h"
#include "../ShuttleGui.h"
#include "../Menus.h"
#include "../toolbars/ToolManager.h"

BEGIN_EVENT_TABLE(BatchPrefs, PrefsPanel)
END_EVENT_TABLE()

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
   }
   S.EndStatic();
   S.EndHorizontalLay();

   return;
}

/// Send changed values back to Prefs, and update Audacity.
bool BatchPrefs::Apply()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   return true;
}

BatchPrefs::~BatchPrefs()
{
}

PrefsPanel *BatchPrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew BatchPrefs(parent);
}
