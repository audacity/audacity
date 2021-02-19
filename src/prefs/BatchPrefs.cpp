/**********************************************************************

  Audacity: A Digital Audio Editor

  BatchPrefs.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class BatchPrefs
\brief A probably unused PrefsPanel that in debug builds could offer a 
setting used in debugging batch (aka macros) processing.
*//*******************************************************************/


#include "BatchPrefs.h"

#include <wx/defs.h>
#include <wx/textdlg.h>

#include "Prefs.h"
#include "ShuttleGui.h"

BEGIN_EVENT_TABLE(BatchPrefs, PrefsPanel)
END_EVENT_TABLE()

/// Constructor
BatchPrefs::BatchPrefs(wxWindow * parent, wxWindowID winid):
   PrefsPanel(parent, winid, XO("Batch"))
{
   Populate();
}

ComponentInterfaceSymbol BatchPrefs::GetSymbol() const
{
   return BATCH_PREFS_PLUGIN_SYMBOL;
}

TranslatableString BatchPrefs::GetDescription() const
{
   return XO("Preferences for Batch");
}

ManualPageID BatchPrefs::HelpPageName()
{
   return  "Batch_Preferences";
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
   S.SetBorder( 2 );
   S.StartScroller();
   S.StartHorizontalLay( wxEXPAND, 0 );

   S.StartStatic( XO("Behaviors"),1 );
   {
#ifdef _DEBUG
      S.TieCheckBox( XXO("&Don't apply effects in batch mode"),
         {wxT("/Batch/Debug"), false});
#endif
   }
   S.EndStatic();
   S.EndHorizontalLay();
   S.EndScroller();
   return;
}

/// Send changed values back to Prefs, and update Audacity.
bool BatchPrefs::Commit()
{
   ShuttleGui S( this, eIsSavingToPrefs );
   PopulateOrExchange( S );

   return true;
}

BatchPrefs::~BatchPrefs()
{
}

#if 0
namespace{
PrefsPanel::Registration sAttachment{ "Batch",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew BatchPrefs(parent, winid);
   },
   false,
   // Register with an explicit ordering hint because this one is
   // only conditionally compiled
   { "", { Registry::OrderingHint::Before, "KeyConfig" } }
};
}
#endif
