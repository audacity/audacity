/**********************************************************************

  Audacity: A Digital Audio Editor

  ApplicationPrefs.cpp

  Anton Gerasimov


*******************************************************************//**

\class ApplicationPrefs
\brief A PrefsPanel to enable/disable certain general application options like checking updates, etc.

*//*******************************************************************/


#include "ApplicationPrefs.h"

#include <wx/defs.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"

////////////////////////////////////////////////////////////////////////////////

enum { CheckingUpdatesID = wxID_HIGHEST + 1 };

BEGIN_EVENT_TABLE(ApplicationPrefs, PrefsPanel)
    EVT_CHECKBOX(CheckingUpdatesID, ApplicationPrefs::OnCheckingUpdates)
END_EVENT_TABLE()

ApplicationPrefs::ApplicationPrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, XO("Application"))
{
   Populate();
}

ApplicationPrefs::~ApplicationPrefs()
{
}

ComponentInterfaceSymbol ApplicationPrefs::GetSymbol()
{
   return WARNINGS_PREFS_PLUGIN_SYMBOL;
}

TranslatableString ApplicationPrefs::GetDescription()
{
   return XO("Preferences for Application");
}

wxString ApplicationPrefs::HelpPageName()
{
   return "Application_Preferences";
}

void ApplicationPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void ApplicationPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(XO("Update Audacity"));
   {
      // TODO: replace `false` to UpdateManager::GetInstance().isUpdatesChakingEnabled()
      // and XO("Check for Updates...") need remove endian after translation.
      S.Id(CheckingUpdatesID).AddCheckBox(
          XO("Check for Updates"), false);
   }
   S.EndStatic();
   S.EndScroller();
}

bool ApplicationPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

void ApplicationPrefs::OnCheckingUpdates(wxCommandEvent& event)
{
    event.IsChecked();
    // TODO: add UpdateManager::GetInstance().enableUpdatesChaking(event.IsChecked())
}

namespace{
PrefsPanel::Registration sAttachment{ "Application",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew ApplicationPrefs(parent, winid);
   }
};
}
