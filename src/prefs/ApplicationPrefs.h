/**********************************************************************

  Audacity: A Digital Audio Editor

  ApplicationPrefs.h

  Anton Gerasimov

**********************************************************************/

#ifndef __AUDACITY_APPLICATION_PREFS__
#define __AUDACITY_APPLICATION_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class ShuttleGui;

#define WARNINGS_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Application") }

class ApplicationPrefs final : public PrefsPanel
{
 public:
   ApplicationPrefs(wxWindow * parent, wxWindowID winid);
   ~ApplicationPrefs();
   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   bool Commit() override;
   wxString HelpPageName() override;

   void OnCheckingUpdates(wxCommandEvent& event);

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S) override;

public:
	DECLARE_EVENT_TABLE()
};

#endif
