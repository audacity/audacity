/**********************************************************************

  Audacity: A Digital Audio Editor

  TelemetryPrefs.h

  Dmitry Vedenko

**********************************************************************/

#ifndef __AUDACITY_TELEMETRY_PREFS__
#define __AUDACITY_TELEMETRY_PREFS__

#include <wx/defs.h>

#include "PrefsPanel.h"

class ShuttleGui;

#define TELEMETRY_PREFS_PLUGIN_SYMBOL ComponentInterfaceSymbol{ XO("Analytics") }

class TelemetryPrefs final : public PrefsPanel
{
 public:
   TelemetryPrefs(wxWindow * parent, wxWindowID winid);
   ~TelemetryPrefs();
   ComponentInterfaceSymbol GetSymbol() override;
   TranslatableString GetDescription() override;

   bool Commit() override;
   wxString HelpPageName() override;

 private:
   void Populate();
   void PopulateOrExchange(ShuttleGui & S) override;
};

#endif
