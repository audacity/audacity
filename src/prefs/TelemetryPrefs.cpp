/**********************************************************************

  Audacity: A Digital Audio Editor

  TelemetryPrefs.cpp

  Dmitry Vedenko


*******************************************************************//**

\class TelemetryPrefs
\brief A PrefsPanel to enable/disable user data sharing.

*//*******************************************************************/

#include "../Audacity.h"
#include "TelemetryPrefs.h"

#include <wx/defs.h>
#include <wx/hyperlink.h>

#include "../ShuttleGui.h"

////////////////////////////////////////////////////////////////////////////////

TelemetryPrefs::TelemetryPrefs(wxWindow * parent, wxWindowID winid)
:  PrefsPanel(parent, winid, XO("Analytics"))
{
   Populate();
}

TelemetryPrefs::~TelemetryPrefs()
{
}

ComponentInterfaceSymbol TelemetryPrefs::GetSymbol()
{
   return TELEMETRY_PREFS_PLUGIN_SYMBOL;
}

TranslatableString TelemetryPrefs::GetDescription()
{
   return XO("Preferences for Analytics");
}

wxString TelemetryPrefs::HelpPageName()
{
   return "Analytics_Preferences";
}

void TelemetryPrefs::Populate()
{
   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialized with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void TelemetryPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(XO("Analytics"));
   {
       S.StartHorizontalLay (wxEXPAND);
       {
           S.StartVerticalLay (0);
           {
               S.TieCheckBox ({}, { wxT ("/Telemetry/Enabled"), true });
           }
           S.EndVerticalLay ();

           S.StartVerticalLay ();
           {
               S.AddFixedText (XO ("Share anonymous analytics and crash reports"));
               S.AddFixedText (XO ("Help Audacity improve its service, by allowing analytics of usage and data."));
               S.AddWindow (
                   safenew wxHyperlinkCtrl (
                       S.GetParent (), wxID_ANY, 
                       XO ("Analytics Privacy Policy").Translation(), 
                       "https://audacityteam.org/"),
                   wxALIGN_LEFT
               );
           }
           S.EndVerticalLay ();
       }
       S.EndHorizontalLay ();
   }
   S.EndStatic();

}

bool TelemetryPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   return true;
}

namespace{
PrefsPanel::Registration sAttachment{ "Analytics",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew TelemetryPrefs(parent, winid);
   }
};
}
