/**********************************************************************

  Audacity: A Digital Audio Editor

  ApplicationPrefs.cpp

  Anton Gerasimov


*******************************************************************//**

\class ApplicationPrefs
\brief A PrefsPanel to enable/disable certain general application options like checking updates, etc.

*//*******************************************************************/


#include "ApplicationPrefs.h"
#include "update/UpdateManager.h"

#include <wx/defs.h>
#include <wx/hyperlink.h>

#include "../Prefs.h"
#include "../ShuttleGui.h"

////////////////////////////////////////////////////////////////////////////////

static ComponentInterfaceSymbol s_ComponentInterfaceSymbol{ XO("Application") };

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
   return s_ComponentInterfaceSymbol;
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

   /* i18n-hint: Title for the update notifications panel in the preferences dialog. */
   S.StartStatic(XO("Update notifications"));
   {
      /* i18n-hint: Check-box title that configures periodic updates checking. */
      S.TieCheckBox(
         XXC("&Check for updates", "application preferences"),
         DefaultUpdatesCheckingFlag);

      S.StartVerticalLay();
      {
         S.AddFixedText(XO(
            "App update checking requires network access. In order to protect your privacy, Audacity does not store any personal information."),
            false, 470);

         S.StartHorizontalLay(wxALIGN_LEFT);
         {
            S.SetBorder(0);

            S.AddSpace(2, 0);

            /* i18n-hint: The first part of "See our Privacy Policy for more info". Trailing space is required. */
            S.AddFixedText(XO("See "));

            S.AddWindow(
               safenew wxHyperlinkCtrl(
                  /* i18n-hint: The second part of "See our Privacy Policy for more info". */
                  S.GetParent(), wxID_ANY, XO("our Privacy Policy").Translation(),
                  "https://www.audacityteam.org/about/desktop-privacy-notice/"));

            /* i18n-hint: The last part of "See our Privacy Policy for more info". Leading space is required. */
            S.AddFixedText(XO(" for more info."));

            S.SetBorder(2);
         }
         S.EndHorizontalLay();
      }

      S.EndVerticalLay();
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

namespace{
PrefsPanel::Registration sAttachment{ "Application",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew ApplicationPrefs(parent, winid);
   }
};
}
