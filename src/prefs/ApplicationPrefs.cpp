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

#include "Prefs.h"
#include "ShuttleGui.h"

#include "AccessibleLinksFormatter.h"

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

ComponentInterfaceSymbol ApplicationPrefs::GetSymbol() const
{
   return s_ComponentInterfaceSymbol;
}

TranslatableString ApplicationPrefs::GetDescription() const
{
   return XO("Preferences for Application");
}

ManualPageID ApplicationPrefs::HelpPageName()
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

   /* i18n-hint: Title for the panel in the application preferences dialog. */
   S.StartStatic(XO("Update notifications"));
   {
      S.StartVerticalLay();
      {
         S.TieCheckBox(
            /* i18n-hint: Check-box title that configures periodic updates checking. */
            XXC("&Check for updates", "application preferences"),
            *DefaultUpdatesCheckingFlag);

         S.AddFixedText(
            XO("App update checking requires network access. In order to protect your privacy, Audacity does not store any personal information."),
            false, 470);

         S.AddSpace(20);
         /* i18n-hint: %s will be replaced with "our Privacy Policy" */
         AccessibleLinksFormatter privacyPolicy(XO("See %s for more info."));

         privacyPolicy.FormatLink(
            /* i18n-hint: Title of hyperlink to the privacy policy. This is an object of "See". */
            wxT("%s"), XO("our Privacy Policy"),
            "https://www.audacityteam.org/about/desktop-privacy-notice/");

         privacyPolicy.Populate(S);
         S.AddSpace(10);
      }
      S.EndVerticalLay();
   }
   S.EndStatic();
   S.StartStatic(XO("UUID"));
   {
      S.StartVerticalLay();
      {
         S.TieCheckBox(
            /* i18n-hint: Check-box title that enables anonymous usage info. */
            XXC("&Usage info (UUID)", "application preferences"),
            *SendAnonymousUsageInfo
         );

         S.AddFixedText(XO(
            "To help us understand how often people use Audacity, we generate a random ID (UUID) "
            "for each installation. This helps us improve features and plan for future updates. "
            "This ID does not contain any personally identifiable information."),
            false, 470);

         S.AddSpace(20);

         /* i18n-hint: %s will be replaced with "our Privacy Policy" */
         AccessibleLinksFormatter privacyPolicy(XO("See %s for more info."));

         privacyPolicy.FormatLink(
            /* i18n-hint: Title of hyperlink to the privacy policy. This is an object of "See". */
            wxT("%s"), XO("our Privacy Policy"),
            "https://www.audacityteam.org/about/desktop-privacy-notice/");

         privacyPolicy.Populate(S);

         S.AddSpace(10);
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
   DefaultUpdatesCheckingFlag->Invalidate();
   SendAnonymousUsageInfo->Invalidate();

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
