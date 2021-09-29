/**********************************************************************

  Audacity: A Digital Audio Editor

  GUIPrefs.cpp

  Brian Gunlogson
  Joshua Haberman
  Dominic Mazzoni
  James Crook


*******************************************************************//**

\class GUIPrefs
\brief A PrefsPanel for general GUI preferences.

*//*******************************************************************/


#include "GUIPrefs.h"

#include <wx/app.h>
#include <wx/defs.h>

#include "FileNames.h"
#include "Languages.h"
#include "../Theme.h"
#include "Prefs.h"
#include "../ShuttleGui.h"

#include "Decibels.h"

#include "ThemePrefs.h"
#include "../AColor.h"
#include "../widgets/AudacityMessageBox.h"

GUIPrefs::GUIPrefs(wxWindow * parent, wxWindowID winid)
/* i18n-hint: refers to Audacity's user interface settings */
:  PrefsPanel(parent, winid, XC("Interface", "GUI"))
{
   Populate();
}

GUIPrefs::~GUIPrefs()
{
}

ComponentInterfaceSymbol GUIPrefs::GetSymbol()
{
   return GUI_PREFS_PLUGIN_SYMBOL;
}

TranslatableString GUIPrefs::GetDescription()
{
   return XO("Preferences for GUI");
}

ManualPageID GUIPrefs::HelpPageName()
{
   return "Interface_Preferences";
}

void GUIPrefs::GetRangeChoices(
   TranslatableStrings *pChoices,
   wxArrayStringEx *pCodes,
   int *pDefaultRangeIndex
)
{
   static const auto sCodes = {
      wxT("36") ,
      wxT("48") ,
      wxT("60") ,
      wxT("72") ,
      wxT("84") ,
      wxT("96") ,
      wxT("120") ,
      wxT("145") ,
   };
   if (pCodes)
      *pCodes = sCodes;

   static const std::initializer_list<TranslatableString> sChoices = {
      XO("-36 dB (shallow range for high-amplitude editing)") ,
      XO("-48 dB (PCM range of 8 bit samples)") ,
      XO("-60 dB (PCM range of 10 bit samples)") ,
      XO("-72 dB (PCM range of 12 bit samples)") ,
      XO("-84 dB (PCM range of 14 bit samples)") ,
      XO("-96 dB (PCM range of 16 bit samples)") ,
      XO("-120 dB (approximate limit of human hearing)") ,
      XO("-145 dB (PCM range of 24 bit samples)") ,
   };

   if (pChoices)
      *pChoices = sChoices;

   if (pDefaultRangeIndex)
      *pDefaultRangeIndex = 2; // 60 == ENV_DB_RANGE
}

void GUIPrefs::Populate()
{
   // First any pre-processing for constructing the GUI.
   Languages::GetLanguages(
      FileNames::AudacityPathList(), mLangCodes, mLangNames);

   GetRangeChoices(&mRangeChoices, &mRangeCodes, &mDefaultRangeIndex);

#if 0
   mLangCodes.insert( mLangCodes.end(), {
      // only for testing...
      "kg" ,
      "ep" ,
   } );

   mLangNames.insert( mLangNames.end(), {
      "Klingon" ,
      "Esperanto" ,
   } );
#endif

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

ChoiceSetting GUIManualLocation{
   wxT("/GUI/Help"),
   {
      ByColumns,
      { XO("Local") ,  XO("From Internet") , },
      { wxT("Local") , wxT("FromInternet") , }
   },
   0 // "Local"
};

void GUIPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(XO("Display"));
   {
      S.StartMultiColumn(2);
      {

         S.TieChoice( XXO("&Language:"),
            {
               wxT("/Locale/Language"),
               { ByColumns, mLangNames, mLangCodes }
            }
         );

         S.TieChoice( XXO("Location of &Manual:"), GUIManualLocation);

         S.TieChoice( XXO("Th&eme:"), GUITheme());

         S.TieChoice( XXO("Meter dB &range:"),
            {
               DecibelScaleCutoff.GetPath(),
               { ByColumns, mRangeChoices, mRangeCodes },
               mDefaultRangeIndex
            }
         );
      }
      S.EndMultiColumn();

   }
   S.EndStatic();

   S.StartStatic(XO("Options"));
   {
      // Start wording of options with a verb, if possible.
      S.TieCheckBox(XXO("Show 'How to Get &Help' at launch"),
                    {wxT("/GUI/ShowSplashScreen"),
                     true});
      S.TieCheckBox(XXO("Show e&xtra menus"),
                    {wxT("/GUI/ShowExtraMenus"),
                     false});
#ifdef EXPERIMENTAL_THEME_PREFS
      // We do not want to make this option mainstream.  It's a 
      // convenience for developers.
      S.TieCheckBox(XXO("Show alternative &styling (Mac vs PC)"),
                    {wxT("/GUI/ShowMac"),
                     false});
#endif
      S.TieCheckBox(XXO("&Beep on completion of longer activities"),
                    {wxT("/GUI/BeepOnCompletion"),
                     false});
      S.TieCheckBox(XXO("Re&tain labels if selection snaps to a label"),
                    {wxT("/GUI/RetainLabels"),
                     false});
      S.TieCheckBox(XXO("B&lend system and Audacity theme"),
                     GUIBlendThemes);
#ifndef __WXMAC__
      /* i18n-hint: RTL stands for 'Right to Left'  */
      S.TieCheckBox(XXO("Use mostly Left-to-Right layouts in RTL languages"),
         {"/GUI/RtlWorkaround",
          true});
#endif
#ifdef EXPERIMENTAL_CEE_NUMBERS_OPTION
      S.TieCheckBox(XXO("Never use comma as decimal point"),
                    {wxT("/Locale/CeeNumberFormat"),
                     false});
#endif
   }
   S.EndStatic();

   S.StartStatic(XO("Timeline"));
   {
      S.TieCheckBox(XXO("Show Timeline Tooltips"),
                    {wxT("/QuickPlay/ToolTips"),
                     true});
      S.TieCheckBox(XXO("Show Scrub Ruler"),
                    {wxT("/QuickPlay/ScrubbingEnabled"),
                     false});
   }
   S.EndStatic();

   S.EndScroller();
}

bool GUIPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   // If language has changed, we want to change it now, not on the next reboot.
   wxString lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));
   wxString usedLang = SetLang(lang);
   // Bug 1523: Previously didn't check no-language (=System Language)
   if (!(lang.empty() || lang == L"System") && (lang != usedLang)) {
      // lang was not usable and is not system language.  We got overridden.
      gPrefs->Write(wxT("/Locale/Language"), usedLang);
      gPrefs->Flush();
   }

   // Reads preference GUITheme
   theTheme.LoadPreferredTheme();
   ThemePrefs::ApplyUpdatedImages();

   return true;
}

wxString GUIPrefs::SetLang( const wxString & lang )
{
   auto result = Languages::SetLang(FileNames::AudacityPathList(), lang);
   if (!(lang.empty() || lang == L"System") && result != lang)
      ::AudacityMessageBox(
         XO("Language \"%s\" is unknown").Format( lang ) );

#ifdef EXPERIMENTAL_CEE_NUMBERS_OPTION
   bool forceCeeNumbers;
   gPrefs->Read(wxT("/Locale/CeeNumberFormat"), &forceCeeNumbers, false);
   if( forceCeeNumbers )
      Internat::SetCeeNumberFormat();
#endif

#ifdef __WXMAC__
      wxApp::s_macHelpMenuTitleName = _("&Help");
#endif

   return result;
}

int ShowClippingPrefsID()
{
   static int value = wxNewId();
   return value;
}

int ShowTrackNameInWaveformPrefsID()
{
   static int value = wxNewId();
   return value;
}

namespace{
PrefsPanel::Registration sAttachment{ "GUI",
   [](wxWindow *parent, wxWindowID winid, AudacityProject *)
   {
      wxASSERT(parent); // to justify safenew
      return safenew GUIPrefs(parent, winid);
   }
};
}
