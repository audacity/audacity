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

#include "../Audacity.h"
#include "GUIPrefs.h"

#include <wx/defs.h>

#include "../AudacityApp.h"
#include "../Languages.h"
#include "../Prefs.h"
#include "../ShuttleGui.h"

#include "GUISettings.h"

#include "../Experimental.h"

GUIPrefs::GUIPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Interface"))
{
   Populate();
}

GUIPrefs::~GUIPrefs()
{
}

void GUIPrefs::GetRangeChoices(wxArrayString *pChoices, wxArrayString *pCodes)
{
   if (pCodes) {
      wxArrayString &codes = *pCodes;
      codes.Clear();
      codes.Add(wxT("36"));
      codes.Add(wxT("48"));
      codes.Add(wxT("60"));
      codes.Add(wxT("72"));
      codes.Add(wxT("84"));
      codes.Add(wxT("96"));
      codes.Add(wxT("120"));
      codes.Add(wxT("145"));
   }

   if (pChoices) {
      wxArrayString &choices = *pChoices;
      choices.Clear();
      choices.Add(_("-36 dB (shallow range for high-amplitude editing)"));
      choices.Add(_("-48 dB (PCM range of 8 bit samples)"));
      choices.Add(_("-60 dB (PCM range of 10 bit samples)"));
      choices.Add(_("-72 dB (PCM range of 12 bit samples)"));
      choices.Add(_("-84 dB (PCM range of 14 bit samples)"));
      choices.Add(_("-96 dB (PCM range of 16 bit samples)"));
      choices.Add(_("-120 dB (approximate limit of human hearing)"));
      choices.Add(_("-145 dB (PCM range of 24 bit samples)"));
   }
}

void GUIPrefs::Populate()
{
   // First any pre-processing for constructing the GUI.
   GetLanguages(mLangCodes, mLangNames);

   mHtmlHelpCodes.Add(wxT("Local"));
   mHtmlHelpCodes.Add(wxT("FromInternet"));

   mHtmlHelpChoices.Add(_("Local"));
   mHtmlHelpChoices.Add(_("From Internet"));

   GetRangeChoices(&mRangeChoices, &mRangeCodes);

#if 0
   // only for testing...
   mLangCodes.Add("kg");   mLangNames.Add("Klingon");
   mLangCodes.Add("ep");   mLangNames.Add("Esperanto");
#endif

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void GUIPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Display"));
   {
      S.TieCheckBox(_("&Ergonomic order of Transport Toolbar buttons"),
                    wxT("/GUI/ErgonomicTransportButtons"),
                    true);
      S.TieCheckBox(_("S&how 'How to Get Help' dialog box at program start up"),
                    wxT("/GUI/ShowSplashScreen"),
                    true);

      S.AddSpace(10);

      S.StartMultiColumn(2);
      {
         const wxString defaultRange = wxString::Format(wxT("%d"), ENV_DB_RANGE);
         S.TieChoice(_("Meter dB &range:"),
                     ENV_DB_KEY,
                     defaultRange,
                     mRangeChoices,
                     mRangeCodes);
         S.SetSizeHints(mRangeChoices);

         S.TieChoice(_("&Language:"),
                     wxT("/Locale/Language"),
                     wxT(""),
                     mLangNames,
                     mLangCodes);
         S.SetSizeHints(mLangNames);

         S.TieChoice(_("Location of &Manual:"),
                     wxT("/GUI/Help"),
                     wxT("Local"),
                     mHtmlHelpChoices,
                     mHtmlHelpCodes);
         S.SetSizeHints(mHtmlHelpChoices);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Behaviors"));
   {
      S.TieCheckBox(_("&Beep on completion of longer activities"),
                    wxT("/GUI/BeepOnCompletion"),
                    false);
      S.TieCheckBox(_("Re&tain labels if selection snaps to a label edge"),
                    wxT("/GUI/RetainLabels"),
                    false);

#ifdef EXPERIMENTAL_OUTPUT_DISPLAY
      S.TieCheckBox(_("&Display a mono channel as virtual stereo"),
                    wxT("/GUI/MonoAsVirtualStereo"),
                    false);
#endif
   }
   S.EndStatic();
}

bool GUIPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   // If language has changed, we want to change it now, not on the next reboot.
   wxString lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));
   wxString usedLang = wxGetApp().InitLang(lang);
   if (lang != usedLang) {
      // lang was not usable.  We got overridden.
      gPrefs->Write(wxT("/Locale/Language"), usedLang);
      gPrefs->Flush();
   }

   return true;
}

PrefsPanel *GUIPrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew GUIPrefs(parent);
}
