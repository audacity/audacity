/**********************************************************************

  Audacity: A Digital Audio Editor

  QualityPrefs.cpp

  Joshua Haberman
  James Crook


*******************************************************************//**

\class QualityPrefs
\brief A PrefsPanel used for setting audio quality.

*//*******************************************************************/

#include "../Audacity.h"
#include "QualityPrefs.h"

#include <wx/choice.h>
#include <wx/defs.h>
#include <wx/textctrl.h>

#include "../AudioIOBase.h"
#include "../Dither.h"
#include "../Prefs.h"
#include "../Resample.h"
#include "../ShuttleGui.h"

#define ID_SAMPLE_RATE_CHOICE           7001

//////////

static EnumSetting< sampleFormat > formatSetting{
   wxT("/SamplingRate/DefaultProjectSampleFormatChoice"),
   {
      { wxT("Format16Bit"), XO("16-bit") },
      { wxT("Format24Bit"), XO("24-bit") },
      { wxT("Format32BitFloat"), XO("32-bit float") }
   },
   2, // floatSample

   // for migrating old preferences:
   {
      int16Sample,
      int24Sample,
      floatSample
   },
   wxT("/SamplingRate/DefaultProjectSampleFormat"),
};

//////////
BEGIN_EVENT_TABLE(QualityPrefs, PrefsPanel)
   EVT_CHOICE(ID_SAMPLE_RATE_CHOICE, QualityPrefs::OnSampleRateChoice)
END_EVENT_TABLE()

QualityPrefs::QualityPrefs(wxWindow * parent, wxWindowID winid)
/* i18n-hint: meaning accuracy in reproduction of sounds */
:  PrefsPanel(parent, winid, _("Quality"))
{
   Populate();
}

QualityPrefs::~QualityPrefs()
{
}

ComponentInterfaceSymbol QualityPrefs::GetSymbol()
{
   return QUALITY_PREFS_PLUGIN_SYMBOL;
}

wxString QualityPrefs::GetDescription()
{
   return _("Preferences for Quality");
}

wxString QualityPrefs::HelpPageName()
{
   return "Quality_Preferences";
}

void QualityPrefs::Populate()
{
   // First any pre-processing for constructing the GUI.
   GetNamesAndLabels();
   gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"),
                &mOtherSampleRateValue,
                AudioIOBase::GetOptimalSupportedSampleRate());

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------

   wxCommandEvent e;
   OnSampleRateChoice(e); // Enable/disable the control.
}

/// Gets the lists of names and lists of labels which are
/// used in the choice controls.
/// The names are what the user sees in the wxChoice.
/// The corresponding labels are what gets stored.
void QualityPrefs::GetNamesAndLabels()
{
   //------------ Sample Rate Names
   // JKC: I don't understand the following comment.
   //      Can someone please explain or correct it?
   // XXX: This should use a previously changed, but not yet saved
   //      sound card setting from the "I/O" preferences tab.
   // LLL: It means that until the user clicks "Ok" in preferences, the
   //      GetSupportedSampleRates() call should use the devices they
   //      may have changed on the Audio I/O page.  As coded, the sample
   //      rates it will return could be completely invalid as they will
   //      be what's supported by the devices that were selected BEFORE
   //      coming into preferences.
   //
   //      GetSupportedSampleRates() allows passing in device names, but
   //      how do you get at them as they are on the Audio I/O page????
   for (int i = 0; i < AudioIOBase::NumStandardRates; i++) {
      int iRate = AudioIOBase::StandardRates[i];
      mSampleRateLabels.push_back(iRate);
      mSampleRateNames.push_back(
         // Composing strings for the choice control
         // Note: the format string is localized, then substituted,
         // and then the result is treated as if it were a msgid
         // but really isn't in the translation catalog
         /* i18n-hint Hertz, a unit of frequency */
         TranslatableString{ wxString::Format(_("%i Hz"), iRate) } );
   }

   mSampleRateNames.push_back(XO("Other..."));

   // The label for the 'Other...' case can be any value at all.
   mSampleRateLabels.push_back(44100); // If chosen, this value will be overwritten
}

void QualityPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);
   S.StartScroller();

   S.StartStatic(_("Sampling"));
   {
      S.StartMultiColumn(2);
      {
         S.AddPrompt(_("Default Sample &Rate:"));

         S.StartMultiColumn(2);
         {
            // First the choice...
            // We make sure it uses the ID we want, so that we get changes
            S.Id(ID_SAMPLE_RATE_CHOICE);
            // We make sure we have a pointer to it, so that we can drive it.
            mSampleRates = S.TieNumberAsChoice( {},
                                       {wxT("/SamplingRate/DefaultProjectSampleRate"),
                                        AudioIOBase::GetOptimalSupportedSampleRate()},
                                       mSampleRateNames,
                                       &mSampleRateLabels,
                                       // If the value in Prefs isn't in the list, then we want
                                       // the last item, 'Other...' to be shown.
                                       mSampleRateNames.size() - 1
                                       );

            // Now do the edit box...
            mOtherSampleRate = S.TieNumericTextBox( {},
                                                   mOtherSampleRateValue,
                                                   15);
         }
         S.EndHorizontalLay();

         S.TieChoice(_("Default Sample &Format:"),
                     formatSetting);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Real-time Conversion"));
   {
      S.StartMultiColumn(2, wxEXPAND);
      {
         S.TieChoice(_("Sample Rate Con&verter:"),
                     Resample::FastMethodSetting);

         /* i18n-hint: technical term for randomization to reduce undesirable resampling artifacts */
         S.TieChoice(_("&Dither:"),
                     Dither::FastSetting);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("High-quality Conversion"));
   {
      S.StartMultiColumn(2);
      {
         S.TieChoice(_("Sample Rate Conver&ter:"),
                     Resample::BestMethodSetting);

         /* i18n-hint: technical term for randomization to reduce undesirable resampling artifacts */
         S.TieChoice(_("Dit&her:"),
                     Dither::BestSetting);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   S.EndScroller();

}

/// Enables or disables the Edit box depending on
/// whether we selected 'Other...' or not.
void QualityPrefs::OnSampleRateChoice(wxCommandEvent & WXUNUSED(e))
{
   int sel = mSampleRates->GetSelection();
   mOtherSampleRate->Enable(sel == (int)mSampleRates->GetCount() - 1);
}

bool QualityPrefs::Commit()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   // The complex compound control may have value 'other' in which case the
   // value in prefs comes from the second field.
   if (mOtherSampleRate->IsEnabled()) {
      gPrefs->Write(wxT("/SamplingRate/DefaultProjectSampleRate"), mOtherSampleRateValue);
      gPrefs->Flush();
   }

   // Tell CopySamples() to use these ditherers now
   InitDitherers();

   return true;
}

PrefsPanel::Factory
QualityPrefsFactory = [](wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew QualityPrefs(parent, winid);
};

sampleFormat QualityPrefs::SampleFormatChoice()
{
   return formatSetting.ReadEnum();
}

