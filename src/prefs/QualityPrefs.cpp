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

#include "../AudioIO.h"
#include "../Dither.h"
#include "../Prefs.h"
#include "../Resample.h"
#include "../SampleFormat.h"
#include "../ShuttleGui.h"
#include "../Internat.h"

#define ID_SAMPLE_RATE_CHOICE           7001

//////////

static const EnumValueSymbol choicesFormat[] = {
   { wxT("Format16Bit"), XO("16-bit") },
   { wxT("Format24Bit"), XO("24-bit") },
   { wxT("Format32BitFloat"), XO("32-bit float") }
};
static const size_t nChoicesFormat = WXSIZEOF( choicesFormat );
static const int intChoicesFormat[] = {
   int16Sample,
   int24Sample,
   floatSample
};
static_assert( nChoicesFormat == WXSIZEOF(intChoicesFormat), "size mismatch" );

static const size_t defaultChoiceFormat = 2; // floatSample

static EncodedEnumSetting formatSetting{
   wxT("/SamplingRate/DefaultProjectSampleFormatChoice"),
   choicesFormat, nChoicesFormat, defaultChoiceFormat,
   
   intChoicesFormat,
   wxT("/SamplingRate/DefaultProjectSampleFormat"),
};

//////////
static const EnumValueSymbol choicesDither[] = {
   { XO("None") },
   { XO("Rectangle") },
   { XO("Triangle") },
   { XO("Shaped") },
};
static const size_t nChoicesDither = WXSIZEOF( choicesDither );
static const int intChoicesDither[] = {
   (int) DitherType::none,
   (int) DitherType::rectangle,
   (int) DitherType::triangle,
   (int) DitherType::shaped,
};
static_assert(
   nChoicesDither == WXSIZEOF( intChoicesDither ),
   "size mismatch"
);

static const size_t defaultFastDither = 0; // none

static EncodedEnumSetting fastDitherSetting{
   wxT("Quality/DitherAlgorithmChoice"),
   choicesDither, nChoicesDither, defaultFastDither,
   intChoicesDither,
   wxT("Quality/DitherAlgorithm")
};

static const size_t defaultBestDither = 3; // shaped

static EncodedEnumSetting bestDitherSetting{
   wxT("Quality/HQDitherAlgorithmChoice"),
   choicesDither, nChoicesDither, defaultBestDither,

   intChoicesDither,
   wxT("Quality/HQDitherAlgorithm")
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

void QualityPrefs::Populate()
{
   // First any pre-processing for constructing the GUI.
   GetNamesAndLabels();
   gPrefs->Read(wxT("/SamplingRate/DefaultProjectSampleRate"),
                &mOtherSampleRateValue,
                44100);

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
   for (int i = 0; i < AudioIO::NumStandardRates; i++) {
      int iRate = AudioIO::StandardRates[i];
      mSampleRateLabels.push_back(iRate);
      mSampleRateNames.push_back(wxString::Format(wxT("%i Hz"), iRate));
   }

   mSampleRateNames.push_back(_("Other..."));

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
            // If the value in Prefs isn't in the list, then we want
            // the last item, 'Other...' to be shown.
            S.SetNoMatchSelector(mSampleRateNames.size() - 1);
            // First the choice...
            // We make sure it uses the ID we want, so that we get changes
            S.Id(ID_SAMPLE_RATE_CHOICE);
            // We make sure we have a pointer to it, so that we can drive it.
            mSampleRates = S.TieNumberAsChoice( {},
                                       wxT("/SamplingRate/DefaultProjectSampleRate"),
                                       AudioIO::GetOptimalSupportedSampleRate(),
                                       mSampleRateNames,
                                       mSampleRateLabels);

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
                     fastDitherSetting);
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
                     bestDitherSetting);
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

wxString QualityPrefs::HelpPageName()
{
   return "Quality_Preferences";
}

PrefsPanel *QualityPrefsFactory::operator () (wxWindow *parent, wxWindowID winid)
{
   wxASSERT(parent); // to justify safenew
   return safenew QualityPrefs(parent, winid);
}

sampleFormat QualityPrefs::SampleFormatChoice()
{
   return (sampleFormat)formatSetting.ReadInt();
}

DitherType QualityPrefs::FastDitherChoice()
{
   return (DitherType) fastDitherSetting.ReadInt();
}

DitherType QualityPrefs::BestDitherChoice()
{
   return (DitherType) bestDitherSetting.ReadInt();
}

