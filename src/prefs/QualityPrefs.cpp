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

#include <wx/defs.h>

#include "../AudioIO.h"
#include "../Dither.h"
#include "../Prefs.h"
#include "../Resample.h"
#include "../SampleFormat.h"
#include "../ShuttleGui.h"

#define ID_SAMPLE_RATE_CHOICE           7001

BEGIN_EVENT_TABLE(QualityPrefs, PrefsPanel)
   EVT_CHOICE(ID_SAMPLE_RATE_CHOICE, QualityPrefs::OnSampleRateChoice)
END_EVENT_TABLE()

QualityPrefs::QualityPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Quality"))
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
   //------------ Dither Names
   mDitherNames.Add(_("None"));        mDitherLabels.Add(Dither::none);
   mDitherNames.Add(_("Rectangle"));   mDitherLabels.Add(Dither::rectangle);
   mDitherNames.Add(_("Triangle"));    mDitherLabels.Add(Dither::triangle);
   mDitherNames.Add(_("Shaped"));      mDitherLabels.Add(Dither::shaped);

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
      mSampleRateLabels.Add(iRate);
      mSampleRateNames.Add(wxString::Format(wxT("%i Hz"), iRate));
   }

   mSampleRateNames.Add(_("Other..."));

   // The label for the 'Other...' case can be any value at all.
   mSampleRateLabels.Add(44100); // If chosen, this value will be overwritten

   //------------- Sample Format Names
   mSampleFormatNames.Add(wxT("16-bit"));       mSampleFormatLabels.Add(int16Sample);
   mSampleFormatNames.Add(wxT("24-bit"));       mSampleFormatLabels.Add(int24Sample);
   mSampleFormatNames.Add(wxT("32-bit float")); mSampleFormatLabels.Add(floatSample);

   //------------- Converter Names
   int numConverters = Resample::GetNumMethods();
   for (int i = 0; i < numConverters; i++) {
      mConverterNames.Add(Resample::GetMethodName(i));
      mConverterLabels.Add(i);
   }
}

void QualityPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   S.StartStatic(_("Sampling"));
   {
      S.StartMultiColumn(2);
      {
         S.AddPrompt(_("Default Sample &Rate:"));

         S.StartMultiColumn(2);
         {
            // If the value in Prefs isn't in the list, then we want
            // the last item, 'Other...' to be shown.
            S.SetNoMatchSelector(mSampleRateNames.GetCount() - 1);
            // First the choice...
            // We make sure it uses the ID we want, so that we get changes
            S.Id(ID_SAMPLE_RATE_CHOICE);
            // We make sure we have a pointer to it, so that we can drive it.
            mSampleRates = S.TieChoice(wxT(""),
                                       wxT("/SamplingRate/DefaultProjectSampleRate"),
                                       AudioIO::GetOptimalSupportedSampleRate(),
                                       mSampleRateNames,
                                       mSampleRateLabels);
            S.SetSizeHints(mSampleRateNames);

            // Now do the edit box...
            mOtherSampleRate = S.TieNumericTextBox(wxT(""),
                                                   mOtherSampleRateValue,
                                                   15);
         }
         S.EndHorizontalLay();

         S.TieChoice(_("Default Sample &Format:"),
                     wxT("/SamplingRate/DefaultProjectSampleFormat"),
                     floatSample,
                     mSampleFormatNames,
                     mSampleFormatLabels);
         S.SetSizeHints(mSampleFormatNames);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Real-time Conversion"));
   {
      S.StartMultiColumn(2, wxEXPAND);
      {
         S.TieChoice(_("Sample Rate Con&verter:"),
                     Resample::GetFastMethodKey(),
                     Resample::GetFastMethodDefault(),
                     mConverterNames,
                     mConverterLabels);
         S.SetSizeHints(mConverterNames);

         S.TieChoice(_("&Dither:"),
                     wxT("/Quality/DitherAlgorithm"),
                     Dither::none,
                     mDitherNames,
                     mDitherLabels);
         S.SetSizeHints(mDitherNames);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   S.StartStatic(_("High-quality Conversion"));
   {
      S.StartMultiColumn(2);
      {
         S.TieChoice(_("Sample Rate Conver&ter:"),
                     Resample::GetBestMethodKey(),
                     Resample::GetBestMethodDefault(),
                     mConverterNames,
                     mConverterLabels);
         S.SetSizeHints(mConverterNames);

         S.TieChoice(_("Dit&her:"),
                     wxT("/Quality/HQDitherAlgorithm"),
                     Dither::shaped,
                     mDitherNames,
                     mDitherLabels);
         S.SetSizeHints(mDitherNames);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
}

/// Enables or disables the Edit box depending on
/// whether we selected 'Other...' or not.
void QualityPrefs::OnSampleRateChoice(wxCommandEvent & WXUNUSED(e))
{
   int sel = mSampleRates->GetSelection();
   mOtherSampleRate->Enable(sel == (int)mSampleRates->GetCount() - 1);
}

bool QualityPrefs::Apply()
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

PrefsPanel *QualityPrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew QualityPrefs(parent);
}
