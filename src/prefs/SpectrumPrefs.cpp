/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class SpectrumPrefs
\brief A PrefsPanel for spectrum settings.

*//*******************************************************************/

#include "../Audacity.h"
#include "SpectrumPrefs.h"

#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>

#include "../FFT.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"

#include <algorithm>

SpectrumPrefs::SpectrumPrefs(wxWindow * parent, WaveTrack *wt)
:  PrefsPanel(parent, _("Spectrograms"))
, mWt(wt)
{
   SpectrogramSettings *const pSettings = mWt
      ? &mWt->GetIndependentSpectrogramSettings()
      : &SpectrogramSettings::defaults();

   mTempSettings = *pSettings;
   mTempSettings.ConvertToEnumeratedWindowSizes();
   Populate(pSettings->windowSize);
}

SpectrumPrefs::~SpectrumPrefs()
{
}

enum {
   ID_WINDOW_SIZE = 10001,
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   ID_PADDING_SIZE = 10002,
#endif
};

void SpectrumPrefs::Populate(int windowSize)
{
   mSizeChoices.Add(_("8 - most wideband"));
   mSizeChoices.Add(wxT("16"));
   mSizeChoices.Add(wxT("32"));
   mSizeChoices.Add(wxT("64"));
   mSizeChoices.Add(wxT("128"));
   mSizeChoices.Add(_("256 - default"));
   mSizeChoices.Add(wxT("512"));
   mSizeChoices.Add(wxT("1024"));
   mSizeChoices.Add(wxT("2048"));
   mSizeChoices.Add(wxT("4096"));
   mSizeChoices.Add(wxT("8192"));
   mSizeChoices.Add(wxT("16384"));
   mSizeChoices.Add(_("32768 - most narrowband"));
   wxASSERT(mSizeChoices.size() == SpectrogramSettings::NumWindowSizes);

   PopulatePaddingChoices(windowSize);

   for (int i = 0; i < NumWindowFuncs(); i++) {
      mTypeChoices.Add(WindowFuncName(i));
   }


   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void SpectrumPrefs::PopulatePaddingChoices(int windowSize)
{
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   mZeroPaddingChoice = 1;

   // The choice of window size restricts the choice of padding.
   // So the padding menu might grow or shrink.

   // If pPaddingSizeControl is NULL, we have not yet tied the choice control.
   // If it is not NULL, we rebuild the control by hand.
   // I don't yet know an easier way to do this with ShuttleGUI functions.
   // PRL
   wxChoice *const pPaddingSizeControl =
      static_cast<wxChoice*>(wxWindow::FindWindowById(ID_PADDING_SIZE, this));

   if (pPaddingSizeControl) {
      mZeroPaddingChoice = pPaddingSizeControl->GetSelection();
      pPaddingSizeControl->Clear();
   }

   int padding = 1;
   int numChoices = 0;
   const int maxWindowSize = 1 << (SpectrogramSettings::LogMaxWindowSize);
   while (windowSize <= maxWindowSize) {
      const wxString numeral = wxString::Format(wxT("%d"), padding);
      mZeroPaddingChoices.Add(numeral);
      if (pPaddingSizeControl)
         pPaddingSizeControl->Append(numeral);
      windowSize <<= 1;
      padding <<= 1;
      ++numChoices;
   }

   mZeroPaddingChoice = std::min(mZeroPaddingChoice, numChoices - 1);

   if (pPaddingSizeControl)
      pPaddingSizeControl->SetSelection(mZeroPaddingChoice);
#endif
}

void SpectrumPrefs::PopulateOrExchange(ShuttleGui & S)
{
   S.SetBorder(2);

   // S.StartStatic(_("Track Settings"));
   {
      S.StartStatic(_("FFT Window"));
      {
         S.StartMultiColumn(2);
         {
            S.Id(ID_WINDOW_SIZE).TieChoice(_("Window &size:"),
               mTempSettings.windowSize,
               &mSizeChoices);
            S.SetSizeHints(mSizeChoices);

            S.TieChoice(_("Window &type:"),
               mTempSettings.windowType,
               &mTypeChoices);
            S.SetSizeHints(mTypeChoices);

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
            S.Id(ID_PADDING_SIZE).TieChoice(_("&Zero padding factor") + wxString(wxT(":")),
               mTempSettings.zeroPaddingFactor,
               &mZeroPaddingChoices);
            S.SetSizeHints(mZeroPaddingChoices);
#endif
         }
         S.EndMultiColumn();
      }
      S.EndStatic();

      S.StartStatic(_("Display"));
      {
         S.StartTwoColumn();
         {
            mMinFreq =
               S.TieNumericTextBox(_("Mi&nimum Frequency (Hz):"),
               mTempSettings.minFreq,
               12);

            mMaxFreq =
               S.TieNumericTextBox(_("Ma&ximum Frequency (Hz):"),
               mTempSettings.maxFreq,
               12);

            mGain =
               S.TieNumericTextBox(_("&Gain (dB):"),
               mTempSettings.gain,
               8);

            mRange =
               S.TieNumericTextBox(_("&Range (dB):"),
               mTempSettings.range,
               8);

            mFrequencyGain =
               S.TieNumericTextBox(_("Frequency g&ain (dB/dec):"),
               mTempSettings.frequencyGain,
               4);
         }
         S.EndTwoColumn();

         S.TieCheckBox(_("S&how the spectrum using grayscale colors"),
            mTempSettings.isGrayscale);

#ifdef EXPERIMENTAL_FFT_Y_GRID
         S.TieCheckBox(_("Show a grid along the &Y-axis"),
            mTempSettings.fftYGrid);
#endif //EXPERIMENTAL_FFT_Y_GRID
      }
      S.EndStatic();

#ifdef EXPERIMENTAL_FIND_NOTES
      /* i18n-hint: FFT stands for Fast Fourier Transform and probably shouldn't be translated*/
      S.StartStatic(_("FFT Find Notes"));
      {
         S.StartTwoColumn();
         {
            mFindNotesMinA =
               S.TieNumericTextBox(_("Minimum Amplitude (dB):"),
               mTempSettings.fftFindNotes,
               8);

            mFindNotesN =
               S.TieNumericTextBox(_("Max. Number of Notes (1..128):"),
               mTempSettings.findNotesMinA,
               8);
         }
         S.EndTwoColumn();

         S.TieCheckBox(_("&Find Notes"),
            mTempSettings.numberOfMaxima);

         S.TieCheckBox(_("&Quantize Notes"),
            mTempSettings.findNotesQuantize);
      }
      S.EndStatic();
#endif //EXPERIMENTAL_FIND_NOTES
   }
   // S.EndStatic();
}

bool SpectrumPrefs::Validate()
{
   // Do checking for whole numbers

   // ToDo: use wxIntegerValidator<unsigned> when available

   long maxFreq;
   if (!mMaxFreq->GetValue().ToLong(&maxFreq)) {
      wxMessageBox(_("The maximum frequency must be an integer"));
      return false;
   }

   long minFreq;
   if (!mMinFreq->GetValue().ToLong(&minFreq)) {
      wxMessageBox(_("The minimum frequency must be an integer"));
      return false;
   }

   long gain;
   if (!mGain->GetValue().ToLong(&gain)) {
      wxMessageBox(_("The gain must be an integer"));
      return false;
   }

   long range;
   if (!mRange->GetValue().ToLong(&range)) {
      wxMessageBox(_("The range must be a positive integer"));
      return false;
   }

   long frequencygain;
   if (!mFrequencyGain->GetValue().ToLong(&frequencygain)) {
      wxMessageBox(_("The frequency gain must be an integer"));
      return false;
   }

#ifdef EXPERIMENTAL_FIND_NOTES
   long findNotesMinA;
   if (!mFindNotesMinA->GetValue().ToLong(&findNotesMinA)) {
      wxMessageBox(_("The minimum amplitude (dB) must be an integer"));
      return false;
   }

   long findNotesN;
   if (!mFindNotesN->GetValue().ToLong(&findNotesN)) {
      wxMessageBox(_("The maximum number of notes must be an integer"));
      return false;
   }
   if (findNotesN < 1 || findNotesN > 128) {
      wxMessageBox(_("The maximum number of notes must be in the range 1..128"));
      return false;
   }
#endif //EXPERIMENTAL_FIND_NOTES

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   // Delegate range checking to SpectrogramSettings class
   mTempSettings.ConvertToActualWindowSizes();
   const bool result = mTempSettings.Validate(false);
   mTempSettings.ConvertToEnumeratedWindowSizes();
   return result;
}

bool SpectrumPrefs::Apply()
{
   const bool isOpenPage = this->IsShown();

   WaveTrack *const partner =
      mWt ? static_cast<WaveTrack*>(mWt->GetLink()) : 0;

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   mTempSettings.ConvertToActualWindowSizes();
   if (mWt) {
      SpectrogramSettings *pSettings =
         &mWt->GetIndependentSpectrogramSettings();
      *pSettings = mTempSettings;
      if (partner) {
         pSettings = &partner->GetIndependentSpectrogramSettings();
         *pSettings = mTempSettings;
      }
   }
   else {
      SpectrogramSettings *const pSettings =
         &SpectrogramSettings::defaults();
      *pSettings = mTempSettings;
      pSettings->SavePrefs();
   }
   mTempSettings.ConvertToEnumeratedWindowSizes();

   if (mWt && isOpenPage) {
      // Future: open page will determine the track view type
      /*
      mWt->SetDisplay(WaveTrack::Spectrum);
      if (partner)
         partner->SetDisplay(WaveTrack::Spectrum);
         */
   }

   return true;
}

void SpectrumPrefs::OnWindowSize(wxCommandEvent &)
{
   wxChoice *const pWindowSizeControl =
      static_cast<wxChoice*>(wxWindow::FindWindowById(ID_WINDOW_SIZE, this));
   int windowSize = 1 <<
      (pWindowSizeControl->GetSelection() + SpectrogramSettings::LogMinWindowSize);
   PopulatePaddingChoices(windowSize);
}

BEGIN_EVENT_TABLE(SpectrumPrefs, PrefsPanel)
   EVT_CHOICE(ID_WINDOW_SIZE, SpectrumPrefs::OnWindowSize)
END_EVENT_TABLE()

SpectrumPrefsFactory::SpectrumPrefsFactory(WaveTrack *wt)
: mWt(wt)
{
}

PrefsPanel *SpectrumPrefsFactory::Create(wxWindow *parent)
{
   return new SpectrumPrefs(parent, mWt);
}
