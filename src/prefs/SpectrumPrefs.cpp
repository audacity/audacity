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
#include <wx/checkbox.h>

#include "../FFT.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "../WaveTrack.h"
#include "../TrackPanel.h"

#include <algorithm>

#include "../Experimental.h"

SpectrumPrefs::SpectrumPrefs(wxWindow * parent, WaveTrack *wt)
:  PrefsPanel(parent, wt ? _("Spectrogram Settings") : _("Spectrograms"))
, mWt(wt)
, mPopulating(false)
{
   if (mWt) {
      SpectrogramSettings &settings = wt->GetSpectrogramSettings();
      mDefaulted = (&SpectrogramSettings::defaults() == &settings);
      mTempSettings = settings;
      float minFreq, maxFreq;
      wt->GetSpectrumBounds(&minFreq, &maxFreq);
      mTempSettings.maxFreq = maxFreq;
      mTempSettings.minFreq = minFreq;
   }
   else  {
      mTempSettings = SpectrogramSettings::defaults();
      mDefaulted = false;
   }

   const auto windowSize = mTempSettings.WindowSize();
   mTempSettings.ConvertToEnumeratedWindowSizes();
   Populate(windowSize);
}

SpectrumPrefs::~SpectrumPrefs()
{
}

enum {
   ID_WINDOW_SIZE = 10001,
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   ID_WINDOW_TYPE,
   ID_PADDING_SIZE,
   ID_SCALE,
   ID_ALGORITHM,
   ID_MINIMUM,
   ID_MAXIMUM,
   ID_GAIN,
   ID_RANGE,
   ID_FREQUENCY_GAIN,
   ID_GRAYSCALE,
   ID_SPECTRAL_SELECTION,
#endif
   ID_DEFAULTS,
};

void SpectrumPrefs::Populate(size_t windowSize)
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

   mScaleChoices = SpectrogramSettings::GetScaleNames();

   mAlgorithmChoices = SpectrogramSettings::GetAlgorithmNames();

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   // ----------------------- End of main section --------------
}

void SpectrumPrefs::PopulatePaddingChoices(size_t windowSize)
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

   unsigned padding = 1;
   int numChoices = 0;
   const size_t maxWindowSize = 1 << (SpectrogramSettings::LogMaxWindowSize);
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
   mPopulating = true;

   S.SetBorder(2);

   // S.StartStatic(_("Track Settings"));
   // {

   mDefaultsCheckbox = 0;
   if (mWt) {
      /* i18n-hint: use is a verb */
      mDefaultsCheckbox = S.Id(ID_DEFAULTS).TieCheckBox(_("&Use Preferences"), mDefaulted);
   }

   S.StartStatic(_("Scale"));
   {
      S.StartTwoColumn();
      {
         S.Id(ID_SCALE).TieChoice(_("S&cale") + wxString(wxT(":")),
            *(int*)&mTempSettings.scaleType,
            &mScaleChoices);

         mMinFreq =
            S.Id(ID_MINIMUM).TieNumericTextBox(_("Mi&nimum Frequency (Hz):"),
            mTempSettings.minFreq,
            12);

         mMaxFreq =
            S.Id(ID_MAXIMUM).TieNumericTextBox(_("Ma&ximum Frequency (Hz):"),
            mTempSettings.maxFreq,
            12);
      }
      S.EndTwoColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Colors"));
   {
      S.StartTwoColumn();
      {
         mGain =
            S.Id(ID_GAIN).TieNumericTextBox(_("&Gain (dB):"),
            mTempSettings.gain,
            8);

         mRange =
            S.Id(ID_RANGE).TieNumericTextBox(_("&Range (dB):"),
            mTempSettings.range,
            8);

         mFrequencyGain =
            S.Id(ID_FREQUENCY_GAIN).TieNumericTextBox(_("Frequency g&ain (dB/dec):"),
            mTempSettings.frequencyGain,
            4);
      }

      S.Id(ID_GRAYSCALE).TieCheckBox(_("S&how the spectrum using grayscale colors"),
         mTempSettings.isGrayscale);

      S.EndTwoColumn();
   }
   S.EndStatic();

   S.StartStatic(_("Algorithm"));
   {
      S.StartMultiColumn(2);
      {
         mAlgorithmChoice =
            S.Id(ID_ALGORITHM).TieChoice(_("A&lgorithm") + wxString(wxT(":")),
            *(int*)&mTempSettings.algorithm,
            &mAlgorithmChoices);

         S.Id(ID_WINDOW_SIZE).TieChoice(_("Window &size:"),
            mTempSettings.windowSize,
            &mSizeChoices);
         S.SetSizeHints(mSizeChoices);

         S.Id(ID_WINDOW_TYPE).TieChoice(_("Window &type:"),
            mTempSettings.windowType,
            &mTypeChoices);
         S.SetSizeHints(mTypeChoices);

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
         mZeroPaddingChoiceCtrl =
            S.Id(ID_PADDING_SIZE).TieChoice(_("&Zero padding factor") + wxString(wxT(":")),
            mTempSettings.zeroPaddingFactor,
            &mZeroPaddingChoices);
         S.SetSizeHints(mZeroPaddingChoices);
#endif
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

#ifndef SPECTRAL_SELECTION_GLOBAL_SWITCH
   S.Id(ID_SPECTRAL_SELECTION).TieCheckBox(_("Ena&ble Spectral Selection"),
      mTempSettings.spectralSelection);
#endif

#ifdef EXPERIMENTAL_FFT_Y_GRID
         S.TieCheckBox(_("Show a grid along the &Y-axis"),
            mTempSettings.fftYGrid);
#endif //EXPERIMENTAL_FFT_Y_GRID

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
   // S.EndStatic();

#ifdef SPECTRAL_SELECTION_GLOBAL_SWITCH
   S.StartStatic(_("Global settings"));
   {
      S.TieCheckBox(_("Ena&ble spectral selection"),
         SpectrogramSettings::Globals::Get().spectralSelection);
   }
   S.EndStatic();
#endif

   EnableDisableSTFTOnlyControls();

   mPopulating = false;
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
   if (!Validate())
      return false;

   const bool isOpenPage = this->IsShown();

   const auto partner =
      mWt ?
            // Assume linked track is wave or null
            static_cast<WaveTrack*>(mWt->GetLink())
          : nullptr;

   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);


   mTempSettings.ConvertToActualWindowSizes();
   SpectrogramSettings::Globals::Get().SavePrefs(); // always

   if (mWt) {
      if (mDefaulted) {
         mWt->SetSpectrogramSettings({});
         // ... and so that the vertical scale also defaults:
         mWt->SetSpectrumBounds(-1, -1);
         if (partner) {
            partner->SetSpectrogramSettings({});
            partner->SetSpectrumBounds(-1, -1);
         }
      }
      else {
         SpectrogramSettings *pSettings =
            &mWt->GetIndependentSpectrogramSettings();
         mWt->SetSpectrumBounds(mTempSettings.minFreq, mTempSettings.maxFreq);
         *pSettings = mTempSettings;
         if (partner) {
            pSettings = &partner->GetIndependentSpectrogramSettings();
            partner->SetSpectrumBounds(mTempSettings.minFreq, mTempSettings.maxFreq);
            *pSettings = mTempSettings;
         }
      }
   }

   if (!mWt || mDefaulted) {
      SpectrogramSettings *const pSettings = &SpectrogramSettings::defaults();
      *pSettings = mTempSettings;
      pSettings->SavePrefs();
   }
   mTempSettings.ConvertToEnumeratedWindowSizes();

   if (mWt && isOpenPage) {
      mWt->SetDisplay(WaveTrack::Spectrum);
      if (partner)
         partner->SetDisplay(WaveTrack::Spectrum);
   }

   if (isOpenPage) {
      TrackPanel *const tp = ::GetActiveProject()->GetTrackPanel();
      tp->UpdateVRulers();
      tp->Refresh(false);
   }

   return true;
}

bool SpectrumPrefs::ShowsApplyButton()
{
   return true;
}

void SpectrumPrefs::OnControl(wxCommandEvent&)
{
   // Common routine for most controls
   // If any per-track setting is changed, break the association with defaults
   // Skip this, and View Settings... will be able to change defaults instead
   // when the checkbox is on, as in the original design.

   if (mDefaultsCheckbox && !mPopulating) {
      mDefaulted = false;
      mDefaultsCheckbox->SetValue(false);
   }
}

void SpectrumPrefs::OnWindowSize(wxCommandEvent &evt)
{
   // Restrict choice of zero padding, so that product of window
   // size and padding may not exceed the largest window size.
   wxChoice *const pWindowSizeControl =
      static_cast<wxChoice*>(wxWindow::FindWindowById(ID_WINDOW_SIZE, this));
   size_t windowSize = 1 <<
      (pWindowSizeControl->GetSelection() + SpectrogramSettings::LogMinWindowSize);
   PopulatePaddingChoices(windowSize);

   // Do the common part
   OnControl(evt);
}

void SpectrumPrefs::OnDefaults(wxCommandEvent &)
{
   if (mDefaultsCheckbox->IsChecked()) {
      mTempSettings = SpectrogramSettings::defaults();
      mTempSettings.ConvertToEnumeratedWindowSizes();
      mDefaulted = true;
      ShuttleGui S(this, eIsSettingToDialog);
      PopulateOrExchange(S);
   }
}

void SpectrumPrefs::OnAlgorithm(wxCommandEvent &evt)
{
   EnableDisableSTFTOnlyControls();
   OnControl(evt);
}

void SpectrumPrefs::EnableDisableSTFTOnlyControls()
{
   // Enable or disable other controls that are applicable only to STFT.
   const bool STFT =
      (mAlgorithmChoice->GetSelection() != SpectrogramSettings::algPitchEAC);
   mGain->Enable(STFT);
   mRange->Enable(STFT);
   mFrequencyGain->Enable(STFT);
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   mZeroPaddingChoiceCtrl->Enable(STFT);
#endif
}

BEGIN_EVENT_TABLE(SpectrumPrefs, PrefsPanel)
   EVT_CHOICE(ID_WINDOW_SIZE, SpectrumPrefs::OnWindowSize)
   EVT_CHECKBOX(ID_DEFAULTS, SpectrumPrefs::OnDefaults)
   EVT_CHOICE(ID_ALGORITHM, SpectrumPrefs::OnAlgorithm)

   // Several controls with common routine that unchecks the default box
   EVT_CHOICE(ID_WINDOW_TYPE, SpectrumPrefs::OnControl)
   EVT_CHOICE(ID_PADDING_SIZE, SpectrumPrefs::OnControl)
   EVT_CHOICE(ID_SCALE, SpectrumPrefs::OnControl)
   EVT_TEXT(ID_MINIMUM, SpectrumPrefs::OnControl)
   EVT_TEXT(ID_MAXIMUM, SpectrumPrefs::OnControl)
   EVT_TEXT(ID_GAIN, SpectrumPrefs::OnControl)
   EVT_TEXT(ID_RANGE, SpectrumPrefs::OnControl)
   EVT_TEXT(ID_FREQUENCY_GAIN, SpectrumPrefs::OnControl)
   EVT_CHECKBOX(ID_GRAYSCALE, SpectrumPrefs::OnControl)
   EVT_CHECKBOX(ID_SPECTRAL_SELECTION, SpectrumPrefs::OnControl)

END_EVENT_TABLE()

SpectrumPrefsFactory::SpectrumPrefsFactory(WaveTrack *wt)
: mWt(wt)
{
}

PrefsPanel *SpectrumPrefsFactory::Create(wxWindow *parent)
{
   wxASSERT(parent); // to justify safenew
   return safenew SpectrumPrefs(parent, mWt);
}
