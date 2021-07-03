/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectrumPrefs.cpp

  Dominic Mazzoni
  James Crook

*******************************************************************//**

\class SpectrumPrefs
\brief A PrefsPanel for spectrum settings.

*//*******************************************************************/


#include "SpectrumPrefs.h"

#include <wx/choice.h>
#include <wx/defs.h>
#include <wx/intl.h>
#include <wx/checkbox.h>
#include <wx/textctrl.h>

#include "FFT.h"
#include "../Project.h"
#include "../ShuttleGui.h"

#include "../TrackPanel.h"
#include "../WaveTrack.h"
#include "../tracks/playabletrack/wavetrack/ui/WaveTrackView.h"

#include <algorithm>

#include "../widgets/AudacityMessageBox.h"

SpectrumPrefs::SpectrumPrefs(wxWindow * parent, wxWindowID winid,
   AudacityProject *pProject, WaveTrack *wt)
:  PrefsPanel(parent, winid, wt ? XO("Spectrogram Settings") : XO("Spectrograms"))
, mProject{ pProject }
, mWt(wt)
, mPopulating(false)
{
   if (mWt) {
      SpectrogramSettings &settings = wt->GetSpectrogramSettings();
      mOrigDefaulted = mDefaulted = (&SpectrogramSettings::defaults() == &settings);
      mTempSettings = mOrigSettings = settings;
      wt->GetSpectrumBounds(&mOrigMin, &mOrigMax);
      mTempSettings.maxFreq = mOrigMax;
      mTempSettings.minFreq = mOrigMin;
      mOrigPlacements = WaveTrackView::Get( *mWt ).SavePlacements();
   }
   else  {
      mTempSettings = mOrigSettings = SpectrogramSettings::defaults();
      mOrigDefaulted = mDefaulted = false;
   }

   const auto windowSize = mTempSettings.WindowSize();
   mTempSettings.ConvertToEnumeratedWindowSizes();
   Populate(windowSize);
}

SpectrumPrefs::~SpectrumPrefs()
{
   if (!mCommitted)
      Rollback();
}

ComponentInterfaceSymbol SpectrumPrefs::GetSymbol()
{
   return SPECTRUM_PREFS_PLUGIN_SYMBOL;
}

TranslatableString SpectrumPrefs::GetDescription()
{
   return XO("Preferences for Spectrum");
}

ManualPageID SpectrumPrefs::HelpPageName()
{
   // Currently (May2017) Spectrum Settings is the only preferences
   // we ever display in a dialog on its own without others.
   // We do so when it is configuring spectrums for a track.
   // Because this happens, we want to visit a different help page.
   // So we change the page name in the case of a page on its own.
   return mWt
      ? "Spectrogram_Settings"
      : "Spectrograms_Preferences";
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
   ID_COLOR_SCHEME,
   ID_SPECTRAL_SELECTION,
#endif
   ID_DEFAULTS,
};

void SpectrumPrefs::Populate(size_t windowSize)
{
   PopulatePaddingChoices(windowSize);

   for (int i = 0; i < NumWindowFuncs(); i++) {
      mTypeChoices.push_back( WindowFuncName(i) );
   }

   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   ShuttleGui S(this, eIsCreatingFromPrefs);
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
      const auto numeral = wxString::Format(wxT("%d"), padding);
      mZeroPaddingChoices.push_back( Verbatim( numeral ) );
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
   S.StartScroller(); {

   // S.StartStatic(XO("Track Settings"));
   // {


   mDefaultsCheckbox = 0;
   if (mWt) {
      /* i18n-hint: use is a verb */
      mDefaultsCheckbox = S.Id(ID_DEFAULTS).TieCheckBox(XXO("&Use Preferences"), mDefaulted);
   }

   S.StartMultiColumn(2,wxEXPAND);
   {
      S.SetStretchyCol( 0 );
      S.SetStretchyCol( 1 );
      S.StartStatic(XO("Scale"),1);
      {
         S.StartMultiColumn(2,wxEXPAND);
         {
            S.SetStretchyCol( 0 );
            S.SetStretchyCol( 1 );
            S.Id(ID_SCALE).TieChoice(XXO("S&cale:"),
               mTempSettings.scaleType,
               Msgids( SpectrogramSettings::GetScaleNames() ) );
            mMinFreq =
               S.Id(ID_MINIMUM).TieNumericTextBox(XXO("Mi&n Frequency (Hz):"),
               mTempSettings.minFreq,
               12);
            mMaxFreq =
               S.Id(ID_MAXIMUM).TieNumericTextBox(XXO("Ma&x Frequency (Hz):"),
               mTempSettings.maxFreq,
               12);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();

      S.StartStatic(XO("Colors"),1);
      {
         S.StartMultiColumn(2,wxEXPAND);
         {
            S.SetStretchyCol( 0 );
            S.SetStretchyCol( 1 );
            mGain =
               S.Id(ID_GAIN).TieNumericTextBox(XXO("&Gain (dB):"),
               mTempSettings.gain,
               8);
            mRange =
               S.Id(ID_RANGE).TieNumericTextBox(XXO("&Range (dB):"),
               mTempSettings.range,
               8);

            mFrequencyGain =
               S.Id(ID_FREQUENCY_GAIN).TieNumericTextBox(XXO("High &boost (dB/dec):"),
               mTempSettings.frequencyGain,
               8);

            // i18n-hint Scheme refers to a color scheme for spectrogram colors
            S.Id(ID_COLOR_SCHEME).TieChoice(XC("Sche&me", "spectrum prefs"),
               (int&)mTempSettings.colorScheme,
               Msgids( SpectrogramSettings::GetColorSchemeNames() ) );
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndMultiColumn();

   S.StartStatic(XO("Algorithm"));
   {
      S.StartMultiColumn(2);
      {
         mAlgorithmChoice =
            S.Id(ID_ALGORITHM).TieChoice(XXO("A&lgorithm:"),
            mTempSettings.algorithm,
            SpectrogramSettings::GetAlgorithmNames() );

         S.Id(ID_WINDOW_SIZE).TieChoice(XXO("Window &size:"),
            mTempSettings.windowSize,
            {
               XO("8 - most wideband"),
               XO("16"),
               XO("32"),
               XO("64"),
               XO("128"),
               XO("256"),
               XO("512"),
               XO("1024 - default"),
               XO("2048"),
               XO("4096"),
               XO("8192"),
               XO("16384"),
               XO("32768 - most narrowband"),
            }
         );

         S.Id(ID_WINDOW_TYPE).TieChoice(XXO("Window &type:"),
            mTempSettings.windowType,
            mTypeChoices);

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
         mZeroPaddingChoiceCtrl =
            S.Id(ID_PADDING_SIZE).TieChoice(XXO("&Zero padding factor:"),
            mTempSettings.zeroPaddingFactor,
            mZeroPaddingChoices);
#endif
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

#ifndef SPECTRAL_SELECTION_GLOBAL_SWITCH
   S.Id(ID_SPECTRAL_SELECTION).TieCheckBox(XXO("Ena&ble Spectral Selection"),
      mTempSettings.spectralSelection);
#endif

#ifdef EXPERIMENTAL_FFT_Y_GRID
         S.TieCheckBox(XO("Show a grid along the &Y-axis"),
            mTempSettings.fftYGrid);
#endif //EXPERIMENTAL_FFT_Y_GRID

#ifdef EXPERIMENTAL_FIND_NOTES
      /* i18n-hint: FFT stands for Fast Fourier Transform and probably shouldn't be translated*/
      S.StartStatic(XO("FFT Find Notes"));
      {
         S.StartTwoColumn();
         {
            mFindNotesMinA =
               S.TieNumericTextBox(XXO("Minimum Amplitude (dB):"),
               mTempSettings.findNotesMinA,
               8);

            mFindNotesN =
               S.TieNumericTextBox(XXO("Max. Number of Notes (1..128):"),
               mTempSettings.numberOfMaxima,
               8);
         }
         S.EndTwoColumn();

         S.TieCheckBox(XXO("&Find Notes"),
            mTempSettings.fftFindNotes);

         S.TieCheckBox(XXO("&Quantize Notes"),
            mTempSettings.findNotesQuantize);
      }
      S.EndStatic();
#endif //EXPERIMENTAL_FIND_NOTES
   // S.EndStatic();

#ifdef SPECTRAL_SELECTION_GLOBAL_SWITCH
   S.StartStatic(XO("Global settings"));
   {
      S.TieCheckBox(XXO("Ena&ble spectral selection"),
         SpectrogramSettings::Globals::Get().spectralSelection);
   }
   S.EndStatic();
#endif

   } S.EndScroller();
   
   // Enabling and disabling belongs outside this function.
   if( S.GetMode() != eIsGettingMetadata )
      EnableDisableSTFTOnlyControls();

   mPopulating = false;
}

bool SpectrumPrefs::Validate()
{
   // Do checking for whole numbers

   // ToDo: use wxIntegerValidator<unsigned> when available

   long maxFreq;
   if (!mMaxFreq->GetValue().ToLong(&maxFreq)) {
      AudacityMessageBox( XO("The maximum frequency must be an integer") );
      return false;
   }

   long minFreq;
   if (!mMinFreq->GetValue().ToLong(&minFreq)) {
      AudacityMessageBox( XO("The minimum frequency must be an integer") );
      return false;
   }

   long gain;
   if (!mGain->GetValue().ToLong(&gain)) {
      AudacityMessageBox( XO("The gain must be an integer") );
      return false;
   }

   long range;
   if (!mRange->GetValue().ToLong(&range)) {
      AudacityMessageBox( XO("The range must be a positive integer") );
      return false;
   }

   long frequencygain;
   if (!mFrequencyGain->GetValue().ToLong(&frequencygain)) {
      AudacityMessageBox( XO("The frequency gain must be an integer") );
      return false;
   }

#ifdef EXPERIMENTAL_FIND_NOTES
   long findNotesMinA;
   if (!mFindNotesMinA->GetValue().ToLong(&findNotesMinA)) {
      AudacityMessageBox( XO("The minimum amplitude (dB) must be an integer") );
      return false;
   }

   long findNotesN;
   if (!mFindNotesN->GetValue().ToLong(&findNotesN)) {
      AudacityMessageBox( XO("The maximum number of notes must be an integer") );
      return false;
   }
   if (findNotesN < 1 || findNotesN > 128) {
      AudacityMessageBox( XO(
"The maximum number of notes must be in the range 1..128") );
      return false;
   }
#endif //EXPERIMENTAL_FIND_NOTES

   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   // Delegate range checking to SpectrogramSettings class
   mTempSettings.ConvertToActualWindowSizes();
   const bool result = mTempSettings.Validate(false);
   mTempSettings.ConvertToEnumeratedWindowSizes();
   return result;
}

void SpectrumPrefs::Rollback()
{
   if (mWt) {
      auto channels = TrackList::Channels(mWt);

      for (auto channel : channels) {
         if (mOrigDefaulted) {
            channel->SetSpectrogramSettings({});
            channel->SetSpectrumBounds(-1, -1);
         }
         else {
            auto &settings =
               channel->GetIndependentSpectrogramSettings();
            channel->SetSpectrumBounds(mOrigMin, mOrigMax);
            settings = mOrigSettings;
         }
      }
   }

   if (!mWt || mOrigDefaulted) {
      SpectrogramSettings *const pSettings = &SpectrogramSettings::defaults();
      *pSettings = mOrigSettings;
   }

   const bool isOpenPage = this->IsShown();
   if (mWt && isOpenPage) {
      auto channels = TrackList::Channels(mWt);
      for (auto channel : channels)
         WaveTrackView::Get( *channel ).RestorePlacements( mOrigPlacements );
   }

   if (isOpenPage) {
      if ( mProject ) {
         auto &tp = TrackPanel::Get ( *mProject );
         tp.UpdateVRulers();
         tp.Refresh(false);
      }
   }
}

void SpectrumPrefs::Preview()
{
   if (!Validate())
      return;

   const bool isOpenPage = this->IsShown();

   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);


   mTempSettings.ConvertToActualWindowSizes();

   if (mWt) {
      for (auto channel : TrackList::Channels(mWt)) {
         if (mDefaulted) {
            channel->SetSpectrogramSettings({});
            // ... and so that the vertical scale also defaults:
            channel->SetSpectrumBounds(-1, -1);
         }
         else {
            SpectrogramSettings &settings =
               channel->GetIndependentSpectrogramSettings();
            channel->SetSpectrumBounds(mTempSettings.minFreq, mTempSettings.maxFreq);
            settings = mTempSettings;
         }
      }
   }

   if (!mWt || mDefaulted) {
      SpectrogramSettings *const pSettings = &SpectrogramSettings::defaults();
      *pSettings = mTempSettings;
   }
   mTempSettings.ConvertToEnumeratedWindowSizes();

   // Bug 2278
   // This code destroys any Multi-view we had.
   // Commenting it out as it seems not to be needed.
   /*
   if (mWt && isOpenPage) {
      for (auto channel : TrackList::Channels(mWt))
         WaveTrackView::Get( *channel )
            .SetDisplay( WaveTrackViewConstants::Spectrum );
   }
   */

   if (isOpenPage) {
      if ( mProject ) {
         auto &tp = TrackPanel::Get( *mProject );
         tp.UpdateVRulers();
         tp.Refresh(false);
      }
   }
}

bool SpectrumPrefs::Commit()
{
   if (!Validate())
      return false;

   mCommitted = true;
   SpectrogramSettings::Globals::Get().SavePrefs(); // always
   SpectrogramSettings *const pSettings = &SpectrogramSettings::defaults();
   if (!mWt || mDefaulted) {
      pSettings->SavePrefs();
   }
   pSettings->LoadPrefs(); // always; in case Globals changed

   return true;
}

bool SpectrumPrefs::ShowsPreviewButton()
{
   return mProject != nullptr;
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
   EVT_CHOICE(ID_COLOR_SCHEME, SpectrumPrefs::OnControl)
   EVT_CHECKBOX(ID_SPECTRAL_SELECTION, SpectrumPrefs::OnControl)

END_EVENT_TABLE()

PrefsPanel::Factory
SpectrumPrefsFactory( WaveTrack *wt )
{
   return [=](wxWindow *parent, wxWindowID winid, AudacityProject *pProject)
   {
      wxASSERT(parent); // to justify safenew
      return safenew SpectrumPrefs(parent, winid, pProject, wt);
   };
}

namespace{
PrefsPanel::Registration sAttachment{ "Spectrum",
   SpectrumPrefsFactory( nullptr ),
   false,
   // Place it at a lower tree level
   { "Tracks" }
};
}
