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

#include "../Prefs.h"
#include "../Project.h"
#include "../ShuttleGui.h"
#include "../FFT.h"

#include <algorithm>

SpectrumPrefs::SpectrumPrefs(wxWindow * parent)
:  PrefsPanel(parent, _("Spectrograms"))
{
   int windowSize = gPrefs->Read(wxT("/Spectrum/FFTSize"), 256);
   Populate(windowSize);
}

SpectrumPrefs::~SpectrumPrefs()
{
}

enum { maxWindowSize = 32768 };

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

   int lastCode = 0;
   for (size_t i = 0; i < mSizeChoices.GetCount(); i++) {
      mSizeCodes.Add(lastCode = 1 << (i + 3));
   }
   wxASSERT(lastCode == maxWindowSize);

   PopulatePaddingChoices(windowSize);

   for (int i = 0; i < NumWindowFuncs(); i++) {
      mTypeChoices.Add(WindowFuncName(i));
      mTypeCodes.Add(i);
   }


   //------------------------- Main section --------------------
   // Now construct the GUI itself.
   // Use 'eIsCreatingFromPrefs' so that the GUI is
   // initialised with values from gPrefs.
   ShuttleGui S(this, eIsCreatingFromPrefs);
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

   mZeroPaddingCodes.Clear();

   int padding = 1;
   int numChoices = 0;
   while (windowSize <= maxWindowSize) {
      const wxString numeral = wxString::Format(wxT("%d"), padding);
      mZeroPaddingChoices.Add(numeral);
      mZeroPaddingCodes.Add(padding);
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

   S.StartStatic(_("FFT Window"));
   {
      S.StartMultiColumn(2);
      {
         S.Id(ID_WINDOW_SIZE).TieChoice(_("Window &size:"),
                     wxT("/Spectrum/FFTSize"),
                     256,
                     mSizeChoices,
                     mSizeCodes);
         S.SetSizeHints(mSizeChoices);

         S.TieChoice(_("Window &type:"),
                     wxT("/Spectrum/WindowType"),
                     3,
                     mTypeChoices,
                     mTypeCodes);
         S.SetSizeHints(mTypeChoices);

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
         S.Id(ID_PADDING_SIZE).TieChoice(_("&Zero padding factor") + wxString(wxT(":")),
                     wxT("/Spectrum/ZeroPaddingFactor"),
                     mZeroPaddingChoice,
                     mZeroPaddingChoices,
                     mZeroPaddingCodes);
         S.SetSizeHints(mZeroPaddingChoices);
#endif
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
// Search and replace with _ if you want translation.
#define TRANSLATABLE( x ) wxT(x)
   wxArrayString wskipn;
   wxArrayInt wskipv;

   for (size_t i = 0; i < 7; i++) {
      wskipn.Add(wxString::Format(wxT("%d"), (1 << i) - 1));
      wskipv.Add((1 << i) - 1);
   }

   /* /////i18n-hint: (noun) Experimental.  Don't know what it does.  Don't translate.*/
   S.StartStatic(TRANSLATABLE("FFT Skip Points"));
   {
      S.StartMultiColumn(2);
      {
         /* /////i18n-hint: (noun) here the user chooses points to skip.*/
         S.TieChoice(TRANSLATABLE("Skip Points:"),
                     wxT("/Spectrum/FFTSkipPoints"),
                     0,
                     wskipn,
                     wskipv);
         S.SetSizeHints(wskipn);
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
#endif //EXPERIMENTAL_FFT_SKIP_POINTS

   S.StartStatic(_("Display"));
   {
      S.StartTwoColumn();
      {
         mMinFreq =
            S.TieNumericTextBox(_("Mi&nimum Frequency (Hz):"),
                                wxT("/Spectrum/MinFreq"),
                                0,
                                12);

         mMaxFreq =
            S.TieNumericTextBox(_("Ma&ximum Frequency (Hz):"),
                                wxT("/Spectrum/MaxFreq"),
                                8000,
                                12);

         mGain =
            S.TieNumericTextBox(_("&Gain (dB):"),
                                wxT("/Spectrum/Gain"),
                                20,
                                8);

         mRange =
            S.TieNumericTextBox(_("&Range (dB):"),
                                wxT("/Spectrum/Range"),
                                80,
                                8);

         mFrequencyGain =
            S.TieNumericTextBox(_("Frequency g&ain (dB/dec):"),
                                wxT("/Spectrum/FrequencyGain"),
                                0,
                                4);
      }
      S.EndTwoColumn();

      S.TieCheckBox(_("S&how the spectrum using grayscale colors"),
                    wxT("/Spectrum/Grayscale"),
                    false);

#ifdef EXPERIMENTAL_FFT_Y_GRID
      S.TieCheckBox(_("Show a grid along the &Y-axis"),
                    wxT("/Spectrum/FFTYGrid"),
                    false);
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
                                wxT("/Spectrum/FindNotesMinA"),
                                -30L,
                                8);

         mFindNotesN =
            S.TieNumericTextBox(_("Max. Number of Notes (1..128):"),
                                wxT("/Spectrum/FindNotesN"),
                                5L,
                                8);
      }
      S.EndTwoColumn();

      S.TieCheckBox(_("&Find Notes"),
                    wxT("/Spectrum/FFTFindNotes"),
                    false);

      S.TieCheckBox(_("&Quantize Notes"),
                    wxT("/Spectrum/FindNotesQuantize"),
                    false);
   }
   S.EndStatic();
#endif //EXPERIMENTAL_FIND_NOTES
}

bool SpectrumPrefs::Validate()
{
   long maxFreq;
   if (!mMaxFreq->GetValue().ToLong(&maxFreq)) {
      wxMessageBox(_("The maximum frequency must be an integer"));
      return false;
   }
   if (maxFreq < 100) {
      wxMessageBox(_("Maximum frequency must be 100 Hz or above"));
      return false;
   }

   long minFreq;
   if (!mMinFreq->GetValue().ToLong(&minFreq)) {
      wxMessageBox(_("The minimum frequency must be an integer"));
      return false;
   }
   if (minFreq < 0) {
      wxMessageBox(_("Minimum frequency must be at least 0 Hz"));
      return false;
   }

   if (maxFreq < minFreq) {
      wxMessageBox(_("Minimum frequency must be less than maximum frequency"));
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
   if (range <= 0) {
      wxMessageBox(_("The range must be at least 1 dB"));
      return false;
   }

   long frequencygain;
   if (!mFrequencyGain->GetValue().ToLong(&frequencygain)) {
      wxMessageBox(_("The frequency gain must be an integer"));
      return false;
   }
   if (frequencygain < 0) {
      wxMessageBox(_("The frequency gain cannot be negative"));
      return false;
   }
   if (frequencygain > 60) {
      wxMessageBox(_("The frequency gain must be no more than 60 dB/dec"));
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

   return true;
}

bool SpectrumPrefs::Apply()
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   SpectrogramSettings::defaults().UpdatePrefs();

   return true;
}

void SpectrumPrefs::OnWindowSize(wxCommandEvent &)
{
   wxChoice *const pWindowSizeControl =
      static_cast<wxChoice*>(wxWindow::FindWindowById(ID_WINDOW_SIZE, this));
   int windowSize = 1 << (pWindowSizeControl->GetSelection() + 3);
   PopulatePaddingChoices(windowSize);
}

BEGIN_EVENT_TABLE(SpectrumPrefs, PrefsPanel)
   EVT_CHOICE(ID_WINDOW_SIZE, SpectrumPrefs::OnWindowSize)
END_EVENT_TABLE()

SpectrogramSettings::SpectrogramSettings()
: hFFT(0)
, window(0)
{
   UpdatePrefs();
}

SpectrogramSettings& SpectrogramSettings::defaults()
{
   static SpectrogramSettings instance;
   return instance;
}

void SpectrogramSettings::UpdatePrefs()
{
   bool destroy = false;

   minFreq = gPrefs->Read(wxT("/Spectrum/MinFreq"), -1L);
   maxFreq = gPrefs->Read(wxT("/Spectrum/MaxFreq"), 8000L);

   // These preferences are not written anywhere in the program as of now,
   // but I keep this legacy here.  Who knows, someone might edit prefs files
   // directly.  PRL
   logMaxFreq = gPrefs->Read(wxT("/SpectrumLog/MaxFreq"), -1);
   if (logMaxFreq < 0)
      logMaxFreq = maxFreq;
   logMinFreq = gPrefs->Read(wxT("/SpectrumLog/MinFreq"), -1);
   if (logMinFreq < 0)
      logMinFreq = minFreq;
   if (logMinFreq < 1)
      logMinFreq = 1;

   range = gPrefs->Read(wxT("/Spectrum/Range"), 80L);
   gain = gPrefs->Read(wxT("/Spectrum/Gain"), 20L);
   frequencyGain = gPrefs->Read(wxT("/Spectrum/FrequencyGain"), 0L);

   const int newWindowSize = gPrefs->Read(wxT("/Spectrum/FFTSize"), 256);
   if (newWindowSize != windowSize) {
      destroy = true;
      windowSize = newWindowSize;
   }

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   const int newZeroPaddingFactor = gPrefs->Read(wxT("/Spectrum/ZeroPaddingFactor"), 1);
   if (newZeroPaddingFactor != zeroPaddingFactor) {
      destroy = true;
      zeroPaddingFactor = newZeroPaddingFactor;
   }
#endif

   int newWindowType;
   gPrefs->Read(wxT("/Spectrum/WindowType"), &newWindowType, 3);
   if (newWindowType != windowType) {
      destroy = true;
      windowType = newWindowType;
   }

   isGrayscale = (gPrefs->Read(wxT("/Spectrum/Grayscale"), 0L) != 0);

#ifdef EXPERIMENTAL_FFT_SKIP_POINTS
   fftSkipPoints = gPrefs->Read(wxT("/Spectrum/FFTSkipPoints"), 0L);
#endif

#ifdef EXPERIMENTAL_FFT_Y_GRID
   fftYGrid = (gPrefs->Read(wxT("/Spectrum/FFTYGrid"), 0L) != 0);
#endif //EXPERIMENTAL_FFT_Y_GRID

#ifdef EXPERIMENTAL_FIND_NOTES
   fftFindNotes = (gPrefs->Read(wxT("/Spectrum/FFTFindNotes"), 0L) != 0);
   findNotesMinA = gPrefs->Read(wxT("/Spectrum/FindNotesMinA"), -30.0);
   numberOfMaxima = gPrefs->Read(wxT("/Spectrum/FindNotesN"), 5L);
   findNotesQuantize = (gPrefs->Read(wxT("/Spectrum/FindNotesQuantize"), 0L) != 0);
#endif //EXPERIMENTAL_FIND_NOTES

   if (destroy)
      DestroyWindows();
}

SpectrogramSettings::~SpectrogramSettings()
{
   DestroyWindows();
}

void SpectrogramSettings::DestroyWindows()
{
#ifdef EXPERIMENTAL_USE_REALFFTF
   if (hFFT != NULL) {
      EndFFT(hFFT);
      hFFT = NULL;
   }
   if (window != NULL) {
      delete[] window;
      window = NULL;
   }
#endif
}


namespace
{
   enum { WINDOW, TWINDOW, DWINDOW };
   void RecreateWindow(
      float *&window, int which, int fftLen,
      int padding, int windowType, int windowSize, double &scale)
   {
      if (window != NULL)
         delete[] window;
      // Create the requested window function
      window = new float[fftLen];
      int ii;

      wxASSERT(windowSize % 2 == 0);
      const int endOfWindow = padding + windowSize;
      // Left and right padding
      for (ii = 0; ii < padding; ++ii) {
         window[ii] = 0.0;
         window[fftLen - ii - 1] = 0.0;
      }
      // Default rectangular window in the middle
      for (; ii < endOfWindow; ++ii)
         window[ii] = 1.0;
      // Overwrite middle as needed
      switch (which) {
      case WINDOW:
         WindowFunc(windowType, windowSize, window + padding);
         // NewWindowFunc(windowType, windowSize, extra, window + padding);
         break;
      case TWINDOW:
         wxASSERT(false);
#if 0
         // Future, reassignment
         NewWindowFunc(windowType, windowSize, extra, window + padding);
         for (int ii = padding, multiplier = -windowSize / 2; ii < endOfWindow; ++ii, ++multiplier)
            window[ii] *= multiplier;
         break;
#endif
      case DWINDOW:
         wxASSERT(false);
#if 0
         // Future, reassignment
         DerivativeOfWindowFunc(windowType, windowSize, extra, window + padding);
         break;
#endif
      default:
         wxASSERT(false);
      }
      // Scale the window function to give 0dB spectrum for 0dB sine tone
      if (which == WINDOW) {
         scale = 0.0;
         for (ii = padding; ii < endOfWindow; ++ii)
            scale += window[ii];
         if (scale > 0)
            scale = 2.0 / scale;
      }
      for (ii = padding; ii < endOfWindow; ++ii)
         window[ii] *= scale;
   }
}

void SpectrogramSettings::CacheWindows() const
{
#ifdef EXPERIMENTAL_USE_REALFFTF
   if (hFFT == NULL || window == NULL) {

      double scale;
      const int fftLen = windowSize * zeroPaddingFactor;
      const int padding = (windowSize * (zeroPaddingFactor - 1)) / 2;

      if (hFFT != NULL)
         EndFFT(hFFT);
      hFFT = InitializeFFT(fftLen);
      RecreateWindow(window, WINDOW, fftLen, padding, windowType, windowSize, scale);
   }
#endif // EXPERIMENTAL_USE_REALFFTF
}
