/**********************************************************************

Audacity: A Digital Audio Editor

SpectrogramSettings.cpp

Paul Licameli

*******************************************************************//**

\class SpectrogramSettings
\brief Spectrogram settings, either for one track or as defaults.

*//*******************************************************************/

#include "../Audacity.h"
#include "SpectrogramSettings.h"
#include "../NumberScale.h"

#include <algorithm>
#include <wx/msgdlg.h>

#include "../FFT.h"
#include "../Prefs.h"
#include "../RealFFTf.h"

#include <cmath>

SpectrogramSettings::Globals::Globals()
{
   LoadPrefs();
}

void SpectrogramSettings::Globals::SavePrefs()
{
#ifdef SPECTRAL_SELECTION_GLOBAL_SWITCH
   gPrefs->Write(wxT("/Spectrum/EnableSpectralSelection"), spectralSelection);
#endif
}

void SpectrogramSettings::Globals::LoadPrefs()
{
#ifdef SPECTRAL_SELECTION_GLOBAL_SWITCH
   spectralSelection
      = (gPrefs->Read(wxT("/Spectrum/EnableSpectralSelection"), 0L) != 0);
#endif
}

SpectrogramSettings::Globals
&SpectrogramSettings::Globals::Get()
{
   static Globals instance;
   return instance;
}

SpectrogramSettings::SpectrogramSettings()
   : hFFT(0)
   , window(0)
{
   LoadPrefs();
}

SpectrogramSettings::SpectrogramSettings(const SpectrogramSettings &other)
   : minFreq(other.minFreq)
   , maxFreq(other.maxFreq)
   , logMinFreq(other.logMinFreq)
   , logMaxFreq(other.logMaxFreq)
   , range(other.range)
   , gain(other.gain)
   , frequencyGain(other.frequencyGain)
   , windowType(other.windowType)
   , windowSize(other.windowSize)
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   , zeroPaddingFactor(other.zeroPaddingFactor)
#endif
   , isGrayscale(other.isGrayscale)
   , scaleType(other.scaleType)
#ifndef SPECTRAL_SELECTION_GLOBAL_SWITCH
   , spectralSelection(other.spectralSelection)
#endif
   , algorithm(other.algorithm)
#ifdef EXPERIMENTAL_FFT_Y_GRID
   , fftYGrid(other.fftYGrid)
#endif
#ifdef EXPERIMENTAL_FIND_NOTES
   , fftFindNotes(other.fftFindNotes)
   , findNotesMinA(other.findNotesMinA)
   , numberOfMaxima(other.numberOfMaxima)
   , findNotesQuantize(other.findNotesQuantize)
#endif

   // Do not copy these!
   , hFFT(0)
   , window(0)
{
}

SpectrogramSettings &SpectrogramSettings::operator= (const SpectrogramSettings &other)
{
   if (this != &other) {
      minFreq = other.minFreq;
      maxFreq = other.maxFreq;
      logMinFreq = other.logMinFreq;
      logMaxFreq = other.logMaxFreq;
      range = other.range;
      gain = other.gain;
      frequencyGain = other.frequencyGain;
      windowType = other.windowType;
      windowSize = other.windowSize;
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
      zeroPaddingFactor = other.zeroPaddingFactor;
#endif
      isGrayscale = other.isGrayscale;
      scaleType = other.scaleType;
#ifndef SPECTRAL_SELECTION_GLOBAL_SWITCH
      spectralSelection = other.spectralSelection;
#endif
      algorithm = other.algorithm;
#ifdef EXPERIMENTAL_FFT_Y_GRID
      fftYGrid = other.fftYGrid;
#endif
#ifdef EXPERIMENTAL_FIND_NOTES
      fftFindNotes = other.fftFindNotes;
      findNotesMinA = other.findNotesMinA;
      numberOfMaxima = other.numberOfMaxima;
      findNotesQuantize = other.findNotesQuantize;
#endif

      // Do not copy these!
      hFFT = 0;
      window = 0;
   }
   return *this;
}

SpectrogramSettings& SpectrogramSettings::defaults()
{
   static SpectrogramSettings instance;
   return instance;
}

namespace
{
   wxArrayString &scaleNamesArray()
   {
      static wxArrayString theArray;
      return theArray;
   }

   wxArrayString &algorithmNamesArray()
   {
      static wxArrayString theArray;
      return theArray;
   }
}

//static
void SpectrogramSettings::InvalidateNames()
{
   scaleNamesArray().Clear();
   algorithmNamesArray().Clear();
}

//static
const wxArrayString &SpectrogramSettings::GetScaleNames()
{
   wxArrayString &theArray = scaleNamesArray();

   if (theArray.IsEmpty()) {
      // Keep in correspondence with enum SpectrogramSettings::ScaleType:
      theArray.Add(_("Linear"));
      theArray.Add(_("Logarithmic"));
      theArray.Add(_("Mel"));
      theArray.Add(_("Bark"));
      theArray.Add(_("Erb"));
      theArray.Add(_("Undertone"));
   }

   return theArray;
}

//static
const wxArrayString &SpectrogramSettings::GetAlgorithmNames()
{
   wxArrayString &theArray = algorithmNamesArray();

   if (theArray.IsEmpty()) {
      // Keep in correspondence with enum SpectrogramSettings::Algorithm:
      theArray.Add(_("STFT"));
      theArray.Add(_("Pitch (enhanced autocorrelation)"));
   }

   return theArray;
}

bool SpectrogramSettings::Validate(bool quiet)
{
   if (!quiet &&
      maxFreq < 100) {
      wxMessageBox(_("Maximum frequency must be 100 Hz or above"));
      return false;
   }
   else
      maxFreq = std::max(100, maxFreq);

   if (!quiet &&
      minFreq < 0) {
      wxMessageBox(_("Minimum frequency must be at least 0 Hz"));
      return false;
   }
   else
      minFreq = std::max(0, minFreq);

   if (!quiet &&
      maxFreq <= minFreq) {
      wxMessageBox(_("Minimum frequency must be less than maximum frequency"));
      return false;
   }
   else
      maxFreq = std::max(1 + minFreq, maxFreq);

   if (!quiet &&
      range <= 0) {
      wxMessageBox(_("The range must be at least 1 dB"));
      return false;
   }
   else
      range = std::max(1, range);

   if (!quiet &&
      frequencyGain < 0) {
      wxMessageBox(_("The frequency gain cannot be negative"));
      return false;
   }
   else if (!quiet &&
      frequencyGain > 60) {
      wxMessageBox(_("The frequency gain must be no more than 60 dB/dec"));
      return false;
   }
   else
      frequencyGain =
         std::max(0, std::min(60, frequencyGain));

   // The rest are controlled by drop-down menus so they can't go wrong
   // in the Preferences dialog, but we also come here after reading fom saved
   // preference files, which could be or from future versions.  Validate quietly.
   windowType =
      std::max(0, std::min(NumWindowFuncs() - 1, windowType));
   scaleType =
      ScaleType(std::max(0,
         std::min(int(SpectrogramSettings::stNumScaleTypes) - 1,
            int(scaleType))));
   algorithm = Algorithm(
      std::max(0, std::min(int(algNumAlgorithms) - 1, int(algorithm)))
   );
   ConvertToEnumeratedWindowSizes();
   ConvertToActualWindowSizes();

   return true;
}

void SpectrogramSettings::LoadPrefs()
{
   minFreq = gPrefs->Read(wxT("/Spectrum/MinFreq"), 0L);

   maxFreq = gPrefs->Read(wxT("/Spectrum/MaxFreq"), 8000L);

   range = gPrefs->Read(wxT("/Spectrum/Range"), 80L);
   gain = gPrefs->Read(wxT("/Spectrum/Gain"), 20L);
   frequencyGain = gPrefs->Read(wxT("/Spectrum/FrequencyGain"), 0L);

   windowSize = gPrefs->Read(wxT("/Spectrum/FFTSize"), 256);

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   zeroPaddingFactor = gPrefs->Read(wxT("/Spectrum/ZeroPaddingFactor"), 1);
#endif

   gPrefs->Read(wxT("/Spectrum/WindowType"), &windowType, eWinFuncHanning);

   isGrayscale = (gPrefs->Read(wxT("/Spectrum/Grayscale"), 0L) != 0);

   scaleType = ScaleType(gPrefs->Read(wxT("/Spectrum/ScaleType"), 0L));

#ifndef SPECTRAL_SELECTION_GLOBAL_SWITCH
   spectralSelection = (gPrefs->Read(wxT("/Spectrum/EnableSpectralSelection"), 0L) != 0);
#endif

   algorithm = Algorithm(gPrefs->Read(wxT("/Spectrum/Algorithm"), 0L));

#ifdef EXPERIMENTAL_FFT_Y_GRID
   fftYGrid = (gPrefs->Read(wxT("/Spectrum/FFTYGrid"), 0L) != 0);
#endif //EXPERIMENTAL_FFT_Y_GRID

#ifdef EXPERIMENTAL_FIND_NOTES
   fftFindNotes = (gPrefs->Read(wxT("/Spectrum/FFTFindNotes"), 0L) != 0);
   findNotesMinA = gPrefs->Read(wxT("/Spectrum/FindNotesMinA"), -30.0);
   numberOfMaxima = gPrefs->Read(wxT("/Spectrum/FindNotesN"), 5L);
   findNotesQuantize = (gPrefs->Read(wxT("/Spectrum/FindNotesQuantize"), 0L) != 0);
#endif //EXPERIMENTAL_FIND_NOTES

   // Enforce legal values
   Validate(true);

   // These preferences are not written anywhere in the program as of now,
   // but I keep this legacy here.  Who knows, someone might edit prefs files
   // directly.  PRL
   logMinFreq = gPrefs->Read(wxT("/SpectrumLog/MinFreq"), -1);
   if (logMinFreq < 0)
      logMinFreq = minFreq;
   if (logMinFreq < 1)
      logMinFreq = 1;
   logMaxFreq = gPrefs->Read(wxT("/SpectrumLog/MaxFreq"), -1);
   if (logMaxFreq < 0)
      logMaxFreq = maxFreq;
   logMaxFreq =
      std::max(logMinFreq + 1, logMaxFreq);

   InvalidateCaches();
}

void SpectrogramSettings::SavePrefs()
{
   gPrefs->Write(wxT("/Spectrum/MinFreq"), minFreq);
   gPrefs->Write(wxT("/Spectrum/MaxFreq"), maxFreq);

   // Nothing wrote these.  They only varied from the linear scale bounds in-session. -- PRL
   // gPrefs->Write(wxT("/SpectrumLog/MaxFreq"), logMinFreq);
   // gPrefs->Write(wxT("/SpectrumLog/MinFreq"), logMaxFreq);

   gPrefs->Write(wxT("/Spectrum/Range"), range);
   gPrefs->Write(wxT("/Spectrum/Gain"), gain);
   gPrefs->Write(wxT("/Spectrum/FrequencyGain"), frequencyGain);

   gPrefs->Write(wxT("/Spectrum/FFTSize"), windowSize);

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   gPrefs->Write(wxT("/Spectrum/ZeroPaddingFactor"), zeroPaddingFactor);
#endif

   gPrefs->Write(wxT("/Spectrum/WindowType"), windowType);

   gPrefs->Write(wxT("/Spectrum/Grayscale"), isGrayscale);

   gPrefs->Write(wxT("/Spectrum/ScaleType"), (int) scaleType);

#ifndef SPECTRAL_SELECTION_GLOBAL_SWITCH
   gPrefs->Write(wxT("/Spectrum/EnableSpectralSelection"), spectralSelection);
#endif

   gPrefs->Write(wxT("/Spectrum/Algorithm"), (int) algorithm);

#ifdef EXPERIMENTAL_FFT_Y_GRID
   gPrefs->Write(wxT("/Spectrum/FFTYGrid"), fftYGrid);
#endif //EXPERIMENTAL_FFT_Y_GRID

#ifdef EXPERIMENTAL_FIND_NOTES
   gPrefs->Write(wxT("/Spectrum/FFTFindNotes"), fftFindNotes);
   gPrefs->Write(wxT("/Spectrum/FindNotesMinA"), findNotesMinA);
   gPrefs->Write(wxT("/Spectrum/FindNotesN"), numberOfMaxima);
   gPrefs->Write(wxT("/Spectrum/FindNotesQuantize"), findNotesQuantize);
#endif //EXPERIMENTAL_FIND_NOTES
}

void SpectrogramSettings::InvalidateCaches()
{
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

void SpectrogramSettings::ConvertToEnumeratedWindowSizes()
{
   unsigned size;
   int logarithm;

   logarithm = -LogMinWindowSize;
   size = unsigned(windowSize);
   while (size > 1)
      size >>= 1, ++logarithm;
   windowSize = std::max(0, std::min(NumWindowSizes - 1, logarithm));

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   // Choices for zero padding begin at 1
   logarithm = 0;
   size = unsigned(zeroPaddingFactor);
   while (zeroPaddingFactor > 1)
      zeroPaddingFactor >>= 1, ++logarithm;
   zeroPaddingFactor = std::max(0,
      std::min(LogMaxWindowSize - (windowSize + LogMinWindowSize),
         logarithm
   ));
#endif
}

void SpectrogramSettings::ConvertToActualWindowSizes()
{
   windowSize = 1 << (windowSize + LogMinWindowSize);
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   zeroPaddingFactor = 1 << zeroPaddingFactor;
#endif
}

int SpectrogramSettings::GetMinFreq(double rate) const
{
   const int top = lrint(rate / 2.);
   return std::max(0, std::min(top, minFreq));
}

int SpectrogramSettings::GetMaxFreq(double rate) const
{
   const int top = lrint(rate / 2.);
   if (maxFreq < 0)
      return top;
   else
      return std::max(0, std::min(top, maxFreq));
}

int SpectrogramSettings::GetLogMinFreq(double rate) const
{
   const int top = lrint(rate / 2.);
   if (logMinFreq < 0)
      return top / 1000.0;
   else
      return std::max(1, std::min(top, logMinFreq));
}

int SpectrogramSettings::GetLogMaxFreq(double rate) const
{
   const int top = lrint(rate / 2.);
   if (logMaxFreq < 0)
      return top;
   else
      return std::max(1, std::min(top, logMaxFreq));
}

void SpectrogramSettings::SetMinFreq(int freq)
{
   minFreq = freq;
}

void SpectrogramSettings::SetMaxFreq(int freq)
{
   maxFreq = freq;
}

void SpectrogramSettings::SetLogMinFreq(int freq)
{
   logMinFreq = freq;
}

void SpectrogramSettings::SetLogMaxFreq(int freq)
{
   logMaxFreq = freq;
}

int SpectrogramSettings::GetFFTLength() const
{
   return windowSize
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
      * ((algorithm == algSTFT) ? zeroPaddingFactor : 1);
#endif
   ;
}

NumberScale SpectrogramSettings::GetScale
(double rate, bool bins) const
{
   int minFreq, maxFreq;
   NumberScaleType type = nstLinear;
   const int half = GetFFTLength() / 2;

   // Don't assume the correspondence of the enums will remain direct in the future.
   // Do this switch.
   switch (scaleType) {
   default:
      wxASSERT(false);
   case stLinear:
      type = nstLinear; break;
   case stLogarithmic:
      type = nstLogarithmic; break;
   case stMel:
      type = nstMel; break;
   case stBark:
      type = nstBark; break;
   case stErb:
      type = nstErb; break;
   case stUndertone:
      type = nstUndertone; break;
   }

   switch (scaleType) {
   default:
      wxASSERT(false);
   case stLinear:
      minFreq = GetMinFreq(rate);
      maxFreq = GetMaxFreq(rate);
      break;
   case stLogarithmic:
   case stMel:
   case stBark:
   case stErb:
      minFreq = GetLogMinFreq(rate);
      maxFreq = GetLogMaxFreq(rate);
      break;
   case stUndertone:
   {
      const float bin2 = rate / half;
      minFreq = std::max(int(0.5 + bin2), GetLogMinFreq(rate));
      maxFreq = GetLogMaxFreq(rate);
   }
   break;
   }

   return NumberScale(type, minFreq, maxFreq,
      bins ? rate / (2 * half) : 1.0f);
}

bool SpectrogramSettings::SpectralSelectionEnabled() const
{
#ifdef SPECTRAL_SELECTION_GLOBAL_SWITCH
   return Globals::Get().spectralSelection;
#else
   return spectralSelection;
#endif
}
