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
#include "../FFT.h"
#include "../Prefs.h"
#include "../RealFFTf.h"

#include <algorithm>
#include <cmath>

SpectrogramSettings::SpectrogramSettings()
   : hFFT(0)
   , window(0)
{
   UpdatePrefs();
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

int SpectrogramSettings::GetFFTLength(bool autocorrelation) const
{
   return windowSize
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
      * (!autocorrelation ? zeroPaddingFactor : 1);
#endif
   ;
}
