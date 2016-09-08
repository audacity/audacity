/**********************************************************************

Audacity: A Digital Audio Editor

SpectrogramSettings.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SPECTROGRAM_SETTINGS__
#define __AUDACITY_SPECTROGRAM_SETTINGS__

#include "../Experimental.h"

#undef SPECTRAL_SELECTION_GLOBAL_SWITCH

struct FFTParam;
class NumberScale;
class SpectrumPrefs;
class wxArrayString;

class SpectrogramSettings
{
   friend class SpectrumPrefs;
public:

   // Singleton for settings that are not per-track
   class Globals
   {
   public:
      static Globals &Get();
      void SavePrefs();

#ifdef SPECTRAL_SELECTION_GLOBAL_SWITCH
      bool spectralSelection;
#endif

   private:
      Globals();
      void LoadPrefs();
   };

   enum {
      LogMinWindowSize = 3,
      LogMaxWindowSize = 15,

      NumWindowSizes = LogMaxWindowSize - LogMinWindowSize + 1,
   };

   // Do not assume that this enumeration will remain the
   // same as NumberScaleType in future.  That enum may become
   // more general purpose.
   enum ScaleType {
      stLinear,
      stLogarithmic,
      stMel,
      stBark,
      stErb,
      stPeriod,

      stNumScaleTypes,
   };

   static const wxArrayString &GetScaleNames();
   static const wxArrayString &GetAlgorithmNames();

   static SpectrogramSettings &defaults();
   SpectrogramSettings();
   SpectrogramSettings(const SpectrogramSettings &other);
   SpectrogramSettings& operator= (const SpectrogramSettings &other);
   ~SpectrogramSettings();

   bool IsDefault() const
   {
      return this == &defaults();
   }

   bool Validate(bool quiet);
   void LoadPrefs();
   void SavePrefs();
   void InvalidateCaches();
   void DestroyWindows();
   void CacheWindows() const;
   void ConvertToEnumeratedWindowSizes();
   void ConvertToActualWindowSizes();

   // If "bins" is false, units are Hz
   NumberScale GetScale
      (float minFreq, float maxFreq, double rate, bool bins) const;

   int minFreq;
   int maxFreq;

   bool SpectralSelectionEnabled() const;

public:
   int range;
   int gain;
   int frequencyGain;

   int windowType;

private:
   int windowSize;
public:
   size_t WindowSize() const { return windowSize; }

#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
private:
   int zeroPaddingFactor;
public:
   size_t ZeroPaddingFactor() const { return zeroPaddingFactor; }
#endif

   size_t GetFFTLength() const; // window size (times zero padding, if STFT)

   bool isGrayscale;

   ScaleType scaleType;

#ifndef SPECTRAL_SELECTION_GLOBAL_SWITCH
   bool spectralSelection; // But should this vary per track? -- PRL
#endif

   enum Algorithm {
      algSTFT = 0,
      algReassignment,
      algPitchEAC,

      algNumAlgorithms,
   };
   Algorithm algorithm;

#ifdef EXPERIMENTAL_FFT_Y_GRID
   bool fftYGrid;
#endif //EXPERIMENTAL_FFT_Y_GRID

#ifdef EXPERIMENTAL_FIND_NOTES
   bool fftFindNotes;
   bool findNotesMinA;
   bool numberOfMaxima;
   bool findNotesQuantize;
#endif //EXPERIMENTAL_FIND_NOTES

   // Following fields are derived from preferences.

   // Variables used for computing the spectrum
   mutable FFTParam      *hFFT{};
   mutable float         *window{};

   // Two other windows for computing reassigned spectrogram
   mutable float         *tWindow{}; // Window times time parameter
   mutable float         *dWindow{}; // Derivative of window
};
#endif
