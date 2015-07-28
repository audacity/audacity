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
      stUndertone,

      stNumScaleTypes,
   };

   static void InvalidateNames(); // in case of language change
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
      (double rate, bool bins) const;

private:
   int minFreq;
   int maxFreq;
   int logMinFreq;
   int logMaxFreq;
public:
   int GetMinFreq(double rate) const;
   int GetMaxFreq(double rate) const;
   int GetLogMinFreq(double rate) const;
   int GetLogMaxFreq(double rate) const;
   bool SpectralSelectionEnabled() const;

   void SetMinFreq(int freq);
   void SetMaxFreq(int freq);
   void SetLogMinFreq(int freq);
   void SetLogMaxFreq(int freq);

public:
   int range;
   int gain;
   int frequencyGain;

   int windowType;
   int windowSize;
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   int zeroPaddingFactor;
#endif

   int GetFFTLength() const; // window size (times zero padding, if STFT)

   bool isGrayscale;

   ScaleType scaleType;

#ifndef SPECTRAL_SELECTION_GLOBAL_SWITCH
   bool spectralSelection; // But should this vary per track? -- PRL
#endif

   enum Algorithm {
      algSTFT = 0,
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

#ifdef EXPERIMENTAL_USE_REALFFTF
   // Variables used for computing the spectrum
   mutable FFTParam      *hFFT;
   mutable float         *window;
#endif
};
#endif
