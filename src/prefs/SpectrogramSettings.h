/**********************************************************************

Audacity: A Digital Audio Editor

SpectrogramSettings.h

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SPECTROGRAM_SETTINGS__
#define __AUDACITY_SPECTROGRAM_SETTINGS__

#include "../Experimental.h"

struct FFTParam;

class SpectrogramSettings
{
public:
   static SpectrogramSettings &defaults();
   SpectrogramSettings();
   SpectrogramSettings(const SpectrogramSettings &other);
   SpectrogramSettings& operator= (const SpectrogramSettings &other);
   ~SpectrogramSettings();

   void UpdatePrefs();
   void DestroyWindows();
   void CacheWindows() const;

   int minFreq;
   int maxFreq;

   int logMinFreq;
   int logMaxFreq;

   int range;
   int gain;
   int frequencyGain;

   int windowType;
   int windowSize;
#ifdef EXPERIMENTAL_ZERO_PADDED_SPECTROGRAMS
   int zeroPaddingFactor;
#endif

   int GetFFTLength(bool autocorrelation) const; // window size (times zero padding, if STFT)

   bool isGrayscale;

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
