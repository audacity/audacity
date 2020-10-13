/**********************************************************************

  Audacity: A Digital Audio Editor

  @file SpectrumCache.h

  Paul Licameli split from WaveClip.h

*******************************************************************/

#ifndef __AUDACITY_WAVECLIP_SPECTRUM_CACHE__
#define __AUDACITY_WAVECLIP_SPECTRUM_CACHE__

class sampleCount;
class SpectrogramSettings;
class SampleTrackCache;

#include <vector>
#include "MemoryX.h"
#include "WaveClip.h" // to inherit WaveClipListener

using Floats = ArrayOf<float>;

class AUDACITY_DLL_API SpecCache {
public:

   // Make invalid cache
   SpecCache()
      : algorithm(-1)
      , pps(-1.0)
      , start(-1.0)
      , windowType(-1)
      , frequencyGain(-1)
      , dirty(-1)
   {
   }

   ~SpecCache()
   {
   }

   bool Matches(int dirty_, double pixelsPerSecond,
      const SpectrogramSettings &settings, double rate) const;

   // Calculate one column of the spectrum
   bool CalculateOneSpectrum
      (const SpectrogramSettings &settings,
       SampleTrackCache &waveTrackCache,
       const int xx, sampleCount numSamples,
       double offset, double rate, double pixelsPerSecond,
       int lowerBoundX, int upperBoundX,
       const std::vector<float> &gainFactors,
       float* __restrict scratch,
       float* __restrict out) const;

   // Grow the cache while preserving the (possibly now invalid!) contents
   void Grow(size_t len_, const SpectrogramSettings& settings,
               double pixelsPerSecond, double start_);

   // Calculate the dirty columns at the begin and end of the cache
   void Populate
      (const SpectrogramSettings &settings, SampleTrackCache &waveTrackCache,
       int copyBegin, int copyEnd, size_t numPixels,
       sampleCount numSamples,
       double offset, double rate, double pixelsPerSecond);

   size_t       len { 0 }; // counts pixels, not samples
   int          algorithm;
   double       pps;
   double       leftTrim{ .0 };
   double       rightTrim{ .0 };
   double       start;
   int          windowType;
   size_t       windowSize { 0 };
   unsigned     zeroPaddingFactor { 0 };
   int          frequencyGain;
   std::vector<float> freq;
   std::vector<sampleCount> where;

   int          dirty;
};

class SpecPxCache {
public:
   SpecPxCache(size_t cacheLen)
      : len{ cacheLen }
      , values{ len }
   {
      valid = false;
      scaleType = 0;
      range = gain = -1;
      minFreq = maxFreq = -1;
   }

   size_t  len;
   Floats values;
   bool         valid;

   int scaleType;
   int range;
   int gain;
   int minFreq;
   int maxFreq;
};

struct WaveClipSpectrumCache final : WaveClipListener
{
   WaveClipSpectrumCache();
   ~WaveClipSpectrumCache() override;

   // Cache of values to colour pixels of Spectrogram - used by TrackArtist
   std::unique_ptr<SpecPxCache> mSpecPxCache;
   std::unique_ptr<SpecCache> mSpecCache;
   int mDirty { 0 };

   static WaveClipSpectrumCache &Get( const WaveClip &clip );

   void MarkChanged() override; // NOFAIL-GUARANTEE
   void Invalidate() override; // NOFAIL-GUARANTEE

   /** Getting high-level data for screen display */
   bool GetSpectrogram(const WaveClip &clip, SampleTrackCache &cache,
                       const float *& spectrogram,
                       const sampleCount *& where,
                       size_t numPixels,
                       double t0, double pixelsPerSecond);
};

#endif
