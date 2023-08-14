/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WaveformCache.cpp

  Paul Licameli split from WaveClip.cpp

**********************************************************************/

#include "WaveformCache.h"

#include <cmath>
#include "Sequence.h"
#include "GetWaveDisplay.h"
#include "WaveClipUtilities.h"
#include "WaveTrack.h"

class WaveCache {
public:
   WaveCache()
      : dirty(-1)
      , start(-1)
      , samplesPerPixel(0)
      , rate(-1)
      , where(0)
      , min(0)
      , max(0)
      , rms(0)
   {
   }

   WaveCache(size_t len_, double samplesPerPixel, double rate_, double t0, int dirty_)
      : dirty(dirty_)
      , len(len_)
      , start(t0)
      , samplesPerPixel(samplesPerPixel)
      , rate(rate_)
      , where(1 + len)
      , min(len)
      , max(len)
      , rms(len)
   {
   }

   ~WaveCache()
   {
   }

   int          dirty;
   const size_t len { 0 }; // counts pixels, not samples
   const double start;
   const double samplesPerPixel;
   const int    rate;
   std::vector<sampleCount> where;
   std::vector<float> min;
   std::vector<float> max;
   std::vector<float> rms;
};

//
// Getting high-level data from the track for screen display and
// clipping calculations
//

bool WaveClipWaveformCache::GetWaveDisplay(
   const WaveChannelInterval &clip, WaveDisplay &display,
   double t0, double pixelsPerSecond)
{
   auto &waveCache = mWaveCaches[clip.GetChannelIndex()];

   t0 += clip.GetTrimLeft();

   const bool allocated = (display.where != 0);

   const size_t numPixels = (int)display.width;

   size_t p0 = 0;         // least column requiring computation
   size_t p1 = numPixels; // greatest column requiring computation, plus one

   float *min;
   float *max;
   float *rms;
   std::vector<sampleCount> *pWhere;

   if (allocated) {
      // assume ownWhere is filled.
      min = &display.min[0];
      max = &display.max[0];
      rms = &display.rms[0];
      pWhere = &display.ownWhere;
   }
   else {
      const auto sampleRate = clip.GetRate();
      const auto stretchRatio = clip.GetStretchRatio();
      const double samplesPerPixel =
         sampleRate / pixelsPerSecond / stretchRatio;

      // Make a tolerant comparison of the samples-per-pixel values in this wise:
      // accumulated difference of times over the number of pixels is less than
      // a sample period.
      const bool samplesPerPixelMatch =
         waveCache &&
         (fabs(samplesPerPixel - waveCache->samplesPerPixel) * numPixels < 1.0);

      const bool match = waveCache && samplesPerPixelMatch &&
                         waveCache->len > 0 && waveCache->dirty == mDirty;

      if (match &&
         waveCache->start == t0 &&
         waveCache->len >= numPixels) {

         // Satisfy the request completely from the cache
         display.min = &waveCache->min[0];
         display.max = &waveCache->max[0];
         display.rms = &waveCache->rms[0];
         display.where = &waveCache->where[0];
         return true;
      }

      std::unique_ptr<WaveCache> oldCache(std::move(waveCache));

      int oldX0 = 0;
      double correction = 0.0;
      size_t copyBegin = 0, copyEnd = 0;
      if (match) {
         findCorrection(
            oldCache->where, oldCache->len, numPixels, t0, sampleRate,
            stretchRatio, samplesPerPixel, oldX0, correction);
         // Remember our first pixel maps to oldX0 in the old cache,
         // possibly out of bounds.
         // For what range of pixels can data be copied?
         copyBegin = std::min<size_t>(numPixels, std::max(0, -oldX0));
         copyEnd = std::min<size_t>(numPixels, std::max(0,
            (int)oldCache->len - oldX0
         ));
      }
      if (!(copyEnd > copyBegin))
         oldCache.reset(0);

      waveCache = std::make_unique<WaveCache>(
         numPixels, samplesPerPixel, sampleRate, t0, mDirty);
      min = &waveCache->min[0];
      max = &waveCache->max[0];
      rms = &waveCache->rms[0];
      pWhere = &waveCache->where;

      constexpr auto addBias = false;
      fillWhere(
         *pWhere, numPixels, addBias, correction, t0, sampleRate, stretchRatio,
         samplesPerPixel);

      // The range of pixels we must fetch from the Sequence:
      p0 = (copyBegin > 0) ? 0 : copyEnd;
      p1 = (copyEnd >= numPixels) ? copyBegin : numPixels;

      // Optimization: if the old cache is good and overlaps
      // with the current one, re-use as much of the cache as
      // possible

      if (oldCache) {

         // Copy what we can from the old cache.
         const int length = copyEnd - copyBegin;
         const size_t sizeFloats = length * sizeof(float);
         const int srcIdx = (int)copyBegin + oldX0;
         memcpy(&min[copyBegin], &oldCache->min[srcIdx], sizeFloats);
         memcpy(&max[copyBegin], &oldCache->max[srcIdx], sizeFloats);
         memcpy(&rms[copyBegin], &oldCache->rms[srcIdx], sizeFloats);
      }
   }

   if (p1 > p0) {
      // Cache was not used or did not satisfy the whole request
      std::vector<sampleCount> &where = *pWhere;

      /* handle values in the append buffer */

      const auto &sequence = clip.GetSequence();
      auto numSamples = sequence.GetNumSamples();
      auto a = p0;

      // Not all of the required columns might be in the sequence.
      // Some might be in the append buffer.
      for (; a < p1; ++a) {
         if (where[a + 1] > numSamples)
            break;
      }

      // Handle the columns that land in the append buffer.
      //compute the values that are outside the overlap from scratch.
      if (a < p1) {
         const auto appendBufferLen = clip.GetAppendBufferLen();
         const auto &appendBuffer = clip.GetAppendBuffer();
         sampleFormat seqFormat = sequence.GetSampleFormats().Stored();
         bool didUpdate = false;
         for(auto i = a; i < p1; i++) {
            auto left = std::max(sampleCount{ 0 },
                                 where[i] - numSamples);
            auto right = std::min(sampleCount{ appendBufferLen },
                                  where[i + 1] - numSamples);

            //wxCriticalSectionLocker locker(mAppendCriticalSection);

            if (right > left) {
               Floats b;
               const float *pb{};
               // left is nonnegative and at most mAppendBufferLen:
               auto sLeft = left.as_size_t();
               // The difference is at most mAppendBufferLen:
               size_t len = ( right - left ).as_size_t();

               if (seqFormat == floatSample)
                  pb = &((const float *)appendBuffer)[sLeft];
               else {
                  b.reinit(len);
                  pb = b.get();
                  SamplesToFloats(
                     appendBuffer + sLeft * SAMPLE_SIZE(seqFormat),
                     seqFormat, b.get(), len);
               }

               float theMax, theMin, sumsq;
               {
                  const float val = pb[0];
                  theMax = theMin = val;
                  sumsq = val * val;
               }
               for(decltype(len) j = 1; j < len; j++) {
                  const float val = pb[j];
                  theMax = std::max(theMax, val);
                  theMin = std::min(theMin, val);
                  sumsq += val * val;
               }

               min[i] = theMin;
               max[i] = theMax;
               rms[i] = (float)sqrt(sumsq / len);

               didUpdate=true;
            }
         }

         // Shrink the right end of the range to fetch from Sequence
         if(didUpdate)
            p1 = a;
      }

      // Done with append buffer, now fetch the rest of the cache miss
      // from the sequence
      if (p1 > p0) {
         if (!::GetWaveDisplay(sequence, &min[p0], &max[p0], &rms[p0], p1 - p0,
            &where[p0]))
         {
            return false;
         }
      }
   }

   if (!allocated) {
      // Now report the results
      display.min = min;
      display.max = max;
      display.rms = rms;
      display.where = &(*pWhere)[0];
   }

   return true;
}

WaveClipWaveformCache::WaveClipWaveformCache(size_t nChannels)
   // TODO wide wave tracks -- won't need std::max here
   : mWaveCaches(std::max<size_t>(2, nChannels))
{
   for (auto &pCache : mWaveCaches)
      pCache = std::make_unique<WaveCache>();
}

WaveClipWaveformCache::~WaveClipWaveformCache()
{
}

static WaveClip::Caches::RegisteredFactory sKeyW{ [](WaveClip &clip) {
   return std::make_unique<WaveClipWaveformCache>(clip.GetWidth());
} };

WaveClipWaveformCache &WaveClipWaveformCache::Get( const WaveClip &clip )
{
   return const_cast< WaveClip& >( clip ) // Consider it mutable data
      .Caches::Get< WaveClipWaveformCache >( sKeyW );
}

void WaveClipWaveformCache::MarkChanged()
{
   ++mDirty;
}

void WaveClipWaveformCache::Invalidate()
{
   // Invalidate wave display caches
   for (auto &pCache : mWaveCaches)
      pCache = std::make_unique<WaveCache>();
}
