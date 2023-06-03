/**********************************************************************

Audacity: A Digital Audio Editor

SampleTrackCache.h
@brief Buffer results to avoid repeated calls to WideSampleSequence::Get()

Paul Licameli split from WaveTrack.h

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_TRACK_CACHE__
#define __AUDACITY_SAMPLE_TRACK_CACHE__

#include "SampleCount.h"
#include "SampleFormat.h"
#include <memory>

class WideSampleSequence;

//! A short-lived object, during whose lifetime, the contents of the WaveTrack are assumed not to change.
/*! It can replace repeated calls to WaveTrack::Get() (each of which opens and closes at least one block).
 */
class MIXER_API SampleTrackCache {
public:
   SampleTrackCache()
      : mBufferSize(0)
      , mNValidBuffers(0)
   {
   }

   explicit SampleTrackCache(
      const std::shared_ptr<const WideSampleSequence> &pSequence
   )  : mBufferSize(0)
      , mNValidBuffers(0)
   {
      SetSequence(pSequence);
   }
   ~SampleTrackCache();

   const std::shared_ptr<const WideSampleSequence>& GetSequence() const {
      return mpSequence;
   }
   void SetSequence(
      const std::shared_ptr<const WideSampleSequence> &pSequence);

   //! Retrieve samples as floats from the track or from the memory cache
   /*! Uses fillZero always
    @return both null on failure; this object owns the memory; may be
    invalidated if GetFloats() is called again

    @pre `nChannels <= GetSequence()->NChannels()`
    @pre `nChannels <= 2`
   */
   std::pair<const float *, const float *>
   GetFloatsWide(
      size_t nChannels, sampleCount start, size_t len, bool mayThrow);

   //! fetch first channel only
   const float *GetFloats(sampleCount start, size_t len, bool mayThrow)
   {
      return GetFloatsWide(1, start, len, mayThrow).first;
   }

private:
   void Free();

   struct Buffer {
      FloatBuffers areas;
      sampleCount start;
      sampleCount len;

      Buffer() : start(0), len(0) {}
      void Free() { areas.reset(); start = 0; len = 0; }
      sampleCount end() const { return start + len; }

      void swap ( Buffer &other )
      {
         areas .swap ( other.areas );
         std::swap( start, other.start );
         std::swap( len, other.len );
      }

      auto GetBuffers() const {
         return reinterpret_cast<float **>(areas.get()); }

      std::pair<const float*, const float *>
      GetResults(size_t nChannels, size_t offset) const {
         const auto buffers = GetBuffers();
         const auto buffer0 = buffers[0],
            buffer1 = buffers[1];
         return {
            (buffer0 ? buffer0 + offset : nullptr),
            (nChannels > 1 && buffer1 ? buffer1 + offset : nullptr)
         };
      }
   };

   std::shared_ptr<const WideSampleSequence> mpSequence;
   size_t mBufferSize;
   Buffer mBuffers[2];
   GrowableSampleBuffer mOverlapBuffers[2];
   int mNValidBuffers;
};

#endif
