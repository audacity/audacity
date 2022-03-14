/**********************************************************************

Audacity: A Digital Audio Editor

SampleTrackCache.h
@brief Buffer results to avoid repeated calls to SampleTrack::Get()

Paul Licameli split from WaveTrack.h

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_TRACK_CACHE__
#define __AUDACITY_SAMPLE_TRACK_CACHE__

#include "SampleCount.h"
#include "SampleFormat.h"
#include <memory>

class SampleTrack;

//! A short-lived object, during whose lifetime, the contents of the WaveTrack are assumed not to change.
/*! It can replace repeated calls to WaveTrack::Get() (each of which opens and closes at least one block).
 */
class SAMPLE_TRACK_API SampleTrackCache {
public:
   SampleTrackCache()
      : mBufferSize(0)
      , mOverlapBuffer()
      , mNValidBuffers(0)
   {
   }

   explicit SampleTrackCache(const std::shared_ptr<const SampleTrack> &pTrack)
      : mBufferSize(0)
      , mOverlapBuffer()
      , mNValidBuffers(0)
   {
      SetTrack(pTrack);
   }
   ~SampleTrackCache();

   const std::shared_ptr<const SampleTrack>& GetTrack() const { return mPTrack; }
   void SetTrack(const std::shared_ptr<const SampleTrack> &pTrack);

   //! Retrieve samples as floats from the track or from the memory cache
   /*! Uses fillZero always
    @return null on failure; this object owns the memory; may be invalidated if GetFloats() is called again
   */
   const float *GetFloats(sampleCount start, size_t len, bool mayThrow);

private:
   void Free();

   struct Buffer {
      Floats data;
      sampleCount start;
      sampleCount len;

      Buffer() : start(0), len(0) {}
      void Free() { data.reset(); start = 0; len = 0; }
      sampleCount end() const { return start + len; }

      void swap ( Buffer &other )
      {
         data .swap ( other.data );
         std::swap( start, other.start );
         std::swap( len, other.len );
      }
   };

   std::shared_ptr<const SampleTrack> mPTrack;
   size_t mBufferSize;
   Buffer mBuffers[2];
   GrowableSampleBuffer mOverlapBuffer;
   int mNValidBuffers;
};

#endif
