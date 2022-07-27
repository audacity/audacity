/**********************************************************************

  Audacity: A Digital Audio Editor

  @file SampleTrackSource.h
  @brief Adapter of SampleTrack to the interface AudioGraph::Source

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from PerTrackEffect.h

**********************************************************************/

#ifndef __AUDACITY_SAMPLE_TRACK_SOURCE__
#define __AUDACITY_SAMPLE_TRACK_SOURCE__

#include "AudioGraphSource.h" // to inherit
#include "SampleCount.h"
#include <functional>

class SampleTrack;

//! Adapts SampleTrack to the interface AudioGraph::Source
class SAMPLE_TRACK_API SampleTrackSource final : public AudioGraph::Source {
public:
   //! Type of function returning false if user cancels progress
   using Poller = std::function<bool(sampleCount blockSize)>;

   /*!
    @post `Remaining()` == len
    */
   SampleTrackSource(const SampleTrack &left, const SampleTrack *pRight,
      sampleCount start, sampleCount len, Poller pollUser);
   ~SampleTrackSource() override;

   //! If constructed with positive length, then accepts buffers only when
   //! number of channels is positive
   bool AcceptsBuffers(const Buffers &buffers) const override;

   //! Always true
   bool AcceptsBlockSize(size_t blockSize) const override;

   std::optional<size_t> Acquire(Buffers &data, size_t bound) override;
   sampleCount Remaining() const override;
   //! Can test for user cancellation
   bool Release() override;
private:
   const SampleTrack &mLeft;
   const SampleTrack *const mpRight;
   const Poller mPollUser;

   sampleCount mPos{};
   sampleCount mOutputRemaining{};
   size_t mLastProduced{};
   size_t mFetched{};
   bool mInitialized{ false };
};
#endif
