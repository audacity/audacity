/**********************************************************************

  Audacity: A Digital Audio Editor

  @file WideSampleSource.h
  @brief Adapter of WideSampleSequence to the interface AudioGraph::Source

  Dominic Mazzoni
  Vaughan Johnson

  Paul Licameli split from PerTrackEffect.h

**********************************************************************/
#ifndef __AUDACITY_WIDE_SAMPLE_SOURCE__
#define __AUDACITY_WIDE_SAMPLE_SOURCE__

#include "AudioGraphSource.h" // to inherit
#include "SampleCount.h"
#include <functional>

class WideSampleSequence;

//! Adapts WideSampleSequence to the interface AudioGraph::Source
class MIXER_API WideSampleSource final : public AudioGraph::Source
{
public:
    //! Type of function returning false if user cancels progress
    using Poller = std::function<bool (sampleCount blockSize)>;

    /*!
     @pre `nChannels <= sequence.NChannels()`
     @post `Remaining()` == len
     */
    WideSampleSource(const WideSampleSequence& sequence, size_t nChannels, sampleCount start, sampleCount len, Poller pollUser);
    ~WideSampleSource() override;

    //! If constructed with positive length, then accepts buffers only when
    //! number of channels is positive
    bool AcceptsBuffers(const Buffers& buffers) const override;

    //! Always true
    bool AcceptsBlockSize(size_t blockSize) const override;

    std::optional<size_t> Acquire(Buffers& data, size_t bound) override;
    sampleCount Remaining() const override;
    //! Can test for user cancellation
    bool Release() override;
private:
    const WideSampleSequence& mSequence;
    const size_t mnChannels;
    const Poller mPollUser;

    sampleCount mPos{};
    sampleCount mOutputRemaining{};
    size_t mLastProduced{};
    size_t mFetched{};
    bool mInitialized{ false };
};
#endif
