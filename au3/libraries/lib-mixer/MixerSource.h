/**********************************************************************

  Audacity: A Digital Audio Editor

  @file MixerSource.h
  @brief Fetches from tracks, applies envelopes, warps time, and resamples

  Dominic Mazzoni
  Markus Meyer

  Paul Licameli split from Mix.h

***********************************************************************/
#ifndef __AUDACITY_MIXER_SOURCE__
#define __AUDACITY_MIXER_SOURCE__

#include "AudioGraphSource.h"
#include "MixerOptions.h"
#include "SampleCount.h"
#include <memory>

class Resample;
class SampleTrack;
class WideSampleSequence;

//! Fetches from tracks, applies envelopes; can resample, and warp time, even
//! backwards, as for scrubbing.
/*!
 This class inherits AudioGraph::Source but does not yet fulfill the contracts
 of all of the members.  But it is not yet used through any pointer or
 reference to its base class.
 */
class MixerSource final : public AudioGraph::Source
{
public:
    using TimesAndSpeed = MixerOptions::TimesAndSpeed;
    using ResampleParameters = MixerOptions::ResampleParameters;

    /*!
     @pre `pTimesAndSpeed != nullptr`
     */
    MixerSource(const std::shared_ptr<const WideSampleSequence>& seq, size_t bufferSize, double rate, const MixerOptions::Warp& options,
                bool highQuality, bool mayThrow, std::shared_ptr<TimesAndSpeed> pTimesAndSpeed);
    MixerSource(MixerSource&&) noexcept = default;
    ~MixerSource() override;

    unsigned Channels() const { return mnChannels; }
    const WideSampleSequence& GetSequence() const;

    bool AcceptsBuffers(const Buffers& buffers) const override;
    bool AcceptsBlockSize(size_t blockSize) const override;
    std::optional<size_t> Acquire(Buffers& data, size_t bound) override;
    sampleCount Remaining() const override;
    bool Release() override;
    //! @return false
    bool Terminates() const override;
    void Reposition(double time, bool skipping);

    bool VariableRates() const { return mResampleParameters.mVariableRates; }

private:
    void MakeResamplers();

    //! Cut the queue into blocks of this finer size
    //! for variable rate resampling.  Each block is resampled at some
    //! constant rate.
    static constexpr size_t sProcessLen = 1024;

    //! This is the number of samples grabbed in one go from a track
    //! and placed in a queue, when mixing with resampling.
    /*!
     (Should we use SampleTrack::GetBestBlockSize instead?)
     */
    static constexpr size_t sQueueMaxLen = 65536;

    /*!
     Assume floatBuffers has extent nChannels
     @post result: `result <= maxOut`
     */
    size_t MixSameRate(unsigned nChannels, size_t maxOut, float* floatBuffers[]);

    /*!
     Assume floatBuffers has extent nChannels
     @post result: `result <= maxOut`
     */
    size_t MixVariableRates(
        unsigned nChannels, size_t maxOut, float* floatBuffers[]);

    /*!
     @pre `produced <= max`
     */
    void ZeroFill(size_t produced, size_t max, float& floatBuffer);

    const std::shared_ptr<const WideSampleSequence> mpSeq;
    size_t i;

    const size_t mnChannels;
    const double mRate; // may require resampling

    //! Resampling, as needed, after gain envelope
    const BoundedEnvelope* const mEnvelope; // for time warp which also resamples
    const bool mMayThrow;

    const std::shared_ptr<TimesAndSpeed> mTimesAndSpeed;

    //! Fetch position for source
    /*!
     mSamplePos holds the next sample position not yet processed
     */
    sampleCount mSamplePos;

    //! First intermediate buffer when resampling is needed
    std::vector<std::vector<float> > mSampleQueue;

    //! Position of the start of the next block to resample
    int mQueueStart;

    //! The number of available samples after the queue start
    int mQueueLen;

    const ResampleParameters mResampleParameters;
    std::vector<std::unique_ptr<Resample> > mResample;

    //! Gain envelopes are applied to input before other transformations
    std::vector<double> mEnvValues;

    //! Remember how many channels were passed to Acquire()
    unsigned mMaxChannels{};
    size_t mLastProduced{};
};
#endif
