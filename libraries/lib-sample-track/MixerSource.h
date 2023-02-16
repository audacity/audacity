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

class Resample;
class SampleTrackCache;

//! Fetches from tracks, applies envelopes; can resample, and warp time, even
//! backwards, as for scrubbing.
/*!
 This class inherits AudioGraph::Source but does not yet fulfill the contracts
 of all of the members.  But it is not yet used through any pointer or
 reference to its base class.
 */
class MixerSource final : public AudioGraph::Source {
public:
   using TimesAndSpeed = MixerOptions::TimesAndSpeed;
   using ResampleParameters = MixerOptions::ResampleParameters;

   /*!
    @pre `pTimesAndSpeed != nullptr`
    */
   MixerSource(const SampleTrack &leader, size_t bufferSize,
      double rate, const MixerOptions::Warp &options, bool highQuality,
      bool mayThrow, std::shared_ptr<TimesAndSpeed> pTimesAndSpeed,
      //! Null or else must have a lifetime enclosing this objects's
      const ArrayOf<bool> *pMap
   );
   MixerSource(MixerSource&&) = default;
   MixerSource &operator=(MixerSource&&) = delete;
   ~MixerSource();

   unsigned Channels() const { return mnChannels; }
   const SampleTrack *GetChannel(unsigned iChannel) const;
   const bool *MixerSpec(unsigned iChannel) const;

   bool AcceptsBuffers(const Buffers &buffers) const override;
   bool AcceptsBlockSize(size_t blockSize) const override;
   std::optional<size_t> Acquire(Buffers &data, size_t bound) override;
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
    @post result: `result <= maxOut`
    */
   size_t MixSameRate(unsigned iChannel, size_t maxOut, float &floatBuffer);

   /*!
    @post result: `result <= maxOut`
    */
   size_t MixVariableRates(
      unsigned iChannel, size_t maxOut, float &floatBuffer);

   /*!
    @pre `produced <= max`
    */
   void ZeroFill(size_t produced, size_t max, float &floatBuffer);

   const std::shared_ptr<const SampleTrack> mpLeader;
   size_t i;

   const size_t mnChannels;
   const double mRate; // may require resampling

   //! Resampling, as needed, after gain envelope
   const BoundedEnvelope *const mEnvelope; // for time warp which also resamples
   const bool mMayThrow;

   const std::shared_ptr<TimesAndSpeed> mTimesAndSpeed;

   //! SampleTrackCaches are the source of data
   std::vector<SampleTrackCache> mInputTrack;

   //! Fetch position for source
   /*!
    mSamplePos holds for each track the next sample position not yet processed.
    */
   std::vector<sampleCount> mSamplePos;

   //! First intermediate buffer when resampling is needed
   std::vector<std::vector<float>> mSampleQueue;

   //! Position in each queue of the start of the next block to resample
   std::vector<int>     mQueueStart;

   //! For each queue, the number of available samples after the queue start
   std::vector<int>     mQueueLen;

   const ResampleParameters mResampleParameters;
   std::vector<std::unique_ptr<Resample>> mResample;

   //! Gain envelopes are applied to input before other transformations
   std::vector<double> mEnvValues;

   //! many-to-one mixing of channels
   //! Pointer into array of arrays
   const ArrayOf<bool> *const mpMap;

   //! Remember how many channels were passed to Acquire()
   unsigned mMaxChannels{};
   size_t mLastProduced{};
};
#endif
