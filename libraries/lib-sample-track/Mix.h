/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.h

  Dominic Mazzoni
  Markus Meyer

***********************************************************************/

#ifndef __AUDACITY_MIX__
#define __AUDACITY_MIX__

#include "AudioGraphBuffers.h"
#include "AudioGraphSource.h"
#include "GlobalVariable.h"
#include "SampleFormat.h"
#include <functional>
#include <vector>

class sampleCount;
class Resample;
class BoundedEnvelope;
class TrackList;
class SampleTrack;
using SampleTrackConstArray = std::vector < std::shared_ptr < const SampleTrack > >;
class SampleTrackCache;

namespace MixerOptions {

//! A matrix of booleans, one row per input channel, column per output
class SAMPLE_TRACK_API Downmix final {
   unsigned mNumTracks, mNumChannels, mMaxNumChannels;

   void Alloc();

public:
   ArraysOf<bool> mMap;

   Downmix(unsigned numTracks, unsigned maxNumChannels);
   Downmix(const Downmix &mixerSpec);
   ~Downmix();

   bool SetNumChannels(unsigned numChannels);
   unsigned GetNumChannels() { return mNumChannels; }

   unsigned GetMaxNumChannels() { return mMaxNumChannels; }
   unsigned GetNumTracks() { return mNumTracks; }

   Downmix& operator=(const Downmix &mixerSpec);
};

//! Immutable structure is an argument to Mixer's constructor
struct SAMPLE_TRACK_API Warp final {
   //! Hook function for default time warp
   struct SAMPLE_TRACK_API DefaultWarp : GlobalHook<DefaultWarp,
    const BoundedEnvelope*(const TrackList&)
   >{};

   //! Construct using the default warp function
   explicit Warp(const TrackList &list);

   //! Construct with an explicit warp
   explicit Warp(const BoundedEnvelope *e);

   //! Construct with no time warp
   /*!
   @pre `min >= 0`
   @pre `max >= 0`
   @pre `min <= max`
   */
   Warp(double min, double max, double initial = 1.0);

   const BoundedEnvelope *const envelope = nullptr;
   const double minSpeed, maxSpeed;
   const double initialSpeed{ 1.0 };
};

// Information derived from Warp and other data
struct ResampleParameters final {
   ResampleParameters(bool highQuality,
      const SampleTrack &leader, double rate, const Warp &options);
   bool             mHighQuality{};
   bool             mVariableRates{ false };
   std::vector<double> mMinFactor, mMaxFactor;
};

//! Reassignable bounds and speed for a Mixer's fetch from tracks, and a
//! readout of last fetched time
struct TimesAndSpeed final {
   // Bounds for fetch position in the sample source
   double           mT0; // Start time
   double           mT1; // Stop time (none if mT0==mT1)
   // Varying scrub speed is one cause for resampling
   double           mSpeed;

   // For output purposes only (like progress indicator update)
   double           mTime;  // Current time (renamed from mT to mTime for
   // consistency with AudioIO - mT represented warped time there)
};

}

class MixerSource;

class SAMPLE_TRACK_API Mixer {
 public:
   using WarpOptions = MixerOptions::Warp;
   using MixerSpec = MixerOptions::Downmix;
   using ResampleParameters = MixerOptions::ResampleParameters;
   using TimesAndSpeed = MixerOptions::TimesAndSpeed;

   //
   // Constructor / Destructor
   //

   /*!
    @pre all `inputTracks` are non-null
    @pre any left channels in inputTracks are immediately followed by their
       partners
    @post `BufferSize() == outBufferSize`
    */
   Mixer(const SampleTrackConstArray &inputTracks, bool mayThrow,
         const WarpOptions &warpOptions,
         double startTime, double stopTime,
         unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         bool highQuality = true,
         //! Null or else must have a lifetime enclosing this object's
         MixerSpec *mixerSpec = nullptr,
         bool applytTrackGains = true);

   Mixer(const Mixer&) = delete;
   Mixer &operator=(const Mixer&) = delete;

   virtual ~ Mixer();

   size_t BufferSize() const { return mBufferSize; }

   //
   // Processing
   //

   /// Process a maximum of 'maxSamples' samples and put them into
   /// a buffer which can be retrieved by calling GetBuffer().
   /// Returns number of output samples, or 0, if there are no
   /// more samples that must be processed.
   /*!
    @pre `maxSamples <= BufferSize()`
    @post result: `result <= maxSamples`
    */
   size_t Process(size_t maxSamples);

   /// Restart processing at beginning of buffer next time
   /// Process() is called.
   void Restart();

   /// Reposition processing to absolute time next time
   /// Process() is called.
   void Reposition(double t, bool bSkipping = false);

   // Used in scrubbing and other nonuniform playback policies.
   void SetTimesAndSpeed(
      double t0, double t1, double speed, bool bSkipping = false);
   void SetSpeedForKeyboardScrubbing(double speed, double startTime);

   /// Current time in seconds (unwarped, i.e. always between startTime and stopTime)
   /// This value is not accurate, it's useful for progress bars and indicators, but nothing else.
   double MixGetCurrentTime();

   /// Retrieve the main buffer or the interleaved buffer
   constSamplePtr GetBuffer();

   /// Retrieve one of the non-interleaved buffers
   constSamplePtr GetBuffer(int channel);

 private:

   void Clear();

 private:

   // GIVEN PARAMETERS

   // Input
   const size_t     mNumInputTracks;
   const unsigned   mNumChannels;

   // Transformations
   const size_t     mBufferSize;
   // Resampling, as needed, after gain envelope
   const double     mRate; // may require resampling
   const BoundedEnvelope *const mEnvelope; // for time warp which also resamples

   // Output
   const bool       mApplyTrackGains;
   MixerSpec *const mMixerSpec; // many-to-one mixing of channels
   const bool       mHighQuality; // dithering
   const sampleFormat mFormat; // output format also influences dithering
   const bool       mInterleaved;

   // General
   const bool       mMayThrow;

   // INPUT

   const std::shared_ptr<TimesAndSpeed> mTimesAndSpeed;

   // BUFFERS

   // Resample into these buffers, or produce directly when not resampling
   AudioGraph::Buffers mFloatBuffers;

   // Each channel's data is transformed, including application of
   // gains and pans, and then (maybe many-to-one) mixer specifications
   // determine where in mTemp it is accumulated
   std::vector<std::vector<float>> mTemp;

   // Final result applies dithering and interleaving
   const std::vector<SampleBuffer> mBuffer;

   std::vector<MixerSource> mSources;
};

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
      double rate, const Mixer::WarpOptions &options, bool highQuality,
      const BoundedEnvelope *const pEnvelope, bool mayThrow

      , std::shared_ptr<TimesAndSpeed> pTimesAndSpeed

      //! Null or else must have a lifetime enclosing this objects's
      , const ArrayOf<bool> *pMap
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
   void Reposition(double time, bool skipping);

private:
   void MakeResamplers();

   // Cut the queue into blocks of this finer size
   // for variable rate resampling.  Each block is resampled at some
   // constant rate.
   static constexpr size_t sProcessLen = 1024;

   // This is the number of samples grabbed in one go from a track
   // and placed in a queue, when mixing with resampling.
   // (Should we use SampleTrack::GetBestBlockSize instead?)
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
   const double mRate;
   const BoundedEnvelope *const mEnvelope;
   const bool mMayThrow;

   const std::shared_ptr<TimesAndSpeed> mTimesAndSpeed;

   // SampleTrackCaches are the source of data
   std::vector<SampleTrackCache> mInputTrack;

   // Fetch position for source
   // mSamplePos holds for each track the next sample position not
   // yet processed.
   std::vector<sampleCount> mSamplePos;

   // First intermediate buffer when resampling is needed
   std::vector<std::vector<float>> mSampleQueue;

   // Position in each queue of the start of the next block to resample
   std::vector<int>     mQueueStart;

   // For each queue, the number of available samples after the queue start
   std::vector<int>     mQueueLen;

   const ResampleParameters mResampleParameters;
   std::vector<std::unique_ptr<Resample>> mResample;

   // Gain envelopes are applied to input before other transformations
   std::vector<double> mEnvValues;

   // Pointer into array of arrays
   const ArrayOf<bool> *const mpMap;
};

#endif

