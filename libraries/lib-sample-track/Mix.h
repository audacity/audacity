/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.h

  Dominic Mazzoni
  Markus Meyer

***********************************************************************/

#ifndef __AUDACITY_MIX__
#define __AUDACITY_MIX__

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

class SAMPLE_TRACK_API MixerSpec
{
   unsigned mNumTracks, mNumChannels, mMaxNumChannels;

   void Alloc();

public:
   ArraysOf<bool> mMap;

   MixerSpec( unsigned numTracks, unsigned maxNumChannels );
   MixerSpec( const MixerSpec &mixerSpec );
   virtual ~MixerSpec();

   bool SetNumChannels( unsigned numChannels );
   unsigned GetNumChannels() { return mNumChannels; }

   unsigned GetMaxNumChannels() { return mMaxNumChannels; }
   unsigned GetNumTracks() { return mNumTracks; }

   MixerSpec& operator=( const MixerSpec &mixerSpec );
};

class SAMPLE_TRACK_API Mixer {
 public:

    // An argument to Mixer's constructor
    class SAMPLE_TRACK_API WarpOptions
    {
    public:
       //! Hook function for default time warp
       struct SAMPLE_TRACK_API DefaultWarp : GlobalHook<DefaultWarp,
          const BoundedEnvelope*(const TrackList&)
       >{};

       //! Construct using the default warp function
       explicit WarpOptions(const TrackList &list);

       //! Construct with an explicit warp
       explicit WarpOptions(const BoundedEnvelope *e);

       //! Construct with no time warp
       WarpOptions(double min, double max, double initial = 1.0);

    private:
       friend class Mixer;
       const BoundedEnvelope *envelope = nullptr;
       double minSpeed, maxSpeed;
       double initialSpeed{ 1.0 };
    };

   // Information derived from WarpOptions and other data
   struct ResampleParameters {
      ResampleParameters(
         const SampleTrackConstArray &inputTracks, double rate,
         const WarpOptions &options);
      bool             mbVariableRates{ false };
      std::vector<double> mMinFactor, mMaxFactor;
   };

    //
   // Constructor / Destructor
   //

   /*!
    @pre all `inputTracks` are non-null
    @post `BufferSize() == outBufferSize`
    */
   Mixer(const SampleTrackConstArray &inputTracks, bool mayThrow,
         const WarpOptions &warpOptions,
         double startTime, double stopTime,
         unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         bool highQuality = true, MixerSpec *mixerSpec = nullptr,
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
   /*!
    @post result: `result <= maxOut`
    */
   size_t MixSameRate(size_t maxOut,
      int *channelFlags, SampleTrackCache &cache, sampleCount *pos);

   /*!
    @post result: `result <= maxOut`
    */
   size_t MixVariableRates(size_t maxOut,
      int *channelFlags, SampleTrackCache &cache,
      sampleCount *pos, float *queue,
      int *queueStart, int *queueLen,
      Resample * pResample);

   void MakeResamplers();

 private:
   // Cut the queue into blocks of this finer size
   // for variable rate resampling.  Each block is resampled at some
   // constant rate.
   static constexpr size_t sProcessLen = 1024;

   // This is the number of samples grabbed in one go from a track
   // and placed in a queue, when mixing with resampling.
   // (Should we use SampleTrack::GetBestBlockSize instead?)
   static constexpr size_t sQueueMaxLen = 65536;

   // GIVEN PARAMETERS

   // Input
   const size_t     mNumInputTracks;
   const unsigned   mNumChannels;

   // Transformations
   const size_t     mBufferSize;
   // Resampling, as needed, after gain envelope
   const double     mRate; // may require resampling
   const BoundedEnvelope *const mEnvelope; // for time warp which also resamples
   // derived parameters
   const ResampleParameters mResampleParameters;

   // Output
   const bool       mApplyTrackGains;
   MixerSpec *const mMixerSpec; // many-to-one mixing of channels
   const bool       mHighQuality; // dithering
   const sampleFormat mFormat; // output format also influences dithering
   const bool       mInterleaved;

   // General
   const bool       mMayThrow;


   // INPUT

   // SampleTrackCaches are the source of data
   std::vector<SampleTrackCache> mInputTrack;

   // Fetch position for source
   // mSamplePos holds for each track the next sample position not
   // yet processed.
   std::vector<sampleCount> mSamplePos;
   // There is also a double-valued fetch position with (reassignable) bounds
   double           mT0; // Start time
   double           mT1; // Stop time (none if mT0==mT1)
   double           mTime;  // Current time (renamed from mT to mTime for consistency with AudioIO - mT represented warped time there)

   // BUFFERS

   // First intermediate buffer when resampling is needed
   std::vector<std::vector<float>> mSampleQueue;
   // Position in each queue of the start of the next block to resample
   std::vector<int>     mQueueStart;
   // For each queue, the number of available samples after the queue start
   std::vector<int>     mQueueLen;

   // Resample into this buffer, or produce directly when not resampling
   const size_t     mInterleavedBufferSize;
   std::vector<float>   mFloatBuffer;

   // Each channel's data is transformed, including application of
   // gains and pans, and then (maybe many-to-one) mixer specifications
   // determine where in mTemp it is accumulated
   const unsigned   mNumBuffers;
   std::vector<std::vector<float>> mTemp;

   // Final result applies dithering and interleaving
   const std::vector<SampleBuffer> mBuffer;

   // TRANSFORMATION STATE

   // Gain envelopes are applied to input before other transformations
   std::vector<double> mEnvValues;

   std::vector<std::unique_ptr<Resample>> mResample;
   // Varying scrub speed is one cause for resampling
   double           mSpeed;

   // TODO -- insert effect stages here

   // Gain slider settings are applied late
   std::vector<float> mGains;

   // Last step is accumulating results into the output buffers, with dithering
};

#endif

