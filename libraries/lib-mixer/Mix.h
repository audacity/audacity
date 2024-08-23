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
#include "MixerOptions.h"
#include "SampleFormat.h"
#include <optional>

class sampleCount;
class BoundedEnvelope;
class EffectStage;
class MixerSource;
class TrackList;
class WideSampleSequence;

class MIXER_API Mixer : public AudioGraph::Source
{
public:
   using WarpOptions = MixerOptions::Warp;
   using MixerSpec = MixerOptions::Downmix;
   using ResampleParameters = MixerOptions::ResampleParameters;
   using TimesAndSpeed = MixerOptions::TimesAndSpeed;
   using Stages = std::vector<MixerOptions::StageSpecification>;

   struct Input {
      Input(
         std::shared_ptr<const WideSampleSequence> pSequence = {},
         Stages stages = {}
      )  : pSequence{ move(pSequence) }, stages{ move(stages) }
      {}

      std::shared_ptr<const WideSampleSequence> pSequence;
      Stages stages;
   };
   using Inputs = std::vector<Input>;

   enum class ApplyVolume   {
      Discard,//< No volume is applied on the sources' (including effect stages) output
      MapChannels, //< Apply volume per source's channel
      Mixdown, //< Average volume from all channels in the source, numOutChannels should be 1
   };

   //
   // Constructor / Destructor
   //

   /*!
    @pre all sequences in `inputs` are non-null
    @pre any left channels in inputs are immediately followed by their
       partners
    @post `BufferSize() <= outBufferSize` (equality when no inputs have stages)
    */
   Mixer(
      Inputs inputs, std::optional<Stages> masterEffects, bool mayThrow,
      const WarpOptions& warpOptions, double startTime, double stopTime,
      unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
      double outRate, sampleFormat outFormat, bool highQuality = true,
      //! Null or else must have a lifetime enclosing this object's
      MixerSpec* mixerSpec = nullptr,
      ApplyVolume applyVolume = ApplyVolume::MapChannels,
      std::function<bool()> pullFromTracks = {});

   Mixer(const Mixer&) = delete;
   Mixer &operator=(const Mixer&) = delete;

   virtual ~ Mixer();

   size_t BufferSize() const { return mBufferSize; }

   //
   // Processing
   //

   //! Process a maximum of 'maxSamples' samples and put them into the buffer,
   //! at GetBuffer().
   /*!
    @pre `maxSamples <= BufferSize()`
    @post result: `result <= maxSamples`
    @return number of output samples, or 0, if there are no more samples that
    must be processed.
    */
   size_t Process(size_t maxSamples);

   /*!
    @post result: `result <= BufferSize()`
    */
   size_t Process() { return Process(BufferSize()); }

   //! Reposition processing to absolute time next time Process() is called.
   void Reposition(double t, bool bSkipping = false);

   //! Used in scrubbing and other nonuniform playback policies.
   void SetTimesAndSpeed(
      double t0, double t1, double speed, bool bSkipping = false);
   void SetSpeedForKeyboardScrubbing(double speed, double startTime);

   //! Current time in seconds (unwarped, i.e. always between startTime and stopTime)
   /*! This value is not accurate, it's useful for progress bars and indicators, but nothing else. */
   double MixGetCurrentTime();

   //! Retrieve the main buffer or the interleaved buffer
   constSamplePtr GetBuffer();

   //! Retrieve one of the non-interleaved buffers
   constSamplePtr GetBuffer(int channel);

   //! Deduce the effective width of the output, which may be narrower than the stored format
   sampleFormat EffectiveFormat() const;

 private:

   void Clear();

   // AudioGraph::Source
private:
   bool AcceptsBuffers(const Buffers& buffers) const override;
   bool AcceptsBlockSize(size_t blockSize) const override;
   std::optional<size_t> Acquire(Buffers& data, size_t bound) override;
   sampleCount Remaining() const override;
   bool Release() override;

private:
   std::unique_ptr<EffectStage>& RegisterEffectStage(
      AudioGraph::Source& upstream,
      const MixerOptions::StageSpecification& stage, double outRate);

   // Input
   const unsigned   mNumChannels;
   Inputs           mInputs;
   const std::optional<Stages> mMasterEffects;

   // Transformations
   const size_t     mBufferSize;

   std::pair<bool, sampleFormat>
   NeedsDither(bool needsDither, double rate) const;

 private:

   // Output
   const ApplyVolume mApplyVolume;
   const bool       mHighQuality; // dithering
   const sampleFormat mFormat; // output format also influences dithering
   const bool       mInterleaved;

   // INPUT
   sampleFormat     mEffectiveFormat;
   bool             mNeedsDither;
   bool             mHasMixerSpec{false};

   const std::shared_ptr<TimesAndSpeed> mTimesAndSpeed;

   // BUFFERS

   // Resample into these buffers, or produce directly when not resampling
   AudioGraph::Buffers mFloatBuffers;

   // Each channel's data is transformed, including application of
   // gains and pans, and then (maybe many-to-one) mixer specifications
   // determine where in mTemp it is accumulated
   AudioGraph::Buffers mTemp;

   // Final result applies dithering and interleaving
   const std::vector<SampleBuffer> mBuffer;

   std::vector<MixerSource> mSources;
   std::vector<EffectSettings> mSettings;
   std::vector<AudioGraph::Buffers> mStageBuffers;
   std::vector<std::unique_ptr<EffectStage>> mStages;
   std::vector<AudioGraph::Source*> mMasterStages;

   struct Source { MixerSource &upstream; AudioGraph::Source &downstream; };
   std::vector<Source> mDecoratedSources;
   const std::function<bool()> mPullFromTracks;
};
#endif
