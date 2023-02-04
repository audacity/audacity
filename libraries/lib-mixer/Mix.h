/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.h

  Dominic Mazzoni
  Markus Meyer

***********************************************************************/

#ifndef __AUDACITY_MIX__
#define __AUDACITY_MIX__

#include "AudioGraphBuffers.h"
#include "MixerOptions.h"
#include "SampleFormat.h"

class sampleCount;
class BoundedEnvelope;
class EffectStage;
namespace AudioGraph{ class Source; }
class MixerSource;
class TrackList;
class WideSampleSequence;

class MIXER_API Mixer {
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

   //
   // Constructor / Destructor
   //

   /*!
    @pre all sequences in `inputs` are non-null
    @pre any left channels in inputs are immediately followed by their
       partners
    @post `BufferSize() <= outBufferSize` (equality when no inputs have stages)
    @post `HasDoubleBuffers() == doubleBuffers`

    @param doubleBuffers whether to allocate a second set of output buffers
    and enable swapping
    */
   Mixer(Inputs inputs, bool mayThrow,
         const WarpOptions &warpOptions,
         double startTime, double stopTime,
         unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         bool doubleBuffers = false,
         bool highQuality = true,
         //! Null or else must have a lifetime enclosing this object's
         MixerSpec *mixerSpec = nullptr,
         bool applyTrackGains = true);

   Mixer(const Mixer&) = delete;
   Mixer &operator=(const Mixer&) = delete;

   virtual ~ Mixer();

   size_t BufferSize() const { return mBufferSize; }
   bool HasDoubleBuffers() const { return !mAltBuffer.empty(); }

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

   //! Swap the output buffer in use
   /*!
    @pre `HasDoubleBuffers()`
    */
   void SwapBuffers();

   //! Deduce the effective width of the output, which may be narrower than the stored format
   sampleFormat EffectiveFormat() const;

 private:

   void Clear();

 private:

   // Input
   const unsigned   mNumChannels;
   Inputs           mInputs;

   // Transformations
   const size_t     mBufferSize;

   std::pair<bool, sampleFormat>
   NeedsDither(bool needsDither, double rate) const;

 private:

   // Output
   const bool       mApplyTrackGains;
   const bool       mHighQuality; // dithering
   const sampleFormat mFormat; // output format also influences dithering
   const bool       mInterleaved;

   // INPUT
   sampleFormat     mEffectiveFormat;
   bool             mNeedsDither;

   const std::shared_ptr<TimesAndSpeed> mTimesAndSpeed;

   // BUFFERS

   // Resample into these buffers, or produce directly when not resampling
   AudioGraph::Buffers mFloatBuffers;

   // Each channel's data is transformed, including application of
   // gains and pans, and then (maybe many-to-one) mixer specifications
   // determine where in mTemp it is accumulated
   std::vector<std::vector<float>> mTemp;

   // Final result applies dithering and interleaving
   std::vector<SampleBuffer> mBuffer;

   // If not null, can swap with mBuffer
   std::vector<SampleBuffer> mAltBuffer;

   std::vector<MixerSource> mSources;
   std::vector<EffectSettings> mSettings;
   std::vector<AudioGraph::Buffers> mStageBuffers;
   std::vector<std::unique_ptr<EffectStage>> mStages;

   struct Source { MixerSource &upstream; AudioGraph::Source &downstream; };
   std::vector<Source> mDecoratedSources;
};
#endif
