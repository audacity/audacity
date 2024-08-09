/**********************************************************************

  Audacity: A Digital Audio Editor

  Mix.cpp

  Dominic Mazzoni
  Markus Meyer
  Vaughan Johnson

*******************************************************************//**

\class Mixer
\brief Functions for doing the mixdown of the tracks.

*//*******************************************************************/
#include "Mix.h"
#include "Mix.h"
#include "MixerSource.h"

#include <cmath>
#include "EffectStage.h"
#include "Dither.h"
#include "Resample.h"
#include "WideSampleSequence.h"
#include "float_cast.h"
#include <numeric>

#define stackAllocate(T, count) static_cast<T*>(alloca(count * sizeof(T)))

namespace {
template<typename T, typename F> std::vector<T>
initVector(size_t dim1, const F &f)
{
   std::vector<T> result( dim1 );
   for (auto &row : result)
      f(row);
   return result;
}

template<typename T> std::vector<std::vector<T>>
initVector(size_t dim1, size_t dim2)
{
   return initVector<std::vector<T>>(dim1,
      [dim2](auto &row){ row.resize(dim2); });
}
}

namespace
{

void MixBuffers(unsigned numChannels,
   const unsigned char *channelFlags, const float *gains,
   const float &src, AudioGraph::Buffers &dests, int len)
{
   const auto pSrc = &src;
   for (unsigned int c = 0; c < numChannels; c++) {
      if (!channelFlags[c])
         continue;
      auto dest = &dests.GetWritePosition(c);
      for (int j = 0; j < len; ++j)
         dest[j] += pSrc[j] * gains[c];   // the actual mixing process
   }
}

void ConsiderStages(const Mixer::Stages& stages, size_t& blockSize)
{
   for (const auto& stage : stages)
   {
      // Need an instance to query acceptable block size
      const auto pInstance = stage.factory();
      if (pInstance)
         blockSize = std::min(blockSize, pInstance->SetBlockSize(blockSize));
      // Cache the first factory call
      stage.mpFirstInstance = move(pInstance);
   }
}

// Find a block size acceptable to all stages; side-effects on instances
size_t FindBufferSize(
   const Mixer::Inputs& inputs,
   const std::optional<Mixer::Stages>& masterEffects, size_t bufferSize)
{
   size_t blockSize = bufferSize;
   for (const auto &input : inputs) {
      const auto sequence = input.pSequence.get();
      const auto nInChannels = sequence->NChannels();
      if (!sequence) {
         assert(false);
         break;
      }
      ConsiderStages(input.stages, blockSize);
   }
   if (masterEffects)
      ConsiderStages(*masterEffects, blockSize);

   return blockSize;
}

//! Describes an input source for DownmixStage
class DownmixSource
{
public:
   DownmixSource() = default;

   virtual ~DownmixSource() = default;
   //! Returns underlying `AudioGraph::Source` processed by Mixer
   virtual AudioGraph::Source& GetDownstream() const = 0;
   //! Number of output channels of the underlying Source
   virtual size_t NChannels() const = 0;
   //! Gain multiplier that should be applied to the channel
   virtual float GetChannelGain(size_t channel) const = 0;
   //! For the given `iChannel` fills the channelFlags array, that describes
   //! to which output it should go.
   virtual void FindChannelFlags(unsigned char *channelFlags, size_t numChannels, size_t iChannel) = 0;
   //! Returns true if source channels could be combined into mono if needed
   virtual bool CanMakeMono() const = 0;
};

class SequenceDownmixSource final : public DownmixSource
{
   AudioGraph::Source& mDownstream;
   const WideSampleSequence& mSequence;
   //! many-to-one mixing of channels
   //! Pointer into array of arrays
   const ArrayOf<bool> * mpMap {};
public:

   SequenceDownmixSource(AudioGraph::Source& downstream,
                         const WideSampleSequence& sequence,
                         //! Null or else must have a lifetime enclosing this objects's
                         const ArrayOf<bool> *channelMap)
      : mDownstream(downstream)
      , mSequence(sequence)
      , mpMap(channelMap)
   {
   }

   AudioGraph::Source& GetDownstream() const override { return mDownstream; }
   size_t NChannels() const override { return mSequence.NChannels(); }
   float GetChannelGain(size_t channel) const override { return mSequence.GetChannelGain(channel); }
   void FindChannelFlags(unsigned char *channelFlags, size_t numChannels, size_t iChannel) override
   {
      const bool* map = mpMap ? mpMap[iChannel].get() : nullptr;
      const auto end = channelFlags + numChannels;
      std::fill(channelFlags, end, 0);
      if (map)
         // ignore left and right when downmixing is customized
         std::copy(map, map + numChannels, channelFlags);
      else if (AudioGraph::IsMono(mSequence))
         std::fill(channelFlags, end, 1);
      else if (iChannel == 0)
         channelFlags[0] = 1;
      else if (iChannel == 1) {
         if (numChannels >= 2)
            channelFlags[1] = 1;
         else
            channelFlags[0] = 1;
      }
   }
   bool CanMakeMono() const override { return mpMap == nullptr; }
};

class StereoToMono final : public DownmixSource
{
   AudioGraph::Source& mMasterSource;
public:

   StereoToMono(AudioGraph::Source& masterSource)
      : mMasterSource(masterSource)
   { }

   AudioGraph::Source& GetDownstream() const override { return mMasterSource; }
   size_t NChannels() const override { return 2; }
   float GetChannelGain(size_t) const override { return 1.f; }
   void FindChannelFlags(unsigned char* channelFlags, size_t numChannels, size_t) override
   {
      for(size_t i = 0; i < numChannels; ++i)
         channelFlags[i] = true;
   }
   bool CanMakeMono() const override { return true; }
};

//! Combines multiple audio graph sources into a single source
class DownmixStage final : public AudioGraph::Source
{
   std::vector<std::unique_ptr<DownmixSource>> mDownmixSources;
   // Resample into these buffers, or produce directly when not resampling
   AudioGraph::Buffers mFloatBuffers;
   size_t mNumChannels;
   Mixer::ApplyGain mApplyGain;

public:

   DownmixStage(std::vector<std::unique_ptr<DownmixSource>> downmixSources,
                size_t numChannels,
                size_t bufferSize,
                Mixer::ApplyGain applyGain)
      : mDownmixSources(std::move(downmixSources))
    // PRL:  Bug2536: see other comments below for the last, padding argument
    // TODO: more-than-two-channels
    // Issue 3565 workaround:  allocate one extra buffer when applying a
    // GVerb effect stage.  It is simply discarded
    // See also issue 3854, when the number of out channels expected by the
    // plug-in is yet larger
      , mFloatBuffers { 3, bufferSize, 1, 1 }
      , mNumChannels(numChannels)
      , mApplyGain(applyGain)
   {
      
   }

   ~DownmixStage() override = default;

   bool AcceptsBuffers(const Buffers& buffers) const override
   {
      return buffers.Channels() == mNumChannels &&
         AcceptsBlockSize(buffers.BlockSize());  
   }

   bool AcceptsBlockSize(size_t blockSize) const override
   {
      return blockSize <= mFloatBuffers.BufferSize();
   }

   std::optional<size_t> Acquire(Buffers& data, size_t maxToProcess) override
   {
      // TODO: more-than-two-channels
      auto maxChannels = std::max(2u, mFloatBuffers.Channels());
      const auto channelFlags = stackAllocate(unsigned char, mNumChannels);
      const auto gains = stackAllocate(float, mNumChannels);
      if (mApplyGain == Mixer::ApplyGain::Discard)
         std::fill(gains, gains + mNumChannels, 1.0f);

      size_t maxOut = 0;
      for (auto c = 0;c < data.Channels(); ++c)
         data.ClearBuffer(c, maxToProcess);

      for (auto& downmixSource : mDownmixSources)
      {
         auto oResult = downmixSource->GetDownstream().Acquire(mFloatBuffers, maxToProcess);
         // One of MixVariableRates or MixSameRate assigns into mTemp[*][*]
         // which are the sources for the CopySamples calls, and they copy into
         // mBuffer[*][*]
         if (!oResult)
            return 0;
         const auto result = *oResult;
         maxOut = std::max(maxOut, result);

         // Insert effect stages here!  Passing them all channels of the track

         const auto limit = std::min<size_t>(downmixSource->NChannels(), maxChannels);
         for (size_t j = 0; j < limit; ++j)
         {
            const auto pFloat = (const float*)mFloatBuffers.GetReadPosition(j);
            if (mApplyGain != Mixer::ApplyGain::Discard)
            {
               for (size_t c = 0; c < mNumChannels; ++c)
               {
                  if (mNumChannels > 1)
                     gains[c] = downmixSource->GetChannelGain(c);
                  else
                     gains[c] = downmixSource->GetChannelGain(j);
               }
               if (mApplyGain == Mixer::ApplyGain::Mixdown &&
                   mNumChannels == 1 &&
                   downmixSource->CanMakeMono())
               {
                  gains[0] /= static_cast<float>(limit);
               }
            }

            downmixSource->FindChannelFlags(channelFlags, mNumChannels, j);
            MixBuffers(mNumChannels, channelFlags, gains, *pFloat, data, result);
         }

         downmixSource->GetDownstream().Release();
         mFloatBuffers.Advance(result);
         mFloatBuffers.Rotate();
      }

      // MB: this doesn't take warping into account, replaced with code based on mSamplePos
      //mT += (maxOut / mRate);

      assert(maxOut <= maxToProcess);
      return maxOut;
   }
   sampleCount Remaining() const override
   {
      return std::accumulate(
         mDownmixSources.begin(), mDownmixSources.end(), sampleCount { 0 },
         [](sampleCount sum, const auto& source) {
            return std::max(sum, source->GetDownstream().Remaining());
         });
   }

   bool Release() override
   {
      return true;
   }
};

auto NeedsDitherPred(const MixerOptions::StageSpecification &spec) {
   return spec.mpFirstInstance && spec.mpFirstInstance->NeedsDither();
};

} // namespace

Mixer::Mixer(
   Inputs inputs, std::optional<Stages> masterEffects, const bool mayThrow,
   const WarpOptions& warpOptions, const double startTime,
   const double stopTime, const unsigned numOutChannels,
   const size_t outBufferSize, const bool outInterleaved, double outRate,
   sampleFormat outFormat, const bool highQuality, MixerSpec* const mixerSpec,
   ApplyGain applyGain)
    : mNumChannels { numOutChannels }
    , mInputs { move(inputs) }
    , mMasterEffects { move(masterEffects) }
    , mBufferSize { FindBufferSize(mInputs, mMasterEffects, outBufferSize) }
    , mApplyGain { applyGain }
    , mHighQuality { highQuality }
    , mFormat { outFormat }
    , mInterleaved { outInterleaved }
    , mTimesAndSpeed { std::make_shared<TimesAndSpeed>(TimesAndSpeed {
         startTime, stopTime, warpOptions.initialSpeed, startTime }) }

    // non-interleaved
    , mTemp { mNumChannels, mBufferSize, 1, 1 }
    , mBuffer { initVector<SampleBuffer>(
         mInterleaved ? 1 : mNumChannels,
         [format = mFormat,
          size = mBufferSize * (mInterleaved ? mNumChannels : 1)](
            auto& buffer) { buffer.Allocate(size, format); }) }
    , mEffectiveFormat { floatSample }
{
   assert(BufferSize() <= outBufferSize);
   const auto nChannelsIn =
   std::accumulate(mInputs.begin(), mInputs.end(), size_t{},
      [](auto sum, const auto &input){
         return sum + input.pSequence->NChannels(); });

   // Examine the temporary instances that were made in FindBufferSize
   // This finds a sufficient, but not necessary, condition to do dithering
   bool needsDither = std::any_of(mInputs.begin(), mInputs.end(),
      [](const Input &input){
         return std::any_of(input.stages.begin(), input.stages.end(), NeedsDitherPred); }
   );
   if (mMasterEffects)
      needsDither |= std::any_of(
         mMasterEffects->begin(), mMasterEffects->end(), NeedsDitherPred);

   auto pMixerSpec = ( mixerSpec &&
      mixerSpec->GetNumChannels() == mNumChannels &&
      mixerSpec->GetNumTracks() == nChannelsIn
   ) ? mixerSpec : nullptr;
   mHasMixerSpec = pMixerSpec != nullptr;

   // Reserve vectors first so we can take safe references to pushed elements
   mSources.reserve(nChannelsIn);
   // One stereo-capable stage per effect.
   const auto nMasterStages = mMasterEffects ? mMasterEffects->size() : 0;
   const auto nStages =
      std::accumulate(
         mInputs.begin(), mInputs.end(), 0,
         [](auto sum, const auto& input) {
            return sum + input.stages.size();
         }) +
      nMasterStages;
   mSettings.reserve(nStages);
   mStageBuffers.reserve(nStages);

   size_t i = 0;
   std::vector<std::unique_ptr<DownmixSource>> mixdownSources;
   for (auto &input : mInputs) {
      const auto &sequence = input.pSequence;
      if (!sequence) {
         assert(false);
         break;
      }

      auto &source = mSources.emplace_back(sequence, BufferSize(), outRate,
         warpOptions, highQuality, mayThrow, mTimesAndSpeed);
      AudioGraph::Source *pDownstream = &source;
      for (const auto &stage : input.stages)
         if (
            auto& pNewDownstream =
               RegisterEffectStage(*pDownstream, sequence->NChannels(), stage, outRate))
         {
            pDownstream = pNewDownstream.get();
         }
      mixdownSources.emplace_back(
         std::make_unique<SequenceDownmixSource>(
            *pDownstream,
            *sequence,
            pMixerSpec ? &pMixerSpec->mMap[i] : nullptr
         )
      );

      i += sequence->NChannels();
   }
   
   if (mMasterEffects && !mMasterEffects->empty())
   {
      mMixDownStage = std::make_unique<DownmixStage>(
         std::move(mixdownSources), 2, mBufferSize, ApplyGain::MapChannels
      );

      AudioGraph::Source* pDownstream = mMixDownStage.get();
      for (const auto& stage : *mMasterEffects)
         if (
            auto& pNewDownstream =
               RegisterEffectStage(*pDownstream, 2, stage, outRate))
         {
            pDownstream = pNewDownstream.get();
         }
      if(mNumChannels == 1)
      {
         std::vector<std::unique_ptr<DownmixSource>> masterMixdownSources;
         masterMixdownSources.push_back(std::make_unique<StereoToMono>(*pDownstream));
         mMasterMixStage = std::make_unique<DownmixStage>(
            std::move(masterMixdownSources), 1u, mBufferSize, ApplyGain::Mixdown);
         mDownstream = mMasterMixStage.get();
      }
      else
         mDownstream = pDownstream;
   }
   else
   {
      mMixDownStage = std::make_unique<DownmixStage>(
         std::move(mixdownSources),
         mNumChannels,
         mBufferSize,
         mApplyGain
      );
      mDownstream = mMixDownStage.get();
   }

   // Decide once at construction time
   std::tie(mNeedsDither, mEffectiveFormat) = NeedsDither(needsDither, outRate);
}

Mixer::~Mixer() = default;

std::pair<bool, sampleFormat>
Mixer::NeedsDither(bool needsDither, double rate) const
{
   // This will accumulate the widest effective format of any input
   // clip
   auto widestEffectiveFormat = narrowestSampleFormat;

   // needsDither may already be given as true.
   // There are many other possible disqualifiers for the avoidance of dither.
   if (std::any_of(mSources.begin(), mSources.end(),
      std::mem_fn(&MixerSource::VariableRates))
   )
      // We will call MixVariableRates(), so we need nontrivial resampling
      needsDither = true;

   for (const auto &input : mSources) {
      auto &sequence = input.GetSequence();

      if (sequence.GetRate() != rate)
         // Also leads to MixVariableRates(), needs nontrivial resampling
         needsDither = true;
      else if (mApplyGain == ApplyGain::Mixdown &&
         !mHasMixerSpec &&
         sequence.NChannels() > 1 && mNumChannels == 1)
      {
         needsDither = true;
      }
      else if (mApplyGain != ApplyGain::Discard) {
         /// TODO: more-than-two-channels
         for (auto c : {0, 1}) {
            const auto gain = sequence.GetChannelGain(c);
            if (!(gain == 0.0 || gain == 1.0))
               // Fractional gain may be applied even in MixSameRate
               needsDither = true;
         }
      }
      // Examine all tracks.  (This ignores the time bounds for the mixer.
      // If it did not, we might avoid dither in more cases.  But if we fix
      // that, remember that some mixers change their time bounds after
      // construction, as when scrubbing.)
      if (!sequence.HasTrivialEnvelope())
         // Varying or non-unit gain may be applied even in MixSameRate
         needsDither = true;
      auto effectiveFormat = sequence.WidestEffectiveFormat();
      if (effectiveFormat > mFormat)
         // Real, not just nominal, precision loss would happen in at
         // least one clip
         needsDither = true;
      widestEffectiveFormat =
         std::max(widestEffectiveFormat, effectiveFormat);
   }

   if (needsDither)
      // Results will be dithered to width mFormat
      return { true, mFormat };
   else {
      // Results will not be dithered
      assert(widestEffectiveFormat <= mFormat);
      return { false, widestEffectiveFormat };
   }
}

void Mixer::Clear()
{
   for (auto c = 0; c < mTemp.Channels(); ++c)
      mTemp.ClearBuffer(c, mBufferSize);
}

size_t Mixer::Process(const size_t maxToProcess)
{
   assert(maxToProcess <= BufferSize());

   // MB: this is wrong! mT represented warped time, and mTime is too inaccurate to use
   // it here. It's also unnecessary I think.
   //if (mT >= mT1)
   //   return 0;

   auto &[mT0, mT1, _, mTime] = *mTimesAndSpeed;
   auto oldTime = mTime;
   // backwards (as possibly in scrubbing)
   const auto backwards = (mT0 > mT1);

   Clear();

   std::optional<size_t> maxOut;
   
   maxOut = mDownstream->Acquire(mTemp, maxToProcess);
   mDownstream->Release();

   if (!maxOut)
      return 0;

   if (backwards)
      mTime = std::clamp(mTime, mT1, oldTime);
   else
      mTime = std::clamp(mTime, oldTime, mT1);

   const auto dstStride = (mInterleaved ? mNumChannels : 1);
   auto ditherType = mNeedsDither
      ? (mHighQuality ? gHighQualityDither : gLowQualityDither)
      : DitherType::none;
   for (size_t c = 0; c < mNumChannels; ++c)
      CopySamples(mTemp.GetReadPosition(c), floatSample,
         (mInterleaved
            ? mBuffer[0].ptr() + (c * SAMPLE_SIZE(mFormat))
            : mBuffer[c].ptr()
         ),
         mFormat, *maxOut, ditherType,
         1, dstStride);

   // MB: this doesn't take warping into account, replaced with code based on mSamplePos
   //mT += (maxOut / mRate);

   assert(*maxOut <= maxToProcess);
   return *maxOut;
}

constSamplePtr Mixer::GetBuffer()
{
   return mBuffer[0].ptr();
}

constSamplePtr Mixer::GetBuffer(int channel)
{
   return mBuffer[channel].ptr();
}

sampleFormat Mixer::EffectiveFormat() const
{
   return mEffectiveFormat;
}

double Mixer::MixGetCurrentTime()
{
   return mTimesAndSpeed->mTime;
}

void Mixer::Reposition(double t, bool bSkipping)
{
   const auto &[mT0, mT1, _, __] = *mTimesAndSpeed;
   auto &mTime = mTimesAndSpeed->mTime;
   mTime = t;
   const bool backwards = (mT1 < mT0);
   if (backwards)
      mTime = std::clamp(mTime, mT1, mT0);
   else
      mTime = std::clamp(mTime, mT0, mT1);

   for (auto &source : mSources)
      source.Reposition(mTime, bSkipping);
}

void Mixer::SetTimesAndSpeed(double t0, double t1, double speed, bool bSkipping)
{
   wxASSERT(std::isfinite(speed));
   auto &[mT0, mT1, mSpeed, _] = *mTimesAndSpeed;
   mT0 = t0;
   mT1 = t1;
   mSpeed = fabs(speed);
   Reposition(t0, bSkipping);
}

void Mixer::SetSpeedForKeyboardScrubbing(double speed, double startTime)
{
   wxASSERT(std::isfinite(speed));
   auto &[mT0, mT1, mSpeed, _] = *mTimesAndSpeed;

   // Check if the direction has changed
   if ((speed > 0.0 && mT1 < mT0) || (speed < 0.0 && mT1 > mT0)) {
      // It's safe to use 0 and std::numeric_limits<double>::max(),
      // because Mixer::MixVariableRates() doesn't sample past the start
      // or end of the audio in a track.
      if (speed > 0.0 && mT1 < mT0) {
         mT0 = 0;
         mT1 = std::numeric_limits<double>::max();
      }
      else {
         mT0 = std::numeric_limits<double>::max();
         mT1 = 0;
      }

      Reposition(startTime, true);
   }

   mSpeed = fabs(speed);
}

std::unique_ptr<EffectStage>& Mixer::RegisterEffectStage(
   AudioGraph::Source& upstream, size_t numChannels,
   const MixerOptions::StageSpecification& stage,
   double outRate)
{
   // Make a mutable copy of stage.settings
   auto& settings = mSettings.emplace_back(stage.settings);
   // TODO: more-than-two-channels
   // Like mFloatBuffers but padding not needed for soxr
   // Allocate one extra buffer to hold dummy zero inputs
   // (Issue 3854)
   auto& stageInput = mStageBuffers.emplace_back(3, mBufferSize, 1);
   const auto& factory = [&stage] {
      // Avoid unnecessary repeated calls to the factory
      return stage.mpFirstInstance ? move(stage.mpFirstInstance) :
                                     stage.factory();
   };
   auto& pNewDownstream = mStages.emplace_back(EffectStage::Create(
      -1, numChannels, upstream, stageInput, factory, settings, outRate,
      std::nullopt));
   if (!pNewDownstream)
   {
      // Just omit the failed stage from rendering
      // TODO propagate the error?
      mStageBuffers.pop_back();
      mSettings.pop_back();
   }
   return pNewDownstream;
}
