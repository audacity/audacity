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
#include "MixerSource.h"

#include <cmath>
#include "EffectStage.h"
#include "Dither.h"
#include "Resample.h"
#include "WideSampleSequence.h"
#include "float_cast.h"
#include <numeric>

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
} // namespace

Mixer::Mixer(
   Inputs inputs, std::optional<Stages> masterEffects, const bool mayThrow,
   const WarpOptions& warpOptions, const double startTime,
   const double stopTime, const unsigned numOutChannels,
   const size_t outBufferSize, const bool outInterleaved, double outRate,
   sampleFormat outFormat, const bool highQuality, MixerSpec* const mixerSpec,
   ApplyVolume applyVolume, std::function<bool()> pullFromTracks)
    : mNumChannels { numOutChannels }
    , mInputs { move(inputs) }
    , mMasterEffects { move(masterEffects) }
    , mBufferSize { FindBufferSize(mInputs, mMasterEffects, outBufferSize) }
    , mApplyVolume { applyVolume }
    , mHighQuality { highQuality }
    , mFormat { outFormat }
    , mInterleaved { outInterleaved }
    , mTimesAndSpeed { std::make_shared<TimesAndSpeed>(TimesAndSpeed {
         startTime, stopTime, warpOptions.initialSpeed, startTime }) }

    // PRL:  Bug2536: see other comments below for the last, padding argument
    // TODO: more-than-two-channels
    // Issue 3565 workaround:  allocate one extra buffer when applying a
    // GVerb effect stage.  It is simply discarded
    // See also issue 3854, when the number of out channels expected by the
    // plug-in is yet larger
    , mFloatBuffers { 3, mBufferSize, 1, 1 }

    // non-interleaved
    , mTemp { mNumChannels, mBufferSize, 1, 1 }
    , mBuffer { initVector<SampleBuffer>(
         mInterleaved ? 1 : mNumChannels,
         [format = mFormat,
          size = mBufferSize * (mInterleaved ? mNumChannels : 1)](
            auto& buffer) { buffer.Allocate(size, format); }) }
    , mEffectiveFormat { floatSample }
    , mPullFromTracks { move(pullFromTracks) }
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
         return std::any_of(input.stages.begin(), input.stages.end(),
            [](const MixerOptions::StageSpecification &spec){
               return spec.mpFirstInstance &&
                  spec.mpFirstInstance->NeedsDither(); } ); } );
   if (mMasterEffects)
      needsDither |= std::any_of(
         mMasterEffects->begin(), mMasterEffects->end(),
         [](const MixerOptions::StageSpecification& spec) {
            return spec.mpFirstInstance && spec.mpFirstInstance->NeedsDither();
         });

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
            return sum + input.stages.size() * input.pSequence->NChannels();
         }) +
      nMasterStages;
   mSettings.reserve(nStages);
   mStageBuffers.reserve(nStages);

   size_t i = 0;
   for (auto &input : mInputs) {
      const auto &sequence = input.pSequence;
      if (!sequence) {
         assert(false);
         break;
      }
      auto increment = finally([&]{ i += sequence->NChannels(); });

      auto& source = mSources.emplace_back(
         sequence, BufferSize(), outRate, warpOptions, highQuality, mayThrow,
         mTimesAndSpeed, (pMixerSpec ? &pMixerSpec->mMap[i] : nullptr),
         mPullFromTracks);
      AudioGraph::Source *pDownstream = &source;
      for (const auto &stage : input.stages)
         if (
            auto& pNewDownstream =
               RegisterEffectStage(*pDownstream, stage, outRate))
         {
            pDownstream = pNewDownstream.get();
         }
      mDecoratedSources.emplace_back(Source{ source, *pDownstream });
   }

   if (mMasterEffects)
   {
      AudioGraph::Source* pDownstream = this;
      for (const auto& stage : *mMasterEffects)
         if (
            auto& pNewDownstream =
               RegisterEffectStage(*pDownstream, stage, outRate))
         {
            mMasterStages.emplace_back(pDownstream = pNewDownstream.get());
         }
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
      else if (mApplyVolume == ApplyVolume::Mixdown &&
         !mHasMixerSpec &&
         sequence.NChannels() > 1 && mNumChannels == 1)
      {
         needsDither = true;
      }
      else if (mApplyVolume != ApplyVolume::Discard) {
         /// TODO: more-than-two-channels
         for (auto c : {0, 1}) {
            const auto volume = sequence.GetChannelVolume(c);
            if (!(volume == 0.0 || volume == 1.0))
               // Fractional volume may be applied even in MixSameRate
               needsDither = true;
         }
      }
      // Examine all tracks.  (This ignores the time bounds for the mixer.
      // If it did not, we might avoid dither in more cases.  But if we fix
      // that, remember that some mixers change their time bounds after
      // construction, as when scrubbing.)
      if (!sequence.HasTrivialEnvelope())
         // Varying or non-unit volume may be applied even in MixSameRate
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

static void MixBuffers(unsigned numChannels,
   const unsigned char *channelFlags, const float *volumes,
   const float &src, AudioGraph::Buffers &dests, int len)
{
   const auto pSrc = &src;
   for (unsigned int c = 0; c < numChannels; c++) {
      if (!channelFlags[c])
         continue;
      auto dest = &dests.GetWritePosition(c);
      for (int j = 0; j < len; ++j)
         dest[j] += pSrc[j] * volumes[c];   // the actual mixing process
   }
}

#define stackAllocate(T, count) static_cast<T*>(alloca(count * sizeof(T)))

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
   if (mMasterStages.empty())
      maxOut = Acquire(mTemp, maxToProcess);
   else
   {
      const auto stage = mMasterStages.back();
      maxOut = stage->Acquire(mTemp, maxToProcess);
      stage->Release();
   }
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

bool Mixer::AcceptsBuffers(const Buffers& buffers) const
{
   return buffers.Channels() == mNumChannels &&
          AcceptsBlockSize(buffers.BlockSize());
}

bool Mixer::AcceptsBlockSize(size_t blockSize) const
{
   return blockSize <= BufferSize();
}

sampleCount Mixer::Remaining() const
{
   return std::accumulate(
      mDecoratedSources.begin(), mDecoratedSources.end(), sampleCount { 0 },
      [](sampleCount sum, const Source& source) {
         return std::max(sum, source.downstream.Remaining());
      });
}

bool Mixer::Release()
{
   return true;
}

std::optional<size_t> Mixer::Acquire(Buffers& data, size_t maxToProcess)
{
   // TODO: more-than-two-channels
   auto maxChannels = std::max(2u, mFloatBuffers.Channels());
   const auto channelFlags = stackAllocate(unsigned char, mNumChannels);
   const auto volumes = stackAllocate(float, mNumChannels);
   if (mApplyVolume == ApplyVolume::Discard)
      std::fill(volumes, volumes + mNumChannels, 1.0f);

   // Decides which output buffers an input channel accumulates into
   auto findChannelFlags = [&channelFlags, numChannels = mNumChannels]
   (const bool *map, const WideSampleSequence &sequence, size_t iChannel){
      const auto end = channelFlags + numChannels;
      std::fill(channelFlags, end, 0);
      if (map)
         // ignore left and right when downmixing is customized
         std::copy(map, map + numChannels, channelFlags);
      else if (IsMono(sequence))
         std::fill(channelFlags, end, 1);
      else if (iChannel == 0)
         channelFlags[0] = 1;
      else if (iChannel == 1) {
         if (numChannels >= 2)
            channelFlags[1] = 1;
         else
            channelFlags[0] = 1;
      }
      return channelFlags;
   };

   size_t maxOut = 0;
   for (auto c = 0;c < data.Channels(); ++c)
      data.ClearBuffer(c, maxToProcess);

   for (auto& [upstream, downstream] : mDecoratedSources)
   {
      auto oResult = downstream.Acquire(mFloatBuffers, maxToProcess);
      // One of MixVariableRates or MixSameRate assigns into mTemp[*][*]
      // which are the sources for the CopySamples calls, and they copy into
      // mBuffer[*][*]
      if (!oResult)
         return 0;
      const auto result = *oResult;
      maxOut = std::max(maxOut, result);

      // Insert effect stages here!  Passing them all channels of the track

      const auto limit = std::min<size_t>(upstream.Channels(), maxChannels);
      for (size_t j = 0; j < limit; ++j)
      {
         const auto pFloat = (const float*)mFloatBuffers.GetReadPosition(j);
         auto& sequence = upstream.GetSequence();
         if (mApplyVolume != ApplyVolume::Discard)
         {
            for (size_t c = 0; c < mNumChannels; ++c)
            {
               if (mNumChannels > 1)
                  volumes[c] = sequence.GetChannelVolume(c);
               else
                  volumes[c] = sequence.GetChannelVolume(j);
            }
            if (
               mApplyVolume == ApplyVolume::Mixdown && !mHasMixerSpec &&
               mNumChannels == 1)
               volumes[0] /= static_cast<float>(limit);
         }

         const auto flags =
            findChannelFlags(upstream.MixerSpec(j), sequence, j);
         MixBuffers(mNumChannels, flags, volumes, *pFloat, data, result);
      }

      downstream.Release();
      mFloatBuffers.Advance(result);
      mFloatBuffers.Rotate();
   }

   // MB: this doesn't take warping into account, replaced with code based on mSamplePos
   //mT += (maxOut / mRate);

   assert(maxOut <= maxToProcess);
   return maxOut;
}

std::unique_ptr<EffectStage>& Mixer::RegisterEffectStage(
   AudioGraph::Source& upstream, const MixerOptions::StageSpecification& stage,
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
      -1, mNumChannels, upstream, stageInput, factory, settings, outRate,
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
