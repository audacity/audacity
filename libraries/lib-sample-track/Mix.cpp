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
#include "SampleTrack.h"
#include "SampleTrackCache.h"
#include "Resample.h"
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

namespace {
// Find a block size acceptable to all stages; side-effects on instances
size_t FindBufferSize(const Mixer::Inputs &inputs, size_t bufferSize)
{
   size_t blockSize = bufferSize;
   const auto nTracks = inputs.size();
   for (size_t i = 0; i < nTracks;) {
      const auto &input = inputs[i];
      const auto leader = input.pTrack.get();
      const auto nInChannels = TrackList::Channels(leader).size();
      if (!leader || i + nInChannels > nTracks) {
         assert(false);
         break;
      }
      auto increment = finally([&]{ i += nInChannels; });
      for (const auto &stage : input.stages) {
         // Need an instance to query acceptable block size
         const auto pInstance = stage.factory();
         if (pInstance)
            blockSize = std::min(blockSize, pInstance->SetBlockSize(blockSize));
         // Cache the first factory call
         stage.mpFirstInstance = move(pInstance);
      }
   }
   return blockSize;
}
}

Mixer::Mixer(Inputs inputs,
   const bool mayThrow,
   const WarpOptions &warpOptions,
   const double startTime, const double stopTime,
   const unsigned numOutChannels,
   const size_t outBufferSize, const bool outInterleaved,
   double outRate, sampleFormat outFormat,
   const bool highQuality, MixerSpec *const mixerSpec,
   const bool applyTrackGains
)  : mNumChannels{ numOutChannels }
   , mInputs{ move(inputs) }
   , mBufferSize{ FindBufferSize(mInputs, outBufferSize) }
   , mApplyTrackGains{ applyTrackGains }
   , mHighQuality{ highQuality }
   , mFormat{ outFormat }
   , mInterleaved{ outInterleaved }

   , mTimesAndSpeed{ std::make_shared<TimesAndSpeed>( TimesAndSpeed{
      startTime, stopTime, warpOptions.initialSpeed, startTime
   } ) }

   // PRL:  Bug2536: see other comments below for the last, padding argument
   // TODO: more-than-two-channels
   // Issue 3565 workaround:  allocate one extra buffer when applying a
   // GVerb effect stage.  It is simply discarded
   // See also issue 3854, when the number of out channels expected by the
   // plug-in is yet larger
   , mFloatBuffers{ 3, mBufferSize, 1, 1 }

   // non-interleaved
   , mTemp{ initVector<float>(mNumChannels, mBufferSize) }
   , mBuffer{ initVector<SampleBuffer>(mInterleaved ? 1 : mNumChannels,
      [format = mFormat,
         size = mBufferSize * (mInterleaved ? mNumChannels : 1)
      ](auto &buffer){ buffer.Allocate(size, format); }
   )}
   , mEffectiveFormat{ floatSample }
{
   assert(BufferSize() <= outBufferSize);
   const auto nTracks =  mInputs.size();

   // Examine the temporary instances that were made in FindBufferSize
   // This finds a sufficient, but not necessary, condition to do dithering
   bool needsDither = std::any_of(mInputs.begin(), mInputs.end(),
      [](const Input &input){
         return std::any_of(input.stages.begin(), input.stages.end(),
            [](const MixerOptions::StageSpecification &spec){
               return spec.mpFirstInstance &&
                  spec.mpFirstInstance->NeedsDither(); } ); } );

   auto pMixerSpec = ( mixerSpec &&
      mixerSpec->GetNumChannels() == mNumChannels &&
      mixerSpec->GetNumTracks() == nTracks
   ) ? mixerSpec : nullptr;

   // Reserve vectors first so we can take safe references to pushed elements
   mSources.reserve(nTracks);
   auto nStages = std::accumulate(mInputs.begin(), mInputs.end(), 0,
      [](auto sum, auto &input){ return sum + input.stages.size(); });
   mSettings.reserve(nStages);
   mStageBuffers.reserve(nStages);

   for (size_t i = 0; i < nTracks;) {
      const auto &input = mInputs[i];
      const auto leader = input.pTrack.get();
      const auto nInChannels = TrackList::Channels(leader).size();
      if (!leader || i + nInChannels > nTracks) {
         assert(false);
         break;
      }
      auto increment = finally([&]{ i += nInChannels; });

      auto &source = mSources.emplace_back( *leader, BufferSize(), outRate,
         warpOptions, highQuality, mayThrow, mTimesAndSpeed,
         (pMixerSpec ? &pMixerSpec->mMap[i] : nullptr));
      AudioGraph::Source *pDownstream = &source;
      for (const auto &stage : input.stages) {
         // Make a mutable copy of stage.settings
         auto &settings = mSettings.emplace_back(stage.settings);
         // TODO: more-than-two-channels
         // Like mFloatBuffers but padding not needed for soxr
         // Allocate one extra buffer to hold dummy zero inputs
         // (Issue 3854)
         auto &stageInput = mStageBuffers.emplace_back(3, mBufferSize, 1);
         const auto &factory = [&stage]{
            // Avoid unnecessary repeated calls to the factory
            return stage.mpFirstInstance
               ? move(stage.mpFirstInstance)
               : stage.factory();
         };
         auto &pNewDownstream =
         mStages.emplace_back(AudioGraph::EffectStage::Create(true,
            *pDownstream, stageInput,
            factory, settings, outRate, std::nullopt, *leader
         ));
         if (pNewDownstream)
            pDownstream = pNewDownstream.get();
         else {
            // Just omit the failed stage from rendering
            // TODO propagate the error?
            mStageBuffers.pop_back();
            mSettings.pop_back();
         }
      }
      mDecoratedSources.emplace_back(Source{ source, *pDownstream });
   }

   // Decide once at construction time
   std::tie(mNeedsDither, mEffectiveFormat) = NeedsDither(needsDither, outRate);
}

Mixer::~Mixer()
{
}

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

   for (const auto &input : mInputs) {
      auto &pTrack = input.pTrack;
      if (!pTrack)
         continue;
      auto &track = *pTrack;
      if (track.GetRate() != rate)
         // Also leads to MixVariableRates(), needs nontrivial resampling
         needsDither = true;
      if (mApplyTrackGains) {
         /// TODO: more-than-two-channels
         for (auto c : {0, 1}) {
            const auto gain = track.GetChannelGain(c);
            if (!(gain == 0.0 || gain == 1.0))
               // Fractional gain may be applied even in MixSameRate
               needsDither = true;
         }
      }

      // Examine all tracks.  (This ignores the time bounds for the mixer.
      // If it did not, we might avoid dither in more cases.  But if we fix
      // that, remember that some mixers change their time bounds after
      // construction, as when scrubbing.)
      if (!track.HasTrivialEnvelope())
         // Varying or non-unit gain may be applied even in MixSameRate
         needsDither = true;
      auto effectiveFormat = track.WidestEffectiveFormat();
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
   for (auto &buffer: mTemp)
      std::fill(buffer.begin(), buffer.end(), 0);
}

static void MixBuffers(unsigned numChannels,
   const unsigned char *channelFlags, const float *gains,
   const float &src, std::vector<std::vector<float>> &dests, int len)
{
   const auto pSrc = &src;
   for (unsigned int c = 0; c < numChannels; c++) {
      if (!channelFlags[c])
         continue;
      float *dest = dests[c].data();
      float gain = gains[c];
      for (int j = 0; j < len; ++j)
         *dest++ += pSrc[j] * gain;   // the actual mixing process
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

   size_t maxOut = 0;
   const auto channelFlags = stackAllocate(unsigned char, mNumChannels);
   const auto gains = stackAllocate(float, mNumChannels);
   if (!mApplyTrackGains)
      std::fill(gains, gains + mNumChannels, 1.0f);

   // Decides which output buffers an input channel accumulates into
   auto findChannelFlags = [&channelFlags, numChannels = mNumChannels]
   (const bool *map, Track::ChannelType channel){
      const auto end = channelFlags + numChannels;
      std::fill(channelFlags, end, 0);
      if (map)
         // ignore left and right when downmixing is customized
         std::copy(map, map + numChannels, channelFlags);
      else switch(channel) {
      case Track::MonoChannel:
      default:
         std::fill(channelFlags, end, 1);
         break;
      case Track::LeftChannel:
         channelFlags[0] = 1;
         break;
      case Track::RightChannel:
         if (numChannels >= 2)
            channelFlags[1] = 1;
         else
            channelFlags[0] = 1;
         break;
      }
      return channelFlags;
   };

   auto &[mT0, mT1, _, mTime] = *mTimesAndSpeed;
   auto oldTime = mTime;
   // backwards (as possibly in scrubbing)
   const auto backwards = (mT0 > mT1);

   Clear();
   // TODO: more-than-two-channels
   auto maxChannels = std::max(2u, mFloatBuffers.Channels());

   for (auto &[ upstream, downstream ] : mDecoratedSources) {
      auto oResult = downstream.Acquire(mFloatBuffers, maxToProcess);
      // One of MixVariableRates or MixSameRate assigns into mTemp[*][*] which
      // are the sources for the CopySamples calls, and they copy into
      // mBuffer[*][*]
      if (!oResult)
         return 0;
      auto result = *oResult;
      maxOut = std::max(maxOut, result);

      // Insert effect stages here!  Passing them all channels of the track

      const auto limit = std::min<size_t>(upstream.Channels(), maxChannels);
      for (size_t j = 0; j < limit; ++j) {
         const auto pFloat = (const float *)mFloatBuffers.GetReadPosition(j);
         const auto track = upstream.GetChannel(j);
         if (mApplyTrackGains)
            for (size_t c = 0; c < mNumChannels; ++c)
               gains[c] = track->GetChannelGain(c);
         const auto flags =
            findChannelFlags(upstream.MixerSpec(j), track->GetChannel());
         MixBuffers(mNumChannels, flags, gains, *pFloat, mTemp, result);
      }

      downstream.Release();
      mFloatBuffers.Advance(result);
      mFloatBuffers.Rotate();
   }

   if (backwards)
      mTime = std::clamp(mTime, mT1, oldTime);
   else
      mTime = std::clamp(mTime, oldTime, mT1);

   const auto dstStride = (mInterleaved ? mNumChannels : 1);
   auto ditherType = mNeedsDither
      ? (mHighQuality ? gHighQualityDither : gLowQualityDither)
      : DitherType::none;
   for (size_t c = 0; c < mNumChannels; ++c)
      CopySamples((constSamplePtr)mTemp[c].data(), floatSample,
         (mInterleaved
            ? mBuffer[0].ptr() + (c * SAMPLE_SIZE(mFormat))
            : mBuffer[c].ptr()
         ),
         mFormat, maxOut, ditherType,
         1, dstStride);

   // MB: this doesn't take warping into account, replaced with code based on mSamplePos
   //mT += (maxOut / mRate);

   assert(maxOut <= maxToProcess);
   return maxOut;
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

#if 0
// Was used before 3.1.0 whenever looping play restarted
// No longer used
void Mixer::Restart()
{
   mTime = mT0;

   for(size_t i=0; i<mNumInputTracks; i++)
      mSamplePos[i] = mInputTrack[i].GetTrack()->TimeToLongSamples(mT0);

   for(size_t i=0; i<mNumInputTracks; i++) {
      mQueueStart[i] = 0;
      mQueueLen[i] = 0;
   }

   // Bug 1887:  libsoxr 0.1.3, first used in Audacity 2.3.0, crashes with
   // constant rate resampling if you try to reuse the resampler after it has
   // flushed.  Should that be considered a bug in sox?  This works around it:
   MakeResamplers();
}
#endif

void Mixer::Reposition(double t, bool bSkipping)
{
   auto &[mT0, mT1, _, mTime] = *mTimesAndSpeed;
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
