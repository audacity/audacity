/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  WaveChannelUtilities.cpp

  Paul Licameli

**********************************************************************/
#include "WaveChannelUtilities.h"
#include "PlaybackDirection.h"
#include "WaveClip.h"
#include "WaveClipUtilities.h"
#include "WaveTrack.h"

#include <cmath>
#include <limits>

std::pair<float, float>
WaveChannelUtilities::GetMinMax(const WaveChannel &channel,
   double t0, double t1, bool mayThrow)
{
   std::pair<float, float> results {
      // we need these at extremes to make sure we find true min and max
      std::numeric_limits<float>::max(),
      std::numeric_limits<float>::lowest(),
   };
   bool clipFound = false;

   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return results;
   }

   if (t0 == t1)
      return results;

   for (const auto &clip: channel.Intervals())
   {
      if (t1 >= clip->GetPlayStartTime() && t0 <= clip->GetPlayEndTime())
      {
         clipFound = true;
         auto clipResults = clip->GetMinMax(t0, t1, mayThrow);
         if (clipResults.first < results.first)
            results.first = clipResults.first;
         if (clipResults.second > results.second)
            results.second = clipResults.second;
      }
   }

   if(!clipFound)
   {
      results = { 0.f, 0.f }; // sensible defaults if no clips found
   }

   return results;
}

float WaveChannelUtilities::GetRMS(const WaveChannel &channel,
   double t0, double t1, bool mayThrow)
{
   if (t0 > t1) {
      if (mayThrow)
         THROW_INCONSISTENCY_EXCEPTION;
      return 0.f;
   }

   if (t0 == t1)
      return 0.f;

   double sumsq = 0.0;
   double duration = 0;

   for (const auto &clip: channel.Intervals())
   {
      // If t1 == clip->GetStartTime() or t0 == clip->GetEndTime(), then the clip
      // is not inside the selection, so we don't want it.
      // if (t1 >= clip->GetStartTime() && t0 <= clip->GetEndTime())
      if (t1 >= clip->GetPlayStartTime() && t0 <= clip->GetPlayEndTime())
      {
         const auto clipStart = std::max(t0, clip->GetPlayStartTime());
         const auto clipEnd = std::min(t1, clip->GetPlayEndTime());

         float cliprms = clip->GetRMS(t0, t1, mayThrow);

         sumsq += cliprms * cliprms * (clipEnd - clipStart);
         duration += (clipEnd - clipStart);
      }
   }
   return duration > 0 ? sqrt(sumsq / duration) : 0.0;
}

namespace {
using namespace WaveChannelUtilities;

void RoundToNearestClipSample(const WaveChannel &channel, double& t)
{
   const auto clip = GetClipAtTime(channel, t);
   if (!clip)
      return;
   t = clip->SamplesToTime(clip->TimeToSamples(t - clip->GetPlayStartTime())) +
       clip->GetPlayStartTime();
}
} // namespace

std::pair<size_t, size_t>
WaveChannelUtilities::GetFloatsCenteredAroundTime(const WaveChannel &channel,
   double t, float* buffer, size_t numSideSamples,
   bool mayThrow)
{
   const auto numSamplesReadLeft = GetFloatsFromTime(channel,
      t, buffer, numSideSamples, mayThrow, PlaybackDirection::backward);
   const auto numSamplesReadRight = GetFloatsFromTime(channel,
      t, buffer + numSideSamples, numSideSamples + 1, mayThrow,
      PlaybackDirection::forward);
   return { numSideSamples - numSamplesReadLeft,
            numSideSamples + numSamplesReadRight };
}

namespace
{
template <typename FloatType>
using BufferCharType = std::conditional_t<
   std::is_const_v<std::remove_pointer_t<FloatType>>, constSamplePtr,
   samplePtr>;

template <typename BufferType> struct SampleAccessArgs
{
   const BufferCharType<BufferType> offsetBuffer;
   const sampleCount start;
   const size_t len;
};

template <typename BufferType>
SampleAccessArgs<BufferType> GetSampleAccessArgs(
   const WaveClip& clip, double startOrEndTime /*absolute*/, BufferType buffer,
   size_t totalToRead, size_t alreadyRead, bool forward)
{
   assert(totalToRead >= alreadyRead);
   const auto remainingToRead = totalToRead - alreadyRead;
   const auto sampsInClip = clip.GetVisibleSampleCount();
   const auto sampsPerSec = clip.GetRate() / clip.GetStretchRatio();
   if (forward)
   {
      const auto startTime =
         std::max(startOrEndTime - clip.GetPlayStartTime(), 0.);
      const sampleCount startSamp { std::round(startTime * sampsPerSec) };
      if (startSamp >= sampsInClip)
         return { nullptr, sampleCount { 0u }, 0u };
      const auto len =
         limitSampleBufferSize(remainingToRead, sampsInClip - startSamp);
      return { reinterpret_cast<BufferCharType<BufferType>>(
                  buffer + alreadyRead),
               startSamp, len };
   }
   else
   {
      const auto endTime = std::min(
         startOrEndTime - clip.GetPlayStartTime(), clip.GetPlayDuration());
      const sampleCount endSamp { std::round(endTime * sampsPerSec) };
      const auto startSamp =
         std::max(endSamp - remainingToRead, sampleCount { 0 });
      // `len` cannot be greater than `remainingToRead`, itself a `size_t` ->
      // safe cast.
      const auto len = (endSamp - startSamp).as_size_t();
      if (len == 0 || startSamp >= sampsInClip)
         return { nullptr, sampleCount { 0u }, 0u };
      const auto bufferEnd = buffer + remainingToRead;
      return { reinterpret_cast<BufferCharType<BufferType>>(bufferEnd - len),
               startSamp, len };
   }
}
} // namespace

size_t WaveChannelUtilities::GetFloatsFromTime(const WaveChannel &channel,
   double t, float* buffer, size_t numSamples, bool mayThrow,
   PlaybackDirection direction)
{
   RoundToNearestClipSample(channel, t);
   auto clip = GetClipAtTime(channel, t);
   auto numSamplesRead = 0u;
   const auto forward = direction == PlaybackDirection::forward;
   const auto clips = SortedClipArray(channel);
   while (clip)
   {
      const auto args = GetSampleAccessArgs(
         *clip, t, buffer, numSamples, numSamplesRead, forward);
      constexpr size_t iChannel = 0;
      if (!clip->GetSamples(
            iChannel, args.offsetBuffer, floatSample, args.start, args.len,
            mayThrow))
         return 0u;
      numSamplesRead += args.len;
      if (numSamplesRead >= numSamples)
         break;
      clip = GetAdjacentClip(clips, *clip, direction);
   }
   return numSamplesRead;
}

bool WaveChannelUtilities::GetFloatAtTime(const WaveChannel &channel,
   double t, float& value, bool mayThrow)
{
   const auto clip = GetClipAtTime(channel, t);
   if (!clip)
      return false;
   constexpr size_t iChannel = 0;
   WaveClipUtilities::GetFloatAtTime(*clip,
      t - clip->GetPlayStartTime(), iChannel, value, mayThrow);
   return true;
}

void WaveChannelUtilities::SetFloatsCenteredAroundTime(WaveChannel &channel,
   double t, const float* buffer, size_t numSideSamples,
   sampleFormat effectiveFormat)
{
   SetFloatsFromTime(channel,
      t, buffer, numSideSamples, effectiveFormat,
      PlaybackDirection::backward);
   SetFloatsFromTime(channel,
      t, buffer + numSideSamples, numSideSamples + 1, effectiveFormat,
      PlaybackDirection::forward);
}

void WaveChannelUtilities::SetFloatsFromTime(WaveChannel &channel,
   double t, const float* buffer, size_t numSamples,
   sampleFormat effectiveFormat, PlaybackDirection direction)
{
   RoundToNearestClipSample(channel, t);
   auto clip = GetClipAtTime(channel, t);
   auto numSamplesWritten = 0u;
   const auto forward = direction == PlaybackDirection::forward;
   const auto clips = SortedClipArray(channel);
   while (clip)
   {
      const auto args = GetSampleAccessArgs(
         *clip, t, buffer, numSamples, numSamplesWritten, forward);
      if (args.len > 0u)
      {
         constexpr size_t iChannel = 0;
         clip->SetSamples(
            iChannel, args.offsetBuffer, floatSample, args.start, args.len,
            effectiveFormat);
         numSamplesWritten += args.len;
         if (numSamplesWritten >= numSamples)
            break;
      }
      clip = GetAdjacentClip(clips, *clip, direction);
   }
}

void WaveChannelUtilities::SetFloatAtTime(WaveChannel &channel,
   double t, float value, sampleFormat effectiveFormat)
{
   SetFloatsCenteredAroundTime(channel, t, &value, 0u, effectiveFormat);
}

void WaveChannelUtilities::SetFloatsWithinTimeRange(WaveChannel &channel,
   double t0, double t1,
   const std::function<float(double sampleTime)>& producer,
   sampleFormat effectiveFormat)
{
   if (t0 >= t1)
      return;
   const auto sortedClips = SortedClipArray(channel);
   if (sortedClips.empty())
      return;
   t0 = std::max(t0, (*sortedClips.begin())->GetPlayStartTime());
   t1 = std::min(t1, (*sortedClips.rbegin())->GetPlayEndTime());
   auto clip = GetClipAtTime(channel, t0);
   const auto clips = SortedClipArray(channel);
   while (clip) {
      const auto clipStartTime = clip->GetPlayStartTime();
      const auto clipEndTime = clip->GetPlayEndTime();
      const auto sampsPerSec = clip->GetRate() / clip->GetStretchRatio();
      const auto roundedT0 =
         std::round((t0 - clipStartTime) * sampsPerSec) / sampsPerSec +
         clipStartTime;
      const auto roundedT1 =
         std::round((t1 - clipStartTime) * sampsPerSec) / sampsPerSec +
         clipStartTime;
      if (clipStartTime > roundedT1)
         break;
      const auto tt0 = std::max(clipStartTime, roundedT0);
      const auto tt1 = std::min(clipEndTime, roundedT1);
      const size_t numSamples = (tt1 - tt0) * sampsPerSec + .5;
      std::vector<float> values(numSamples);
      for (auto i = 0u; i < numSamples; ++i)
         values[i] = producer(tt0 + clip->SamplesToTime(i));
      constexpr size_t iChannel = 0;
      WaveClipUtilities::SetFloatsFromTime(*clip,
         tt0 - clipStartTime, iChannel, values.data(), numSamples,
         effectiveFormat);
      clip = GetNextClip(clips, *clip, PlaybackDirection::forward);
   }
}

namespace {
template<typename Clip>
auto DoGetNextClip(const std::vector<Clip *> &clips,
   const WaveClip& clip, PlaybackDirection direction) -> Clip *
{
   const auto p = find(clips.begin(), clips.end(), &clip);
   if (p == clips.end())
      return nullptr;
   else if (direction == PlaybackDirection::forward)
      return p == clips.end() - 1 ? nullptr : *(p + 1);
   else
      return p == clips.begin() ? nullptr : *(p - 1);
}

template<typename Clip>
auto DoGetAdjacentClip(const std::vector<Clip *> &clips,
   const WaveClip& clip, PlaybackDirection direction) -> Clip *
{
   const auto neighbour = GetNextClip(clips, clip, direction);
   if (!neighbour)
      return nullptr;
   else if (direction == PlaybackDirection::forward)
      return std::abs(clip.GetPlayEndTime() - neighbour->GetPlayStartTime()) <
                   1e-9 ?
                neighbour :
                nullptr;
   else
      return std::abs(clip.GetPlayStartTime() - neighbour->GetPlayEndTime()) <
                   1e-9 ?
                neighbour :
                nullptr;
}
}

const WaveClip*
WaveChannelUtilities::GetAdjacentClip(const WaveClipConstPointers &clips,
   const WaveClip& clip, PlaybackDirection direction)
{
   assert(IsSortedByPlayStartTime(clips));
   return DoGetAdjacentClip(clips, clip, direction);
}

WaveClip *WaveChannelUtilities::GetAdjacentClip(const WaveClipPointers &clips,
   const WaveClip& clip, PlaybackDirection direction)
{
   assert(IsSortedByPlayStartTime(clips));
   return DoGetAdjacentClip(clips, clip, direction);
}

const WaveClip* WaveChannelUtilities::GetNextClip(
   const WaveClipConstPointers &clips,
   const WaveClip& clip, PlaybackDirection direction)
{
   assert(IsSortedByPlayStartTime(clips));
   return DoGetNextClip(clips, clip, direction);
}

WaveClip* WaveChannelUtilities::GetNextClip(const WaveClipPointers &clips,
   const WaveClip& clip, PlaybackDirection direction)
{
   assert(IsSortedByPlayStartTime(clips));
   return DoGetNextClip(clips, clip, direction);
}

Envelope* WaveChannelUtilities::GetEnvelopeAtTime(
   WaveChannel &channel, double time)
{
   // Substitute the leader track
   auto &leader = **channel.GetTrack().Channels().begin();
   if (const auto clip = GetClipAtTime(leader, time))
      return clip->GetEnvelope();
   else
      return nullptr;
}

bool WaveChannelUtilities::CompareClipsByPlayStartTime(
   const WaveClip *x, const WaveClip *y)
{
   return x->GetPlayStartTime() < y->GetPlayStartTime();
}

auto WaveChannelUtilities::SortedClipArray(WaveChannel &channel)
   -> WaveClipPointers
{
   WaveClipPointers result;
   auto &&clips = channel.Intervals();
   // Get the (narrow) clips
   transform(clips.begin(), clips.end(), back_inserter(result),
      [](const auto &interval){ return &interval->GetClip(); });
   sort(result.begin(), result.end(), CompareClipsByPlayStartTime);
   return result;
}


auto WaveChannelUtilities::SortedClipArray(const WaveChannel &channel)
   -> WaveClipConstPointers
{
   auto pointers = SortedClipArray(const_cast<WaveChannel &>(channel));
   return { pointers.begin(), pointers.end() };
}

WaveClip* WaveChannelUtilities::GetClipAtTime(
   WaveChannel &channel, double time)
{
   const auto clips = SortedClipArray(channel);
   auto p = std::find_if(
      clips.rbegin(), clips.rend(), [&](const WaveClip *clip) {
         return clip->WithinPlayRegion(time);
      });
   return p != clips.rend() ? *p : nullptr;
}


const WaveClip* WaveChannelUtilities::GetClipAtTime(
   const WaveChannel &channel, double time)
{
   return GetClipAtTime(const_cast<WaveChannel&>(channel), time);
}
