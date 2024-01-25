/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  ClipMirAudioReader.h

  Matthieu Hodgkinson

**********************************************************************/
#include "ClipMirAudioReader.h"
#include "ClipInterface.h"

#include <cassert>

ClipMirAudioReader::ClipMirAudioReader(const ClipInterface& clip)
    : mClip(clip)
{
}

double ClipMirAudioReader::GetSampleRate() const
{
   return mClip.GetRate();
}

long long ClipMirAudioReader::GetNumSamples() const
{
   return mClip.GetVisibleSampleCount().as_long_long();
}

void ClipMirAudioReader::ReadFloats(
   float* buffer, long long where, size_t numFrames) const
{
   std::fill(buffer, buffer + numFrames, 0.f);
   AddChannel(0, buffer, where, numFrames);
   if (mClip.GetWidth() == 1)
      return;
   AddChannel(1, buffer, where, numFrames);
   std::transform(
      buffer, buffer + numFrames, buffer, [](float f) { return f / 2; });
}

void ClipMirAudioReader::AddChannel(
   size_t iChannel, float* buffer, sampleCount start, size_t len) const
{
   constexpr auto mayThrow = false;
   auto &cache = mCache[iChannel];
   cache = mClip.GetSampleView(iChannel, start, len, mayThrow);
   cache.AddTo(buffer, len);
}
